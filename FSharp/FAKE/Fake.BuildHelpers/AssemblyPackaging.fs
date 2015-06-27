module Fake.BuildHelpers.AssemblyPackaging
open System
open System.Reflection
open System.IO

module AssemblyHelper =
  open System
  open System.Reflection
  open System.Text.RegularExpressions

  let defaultAssemblyExclusionRegex = Regex("""^System|^mscorlib|^Microsoft.CSharp$|^WindowsBase$""", RegexOptions.Compiled ||| RegexOptions.IgnoreCase)
//  type AssemblyCompanyInfo = 
//    | AssemblyCompany 
//    | NoAssemblyCompany

  let (|AssemblyCompany|NoCompany|) (assembly:Assembly) = 
    match assembly.GetCustomAttribute<AssemblyCompanyAttribute>() with
    | null -> NoCompany
    | attrib -> AssemblyCompany attrib.Company  

  type AssemblyHandle =
    | AssemblyInstance of Value:Assembly
    | AssemblyNameInstance of Value:AssemblyName
    with
    member this.FullName =
      match this with
      | AssemblyInstance value -> value.GetName().FullName
      | AssemblyNameInstance value -> value.FullName

    member this.Name =
      match this with
      | AssemblyInstance value -> value.GetName().Name
      | AssemblyNameInstance value -> value.Name

  type AssemblyFile =
    |AssemblyFile of Path:string * Assembly:Assembly
    |NotAnAssemblyFile
    static member Create path = 
      try
        let asm = Assembly.LoadFrom(path)
        AssemblyFile (path,asm)
      with
        | _ ->  NotAnAssemblyFile     

  let FilterSystemAssemblies (assemblies: AssemblyHandle seq) =
    assemblies |> Seq.where (fun asm -> not(defaultAssemblyExclusionRegex.IsMatch(asm.Name)))

  let FilterGacAssemblies (assemblies: AssemblyHandle seq) =
    assemblies |> Seq.where (fun asm -> 
      match asm with
      | AssemblyNameInstance assemblyName ->
        printfn "[AssemblyName:%O]" assemblyName
        true
      | _ -> true
    )

open AssemblyHelper

type PackageId = PackageId of string | Empty
type PackageIdResolver = Assembly -> PackageId
type OutputDirectory = DirectoryName of string  | Directory of DirectoryInfo | SameAsAssembly
type AssemblyPackagingParams = {
  IdResolver: PackageIdResolver
  AuthorsResolver:Assembly -> string list
  ReferencedAssembliesFilter:AssemblyHandle seq -> AssemblyHandle seq
  OutputDir: OutputDirectory
}

type AssemblyFile =
  |AssemblyFile of Path:string * Assembly:Assembly
  |NotAnAssemblyFile
  static member Create path = 
    try
      let asm = Assembly.LoadFrom(path)
      AssemblyFile (path,asm)
    with
      | _ ->  NotAnAssemblyFile    

let (|AssemblyCompany|NoCompany|) (assembly:Assembly) = 
  match assembly.GetCustomAttribute<AssemblyCompanyAttribute>() with
  | null -> NoCompany
  | attrib -> AssemblyCompany attrib.Company


let defaultPackagingParams = {
  OutputDir = SameAsAssembly
  IdResolver = (fun asm -> asm.GetName().Name |> PackageId)
  ReferencedAssembliesFilter = FilterSystemAssemblies >> FilterGacAssemblies
  AuthorsResolver = (fun asm ->
    match asm with
    | AssemblyCompany company when String.IsNullOrWhiteSpace(company) -> []
    | AssemblyCompany company -> [company]    
    | _ -> []  
  )
}

type PaketTemplateType =
  | Project
  | File

type PaketTemplate ={
  Type:PaketTemplateType
  Id:PackageId
  Authors: string list
  Files: string list
}

let defaultPaketTemplate = {
  PaketTemplate.Type=File
  Id = PackageId.Empty
  Authors = []
  Files = []
}

module PathHelper =
  open System
  open System.IO
  open System.Text.RegularExpressions

  let internal libFileExtensionsRegex = new Regex("""\.dll$|\.exe""", RegexOptions.Compiled ||| RegexOptions.IgnoreCase)
  let MakeRelativePath basePath absolutePath =
    let dirSep = Path.DirectorySeparatorChar
    let basePath = 
      match basePath with
      | str when String.IsNullOrWhiteSpace(str) -> Directory.GetCurrentDirectory()
      | str -> str

    let absolutePath =
      match absolutePath with
      | str when String.IsNullOrWhiteSpace(str) -> Directory.GetCurrentDirectory()
      | str -> str

    let rec findCommon pathA pathB accum =
      let accum = match accum with null -> "" | str -> str
      match (pathA,pathB) with
      | (aHead::aRest,bHead::bRest) when String.Equals(aHead,bHead, StringComparison.OrdinalIgnoreCase) ->
        sprintf "%s%s%c" accum aHead dirSep |> findCommon aRest bRest        
      | _ -> accum

    let baseParts =  basePath.Split(dirSep) |> Array.toList 
    let absParts = absolutePath.Split(dirSep) |> Array.toList
    let common = findCommon baseParts absParts ""
    let returnPath =
      if common.Length > 0 then      
        Array.fold(fun acc elem ->
            match elem with
            | null | "" -> acc
            | str -> sprintf"%s%s%c" acc  ".."  dirSep
          ) (String.Empty) <| (basePath.Substring(common.Length - 1).Split(dirSep))        
      else
        ""
    returnPath + absolutePath.Substring(common.Length)
      


let CreateTemplateForAssembly (configure:AssemblyPackagingParams -> AssemblyPackagingParams) (assemblyFile:AssemblyFile) =
  match assemblyFile with
  | NotAnAssemblyFile -> None
  | AssemblyFile (path,assembly) ->
    let baseDir = Path.GetDirectoryName(path)
    let relativePath = PathHelper.MakeRelativePath baseDir path
    let parameters = defaultPackagingParams |> configure
    let packageId = parameters.IdResolver assembly
    let authors = parameters.AuthorsResolver assembly

    let references = 
      assembly.GetReferencedAssemblies() 
      |> Seq.map (fun asm -> AssemblyNameInstance asm)
      |> parameters.ReferencedAssembliesFilter

    let files = [relativePath] @ [for asm in references -> asm.Name]
    {defaultPaketTemplate with
      Id=packageId
      Authors=authors
      Files=files} |> Some

let CreatePaketTemplate (configure:AssemblyPackagingParams -> AssemblyPackagingParams) (assemblyFilePath:string) =
  assemblyFilePath |> AssemblyFile.Create |> CreateTemplateForAssembly configure
    
type PaketTemplate with
  member this.Print (writer:TextWriter) = 
    let println format = fprintfn writer format
    let print format = fprintf writer format

    match this.Type with
    | Project -> println "type project"  
    | File -> println "type file"

    match this.Id with
    | Empty -> println "id <N/A>"
    | PackageId id -> println "id %s" id

    match this.Authors with
    | [] -> ()
    | authors -> String.Join(",",authors) |>  println "authors %s"

    match this.Files with
    | [] -> ()
    | files ->
      println "files"
      files|> Seq.iter (fun file ->        
        if PathHelper.libFileExtensionsRegex.IsMatch(file) then
          println "\t%s ==> lib" file
        else
          println "\t%s.dll ==> lib" file
          println "\t%s.exe ==> lib" file
          println "\t%s.pdb ==> lib" file
          println "\t%s.xml ==> lib" file
      )

let printPaketTemplateFor assemblyFile = 
  let template = CreatePaketTemplate (fun x->x) assemblyFile
  template |> printfn "PaketTemplate: %A"
  match template with
  | None -> ()
  | Some tpl -> tpl.Print Console.Out

let printPaketTemplates (assemblyFiles:seq<string>) = 
  assemblyFiles
  |> Seq.iter (fun path ->     
    let template = CreatePaketTemplate (fun x->x) path
    match template with
    | None -> ()
    | Some tpl -> 
      printfn "#########################################################################"
      printfn "# Template For: %s" path
      printfn "#########################################################################"
      let templatePath = sprintf "%s.paket.template" path
      tpl.Print Console.Out
  )

let WritePaketTemplates configure (assemblyFiles:seq<string>) = 
  assemblyFiles
  |> Seq.iter (fun path ->     
    let template = CreatePaketTemplate configure path
    match template with
    | None -> ()
    | Some tpl -> 
      let templatePath = sprintf "%s.paket.template" path
      use sw = new StreamWriter(path=templatePath)
      tpl.Print sw
  )

let writePaketTemplates (assemblyFiles:seq<string>) = 
  assemblyFiles
  |> Seq.iter (fun path ->     
    let template = CreatePaketTemplate (fun x->x) path
    match template with
    | None -> ()
    | Some tpl -> 
      let templatePath = sprintf "%s.paket.template" path
      use sw = new StreamWriter(path=templatePath)
      tpl.Print sw
  )