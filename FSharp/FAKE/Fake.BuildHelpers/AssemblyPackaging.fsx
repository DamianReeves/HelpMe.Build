open System
open System.Reflection
open System.IO

type PackageId = PackageId of string | Empty
type PackageIdResolver = Assembly -> PackageId

type AssemblyPackagingParams = {
  IdResolver: PackageIdResolver
  AuthorsResolver:Assembly -> string list
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
  IdResolver = (fun asm -> asm.GetName().Name |> PackageId)
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

let CreateTemplateForAssembly (configure:AssemblyPackagingParams -> AssemblyPackagingParams) (assemblyFile:AssemblyFile) =
  match assemblyFile with
  | NotAnAssemblyFile -> None
  | AssemblyFile (path,assembly) ->
    let parameters = defaultPackagingParams |> configure
    let packageId = parameters.IdResolver assembly
    let authors = parameters.AuthorsResolver assembly
    let files = [path]
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
        println "\t%s ==> lib" file
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