#load "AssemblyPackaging.fs"
open System
open System.IO
open System.Reflection
open Fake.BuildHelpers.AssemblyPackaging

let dir path = DirectoryInfo(path)
let fullPath (dir:DirectoryInfo) = dir.FullName

let fakeToolsDir = Path.Combine(__SOURCE_DIRECTORY__, """..\..\..\packages\FAKE\tools""") |> dir
fakeToolsDir.FullName |> printfn "FakeToolsFolder: %s" 

fakeToolsDir.EnumerateFiles()
  |> Seq.map (fun file -> file.FullName)
  |> printPaketTemplates