open System
open System.Reflection

type PackageId = PackageId of string
type PackageIdResolver = Assembly -> PackageId

type AssemblyPackagingParams = {
  PackageResolver: PackageIdResolver
}

let Package (configure:AssemblyPackagingParams -> AssemblyPackagingParams) =
  ()