{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "ansi"
  , "console"
  , "effect"
  , "filterable"
  , "foldable-traversable"
  , "generics-rep"
  , "integers"
  , "lists"
  , "node-fs"
  , "node-process"
  , "parsing"
  , "psci-support"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
