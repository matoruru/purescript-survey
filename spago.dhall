{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "ansi"
  , "avar"
  , "console"
  , "effect"
  , "newtype"
  , "node-process"
  , "node-readline"
  , "psci-support"
  , "spec"
  , "unicode"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
