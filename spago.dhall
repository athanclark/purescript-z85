{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "z85"
, dependencies =
  [ "arraybuffer"
  , "assert"
  , "console"
  , "effect"
  , "prelude"
  , "psci-support"
  , "stringutils"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
