{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "z85"
, dependencies =
  [ "arraybuffer"
  , "numbers"
  , "prelude"
  , "sized-vectors"
  , "stringutils"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "BSD-3-Clause"
, repository = "https://github.com/athanclark/purescript-z85.git"
}
