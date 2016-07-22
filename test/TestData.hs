module TestData where

import Data.IntMap as IMap
import Data.Map.Strict as Map
import Data.Set as Set

type Index = Int


-- A record to hold the metadata of a test
data TestData = TestData {
  -- The name of the test, which should also be the name of the subdirectory
  -- This will be computed from `id` and `testsMap` in TestFamily
  testName :: String,
  -- The code generator that the test is compatible with ; see below
  compatCodegen :: CompatCodegen
}

data CompatCodegen = ANY -- the test is compatible with any code generator
                   | C_CG -- only compatible with the c code generator
                   | NODE_CG -- only compatible with the node code generator
                   | NONE -- does not perform code generation
                   deriving (Eq, Ord, Show)

-- A TestFamily groups tests that share the same theme
data TestFamily = TestFamily {
  -- A shorter lowcase name to use in filenames
  id :: String,
  -- A proper name for the test family that will be displayed
  familyName :: String,
  -- A map of test metadata where the key is the index (>=1 && <1000)
  testsMap :: IntMap CompatCodegen
} deriving (Show)

-- Turns a TestFamily into the list of tests metada is holds
tests :: TestFamily -> [TestData]
tests (TestFamily id _ tests) = IMap.elems $ IMap.mapWithKey mkTest tests where
  mkTest index compatCodegen = TestData (id ++ indexToString index) compatCodegen

-- Should always output a 3-charater string from a postive Int
indexToString :: Int -> String
indexToString index = let str = show index in
                          (replicate (3 - length str) '0') ++ str

-- The list of all TestFamily
testFamilies :: [TestFamily]
testFamilies = fmap instanciate testFamiliesData where
  instanciate (id, name, testsData) =
    TestFamily id name (IMap.fromList testsData)

-- The data to instanciate testFamilies
-- The first column is the id (the prefix of the subfolders)
-- The second column is the proper name
-- The third column is the data for each test
testFamiliesData :: [(String, String, [(Index, CompatCodegen)])]
testFamiliesData = [
  ("basic",           "Basic",
    [ (  1, ANY  ),
      (  2, ANY  ),
      (  3, ANY  ),
      (  4, ANY  ),
      (  5, ANY  ),
      (  6, ANY  ),
      (  7, C_CG ),
      (  8, ANY  ),
      (  9, NONE ),
      ( 10, ANY  ),
      ( 11, C_CG ),
      ( 12, ANY  ),
      ( 13, ANY  ),
      ( 14, NONE ),
      ( 15, ANY  ),
      ( 16, NONE ),
      ( 17, NONE ),
      ( 18, NONE )]),
  ("bignum",          "Bignum",
    [ (  1, ANY  ),
      (  2, ANY  )]),
  ("bounded",         "Bounded",
    [ (  1, ANY  )]),
  ("corecords",       "Corecords",
    [ (  1, ANY  ),
      (  2, ANY  )]),
  ("delab",           "De-elaboration",
    [ (  1, ANY  )]),
  ("directives",      "Directives",
    [ (  1, NONE ),
      (  2, NONE )]),
  ("disambig",        "Disambiguation",
    [ (  2, NONE )]),
  ("docs",            "Documentation",
    [ (  1, NONE  ),
      (  2, NONE  ),
      (  3, NONE  ),
      (  4, NONE  ),
      (  5, NONE  )]),
  ("dsl",             "DSL",
    [ (  1, ANY  ),
      (  2, C_CG ),
      (  3, NONE ),
      (  4, NONE )]),
  ("effects",         "Effects",
    [ (  1, C_CG ),
      (  2, C_CG ),
      (  3, ANY  ),
      (  4, ANY  ),
      (  5, ANY  )]),
  ("error",           "Errors",
    [ (  1, NONE ),
      (  2, ANY  ),
      (  3, NONE ),
      (  4, NONE ),
      (  5, NONE ),
      (  6, NONE ),
      (  7, NONE ),
      (  8, NONE )]),
  ("ffi",             "FFI",
    [ (  1, ANY  ),
      (  2, ANY  ),
      (  3, ANY  ),
      (  4, NONE ),
      (  5, ANY  ),
      (  6, C_CG ),
      (  7, C_CG ),
      (  8, C_CG )]),
  ("folding",         "Folding",
    [ (  1 , ANY  )]),
  ("idrisdoc",        "Idris documentation",
    [ (  1, NONE ),
      (  2, NONE ),
      (  3, NONE ),
      (  4, NONE ),
      (  5, NONE ),
      (  6, NONE ),
      (  7, NONE ),
      (  8, NONE ),
      (  9, NONE )]),
  ("interactive",     "Interactive editing",
    [ (  1, NONE ),
      (  2, NONE ),
      (  3, NONE ),
      (  4, NONE ),
      (  5, ANY  ),
      (  6, NONE ),
      (  7, NONE ),
      (  8, NONE ),
      (  9, NONE ),
      ( 10, NONE ),
      ( 11, NONE ),
      ( 12, NONE ),
      ( 13, NONE )]),
  ("interfaces",      "Interfaces",
    [ (  1, NONE ),
      (  2, ANY  ),
      (  3, ANY  ),
      (  4, ANY  ),
      (  5, NONE ),
      (  6, NONE )]),
  ("io",              "IO monad",
    [ (  1, C_CG ),
      (  2, ANY  ),
      (  3, C_CG )]),
  ("literate",        "Literate programming",
    [ (  1, ANY  )]),
  ("meta",            "Meta-programming",
    [ (  1, ANY  ),
      (  2, NONE ),
      (  3, NONE ),
      (  4, NONE )]),
  ("pkg",             "Packages",
    [ (  1, ANY  ),
      (  2, ANY  ),
      (  3, ANY  ),
      (  4, ANY  )]),
  ("primitives",      "Primitive types",
    [ (  1, ANY  ),
      (  2, ANY  ),
      (  3, ANY  ),
      (  5, C_CG ),
      (  6, C_CG )]),
  ("proof",           "Theorem proving",
    [ (  1, NONE ),
      (  2, NONE ),
      (  3, ANY  ),
      (  4, NONE ),
      (  5, NONE ),
      (  6, NONE ),
      (  7, NONE ),
      (  8, NONE ),
      (  9, NONE ),
      ( 10, ANY  ),
      ( 11, NONE )]),
  ("proofsearch",     "Proof search",
    [ (  1, NONE ),
      (  2, NONE ),
      (  3, NONE )]),
  ("pruviloj",        "Pruviloj",
    [ (  1, ANY  )]),
  ("quasiquote",      "Quasiquotations",
    [ (  1, ANY  ),
      (  2, ANY  ),
      (  3, ANY  ),
      (  4, ANY  ),
      (  5, ANY  ),
      (  6, ANY  )]),
  ("records",         "Records",
    [ (  1, ANY  ),
      (  2, ANY  ),
      (  3, ANY  ),
      (  4, ANY  ),
      (  5, ANY  )]),
  ("reg",             "Regressions",
    [ (  2, ANY  ),
      (  3, NONE ),
      (  4, ANY  ),
      (  5, ANY  ),
      (  6, NONE ),
      (  7, NONE ),
      ( 10, NONE ),
      ( 13, ANY  ),
      ( 16, ANY  ),
      ( 17, ANY  ),
      ( 18, NONE ),
      ( 20, ANY  ),
      ( 23, NONE ),
      ( 24, ANY  ),
      ( 25, ANY  ),
      ( 27, ANY  ),
      ( 28, NONE ),
      ( 29, C_CG ),
      ( 31, ANY  ),
      ( 32, ANY  ),
      ( 34, NONE ),
      ( 35, NONE ),
      ( 39, ANY  ),
      ( 40, ANY  ),
      ( 41, ANY  ),
      ( 42, ANY  ),
      ( 44, ANY  ),
      ( 45, ANY  ),
      ( 48, ANY  ),
      ( 49, NONE ),
      ( 50, NONE ),
      ( 52, C_CG ),
      ( 54, NONE ),
      ( 55, NONE ),
      ( 56, ANY  ),
      ( 67, ANY  ),
      ( 68, NONE ),
      ( 69, NONE ),
      ( 70, NONE ),
      ( 72, NONE ),
      ( 75, NONE )]),
  ("regression",      "Regression (aggregation)",
    [ (  1 , NONE )]),
  ("sourceLocation",  "Source location",
    [ (  1 , ANY  )]),
  ("sugar",           "Syntactic sugar",
    [ (  1, ANY  ),
      (  2, ANY  ),
      (  3, ANY  ),
      (  4, C_CG ),
      (  5, ANY  )]),
  ("syntax",          "Syntax extensions",
    [ (  1, NONE ),
      (  2, ANY  )]),
  ("tactics",         "Tactics",
    [ (  1, NONE )]),
  ("totality",        "Totality checking",
    [ (  1, NONE ),
      (  2, NONE ),
      (  3, NONE ),
      (  4, ANY  ),
      (  5, ANY  ),
      (  6, NONE ),
      (  7, ANY  ),
      (  8, NONE ),
      (  9, NONE ),
      ( 10, NONE ),
      ( 11, NONE ),
      ( 12, NONE ),
      ( 13, NONE ),
      ( 14, NONE ),
      ( 15, NONE )]),
  ("tutorial",        "Tutorial examples",
    [ (  1, NONE ),
      (  2, NONE ),
      (  3, NONE ),
      (  4, NONE ),
      (  5, NONE ),
      (  6, NONE ),
      (  7, C_CG )]),
  ("unique",          "Uniqueness types",
    [ (  1, ANY  ),
      (  2, NONE ),
      (  3, NONE )]),
  ("universes",       "Universes",
    [ (  1, NONE ),
      (  2, NONE )]),
  ("views",           "Views",
    [ (  1, ANY  ),
      (  2, ANY  ),
      (  3, C_CG )])]

