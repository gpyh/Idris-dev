module Main where

import Control.Monad
import Data.Typeable
import Data.Proxy
import Data.List
import Data.Map.Strict as Map
import Data.IntSet as ISet

import System.Directory
import System.Environment
import System.Process
import System.Info
import System.IO

import Options.Applicative
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.Runners
import Test.Tasty.Options
import Test.Tasty.Ingredients.Rerun

import Paths_idris as Paths

---------------------------------------------------------------------- [ Types ]

-- A TestFamily groups tests that share the same theme
data TestFamily = TestFamily {
  name :: String, -- A proper name for the test family that will be displayed
  shorthand :: String, -- A shorter lowcase name to use in filenames
  indexes :: IntSet -- The indexes of expected tests ; must be >= 1 and < 1000
} deriving (Show)

type Environment = [(String, String)]
type Flags = [String]

-------------------------------------------------------------------- [ Options ]

-- The `--node` option makes idris use the node code generator
-- As a consequence, incompatible tests are removed

newtype Node = Node Bool deriving (Eq, Ord, Typeable)

nodeArg = "node"
nodeHelp = "Performs the tests with the node code generator"
instance IsOption Node where
  defaultValue = Node False
  parseValue = fmap Node . safeRead
  optionName = return nodeArg
  optionHelp = return nodeHelp
  optionCLParser = fmap Node $ switch (long nodeArg <> help nodeHelp)

ingredients :: [Ingredient]
ingredients = [rerunningTests [consoleTestReporter],
               includingOptions [Option (Proxy :: Proxy Node)] ]

----------------------------------------------------------------------- [ Core ]

-- Turns the collection of TestFamily into a Tree usable by Tasty
goldenTests :: [TestFamily] -> Flags -> Environment -> TestTree
goldenTests testFamilies flags env = testGroup
  "Regression and sanity tests"
  (fmap makeGoldenTestsFamily testFamilies) where
    makeGoldenTestsFamily (TestFamily name shorthand indexes) =
      testGroup name (fmap (makeGoldenTest . indexToString)
                           (ISet.toList indexes)) where
        makeGoldenTest suffix =
          let propername = name ++ " " ++ suffix
              file = (+/) $ testDirectory +/ (shorthand ++ suffix) in
              goldenVsFile propername 
                     (file "expected")
                     (file "output")
                     (runTest file flags env) 


-- Runs a test script
-- "bash" needed because Haskell has cmd as the default shell on windows, and
-- we also want to run the process with another current directory, so we get
-- this thing.
runTest :: (String -> String) -> [String] -> Environment -> IO ()
runTest file flags env = do
  let bash = (proc "bash" ("run" : flags)) {cwd = Just (file ""),
                                            std_out = CreatePipe,
                                            env = Just env}
  (_, output, _) <- readCreateProcessWithExitCode bash ""
  writeFile (file "output") output

-- Called by cabal without options
-- Options available by manual invocation:
-- * `--reuse-`, so it doesn't perform tests that have already been performed
-- * `--node` to test against the node code generator
main :: IO ()
main = do
  -- Copying the environment then adding the IDRIS variable
  -- Check if this works on every platform
  env <- getEnvironment
  idrisPath <- getIdrisPath
  entries <- Main.listDirectory testDirectory
  directories <- filterM (doesDirectoryExist . ((+/) testDirectory)) entries
  let testFamilies = populate directories testFamiliesMap
  defaultMainWithIngredients ingredients $
    askOption $ \(Node node) ->
      goldenTests
        (fmap snd . Map.toList $
          (if node then removeNodeIncompat else id) testFamilies)
          ((if node then ["--codegen node"] else []) ++ idrisFlags)
        (("IDRIS", idrisPath) : env)

----------------------------------------------------------------------- [ Data ]

-- The data to instanciate the collection of TestFamilies
-- The first column is the proper name
-- The second column is the prefix of the directories
testFamiliesData :: [(String, String)]
testFamiliesData = [
  ("Basic",                "basic"           ),
  ("Bignum",               "bignum"          ),
  ("Bounded",              "bounded"         ),
  ("Corecords",            "corecords"       ),
  ("De-elaboration",       "delab"           ),
  ("Directives",           "directives"      ),
  ("Disambiguation",       "disambig"        ),
  ("Documentation",        "docs"            ),
  ("DSL",                  "dsl"             ),
  ("Effects",              "effects"         ),
  ("Errors",               "error"           ),
  ("FFI",                  "ffi"             ),
  ("Folding",              "folding"         ),
  ("Idris documentation",  "idrisdoc"        ),
  ("Interactive editing",  "interactive"     ),
  ("Interfaces",           "interfaces"      ),
  ("IO monad",             "io"              ),
  ("Literate programming", "literate"        ),
  ("Meta-programming",     "meta"            ),
  ("Packages",             "pkg"             ),
  ("Primitive types",      "primitives"      ),
  ("Theorem proving",      "proof"           ),
  ("Proof search",         "proofsearch"     ),
  ("Pruviloj",             "pruviloj"        ),
  ("Quasiquotations",      "quasiquote"      ),
  ("Records",              "records"         ),
  ("Regressions",          "reg"             ),
  ("Regression (loner)",   "regression"      ),
  ("Source location",      "sourceLocation"  ),
  ("Syntactic sugar",      "sugar"           ),
  ("Syntax",               "syntax"          ),
  ("Tactics",              "tactics"         ),
  ("Totality checking",    "totality"        ),
  ("Uniqueness types",     "unique"          ),
  ("Universes",            "universes"       ),
  ("Views",                "views"           )]

-- Maps some TestFamilies to tests that must *not* be performed
-- when using the node code generator
nodeIncompat :: Map String IntSet
nodeIncompat = fmap ISet.fromList $
  Map.fromList [("tutorial",   [7]),
                ("sugar",      [4]),
                ("reg",        [29, 52]),
                ("io",         [1, 3]),
                ("dsl",        [2]),
                ("effects",    [1, 2]),
                ("basic",      [7, 11]),
                ("ffi",        [6, 7, 8]),
                ("primitives", [5, 6]),
                ("views",      [3])]

testDirectory :: String
testDirectory = "test"

idrisFlags :: [String]
idrisFlags = []

----------------------------------------------------------------------- [ Misc ]

-- A safe version of `read`
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
                [(r, "")] -> Just r
                _         -> Nothing

-- Adds a test to a family using its index
addIndex :: Int -> TestFamily -> TestFamily
addIndex index tf = tf { indexes = ISet.insert index (indexes tf) }

-- Adds a test to a family directly via the collection
insertIndex :: String -> Int -> Map String TestFamily -> Map String TestFamily
insertIndex sh index = adjust (addIndex index) sh

-- Adds tests in bulk from a collection of test directories
populate :: [String] -> Map String TestFamily -> Map String TestFamily
populate dirs tfs = Data.List.foldr f tfs dirs where
  f dir tfs = if length dir > 3
                then let (pref, suff) = splitAt (length dir - 3) dir in
                  case readMaybe suff :: Maybe Int of
                    Just index -> insertIndex pref index tfs
                    Nothing -> tfs
                else tfs

-- An empty map of TestFamily
-- The key is the shorthand
testFamiliesMap :: Map String TestFamily
testFamiliesMap = Map.fromList (fmap instanciate testFamiliesData) where
  instanciate (name, shorthand) =
    (shorthand, TestFamily name shorthand ISet.empty)

-- Removes the undesirable tests from the collecton of families when
-- using the node code generator
removeNodeIncompat :: Map String TestFamily -> Map String TestFamily
removeNodeIncompat tfs =
  differenceWith removeBadIndexes tfs nodeIncompat where
    removeBadIndexes tf badIndexes =
      Just (tf { indexes = ISet.difference (indexes tf) badIndexes })

exeExtension :: String
exeExtension =
  if (os `elem` ["win32", "mingw32", "cygwin32"]) then ".exe" else ""

-- Should always output a 3-charater string
indexToString :: Int -> String
indexToString index = let str = show index in
                          (replicate (3 - length str) '0') ++ str

--NOTE: We should Check if the cabal hack works on every platform
getIdrisPath :: IO (String)
getIdrisPath = do
  binDir <- getBinDir
  return (binDir +/ ("idris" ++ exeExtension))

-- Haskell's ls
listDirectory :: String -> IO ([String])
listDirectory d = do
  entries <- getDirectoryContents d
  return $ entries Data.List.\\ [".", ".."]

infixr 7 +/
-- Should form a path
(+/) :: String -> String -> String
(+/) x y = x ++ ('/' : y)

