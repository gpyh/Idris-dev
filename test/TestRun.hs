module Main where

import Control.Monad
import Data.Typeable
import Data.Proxy
import Data.List
import Data.Maybe
import Data.Char
import Data.List.Split (endBy)
import qualified Data.Set as Set
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import System.Directory
import System.Environment
import System.Process
import System.Info
import System.IO
import System.FilePath ((</>))
import Options.Applicative
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.Golden.Advanced
import Test.Tasty.Runners
import Test.Tasty.Options
import Test.Tasty.Ingredients.Rerun

import TestData

---------------------------------------------------------------------- [ Types ]

-- For convenience
type Flags = [String]

-- Actual code generators ; this is reminiscent of TestData.CompatCodegen
data Codegen = None -- When no code generation is performed
             | C
             | Node
             | Custom String -- For code generators not shipped with idris
             deriving (Eq, Ord)

--------------------------------------------------------------------- [ Config ]

-- This list is added in each call of the idris executable
idrisFlags :: Flags
idrisFlags = []

testDirectory :: String
testDirectory = "test"

-------------------------------------------------------------------- [ Codegen ]

instance Show Codegen where
  show None        = "none"
  show C           = "c"
  show Node        = "node"
  show (Custom cg) = cg

parseCodegen :: String -> Codegen
parseCodegen "none" = None
parseCodegen "c"    = C
parseCodegen "node" = Node
parseCodegen cg     = Custom cg

title :: Codegen -> String
title None = "No code generation"
title cg = capitalize (show cg) ++ " code generator" where
  capitalize [] = []
  capitalize (c : str) = (toUpper c) : str

-- Rule to match Codegen with CompatCodegen
compatibleWith :: Codegen -> CompatCodegen -> Bool
compatibleWith cg    ANY     = cg /= None
compatibleWith C     C_CG    = True
compatibleWith Node  NODE_CG = True
compatibleWith None  NONE    = True
compatibleWith _     _       = False

codegenFlags :: Codegen -> Flags
codegenFlags None = []
codegenFlags cg   = [ "--codegen", (show cg)]

-------------------------------------------------------------------- [ Options ]

-- This option accepts comma separated lists of code generators
-- These code generators will be used to perform the tests
-- The `none` code generator only match tests that don't generate code

newtype CodegenOpt = CodegenOpt (Set.Set Codegen) deriving (Typeable)

codegenArg = "codegen"
codegenHelp = "TODO"
codegenDefault = Set.fromList [ None, C, Node ]

instance IsOption CodegenOpt where
  optionName = return codegenArg
  optionHelp = return codegenHelp
  defaultValue = CodegenOpt codegenDefault
  parseValue =
    Just . CodegenOpt . Set.fromList . (fmap (parseCodegen . trim)) . endBy ","
      where trim = reverse. dropWhile isSpace . reverse . dropWhile isSpace

ingredients :: [Ingredient]
ingredients = [ rerunningTests [consoleTestReporter], -- use Tasty.Rerun
                includingOptions
                  [Option (Proxy :: Proxy CodegenOpt)] -- register CodegenOpt
              ]

----------------------------------------------------------------------- [ Core ]

main :: IO ()
main = do
  defaultMainWithIngredients ingredients $
    askOption $ \(CodegenOpt codegens) -> mkGoldenTests codegens testFamilies

-- Turns the collection of TestFamily into actual tests usable by Tasty
mkGoldenTests :: Set.Set Codegen -> [TestFamily] -> TestTree
mkGoldenTests codegens testFamilies =
  testGroup "Regression and sanity tests"
            (fmap mkCodegenGroup $ Set.toList codegens)
    where
      mkCodegenGroup cg =
        testGroup (title cg) (fmap (mkTests cg) testFamilies)
      mkTests cg tf =
        testGroup (familyName tf) (mapMaybe (mkTest cg) (tests tf))
      mkTest cg (TestData name compatCodegen) =
        if (cg `compatibleWith` compatCodegen)
           then Just (test name (runTest name (codegenFlags cg ++ idrisFlags)))
           else Nothing

-- Compare a given file contents against the golden file contents
-- A ripoff of goldenVsFile from Tasty.Golden
test :: String -> IO () -> TestTree 
test name act =
  goldenTest name (BS.readFile ref) (act >> BS.readFile new) cmp upd
    where
      path = testDirectory </> name
      ref = path </> "expected"
      new = path </> "output"
      cmp x y = return $ if x == y then Nothing
                                   else Just $ printDiff (ref, x) (new, y)
      upd = BS.writeFile ref

-- Takes the filepath and content of `expected` and `output`
-- and formats an error message stating their difference
printDiff :: (String, BS.ByteString) -> (String, BS.ByteString) -> String
printDiff (ref, x) (new, y) =
  let refcnt = BSC.unpack x
      newcnt = BSC.unpack y
      printContent cnt =
        if Data.List.null cnt
           then " is empty...\n"
           else " is: \n" ++ unlines (fmap ((++) "  ") (lines cnt))
   in
     "Test mismatch!\n" ++
       "Golden file " ++ ref ++ printContent refcnt ++
       "However, " ++ new ++ printContent newcnt

-- Runs a test script
-- "bash" needed because Haskell has cmd as the default shell on windows, and
-- we also want to run the process with another current directory, so we get
-- this thing.
runTest :: String -> Flags -> IO ()
runTest name flags = do
  let path = testDirectory </> name
  let run = (proc "bash" ("run" : flags)) {cwd = Just path,
                                           std_out = CreatePipe}
  (_, output, _) <- readCreateProcessWithExitCode run ""
  writeFile (path </> "output") output

