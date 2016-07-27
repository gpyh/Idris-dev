# Idris' Testing Suite

## Running the test suite

Do `make test_all` to run the whole test suite. It will run in parallel, using `cabal test`. Both the C code generator and the node code generator will be used. Use the respective `make test_c` and `make test_js` to only use one of them.

You can also manually call `cabal test` or `stack test`.

Note that cabal will only print the report when all tests are done. Provided your version of cabal supports it, you can do `cabal test --show-details=direct` to output the report in real time.

### Passing arguments

#### To the test program

You can pass arguments to the test program. For example, to pass the `--help` argument:

```
# Via make
make TEST-ARGS="--help" test_all
# Via cabal
cabal test --test-options="--help"
# Via stack
stack test --test-arguments="--help"
```

Try it to learn more about the other arguments you can provide. Two are of particular interest:

- `--codegen` takes a comma-separated list of code generators and will run the test suite with those. You can use any value that would otherwise be valid with the idris option of the same name. A code generator named `none` is used as a placeholder to run tests that don't involve code generation. You should not use this option with `make test_c` and `make test_js` as they set it already.
- `--pattern` takes a regular expression and will only run the tests that match it.

#### To idris

You can pass arguments to idris in each of its invocation by the tests. There are two ways to this. You can either modify the `idrisFlags` term in `TestRun`, or set the `$IDRIS` environment variable accordingly

### Specifying the number of parallel jobs

With make, the test suite runs in parallel by default. For stack and cabal, you need to explicitly enable parallelism as you would do with any other GHC-compiled executable. Examples:

```
# Via cabal
cabal test --test-options="+RTS -N -RTS"
# Via stack
stack test --test-arguments="+RTS -N -RTS"
```

You can put a specific number of threads to be used after `-N`. This cannot be done using `make`.

### Running only previously failed tests

Because of the `--rerun-update` option, `make test_all` will create a `.tasty-rerun-log` file in the root directory of the project. Each time the test suite is run, the file will be written with the result of the tests. The next time you do `make test_all`, you can specify the `rerun-filter` argument to, for example, only run previously failed tests. Valid values are given in the `--help`.

## Extending the test suite

### Content of the directory

To each test there shall be a folder that holds at least a bash script `run` and a golden file `expected`. When running the test suite, the standard output of the script is compared against the golden file. Any mismatch is reported as a failure.

The name of a test folder is the identifier of the *test family* followed by a three digit number. A test family gathers related tests.

Add to that four top level files:

- The program `TestRun.hs` that runs the test suite
- A Haskell module `TestData.hs` with the metadata for each test
- The Perl script `mktest.pl` to ease the creation of a new test
- This `README.md`

### Test families

Tests are categorised with the following test families (and their identifiers):

+ *basic*:          Basic language features, some complete programs
+ *bignum*:         Bignums and GMP
+ *bounded*:        Bounds testing for bits
+ *corecords*:      Tests for corecord
+ *delab*:          De-elaboration tests
+ *directives*:     Tests for directives
+ *disambig*:       Disambiguaton tests
+ *docs*:           Testing documentation functionality
+ *dsl*:            Embedded DSLs and features to support DSL development
+ *effects*:        Effects package
+ *error*:          Error messages and error reflection
+ *ffi*:            FFI calls, including type providers
+ *folding*         Testing some folds
+ *idrisdoc*:       Documentation tool functionality
+ *interactive*:    Interactive editing, proof search
+ *interfaces*:     Testing interfaces.
+ *io*:             IO monad
+ *literate*:       `.lidr` files; literate programming
+ *meta*:           Meta-programming
+ *pkg*:            Packages
+ *primitives*:     Primitive types
+ *proof*:          Theorem proving, tactics
+ *proofsearch*:    Proof search
+ *pruviloj*:       Elaborator reflection
+ *quasiquote*:     Quasiquotations
+ *records*:        Records
+ *reg*:            Regression tests, covering previous bug fixes
+ *regression*:     Regression tests, covering previous bug fixes
+ *sourceLocation*: Interaction with files from Idris
+ *sugar*:          Syntactic sugar, syntax extensions
+ *syntax*:         Syntax extensions
+ *tactics*:        Testing for tactics
+ *totality*:       Totality checking
+ *tutorial*:       Examples from the tutorial
+ *unique*:         Uniqueness types
+ *universes*:      Universes
+ *views*:          Views and covering functions

### Adding a test family

1. Choose an available identifier for your test family. It should be short
and somewhat self-explanatory.
2. Add it in the `README.md` file, i.e. just above.
3. Add it in `TestData.hs`, in the `testFamiliesData` list. Say the chosen identifier is `foo`. You should append to the list `("foo", "A proper name for foo", [ ])`. The empty list will be replaced with the metadata of each test in the family.
4. Add your tests.

### Adding a test

1. Choose the family your test shall belong to and remember its identifier.
2. Pick the next available integer in the test family. It will be the index.
3. Say the family's identifier is `foo` and the index is `42`. You should call `./mktest.pl foo042` ; this will create the directory and a simple `run` script.
4. Modify the `run` script to your liking. If you want to call the idris executable, write `${IDRIS:-idris} $@`.
5. Add any file you may need in the directory. If they don't end in `.idr`, you should remember them for the next step.
6. Add your test in `TestData.hs`. Each family has a list of `(Index, CompatCodegen)`.  See the next section for the available values in `CompatCodegen`. In most cases, you should write `( 42, ANY)`.
7. Generate the `expected` file by doing:
    ```
    # Using cabal
    cabal test --test-options="--pattern foo042 --accept"
    # Using stack
    stack test --test-arguments"--pattern foo042 --accept"
    ```
8. Check the content of `expected`. Maybe the test didn't do what you thought it would. Fix and go back to 7 until it's ok.
9. Add under `Extra-source-files` in `idris.cabal` the patterns that match the folder's content. If you forget this, your test will fail in Travis CI. With the previous example, it should be at least:
    ```
    test/foo042/run
    test/foo042/expected
    test/foo042/*.idr
    ```

### Specifying compatible backends

Some tests only make sense with specific code generators. Others don't generate code. You need to supply this information in `testFamiliesData`. Available values
for the compatible backends are:

- `ANY`: choose this if your test will work with any code generator
- `C_CG`: choose this to only test against the C code generator
- `JS_CG`: choose this to only test against the Node code generator
- `NONE`: choose this if you don't want your test to be run with multiple code
generators (mainly for tests that only perform type checking)

Currently, `NONE` has the same effect as `ANY`, but this will change.

### Removing a test

1. Delete the corresponding directory
2. Remove the corresponding line in the definition of `testFamiliesData` in `TestData.hs`
3. Remove the related lines under `Extra-source-files` in `idris.cabal`

### Updating golden files

To update the `expected` file for every test, do one of the following:

```
# Using make
make test_update
# Using cabal
cabal test --test-options="--accept"
# Using stack
stack test --test-arguments="--accept"
```

"Accepted" tests are the ones that update the golden file. A test can still fail if the `run` script itself crashes.
