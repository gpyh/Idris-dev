#!/usr/bin/env bash

## before_install

# Prepare and output the environment
function before_install_env() {
  # Capturing cabal version
  export CABALVER=${BUILD:6}
  # CC is set by the `compiler` attribute.
  # It is of the form `#GHC-a.b.c`. We capture the version and unsets CC so
  # it does not interfere with C compilation
  export GHCVER=${CC:5}
  unset CC

  export PATH=/opt/ghc/$GHCVER/bin:/opt/cabal/$CABALVER/bin:$HOME/.cabal/bin:$PATH 

  # Uncomment the following line to enable profiling
  # export GHCRTS='-s'

  echo "Environment:"
  env || return $?

  echo "VM stats:"
  vmstat -s || return $?
}

## install

# Output versions used
function install_version() {
  cabal --version || return $?
  ghc --version   || return $?
}

# Install dependencies or fetch them from the cache
function install_dependencies() (
  set -e
  echo "Unpacking the hackage index..."
  if [ -f $HOME/.cabal/packages/hackage.haskell.org/00-index.tar.gz ];
  then
    zcat $HOME/.cabal/packages/hackage.haskell.org/00-index.tar.gz > \
        $HOME/.cabal/packages/hackage.haskell.org/00-index.tar;
  fi
  echo "Cabal update..."
  travis_retry cabal update -v
  # Run build with 2 parallel jobs
  # The container environment reports 16 cores,
  # causing cabal's default configuration (jobs: $ncpus)
  # to run into the GHC #9221 bug which can result in longer build-times.
  echo "Fixing the number of jobs..."
  sed -i -r 's/(^jobs:).*/\1 2/' $HOME/.cabal/config
  
  echo "Fetching the list of dependencies..."
  cabal install -f FFI --only-dependencies --enable-tests --dry -v | \
    sort | sed -e '1,/^Resolving /d' > installplan.txt

  echo "Diff between the fetched list and the cache:"
  if diff -u installplan.txt $HOME/.cabsnap/installplan.txt;
  then
    echo "Cabal build-cache HIT!";
    rm -rfv .ghc;
    echo "Copying the cache..."
    cp -a $HOME/.cabsnap/ghc $HOME/.ghc;
    cp -a $HOME/.cabsnap/lib $HOME/.cabsnap/share $HOME/.cabsnap/bin $HOME/.cabal/;
  else
    echo "Cabal build-cache MISS!";
    rm -rf $HOME/.cabsnap;
    mkdir -p $HOME/.ghc $HOME/.cabal/lib $HOME/.cabal/share $HOME/.cabal/bin;
    echo "Installing the dependencies..."
    cabal install -f FFI --only-dependencies --enable-tests;
  fi

  # snapshot package-db on cache miss
  if [ ! -d $HOME/.cabsnap ];
  then
    echo "Snapshotting package-db to build-cache...";
    mkdir $HOME/.cabsnap;
    cp -a $HOME/.ghc $HOME/.cabsnap/ghc;
    cp -a $HOME/.cabal/lib $HOME/.cabal/share $HOME/.cabal/bin installplan.txt $HOME/.cabsnap/;
  fi

  echo "Done."
)

## before_script

# Pack idris into a distributable archive and untar it right away
function before_script_dist() {
  echo "Cabal sdist..."
  cabal sdist || return $?

  echo "Unpacking the distribution archive..."
  cd ..
  tar -xf Idris-dev/dist/idris*.tar.gz
  cd idris*

  echo "Done."
}

## script

# Cabal configure
function script_configure() (
  start_script_fold "configure"

  echo "Cabal configure..."
  cabal configure -f FFI -f CI --enable-tests

  echo "Done."
  end_script_fold
)

# Cabal build
function script_build() (
  start_script_fold "build"

  echo "Cabal build..."
  cabal build
  
  echo "Done."
  end_script_fold
)

# Cabal copy
function script_copy() (
  start_script_fold "copy"

  echo "Cabal copy..."
  cabal copy

  echo "Done."
  end_script_fold
)

# Cabal register
function script_register() (
  start_script_fold "register"

  echo "Cabal register..."
  cabal register

  echo "Done."
  end_script_fold
)

function script_task() {
  script_$TASK
  return $?
}

# Perform the tests
function script_tests() (
  start_script_fold "tests"

  echo "Cppcheck on mini-gmp.c..."
  cppcheck -i 'mini-gmp.c' rts;

  echo "Perfoming tests..."
  case $CABALVER in
    "1.20")
      # travis_wait because cabal only prints the output
      # when the tests are done, and this can exceed 10 minutes
      travis_wait make ARGS="--show-details=always" TEST-JOBS=2 test_all;
      ;;
    "1.22")
      make ARGS="--show-details=streaming" TEST-JOBS=2 test_all;
      ;;
    *) # 1.24 and beyond
      make ARGS="--show-details=direct" TEST-JOBS=2 test_all;
      ;;
  esac

  echo "Done."
  end_script_fold
)

function script_docs() (
  start_script_fold "docs"

  echo "Generation documentation..."
  make -j2 lib_doc doc

  echo "Done."
  end_script_fold
)

# Benchmarks
function script_benchmarks() (
  start_script_fold "benchmarks"

  echo "Perfoming benchmarks..."
  cd benchmarks && ./build.pl && ./run.pl && cd ..

  echo "Done."
  end_script_fold
)

## helpers

# Using an undocumented "feature" to enable folding during the script phase
# This allows to selectively hide parts of the output
# See https://github.com/travis-ci/travis-ci/issues/2285

function start_script_fold() {
  set -e
  export CURRENT_FOLD=$1
  echo -en "travis_fold:start:script.$CURRENT_FOLD\\r"
  echo "script_$CURRENT_FOLD"
}

function end_script_fold() {
  echo -en "travis_fold:end:script.$CURRENT_FOLD\\r"
}
