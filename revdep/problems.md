# AMR

Version: 0.5.0

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        R      2.0Mb
        data   2.0Mb
    ```

# amt

Version: 0.0.5.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Rcpp’ ‘magrittr’
      All declared Imports should be used.
    ```

# anomalize

Version: 0.1.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        help   4.7Mb
    ```

# arena2r

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘purrr’ ‘shinyBS’ ‘shinydashboard’ ‘shinyjs’
      All declared Imports should be used.
    ```

# auk

Version: 0.3.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 552 marked UTF-8 strings
    ```

# BaMORC

Version: 1.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.9Mb
      sub-directories of 1Mb or more:
        data   5.0Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘devtools’ ‘docopt’
      All declared Imports should be used.
    ```

# banR

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘stringr’
      All declared Imports should be used.
    ```

# banter

Version: 0.9.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ranger’
      All declared Imports should be used.
    ```

# bayesdfa

Version: 0.1.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        libs   4.5Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# BayesMallows

Version: 0.2.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Using exact partition function.
    Computing Mallows model with 4 clusters.
    Using exact partition function.
    Computing Mallows model with 5 clusters.
    Using exact partition function.
    Computing Mallows model with 6 clusters.
    Using exact partition function.
    Computing Mallows model with 7 clusters.
    Using exact partition function.
    Computing Mallows model with 8 clusters.
    Using exact partition function.
    Computing Mallows model with 9 clusters.
    Using exact partition function.
    Computing Mallows model with 10 clusters.
    Using exact partition function.
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'BayesMallowsPackage.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# bayesplot

Version: 1.6.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.2Mb
      sub-directories of 1Mb or more:
        R     2.5Mb
        doc   4.0Mb
    ```

# baystability

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ggfortify’ ‘ggplot2’ ‘matrixStats’ ‘reshape2’ ‘scales’
      All declared Imports should be used.
    ```

# bigQueryR

Version: 0.4.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘bigrquery’
    ```

# biovizBase

Version: 1.30.1

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘biovizBase-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: GCcontent
    > ### Title: GC content computation for BSgenome
    > ### Aliases: GCcontent
    > 
    > ### ** Examples
    > 
    > library(BSgenome.Hsapiens.UCSC.hg19)
    Error in library(BSgenome.Hsapiens.UCSC.hg19) : 
      there is no package called 'BSgenome.Hsapiens.UCSC.hg19'
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/runTests.R’ failed.
    Last 13 lines of output:
       
      1 Test Suite : 
      biovizBase RUnit Tests - 1 test function, 1 error, 0 failures
      ERROR in /Users/lionel/Desktop/rlang/revdep/checks.noindex/biovizBase/new/biovizBase.Rcheck/biovizBase/unitTests/test_ensdb.R: Error while sourcing  /Users/lionel/Desktop/rlang/revdep/checks.noindex/biovizBase/new/biovizBase.Rcheck/biovizBase/unitTests/test_ensdb.R : Error in library(EnsDb.Hsapiens.v75) : 
        there is no package called 'EnsDb.Hsapiens.v75'
      
      Test files with failing tests
      
         test_ensdb.R 
           /Users/lionel/Desktop/rlang/revdep/checks.noindex/biovizBase/new/biovizBase.Rcheck/biovizBase/unitTests/test_ensdb.R 
      
      
      Error in BiocGenerics:::testPackage("biovizBase") : 
        unit tests failed for package biovizBase
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘BSgenome.Hsapiens.UCSC.hg19’ ‘TxDb.Hsapiens.UCSC.hg19.knownGene’
      ‘EnsDb.Hsapiens.v75’
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Title field: should not end in a period.
    ```

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' calls in package code:
      'BSgenome' 'rtracklayer'
      Please use :: or requireNamespace() instead.
      See section 'Suggested packages' in the 'Writing R Extensions' manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/biovizBase/new/biovizBase.Rcheck/00_pkg_src/biovizBase/R/transform.R:447)
    transformToLinkInCircle : <anonymous>: no visible binding for global
      variable 'to.y'
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/biovizBase/new/biovizBase.Rcheck/00_pkg_src/biovizBase/R/transform.R:447)
    mold,ExpressionSet: no visible global function definition for 'exprs'
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/biovizBase/new/biovizBase.Rcheck/00_pkg_src/biovizBase/R/mold-method.R:55)
    mold,ExpressionSet: no visible global function definition for 'pData'
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/biovizBase/new/biovizBase.Rcheck/00_pkg_src/biovizBase/R/mold-method.R:56)
    mold,ExpressionSet: no visible global function definition for 'exprs'
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/biovizBase/new/biovizBase.Rcheck/00_pkg_src/biovizBase/R/mold-method.R:57)
    mold,RleList: no visible binding for global variable 'xRleList'
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/biovizBase/new/biovizBase.Rcheck/00_pkg_src/biovizBase/R/mold-method.R:162)
    mold,eSet: no visible global function definition for 'phenoData'
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/biovizBase/new/biovizBase.Rcheck/00_pkg_src/biovizBase/R/mold-method.R:5)
    mold,eSet: no visible global function definition for 'melt'
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/biovizBase/new/biovizBase.Rcheck/00_pkg_src/biovizBase/R/mold-method.R:8)
    mold,eSet: no visible global function definition for 'varLabels'
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/biovizBase/new/biovizBase.Rcheck/00_pkg_src/biovizBase/R/mold-method.R:8)
    Undefined global functions or variables:
      .circle.x .circle.y Chromosome end_location exprs from.x from.y melt
      pData phenoData start_location symbol to.x to.y varLabels xRleList
    ```

# blorr

Version: 0.2.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘lmtest’
    ```

# bsplus

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘methods’
      All declared Imports should be used.
    ```

# c14bazAAR

Version: 1.0.2

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 76 marked UTF-8 strings
    ```

# ccfa

Version: 1.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘doParallel’ ‘foreach’
      All declared Imports should be used.
    ```

# celaref

Version: 1.0.0

## In both

*   R CMD check timed out
    

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .github
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

# CGPfunctions

Version: 0.4

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘devtools’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘BSDA’, ‘janitor’
    ```

# circumplex

Version: 0.2.1

## In both

*   checking whether package ‘circumplex’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/circumplex/new/circumplex.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘circumplex’ ...
** package ‘circumplex’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/circumplex/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/circumplex/RcppArmadillo/include" -I/usr/local/include  -fopenmp  -fPIC  -Wall -g -O2 -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -c RcppExports.cpp -o RcppExports.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/circumplex/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/circumplex/RcppArmadillo/include" -I/usr/local/include  -fopenmp  -fPIC  -Wall -g -O2 -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -c circular.cpp -o circular.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/circumplex/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/circumplex/RcppArmadillo/include" -I/usr/local/include  -fopenmp  -fPIC  -Wall -g -O2 -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -c parameters.cpp -o parameters.o
clang: error: unsupported option '-fopenmp'clang: error: 
unsupported option '-fopenmp'
clang: error: unsupported option '-fopenmp'
make: *** [parameters.o] Error 1
make: *** Waiting for unfinished jobs....
make: *** [circular.o] Error 1
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘circumplex’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/circumplex/new/circumplex.Rcheck/circumplex’

```
### CRAN

```
* installing *source* package ‘circumplex’ ...
** package ‘circumplex’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/circumplex/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/circumplex/RcppArmadillo/include" -I/usr/local/include  -fopenmp  -fPIC  -Wall -g -O2 -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -c RcppExports.cpp -o RcppExports.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/circumplex/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/circumplex/RcppArmadillo/include" -I/usr/local/include  -fopenmp  -fPIC  -Wall -g -O2 -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -c circular.cpp -o circular.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/circumplex/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/circumplex/RcppArmadillo/include" -I/usr/local/include  -fopenmp  -fPIC  -Wall -g -O2 -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -c parameters.cpp -o parameters.o
clang: error: unsupported option '-fopenmp'clang: error: clang: error: unsupported option '-fopenmp'
unsupported option '-fopenmp'

make: *** [parameters.o] Error 1
make: *** Waiting for unfinished jobs....
make: *** [circular.o] Error 1
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘circumplex’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/circumplex/old/circumplex.Rcheck/circumplex’

```
# codebook

Version: 0.7.5

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘graphics’ ‘pander’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘mice’
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 65 marked UTF-8 strings
    ```

# codified

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘methods’ ‘readr’
      All declared Imports should be used.
    ```

# COMPASS

Version: 1.20.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 39-41 (SimpleCOMPASS.Rmd) 
    Error: processing vignette 'SimpleCOMPASS.Rmd' failed with diagnostics:
    there is no package called 'readxl'
    Execution halted
    ```

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .github
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘BiocStyle’ ‘rmarkdown’
      All declared Imports should be used.
    ':::' call which should be '::': ‘flowWorkspace:::.getNodeInd’
      See the note in ?`:::` about the use of this operator.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘_COMPASS_CellCounts’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/COMPASS/new/COMPASS.Rcheck/00_pkg_src/COMPASS/R/RcppExports.R:9)
    COMPASSfitToCountsTable: no visible binding for global variable
      ‘population’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/COMPASS/new/COMPASS.Rcheck/00_pkg_src/COMPASS/R/utils.R:193)
    COMPASSfitToCountsTable: no visible binding for global variable ‘Count’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/COMPASS/new/COMPASS.Rcheck/00_pkg_src/COMPASS/R/utils.R:193)
    COMPASSfitToCountsTable: no visible binding for global variable
      ‘population’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/COMPASS/new/COMPASS.Rcheck/00_pkg_src/COMPASS/R/utils.R:194)
    COMPASSfitToCountsTable: no visible binding for global variable ‘Count’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/COMPASS/new/COMPASS.Rcheck/00_pkg_src/COMPASS/R/utils.R:194)
    COMPASSfitToCountsTable: no visible binding for global variable ‘id’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/COMPASS/new/COMPASS.Rcheck/00_pkg_src/COMPASS/R/utils.R:200)
    COMPASSfitToCountsTable: no visible binding for global variable ‘id’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/COMPASS/new/COMPASS.Rcheck/00_pkg_src/COMPASS/R/utils.R:206)
    CellCounts_character: no visible binding for global variable
      ‘_COMPASS_CellCounts_character’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/COMPASS/new/COMPASS.Rcheck/00_pkg_src/COMPASS/R/RcppExports.R:5)
    Undefined global functions or variables:
      Count _COMPASS_CellCounts _COMPASS_CellCounts_character id population
    ```

*   checking for unstated dependencies in vignettes ... NOTE
    ```
    'library' or 'require' calls not declared from:
      ‘ggplot2’ ‘readxl’
    ```

# condformat

Version: 0.8.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > test_check("condformat")
      Unable to find any JVMs matching version "(null)".
      No Java runtime present, try --request to install.
      -- 1. Error: condformat2excel generates a file (@test_rendering.R#42)  ---------
      Please install the xlsx package in order to export to excel
      1: condformat2excel(condformat(head(iris, n = rows_to_write)), filename = filename) at testthat/test_rendering.R:42
      2: require_xlsx() at /Users/lionel/Desktop/rlang/revdep/checks.noindex/condformat/new/condformat.Rcheck/00_pkg_src/condformat/R/render_xlsx.R:19
      3: stop("Please install the xlsx package in order to export to excel") at /Users/lionel/Desktop/rlang/revdep/checks.noindex/condformat/new/condformat.Rcheck/00_pkg_src/condformat/R/render_xlsx.R:3
      
      == testthat results  ===========================================================
      OK: 126 SKIPPED: 0 FAILED: 1
      1. Error: condformat2excel generates a file (@test_rendering.R#42) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unable to find any JVMs matching version "(null)".
    No Java runtime present, try --request to install.
    Namespace in Imports field not imported from: ‘graphics’
      All declared Imports should be used.
    ```

# conflicted

Version: 1.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘memoise’
      All declared Imports should be used.
    ```

# customsteps

Version: 0.7.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘rlang’ ‘tidyselect’
      All declared Imports should be used.
    ```

# dabestr

Version: 0.1.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Warning: The `printer` argument is soft-deprecated as of rlang 0.3.0.
    This warning is displayed once per session.
    Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  :
      no font could be found for family "Rubik Medium"
    Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  :
      no font could be found for family "Rubik Medium"
    Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  :
      no font could be found for family "Rubik Medium"
    Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  :
      no font could be found for family "Work Sans Medium"
    Quitting from lines 72-103 (bootstrap-confidence-intervals.Rmd) 
    Error: processing vignette 'bootstrap-confidence-intervals.Rmd' failed with diagnostics:
    polygon edge not found
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        doc   4.5Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘grid’
      All declared Imports should be used.
    ```

# dartR

Version: 1.1.6

## Newly broken

*   checking examples ... ERROR
    ```
    ...
        3 EmmacFitzAllig EmmacMDBCond EmmacMDBSanf EmmacRussEube 
        4 EmmacBurdMist EmmacJohnWari EmmacRoss 
        6 EmmacCoopEulb 
        7 EmmacCoopCully 
        16 EmmacCoopAvin 
    
      Data retained for the unknown individual and remaining candidate source populations ( 1 or less loci with private alleles)
       EmmacBurnBara EmmacMaclGeor EmmacRichCasi EmmacTweeUki unknown 
    
    Completed gl.report.pa
    
    
    
    COMPUTING ASSIGNMENT BASED ON CONFIDENCE ENVELOPES
    
      Number of populations, including the unknown: 5 
      Number of dimensions with substantial eigenvalues: 7 
      Hard coded upper limit to dimensions: 8 
      User specified dimensions to retain: Not specified
        Dimension of confidence envelope set at 5 
    Performing a PCoA, individuals as entities, SNP loci as attributes
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘plotly’ ‘rgl’
      All declared Imports should be used.
    ```

# DChIPRep

Version: 1.12.0

## In both

*   checking whether package ‘DChIPRep’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/DChIPRep/new/DChIPRep.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘DChIPRep’ ...
** R
** data
*** moving datasets to lazyload DB
Warning: namespace ‘DChIPRep’ is not available and has been replaced
by .GlobalEnv when processing object ‘testData’
Warning: namespace ‘DChIPRep’ is not available and has been replaced
by .GlobalEnv when processing object ‘testData’
** exec
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘GO.db’
ERROR: lazy loading failed for package ‘DChIPRep’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/DChIPRep/new/DChIPRep.Rcheck/DChIPRep’

```
### CRAN

```
* installing *source* package ‘DChIPRep’ ...
** R
** data
*** moving datasets to lazyload DB
Warning: namespace ‘DChIPRep’ is not available and has been replaced
by .GlobalEnv when processing object ‘testData’
Warning: namespace ‘DChIPRep’ is not available and has been replaced
by .GlobalEnv when processing object ‘testData’
** exec
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘GO.db’
ERROR: lazy loading failed for package ‘DChIPRep’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/DChIPRep/old/DChIPRep.Rcheck/DChIPRep’

```
# DeclareDesign

Version: 0.12.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(DeclareDesign)
      Loading required package: randomizr
      Loading required package: fabricatr
      Loading required package: estimatr
      > 
      > test_check("DeclareDesign")
      ── 1. Failure: gam (@test-model.R#247)  ────────────────────────────────────────
      `expect_equal(ncol(draw_estimates(des)), 7)` did not produce any warnings.
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 457 SKIPPED: 5 FAILED: 1
      1. Failure: gam (@test-model.R#247) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# DEGreport

Version: 1.18.0

## In both

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .travis.yml
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Package listed in more than one of Depends, Imports, Suggests, Enhances:
      ‘knitr’
    A package should be listed in only one of these fields.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    degPlotCluster: no visible binding for global variable ‘cluster’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/DEGreport/new/DEGreport.Rcheck/00_pkg_src/DEGreport/R/clustering.R:44)
    degPlotWide : <anonymous>: no visible binding for global variable
      ‘count’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/DEGreport/new/DEGreport.Rcheck/00_pkg_src/DEGreport/R/genePlots.R:235-238)
    significants,TopTags: no visible binding for global variable ‘FDR’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/DEGreport/new/DEGreport.Rcheck/00_pkg_src/DEGreport/R/AllMethods.R:153-157)
    significants,TopTags: no visible binding for global variable ‘logFC’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/DEGreport/new/DEGreport.Rcheck/00_pkg_src/DEGreport/R/AllMethods.R:153-157)
    significants,list : <anonymous>: no visible binding for global variable
      ‘gene’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/DEGreport/new/DEGreport.Rcheck/00_pkg_src/DEGreport/R/AllMethods.R:248)
    Undefined global functions or variables:
      .x FDR base_mean boxplot cluster comp compare count counts covar desc
      enrichGO fdr gene genes itemConsensus k keys lm log2FoldChange log2fc
      logFC max_sd min_median n p.value r ratios rowMedians score simplify
      value_fc value_fdr x xend y yend
    Consider adding
      importFrom("graphics", "boxplot")
      importFrom("stats", "lm")
    to your NAMESPACE file.
    ```

# destiny

Version: 2.12.0

## In both

*   checking running R code from vignettes ...
    ```
    ...
      When tangling ‘Diffusion-Maps.ipynbmeta’:
    Error: Either IPython 3+ or Jupyter has to be installed, but neither could be called.
    Execution halted
    when running code in ‘DPT.ipynbmeta’
      ...
    
      When tangling ‘DPT.ipynbmeta’:
    Error: Either IPython 3+ or Jupyter has to be installed, but neither could be called.
    Execution halted
    when running code in ‘Global-Sigma.ipynbmeta’
      ...
    
      When tangling ‘Global-Sigma.ipynbmeta’:
    Error: Either IPython 3+ or Jupyter has to be installed, but neither could be called.
    Execution halted
    when running code in ‘tidyverse.ipynbmeta’
      ...
    
      When tangling ‘tidyverse.ipynbmeta’:
    Error: Either IPython 3+ or Jupyter has to be installed, but neither could be called.
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘rgl’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.5Mb
      sub-directories of 1Mb or more:
        doc   3.8Mb
        R     2.1Mb
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Package listed in more than one of Depends, Imports, Suggests, Enhances:
      ‘SingleCellExperiment’
    A package should be listed in only one of these fields.
    'LinkingTo' for ‘grDevices’ is unused as it has no 'include' directory
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘rgl’
    ```

*   checking Rd \usage sections ... NOTE
    ```
    S3 methods shown with full name in documentation object 'plot.DPT':
      ‘plot.DPT’
    
    S3 methods shown with full name in documentation object 'plot.DiffusionMap':
      ‘plot.DiffusionMap’
    
    The \usage entries for S3 methods should use the \method markup and
    not their full name.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking for unstated dependencies in vignettes ... NOTE
    ```
    '::' or ':::' imports not declared from:
      ‘gridExtra’ ‘viridis’
    'library' or 'require' calls not declared from:
      ‘base64enc’ ‘forcats’ ‘IRdisplay’ ‘IRkernel’ ‘readxl’ ‘repr’
      ‘tidyverse’
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error in re-building vignettes:
      ...
    Error: processing vignette 'Diffusion-Map-recap.ipynbmeta' failed with diagnostics:
    Either IPython 3+ or Jupyter has to be installed, but neither could be called.
    Execution halted
    ```

# detrendr

Version: 0.6.0

## In both

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# dexter

Version: 0.8.3

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    no column `person_id` provided, automatically generating unique person id's
    no column `person_id` provided, automatically generating unique person id's
    no column `person_id` provided, automatically generating unique person id's
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'Equating.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# dextergui

Version: 0.1.5

## In both

*   checking dependencies in R code ... NOTE
    ```
    Unexported objects imported by ':::' calls:
      ‘dexter:::get_resp_data’ ‘dexter:::qcolors’
      See the note in ?`:::` about the use of this operator.
    ```

# DiagrammeR

Version: 1.0.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.9Mb
      sub-directories of 1Mb or more:
        R             3.0Mb
        htmlwidgets   3.0Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

# dimRed

Version: 0.2.2

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘dimRed-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: PCA_L1-class
    > ### Title: Principal Component Analysis with L1 error.
    > ### Aliases: PCA_L1-class PCA_L1
    > 
    > ### ** Examples
    > 
    > dat <- loadDataSet("Iris")
    > 
    > ## using the S4 Class
    > pca_l1 <- PCA_L1()
    > emb <- pca_l1@fun(dat, pca_l1@stdpars)
    Error in chckpkg("pcaL1") : 
      require 'pcaL1' package, install it using install.packages('pcaL1')
    Calls: <Anonymous> -> chckpkg
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      The following object is masked from 'package:stats':
      
          embed
      
      The following object is masked from 'package:base':
      
          as.data.frame
      
      > 
      > test_check("dimRed", reporter = ListReporter)
      2019-01-02 15:50:53.109210: I tensorflow/core/platform/cpu_feature_guard.cc:141] Your CPU supports instructions that this TensorFlow binary was not compiled to use: AVX2 FMA
      Using TensorFlow backend.
      Error: Test failures
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘pcaL1’
    ```

# DisImpact

Version: 0.0.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘magrittr’
      All declared Imports should be used.
    ```

# dlookr

Version: 0.3.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        doc   4.2Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dbplyr’ ‘randomForest’
      All declared Imports should be used.
    ```

# dodgr

Version: 0.1.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    1 of 1 page converted in 0.422531 seconds
    
    Attaching package: 'magrittr'
    
    The following objects are masked from 'package:testthat':
    
        equals, is_less_than, not
    
    processing of PostScript specials is disabled (Ghostscript not found)
    pre-processing DVI file (format 2)
    processing page 1
      WARNING: 658 PostScript specials ignored. The resulting SVG might look wrong.
      page size: 29.6887pt x 17.2154pt (10.4344mm x 6.05054mm)
      page written to fig3-1.svg
    1 of 1 page converted in 0.422384 seconds
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'dodgr.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# dotwhisker

Version: 0.5.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
      ...
    Loading required package: ggplot2
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Warning: The `printer` argument is soft-deprecated as of rlang 0.3.0.
    This warning is displayed once per session.
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'dotwhisker-vignette.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# dplyr

Version: 0.7.8

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        R      2.1Mb
        libs   2.1Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4 marked UTF-8 strings
    ```

# dplyrAssist

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘tidyr’ ‘tidyverse’
      All declared Imports should be used.
    ```

# emuR

Version: 1.1.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    
    Attaching package: 'emuR'
    
    The following object is masked from 'package:base':
    
        norm
    
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'EQL.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  7.1Mb
      sub-directories of 1Mb or more:
        R         3.0Mb
        doc       1.2Mb
        extdata   1.5Mb
    ```

# enrichplot

Version: 1.2.0

## In both

*   checking whether package ‘enrichplot’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/enrichplot/new/enrichplot.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘org.Hs.eg.db’
    ```

## Installation

### Devel

```
* installing *source* package ‘enrichplot’ ...
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘GO.db’
ERROR: lazy loading failed for package ‘enrichplot’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/enrichplot/new/enrichplot.Rcheck/enrichplot’

```
### CRAN

```
* installing *source* package ‘enrichplot’ ...
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘GO.db’
ERROR: lazy loading failed for package ‘enrichplot’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/enrichplot/old/enrichplot.Rcheck/enrichplot’

```
# epitable

Version: 0.1.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘magrittr’ ‘readr’
      All declared Imports should be used.
    ```

# ergm

Version: 3.9.4

## In both

*   R CMD check timed out
    

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘Rmpi’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  7.1Mb
      sub-directories of 1Mb or more:
        R     4.0Mb
        doc   1.7Mb
    ```

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘tergm’, ‘ergm.count’, ‘networkDynamic’
    ```

# estimatr

Version: 0.14

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘texreg’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘texreg’
    ```

# factoextra

Version: 1.0.5

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘NbClust’
    ```

# filesstrings

Version: 3.0.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘dplyr’
    ```

# fingertipscharts

Version: 0.0.3

## In both

*   R CMD check timed out
    

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘mapproj’
      All declared Imports should be used.
    ```

# fishtree

Version: 0.1.0

## In both

*   R CMD check timed out
    

# fold

Version: 0.2.6

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘fold-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: boxplot.folded
    > ### Title: Boxplot Method for Folded
    > ### Aliases: boxplot.folded
    > 
    > ### ** Examples
    > 
    > data(eventsf)
    > boxplot(eventsf, SEX, WT, ref = 68)
    Error in get(fun, mode = "function", envir = parent.frame()) : 
      object 'boxplot_panel' of mode 'function' was not found
    Calls: boxplot ... <Anonymous> -> bwplot.formula -> %in% -> formals -> get
    Execution halted
    ```

# FoldGO

Version: 1.0.1

## In both

*   checking whether package ‘FoldGO’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/FoldGO/new/FoldGO.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘FoldGO’ ...
** R
** data
*** moving datasets to lazyload DB
Warning: namespace ‘topGO’ is not available and has been replaced
by .GlobalEnv when processing object ‘down_annotobj’
Warning: namespace ‘topGO’ is not available and has been replaced
by .GlobalEnv when processing object ‘down_annotobj’
Warning: namespace ‘topGO’ is not available and has been replaced
by .GlobalEnv when processing object ‘up_annotobj’
Warning: namespace ‘topGO’ is not available and has been replaced
by .GlobalEnv when processing object ‘up_annotobj’
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘GO.db’
ERROR: lazy loading failed for package ‘FoldGO’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/FoldGO/new/FoldGO.Rcheck/FoldGO’

```
### CRAN

```
* installing *source* package ‘FoldGO’ ...
** R
** data
*** moving datasets to lazyload DB
Warning: namespace ‘topGO’ is not available and has been replaced
by .GlobalEnv when processing object ‘down_annotobj’
Warning: namespace ‘topGO’ is not available and has been replaced
by .GlobalEnv when processing object ‘down_annotobj’
Warning: namespace ‘topGO’ is not available and has been replaced
by .GlobalEnv when processing object ‘up_annotobj’
Warning: namespace ‘topGO’ is not available and has been replaced
by .GlobalEnv when processing object ‘up_annotobj’
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘GO.db’
ERROR: lazy loading failed for package ‘FoldGO’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/FoldGO/old/FoldGO.Rcheck/FoldGO’

```
# forecastHybrid

Version: 4.1.16

## Newly broken

*   R CMD check timed out
    

# forestmangr

Version: 0.9.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘covr’ ‘curl’ ‘graphics’ ‘htmltools’ ‘psych’ ‘rmarkdown’ ‘scales’
      All declared Imports should be used.
    ```

# fredr

Version: 1.0.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

# gestalt

Version: 0.1.5

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘gestalt-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: compose
    > ### Title: Compose Functions
    > ### Aliases: compose %>>>%
    > 
    > ### ** Examples
    > 
    > # Functions are applied in the order in which they are listed
    > inv <- partial(`/`, 1)  # reciprocal
    > f0 <- compose(abs, log, inv)
    > stopifnot(all.equal(f0(-2), 1 / log(abs(-2))))
    Error in all.equal(f0(-2), 1/log(abs(-2))) : 
      Can't supply both `e1` and `.x` to binary operator
    Calls: stopifnot ... tryCatch -> tryCatchList -> tryCatchOne -> <Anonymous>
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      2/19 mismatches
      x[9]: "  function(e2, .x = ^1, .y = e2) {"
      y[9]: "  function(.y) {"
      
      x[10]: "    (^1) + e2"
      y[10]: "    (^1) + .y"
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 835 SKIPPED: 0 FAILED: 3
      1. Failure: departial() of a partial function is the closure of the original function (@test-partial.R#573) 
      2. Failure: departial() of a partial function is the closure of the original function (@test-partial.R#574) 
      3. Failure: composition of functions shows composite functions (@test-print.R#46) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# GetTDData

Version: 1.3.1

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 17-21 (gtdd-vignette_YieldCurve.Rmd) 
    Error: processing vignette 'gtdd-vignette_YieldCurve.Rmd' failed with diagnostics:
    failed to load HTTP resource
    Execution halted
    ```

# gfer

Version: 0.1.10

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyr’
      All declared Imports should be used.
    ```

# GGally

Version: 1.4.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Unable to find any JVMs matching version "(null)".
    No Java runtime present, try --request to install.
    ```

# ggcyto

Version: 1.10.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.6Mb
      sub-directories of 1Mb or more:
        doc   5.5Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘scales’
      All declared Imports should be used.
    ':::' call which should be '::': ‘flowWorkspace:::isNegated’
      See the note in ?`:::` about the use of this operator.
    Unexported objects imported by ':::' calls:
      ‘flowWorkspace:::.mergeGates’ ‘flowWorkspace:::compact’
      ‘flowWorkspace:::fix_y_axis’ ‘ggplot2:::+.gg’ ‘ggplot2:::add_group’
      ‘ggplot2:::as_gg_data_frame’ ‘ggplot2:::check_aesthetics’
      ‘ggplot2:::is.waive’ ‘ggplot2:::is_calculated_aes’
      ‘ggplot2:::make_labels’ ‘ggplot2:::make_scale’ ‘ggplot2:::plot_clone’
      ‘ggplot2:::print.ggplot’ ‘ggplot2:::scales_add_defaults’
      ‘ggplot2:::scales_list’ ‘ggplot2:::update_theme’
      See the note in ?`:::` about the use of this operator.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    ggcyto.flowSet: no visible binding for global variable ‘name’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/ggcyto/new/ggcyto.Rcheck/00_pkg_src/ggcyto/R/ggcyto_flowSet.R:61)
    ggcyto.flowSet: no visible binding for global variable ‘axis’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/ggcyto/new/ggcyto.Rcheck/00_pkg_src/ggcyto/R/ggcyto_flowSet.R:63-64)
    ggcyto.flowSet: no visible binding for global variable ‘name’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/ggcyto/new/ggcyto.Rcheck/00_pkg_src/ggcyto/R/ggcyto_flowSet.R:63-64)
    ggcyto.ncdfFlowList: no visible global function definition for
      ‘getS3method’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/ggcyto/new/ggcyto.Rcheck/00_pkg_src/ggcyto/R/ggcyto_flowSet.R:103)
    ggcyto_arrange: no visible binding for global variable ‘name’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/ggcyto/new/ggcyto.Rcheck/00_pkg_src/ggcyto/R/ggcyto_GatingLayout.R:42)
    ggcyto_arrange: no visible binding for global variable ‘name’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/ggcyto/new/ggcyto.Rcheck/00_pkg_src/ggcyto/R/ggcyto_GatingLayout.R:47)
    Undefined global functions or variables:
      approx axis density desc dist getS3method gray modifyList name
    Consider adding
      importFrom("grDevices", "gray")
      importFrom("graphics", "axis")
      importFrom("stats", "approx", "density", "dist")
      importFrom("utils", "getS3method", "modifyList")
    to your NAMESPACE file.
    ```

# ggedit

Version: 0.3.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘magrittr’
      All declared Imports should be used.
    ```

# ggeffects

Version: 0.7.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘ordinal’
    ```

# ggformula

Version: 0.9.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.0Mb
      sub-directories of 1Mb or more:
        R     3.1Mb
        doc   2.7Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyr’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘quantreg’
    ```

# gginnards

Version: 0.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘grid’ ‘tibble’
      All declared Imports should be used.
    ```

# ggplot2

Version: 3.1.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.3Mb
      sub-directories of 1Mb or more:
        R     3.8Mb
        doc   1.8Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘mgcv’ ‘reshape2’ ‘viridisLite’
      All declared Imports should be used.
    ```

# ggpmisc

Version: 0.3.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘gginnards’
    ```

# ggpol

Version: 0.0.4

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘grDevices’
      All declared Imports should be used.
    ```

# ggquickeda

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DT’ ‘Formula’ ‘Hmisc’ ‘colourpicker’ ‘dplyr’ ‘ggpmisc’ ‘ggrepel’
      ‘grDevices’ ‘gridExtra’ ‘lazyeval’ ‘markdown’ ‘plotly’ ‘quantreg’
      ‘rlang’ ‘shinyjs’ ‘table1’ ‘tidyr’
      All declared Imports should be used.
    ```

# ggspatial

Version: 1.0.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘reshape2’ ‘rosm’
      All declared Imports should be used.
    ```

# ggstatsplot

Version: 0.0.7

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        'x' must at least have 2 elements
      Warning: Proportion test will not be run because it requires x to have at least 
      2 levels with non-zero frequencies.
      Error in chisq.test(counts, p = expProps) : 
        'x' must at least have 2 elements
      ── 1. Failure: parametric t-test works (between-subjects without NAs) (@test_sub
      `using_function1` not equal to `results1`.
      target, current do not match when deparsed
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 654 SKIPPED: 0 FAILED: 1
      1. Failure: parametric t-test works (between-subjects without NAs) (@test_subtitle_t_parametric.R#63) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   R CMD check timed out
    

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘glmmTMB’
      All declared Imports should be used.
    ```

# ggthemes

Version: 4.0.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 138 marked UTF-8 strings
    ```

# ggtree

Version: 1.14.4

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'ggtree.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 10.9Mb
      sub-directories of 1Mb or more:
        R          2.0Mb
        doc        4.9Mb
        examples   3.7Mb
    ```

# glmSparseNet

Version: 1.0.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Quitting from lines 28-43 (example_brca_logistic.Rmd) 
    Error: processing vignette 'example_brca_logistic.Rmd' failed with diagnostics:
    there is no package called 'curatedTCGAData'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘curatedTCGAData’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        doc   5.2Mb
    ```

# grattan

Version: 1.7.0.0

## In both

*   R CMD check timed out
    

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘taxstats’ ‘taxstats1516’
    ```

# gravity

Version: 0.9.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    
    Attaching package: 'gravity'
    
    The following object is masked from 'package:stats':
    
        nls
    
    Warning: The `printer` argument is soft-deprecated as of rlang 0.3.0.
    This warning is displayed once per session.
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'crash-course-on-gravity-models.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# helixvis

Version: 1.0.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'intro.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# hpiR

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘knitr’
      All declared Imports should be used.
    ```

# huxtable

Version: 4.3.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        R     2.1Mb
        doc   2.9Mb
    ```

# iadf

Version: 0.1.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'falsering-proportion.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# idealstan

Version: 0.5.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.2Mb
      sub-directories of 1Mb or more:
        libs   4.1Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# implyr

Version: 0.2.4

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Loading required package: rJava
      Unable to find any JVMs matching version "(null)".
      No Java runtime present, try --request to install.
      Error: package or namespace load failed for 'rJava':
       .onLoad failed in loadNamespace() for 'rJava', details:
        call: dyn.load(file, DLLpath = DLLpath, ...)
        error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/implyr/rJava/libs/rJava.so':
        dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/implyr/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
        Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/implyr/rJava/libs/rJava.so
        Reason: image not found
      Error: package 'rJava' could not be loaded
      In addition: Warning message:
      In system("/usr/libexec/java_home", intern = TRUE) :
        running command '/usr/libexec/java_home' had status 1
      Execution halted
    ```

# ipumsr

Version: 0.3.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘R6’
      All declared Imports should be used.
    ```

# irteQ

Version: 1.0.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘rpf’, ‘plink’
    ```

# jpmesh

Version: 1.1.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 176 marked UTF-8 strings
    ```

# jpndistrict

Version: 0.3.2

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 502 marked UTF-8 strings
    ```

# kokudosuuchi

Version: 0.4.2

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 52458 marked UTF-8 strings
    ```

# linguisticsdown

Version: 1.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyr’
      All declared Imports should be used.
    ```

# LipidMS

Version: 1.0.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.4Mb
      sub-directories of 1Mb or more:
        data   5.8Mb
        R      2.0Mb
    ```

# listarrays

Version: 0.1.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘keras’
    ```

# loose.rock

Version: 1.0.9

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      cannot read from connection
      1: coding.genes(verbose = FALSE) at testthat/test-coding.genes.R:4
      2: utils::read.table(url("ftp://ftp.ncbi.nih.gov/pub/CCDS/current_human/CCDS.current.txt"), 
             sep = "\t", header = TRUE, comment.char = "|", stringsAsFactors = FALSE) at /Users/lionel/Desktop/rlang/revdep/checks.noindex/loose.rock/new/loose.rock.Rcheck/00_pkg_src/loose.rock/R/coding.genes.R:34
      3: scan(file = file, what = what, sep = sep, quote = quote, dec = dec, nmax = nrows, 
             skip = 0, na.strings = na.strings, quiet = TRUE, fill = fill, strip.white = strip.white, 
             blank.lines.skip = blank.lines.skip, multi.line = FALSE, comment.char = comment.char, 
             allowEscapes = allowEscapes, flush = flush, encoding = encoding, skipNul = skipNul)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 148 SKIPPED: 0 FAILED: 1
      1. Error: coding genes retrieves some genes (@test-coding.genes.R#4) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    Attaching package: 'dplyr'
    
    The following object is masked from 'package:testthat':
    
        matches
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Warning in scan(file = file, what = what, sep = sep, quote = quote, dec = dec,  :
      URL 'ftp://ftp.ncbi.nih.gov/pub/CCDS/current_human/CCDS.current.txt': status was 'Transferred a partial file'
    Quitting from lines 75-80 (Overview.Rmd) 
    Error: processing vignette 'Overview.Rmd' failed with diagnostics:
    cannot read from connection
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘futile.options’ ‘ggfortify’ ‘grDevices’ ‘stats’
      All declared Imports should be used.
    ```

# mafs

Version: 0.0.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘cmprsk’ ‘colorspace’ ‘etm’ ‘fracdiff’ ‘gtable’ ‘munsell’ ‘numDeriv’
      ‘plyr’ ‘quadprog’ ‘Rcpp’ ‘scales’ ‘timeDate’ ‘tseries’ ‘zoo’
      All declared Imports should be used.
    ```

# malariaAtlas

Version: 0.0.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘grid’
      All declared Imports should be used.
    ```

# MANOVA.RM

Version: 0.3.1

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘RGtk2’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘nparLD’
    ```

# markmyassignment

Version: 0.8.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘lazyeval’
      All declared Imports should be used.
    ```

# memery

Version: 0.5.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘colourpicker’ ‘shinyBS’ ‘shinycssloaders’
      All declared Imports should be used.
    ```

# metagene

Version: 2.14.0

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘EnsDb.Hsapiens.v86’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# miWQS

Version: 0.0.9

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘graphics’
      All declared Imports should be used.
    ```

# MlBayesOpt

Version: 0.3.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘data.table’ ‘foreach’
      All declared Imports should be used.
    ```

# mlflow

Version: 0.8.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘aws.s3’
      All declared Imports should be used.
    ```

# mlr

Version: 2.13

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.5Mb
      sub-directories of 1Mb or more:
        data   2.3Mb
        R      5.1Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unable to find any JVMs matching version "(null)".
    No Java runtime present, try --request to install.
    Unable to find any JVMs matching version "(null)".
    No Java runtime present, try --request to install.
    Unable to find any JVMs matching version "(null)".
    No Java runtime present, try --request to install.
    ```

# modelr

Version: 0.1.2

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘lme4’, ‘rstanarm’
    ```

# moonBook

Version: 0.2.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘purrr’
      All declared Imports should be used.
    ```

# MortalityLaws

Version: 1.7.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'Installation.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# MPTmultiverse

Version: 0.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      3: getExportedValue(pkg, name)
      4: asNamespace(ns)
      5: getNamespace(ns)
      6: tryCatch(loadNamespace(name), error = function(e) stop(e))
      7: tryCatchList(expr, classes, parentenv, handlers)
      8: tryCatchOne(expr, names, parentenv, handlers[[1L]])
      9: value[[3L]](cond)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 0 SKIPPED: 3 FAILED: 2
      1. Error: No-pooling approaches work (@test-mptinr.R#23) 
      2. Error: Complete-pooling approaches work (@test-mptinr.R#164) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 57-80 (introduction-bayen_kuhlmann_2011.rmd) 
    Error: processing vignette 'introduction-bayen_kuhlmann_2011.rmd' failed with diagnostics:
    .onLoad failed in loadNamespace() for 'rjags', details:
      call: dyn.load(file, DLLpath = DLLpath, ...)
      error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/MPTmultiverse/rjags/libs/rjags.so':
      dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/MPTmultiverse/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
      Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/MPTmultiverse/rjags/libs/rjags.so
      Reason: image not found
    Execution halted
    ```

# msigdbr

Version: 6.2.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        R   5.1Mb
    ```

# MultiAssayExperiment

Version: 1.8.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Unexported objects imported by ':::' calls:
      ‘BiocGenerics:::replaceSlots’ ‘S4Vectors:::selectSome’
      See the note in ?`:::` about the use of this operator.
    ```

# nandb

Version: 2.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘MASS’ ‘stats’
      All declared Imports should be used.
    ```

# ncappc

Version: 0.3.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘bookdown’
      All declared Imports should be used.
    ```

# ndexr

Version: 1.4.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [timeStamp]
      2019-01-02 7:00:17,338
      1: ndex_network_get_metadata(con, uuid) at testthat/test_04_network_aspects_and_metadata.r:39
      2: ndex_rest_GET(ndexcon, route) at /Users/lionel/Desktop/rlang/revdep/checks.noindex/ndexr/new/ndexr.Rcheck/00_pkg_src/ndexr/R/ndex_networks_aspects_and_metadata.r:49
      3: ndex_helper_httpResponseHandler(response, paste("GET: [", url, "]"), ndexcon$verbose) at /Users/lionel/Desktop/rlang/revdep/checks.noindex/ndexr/new/ndexr.Rcheck/00_pkg_src/ndexr/R/ndex_connect.r:204
      4: stop(paste(description, "Some internal server error occurred (500):", "\n[errorCode]", 
             error_content$errorCode, "\n[message]", error_content$message, "\n[stackTrace]", 
             error_content$stackTrace, "\n[timeStamp]", error_content$timeStamp, "", sep = "\n")) at /Users/lionel/Desktop/rlang/revdep/checks.noindex/ndexr/new/ndexr.Rcheck/00_pkg_src/ndexr/R/ndex_helper.r:177
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 140 SKIPPED: 0 FAILED: 1
      1. Error: Get network meta-data (ndex_network_get_metadata) (@test_04_network_aspects_and_metadata.r#39) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Deprecated license: BSD
    ```

*   checking dependencies in R code ... NOTE
    ```
    Package in Depends field not imported from: ‘igraph’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    Unexported object imported by a ':::' call: ‘httr:::default_ua’
      See the note in ?`:::` about the use of this operator.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    rcx_toRCXgraph: no visible global function definition for ‘E’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/ndexr/new/ndexr.Rcheck/00_pkg_src/ndexr/R/ndex_RCXgraph.r:117)
    rcx_toRCXgraph: no visible global function definition for ‘E<-’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/ndexr/new/ndexr.Rcheck/00_pkg_src/ndexr/R/ndex_RCXgraph.r:117)
    rcxgraph_fromRCX: no visible global function definition for ‘V’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/ndexr/new/ndexr.Rcheck/00_pkg_src/ndexr/R/ndex_RCXgraph.r:116)
    rcxgraph_fromRCX: no visible global function definition for ‘V<-’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/ndexr/new/ndexr.Rcheck/00_pkg_src/ndexr/R/ndex_RCXgraph.r:116)
    rcxgraph_fromRCX: no visible global function definition for ‘E’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/ndexr/new/ndexr.Rcheck/00_pkg_src/ndexr/R/ndex_RCXgraph.r:117)
    rcxgraph_fromRCX: no visible global function definition for ‘E<-’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/ndexr/new/ndexr.Rcheck/00_pkg_src/ndexr/R/ndex_RCXgraph.r:117)
    rcxgraph_toRCX: no visible binding for global variable ‘po’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/ndexr/new/ndexr.Rcheck/00_pkg_src/ndexr/R/ndex_RCXgraph.r:268)
    rcxgraph_toRCX: no visible binding for global variable ‘po’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/ndexr/new/ndexr.Rcheck/00_pkg_src/ndexr/R/ndex_RCXgraph.r:294)
    Undefined global functions or variables:
      E E<- packageVersion po tail V V<-
    Consider adding
      importFrom("utils", "packageVersion", "tail")
    to your NAMESPACE file.
    ```

# NetworkChange

Version: 0.4

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘plyr’
      All declared Imports should be used.
    ```

# nullabor

Version: 0.3.5

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘forecast’ ‘rlang’ ‘tidyverse’ ‘tsibble’
      All declared Imports should be used.
    ```

# oec

Version: 2.7.8

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘readr’
      All declared Imports should be used.
    ```

# OMICsPCA

Version: 1.0.0

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘OMICsPCAdata’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# openair

Version: 2.6-1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        R   3.0Mb
    ```

# openVA

Version: 1.0.7

## In both

*   checking whether package ‘openVA’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/openVA/new/openVA.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘openVA’ ...
** package ‘openVA’ successfully unpacked and MD5 sums checked
** R
** inst
** byte-compile and prepare package for lazy loading
Unable to find any JVMs matching version "(null)".
No Java runtime present, try --request to install.
Warning in system("/usr/libexec/java_home", intern = TRUE) :
  running command '/usr/libexec/java_home' had status 1
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/openVA/rJava/libs/rJava.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/openVA/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/openVA/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘openVA’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/openVA/new/openVA.Rcheck/openVA’

```
### CRAN

```
* installing *source* package ‘openVA’ ...
** package ‘openVA’ successfully unpacked and MD5 sums checked
** R
** inst
** byte-compile and prepare package for lazy loading
Unable to find any JVMs matching version "(null)".
No Java runtime present, try --request to install.
Warning in system("/usr/libexec/java_home", intern = TRUE) :
  running command '/usr/libexec/java_home' had status 1
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/openVA/rJava/libs/rJava.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/openVA/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/openVA/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘openVA’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/openVA/old/openVA.Rcheck/openVA’

```
# optiSel

Version: 2.0.2

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'ocs-vignette.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# paletteer

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘jcolors’ ‘oompaBase’ ‘palr’ ‘pals’ ‘viridisLite’
      All declared Imports should be used.
    ```

# particles

Version: 0.2.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# pcaExplorer

Version: 2.8.0

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘GO.db’
    
    Packages suggested but not available for checking: ‘airway’ ‘org.Hs.eg.db’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# perturbatr

Version: 1.2.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    convert: profile 'icc': 'RGB ': RGB color space not permitted on grayscale PNG `perturbatr_files/figure-html/unnamed-chunk-7-1.png' @ warning/png.c/MagickPNGWarningHandler/1656.
    Fitting hierarchical model
    Only taking largest connected component to ensure ergodicity.
    Diffusion using Markov random walks.
    Correcting for hub degrees.
    normalizing column vectors!
    normalizing column vectors!
    convert: profile 'icc': 'RGB ': RGB color space not permitted on grayscale PNG `perturbatr_files/figure-html/unnamed-chunk-14-1.png' @ warning/png.c/MagickPNGWarningHandler/1656.
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'perturbatr.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        data   3.1Mb
    ```

# Pi

Version: 1.10.0

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘XGR’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# pivot

Version: 18.4.17

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘odbc’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘colorspace’ ‘lubridate’
      All declared Imports should be used.
    ```

# PKPDmisc

Version: 2.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘purrr’
      All declared Imports should be used.
    ```

# plotGrouper

Version: 1.0.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    gplot: no visible binding for global variable ‘max_value’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/plotGrouper/new/plotGrouper.Rcheck/00_pkg_src/plotGrouper/R/gplot.R:360-395)
    gplot: no visible binding for global variable ‘max_error’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/plotGrouper/new/plotGrouper.Rcheck/00_pkg_src/plotGrouper/R/gplot.R:360-395)
    Undefined global functions or variables:
      max_error max_value
    ```

# plotly

Version: 4.8.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.1Mb
      sub-directories of 1Mb or more:
        R             2.3Mb
        htmlwidgets   3.1Mb
    ```

# pmatch

Version: 0.1.4

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(pmatch)
      > 
      > test_check("pmatch")
      ── 1. Failure: we can transform a function that contains a call to cases (@test-
      tailr::can_loop_transform(llength) isn't true.
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 168 SKIPPED: 0 FAILED: 1
      1. Failure: we can transform a function that contains a call to cases (@test-transforms.R#62) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# poppr

Version: 2.8.1

## In both

*   checking whether package ‘poppr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/poppr/new/poppr.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘poppr’ ...
** package ‘poppr’ successfully unpacked and MD5 sums checked
** libs
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include  -fopenmp -fPIC  -mtune=core2  -O3 -c adjust_missing.c -o adjust_missing.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include  -fopenmp -fPIC  -mtune=core2  -O3 -c bitwise_distance.c -o bitwise_distance.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include  -fopenmp -fPIC  -mtune=core2  -O3 -c init.c -o init.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include  -fopenmp -fPIC  -mtune=core2  -O3 -c mlg_clustering.c -o mlg_clustering.o
clang: clang: error: errorunsupported option '-fopenmp': 
unsupported option '-fopenmp'clang: error: unsupported option '-fopenmp'

clang: error: unsupported option '-fopenmp'
make: *** [mlg_clustering.o] Error 1
make: *** Waiting for unfinished jobs....
make: *** [init.o] Error 1
make: *** [bitwise_distance.o] Error 1
make: *** [adjust_missing.o] Error 1
ERROR: compilation failed for package ‘poppr’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/poppr/new/poppr.Rcheck/poppr’

```
### CRAN

```
* installing *source* package ‘poppr’ ...
** package ‘poppr’ successfully unpacked and MD5 sums checked
** libs
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include  -fopenmp -fPIC  -mtune=core2  -O3 -c adjust_missing.c -o adjust_missing.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include  -fopenmp -fPIC  -mtune=core2  -O3 -c bitwise_distance.c -o bitwise_distance.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include  -fopenmp -fPIC  -mtune=core2  -O3 -c init.c -o init.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include  -fopenmp -fPIC  -mtune=core2  -O3 -c mlg_clustering.c -o mlg_clustering.o
clang: error: unsupported option '-fopenmp'clangclang
: error: clangunsupported option '-fopenmp': error: : unsupported option '-fopenmp'
error
: unsupported option '-fopenmp'
make: *** [mlg_clustering.o] Error 1
make: *** Waiting for unfinished jobs....
make: *** [init.o] Error 1
make: *** [bitwise_distance.o] Error 1
make: *** [adjust_missing.o] Error 1
ERROR: compilation failed for package ‘poppr’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/poppr/old/poppr.Rcheck/poppr’

```
# portalr

Version: 0.1.4

## In both

*   R CMD check timed out
    

# prisonbrief

Version: 0.1.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2 marked UTF-8 strings
    ```

# processanimateR

Version: 1.0.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.2Mb
      sub-directories of 1Mb or more:
        doc           6.5Mb
        help          2.1Mb
        htmlwidgets   2.5Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘zoo’
      All declared Imports should be used.
    ```

# processmapR

Version: 0.3.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘viridis’
      All declared Imports should be used.
    ```

# promises

Version: 1.0.1

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘purrr’
    ```

# proustr

Version: 0.2.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 20105 marked UTF-8 strings
    ```

# psychmeta

Version: 2.2.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'overview.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.8Mb
      sub-directories of 1Mb or more:
        R   7.1Mb
    ```

# purrr

Version: 0.2.5

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
          is_null
      
      > 
      > test_check("purrr")
      ── 1. Failure: creates simple call (@test-map.R#10)  ───────────────────────────
      `out` not identical to quote(.f(.x[[i]], ...)).
      Objects equal but not identical
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 393 SKIPPED: 0 FAILED: 1
      1. Failure: creates simple call (@test-map.R#10) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# qsort

Version: 0.2.3

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'qsort-vignette.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 6 marked UTF-8 strings
    ```

# quickReg

Version: 1.5.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘psych’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘PredictABEL’
    ```

# radiant.data

Version: 0.9.7

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘shinyFiles’
      All declared Imports should be used.
    ```

# rclimateca

Version: 1.0.2

## In both

*   R CMD check timed out
    

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 24 marked UTF-8 strings
    ```

# rdomains

Version: 0.1.7

## In both

*   R CMD check timed out
    

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rlang’
      All declared Imports should be used.
    ```

# Rdrools

Version: 1.1.1

## In both

*   checking whether package ‘Rdrools’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/Rdrools/new/Rdrools.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘Rdrools’ ...
** package ‘Rdrools’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Unable to find any JVMs matching version "(null)".
No Java runtime present, try --request to install.
Warning in system("/usr/libexec/java_home", intern = TRUE) :
  running command '/usr/libexec/java_home' had status 1
Error: package or namespace load failed for ‘rJava’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/Rdrools/rJava/libs/rJava.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/Rdrools/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/Rdrools/rJava/libs/rJava.so
  Reason: image not found
Error : package ‘rJava’ could not be loaded
ERROR: lazy loading failed for package ‘Rdrools’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/Rdrools/new/Rdrools.Rcheck/Rdrools’

```
### CRAN

```
* installing *source* package ‘Rdrools’ ...
** package ‘Rdrools’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Unable to find any JVMs matching version "(null)".
No Java runtime present, try --request to install.
Warning in system("/usr/libexec/java_home", intern = TRUE) :
  running command '/usr/libexec/java_home' had status 1
Error: package or namespace load failed for ‘rJava’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/Rdrools/rJava/libs/rJava.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/Rdrools/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/Rdrools/rJava/libs/rJava.so
  Reason: image not found
Error : package ‘rJava’ could not be loaded
ERROR: lazy loading failed for package ‘Rdrools’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/Rdrools/old/Rdrools.Rcheck/Rdrools’

```
# recipes

Version: 0.1.4

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      14: dimRed::FastICA at /Users/lionel/Desktop/rlang/revdep/checks.noindex/recipes/new/recipes.Rcheck/00_pkg_src/recipes/R/ica.R:158
      15: getExportedValue(pkg, name)
      16: asNamespace(ns)
      17: getNamespace(ns)
      18: tryCatch(loadNamespace(name), error = function(e) stop(e))
      19: tryCatchList(expr, classes, parentenv, handlers)
      20: tryCatchOne(expr, names, parentenv, handlers[[1L]])
      21: value[[3L]](cond)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 1119 SKIPPED: 9 FAILED: 1
      1. Error: printing (@test_ica.R#127) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘dimRed’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘RcppRoll’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘dimRed’
    ```

# redcapAPI

Version: 2.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DBI’ ‘readr’
      All declared Imports should be used.
    ```

# reinforcelearn

Version: 0.1.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Episode 482 finished after 3 steps with a return of -3
    Episode 483 finished after 1 steps with a return of -1
    Episode 484 finished after 2 steps with a return of -2
    Episode 485 finished after 2 steps with a return of -2
    Episode 486 finished after 2 steps with a return of -2
    Episode 487 finished after 1 steps with a return of -1
    Episode 488 finished after 3 steps with a return of -3
    Episode 489 finished after 3 steps with a return of -3
    Episode 490 finished after 1 steps with a return of -1
    Episode 491 finished after 1 steps with a return of -1
    Episode 492 finished after 2 steps with a return of -2
    Episode 493 finished after 5 steps with a return of -5
    Episode 494 finished after 2 steps with a return of -2
    Episode 495 finished after 2 steps with a return of -2
    Episode 496 finished after 2 steps with a return of -2
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'agents.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘nnet’ ‘purrr’ ‘R6’
      All declared Imports should be used.
    ```

# replyr

Version: 0.9.8

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘rquery’
    ```

# reprex

Version: 0.2.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tools’
      All declared Imports should be used.
    ```

# repurrrsive

Version: 0.1.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 8 marked UTF-8 strings
    ```

# rfishbase

Version: 3.0.0

## In both

*   R CMD check timed out
    

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 44 marked UTF-8 strings
    ```

# rhierbaps

Version: 1.1.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
      Failed to tidy R code in chunk 'unnamed-chunk-11'. Reason:
    Error in loadNamespace(name) : there is no package called 'formatR'
    
    Warning in block_exec(params) :
      Failed to tidy R code in chunk 'unnamed-chunk-12'. Reason:
    Error in loadNamespace(name) : there is no package called 'formatR'
    
    Warning in block_exec(params) :
      Failed to tidy R code in chunk 'unnamed-chunk-13'. Reason:
    Error in loadNamespace(name) : there is no package called 'formatR'
    
    Warning in block_exec(params) :
      Failed to tidy R code in chunk 'unnamed-chunk-14'. Reason:
    Error in loadNamespace(name) : there is no package called 'formatR'
    
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'introduction.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# rhmmer

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# riingo

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rlang’
      All declared Imports should be used.
    ```

# rmapzen

Version: 0.4.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 31 marked UTF-8 strings
    ```

# rmsfuns

Version: 0.0.0.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Missing or unexported object: ‘base::shell.exec’
    ```

# RPyGeo

Version: 1.0.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'RPyGeo.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# RSDA

Version: 2.0.8

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘randomcoloR’
      All declared Imports should be used.
    ```

# RtutoR

Version: 1.2

## In both

*   checking whether package ‘RtutoR’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/RtutoR/new/RtutoR.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘RtutoR’ ...
** package ‘RtutoR’ successfully unpacked and MD5 sums checked
** R
** inst
** byte-compile and prepare package for lazy loading
Unable to find any JVMs matching version "(null)".
No Java runtime present, try --request to install.
Warning in system("/usr/libexec/java_home", intern = TRUE) :
  running command '/usr/libexec/java_home' had status 1
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/RtutoR/rJava/libs/rJava.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/RtutoR/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/RtutoR/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘RtutoR’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/RtutoR/new/RtutoR.Rcheck/RtutoR’

```
### CRAN

```
* installing *source* package ‘RtutoR’ ...
** package ‘RtutoR’ successfully unpacked and MD5 sums checked
** R
** inst
** byte-compile and prepare package for lazy loading
Unable to find any JVMs matching version "(null)".
No Java runtime present, try --request to install.
Warning in system("/usr/libexec/java_home", intern = TRUE) :
  running command '/usr/libexec/java_home' had status 1
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/RtutoR/rJava/libs/rJava.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/RtutoR/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/RtutoR/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘RtutoR’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/RtutoR/old/RtutoR.Rcheck/RtutoR’

```
# rubias

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ggplot2’
      All declared Imports should be used.
    ```

# sdcTable

Version: 0.25

## In both

*   checking whether package ‘sdcTable’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/sdcTable/new/sdcTable.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘sdcTable’ ...
** package ‘sdcTable’ successfully unpacked and MD5 sums checked
GLPK is not available
ERROR: configuration failed for package ‘sdcTable’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/sdcTable/new/sdcTable.Rcheck/sdcTable’

```
### CRAN

```
* installing *source* package ‘sdcTable’ ...
** package ‘sdcTable’ successfully unpacked and MD5 sums checked
GLPK is not available
ERROR: configuration failed for package ‘sdcTable’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/sdcTable/old/sdcTable.Rcheck/sdcTable’

```
# sevenC

Version: 1.2.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
        anyMissing, rowMedians
    
    Loading required package: BiocParallel
    
    Attaching package: 'DelayedArray'
    
    The following objects are masked from 'package:matrixStats':
    
        colMaxs, colMins, colRanges, rowMaxs, rowMins, rowRanges
    
    The following objects are masked from 'package:base':
    
        aperm, apply
    
    convert: profile 'icc': 'RGB ': RGB color space not permitted on grayscale PNG `sevenC_files/figure-html/unnamed-chunk-22-1.png' @ warning/png.c/MagickPNGWarningHandler/1656.
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'sevenC.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported object imported by a ':::' call: ‘GenomicRanges:::get_out_of_bound_index’
      See the note in ?`:::` about the use of this operator.
    ```

# sf

Version: 0.7-2

## In both

*   checking tests ...
    ```
    ...
    > ================================================================================
    > ================================================================================
    > ================================================================================
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      OGR: Unsupported geometry type
      Reading layer `nc' from data source `/Users/lionel/Desktop/rlang/revdep/checks.noindex/sf/new/sf.Rcheck/sf/shape/nc.shp' using driver `ESRI Shapefile'
      Simple feature collection with 100 features and 14 fields
      geometry type:  MULTIPOLYGON
      dimension:      XY
      bbox:           xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
      epsg (SRID):    4267
      proj4string:    +proj=longlat +datum=NAD27 +no_defs
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 457 SKIPPED: 59 FAILED: 2
      1. Error: gdal_subdatasets works (@test_gdal.R#50) 
      2. Error: (unknown) (@test_geos.R#89) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 18.7Mb
      sub-directories of 1Mb or more:
        R        2.0Mb
        doc     11.8Mb
        sqlite   1.5Mb
    ```

# shiny

Version: 1.2.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 12.9Mb
      sub-directories of 1Mb or more:
        R     4.1Mb
        www   7.9Mb
    ```

# SingleCaseES

Version: 0.4.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'Effect-size-definitions.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# singscore

Version: 1.2.2

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    Attaching package: 'XML'
    
    The following object is masked from 'package:tools':
    
        toHTML
    
    Loading required package: graph
    
    Attaching package: 'graph'
    
    The following object is masked from 'package:XML':
    
        addNode
    
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'singscore.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# sjstats

Version: 0.17.2

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘arm’
    ```

# sparklyr

Version: 0.9.3

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.1Mb
      sub-directories of 1Mb or more:
        R      4.1Mb
        java   1.9Mb
    ```

# sperrorest

Version: 2.1.5

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'spatial-modeling-use-case.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# starmie

Version: 0.1.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.2Mb
      sub-directories of 1Mb or more:
        doc       1.1Mb
        extdata   4.9Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘MCMCpack’
      All declared Imports should be used.
    ```

# stars

Version: 0.2-0

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘starsdata’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 10.9Mb
      sub-directories of 1Mb or more:
        doc   6.4Mb
        nc    2.9Mb
    ```

# stplanr

Version: 0.2.6

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘stplanr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geo_buffer
    > ### Title: Perform a buffer operation on a temporary projected CRS
    > ### Aliases: geo_buffer
    > 
    > ### ** Examples
    > 
    > buff_sp <- geo_buffer(routes_fast, width = 100)
    Transforming to CRS +proj=aeqd +lat_0=53.81895189 +lon_0=-1.52748741 +x_0=0 +y_0=0 +ellps=WGS84
    > class(buff_sp)
    [1] "SpatialPolygons"
    attr(,"package")
    [1] "sp"
    > plot(buff_sp, col = "red")
    > routes_fast_sf <- sf::st_as_sf(routes_fast)
    > buff_sf <- geo_buffer(routes_fast_sf, dist = 50)
    Assertion failed: (0), function query, file ../../../../src/geos-3.6.1/src/index/strtree/AbstractSTRtree.cpp, line 287.
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'introducing-stplanr.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# sugrrants

Version: 0.2.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Loading required package: viridisLite
    Loading required package: ggplot2
    Warning: The `printer` argument is soft-deprecated as of rlang 0.3.0.
    This warning is displayed once per session.
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'frame-calendar.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘gtable’
      All declared Imports should be used.
    ```

# SWMPrExtension

Version: 0.3.16

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        data   4.0Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rgeos’
      All declared Imports should be used.
    ```

# tableschema.r

Version: 1.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘iterators’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘parsedate’
    ```

# tabula

Version: 1.0.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'diversity.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘tidyr’
      All declared Imports should be used.
    ```

# tabularaster

Version: 0.5.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘methods’
      All declared Imports should be used.
    ```

# taxa

Version: 0.3.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        R      2.0Mb
        data   1.1Mb
        doc    1.7Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘knitr’ ‘lazyeval’ ‘rlang’ ‘tidyr’
      All declared Imports should be used.
    ```

# textfeatures

Version: 0.3.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘utils’
      All declared Imports should be used.
    ```

# textrecipes

Version: 0.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘stringr’
      All declared Imports should be used.
    ```

# tfdatasets

Version: 1.9

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        File "/Users/lionel/Desktop/rlang/revdep/library.noindex/tfdatasets/reticulate/python/rpytools/call.py", line 21, in python_function
          raise RuntimeError(res[kErrorKey])
      
      Class:   Rcpp::exception/C++Error/error/condition
      
      WARNING:tensorflow:From /Users/lionel/.virtualenvs/r-tensorflow/lib/python2.7/site-packages/tensorflow/python/util/deprecation.py:268: from_sparse_tensor_slices (from tensorflow.python.data.ops.dataset_ops) is deprecated and will be removed in a future version.
      Instructions for updating:
      Use `tf.data.Dataset.from_tensor_slices()`.
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 99 SKIPPED: 0 FAILED: 2
      1. Failure: input_fn feeds data to train and evaluate 
      2. Failure: input_fn works with custom estimators 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# tidybayes

Version: 1.0.3

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
             modules = modules, factories = factories, jags = jags, call.setup = TRUE, method = method, 
             mutate = mutate)
      10: setup.jags(model = outmodel, monitor = outmonitor, data = outdata, n.chains = n.chains, 
             inits = outinits, modules = modules, factories = factories, response = response, 
             fitted = fitted, residual = residual, jags = jags, method = method, mutate = mutate)
      11: loadandcheckrjags()
      12: stop("Loading the rjags package failed (diagnostics are given above this error message)", 
             call. = FALSE)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 260 SKIPPED: 2 FAILED: 1
      1. Error: tidy_draws works with runjags (@test.tidy_draws.R#87) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# tidyhydat

Version: 0.3.5

## In both

*   R CMD check timed out
    

# tidyinftheo

Version: 0.2.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      12: shannon_entropy(.)
      13: reduce_data(.data, !!enquo(X), numvars = 1, na.rm = na.rm) at /Users/lionel/Desktop/rlang/revdep/checks.noindex/tidyinftheo/new/tidyinftheo.Rcheck/00_pkg_src/tidyinftheo/R/inftheo.R:70
      14: tidyselect::vars_select(names(reduced_tab), !!!quos(...)) at /Users/lionel/Desktop/rlang/revdep/checks.noindex/tidyinftheo/new/tidyinftheo.Rcheck/00_pkg_src/tidyinftheo/R/inftheo.R:26
      15: vars_select_eval(.vars, quos)
      16: map_lgl(quos, quo_is_helper)
      17: .f(.x[[i]], ...)
      18: extract_expr(quo)
      19: is_call(expr, paren_sym)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 38 SKIPPED: 0 FAILED: 1
      1. Error: right number of columns given (@test-errors.R#10) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'math.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘tibble’ ‘tidyverse’
      All declared Imports should be used.
    ```

# tidymodels

Version: 0.0.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘broom’ ‘dials’ ‘parsnip’
      All declared Imports should be used.
    ```

# tidyquant

Version: 0.5.5

## Newly fixed

*   R CMD check timed out
    

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        doc   4.1Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unable to find any JVMs matching version "(null)".
    No Java runtime present, try --request to install.
    ```

# tidyr

Version: 0.8.2

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 23 marked UTF-8 strings
    ```

# tidytext

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Unable to find any JVMs matching version "(null)".
    No Java runtime present, try --request to install.
    ```

# tidytransit

Version: 0.3.5

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        extdata   4.4Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘here’ ‘htmltools’ ‘scales’ ‘stringr’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 41 marked UTF-8 strings
    ```

# tidyverse

Version: 1.2.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dbplyr’ ‘reprex’ ‘rlang’
      All declared Imports should be used.
    ```

# tidyxl

Version: 1.0.3

## In both

*   checking compiled code ... WARNING
    ```
    File ‘tidyxl/libs/tidyxl.so’:
      Found ‘_abort’, possibly from ‘abort’ (C)
        Object: ‘xlex.o’
    
    Compiled code should not call entry points which might terminate R
    nor write to stdout/stderr instead of to the console, nor use
    Fortran I/O nor system RNGs.
    
    See ‘Writing portable packages’ in the ‘Writing R Extensions’
    manual.
    ```

# tipr

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘broom’ ‘tibble’
      All declared Imports should be used.
    ```

# totalcensus

Version: 0.5.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        data   5.3Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 64 marked Latin-1 strings
      Note: found 548 marked UTF-8 strings
    ```

# transcriptogramer

Version: 1.4.1

## In both

*   checking whether package ‘transcriptogramer’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/transcriptogramer/new/transcriptogramer.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘transcriptogramer’ ...
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘GO.db’
ERROR: lazy loading failed for package ‘transcriptogramer’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/transcriptogramer/new/transcriptogramer.Rcheck/transcriptogramer’

```
### CRAN

```
* installing *source* package ‘transcriptogramer’ ...
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘GO.db’
ERROR: lazy loading failed for package ‘transcriptogramer’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/transcriptogramer/old/transcriptogramer.Rcheck/transcriptogramer’

```
# translateSPSS2R

Version: 1.0.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    xpssTtest: no visible global function definition for ‘t.test’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/translateSPSS2R/new/translateSPSS2R.Rcheck/00_pkg_src/translateSPSS2R/R/xpssTtest.R:617)
    xpssTtest: no visible global function definition for ‘na.omit’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/translateSPSS2R/new/translateSPSS2R.Rcheck/00_pkg_src/translateSPSS2R/R/xpssTtest.R:627)
    xpssTtest: no visible global function definition for ‘sd’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/translateSPSS2R/new/translateSPSS2R.Rcheck/00_pkg_src/translateSPSS2R/R/xpssTtest.R:628)
    xpssTtest: no visible global function definition for ‘na.omit’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/translateSPSS2R/new/translateSPSS2R.Rcheck/00_pkg_src/translateSPSS2R/R/xpssTtest.R:628)
    xpssTtest: no visible global function definition for ‘cor.test’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/translateSPSS2R/new/translateSPSS2R.Rcheck/00_pkg_src/translateSPSS2R/R/xpssTtest.R:631)
    Undefined global functions or variables:
      anova as.formula complete.cases cor.test density frequency
      globalVariables head lines lm median na.omit quantile sd summary.lm
      t.test tail title var
    Consider adding
      importFrom("graphics", "lines", "title")
      importFrom("stats", "anova", "as.formula", "complete.cases",
                 "cor.test", "density", "frequency", "lm", "median",
                 "na.omit", "quantile", "sd", "summary.lm", "t.test", "var")
      importFrom("utils", "globalVariables", "head", "tail")
    to your NAMESPACE file.
    ```

# treeio

Version: 1.6.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'Exporter.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# tsibble

Version: 0.6.2

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
        filter
    
    
    Attaching package: 'lubridate'
    
    The following objects are masked from 'package:tsibble':
    
        interval, new_interval
    
    The following object is masked from 'package:base':
    
        date
    
    Warning: The `printer` argument is soft-deprecated as of rlang 0.3.0.
    This warning is displayed once per session.
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'intro-tsibble.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# UKgrid

Version: 0.1.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 12.5Mb
      sub-directories of 1Mb or more:
        data   4.1Mb
        doc    8.3Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rlang’
      All declared Imports should be used.
    ```

# valaddin

Version: 0.1.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘purrr’
      All declared Imports should be used.
    ```

# veccompare

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘pander’
      All declared Imports should be used.
    ```

# vidger

Version: 1.2.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 10.8Mb
      sub-directories of 1Mb or more:
        data   4.1Mb
        doc    6.1Mb
    ```

# visdat

Version: 0.5.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘plotly’ ‘rlang’
      All declared Imports should be used.
    ```

# voxel

Version: 1.3.5

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘methods’
      All declared Imports should be used.
    ```

# VWPre

Version: 1.1.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        data   3.1Mb
        doc    1.3Mb
    ```

# wbstats

Version: 0.2

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1528 marked UTF-8 strings
    ```

# weathercan

Version: 0.2.8

## In both

*   R CMD check timed out
    

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘xml2’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 25 marked UTF-8 strings
    ```

# wordbankr

Version: 0.3.0

## In both

*   R CMD check timed out
    

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dbplyr’
      All declared Imports should be used.
    ```

# xpose

Version: 0.4.3

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
          filter
      
      > 
      > test_check("xpose")
      ── 1. Failure: dot arguments are properly passed to readr (@test-read_nm_tables.
      nrow(...) not equal to 3.
      1/1 mismatches
      [1] 2 - 3 == -1
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 525 SKIPPED: 6 FAILED: 1
      1. Failure: dot arguments are properly passed to readr (@test-read_nm_tables.R#57) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# ypr

Version: 0.3.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'ypr.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘graphics’
      All declared Imports should be used.
    ```

# ZipRadius

Version: 1.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ggplot2’ ‘testthat’
      All declared Imports should be used.
    ```

# zscorer

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyr’
      All declared Imports should be used.
    ```

