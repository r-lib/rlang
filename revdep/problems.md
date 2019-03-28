# abjutils

Version: 0.2.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘httr’ ‘progress’
      All declared Imports should be used.
    ```

# AMR

Version: 0.6.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.5Mb
      sub-directories of 1Mb or more:
        data   2.9Mb
        R      2.0Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘microbenchmark’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 7 marked UTF-8 strings
    ```

# amt

Version: 0.0.6

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘magrittr’ ‘Rcpp’
      All declared Imports should be used.
    ```

# analysisPipelines

Version: 1.0.0

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      ‘SparkR’ ‘reticulate’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘devtools’
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

# areal

Version: 0.1.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘lwgeom’ ‘tibble’
      All declared Imports should be used.
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

Version: 0.3.2

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 552 marked UTF-8 strings
    ```

# banR

Version: 0.2.0

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### Name: geocode_tbl
    > ### Title: Geocode tbl
    > ### Aliases: geocode_tbl
    > 
    > ### ** Examples
    > 
    > 
    > table_test <- tibble::tibble(
    + x = c("39 quai Andre Citroen", "64 Allee de Bercy", "20 avenue de Segur"), 
    + y = c("75015", "75012", "75007"), 
    + z = rnorm(3)
    + )
    > 
    > geocode_tbl(tbl = table_test, adresse = x)
    Writing tempfile to.../var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//RtmpBP6sm0/file127c81d5902d5.csv
    If file is larger than 8 MB, it must be splitted
    Size is : 61 bytes
    Server errorService UnavailableServer error: (503) Service Unavailable
    Error in geocode_tbl(tbl = table_test, adresse = x) : 
      The API sent back an error 503
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
             class = "tbl_df") at testthat/test_geocodetbl.R:60
      2: quasi_label(enquo(object), label)
      3: eval_bare(get_expr(quo), get_env(quo))
      4: reverse_geocode_tbl(tbl = table_reverse, longitude = x, latitude = y) at /private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/RtmpWzsDrT/R.INSTALLd481114fc489/rlang/R/eval.R:99
      5: stop("The API sent back an error ", httr::status_code(query_results)) at /Users/lionel/Desktop/rlang/revdep/checks.noindex/banR/new/banR.Rcheck/00_pkg_src/banR/R/geocode_tbl.R:197
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 3 SKIPPED: 0 FAILED: 4
      1. Error: Geocode tbl works  (@test_geocodetbl.R#12) 
      2. Error: Input and output DFs have a similar number of rows (@test_geocodetbl.R#31) 
      3. Error: Geocode_tbl works with a single-column input data.frame (@test_geocodetbl.R#48) 
      4. Error: Reverse geocode tbl works  (@test_geocodetbl.R#60) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 47-49 (geocode.Rmd) 
    Error: processing vignette 'geocode.Rmd' failed with diagnostics:
    The API sent back an error 503
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘stringr’
      All declared Imports should be used.
    ```

# banter

Version: 0.9.3

## In both

*   checking examples ... ERROR
    ```
    ...
    
    > ### Name: plotDetectorTrace
    > ### Title: Plot BANTER Detector Traces
    > ### Aliases: plotDetectorTrace
    > 
    > ### ** Examples
    > 
    > data(train.data)
    > # initialize BANTER model with event data
    > bant.mdl <- initBanterModel(train.data$events)
    > # add all detector models
    > bant.mdl <- addBanterDetector(
    +   bant.mdl, train.data$detectors, 
    +   ntree = 50, sampsize = 1, num.cores = 1
    + )
    > 
    > plotDetectorTrace(bant.mdl)
    Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
      there is no package called ‘spatstat’
    Calls: plotDetectorTrace ... tryCatch -> tryCatchList -> tryCatchOne -> <Anonymous>
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ranger’
      All declared Imports should be used.
    ```

# bayesdfa

Version: 0.1.2

## In both

*   checking whether package ‘bayesdfa’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘Rcpp’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/bayesdfa/new/bayesdfa.Rcheck/00install.out’ for details.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.8Mb
      sub-directories of 1Mb or more:
        libs   4.9Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# BayesMallows

Version: 0.4.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
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
      installed size is  7.1Mb
      sub-directories of 1Mb or more:
        doc   4.0Mb
        R     2.5Mb
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
    mold,eSet: no visible global function definition for 'phenoData'
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/biovizBase/new/biovizBase.Rcheck/00_pkg_src/biovizBase/R/mold-method.R:5)
    mold,eSet: no visible global function definition for 'melt'
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/biovizBase/new/biovizBase.Rcheck/00_pkg_src/biovizBase/R/mold-method.R:8)
    mold,eSet: no visible global function definition for 'varLabels'
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/biovizBase/new/biovizBase.Rcheck/00_pkg_src/biovizBase/R/mold-method.R:8)
    mold,ExpressionSet: no visible global function definition for 'exprs'
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/biovizBase/new/biovizBase.Rcheck/00_pkg_src/biovizBase/R/mold-method.R:55)
    mold,ExpressionSet: no visible global function definition for 'pData'
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/biovizBase/new/biovizBase.Rcheck/00_pkg_src/biovizBase/R/mold-method.R:56)
    mold,ExpressionSet: no visible global function definition for 'exprs'
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/biovizBase/new/biovizBase.Rcheck/00_pkg_src/biovizBase/R/mold-method.R:57)
    mold,RleList: no visible binding for global variable 'xRleList'
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/biovizBase/new/biovizBase.Rcheck/00_pkg_src/biovizBase/R/mold-method.R:162)
    Undefined global functions or variables:
      .circle.x .circle.y Chromosome end_location exprs from.x from.y melt
      pData phenoData start_location symbol to.x to.y varLabels xRleList
    ```

# blorr

Version: 0.2.1

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘lmtest’
    ```

# c14bazAAR

Version: 1.0.2

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 76 marked UTF-8 strings
    ```

# celaref

Version: 1.0.1

## In both

*   checking whether package ‘celaref’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘GenomeInfoDb’ was built under R version 3.5.2
      Warning: package ‘BiocParallel’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/celaref/new/celaref.Rcheck/00install.out’ for details.
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    snapshotDate(): 2018-10-30
    see ?celarefData and browseVignettes('celarefData') for documentation
    downloading 0 resources
    loading from cache 
        '/Users/lionel//.ExperimentHub/1586'
    snapshotDate(): 2018-10-30
    see ?celarefData and browseVignettes('celarefData') for documentation
    downloading 0 resources
    loading from cache 
        '/Users/lionel//.ExperimentHub/1587'
    snapshotDate(): 2018-10-30
    see ?celarefData and browseVignettes('celarefData') for documentation
    downloading 0 resources
    loading from cache 
        '/Users/lionel//.ExperimentHub/1588'
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'celaref_doco.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .github
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

# CGPfunctions

Version: 0.5.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘devtools’ ‘pwr’
      All declared Imports should be used.
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
clang: error: unsupported option '-fopenmp'
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
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘circumplex’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/circumplex/old/circumplex.Rcheck/circumplex’

```
# clustree

Version: 0.3.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > 
      > test_check("clustree")
      ── 1. Error: show_axis works (@test-clustree.R#77)  ────────────────────────────
      object 'seurat' not found
      1: expect_is(clustree(seurat, show_axis = TRUE), c("gg", "ggplot")) at testthat/test-clustree.R:77
      2: quasi_label(enquo(object), label)
      3: eval_bare(get_expr(quo), get_env(quo))
      4: clustree(seurat, show_axis = TRUE) at /private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/RtmpWzsDrT/R.INSTALLd481114fc489/rlang/R/eval.R:99
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 51 SKIPPED: 4 FAILED: 1
      1. Error: show_axis works (@test-clustree.R#77) 
      
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
    Error: processing vignette 'clustree.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# codebook

Version: 0.8.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘graphics’ ‘jsonlite’ ‘pander’ ‘rlang’
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

Version: 1.20.1

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
    CellCounts_character: no visible binding for global variable
      ‘_COMPASS_CellCounts_character’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/COMPASS/new/COMPASS.Rcheck/00_pkg_src/COMPASS/R/RcppExports.R:5)
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
    Undefined global functions or variables:
      _COMPASS_CellCounts _COMPASS_CellCounts_character Count id population
    ```

*   checking for unstated dependencies in vignettes ... NOTE
    ```
    'library' or 'require' calls not declared from:
      ‘ggplot2’ ‘readxl’
    ```

# comperank

Version: 0.1.0

## In both

*   checking whether package ‘comperank’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘comperes’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/comperank/new/comperank.Rcheck/00install.out’ for details.
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

# crplyr

Version: 0.2.0

## In both

*   checking whether package ‘crplyr’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘crunch’ was built under R version 3.5.2
      Warning: package ‘dplyr’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/crplyr/new/crplyr.Rcheck/00install.out’ for details.
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

Version: 0.2.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Warning: package 'cowplot' was built under R version 3.5.2
    
    Attaching package: 'cowplot'
    
    The following object is masked from 'package:ggplot2':
    
        ggsave
    
    Warning: `data_frame()` is deprecated, use `tibble()`.
    This warning is displayed once per session.
    Loading required package: boot
    Loading required package: magrittr
    Warning: Some components of ... were not used: ..1
    Quitting from lines 110-166 (robust-statistical-visualization.Rmd) 
    Error: processing vignette 'robust-statistical-visualization.Rmd' failed with diagnostics:
    polygon edge not found
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.9Mb
      sub-directories of 1Mb or more:
        doc   5.8Mb
    ```

# DeclareDesign

Version: 0.16.0

## In both

*   checking whether package ‘DeclareDesign’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘fabricatr’ was built under R version 3.5.2
      Warning: package ‘estimatr’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/DeclareDesign/new/DeclareDesign.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘lfe’
    ```

# DEGreport

Version: 1.18.1

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
    significants,list : <anonymous>: no visible binding for global variable
      ‘gene’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/DEGreport/new/DEGreport.Rcheck/00_pkg_src/DEGreport/R/AllMethods.R:248)
    significants,TopTags: no visible binding for global variable ‘FDR’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/DEGreport/new/DEGreport.Rcheck/00_pkg_src/DEGreport/R/AllMethods.R:153-157)
    significants,TopTags: no visible binding for global variable ‘logFC’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/DEGreport/new/DEGreport.Rcheck/00_pkg_src/DEGreport/R/AllMethods.R:153-157)
    Undefined global functions or variables:
      .x base_mean boxplot cluster comp compare count counts covar desc
      enrichGO fdr FDR gene genes itemConsensus k keys lm log2fc
      log2FoldChange logFC max_sd min_median n p.value r ratios rowMedians
      score simplify value_fc value_fdr x xend y yend
    Consider adding
      importFrom("graphics", "boxplot")
      importFrom("stats", "lm")
    to your NAMESPACE file.
    ```

# DesignLibrary

Version: 0.1.2

## In both

*   checking whether package ‘DesignLibrary’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘DeclareDesign’ was built under R version 3.5.2
      Warning: package ‘fabricatr’ was built under R version 3.5.2
      Warning: package ‘estimatr’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/DesignLibrary/new/DesignLibrary.Rcheck/00install.out’ for details.
    ```

# detrendr

Version: 0.6.0

## In both

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# dexter

Version: 0.8.4

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
    
    Loading required package: RSQLite
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'dexter.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# dextergui

Version: 0.1.6

## In both

*   checking whether package ‘dextergui’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘dexter’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/dextergui/new/dextergui.Rcheck/00install.out’ for details.
    ```

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
        htmlwidgets   3.0Mb
        R             3.0Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

# directlabels

Version: 2018.05.22

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘inlinedocs’
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

Version: 0.3.9

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        doc   4.1Mb
    ```

# dotwhisker

Version: 0.5.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Warning in native_encode(res, to = encoding) :
      some characters may not work under the current locale
    Warning in readLines(con, warn = FALSE) :
      invalid input found on input connection 'dotwhisker-vignette.knit.md'
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'dotwhisker-vignette.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# dplyr

Version: 0.8.0.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.3Mb
      sub-directories of 1Mb or more:
        libs   2.4Mb
        R      2.1Mb
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘vctrs’
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4 marked UTF-8 strings
    ```

# easyalluvial

Version: 0.1.8

## In both

*   checking examples ... ERROR
    ```
    ...
        filter, lag
    
    The following objects are masked from ‘package:base’:
    
        intersect, setdiff, setequal, union
    
    > 
    > data = as_tibble(mtcars)
    > categoricals = c('cyl', 'vs', 'am', 'gear', 'carb')
    > numericals = c('mpg', 'cyl', 'disp', 'hp', 'drat', 'wt', 'qsec')
    > max_variables = 5
    > 
    > data = data %>%
    +   mutate_at( vars(categoricals), as.factor )
    > 
    > 
    > alluvial_wide( data = data
    +                 , max_variables = max_variables
    +                 , fill_by = 'first_variable' )
    Error: No role currently exists for column(s): 'easyalluvialid'. Please use `update_role()` instead.
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      14: add_role(., easyalluvialid, new_role = "id variable")
      15: stop(glue::glue("No role currently exists for column(s): {vars}. Please use ", "`update_role()` instead."), 
             call. = FALSE) at /private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/RtmpRe0Uvl/R.INSTALLf9503f599132/recipes/R/roles.R:138
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 6 SKIPPED: 9 FAILED: 6
      1. Error: alluvial_long (@test_alluvial_long.R#94) 
      2. Error: alluvial_wide (@test_alluvial_wide.R#17) 
      3. Error: manip_bin_numerics (@test_manip.R#28) 
      4. Error: manip_bin_numerics zero variance columns (@test_manip.R#84) 
      5. Error: manip_bin_numerics with vector (@test_manip.R#96) 
      6. Error: plot condensation (@test_plot_condensation.R#15) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# echor

Version: 0.1.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘lubridate’
      All declared Imports should be used.
    ```

# embed

Version: 0.0.2

## In both

*   checking whether package ‘embed’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘dplyr’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/embed/new/embed.Rcheck/00install.out’ for details.
    ```

# emuR

Version: 1.1.2

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    
    Attaching package: 'emuR'
    
    The following object is masked from 'package:base':
    
        norm
    
    Warning: call dbDisconnect() when finished working with a connection
    [WARNING] Could not parse YAML metadata at line 1 column 1: :3:79: Unexpected '
      '
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'emuDB.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  7.1Mb
      sub-directories of 1Mb or more:
        doc       1.2Mb
        extdata   1.5Mb
        R         3.0Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘git2r’ ‘servr’
      All declared Imports should be used.
    ```

# encryptr

Version: 0.1.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘knitr’
      All declared Imports should be used.
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
    

*   checking whether package ‘ergm’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘network’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/ergm/new/ergm.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘Rmpi’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  7.1Mb
      sub-directories of 1Mb or more:
        doc   1.7Mb
        R     4.0Mb
    ```

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘tergm’, ‘ergm.count’, ‘networkDynamic’
    ```

# ergm.ego

Version: 0.4.0

## In both

*   checking whether package ‘ergm.ego’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘network’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/ergm.ego/new/ergm.ego.Rcheck/00install.out’ for details.
    ```

*   checking Rd \usage sections ... NOTE
    ```
    S3 methods shown with full name in documentation object 'mixingmatrix.egodata':
      ‘mixingmatrix.egodata’
    
    The \usage entries for S3 methods should use the \method markup and
    not their full name.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

# estimatr

Version: 0.16

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      2: getExportedValue(pkg, name)
      3: asNamespace(ns)
      4: getNamespace(ns)
      5: tryCatch(loadNamespace(name), error = function(e) stop(e))
      6: tryCatchList(expr, classes, parentenv, handlers)
      7: tryCatchOne(expr, names, parentenv, handlers[[1L]])
      8: value[[3L]](cond)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 1277 SKIPPED: 0 FAILED: 2
      1. Error: IV FE matches lfe including proj r2 (@test-iv-robust-fes.R#112) 
      2. Error: FEs give correct projected F-stats (@test-lm-robust-fes.R#456) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘lfe’
    
    Package which this enhances but not available for checking: ‘texreg’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘texreg’
    ```

# etl

Version: 0.3.7

## In both

*   checking whether package ‘etl’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘dplyr’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/etl/new/etl.Rcheck/00install.out’ for details.
    ```

# EventStudy

Version: 0.36

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.4Mb
      sub-directories of 1Mb or more:
        doc   5.0Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘curl’ ‘openxlsx’ ‘stringr’
      All declared Imports should be used.
    ```

# ezplot

Version: 0.2.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘magrittr’
      All declared Imports should be used.
    ```

# filesstrings

Version: 3.0.0

## In both

*   checking whether package ‘filesstrings’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘stringr’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/filesstrings/new/filesstrings.Rcheck/00install.out’ for details.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘dplyr’
    ```

# finalfit

Version: 0.9.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘readr’
      All declared Imports should be used.
    ```

# fingertipscharts

Version: 0.0.4

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘curl’ ‘mapproj’
      All declared Imports should be used.
    ```

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

# fredr

Version: 1.0.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

# furrr

Version: 0.1.0

## In both

*   checking whether package ‘furrr’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘future’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/furrr/new/furrr.Rcheck/00install.out’ for details.
    ```

# GGally

Version: 1.4.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Unable to find any JVMs matching version "(null)".
    No Java runtime present, try --request to install.
    ```

# ggbio

Version: 1.30.0

## In both

*   checking examples ... ERROR
    ```
    ...
    > ###################################################
    > p1 <- autoplot(xRleList, stat = "identity")
    > p2 <- autoplot(xRleList, stat = "identity", geom = "point", color = "red")
    > tracks('line' = p1, "point" = p2)
    > 
    > 
    > ###################################################
    > ### code chunk number 25: rlel-slice
    > ###################################################
    > p1 <- autoplot(xRleList, type = "viewMaxs", stat = "slice", lower = 5)
    > p2 <- autoplot(xRleList, type = "viewMaxs", stat = "slice", lower = 5, geom = "heatmap")
    > tracks('bar' = p1, "heatmap" = p2)
    > 
    > 
    > ###################################################
    > ### code chunk number 26: txdb
    > ###################################################
    > library(TxDb.Hsapiens.UCSC.hg19.knownGene)
    Error in library(TxDb.Hsapiens.UCSC.hg19.knownGene) : 
      there is no package called 'TxDb.Hsapiens.UCSC.hg19.knownGene'
    Execution halted
    ```

*   R CMD check timed out
    

*   checking whether package ‘ggbio’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: subclass "GeneNameFilter" of class "AnnotationFilter" is not local and cannot be updated for new inheritance information; consider setClassUnion()
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/ggbio/new/ggbio.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘BSgenome.Hsapiens.UCSC.hg19’ ‘Homo.sapiens’
      ‘TxDb.Hsapiens.UCSC.hg19.knownGene’
      ‘TxDb.Mmusculus.UCSC.mm9.knownGene’ ‘EnsDb.Hsapiens.v75’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.1Mb
      sub-directories of 1Mb or more:
        doc   1.8Mb
        R     4.0Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    ':::' call which should be '::': 'ggplot2:::set_last_plot'
      See the note in ?`:::` about the use of this operator.
    Unexported objects imported by ':::' calls:
      'ggplot2:::add_ggplot' 'ggplot2:::cunion' 'ggplot2:::rename_aes'
      'ggplot2:::rescale01' 'S4Vectors:::top_prenv'
      See the note in ?`:::` about the use of this operator.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      "overlapping")
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/ggbio/new/ggbio.Rcheck/00_pkg_src/ggbio/R/plotSpliceSum-method.R:33)
    stat_mismatch,GRanges: no visible binding for global variable 'sts'
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/ggbio/new/ggbio.Rcheck/00_pkg_src/ggbio/R/stat_mismatch-method.R:90-92)
    stat_mismatch,GRanges: no visible binding for global variable 'eds'
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/ggbio/new/ggbio.Rcheck/00_pkg_src/ggbio/R/stat_mismatch-method.R:90-92)
    stat_mismatch,GRanges: no visible binding for global variable 'read'
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/ggbio/new/ggbio.Rcheck/00_pkg_src/ggbio/R/stat_mismatch-method.R:90-92)
    stat_mismatch,GRanges: no visible binding for global variable 'sts'
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/ggbio/new/ggbio.Rcheck/00_pkg_src/ggbio/R/stat_mismatch-method.R:95-99)
    stat_mismatch,GRanges: no visible binding for global variable 'eds'
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/ggbio/new/ggbio.Rcheck/00_pkg_src/ggbio/R/stat_mismatch-method.R:95-99)
    stat_mismatch,GRanges: no visible binding for global variable 'read'
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/ggbio/new/ggbio.Rcheck/00_pkg_src/ggbio/R/stat_mismatch-method.R:95-99)
    Undefined global functions or variables:
      .fragLength .layout_circle.stats .x breaks coefs data eds fe fl
      gieStain ideoCyto indexProbesProcessed midpoint mt name read se
      stepping sts value variable x xend y y.text y2 yend yend2
    Consider adding
      importFrom("utils", "data")
    to your NAMESPACE file.
    ```

# ggcyto

Version: 1.10.2

## In both

*   checking whether package ‘ggcyto’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘flowCore’ was built under R version 3.5.2
      Warning: package ‘ncdfFlow’ was built under R version 3.5.2
      Warning: package ‘flowWorkspace’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/ggcyto/new/ggcyto.Rcheck/00install.out’ for details.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.5Mb
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
      ‘ggplot2:::is_calculated_aes’ ‘ggplot2:::is.waive’
      ‘ggplot2:::make_labels’ ‘ggplot2:::make_scale’ ‘ggplot2:::plot_clone’
      ‘ggplot2:::print.ggplot’ ‘ggplot2:::scales_add_defaults’
      ‘ggplot2:::scales_list’ ‘ggplot2:::update_theme’
      See the note in ?`:::` about the use of this operator.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/ggcyto/new/ggcyto.Rcheck/00_pkg_src/ggcyto/R/ggcyto_GatingLayout.R:47)
    ggcyto.flowSet: no visible binding for global variable ‘name’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/ggcyto/new/ggcyto.Rcheck/00_pkg_src/ggcyto/R/ggcyto_flowSet.R:61)
    ggcyto.flowSet: no visible binding for global variable ‘axis’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/ggcyto/new/ggcyto.Rcheck/00_pkg_src/ggcyto/R/ggcyto_flowSet.R:63-64)
    ggcyto.flowSet: no visible binding for global variable ‘name’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/ggcyto/new/ggcyto.Rcheck/00_pkg_src/ggcyto/R/ggcyto_flowSet.R:63-64)
    ggcyto.GatingSetList: no visible global function definition for
      ‘getS3method’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/ggcyto/new/ggcyto.Rcheck/00_pkg_src/ggcyto/R/ggcyto_GatingSet.R:45)
    ggcyto.ncdfFlowList: no visible global function definition for
      ‘getS3method’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/ggcyto/new/ggcyto.Rcheck/00_pkg_src/ggcyto/R/ggcyto_flowSet.R:103)
    Undefined global functions or variables:
      approx axis density desc dist getS3method gray modifyList name
    Consider adding
      importFrom("graphics", "axis")
      importFrom("grDevices", "gray")
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

# ggetho

Version: 0.3.4

## In both

*   checking whether package ‘ggetho’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘behavr’ was built under R version 3.5.2
      Warning: package ‘data.table’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/ggetho/new/ggetho.Rcheck/00install.out’ for details.
    ```

# ggforce

Version: 0.2.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        doc   2.8Mb
        R     2.1Mb
    ```

# ggformula

Version: 0.9.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.0Mb
      sub-directories of 1Mb or more:
        doc   2.7Mb
        R     3.0Mb
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

Version: 0.0.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tibble’
      All declared Imports should be used.
    ```

# ggplot2

Version: 3.1.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.5Mb
      sub-directories of 1Mb or more:
        doc   1.8Mb
        R     4.0Mb
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

Version: 0.0.5

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘grDevices’
      All declared Imports should be used.
    ```

# ggquickeda

Version: 0.1.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘colourpicker’ ‘dplyr’ ‘DT’ ‘Formula’ ‘ggpmisc’ ‘ggrepel’ ‘grDevices’
      ‘gridExtra’ ‘Hmisc’ ‘lazyeval’ ‘markdown’ ‘plotly’ ‘quantreg’ ‘rlang’
      ‘shinyjs’ ‘table1’ ‘tidyr’
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

Version: 0.0.10

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.4Mb
      sub-directories of 1Mb or more:
        help   4.1Mb
        R      2.0Mb
    ```

# ggthemes

Version: 4.1.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 138 marked UTF-8 strings
    ```

# ggtree

Version: 1.14.6

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
        doc        4.9Mb
        examples   3.7Mb
        R          2.0Mb
    ```

# glmSparseNet

Version: 1.0.0

## In both

*   checking whether package ‘glmSparseNet’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘MultiAssayExperiment’ was built under R version 3.5.2
      Warning: package ‘GenomeInfoDb’ was built under R version 3.5.2
      Warning: package ‘BiocParallel’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/glmSparseNet/new/glmSparseNet.Rcheck/00install.out’ for details.
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Warning: package 'dplyr' was built under R version 3.5.2
    
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

# graphTweets

Version: 0.5.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘utils’
      All declared Imports should be used.
    ```

# grattan

Version: 1.7.1.0

## In both

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘taxstats’ ‘taxstats1516’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.5Mb
      sub-directories of 1Mb or more:
        doc   3.5Mb
        R     2.2Mb
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

# highcharter

Version: 0.7.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.2Mb
      sub-directories of 1Mb or more:
        doc           3.7Mb
        htmlwidgets   4.0Mb
    ```

# Hmisc

Version: 4.2-0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.7Mb
      sub-directories of 1Mb or more:
        help   2.1Mb
        R      7.0Mb
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

Version: 4.5.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        doc   2.9Mb
        R     2.0Mb
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

# ICD10gm

Version: 1.0.3

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.9Mb
      sub-directories of 1Mb or more:
        data   7.0Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 252748 marked UTF-8 strings
    ```

# iCNV

Version: 1.2.1

## In both

*   checking whether package ‘iCNV’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/iCNV/new/iCNV.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘iCNV’ ...
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Warning: package ‘CODEX’ was built under R version 3.5.2
Warning: package ‘Rsamtools’ was built under R version 3.5.2
Warning: package ‘GenomeInfoDb’ was built under R version 3.5.2
Warning: package ‘Biostrings’ was built under R version 3.5.2
Error : package ‘BSgenome.Hsapiens.UCSC.hg19’ required by ‘CODEX’ could not be found
ERROR: lazy loading failed for package ‘iCNV’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/iCNV/new/iCNV.Rcheck/iCNV’

```
### CRAN

```
* installing *source* package ‘iCNV’ ...
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Warning: package ‘CODEX’ was built under R version 3.5.2
Warning: package ‘Rsamtools’ was built under R version 3.5.2
Warning: package ‘GenomeInfoDb’ was built under R version 3.5.2
Warning: package ‘Biostrings’ was built under R version 3.5.2
Error : package ‘BSgenome.Hsapiens.UCSC.hg19’ required by ‘CODEX’ could not be found
ERROR: lazy loading failed for package ‘iCNV’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/iCNV/old/iCNV.Rcheck/iCNV’

```
# idealstan

Version: 0.7.1

## In both

*   checking whether package ‘idealstan’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘Rcpp’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/idealstan/new/idealstan.Rcheck/00install.out’ for details.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.9Mb
      sub-directories of 1Mb or more:
        libs   5.2Mb
        R      1.0Mb
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

*   checking whether package ‘implyr’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘dplyr’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/implyr/new/implyr.Rcheck/00install.out’ for details.
    ```

# INDperform

Version: 0.2.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.3Mb
      sub-directories of 1Mb or more:
        data   4.0Mb
        help   1.1Mb
        R      1.0Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘lazyeval’
      All declared Imports should be used.
    ```

# interactions

Version: 1.0.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘quantreg’, ‘effects’, ‘Hmisc’, ‘rockchalk’, ‘pequod’
    ```

# ipumsr

Version: 0.4.0

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

# isomiRs

Version: 1.10.1

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘isomiRs-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: mirna2targetscan
    > ### Title: Find targets in targetscan database
    > ### Aliases: mirna2targetscan
    > 
    > ### ** Examples
    > 
    > library(targetscan.Hs.eg.db)
    Error in library(targetscan.Hs.eg.db) : 
      there is no package called ‘targetscan.Hs.eg.db’
    Execution halted
    ```

*   checking whether package ‘isomiRs’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘GenomeInfoDb’ was built under R version 3.5.2
      Warning: package ‘BiocParallel’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/isomiRs/new/isomiRs.Rcheck/00install.out’ for details.
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    cranial nerve structural organization
    hepatocyte growth factor receptor signaling pathway
    regulation of neurotransmitter uptake
    establishment of blood-brain barrier
    regulation of cellular response to hypoxia
    positive regulation of membrane depolarization
    response to hexose
    negative regulation of phosphorylation
    response to monosaccharide
    cell-cell adhesion via plasma-membrane adhesion molecules
    Number of mirnas 20
    Number of genes 20
    Factors genescontrolday1day2day3day7day14
    Factors mirnascontrolday1day2day3day7day14
    Order genescontrolcontrolcontrolday1day1day1day2day2day2day3day3day3day7day7day7day14day14day14
    Order mirnascontrolcontrolcontrolday1day1day1day2day2day2day3day3day3day7day7day7day14day14day14
    Calculating correlation matrix
    Quitting from lines 301-304 (isomiRs.Rmd) 
    Error: processing vignette 'isomiRs.Rmd' failed with diagnostics:
    there is no package called 'targetscan.Hs.eg.db'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘org.Mm.eg.db’ ‘targetscan.Hs.eg.db’
    ```

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .travis.yml
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/isomiRs/new/isomiRs.Rcheck/00_pkg_src/isomiRs/R/targets.R:535)
    mirna2targetscan: no visible binding for global variable
      ‘targetscan.Hs.egTARGETSFULL’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/isomiRs/new/isomiRs.Rcheck/00_pkg_src/isomiRs/R/targets.R:536)
    mirna2targetscan: no visible binding for global variable
      ‘targetscan.Mm.egMIRNA’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/isomiRs/new/isomiRs.Rcheck/00_pkg_src/isomiRs/R/targets.R:538)
    mirna2targetscan: no visible binding for global variable
      ‘targetscan.Mm.egMIRBASE2FAMILY’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/isomiRs/new/isomiRs.Rcheck/00_pkg_src/isomiRs/R/targets.R:539)
    mirna2targetscan: no visible binding for global variable
      ‘targetscan.Mm.egTARGETS’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/isomiRs/new/isomiRs.Rcheck/00_pkg_src/isomiRs/R/targets.R:540)
    mirna2targetscan: no visible binding for global variable
      ‘targetscan.Mm.egTARGETSFULL’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/isomiRs/new/isomiRs.Rcheck/00_pkg_src/isomiRs/R/targets.R:541)
    Undefined global functions or variables:
      iso_sample targetscan.Hs.egMIRBASE2FAMILY targetscan.Hs.egMIRNA
      targetscan.Hs.egTARGETS targetscan.Hs.egTARGETSFULL
      targetscan.Mm.egMIRBASE2FAMILY targetscan.Mm.egMIRNA
      targetscan.Mm.egTARGETS targetscan.Mm.egTARGETSFULL
    ```

# JointAI

Version: 0.5.0

## In both

*   checking whether package ‘JointAI’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/JointAI/new/JointAI.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘JointAI’ ...
** package ‘JointAI’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rjags’:
 .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/JointAI/rjags/libs/rjags.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/JointAI/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/JointAI/rjags/libs/rjags.so
  Reason: image not found
Error : package ‘rjags’ could not be loaded
ERROR: lazy loading failed for package ‘JointAI’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/JointAI/new/JointAI.Rcheck/JointAI’

```
### CRAN

```
* installing *source* package ‘JointAI’ ...
** package ‘JointAI’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rjags’:
 .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/JointAI/rjags/libs/rjags.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/JointAI/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/JointAI/rjags/libs/rjags.so
  Reason: image not found
Error : package ‘rjags’ could not be loaded
ERROR: lazy loading failed for package ‘JointAI’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/JointAI/old/JointAI.Rcheck/JointAI’

```
# jpmesh

Version: 1.1.2

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

# jtools

Version: 2.0.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.0Mb
      sub-directories of 1Mb or more:
        doc   2.2Mb
        R     2.1Mb
    ```

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘wec’, ‘arm’, ‘effects’, ‘piecewiseSEM’
    ```

# kokudosuuchi

Version: 0.4.2

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 52458 marked UTF-8 strings
    ```

# lenses

Version: 0.0.3

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘dplyr’
    ```

# listarrays

Version: 0.2.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘keras’
    ```

# loose.rock

Version: 1.0.9

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘futile.options’ ‘ggfortify’ ‘grDevices’ ‘stats’
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

# markmyassignment

Version: 0.8.1

## In both

*   checking whether package ‘markmyassignment’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘checkmate’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/markmyassignment/new/markmyassignment.Rcheck/00install.out’ for details.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘lazyeval’
      All declared Imports should be used.
    ```

# matsindf

Version: 0.3.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'matsindf.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# metacoder

Version: 0.3.1

## In both

*   checking whether package ‘metacoder’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘taxa’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/metacoder/new/metacoder.Rcheck/00install.out’ for details.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ggrepel’ ‘reshape’ ‘svglite’
      All declared Imports should be used.
    ```

# mice

Version: 3.4.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘gamlss’
    ```

# MlBayesOpt

Version: 0.3.4

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

# modelr

Version: 0.1.4

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘lme4’, ‘rstanarm’
    ```

# MPTmultiverse

Version: 0.2-0

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
      2. Error: Complete-pooling approaches work (@test-mptinr.R#167) 
      
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

*   checking whether package ‘msigdbr’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘dplyr’ was built under R version 3.5.2
      Warning: package ‘tibble’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/msigdbr/new/msigdbr.Rcheck/00install.out’ for details.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        R   5.1Mb
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

# neo4r

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘igraph’ ‘rlang’ ‘tidyselect’
      All declared Imports should be used.
    ```

# netgsa

Version: 3.1.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'netgsa.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.6Mb
      sub-directories of 1Mb or more:
        data   6.0Mb
    ```

# nhdR

Version: 0.5.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(nhdR)
      Loading required package: maps
      The 7-zip program is needed to unpack NHD downloads (http://www.7-zip.org/).
      > 
      > test_check("nhdR")
      Assertion failed: (0), function query, file ../../../../src/geos-3.6.1/src/index/strtree/AbstractSTRtree.cpp, line 176.
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Loading required package: maps
    The 7-zip program is needed to unpack NHD downloads (http://www.7-zip.org/).
    Warning: package 'dplyr' was built under R version 3.5.2
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Warning: package 'sf' was built under R version 3.5.2
    Linking to GEOS 3.6.1, GDAL 2.1.3, PROJ 4.9.3
    Assertion failed: (0), function query, file ../../../../src/geos-3.6.1/src/index/strtree/AbstractSTRtree.cpp, line 176.
    ```

# nonet

Version: 0.4.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘e1071’ ‘pROC’ ‘purrr’ ‘randomForest’ ‘rlang’
      All declared Imports should be used.
    ```

# NPMOD

Version: 0.1.0

## In both

*   checking whether package ‘NPMOD’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘gWidgets’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/NPMOD/new/NPMOD.Rcheck/00install.out’ for details.
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

# nyctaxi

Version: 0.0.1

## In both

*   checking whether package ‘nyctaxi’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘dplyr’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/nyctaxi/new/nyctaxi.Rcheck/00install.out’ for details.
    ```

# oec

Version: 2.7.8

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘readr’
      All declared Imports should be used.
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

# paletteer

Version: 0.2.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘jcolors’ ‘oompaBase’ ‘palr’ ‘pals’ ‘viridisLite’
      All declared Imports should be used.
    ```

# parsnip

Version: 0.0.2

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘earth’
    ```

# particles

Version: 0.2.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# pathlibr

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘glue’ ‘logging’ ‘purrr’ ‘R6’
      All declared Imports should be used.
    ```

# PAutilities

Version: 0.1.2

## In both

*   checking examples ... ERROR
    ```
    ...
    > ### Name: get_transition_info
    > ### Title: Convert a set of predicted and actual activity transitions to an
    > ###   object that can be analyzed
    > ### Aliases: get_transition_info
    > 
    > ### ** Examples
    > 
    > predictions <- sample(c(0,1), 100, TRUE, c(3, 1))
    > references  <- sample(c(0,1), 100, TRUE, c(4,1))
    > get_transition_info(predictions, references, 10)
    Unable to find any JVMs matching version "(null)".
    No Java runtime present, try --request to install.
    Warning in system("/usr/libexec/java_home", intern = TRUE) :
      running command '/usr/libexec/java_home' had status 1
    Error: .onLoad failed in loadNamespace() for 'rJava', details:
      call: dyn.load(file, DLLpath = DLLpath, ...)
      error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/PAutilities/rJava/libs/rJava.so':
      dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/PAutilities/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
      Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/PAutilities/rJava/libs/rJava.so
      Reason: image not found
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      3: matchingMarkets::hri at /Users/lionel/Desktop/rlang/revdep/checks.noindex/PAutilities/new/PAutilities.Rcheck/00_pkg_src/PAutilities/R/get_matchings.R:15
      4: getExportedValue(pkg, name)
      5: asNamespace(ns)
      6: getNamespace(ns)
      7: tryCatch(loadNamespace(name), error = function(e) stop(e))
      8: tryCatchList(expr, classes, parentenv, handlers)
      9: tryCatchOne(expr, names, parentenv, handlers[[1L]])
      10: value[[3L]](cond)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 6 SKIPPED: 0 FAILED: 1
      1. Error: Transition analyses produce expected output (@test_transitions.R#31) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unable to find any JVMs matching version "(null)".
    No Java runtime present, try --request to install.
    Namespace in Imports field not imported from: ‘AGread’
      All declared Imports should be used.
    ```

# perturbatr

Version: 1.2.1

## In both

*   R CMD check timed out
    

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        data   2.8Mb
    ```

# photobiology

Version: 0.9.26

## In both

*   checking whether package ‘photobiology’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘tibble’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/photobiology/new/photobiology.Rcheck/00install.out’ for details.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        R   3.1Mb
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

# pkgcache

Version: 1.0.3

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      1: read_packages_file(pf[1], mirror = "m1", repodir = "r1", platform = "source", rversion = "*") at testthat/test-packages-gz.R:88
      2: as_tibble(read.dcf.gz(path)) at /Users/lionel/Desktop/rlang/revdep/checks.noindex/pkgcache/new/pkgcache.Rcheck/00_pkg_src/pkgcache/R/packages-gz.R:19
      3: read.dcf.gz(path) at /Users/lionel/Desktop/rlang/revdep/checks.noindex/pkgcache/new/pkgcache.Rcheck/00_pkg_src/pkgcache/R/packages-gz.R:19
      4: gzfile(x, open = "r") at /Users/lionel/Desktop/rlang/revdep/checks.noindex/pkgcache/new/pkgcache.Rcheck/00_pkg_src/pkgcache/R/utils.R:32
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 282 SKIPPED: 25 FAILED: 5
      1. Error: load_primary_pkgs (@test-metadata-cache.R#177) 
      2. Error: update_replica_rds (@test-metadata-cache.R#236) 
      3. Error: read_packages_file (@test-packages-gz.R#63) 
      4. Error: packages_parse_deps (@test-packages-gz.R#71) 
      5. Error: merge_packages_data (@test-packages-gz.R#88) 
      
      Error: testthat unit tests failed
      Execution halted
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

Version: 1.0.1

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
      installed size is  7.0Mb
      sub-directories of 1Mb or more:
        htmlwidgets   3.1Mb
        R             2.3Mb
    ```

# plyranges

Version: 1.2.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
             e$handled <- TRUE
             test_error <<- e
         }, "could not find function \"WIGFile\"", quote(WIGFile(test_wig))) at testthat/test-io-wig.R:24
      2: eval(code, test_env)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 284 SKIPPED: 0 FAILED: 5
      1. Error: read_bed returns correct GRanges (@test-io-bed.R#67) 
      2. Error: read_bed_graph returns correct GRanges (@test-io-bedGraph.R#39) 
      3. Error: reading/ writing bigwig files returns correct GRanges (@test-io-bw.R#19) 
      4. Error: reading GFF files returns correct GRanges (@test-io-gff.R#87) 
      5. Error: reading WIG files (@test-io-wig.R#24) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking whether package ‘plyranges’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘GenomeInfoDb’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/plyranges/new/plyranges.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘BSgenome.Hsapiens.UCSC.hg19’
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

Version: 2.8.2

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
clang: error: unsupported option '-fopenmp'
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
clang: error: unsupported option '-fopenmp'
make: *** [adjust_missing.o] Error 1
ERROR: compilation failed for package ‘poppr’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/poppr/old/poppr.Rcheck/poppr’

```
# predict3d

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘TH.data’
      All declared Imports should be used.
    ```

# prevtoinc

Version: 0.11.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Loading required package: prevtoinc
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'prevtoinc_vignette.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

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

# processR

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘jtools’ ‘modelr’ ‘prediction’ ‘rlang’ ‘TH.data’ ‘tidyr’
      All declared Imports should be used.
    ```

# promises

Version: 1.0.1

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘purrr’
    ```

# prophet

Version: 0.4

## In both

*   checking whether package ‘prophet’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘Rcpp’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/prophet/new/prophet.Rcheck/00install.out’ for details.
    ```

# proustr

Version: 0.4.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 12717 marked UTF-8 strings
    ```

# psychmeta

Version: 2.3.2

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
      installed size is  8.9Mb
      sub-directories of 1Mb or more:
        R   7.0Mb
    ```

# PWFSLSmoke

Version: 1.1.3

## In both

*   checking whether package ‘PWFSLSmoke’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘dplyr’ was built under R version 3.5.2
      Warning: package ‘MazamaSpatialUtils’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/PWFSLSmoke/new/PWFSLSmoke.Rcheck/00install.out’ for details.
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

Version: 0.9.9

## In both

*   checking whether package ‘radiant.data’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘tidyr’ was built under R version 3.5.2
      Warning: package ‘dplyr’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/radiant.data/new/radiant.data.Rcheck/00install.out’ for details.
    ```

# radiant.model

Version: 0.9.9

## In both

*   checking whether package ‘radiant.model’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘radiant.data’ was built under R version 3.5.2
      Warning: package ‘tidyr’ was built under R version 3.5.2
      Warning: package ‘dplyr’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/radiant.model/new/radiant.model.Rcheck/00install.out’ for details.
    ```

# radiant.multivariate

Version: 0.9.9

## In both

*   checking whether package ‘radiant.multivariate’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘radiant.data’ was built under R version 3.5.2
      Warning: package ‘tidyr’ was built under R version 3.5.2
      Warning: package ‘dplyr’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/radiant.multivariate/new/radiant.multivariate.Rcheck/00install.out’ for details.
    ```

# rbgm

Version: 0.0.5

## In both

*   checking whether package ‘rbgm’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘raster’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/rbgm/new/rbgm.Rcheck/00install.out’ for details.
    ```

# rbin

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘utils’
      All declared Imports should be used.
    ```

# rclimateca

Version: 1.0.2

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 24 marked UTF-8 strings
    ```

# rdomains

Version: 0.1.7

## In both

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

Version: 0.1.5

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
      OK: 1153 SKIPPED: 9 FAILED: 1
      1. Error: printing (@test_ica.R#127) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking whether package ‘recipes’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘dplyr’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/recipes/new/recipes.Rcheck/00install.out’ for details.
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

# regrrr

Version: 0.1.0

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘spatstat’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# replyr

Version: 0.9.9

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

# restfulSE

Version: 1.4.1

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘GO.db’
    
    Packages suggested but not available for checking:
      ‘org.Mm.eg.db’ ‘org.Hs.eg.db’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# rfishbase

Version: 3.0.3

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 44 marked UTF-8 strings
    ```

# rfPermute

Version: 2.1.6

## In both

*   checking whether package ‘rfPermute’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/rfPermute/new/rfPermute.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘rfPermute’ ...
** package ‘rfPermute’ successfully unpacked and MD5 sums checked
** R
** data
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘spatstat’
ERROR: lazy loading failed for package ‘rfPermute’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/rfPermute/new/rfPermute.Rcheck/rfPermute’

```
### CRAN

```
* installing *source* package ‘rfPermute’ ...
** package ‘rfPermute’ successfully unpacked and MD5 sums checked
** R
** data
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘spatstat’
ERROR: lazy loading failed for package ‘rfPermute’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/rfPermute/old/rfPermute.Rcheck/rfPermute’

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

# Rnets

Version: 1.0.2

## In both

*   checking whether package ‘Rnets’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘igraph’ was built under R version 3.5.2
      Warning: package ‘data.table’ was built under R version 3.5.2
      Warning: package ‘stringr’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/Rnets/new/Rnets.Rcheck/00install.out’ for details.
    ```

# RNeXML

Version: 2.3.0

## In both

*   checking whether package ‘RNeXML’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘ape’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/RNeXML/new/RNeXML.Rcheck/00install.out’ for details.
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Loading required package: maps
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'simmap.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘taxadb’
    ```

# rsample

Version: 0.0.4

## In both

*   checking whether package ‘rsample’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘tidyr’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/rsample/new/rsample.Rcheck/00install.out’ for details.
    ```

# RSDA

Version: 2.0.8

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘randomcoloR’
      All declared Imports should be used.
    ```

# rsimsum

Version: 0.5.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        doc    1.0Mb
        help   3.4Mb
    ```

# RTest

Version: 1.2.3

## In both

*   checking whether package ‘RTest’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘XML’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/RTest/new/RTest.Rcheck/00install.out’ for details.
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

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ggplot2’
      All declared Imports should be used.
    ```

# scFeatureFilter

Version: 1.2.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error: processing vignette 'Introduction.Rmd' failed with diagnostics:
    there is no package called ‘BiocStyle’
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘scRNAseq’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.5Mb
      sub-directories of 1Mb or more:
        data   3.7Mb
        doc    2.4Mb
    ```

# scriptName

Version: 1.0.0

## In both

*   checking examples ... ERROR
    ```
    ...
    > ### Name: current_filename
    > ### Title: Determine a script's filename from within the script itself
    > ### Aliases: current_filename current_source_filename current_cli_filename
    > 
    > ### ** Examples
    > 
    > # Put this in example.R and try running source("example.R")
    > # and `Rscript example.R`
    > filename <- current_filename()
    Error: Predicate functions must return a single `TRUE` or `FALSE`, not a logical vector of length 0
    Backtrace:
        █
     1. └─scriptName::current_filename()
     2.   ├─current_source_filename() %||% current_cli_filename() 00_pkg_src/scriptName/R/current_filename.R:37:20
     3.   │ └─rlang::is_null(x)
     4.   └─scriptName::current_source_filename() 00_pkg_src/scriptName/R/current_filename.R:37:20
     5.     ├─purrr::pluck(closest_source_frame(), "env", "filename") 00_pkg_src/scriptName/R/current_filename.R:45:27
     6.     └─scriptName:::closest_source_frame() 00_pkg_src/scriptName/R/current_filename.R:45:27
     7.       └─purrr::detect(rlang::ctxt_stack(), ~.$fn_name == "source") 00_pkg_src/scriptName/R/current_filename.R:48:24
     8.         └─purrr:::.f(.x[[i]], ...)
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── 6. Failure: functions work under Rscript (@test.all.R#45)  ──────────────────
      run_rscript("current_source_filename.R") not identical to " NULL".
      Lengths differ: 0 is not 1
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 12 SKIPPED: 0 FAILED: 6
      1. Failure: functions work under R --file (@test.all.R#22) 
      2. Failure: functions work under R --file (@test.all.R#23) 
      3. Failure: functions work under R -f (@test.all.R#33) 
      4. Failure: functions work under R -f (@test.all.R#34) 
      5. Failure: functions work under Rscript (@test.all.R#44) 
      6. Failure: functions work under Rscript (@test.all.R#45) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# sdcTable

Version: 0.27

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
# sensobol

Version: 0.1.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'sensobol.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# SeqVarTools

Version: 1.20.2

## In both

*   checking package dependencies ... ERROR
    ```
    Packages required but not available:
      ‘SeqArray’ ‘Biobase’ ‘gdsfmt’ ‘GenomicRanges’ ‘IRanges’
      ‘S4Vectors’ ‘GWASExactHW’ ‘logistf’ ‘dplyr’ ‘tidyr’
    
    Packages suggested but not available for checking:
      ‘BiocGenerics’ ‘BiocStyle’ ‘RUnit’ ‘stringr’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# servosphereR

Version: 0.1.0

## In both

*   checking package dependencies ... ERROR
    ```
    Packages required but not available:
      ‘purrr’ ‘data.table’ ‘dplyr’ ‘magrittr’
    
    Packages suggested but not available for checking:
      ‘knitr’ ‘rmarkdown’ ‘testthat’
    
    VignetteBuilder package required for checking but not installed: ‘knitr’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# sf

Version: 0.7-3

## In both

*   checking package dependencies ... ERROR
    ```
    Packages required but not available:
      ‘classInt’ ‘DBI’ ‘magrittr’ ‘Rcpp’ ‘units’
    
    Packages suggested but not available for checking:
      ‘blob’ ‘covr’ ‘dplyr’ ‘ggplot2’ ‘knitr’ ‘lwgeom’ ‘maps’
      ‘maptools’ ‘mapview’ ‘microbenchmark’ ‘odbc’ ‘pillar’ ‘pool’
      ‘raster’ ‘rgdal’ ‘rgeos’ ‘rmarkdown’ ‘RPostgres’ ‘RPostgreSQL’
      ‘RSQLite’ ‘sp’ ‘spatstat’ ‘stars’ ‘testthat’ ‘tibble’ ‘tidyr’
      ‘tidyselect’ ‘tmap’
    
    VignetteBuilder package required for checking but not installed: ‘knitr’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# shiny

Version: 1.2.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 12.9Mb
      sub-directories of 1Mb or more:
        R     4.0Mb
        www   7.9Mb
    ```

# simglm

Version: 0.7.2

## In both

*   checking package dependencies ... ERROR
    ```
    Packages required but not available: ‘dplyr’ ‘purrr’ ‘broom’
    
    Packages suggested but not available for checking:
      ‘knitr’ ‘lme4’ ‘testthat’ ‘shiny’ ‘e1071’ ‘ggplot2’ ‘tidyr’
      ‘geepack’ ‘rmarkdown’
    
    VignetteBuilder package required for checking but not installed: ‘knitr’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# SimRVSequences

Version: 0.1.2

## In both

*   checking package dependencies ... ERROR
    ```
    Packages required but not available:
      ‘SimRVPedigree’ ‘kinship2’ ‘intervals’ ‘reshape2’ ‘dplyr’
      ‘magrittr’
    
    Packages suggested but not available for checking:
      ‘knitr’ ‘rmarkdown’ ‘testthat’
    
    VignetteBuilder package required for checking but not installed: ‘knitr’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# SingleCaseES

Version: 0.4.1

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

# sjPlot

Version: 2.6.2

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘snakecase’
    ```

# sjstats

Version: 0.17.4

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘arm’
    ```

# skimr

Version: 1.0.5

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘dtplyr’, ‘dbplyr’
    ```

# sparklyr

Version: 1.0.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.8Mb
      sub-directories of 1Mb or more:
        java   1.5Mb
        R      4.1Mb
    ```

# spdplyr

Version: 0.2.0

## In both

*   checking whether package ‘spdplyr’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘dplyr’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/spdplyr/new/spdplyr.Rcheck/00install.out’ for details.
    ```

# stars

Version: 0.3-0

## In both

*   checking whether package ‘stars’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘sf’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/stars/new/stars.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘starsdata’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 15.5Mb
      sub-directories of 1Mb or more:
        doc  10.3Mb
        nc    3.5Mb
    ```

# stlcsb

Version: 0.1.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tibble’
      All declared Imports should be used.
    ```

# stplanr

Version: 0.2.8

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
    ...
    Warning in igraph::get.shortest.paths(sln@g, x, output = "epath") :
      At structural_properties.c:4597 :Couldn't reach some vertices
    Warning in igraph::get.shortest.paths(sln@g, x, output = "epath") :
      At structural_properties.c:4597 :Couldn't reach some vertices
    Warning in igraph::get.shortest.paths(sln@g, x, output = "epath") :
      At structural_properties.c:4597 :Couldn't reach some vertices
    Warning in igraph::get.shortest.paths(sln@g, x, output = "epath") :
      At structural_properties.c:4597 :Couldn't reach some vertices
    Warning in igraph::get.shortest.paths(sln@g, x, output = "epath") :
      At structural_properties.c:4597 :Couldn't reach some vertices
    Warning in igraph::get.shortest.paths(sln@g, x, output = "epath") :
      At structural_properties.c:4597 :Couldn't reach some vertices
    Warning in igraph::get.shortest.paths(sln@g, x, output = "epath") :
      At structural_properties.c:4597 :Couldn't reach some vertices
    Transforming to CRS +proj=aeqd +lat_0=53.81907246 +lon_0=-1.52841182 +x_0=0 +y_0=0 +ellps=WGS84
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'stplanr-paper.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# strex

Version: 0.1.3

## In both

*   checking whether package ‘strex’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘stringr’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/strex/new/strex.Rcheck/00install.out’ for details.
    ```

# sugrrants

Version: 0.2.3

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Warning: package 'tidyr' was built under R version 3.5.2
    Warning: package 'dplyr' was built under R version 3.5.2
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Loading required package: viridisLite
    Loading required package: ggplot2
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'frame-calendar.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# SummarizedBenchmark

Version: 2.0.1

## In both

*   checking whether package ‘SummarizedBenchmark’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘tidyr’ was built under R version 3.5.2
      Warning: package ‘GenomeInfoDb’ was built under R version 3.5.2
      Warning: package ‘BiocParallel’ was built under R version 3.5.2
      Warning: package ‘stringr’ was built under R version 3.5.2
      Warning: package ‘dplyr’ was built under R version 3.5.2
      Warning: package ‘tibble’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/SummarizedBenchmark/new/SummarizedBenchmark.Rcheck/00install.out’ for details.
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    ==================================================
    downloaded 1.6 MB
    
    trying URL 'https://www.ebi.ac.uk/arrayexpress/files/E-MTAB-4119/E-MTAB-4119.processed.2.zip'
    Content type 'application/zip' length 541401 bytes (528 KB)
    ==================================================
    downloaded 528 KB
    
    
    Attaching package: 'rnaseqcomp'
    
    The following object is masked from 'package:SummarizedBenchmark':
    
        plotROC
    
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'QuantificationBenchmark.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘scRNAseq’
    
    Depends: includes the non-default packages:
      ‘tidyr’ ‘SummarizedExperiment’ ‘S4Vectors’ ‘BiocGenerics’
      ‘UpSetR’ ‘rlang’ ‘stringr’ ‘BiocParallel’ ‘ggplot2’ ‘mclust’
      ‘dplyr’ ‘digest’ ‘sessioninfo’ ‘crayon’ ‘tibble’
    Adding so many packages to the search path is excessive and
    importing selectively is preferable.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 16.7Mb
      sub-directories of 1Mb or more:
        data  11.1Mb
        doc    3.4Mb
        R      2.0Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Package in Depends field not imported from: ‘BiocGenerics’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    Unexported object imported by a ':::' call: ‘BiocGenerics:::replaceSlots’
      See the note in ?`:::` about the use of this operator.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/SummarizedBenchmark/new/SummarizedBenchmark.Rcheck/00_pkg_src/SummarizedBenchmark/R/BDData-show.R:11)
    show,BDMethodList: no visible global function definition for ‘head’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/SummarizedBenchmark/new/SummarizedBenchmark.Rcheck/00_pkg_src/SummarizedBenchmark/R/BDMethodList-show.R:8)
    show,BDMethodList: no visible global function definition for ‘head’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/SummarizedBenchmark/new/SummarizedBenchmark.Rcheck/00_pkg_src/SummarizedBenchmark/R/BDMethodList-show.R:10)
    show,BenchDesign: no visible global function definition for ‘head’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/SummarizedBenchmark/new/SummarizedBenchmark.Rcheck/00_pkg_src/SummarizedBenchmark/R/BenchDesign-show.R:19)
    show,BenchDesign: no visible global function definition for ‘head’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/SummarizedBenchmark/new/SummarizedBenchmark.Rcheck/00_pkg_src/SummarizedBenchmark/R/BenchDesign-show.R:21)
    show,BenchDesign: no visible global function definition for ‘head’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/SummarizedBenchmark/new/SummarizedBenchmark.Rcheck/00_pkg_src/SummarizedBenchmark/R/BenchDesign-show.R:23-28)
    Undefined global functions or variables:
      . .id .method .val .valueClassTest colid comparison cor f FDR head
      label meta metadata method overlap params post rerun sd slot TPR
      value
    Consider adding
      importFrom("methods", ".valueClassTest", "slot")
      importFrom("stats", "cor", "sd")
      importFrom("utils", "head")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports
    field contains 'methods').
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

# tabula

Version: 1.2.0

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
    Namespace in Imports field not imported from: ‘plyr’
      All declared Imports should be used.
    ```

# tabularaster

Version: 0.5.0

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘spatstat’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# taxa

Version: 0.3.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.2Mb
      sub-directories of 1Mb or more:
        data   2.0Mb
        doc    1.7Mb
        R      2.1Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘knitr’ ‘lazyeval’ ‘rlang’ ‘tidyr’
      All declared Imports should be used.
    ```

# testextra

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘stringi’ ‘utils’
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

*   checking whether package ‘textrecipes’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘dplyr’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/textrecipes/new/textrecipes.Rcheck/00install.out’ for details.
    ```

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

Version: 1.0.4

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
      OK: 227 SKIPPED: 43 FAILED: 1
      1. Error: tidy_draws works with runjags (@test.tidy_draws.R#87) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

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
      OK: 37 SKIPPED: 0 FAILED: 1
      1. Error: right number of columns given (@test-errors.R#10) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking whether package ‘tidyinftheo’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘tibble’ was built under R version 3.5.2
      Warning: package ‘tidyr’ was built under R version 3.5.2
      Warning: package ‘dplyr’ was built under R version 3.5.2
      Warning: package ‘stringr’ was built under R version 3.5.2
      Warning: package ‘forcats’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/tidyinftheo/new/tidyinftheo.Rcheck/00install.out’ for details.
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

*   checking whether package ‘tidymodels’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘dplyr’ was built under R version 3.5.2
      Warning: package ‘rsample’ was built under R version 3.5.2
      Warning: package ‘tidyr’ was built under R version 3.5.2
      Warning: package ‘tibble’ was built under R version 3.5.2
      Warning: package ‘yardstick’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/tidymodels/new/tidymodels.Rcheck/00install.out’ for details.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘broom’ ‘dials’ ‘parsnip’
      All declared Imports should be used.
    ```

# tidypredict

Version: 0.3.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(tidypredict)
      > 
      > test_check("tidypredict")
      ── 1. Error: (unknown) (@test-earth.R#3)  ──────────────────────────────────────
      there is no package called 'earth'
      1: data("etitanic", package = "earth") at testthat/test-earth.R:3
      2: find.package(package, lib.loc, verbose = verbose)
      3: stop(gettextf("there is no package called %s", sQuote(pkg)), domain = NA)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 23 SKIPPED: 0 FAILED: 1
      1. Error: (unknown) (@test-earth.R#3) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘earth’
    ```

# tidyquant

Version: 0.5.5

## In both

*   checking whether package ‘tidyquant’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘tibble’ was built under R version 3.5.2
      Warning: package ‘tidyr’ was built under R version 3.5.2
      Warning: package ‘dplyr’ was built under R version 3.5.2
      Warning: package ‘stringr’ was built under R version 3.5.2
      Warning: package ‘forcats’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/tidyquant/new/tidyquant.Rcheck/00install.out’ for details.
    ```

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

Version: 0.8.3

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

Version: 0.3.8

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

*   checking whether package ‘tidyverse’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘tibble’ was built under R version 3.5.2
      Warning: package ‘tidyr’ was built under R version 3.5.2
      Warning: package ‘dplyr’ was built under R version 3.5.2
      Warning: package ‘stringr’ was built under R version 3.5.2
      Warning: package ‘forcats’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/tidyverse/new/tidyverse.Rcheck/00install.out’ for details.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dbplyr’ ‘reprex’ ‘rlang’
      All declared Imports should be used.
    ```

# tidyxl

Version: 1.0.4

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

# trackr

Version: 0.10.5

## In both

*   checking whether package ‘trackr’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘knitr’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/trackr/new/trackr.Rcheck/00install.out’ for details.
    ```

# treeio

Version: 1.6.2

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

# trread

Version: 0.2.7

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
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

# tsibble

Version: 0.7.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    The following object is masked from 'package:stats':
    
        filter
    
    
    Attaching package: 'lubridate'
    
    The following objects are masked from 'package:tsibble':
    
        interval, new_interval
    
    The following object is masked from 'package:base':
    
        date
    
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

# unpivotr

Version: 0.5.0

## In both

*   checking examples ... ERROR
    ```
    ...
    > ##D browseURL(nestedspan)
    > ## End(Not run)
    > 
    > as_cells(xml2::read_html(colspan))
    Error: Columns 1, 2 must not have names of the form ... or ..j.
    Backtrace:
         █
      1. ├─unpivotr::as_cells(xml2::read_html(colspan))
      2. └─unpivotr:::as_cells.xml_document(xml2::read_html(colspan)) 00_pkg_src/unpivotr/R/as_cells.R:88:2
      3.   └─base::lapply(tables, as_cells) 00_pkg_src/unpivotr/R/as_cells.R:212:2
      4.     ├─unpivotr:::FUN(X[[i]], ...)
      5.     └─unpivotr:::as_cells.xml_node(X[[i]], ...) 00_pkg_src/unpivotr/R/as_cells.R:88:2
      6.       └─tibble::as_data_frame(out) 00_pkg_src/unpivotr/R/as_cells.R:201:2
      7.         ├─tibble::as_tibble(x, ...)
      8.         └─tibble:::as_tibble.list(x, ...)
      9.           └─tibble:::lst_to_tibble(x, .rows, .name_repair, col_lengths(x))
     10.             └─tibble:::set_repaired_names(x, .name_repair)
     11.               ├─rlang::set_names(x, repaired_names(names(x), .name_repair = .name_repair))
     12.               │ └─rlang:::set_names_impl(x, x, nm, ...)
     13.               │   └─r
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      19: set_names(x, repaired_names(names(x), .name_repair = .name_repair))
      20: set_names_impl(x, x, nm, ...) at /private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/RtmpWzsDrT/R.INSTALLd481114fc489/rlang/R/attr.R:163
      21: is_function(nm) at /private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/RtmpWzsDrT/R.INSTALLd481114fc489/rlang/R/attr.R:170
      22: is_closure(x) at /private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/RtmpWzsDrT/R.INSTALLd481114fc489/rlang/R/fn.R:296
      23: repaired_names(names(x), .name_repair = .name_repair)
      24: check_unique(new_name)
      25: abort(error_column_must_not_be_dot_dot(dot_dot_name))
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 246 SKIPPED: 0 FAILED: 2
      1. Error: as_cells() works with html tables (@test-as_cells.R#45) 
      2. Error: tidy_table works with html tables (@test-tidy_table.R#45) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
      ...
    Quitting from lines 37-49 (html.Rmd) 
    Error: processing vignette 'html.Rmd' failed with diagnostics:
    Columns 1, 2 must not have names of the form ... or ..j.
    Backtrace:
         █
      1. ├─tools::buildVignettes(dir = "/Users/lionel/Desktop/rlang/revdep/checks.noindex/unpivotr/new/unpivotr.Rcheck/vign_test/unpivotr")
      2. │ ├─base::tryCatch(...)
      3. │ │ └─base:::tryCatchList(expr, classes, parentenv, handlers)
      4. │ │   └─base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
      5. │ │     └─base:::doTryCatch(return(expr), name, parentenv, handler)
      6. │ └─engine$weave(file, quiet = quiet, encoding = enc)
      7. │   └─knitr:::vweave_rmarkdown(...)
      8. │     └─rmarkdown::render(...)
      9. │       └─knitr::knit(...)
     10. │         └─knitr:::process_file(text, output)
     11. │           ├─base::withCallingHandlers(...)
     12. │           ├─knitr:::process_group(group)
     13. │           └─knitr:::process_group.block(group)
     14. │             └─k
    Execution halted
    ```

# vdiffr

Version: 0.3.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘freetypeharfbuzz’
      All declared Imports should be used.
    ```

# VWPre

Version: 1.1.0

## In both

*   checking whether package ‘VWPre’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘dplyr’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/VWPre/new/VWPre.Rcheck/00install.out’ for details.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        data   3.1Mb
        doc    1.3Mb
    ```

# weathercan

Version: 0.2.8

## In both

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

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dbplyr’
      All declared Imports should be used.
    ```

# xpose

Version: 0.4.4

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.7Mb
      sub-directories of 1Mb or more:
        doc    2.9Mb
        help   1.1Mb
        R      2.1Mb
    ```

# ZipRadius

Version: 1.0.1

## In both

*   checking whether package ‘ZipRadius’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘dplyr’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/ZipRadius/new/ZipRadius.Rcheck/00install.out’ for details.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ggplot2’ ‘testthat’
      All declared Imports should be used.
    ```

