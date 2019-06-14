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

Version: 0.7.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.1Mb
      sub-directories of 1Mb or more:
        data   3.8Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘scales’
      All declared Imports should be used.
    ```

# amt

Version: 0.0.6

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘ctmm’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# analysisPipelines

Version: 1.0.0

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘analysisPipelines-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: correlationMatPlot
    > ### Title: Correlation Matrix Plot
    > ### Aliases: correlationMatPlot
    > 
    > ### ** Examples
    > 
    > correlationMatPlot(dataset = iris)
    Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
      there is no package called ‘R.utils’
    Calls: correlationMatPlot ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      'SparkR', 'reticulate'
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
      installed size is  5.4Mb
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

Version: 0.3.2

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 552 marked UTF-8 strings
    ```

# AzureKusto

Version: 1.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘jsonlite’ ‘tibble’
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

Version: 0.1.3

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.7Mb
      sub-directories of 1Mb or more:
        libs   4.8Mb
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
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘BayesMallowsPackage.Rmd’ using rmarkdown
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'BayesMallowsPackage.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    --- failed re-building ‘BayesMallowsPackage.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘BayesMallowsPackage.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# bayesplot

Version: 1.7.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.8Mb
      sub-directories of 1Mb or more:
        doc   4.1Mb
        R     2.1Mb
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

# bcrm

Version: 0.5.3

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘BRugs’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘knitr’ ‘rlang’
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
      'BSgenome.Hsapiens.UCSC.hg19',
      'TxDb.Hsapiens.UCSC.hg19.knownGene', 'EnsDb.Hsapiens.v75'
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

# cartograflow

Version: 1.0.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.0Mb
      sub-directories of 1Mb or more:
        doc   4.4Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘g.data’
      All declared Imports should be used.
    ```

# catchr

Version: 0.2.0

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      27: abort(sprintf("`fn` must be an R function, not %s", friendly_type_of(x))) at /private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/Rtmp3Ij4Yr/R.INSTALL917422a6e5c/rlang/R/fn.R:143
      
      simpleWarning in `force(expr)`: internal
      simpleWarning in `force(expr)`: internal
      ── 2. Failure: Ordered handlers respects order when with_handlers doesn't (@test
      `test_val` not equal to NULL.
      Types not compatible: character is not NULL
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 123 SKIPPED: 0 WARNINGS: 0 FAILED: 2
      1. Error: Explictly package-named functions (@test-testing.R#103) 
      2. Failure: Ordered handlers respects order when with_handlers doesn't (@test-testing.R#293) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# celaref

Version: 1.0.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Attaching package: 'AnnotationHub'
    
    The following object is masked from 'package:Biobase':
    
        cache
    
    snapshotDate(): 2019-04-29
    snapshotDate(): 2019-04-29
    Bioconductor version 3.9 (BiocManager 1.30.4), R 3.6.0 (2019-04-26)
    Installing package(s) 'BiocVersion', 'celarefData'
    trying URL 'https://bioconductor.org/packages/3.9/bioc/bin/macosx/el-capitan/contrib/3.6/BiocVersion_3.9.0.tgz'
    Content type 'application/x-gzip' length 5513 bytes
    ==================================================
    downloaded 5513 bytes
    
    installing the source package 'celarefData'
    
    trying URL 'https://bioconductor.org/packages/3.9/data/experiment/src/contrib/celarefData_1.2.0.tar.gz'
    Content type 'application/x-gzip' length 234373 bytes (228 KB)
    ==================================================
    downloaded 228 KB
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

# chillR

Version: 0.70.17

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘R.utils’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# circumplex

Version: 0.3.1

## In both

*   checking whether package ‘circumplex’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/circumplex/new/circumplex.Rcheck/00install.out’ for details.
    ```

# clustree

Version: 0.4.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > 
      > test_check("clustree")
      ── 1. Error: show_axis works (@test-clustree.R#91)  ────────────────────────────
      object 'seurat' not found
      1: expect_is(clustree(seurat, prefix = "res.", show_axis = TRUE), c("gg", "ggplot")) at testthat/test-clustree.R:91
      2: quasi_label(enquo(object), label, arg = "object")
      3: eval_bare(get_expr(quo), get_env(quo))
      4: clustree(seurat, prefix = "res.", show_axis = TRUE) at /private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/Rtmp3Ij4Yr/R.INSTALL917422a6e5c/rlang/R/eval.R:99
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 48 SKIPPED: 4 WARNINGS: 0 FAILED: 1
      1. Error: show_axis works (@test-clustree.R#91) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘clustree.Rmd’ using rmarkdown
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'clustree.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    --- failed re-building ‘clustree.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘clustree.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# clustringr

Version: 1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘stringi’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 5626 marked UTF-8 strings
    ```

# codebook

Version: 0.8.1

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Running examples in ‘codebook-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: codebook
    > ### Title: Generate rmarkdown codebook
    > ### Aliases: codebook
    > 
    > ### ** Examples
    > 
    > # will generate figures in a temporary directory
    > old_base_dir <- knitr::opts_knit$get("base.dir")
    > knitr::opts_knit$set(base.dir = tempdir())
    > on.exit(knitr::opts_knit$set(base.dir = old_base_dir))
    > data("bfi")
    > bfi <- bfi[, c("BFIK_open_1", "BFIK_open_1")]
    > md <- codebook(bfi, survey_repetition = "single", metadata_table = FALSE)
    No missing values.
    Error in env_bind(mask, ... = env_get(current_env(), "...")) : 
      could not find function "env_bind"
    Calls: codebook ... skim.data.frame -> <Anonymous> -> .f -> <Anonymous> -> .f -> .x
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Quitting from lines 86-87 (codebook.Rmd) 
    Error: processing vignette 'codebook.Rmd' failed with diagnostics:
    Could not summarise item age. Error in env_bind(mask, ... = env_get(current_env(), "...")): could not find function "env_bind"
    
    --- failed re-building ‘codebook.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘codebook.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘graphics’ ‘jsonlite’ ‘pander’ ‘rlang’
      All declared Imports should be used.
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
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘SimpleCOMPASS.Rmd’ using rmarkdown
    Quitting from lines 39-41 (SimpleCOMPASS.Rmd) 
    Error: processing vignette 'SimpleCOMPASS.Rmd' failed with diagnostics:
    there is no package called 'readxl'
    --- failed re-building ‘SimpleCOMPASS.Rmd’
    
    --- re-building ‘COMPASS.Rmd’ using docco_linear
    --- finished re-building ‘COMPASS.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘SimpleCOMPASS.Rmd’
    
    Error: Vignette re-building failed.
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

# compstatr

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tibble’
      All declared Imports should be used.
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
      OK: 125 SKIPPED: 0 WARNINGS: 2 FAILED: 1
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

Version: 1.0.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘memoise’
      All declared Imports should be used.
    ```

# corrr

Version: 0.3.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rlang’
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

# cytominer

Version: 0.1.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      46: FUN(X[[i]], ...)
      47: partial_eval_call(get_expr(call), vars, get_env(call))
      48: as_string(call[[1]])
      49: abort_coercion(x, "a string") at /private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/Rtmp3Ij4Yr/R.INSTALL917422a6e5c/rlang/R/sym.R:108
      50: abort(paste0("Can't convert ", x_type, " to ", to_type)) at /private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/Rtmp3Ij4Yr/R.INSTALL917422a6e5c/rlang/R/utils-conditions.R:7
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 20 SKIPPED: 0 WARNINGS: 22 FAILED: 2
      1. Error: `aggregate` aggregates data (@test-aggregate.R#15) 
      2. Error: `generalized_log` generalized_logs data (@test-generalized_log.R#16) 
      
      Error: testthat unit tests failed
      In addition: Warning message:
      call dbDisconnect() when finished working with a connection 
      Execution halted
    ```

# dabestr

Version: 0.2.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    --- failed re-building ‘robust-statistical-visualization.Rmd’
    
    --- re-building ‘using-dabestr.Rmd’ using rmarkdown
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    --- finished re-building ‘using-dabestr.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘robust-statistical-visualization.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.5Mb
      sub-directories of 1Mb or more:
        doc   5.8Mb
    ```

# dbplot

Version: 0.3.1

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
          intersect, setdiff, setequal, union
      
      > library(ggplot2)
      > 
      > test_check("dbplot")
      ── 1. Failure: No error or warning when translated to SQL  ─────────────────────
      `dbplyr::translate_sql(...)` produced warnings.
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 23 SKIPPED: 0 WARNINGS: 1 FAILED: 1
      1. Failure: No error or warning when translated to SQL 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# dbplyr

Version: 1.4.1

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘dtplyr’
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

Version: 0.1.3

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
                 }
             }
         }, args = c("x"), arguments_as_values = F, exclude_args = NULL), "character") at testthat/test_helpers.R:37
      2: quasi_label(enquo(object), label, arg = "object")
      3: force(quo)
      4: enquo(object)
      5: rlang::abort(x) at /private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/Rtmp3Ij4Yr/R.INSTALL917422a6e5c/rlang/R/quotation.R:293
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 309 SKIPPED: 0 WARNINGS: 5 FAILED: 2
      1. Error: internal helpers for when source code is missing work (@test_helpers.R#31) 
      2. Error: construct_design_code works as it should when source is missing (@test_helpers.R#37) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# detrendr

Version: 0.6.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      y[1]: "2ch_ij_detrended_thresh=Triangle=0.6,Triangle=0.6_exponential_for_FFS_tau
      y[1]: =auto=NA,auto=6.1767578125.tif"
      
      x[2]: "bleached_detrended_thresh=Triangle=41.622_exponential_for_FFS_tau=auto=22
      x[2]: .0703125.tif"
      y[2]: "bleached_detrended_thresh=Triangle=41.622_exponential_for_FFS_tau=auto=20
      y[2]: .703125.tif"
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 207 SKIPPED: 0 WARNINGS: 1 FAILED: 2
      1. Failure: detrending entire derectories works (@test-dir_detrend.R#63) 
      2. Failure: detrending entire derectories works (@test-dir_detrend.R#93) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# dexter

Version: 0.8.5

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'profile-plots.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    --- failed re-building ‘profile-plots.Rmd’
    
    --- re-building ‘Test_Individual_differences.Rmd’ using rmarkdown
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'Test_Individual_differences.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    --- failed re-building ‘Test_Individual_differences.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘dexter.Rmd’ ‘Equating.Rmd’ ‘Plausible_Values.Rmd’
      ‘profile-plots.Rmd’ ‘Test_Individual_differences.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# dextergui

Version: 0.1.6

## In both

*   checking dependencies in R code ... NOTE
    ```
    Unexported objects imported by ':::' calls:
      ‘dexter:::get_resp_data’ ‘dexter:::qcolors’
      See the note in ?`:::` about the use of this operator.
    ```

# DiagrammeR

Version: 1.0.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.9Mb
      sub-directories of 1Mb or more:
        htmlwidgets   3.0Mb
        R             2.0Mb
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

Version: 0.0.4

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘magrittr’
      All declared Imports should be used.
    ```

# dotwhisker

Version: 0.5.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Loading required package: ggplot2
    Warning: `legend.margin` must be specified using `margin()`. For the old behavior use legend.spacing
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'kl2007_examples.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    --- failed re-building ‘kl2007_examples.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘dotwhisker-vignette.Rmd’ ‘kl2007_examples.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# dplyr

Version: 0.8.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        libs   2.9Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4 marked UTF-8 strings
    ```

# echor

Version: 0.1.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘lubridate’
      All declared Imports should be used.
    ```

# emuR

Version: 1.1.2

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    --- re-building ‘EQL.Rmd’ using rmarkdown
    
    Attaching package: 'emuR'
    
    The following object is masked from 'package:base':
    
        norm
    
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'EQL.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    --- failed re-building ‘EQL.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘emuDB.Rmd’ ‘emuR_intro.Rmd’ ‘EQL.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.1Mb
      sub-directories of 1Mb or more:
        doc       1.2Mb
        extdata   1.5Mb
        R         2.0Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘git2r’ ‘servr’
      All declared Imports should be used.
    ```

# enc

Version: 0.2.0

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'to_encoding.Rd':
      ‘[rlang:mut_latin1_locale]{rlang::mut_latin1_locale()}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

# encryptr

Version: 0.1.3

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

Version: 3.10.4

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘Rmpi’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        doc   1.6Mb
        R     2.0Mb
    ```

# estimatr

Version: 0.18.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘texreg’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘texreg’
    ```

# EventStudy

Version: 0.36

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
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

# finalfit

Version: 0.9.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘readr’
      All declared Imports should be used.
    ```

# fingertipscharts

Version: 0.0.6

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘curl’ ‘mapproj’
      All declared Imports should be used.
    ```

# fishtree

Version: 0.3.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘memoise’
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
    Error in get(fun, mode = "function", envir = envir) : 
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

# gestalt

Version: 0.1.7

## Newly broken

*   checking examples ... WARNING
    ```
    Found the following significant warnings:
    
      Warning: Passing an environment as data mask is deprecated.
    Deprecated functions may be defunct as soon as of the next release
    of R.
    See ?Deprecated.
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

*   checking whether the package can be loaded ... ERROR
    ```
    ...
    
        anyDuplicated, append, as.data.frame, basename, cbind, colMeans,
        colnames, colSums, dirname, do.call, duplicated, eval, evalq,
        Filter, Find, get, grep, grepl, intersect, is.unsorted, lapply,
        lengths, Map, mapply, match, mget, order, paste, pmax, pmax.int,
        pmin, pmin.int, Position, rank, rbind, Reduce, rowMeans, rownames,
        rowSums, sapply, setdiff, sort, table, tapply, union, unique,
        unsplit, which, which.max, which.min
    
    Loading required package: ggplot2
    From .checkSubclasses(): subclass "GeneNameFilter" of class "AnnotationFilter" is not local and is not updated for new inheritance information currently; 
    [where=<environment: 0x7f948878dd40>, where2=<environment: namespace:ggbio>]
    From .checkSubclasses(): subclass "GeneNameFilter" of class "AnnotationFilter" is not local and is not updated for new inheritance information currently; 
    [where=<environment: 0x7f948878dd40>, where2=<environment: namespace:ggbio>]
    Creating a new generic function for 'rescale' in package 'ggbio'
    Creating a new generic function for 'xlim' in package 'ggbio'
    Error in reconcilePropertiesAndPrototype(name, slots, prototype, superClasses,  : 
      The prototype for class "GGbio" has undefined slot(s): 'fechable'
    Error: package or namespace load failed for 'ggbio':
     unable to load R code in package 'ggbio'
    Execution halted
    ```

*   checking whether package ‘ggbio’ can be installed ... WARNING
    ```
    Found the following significant warnings:
        The prototype for class "GGbio" has undefined slot(s): 'fechable'
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/ggbio/new/ggbio.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      'BSgenome.Hsapiens.UCSC.hg19', 'Homo.sapiens',
      'TxDb.Hsapiens.UCSC.hg19.knownGene',
      'TxDb.Mmusculus.UCSC.mm9.knownGene', 'EnsDb.Hsapiens.v75'
    ```

# ggcyto

Version: 1.10.2

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      8: loadNamespace(name)
      9: namespaceImportFrom(ns, loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]), 
             i[[2L]], from = package)
      10: asNamespace(ns)
      11: loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]])
      12: withRestarts(stop(cond), retry_loadNamespace = function() NULL)
      13: withOneRestart(expr, restarts[[1L]])
      14: doWithOneRestart(return(expr), restart)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 3 SKIPPED: 28 WARNINGS: 0 FAILED: 1
      1. Error: fs (@test-ggcyto-fs.R#55) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Loading required package: ncdfFlow
    Loading required package: RcppArmadillo
    Loading required package: BH
    Loading required package: flowWorkspace
    loading R object...
    loading tree object...
    Done
    Warning: Removed 1 rows containing missing values (geom_hex).
    Warning: Removed 6 rows containing missing values (geom_hex).
    Warning: Removed 11920 rows containing non-finite values (stat_binhex).
    Warning: Removed 14 rows containing missing values (geom_hex).
    Quitting from lines 100-115 (Top_features_of_ggcyto.Rmd) 
    Error: processing vignette 'Top_features_of_ggcyto.Rmd' failed with diagnostics:
    there is no package called 'R.utils'
    --- failed re-building ‘Top_features_of_ggcyto.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘ggcyto.flowSet.Rmd’ ‘Top_features_of_ggcyto.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.1Mb
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

# ggformula

Version: 0.9.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        doc   2.7Mb
        R     2.0Mb
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

# ggpage

Version: 0.2.3

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘tidytext’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# ggplot2

Version: 3.1.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        doc   1.8Mb
        R     2.1Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘mgcv’ ‘reshape2’ ‘viridisLite’
      All declared Imports should be used.
    ```

# ggpmisc

Version: 0.3.1

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

Version: 0.1.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘colourpicker’ ‘dplyr’ ‘DT’ ‘Formula’ ‘GGally’ ‘ggpmisc’ ‘ggpubr’
      ‘ggrepel’ ‘ggstance’ ‘grDevices’ ‘gridExtra’ ‘Hmisc’ ‘lazyeval’
      ‘markdown’ ‘plotly’ ‘quantreg’ ‘rlang’ ‘shinyjs’ ‘survminer’ ‘table1’
      ‘tidyr’
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

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### Aliases: ggbetweenstats
    > 
    > ### ** Examples
    > 
    > 
    > # to get reproducible results from bootstrapping
    > set.seed(123)
    > 
    > # simple function call with the defaults
    > ggstatsplot::ggbetweenstats(
    +   data = mtcars,
    +   x = am,
    +   y = mpg,
    +   title = "Fuel efficiency by type of car transmission",
    +   caption = "Transmission (0 = automatic, 1 = manual)",
    +   bf.message = TRUE
    + )
    Error in env_bind(mask, ... = env_get(current_env(), "...")) : 
      could not find function "env_bind"
    Calls: <Anonymous> ... skim.data.frame -> <Anonymous> -> .f -> <Anonymous> -> .f -> .x
    Execution halted
    ```

# ggthemes

Version: 4.2.0

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
    ...
    Warning in grid.Call.graphics(C_setviewport, vp, TRUE) :
      cannot clip to rotated viewport
    Warning in grid.Call.graphics(C_setviewport, vp, TRUE) :
      cannot clip to rotated viewport
    Warning in grid.Call.graphics(C_setviewport, vp, TRUE) :
      cannot clip to rotated viewport
    Scale for 'x' is already present. Adding another scale for 'x', which
    will replace the existing scale.
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'treeVisualization.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    --- failed re-building ‘treeVisualization.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘ggtree.Rmd’ ‘treeAnnotation.Rmd’ ‘treeManipulation.Rmd’
      ‘treeVisualization.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  9.4Mb
      sub-directories of 1Mb or more:
        doc        4.9Mb
        examples   3.7Mb
    ```

# glmSparseNet

Version: 1.0.0

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘loose.rock’
    
    Package suggested but not available for checking: ‘curatedTCGAData’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
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

Version: 1.7.1.2

## In both

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      'taxstats', 'taxstats1516'
    ```

# gravity

Version: 0.9.8

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Warning: step size truncated due to increasing deviance
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'crash-course-on-gravity-models.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    --- failed re-building ‘crash-course-on-gravity-models.Rmd’
    
    --- re-building ‘creating-gravity-datasets.Rmd’ using rmarkdown
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'creating-gravity-datasets.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    --- failed re-building ‘creating-gravity-datasets.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘crash-course-on-gravity-models.Rmd’ ‘creating-gravity-datasets.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# groupedstats

Version: 0.0.7

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    The error most likely occurred in:
    
    > ### Name: grouped_summary
    > ### Title: Function to get descriptive statistics for multiple variables
    > ###   for all grouping variable levels
    > ### Aliases: grouped_summary
    > 
    > ### ** Examples
    > 
    > 
    > # another possibility
    > groupedstats::grouped_summary(
    +   data = datasets::iris,
    +   grouping.vars = Species,
    +   measures = Sepal.Length:Petal.Width,
    +   measures.type = "numeric"
    + )
    Error in env_bind(mask, ... = env_get(current_env(), "...")) : 
      could not find function "env_bind"
    Calls: <Anonymous> ... skim.data.frame -> <Anonymous> -> .f -> <Anonymous> -> .f -> .x
    Execution halted
    ```

# GSODR

Version: 1.3.2

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘R.utils’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# helixvis

Version: 1.0.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘intro.Rmd’ using rmarkdown
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'intro.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    --- failed re-building ‘intro.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘intro.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# highcharter

Version: 0.7.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.9Mb
      sub-directories of 1Mb or more:
        doc           3.7Mb
        htmlwidgets   4.0Mb
    ```

# Hmisc

Version: 4.2-0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.7Mb
      sub-directories of 1Mb or more:
        help   2.1Mb
        R      3.0Mb
    ```

# hms

Version: 0.4.2

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      y[4]: " 00\033[90m'\033[39m00.000\033[90m\"\033[39m"
      
      x[5]: " 00\033[38;5;246m'\033[39m00.001\033[38;5;246m\"\033[39m"
      y[5]: " 00\033[90m'\033[39m00.001\033[90m\"\033[39m"
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 134 SKIPPED: 0 WARNINGS: 3 FAILED: 5
      1. Failure: pillar (@test-colformat.R#4) 
      2. Failure: pillar (@test-colformat.R#8) 
      3. Failure: pillar (@test-colformat.R#12) 
      4. Failure: pillar (@test-colformat.R#16) 
      5. Failure: pillar (@test-colformat.R#20) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# holodeck

Version: 0.2.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Quitting from lines 25-31 (simulating-data.Rmd) 
    Error: processing vignette 'simulating-data.Rmd' failed with diagnostics:
    there is no package called 'iheatmapr'
    --- failed re-building ‘simulating-data.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘simulating-data.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘iheatmapr’
    ```

# hpiR

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘knitr’
      All declared Imports should be used.
    ```

# hurricaneexposure

Version: 0.1.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘hurricaneexposuredata’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘mapproj’
      All declared Imports should be used.
    ```

# iadf

Version: 0.1.0

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘iadf-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: campelo_chapman
    > ### Title: campelo_chapman
    > ### Aliases: campelo_chapman
    > 
    > ### ** Examples
    > 
    > data('example_iadf')
    > data('example_rwl')
    > model <- campelo_chapman(campelo_freq(example_iadf, example_rwl))
    > campelo_index(example_iadf, example_rwl, model)
    Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
      there is no package called ‘R.utils’
    Calls: campelo_index ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘falsering-proportion.Rmd’ using rmarkdown
    Quitting from lines 140-141 (falsering-proportion.Rmd) 
    Error: processing vignette 'falsering-proportion.Rmd' failed with diagnostics:
    there is no package called 'R.utils'
    --- failed re-building ‘falsering-proportion.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘falsering-proportion.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# ICD10gm

Version: 1.0.3

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.8Mb
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

*   checking whether the package can be loaded ... ERROR
    ```
    ...
    Loading required package: stats4
    
    Attaching package: ‘S4Vectors’
    
    The following object is masked from ‘package:base’:
    
        expand.grid
    
    Loading required package: IRanges
    Loading required package: GenomicRanges
    Loading required package: Biostrings
    Loading required package: XVector
    
    Attaching package: ‘Biostrings’
    
    The following object is masked from ‘package:base’:
    
        strsplit
    
    Error: package ‘BSgenome.Hsapiens.UCSC.hg19’ required by ‘CODEX’ could not be found
    Execution halted
    ```

# idealstan

Version: 0.7.1

## In both

*   checking whether package ‘idealstan’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/idealstan/new/idealstan.Rcheck/00install.out’ for details.
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
        dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/implyr/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
        Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/implyr/rJava/libs/rJava.so
        Reason: image not found
      Error: package 'rJava' could not be loaded
      In addition: Warning message:
      In system("/usr/libexec/java_home", intern = TRUE) :
        running command '/usr/libexec/java_home' had status 1
      Execution halted
    ```

# INDperform

Version: 0.2.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      The conditions set in the crit_scores table for sub-criterion
      C9_1
      are not unique, i.e. conditions are met multiple times!
      Please correct your crit_score table before you continue.  variable required_data_type
      1      edf            numeric
        variable required_data_type
      1     r_sq            numeric
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 618 SKIPPED: 0 WARNINGS: 12 FAILED: 3
      1. Failure: check gams under different distributions (@test_calc_deriv.R#82) 
      2. Failure: test sample_boot (@test_cond_boot.R#69) 
      3. Failure: test sample_boot (@test_cond_boot.R#70) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        data   3.5Mb
        help   1.1Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘lazyeval’
      All declared Imports should be used.
    ```

# inferr

Version: 0.3.0

## In both

*   checking package dependencies ... ERROR
    ```
    Packages required but not available:
      'dplyr', 'magrittr', 'purrr', 'Rcpp', 'shiny', 'tibble', 'tidyr'
    
    Packages suggested but not available for checking:
      'covr', 'descriptr', 'haven', 'jsonlite', 'knitr', 'lubridate',
      'readr', 'readxl', 'rmarkdown', 'scales', 'shinyBS',
      'shinythemes', 'stringr', 'testthat'
    
    VignetteBuilder package required for checking but not installed: ‘knitr’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# interactions

Version: 1.1.0

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      'brms', 'rstanarm'
    ```

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘quantreg’, ‘brms’, ‘effects’, ‘Hmisc’, ‘rockchalk’, ‘pequod’
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

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
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
    --- failed re-building ‘isomiRs.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘isomiRs.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      'org.Mm.eg.db', 'targetscan.Hs.eg.db'
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

Version: 0.5.2

## In both

*   checking whether the package can be loaded ... ERROR
    ```
    Loading this package had a fatal error status code 1
    Loading log:
    Loading required package: rjags
    Loading required package: coda
    Error: package or namespace load failed for ‘rjags’:
     .onLoad failed in loadNamespace() for 'rjags', details:
      call: dyn.load(file, DLLpath = DLLpath, ...)
      error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/JointAI/rjags/libs/rjags.so':
      dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/JointAI/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
      Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/JointAI/rjags/libs/rjags.so
      Reason: image not found
    Error: package ‘rjags’ could not be loaded
    Execution halted
    ```

# jpmesh

Version: 1.1.3

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 176 marked UTF-8 strings
    ```

# jpndistrict

Version: 0.3.4

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 188 marked UTF-8 strings
    ```

# jtools

Version: 2.0.1

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      'brms', 'quantreg', 'rstanarm'
    ```

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘wec’, ‘quantreg’, ‘brms’, ‘arm’, ‘interactions’, ‘effects’, ‘piecewiseSEM’
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

# LOGAN

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘modules’
      All declared Imports should be used.
    ```

# MachineShop

Version: 1.4.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
      Ran out of iterations and did not converge
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'Introduction.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    --- failed re-building ‘Introduction.Rmd’
    
    --- re-building ‘MLModels.Rmd’ using rmarkdown
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'MLModels.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    --- failed re-building ‘MLModels.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘Introduction.Rmd’ ‘MLModels.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unable to find any JVMs matching version "(null)".
    No Java runtime present, try --request to install.
    Namespace in Imports field not imported from: ‘polspline’
      All declared Imports should be used.
    ```

# madrat

Version: 1.61.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘nnls’ ‘reshape2’ ‘stats’
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

Version: 0.8.2

## In both

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
    ...
    --- re-building ‘matsindf.Rmd’ using rmarkdown
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'matsindf.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    --- failed re-building ‘matsindf.Rmd’
    
    --- re-building ‘midf_apply_primer.Rmd’ using rmarkdown
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'midf_apply_primer.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    --- failed re-building ‘midf_apply_primer.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘matsindf.Rmd’ ‘midf_apply_primer.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# metacoder

Version: 0.3.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ggrepel’ ‘reshape’ ‘svglite’
      All declared Imports should be used.
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

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘xml2’
      All declared Imports should be used.
    ```

# modeldb

Version: 0.1.2

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘kmeans.Rmd’ using rmarkdown
    --- finished re-building ‘kmeans.Rmd’
    
    --- re-building ‘linear-regression.Rmd’ using rmarkdown
    Quitting from lines 54-57 (linear-regression.Rmd) 
    Error: processing vignette 'linear-regression.Rmd' failed with diagnostics:
    object of type 'symbol' is not subsettable
    --- failed re-building ‘linear-regression.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘linear-regression.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
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
      7: namespaceImport(ns, loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]), 
             from = package)
      8: loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]])
      9: runHook(".onLoad", env, package.lib, package)
      10: stop(gettextf("%s failed in %s() for '%s', details:\n  call: %s\n  error: %s", hookname, 
             "loadNamespace", pkgname, deparse(conditionCall(res))[1L], conditionMessage(res)), 
             call. = FALSE, domain = NA)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 0 SKIPPED: 3 WARNINGS: 0 FAILED: 2
      1. Error: No-pooling approaches work (@test-mptinr.R#23) 
      2. Error: Complete-pooling approaches work (@test-mptinr.R#167) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘introduction-bayen_kuhlmann_2011.rmd’ using rmarkdown
    Quitting from lines 57-80 (introduction-bayen_kuhlmann_2011.rmd) 
    Error: processing vignette 'introduction-bayen_kuhlmann_2011.rmd' failed with diagnostics:
    .onLoad failed in loadNamespace() for 'rjags', details:
      call: dyn.load(file, DLLpath = DLLpath, ...)
      error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/MPTmultiverse/rjags/libs/rjags.so':
      dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/MPTmultiverse/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
      Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/MPTmultiverse/rjags/libs/rjags.so
      Reason: image not found
    --- failed re-building ‘introduction-bayen_kuhlmann_2011.rmd’
    
    SUMMARY: processing the following file failed:
      ‘introduction-bayen_kuhlmann_2011.rmd’
    
    Error: Vignette re-building failed.
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

# nandb

Version: 2.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘utils’
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
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘netgsa.Rmd’ using rmarkdown
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'netgsa.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    --- failed re-building ‘netgsa.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘netgsa.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.8Mb
      sub-directories of 1Mb or more:
        data   5.4Mb
    ```

# NetworkChange

Version: 0.5

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘sna’
      All declared Imports should be used.
    ```

# nhdR

Version: 0.5.2

## In both

*   checking whether the package can be loaded ... ERROR
    ```
    Loading this package had a fatal error status code 1
    Loading log:
    Loading required package: maps
    Error: package or namespace load failed for ‘nhdR’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
     there is no package called ‘R.utils’
    Execution halted
    ```

# Nmisc

Version: 0.3.5

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > 
      > test_check("Nmisc")
      ── 1. Failure: keep_at(.x, .at) stops with NA (@test_purrr.R#24)  ──────────────
      `keep_at(x, NA)` did not throw an error.
      
      ── 2. Failure: discard_at(.x, .at) stops with NA (@test_purrr.R#92)  ───────────
      `discard_at(x, NA)` did not throw an error.
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 111 SKIPPED: 1 WARNINGS: 0 FAILED: 2
      1. Failure: keep_at(.x, .at) stops with NA (@test_purrr.R#24) 
      2. Failure: discard_at(.x, .at) stops with NA (@test_purrr.R#92) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# noaastormevents

Version: 0.1.1

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘hurricaneexposuredata’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘choroplethr’ ‘choroplethrMaps’ ‘data.table’ ‘forcats’
      ‘hurricaneexposure’ ‘plyr’ ‘RColorBrewer’ ‘XML’
      All declared Imports should be used.
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

# paletteer

Version: 0.2.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘jcolors’ ‘oompaBase’ ‘palr’ ‘pals’ ‘viridisLite’
      All declared Imports should be used.
    ```

# panelr

Version: 0.7.0

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘panelr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: summary.panel_data
    > ### Title: Summarize panel data frames
    > ### Aliases: summary.panel_data
    > 
    > ### ** Examples
    > 
    > 
    > data("WageData")
    > wages <- panel_data(WageData, id = id, wave = t)
    > summary(wages, lwage, exp, wks)
    Loading required namespace: skimr
    Error in env_bind(mask, ... = env_get(current_env(), "...")) : 
      could not find function "env_bind"
    Calls: summary ... skim.data.frame -> <Anonymous> -> .f -> <Anonymous> -> .f -> .x
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      25: eval_tidy(args[[j]], mask)
      26: skim(., ...) at /private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/Rtmp3Ij4Yr/R.INSTALL917422a6e5c/rlang/R/eval-tidy.R:153
      27: skim.data.frame(., ...)
      28: purrr::map(.data[selected], skim_v)
      29: .f(.x[[i]], ...)
      30: purrr::map(funs, ~.x(x))
      31: .f(.x[[i]], ...)
      32: .x(x)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 274 SKIPPED: 0 WARNINGS: 0 FAILED: 1
      1. Error: summary.panel_data works (@test-utils.R#291) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘AER’
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
      dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/PAutilities/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
      Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/PAutilities/rJava/libs/rJava.so
      Reason: image not found
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      7: loadNamespace(name)
      8: namespaceImport(ns, loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]), 
             from = package)
      9: loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]])
      10: runHook(".onLoad", env, package.lib, package)
      11: stop(gettextf("%s failed in %s() for '%s', details:\n  call: %s\n  error: %s", hookname, 
             "loadNamespace", pkgname, deparse(conditionCall(res))[1L], conditionMessage(res)), 
             call. = FALSE, domain = NA)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 5 SKIPPED: 0 WARNINGS: 1 FAILED: 1
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

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    --- re-building ‘perturbatr.Rmd’ using rmarkdown
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
    --- failed re-building ‘perturbatr.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘perturbatr.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.0Mb
      sub-directories of 1Mb or more:
        data   3.1Mb
    ```

# photobiology

Version: 0.9.28

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘fishmethods’
    ```

# photosynthesis

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyr’
      All declared Imports should be used.
    ```

# pivot

Version: 18.4.17

## In both

*   checking examples ... ERROR
    ```
    ...
    
    > # establish db as a database connection
    > ## Don't show: 
    >    con <- simulate_mssql()
    >    src <- src_dbi(con)
    >    base <- list( x = ident('##iris')
    +                , vars  = tbl_vars(iris)
    +                ) %>% structure(class=c('op_base_remote', 'op_base', 'op'))
    >    db_iris <- structure( list( src = src
    +                              , ops = base
    +                              )
    +        , class = c('tbl_dbi', 'tbl_sql', 'tbl_lazy', 'tbl'))
    > ## End(Don't show)
    > ## Not run: 
    > ##D db_iris <- copy_to(db, iris)
    > ## End(Not run)
    > result <- pivot( db_iris, Species, mean(Petal.Length, na.rm=TRUE)
    +                , setosa, versicolor, virginica)
    > sql_render(result)
    Error: rlang::is_quosure(x = value) is not TRUE
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 49 SKIPPED: 1 WARNINGS: 4 FAILED: 9
      1. Error: PIVOT construction (@test-pivot.R#33) 
      2. Error: PIVOT warnings and errors (@test-pivot.R#71) 
      3. Error: PIVOT with nested select (@test-pivot.R#115) 
      4. Error: spread.tbl_lazy (@test-tidyr.R#8) 
      5. Failure: UNPIVOT construction (@test-unpivot.R#45) 
      6. Failure: order_by (@test-unpivot.R#81) 
      7. Failure: find_connection (@test-utils.R#11) 
      8. Failure: find_connection (@test-utils.R#12) 
      9. Error: get_pivot_levels (@test-utils.R#29) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

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

Version: 1.0.5

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      1: read_packages_file(pf[1], mirror = "m1", repodir = "r1", platform = "source", rversion = "*") at testthat/test-packages-gz.R:88
      2: as_tibble(read.dcf.gz(path)) at /Users/lionel/Desktop/rlang/revdep/checks.noindex/pkgcache/new/pkgcache.Rcheck/00_pkg_src/pkgcache/R/packages-gz.R:20
      3: read.dcf.gz(path) at /Users/lionel/Desktop/rlang/revdep/checks.noindex/pkgcache/new/pkgcache.Rcheck/00_pkg_src/pkgcache/R/packages-gz.R:20
      4: gzfile(x, open = "r") at /Users/lionel/Desktop/rlang/revdep/checks.noindex/pkgcache/new/pkgcache.Rcheck/00_pkg_src/pkgcache/R/utils.R:32
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 275 SKIPPED: 28 WARNINGS: 3 FAILED: 5
      1. Error: load_primary_pkgs (@test-metadata-cache.R#177) 
      2. Error: update_replica_rds (@test-metadata-cache.R#236) 
      3. Error: read_packages_file (@test-packages-gz.R#63) 
      4. Error: packages_parse_deps (@test-packages-gz.R#71) 
      5. Error: merge_packages_data (@test-packages-gz.R#88) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# pkgnet

Version: 0.4.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘pkgdown’
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

Version: 4.9.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.5Mb
      sub-directories of 1Mb or more:
        htmlwidgets   3.4Mb
        R             1.1Mb
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
         }, "could not find function \"WIGFile\"", base::quote(WIGFile(test_wig))) at testthat/test-io-wig.R:24
      2: eval(code, test_env)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 277 SKIPPED: 0 WARNINGS: 8 FAILED: 5
      1. Error: read_bed returns correct GRanges (@test-io-bed.R#67) 
      2. Error: read_bed_graph returns correct GRanges (@test-io-bedGraph.R#39) 
      3. Error: reading/ writing bigwig files returns correct GRanges (@test-io-bw.R#19) 
      4. Error: reading GFF files returns correct GRanges (@test-io-gff.R#87) 
      5. Error: reading WIG files (@test-io-wig.R#24) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘BSgenome.Hsapiens.UCSC.hg19’
    ```

# pmatch

Version: 0.1.4

## Newly broken

*   checking examples ... WARNING
    ```
    Found the following significant warnings:
    
      Warning: Passing an environment as data mask is deprecated.
    Deprecated functions may be defunct as soon as of the next release
    of R.
    See ?Deprecated.
    ```

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
      OK: 164 SKIPPED: 0 WARNINGS: 7 FAILED: 1
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

# portalr

Version: 0.2.2

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(portalr)
      > 
      > test_check("portalr")
      ── 1. Failure: data generated by level = Plot, type = Annuals, plots = longterm 
      digest::digest(data) not identical to "f000fca8bc0e2eb77acf76ba55a93f3b".
      1/1 mismatches
      x[1]: "6c6da92ff91a4dd873b834fba3a3d30a"
      y[1]: "f000fca8bc0e2eb77acf76ba55a93f3b"
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 130 SKIPPED: 14 WARNINGS: 0 FAILED: 1
      1. Failure: data generated by level = Plot, type = Annuals, plots = longterm is same (plants) (@test-99-regression.R#92) 
      
      Error: testthat unit tests failed
      Execution halted
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
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘prevtoinc_vignette.Rmd’ using rmarkdown
    Loading required package: prevtoinc
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'prevtoinc_vignette.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    --- failed re-building ‘prevtoinc_vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘prevtoinc_vignette.Rmd’
    
    Error: Vignette re-building failed.
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

Version: 1.0.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.9Mb
      sub-directories of 1Mb or more:
        doc           9.2Mb
        htmlwidgets   2.5Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘processmapR’
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

# projects

Version: 1.3.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘methods’
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

Version: 0.4.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 12717 marked UTF-8 strings
    ```

# ps

Version: 1.3.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── 10. Failure: (unknown) (@test-cleanup-reporter.R#207)  ──────────────────────
      'R connections, unit: testsuite' did not close network connections: 
      # A tibble: 1 x 6
           fd family type  laddr lport raddr
        <int> <chr>  <chr> <chr> <int> <chr>
      1    NA <NA>   <NA>  <NA>     NA <NA> 
        ... and 7 more
      
      
      Maximum number of 10 failures reached, some test results may be missing.
      
      ══ DONE ════════════════════════════════════════════════════════════════════════
      Keep trying!
      Error: Test failures
      Execution halted
    ```

# psychmeta

Version: 2.3.3

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘overview.Rmd’ using rmarkdown
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'overview.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    --- failed re-building ‘overview.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘overview.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.9Mb
      sub-directories of 1Mb or more:
        R   4.1Mb
    ```

# purrrogress

Version: 0.1.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘dplyr’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘R6’
      All declared Imports should be used.
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

# rabhit

Version: 0.1.1

## In both

*   checking whether the package can be loaded ... ERROR
    ```
    Loading this package had a fatal error status code 1
    Loading log:
    Loading required package: ggplot2
    Error: package or namespace load failed for ‘rabhit’ in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
     there is no package called ‘R.utils’
    Execution halted
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

*   checking whether the package can be loaded ... ERROR
    ```
    Loading this package had a fatal error status code 1
    Loading log:
    Loading required package: rJava
    Unable to find any JVMs matching version "(null)".
    No Java runtime present, try --request to install.
    Error: package or namespace load failed for ‘rJava’:
     .onLoad failed in loadNamespace() for 'rJava', details:
      call: dyn.load(file, DLLpath = DLLpath, ...)
      error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/Rdrools/rJava/libs/rJava.so':
      dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/Rdrools/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
      Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/Rdrools/rJava/libs/rJava.so
      Reason: image not found
    Error: package ‘rJava’ could not be loaded
    In addition: Warning message:
    In system("/usr/libexec/java_home", intern = TRUE) :
      running command '/usr/libexec/java_home' had status 1
    Execution halted
    ```

# recipes

Version: 0.1.5

## Newly broken

*   checking whether package ‘recipes’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: Passing an environment wrapper like a function is deprecated.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/recipes/new/recipes.Rcheck/00install.out’ for details.
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘RcppRoll’
      All declared Imports should be used.
    ```

# regrrr

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘rlang’ ‘spatstat’
      All declared Imports should be used.
    ```

# replyr

Version: 1.0.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘rquery’
    ```

# reproducible

Version: 0.2.8

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘R.utils’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# restfulSE

Version: 1.4.1

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘GO.db’
    
    Packages suggested but not available for checking:
      'org.Mm.eg.db', 'org.Hs.eg.db'
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# rfPermute

Version: 2.1.7

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘plyr’
      All declared Imports should be used.
    ```

# rhmmer

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# Rinstapkg

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘methods’ ‘readr’
      All declared Imports should be used.
    ```

# rmapzen

Version: 0.4.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 31 marked UTF-8 strings
    ```

# RNeXML

Version: 2.3.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    --- finished re-building ‘S4.Rmd’
    
    --- re-building ‘simmap.Rmd’ using rmarkdown
    Loading required package: maps
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'simmap.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    --- failed re-building ‘simmap.Rmd’
    
    --- re-building ‘sparql.Rmd’ using rmarkdown
    Loading required package: ape
    Loading required package: maps
    --- finished re-building ‘sparql.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘simmap.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘taxadb’
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

*   checking whether the package can be loaded ... ERROR
    ```
    Loading this package had a fatal error status code 1
    Loading log:
    Unable to find any JVMs matching version "(null)".
    No Java runtime present, try --request to install.
    Error: package or namespace load failed for ‘RtutoR’:
     .onLoad failed in loadNamespace() for 'rJava', details:
      call: dyn.load(file, DLLpath = DLLpath, ...)
      error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/RtutoR/rJava/libs/rJava.so':
      dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/RtutoR/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
      Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/RtutoR/rJava/libs/rJava.so
      Reason: image not found
    In addition: Warning message:
    In system("/usr/libexec/java_home", intern = TRUE) :
      running command '/usr/libexec/java_home' had status 1
    Execution halted
    ```

# rubias

Version: 0.3.0

## In both

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# sapfluxnetr

Version: 0.0.7

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4 marked UTF-8 strings
    ```

# scFeatureFilter

Version: 1.2.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Introduction.Rmd’ using rmarkdown
    Error: processing vignette 'Introduction.Rmd' failed with diagnostics:
    there is no package called ‘BiocStyle’
    --- failed re-building ‘Introduction.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Introduction.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘scRNAseq’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.4Mb
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
    > ### ** Examples
    > 
    > # Put this in example.R and try running source("example.R")
    > # and `Rscript example.R`
    > filename <- current_filename()
    Warning: `ctxt_stack()` is deprecated as of rlang 0.3.0.
    This warning is displayed once per session.
    Warning: `global_frame()` is deprecated as of rlang 0.3.0.
    This warning is displayed once per session.
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
      OK: 6 SKIPPED: 0 WARNINGS: 6 FAILED: 6
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

Version: 0.28

## In both

*   checking whether package ‘sdcTable’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/sdcTable/new/sdcTable.Rcheck/00install.out’ for details.
    ```

# sensobol

Version: 0.2.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘sensobol.Rmd’ using rmarkdown
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'sensobol.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    --- failed re-building ‘sensobol.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘sensobol.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# Seurat

Version: 3.0.1

## In both

*   checking whether the package can be loaded ... ERROR
    ```
    Loading this package had a fatal error status code 1
    Loading log:
    Error: package or namespace load failed for ‘Seurat’ in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
     there is no package called ‘R.utils’
    Execution halted
    ```

*   checking whether package ‘Seurat’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: namespace ‘Seurat’ is not available and has been replaced
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/Seurat/new/Seurat.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘loomR’
    ```

# sf

Version: 0.7-4

## In both

*   checking examples ... ERROR
    ```
    ...
    pj_obj_create: Cannot find proj.db
    proj_create: Cannot find proj.db
    proj_create: init=epsg:/init=IGNF: syntax not supported in non-PROJ4 emulation mode
    [1] 3857
    > st_crs("+init=epsg:3857")$proj4string
    proj_create: Cannot find proj.db
    pj_obj_create: Cannot find proj.db
    proj_create: Cannot find proj.db
    proj_create: init=epsg:/init=IGNF: syntax not supported in non-PROJ4 emulation mode
    [1] "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"
    > st_crs("+init=epsg:3857 +units=m")$b     # numeric
    proj_create: Cannot find proj.db
    pj_obj_create: Cannot find proj.db
    proj_create: Cannot find proj.db
    proj_create: init=epsg:/init=IGNF: syntax not supported in non-PROJ4 emulation mode
    Warning in CPL_crs_from_proj4string(x) :
      GDAL cannot import PROJ.4 string `+init=epsg:3857 +units=m': returning missing CRS
    Error in if (!is.null(u) && crs$units != u) stop(paste0("units ", u, " not recognized: older GDAL version?"),  : 
      missing value where TRUE/FALSE needed
    Calls: st_crs -> st_crs.character -> make_crs
    Execution halted
    ```

*   checking tests ...
    ```
    ...
    < Linking to GEOS 3.7.1, GDAL 2.4.1, PROJ 6.0.0
    ---
    > Linking to GEOS 3.7.0, GDAL 2.3.2, PROJ 5.2.0
     ERROR
    Running the tests in ‘tests/crs.R’ failed.
    Last 13 lines of output:
      address 0x0, cause 'memory not mapped'
      
      Traceback:
       1: CPL_proj_is_valid(p4s)
       2: structure(CPL_proj_is_valid(p4s), names = c("valid", "result"))
       3: valid_proj4string(x)
       4: make_crs(x)
       5: st_crs.character("error")
       6: st_crs("error")
       7: doTryCatch(return(expr), name, parentenv, handler)
       8: tryCatchOne(expr, names, parentenv, handlers[[1L]])
       9: tryCatchList(expr, classes, parentenv, handlers)
      10: tryCatch(expr, error = function(e) {    call <- conditionCall(e)    if (!is.null(call)) {        if (identical(call[[1L]], quote(doTryCatch)))             call <- sys.call(-4L)        dcall <- deparse(call)[1L]        prefix <- paste("Error in", dcall, ": ")        LONG <- 75L        sm <- strsplit(conditionMessage(e), "\n")[[1L]]        w <- 14L + nchar(dcall, type = "w") + nchar(sm[1L], type = "w")        if (is.na(w))             w <- 14L + nchar(dcall, type = "b") + nchar(sm[1L],                 type = "b")        if (w > LONG)             prefix <- paste0(prefix, "\n  ")    }    else prefix <- "Error : "    msg <- paste0(prefix, conditionMessage(e), "\n")    .Internal(seterrmessage(msg[1L]))    if (!silent && isTRUE(getOption("show.error.messages"))) {        cat(msg, file = outFile)        .Internal(printDeferredWarnings())    }    invisible(structure(msg, class = "try-error", condition = e))})
      11: try(st_crs("error"))
      An irrecoverable exception occurred. R is aborting now ...
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 16.3Mb
      sub-directories of 1Mb or more:
        doc     11.2Mb
        sqlite   1.5Mb
    ```

# shiny

Version: 1.3.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.9Mb
      sub-directories of 1Mb or more:
        R     3.0Mb
        www   7.9Mb
    ```

# SingleCaseES

Version: 0.4.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    --- re-building ‘Effect-size-definitions.Rmd’ using rmarkdown
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'Effect-size-definitions.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    --- failed re-building ‘Effect-size-definitions.Rmd’
    
    --- re-building ‘Using-SingleCaseES.Rmd’ using rmarkdown
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'Using-SingleCaseES.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    --- failed re-building ‘Using-SingleCaseES.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘Effect-size-definitions.Rmd’ ‘Using-SingleCaseES.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# sjmisc

Version: 2.8.0

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        4   2  Species
        4   3  Species
        4   4  Species
        4   5  Species
        5   1  Species
        5   2  Species
        5   3  Species
        5   4  Species
        5   5  Species
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 140 SKIPPED: 12 WARNINGS: 0 FAILED: 1
      1. Error: de_mean (@test-demean.R#20) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# sjPlot

Version: 2.6.3

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘snakecase’
    ```

# sjstats

Version: 0.17.5

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘arm’
    ```

# Sojourn

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘AGread’ ‘caret’
      All declared Imports should be used.
    ```

# stars

Version: 0.3-1

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘starsdata’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 16.1Mb
      sub-directories of 1Mb or more:
        doc  10.5Mb
        nc    4.2Mb
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

Version: 0.2.10

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘R.utils’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# suddengains

Version: 0.2.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘suddengains-tutorial.Rmd’ using rmarkdown
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'suddengains-tutorial.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    --- failed re-building ‘suddengains-tutorial.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘suddengains-tutorial.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# sugrrants

Version: 0.2.4

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Loading required package: viridisLite
    Loading required package: ggplot2
    Argument `week_start` only works for the monthly calendar.
    Argument `week_start` only works for the monthly calendar.
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'frame-calendar.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    --- failed re-building ‘frame-calendar.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘frame-calendar.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# SummarizedBenchmark

Version: 2.0.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Rerunning performance metrics only for the following methods: BH2
      
      ── 1. Failure: test that 'plotROC' and 'plotOverlaps'  behave as expected (@test
      plotMethodsOverlap(sb) is not null.
      
      ── 2. Failure: test that 'plotROC' and 'plotOverlaps'  behave as expected (@test
      plotMethodsOverlap(sb2, assay = "assay1") is not null.
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 222 SKIPPED: 0 WARNINGS: 0 FAILED: 2
      1. Failure: test that 'plotROC' and 'plotOverlaps'  behave as expected (@test_plottingFunctions.R#19) 
      2. Failure: test that 'plotROC' and 'plotOverlaps'  behave as expected (@test_plottingFunctions.R#20) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    fitting model and testing
    converting counts to integer mode
    estimating size factors
    estimating dispersions
    gene-wise dispersion estimates
    mean-dispersion relationship
    final dispersion estimates
    fitting model and testing
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'SummarizedBenchmark.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    --- failed re-building ‘SummarizedBenchmark.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘QuantificationBenchmark.Rmd’ ‘SingleCellBenchmark.Rmd’
      ‘SummarizedBenchmark.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘scRNAseq’
    
    Depends: includes the non-default packages:
      'tidyr', 'SummarizedExperiment', 'S4Vectors', 'BiocGenerics',
      'UpSetR', 'rlang', 'stringr', 'BiocParallel', 'ggplot2',
      'mclust', 'dplyr', 'digest', 'sessioninfo', 'crayon', 'tibble'
    Adding so many packages to the search path is excessive and
    importing selectively is preferable.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 15.5Mb
      sub-directories of 1Mb or more:
        data  11.2Mb
        doc    3.4Mb
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

# survivalAnalysis

Version: 0.1.1

## Newly broken

*   checking examples ... WARNING
    ```
    Found the following significant warnings:
    
      Warning: Passing an environment wrapper like a `quosure/formula` object is deprecated.
    Deprecated functions may be defunct as soon as of the next release
    of R.
    See ?Deprecated.
    ```

# SWMPrExtension

Version: 0.3.16

## In both

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
    ...
    --- re-building ‘diversity.Rmd’ using rmarkdown
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'diversity.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    --- failed re-building ‘diversity.Rmd’
    
    --- re-building ‘tabula.Rmd’ using rmarkdown
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'tabula.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    --- failed re-building ‘tabula.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘diversity.Rmd’ ‘tabula.Rmd’
    
    Error: Vignette re-building failed.
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

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘methods’
      All declared Imports should be used.
    ```

# tailr

Version: 0.1.3

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘pmatch’
    ```

# taxa

Version: 0.3.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘knitr’ ‘lazyeval’ ‘rlang’ ‘tidyr’
      All declared Imports should be used.
    ```

# tealeaves

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ggplot2’ ‘tidyr’
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

# textrecipes

Version: 0.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘stringr’
      All declared Imports should be used.
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
      OK: 37 SKIPPED: 0 WARNINGS: 0 FAILED: 1
      1. Error: right number of columns given (@test-errors.R#10) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘math.Rmd’ using rmarkdown
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'math.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    --- failed re-building ‘math.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘math.Rmd’
    
    Error: Vignette re-building failed.
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

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘tidytext’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# tidymv

Version: 2.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘cowplot’
      All declared Imports should be used.
    ```

# tidyquant

Version: 0.5.6

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘xml2’
      All declared Imports should be used.
    ```

# tidyr

Version: 0.8.3

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 23 marked UTF-8 strings
    ```

# tidytext

Version: 0.2.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Unable to find any JVMs matching version "(null)".
    No Java runtime present, try --request to install.
    ```

# tidytidbits

Version: 0.2.0

## Newly broken

*   checking examples ... WARNING
    ```
    Found the following significant warnings:
    
      Warning: Passing an environment wrapper like a `quosure/formula` object is deprecated.
    Deprecated functions may be defunct as soon as of the next release
    of R.
    See ?Deprecated.
    ```

# tidytransit

Version: 0.5.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.9Mb
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
      Note: found 66 marked UTF-8 strings
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

# tigger

Version: 0.3.1

## In both

*   checking whether the package can be loaded ... ERROR
    ```
    Loading this package had a fatal error status code 1
    Loading log:
    Loading required package: ggplot2
    Error: package or namespace load failed for ‘tigger’ in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
     there is no package called ‘R.utils’
    Execution halted
    ```

# trackr

Version: 0.10.5

## In both

*   checking package dependencies ... ERROR
    ```
    Packages required but not available:
      'histry', 'CodeDepends', 'rsolr', 'roprov'
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# treeio

Version: 1.6.2

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 109 SKIPPED: 0 WARNINGS: 0 FAILED: 16
      1. Failure: child works for bifurcating trees (@test-access-related-nodes.R#24) 
      2. Failure: child works for bifurcating trees (@test-access-related-nodes.R#26) 
      3. Failure: child works for non-bifurcating trees (@test-access-related-nodes.R#33) 
      4. Failure: child works for non-bifurcating trees (@test-access-related-nodes.R#35) 
      5. Failure: offspring works on bifurcating trees (@test-access-related-nodes.R#40) 
      6. Failure: offspring works on non-bifurcating trees (@test-access-related-nodes.R#46) 
      7. Failure: parent works for bifurcating trees (@test-access-related-nodes.R#52) 
      8. Failure: parent works for bifurcating trees (@test-access-related-nodes.R#53) 
      9. Failure: parent works for non-bifurcating trees (@test-access-related-nodes.R#60) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Warning: Computation failed in `stat_binhex()`:
    Package `hexbin` required for `stat_binhex`.
    Please install and try again.
    Warning: Computation failed in `stat_binhex()`:
    Package `hexbin` required for `stat_binhex`.
    Please install and try again.
    Warning: Computation failed in `stat_binhex()`:
    Package `hexbin` required for `stat_binhex`.
    Please install and try again.
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'Importer.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    --- failed re-building ‘Importer.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘Exporter.Rmd’ ‘Importer.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# trread

Version: 0.2.7

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
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

Version: 0.8.2

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    
    Attaching package: 'tsibble'
    
    The following object is masked from 'package:dplyr':
    
        id
    
    --- finished re-building ‘window.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘intro-tsibble.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# UKgrid

Version: 0.1.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 12.6Mb
      sub-directories of 1Mb or more:
        data   4.1Mb
        doc    8.4Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rlang’
      All declared Imports should be used.
    ```

# utile.tools

Version: 0.1.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘glue’ ‘survival’
      All declared Imports should be used.
    ```

# vctrs

Version: 0.1.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── 1. Failure: print method gives human friendly output (@test-list_of.R#37)  ──
      `{ ... }` has changed from known value recorded in 'test-list_of-print.txt'.
      2/13 mismatches
      x[12]: "1         [1]"
      y[12]: "1           1"
      
      x[13]: "2         [2]"
      y[13]: "2        2, 3"
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 745 SKIPPED: 0 WARNINGS: 0 FAILED: 1
      1. Failure: print method gives human friendly output (@test-list_of.R#37) 
      
      Error: testthat unit tests failed
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

# vroom

Version: 1.0.1

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘usethis’
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
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        doc    2.9Mb
        help   1.1Mb
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

