# abjutils

Version: 0.2.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘httr’ ‘progress’
      All declared Imports should be used.
    ```

# ADPclust

Version: 0.7

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘fields’ ‘knitr’
      All declared Imports should be used.
    ```

# afex

Version: 0.20-2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Missing or unexported objects:
      ‘lmerTest::anova’ ‘lmerTest::summary’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘ez’, ‘ascii’
    ```

# afmToolkit

Version: 0.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DBI’ ‘assertthat’ ‘tibble’
      All declared Imports should be used.
    ```

# aire.zmvm

Version: 0.6.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 52 marked UTF-8 strings
    ```

# alphavantager

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘devtools’
      All declared Imports should be used.
    ```

# ameco

Version: 0.2.9

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 15.7Mb
      sub-directories of 1Mb or more:
        data  15.6Mb
    ```

# amt

Version: 0.0.4.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Rcpp’ ‘magrittr’
      All declared Imports should be used.
    ```

# anchoredDistr

Version: 1.0.3

## In both

*   checking whether package ‘anchoredDistr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/anchoredDistr/new/anchoredDistr.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘anchoredDistr’ ...
** package ‘anchoredDistr’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘quantreg’
ERROR: lazy loading failed for package ‘anchoredDistr’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/anchoredDistr/new/anchoredDistr.Rcheck/anchoredDistr’

```
### CRAN

```
* installing *source* package ‘anchoredDistr’ ...
** package ‘anchoredDistr’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘quantreg’
ERROR: lazy loading failed for package ‘anchoredDistr’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/anchoredDistr/old/anchoredDistr.Rcheck/anchoredDistr’

```
# ANLP

Version: 1.3

## In both

*   checking whether package ‘ANLP’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/ANLP/new/ANLP.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘ANLP’ ...
** package ‘ANLP’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘qdap’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 there is no package called ‘rJava’
Error : package ‘qdap’ could not be loaded
ERROR: lazy loading failed for package ‘ANLP’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/ANLP/new/ANLP.Rcheck/ANLP’

```
### CRAN

```
* installing *source* package ‘ANLP’ ...
** package ‘ANLP’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘qdap’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 there is no package called ‘rJava’
Error : package ‘qdap’ could not be loaded
ERROR: lazy loading failed for package ‘ANLP’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/ANLP/old/ANLP.Rcheck/ANLP’

```
# anomalize

Version: 0.1.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        help   4.7Mb
    ```

# anomalyDetection

Version: 0.2.5

## In both

*   checking whether package ‘anomalyDetection’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/anomalyDetection/new/anomalyDetection.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘anomalyDetection’ ...
** package ‘anomalyDetection’ successfully unpacked and MD5 sums checked
** libs
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/anomalyDetection/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/anomalyDetection/RcppArmadillo/include" -I/usr/local/include  -fopenmp  -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘anomalyDetection’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/anomalyDetection/new/anomalyDetection.Rcheck/anomalyDetection’

```
### CRAN

```
* installing *source* package ‘anomalyDetection’ ...
** package ‘anomalyDetection’ successfully unpacked and MD5 sums checked
** libs
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/anomalyDetection/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/anomalyDetection/RcppArmadillo/include" -I/usr/local/include  -fopenmp  -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘anomalyDetection’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/anomalyDetection/old/anomalyDetection.Rcheck/anomalyDetection’

```
# archivist

Version: 2.3.1

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘archivist.github’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘rmarkdown’, ‘archivist.github’
    ```

# assertive.types

Version: 0.0-3

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘pryr’
    ```

# atlantistools

Version: 0.4.3

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    ------------
    ------------
    Running 'pdflatex  -halt-on-error -interaction=batchmode -recorder  "model-calibration.tex"'
    ------------
    Latexmk: applying rule 'pdflatex'...
    This is pdfTeX, Version 3.14159265-2.6-1.40.17 (TeX Live 2016) (preloaded format=pdflatex)
     restricted \write18 enabled.
    entering extended mode
    Collected error summary (may duplicate other messages):
      pdflatex: Command for 'pdflatex' gave return code 256
    Latexmk: Use the -f option to force complete processing,
     unless error was exceeding maximum runs of latex/pdflatex.
    Latexmk: Errors, so I did not complete making targets
    ! File ended while scanning use of \@@BOOKMARK.
    <inserted text> 
                    \par 
    l.107 \begin{document}
    
    Error: processing vignette 'model-calibration.Rmd' failed with diagnostics:
    Failed to compile model-calibration.tex. See model-calibration.log for more info.
    Execution halted
    ```

# auctestr

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyr’
      All declared Imports should be used.
    ```

# auk

Version: 0.2.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 552 marked UTF-8 strings
    ```

# available

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘purrr’
      All declared Imports should be used.
    ```

# BALCONY

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘base’
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

# bayesplot

Version: 1.5.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.9Mb
      sub-directories of 1Mb or more:
        R     1.8Mb
        doc   3.6Mb
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

# BETS

Version: 0.4.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# BgeeDB

Version: 2.6.0

## In both

*   checking whether package ‘BgeeDB’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/BgeeDB/new/BgeeDB.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘BgeeDB’ ...
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error : package ‘GO.db’ required by ‘topGO’ could not be found
ERROR: lazy loading failed for package ‘BgeeDB’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/BgeeDB/new/BgeeDB.Rcheck/BgeeDB’

```
### CRAN

```
* installing *source* package ‘BgeeDB’ ...
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error : package ‘GO.db’ required by ‘topGO’ could not be found
ERROR: lazy loading failed for package ‘BgeeDB’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/BgeeDB/old/BgeeDB.Rcheck/BgeeDB’

```
# bigrquery

Version: 1.0.0

## In both

*   checking whether package ‘bigrquery’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      /Users/lionel/Desktop/rlang/revdep/library.noindex/bigrquery/progress/include/RProgress.h:172:13: warning: variable length arrays are a C99 feature [-Wvla-extension]
      /Users/lionel/Desktop/rlang/revdep/library.noindex/bigrquery/progress/include/RProgress.h:220:16: warning: variable length arrays are a C99 feature [-Wvla-extension]
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/bigrquery/new/bigrquery.Rcheck/00install.out’ for details.
    ```

# billboard

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tibble’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 660 marked UTF-8 strings
    ```

# biomartr

Version: 0.7.0

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘biomartr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: getMetaGenomeSummary
    > ### Title: Retrieve the assembly_summary.txt file from NCBI genbank
    > ###   metagenomes
    > ### Aliases: getMetaGenomeSummary
    > 
    > ### ** Examples
    > 
    > meta.summary <- getMetaGenomeSummary()
    trying URL 'ftp://ftp.ncbi.nlm.nih.gov/genomes/genbank/metagenomes/assembly_summary.txt'
    Content type 'unknown' length 2613069 bytes (2.5 MB)
    =============
    Warning in download.file(url, ...) :
      downloaded length 710136 != reported length 2613069
    Warning in download.file(url, ...) :
      URL 'ftp://ftp.ncbi.nlm.nih.gov/genomes/genbank/metagenomes/assembly_summary.txt': status was 'Transferred a partial file'
    Error: The FTP site 'ftp://ftp.ncbi.nlm.nih.gov/genomes/genbank/metagenomes/assembly_summary.txt' cannot be reached. Are you connected to the internet? Is the homepage 'ftp://ftp.ncbi.nlm.nih.gov/' currently available? Please try to re-run this function and see if it works then ...
    Execution halted
    ```

*   R CMD check timed out
    

# bisect

Version: 0.9.0

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘bisect-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: bisect_semi_suprevised
    > ### Title: Add together two numbers.
    > ### Aliases: bisect_semi_suprevised
    > 
    > ### ** Examples
    > 
    > ## Randomly choose samples to be used as known
    > n_known_samples <- 50
    > known_samples_indices <- sample.int(nrow(baseline_GSE40279), size = n_known_samples)
    > known_samples <- as.matrix(baseline_GSE40279[known_samples_indices, ])
    > 
    > ## Fit a dirichlet distribution to known samples to use as prior
    > fit_dirichlet <- sirt::dirichlet.mle(as.matrix(known_samples))
    Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
      there is no package called ‘CDM’
    Calls: :: ... tryCatch -> tryCatchList -> tryCatchOne -> <Anonymous>
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 122-125 (bisect_vignette.Rmd) 
    Error: processing vignette 'bisect_vignette.Rmd' failed with diagnostics:
    there is no package called 'CDM'
    Execution halted
    ```

# blastula

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘purrr’ ‘rlang’ ‘tibble’
      All declared Imports should be used.
    ```

# blkbox

Version: 1.0

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘rJava’
    
    Package suggested but not available for checking: ‘bigrf’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# BloodCancerMultiOmics2017

Version: 1.0.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    ── Conflicts ─────────────────────────────────────── tidyverse_conflicts() ──
    ✖ ggplot2::Position() masks BiocGenerics::Position(), base::Position()
    ✖ dplyr::collapse()   masks IRanges::collapse()
    ✖ dplyr::combine()    masks Biobase::combine(), BiocGenerics::combine()
    ✖ dplyr::count()      masks matrixStats::count()
    ✖ dplyr::desc()       masks IRanges::desc()
    ✖ tidyr::expand()     masks S4Vectors::expand()
    ✖ dplyr::exprs()      masks Biobase::exprs()
    ✖ dplyr::filter()     masks stats::filter()
    ✖ dplyr::first()      masks S4Vectors::first()
    ✖ dplyr::lag()        masks stats::lag()
    ✖ purrr::reduce()     masks GenomicRanges::reduce(), IRanges::reduce()
    ✖ dplyr::rename()     masks S4Vectors::rename()
    ✖ purrr::simplify()   masks DelayedArray::simplify()
    ✖ dplyr::slice()      masks IRanges::slice()
    Warning in has_utility("convert", "ImageMagick") :
      ImageMagick not installed or not in PATH
    Quitting from lines 230-238 (BloodCancerMultiOmics2017-dataOverview.Rmd) 
    Error: processing vignette 'BloodCancerMultiOmics2017-dataOverview.Rmd' failed with diagnostics:
    incorrect number of dimensions
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘org.Hs.eg.db’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 101.4Mb
      sub-directories of 1Mb or more:
        data     66.4Mb
        doc      26.2Mb
        extdata   8.5Mb
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘vsn’
    ```

# blscrapeR

Version: 3.1.2

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘blscrapeR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: qcew_api
    > ### Title: Request data from the Quarterly Census of Employment and Wages.
    > ### Aliases: qcew_api
    > ### Keywords: api bls cpi economics inflation unemployment
    > 
    > ### ** Examples
    > 
    > 
    > # A request for the employment levels and wages for NIACS 5112: Software Publishers.
    > dat <- qcew_api(year=2015, qtr="A", slice="area", sliceCode="US000")
    Please set a numeric year.
    Trying BLS servers...
    URL caused a warning. Please check your parameters and try again: https://data.bls.gov/cew/data/api/2015/A/area/US000.csv
    Error in qcew_api(year = 2015, qtr = "A", slice = "area", sliceCode = "US000") : 
      object 'qcewDat' not found
    Execution halted
    ```

# bmlm

Version: 1.3.5

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘methods’
      All declared Imports should be used.
    ```

# bodenmiller

Version: 0.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.9Mb
      sub-directories of 1Mb or more:
        data   8.7Mb
    ```

# bomrang

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rappdirs’
      All declared Imports should be used.
    ```

# bossMaps

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘rgdal’ ‘tidyr’
      All declared Imports should be used.
    ```

# brazilmaps

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘sp’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘ggthemes’
    ```

# breathtestcore

Version: 0.4.1

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘breathteststan’
    ```

# breathteststan

Version: 0.4.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > # Only one test per file to avoid hanging 32-bit compile
      > #test_check("breathteststan", filter = "stan_fit")
      > Sys.unsetenv("R_TESTS") # https://github.com/r-lib/testthat/issues/603
      > test_check("breathteststan")
      [1] "liquid_normal" "solid_normal"  "solid_patient"
      [1] "liquid_normal" "solid_normal"  "solid_patient"
      ── 1. Failure: Data that cannot be fitted with nls_list/nlme work with stan_fit 
      sigma(fit) is not strictly more than 0.9. Difference: -0.00781
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 27 SKIPPED: 7 FAILED: 1
      1. Failure: Data that cannot be fitted with nls_list/nlme work with stan_fit (@test_stan_fit_2.R#28) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  7.6Mb
      sub-directories of 1Mb or more:
        libs   7.4Mb
    ```

# broom

Version: 0.4.4

## In both

*   R CMD check timed out
    

# bsam

Version: 1.1.2

## In both

*   checking whether package ‘bsam’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/bsam/new/bsam.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘bsam’ ...
** package ‘bsam’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rjags’:
 .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/bsam/rjags/libs/rjags.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/bsam/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/bsam/rjags/libs/rjags.so
  Reason: image not found
Error : package ‘rjags’ could not be loaded
ERROR: lazy loading failed for package ‘bsam’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/bsam/new/bsam.Rcheck/bsam’

```
### CRAN

```
* installing *source* package ‘bsam’ ...
** package ‘bsam’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rjags’:
 .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/bsam/rjags/libs/rjags.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/bsam/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/bsam/rjags/libs/rjags.so
  Reason: image not found
Error : package ‘rjags’ could not be loaded
ERROR: lazy loading failed for package ‘bsam’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/bsam/old/bsam.Rcheck/bsam’

```
# bsplus

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘methods’
      All declared Imports should be used.
    ```

# BubbleTree

Version: 2.10.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 29.2Mb
      sub-directories of 1Mb or more:
        data  23.4Mb
        doc    5.3Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    annoByOverlap,Annotate: no visible binding for global variable
      'queryHits'
    Undefined global functions or variables:
      queryHits
    ```

# caffsim

Version: 0.2.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘markdown’
      All declared Imports should be used.
    ```

# caret

Version: 6.0-80

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.4Mb
      sub-directories of 1Mb or more:
        R        1.0Mb
        data     1.5Mb
        models   2.4Mb
    ```

# CATALYST

Version: 1.4.2

## In both

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .travis.yml
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  9.0Mb
      sub-directories of 1Mb or more:
        data   3.1Mb
        doc    5.1Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    plotDiffHeatmap,matrix-SummarizedExperiment: no visible binding for
      global variable ‘cluster_id’
    plotDiffHeatmap,matrix-SummarizedExperiment: no visible binding for
      global variable ‘sample_id’
    Undefined global functions or variables:
      cluster_id sample_id
    ```

# catenary

Version: 1.1.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyverse’
      All declared Imports should be used.
    ```

# cdcfluview

Version: 0.7.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘units’
      All declared Imports should be used.
    ```

# CDECRetrieve

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘lazyeval’ ‘purrr’ ‘roxygen2’
      All declared Imports should be used.
    ```

# cellscape

Version: 1.4.0

## In both

*   checking Rd \usage sections ... WARNING
    ```
    Duplicated \argument entries in documentation object 'dfs_tree':
      ‘chrom_bounds’ ‘ncols’ ‘chrom_bounds’ ‘cnv_data’ ‘chrom_bounds’
      ‘n_bp_per_pixel’ ‘mut_data’ ‘width’ ‘height’ ‘mutations’ ‘height’
      ‘width’ ‘clonal_prev’ ‘tree_edges’ ‘alpha’ ‘clonal_prev’ ‘tree_edges’
      ‘genotype_position’ ‘clone_colours’ ‘perturbations’ ‘mutations’
      ‘tree_edges’ ‘clonal_prev’ ‘clonal_prev’ ‘tree_edges’ ‘clone_colours’
      ‘mutations’
    
    Functions with \usage entries need to have the appropriate \alias
    entries, and all their arguments documented.
    The \usage entries must correspond to syntactically valid R code.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 76-92 (cellscape_vignette.Rmd) 
    Error: processing vignette 'cellscape_vignette.Rmd' failed with diagnostics:
    there is no package called 'devtools'
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘plyr’
      All declared Imports should be used.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    getMutOrder: no visible binding for global variable ‘VAF’
    getMutOrder: no visible global function definition for ‘lm’
    getMutOrder: no visible binding for global variable ‘na.omit’
    getMutOrder: no visible global function definition for ‘coef’
    getMutationsData: no visible binding for global variable
      ‘show_warnings’
    getTargetedHeatmapForEachSC: no visible binding for global variable
      ‘single_cell_id’
    getTargetedHeatmapForEachSC: no visible binding for global variable
      ‘chr’
    getTargetedHeatmapForEachSC: no visible binding for global variable
      ‘coord’
    Undefined global functions or variables:
      VAF chr chrom_index coef combn coord copy_number cumsum_values dist
      genotype hclust lm melt mode_cnv n n_gt na.omit px px_width sc_id
      setNames show_warnings single_cell_id site timepoint
    Consider adding
      importFrom("stats", "coef", "dist", "hclust", "lm", "na.omit",
                 "setNames")
      importFrom("utils", "combn")
    to your NAMESPACE file.
    ```

*   checking for unstated dependencies in vignettes ... NOTE
    ```
    'library' or 'require' call not declared from: ‘devtools’
    ```

# cepR

Version: 0.1.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 287 marked UTF-8 strings
    ```

# CGPfunctions

Version: 0.3

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘BSDA’, ‘janitor’
    ```

# childesr

Version: 0.1.0

## In both

*   R CMD check timed out
    

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dbplyr’
      All declared Imports should be used.
    ```

# childsds

Version: 0.6.7

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘gamlss.dist’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 20 marked UTF-8 strings
    ```

# chimeraviz

Version: 1.6.0

## In both

*   checking package dependencies ... ERROR
    ```
    Packages required but not available: ‘org.Hs.eg.db’ ‘org.Mm.eg.db’
    
    Depends: includes the non-default packages:
      ‘Biostrings’ ‘GenomicRanges’ ‘IRanges’ ‘Gviz’ ‘S4Vectors’ ‘ensembldb’
      ‘AnnotationFilter’
    Adding so many packages to the search path is excessive and importing
    selectively is preferable.
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# chorrrds

Version: 0.1.4

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘httr’ ‘jsonlite’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 8008 marked UTF-8 strings
    ```

# chromer

Version: 0.1

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Description field: should contain one or more complete sentences.
    ```

*   checking R code for possible problems ... NOTE
    ```
    parse_counts: no visible global function definition for ‘na.omit’
    Undefined global functions or variables:
      na.omit
    Consider adding
      importFrom("stats", "na.omit")
    to your NAMESPACE file.
    ```

# CINdex

Version: 1.8.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
        as.data.frame, basename, cbind, colMeans, colSums, colnames,
        dirname, do.call, duplicated, eval, evalq, get, grep, grepl,
        intersect, is.unsorted, lapply, lengths, mapply, match, mget,
        order, paste, pmax, pmax.int, pmin, pmin.int, rank, rbind,
        rowMeans, rowSums, rownames, sapply, setdiff, sort, table,
        tapply, union, unique, unsplit, which, which.max, which.min
    
    Loading required package: S4Vectors
    
    Attaching package: 'S4Vectors'
    
    The following object is masked from 'package:base':
    
        expand.grid
    
    Loading required package: IRanges
    Loading required package: GenomeInfoDb
    Quitting from lines 33-42 (PrepareInputData.Rmd) 
    Error: processing vignette 'PrepareInputData.Rmd' failed with diagnostics:
    there is no package called 'pd.genomewidesnp.6'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘pd.genomewidesnp.6’ ‘org.Hs.eg.db’
      ‘TxDb.Hsapiens.UCSC.hg18.knownGene’ ‘Homo.sapiens’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 18.4Mb
      sub-directories of 1Mb or more:
        data  17.7Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    comp.heatmap: no visible binding for global variable ‘dataMatrix’
    process.probe.anno: no visible binding for global variable ‘ID’
    process.probe.anno: no visible binding for global variable ‘midpoint’
    process.reference.genome: no visible binding for global variable
      ‘chrom’
    process.reference.genome: no visible binding for global variable ‘name’
    process.reference.genome: no visible binding for global variable
      ‘stain’
    run.cin.chr: no visible global function definition for ‘is’
    run.cin.cyto: no visible global function definition for ‘is’
    Undefined global functions or variables:
      ID chrom dataMatrix is midpoint name stain
    Consider adding
      importFrom("methods", "is")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# cleanNLP

Version: 2.0.3

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘rJava’
    ```

# clustermq

Version: 0.8.4

## In both

*   R CMD check timed out
    

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘R6’ ‘infuser’ ‘purrr’
      All declared Imports should be used.
    ```

# CNPBayes

Version: 1.10.0

## In both

*   R CMD check timed out
    

*   checking Rd \usage sections ... WARNING
    ```
    Undocumented arguments in documentation object 'marginal_lik'
      ‘value’
    
    Functions with \usage entries need to have the appropriate \alias
    entries, and all their arguments documented.
    The \usage entries must correspond to syntactically valid R code.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.1Mb
      sub-directories of 1Mb or more:
        R      1.2Mb
        doc    3.4Mb
        libs   2.4Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    copyNumber,SingleBatchCopyNumber: no visible binding for global
      variable ‘theta.star’
    Undefined global functions or variables:
      theta.star
    ```

# codebook

Version: 0.5.9

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘codebook-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: codebook_missingness
    > ### Title: Codebook missingness
    > ### Aliases: codebook_missingness
    > 
    > ### ** Examples
    > 
    > data("bfi")
    > codebook_missingness(bfi)
    Error in `rownames<-`(`*tmp*`, value = table(pat)) : 
      attempt to set 'rownames' on an object with no dimensions
    Calls: codebook_missingness -> md_pattern -> <Anonymous> -> rownames<-
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘graphics’ ‘htmltools’ ‘pander’ ‘readr’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘labelled’
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 65 marked UTF-8 strings
    ```

# codemetar

Version: 0.1.6

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘memoise’
      All declared Imports should be used.
    ```

# CollapsABEL

Version: 0.10.11

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘rJava’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# collapsibleTree

Version: 0.1.6

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      ‘knitr’ ‘shiny’
    ```

# colorednoise

Version: 1.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘Rcpp’
      All declared Imports should be used.
    ```

# compareDF

Version: 1.5.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘stringr’
      All declared Imports should be used.
    ```

# comtradr

Version: 0.2.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘methods’
      All declared Imports should be used.
    ```

# condformat

Version: 0.7.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > test_check("condformat")
      Unable to find any JVMs matching version "(null)".
      No Java runtime present, try --request to install.
      -- 1. Error: condformat2excel generates a file (@test_rendering.R#38)  ---------
      Please install the xlsx package in order to export to excel
      1: condformat2excel(condformat(head(iris, n = rows_to_write)), filename = filename) at testthat/test_rendering.R:38
      2: require_xlsx()
      3: stop("Please install the xlsx package in order to export to excel")
      
      == testthat results  ===========================================================
      OK: 114 SKIPPED: 0 FAILED: 1
      1. Error: condformat2excel generates a file (@test_rendering.R#38) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unable to find any JVMs matching version "(null)".
    No Java runtime present, try --request to install.
    Unable to find any JVMs matching version "(null)".
    No Java runtime present, try --request to install.
    ```

# congressbr

Version: 0.1.2

## Newly fixed

*   R CMD check timed out
    

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 187 marked UTF-8 strings
    ```

# countyfloods

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘maps’
      All declared Imports should be used.
    ```

# countyweather

Version: 0.1.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

# CRANsearcher

Version: 1.0.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 11 marked Latin-1 strings
      Note: found 57 marked UTF-8 strings
    ```

# cRegulome

Version: 0.1.1

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘org.Hs.eg.db’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# crosswalkr

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# crossword.r

Version: 0.3.5

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘r6extended’
      All declared Imports should be used.
    ```

# crypto

Version: 1.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘yaml’
      All declared Imports should be used.
    ```

# curatedMetagenomicData

Version: 1.10.0

## In both

*   R CMD check timed out
    

*   checking installed package size ... NOTE
    ```
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        doc    1.4Mb
        help   2.7Mb
    ```

# d3r

Version: 0.8.1

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      ‘igraph’ ‘partykit’ ‘treemap’ ‘V8’
    ```

# d3Tree

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘magrittr’
      All declared Imports should be used.
    ```

# dalmatian

Version: 0.3.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      The vignette engine knitr::rmarkdown is not available, because the rmarkdown package is not installed. Please install it.
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      The vignette engine knitr::rmarkdown is not available, because the rmarkdown package is not installed. Please install it.
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      The vignette engine knitr::rmarkdown is not available, because the rmarkdown package is not installed. Please install it.
    Quitting from lines 36-71 (weights-1-simulate.Rmd) 
    Error: processing vignette 'weights-1-simulate.Rmd' failed with diagnostics:
    .onLoad failed in loadNamespace() for 'rjags', details:
      call: dyn.load(file, DLLpath = DLLpath, ...)
      error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/dalmatian/rjags/libs/rjags.so':
      dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/dalmatian/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
      Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/dalmatian/rjags/libs/rjags.so
      Reason: image not found
    Execution halted
    ```

# DAPAR

Version: 1.12.1

## In both

*   checking whether package ‘DAPAR’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/DAPAR/new/DAPAR.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘DAPAR’ ...
** R
** inst
** byte-compile and prepare package for lazy loading
Warning in fun(libname, pkgname) :
  mzR has been built against a different Rcpp version (0.12.16)
than is installed on your system (0.12.17). This might lead to errors
when loading mzR. If you encounter such issues, please send a report,
including the output of sessionInfo() to the Bioc support forum at 
https://support.bioconductor.org/. For details see also
https://github.com/sneumann/mzR/wiki/mzR-Rcpp-compiler-linker-issue.
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘DO.db’
ERROR: lazy loading failed for package ‘DAPAR’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/DAPAR/new/DAPAR.Rcheck/DAPAR’

```
### CRAN

```
* installing *source* package ‘DAPAR’ ...
** R
** inst
** byte-compile and prepare package for lazy loading
Warning in fun(libname, pkgname) :
  mzR has been built against a different Rcpp version (0.12.16)
than is installed on your system (0.12.17). This might lead to errors
when loading mzR. If you encounter such issues, please send a report,
including the output of sessionInfo() to the Bioc support forum at 
https://support.bioconductor.org/. For details see also
https://github.com/sneumann/mzR/wiki/mzR-Rcpp-compiler-linker-issue.
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘DO.db’
ERROR: lazy loading failed for package ‘DAPAR’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/DAPAR/old/DAPAR.Rcheck/DAPAR’

```
# datadr

Version: 0.8.6

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘Rhipe’
    ```

# datasus

Version: 0.4.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘RCurl’
      All declared Imports should be used.
    ```

# ddpcr

Version: 1.9

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Reading data files into plate... DONE (0 seconds)
    Initializing plate of type `custom_thresholds`... DONE (0 seconds)
    Identifying outlier droplets... DONE (0 seconds)
    Classifying droplets... DONE (0 seconds)
    Analysis complete
    Reading data files into plate... DONE (0 seconds)
    Initializing plate of type `fam_positive_pnpp`... DONE (0 seconds)
    Identifying failed wells... DONE (0 seconds)
    Identifying outlier droplets... DONE (0 seconds)
    Identifying empty droplets... DONE (0 seconds)
    Classifying droplets... DONE (0 seconds)
    Reclassifying droplets... skipped (not enough wells with significant mutant clusters)
    Analysis complete
    Initializing plate of type `custom_thresholds`... DONE (0 seconds)
    Reading data files into plate... DONE (0 seconds)
    Initializing plate of type `custom_thresholds`... DONE (0 seconds)
    pandoc: Could not fetch http://www.r-pkg.org/badges/version/ddpcr
    TlsException (HandshakeFailed (Error_Protocol ("expecting server hello, got alert : [(AlertLevel_Fatal,HandshakeFailure)]",True,HandshakeFailure)))
    Error: processing vignette 'overview.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 67
    Execution halted
    ```

# DeepBlueR

Version: 1.6.0

## In both

*   R CMD check timed out
    

*   checking Rd files ... NOTE
    ```
    prepare_Rd: deepblue_enrich_regions_fast.Rd:35-38: Dropping empty section \examples
    ```

# dendroTools

Version: 0.0.8

## In both

*   checking whether package ‘dendroTools’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/dendroTools/new/dendroTools.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘dendroTools’ ...
** package ‘dendroTools’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘rJava’
ERROR: lazy loading failed for package ‘dendroTools’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/dendroTools/new/dendroTools.Rcheck/dendroTools’

```
### CRAN

```
* installing *source* package ‘dendroTools’ ...
** package ‘dendroTools’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘rJava’
ERROR: lazy loading failed for package ‘dendroTools’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/dendroTools/old/dendroTools.Rcheck/dendroTools’

```
# DEP

Version: 1.2.0

## In both

*   checking whether package ‘DEP’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘MSnbase::exprs’ by ‘dplyr::exprs’ when loading ‘DEP’
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/DEP/new/DEP.Rcheck/00install.out’ for details.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        data   1.4Mb
        doc    3.1Mb
    ```

# DepthProc

Version: 2.0.2

## In both

*   checking whether package ‘DepthProc’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/DepthProc/new/DepthProc.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘DepthProc’ ...
** package ‘DepthProc’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -fopenmp -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DepthProc/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DepthProc/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2 -c Depth.cpp -o Depth.o
clang: error: unsupported option '-fopenmp'
make: *** [Depth.o] Error 1
ERROR: compilation failed for package ‘DepthProc’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/DepthProc/new/DepthProc.Rcheck/DepthProc’

```
### CRAN

```
* installing *source* package ‘DepthProc’ ...
** package ‘DepthProc’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -fopenmp -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DepthProc/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DepthProc/RcppArmadillo/include" -I/usr/local/include   -fPIC  -Wall -g -O2 -c Depth.cpp -o Depth.o
clang: error: unsupported option '-fopenmp'
make: *** [Depth.o] Error 1
ERROR: compilation failed for package ‘DepthProc’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/DepthProc/old/DepthProc.Rcheck/DepthProc’

```
# DescriptiveStats.OBeu

Version: 1.3.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘curl’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 5764 marked UTF-8 strings
    ```

# detrendr

Version: 0.5.1

## In both

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# DiagrammeR

Version: 1.0.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

# DiffBind

Version: 2.8.0

## In both

*   checking whether package ‘DiffBind’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/DiffBind/new/DiffBind.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      ‘rgl’ ‘XLConnect’
    ```

## Installation

### Devel

```
* installing *source* package ‘DiffBind’ ...
** libs
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c RcppExports.cpp -o RcppExports.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c bamReader.cpp -o bamReader.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c bedReader.cpp -o bedReader.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c bitBucket.cpp -o bitBucket.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c croi_func.cpp -o croi_func.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c croi_main.cpp -o croi_main.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c densitySet.cpp -o densitySet.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c iBucket.cpp -o iBucket.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c interval.cpp -o interval.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c intervalDensity.cpp -o intervalDensity.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c intervalNode.cpp -o intervalNode.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c intervalSet.cpp -o intervalSet.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c intervalTree.cpp -o intervalTree.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c merge.cpp -o merge.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -Wall  -pedantic -O0 -c mergeOne.c -o mergeOne.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c nodeGroup.cpp -o nodeGroup.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c peakOrder.cpp -o peakOrder.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c reader.cpp -o reader.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c sequence.cpp -o sequence.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c util.cpp -o util.o
clang++ -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o DiffBind.so RcppExports.o bamReader.o bedReader.o bitBucket.o croi_func.o croi_main.o densitySet.o iBucket.o interval.o intervalDensity.o intervalNode.o intervalSet.o intervalTree.o merge.o mergeOne.o nodeGroup.o peakOrder.o reader.o sequence.o util.o /Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/usrlib//libbam.a /Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/usrlib//libbcf.a /Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/usrlib//libtabix.a -lz -pthread -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/lionel/Desktop/rlang/revdep/checks.noindex/DiffBind/new/DiffBind.Rcheck/DiffBind/libs
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘GO.db’
ERROR: lazy loading failed for package ‘DiffBind’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/DiffBind/new/DiffBind.Rcheck/DiffBind’

```
### CRAN

```
* installing *source* package ‘DiffBind’ ...
** libs
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c RcppExports.cpp -o RcppExports.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c bamReader.cpp -o bamReader.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c bedReader.cpp -o bedReader.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c bitBucket.cpp -o bitBucket.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c croi_func.cpp -o croi_func.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c croi_main.cpp -o croi_main.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c densitySet.cpp -o densitySet.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c iBucket.cpp -o iBucket.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c interval.cpp -o interval.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c intervalDensity.cpp -o intervalDensity.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c intervalNode.cpp -o intervalNode.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c intervalSet.cpp -o intervalSet.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c intervalTree.cpp -o intervalTree.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c merge.cpp -o merge.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -Wall  -pedantic -O0 -c mergeOne.c -o mergeOne.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c nodeGroup.cpp -o nodeGroup.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c peakOrder.cpp -o peakOrder.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c reader.cpp -o reader.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c sequence.cpp -o sequence.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c util.cpp -o util.o
clang++ -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o DiffBind.so RcppExports.o bamReader.o bedReader.o bitBucket.o croi_func.o croi_main.o densitySet.o iBucket.o interval.o intervalDensity.o intervalNode.o intervalSet.o intervalTree.o merge.o mergeOne.o nodeGroup.o peakOrder.o reader.o sequence.o util.o /Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/usrlib//libbam.a /Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/usrlib//libbcf.a /Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/usrlib//libtabix.a -lz -pthread -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/lionel/Desktop/rlang/revdep/checks.noindex/DiffBind/old/DiffBind.Rcheck/DiffBind/libs
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘GO.db’
ERROR: lazy loading failed for package ‘DiffBind’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/DiffBind/old/DiffBind.Rcheck/DiffBind’

```
# diffcyt

Version: 1.0.1

## In both

*   checking R code for possible problems ... NOTE
    ```
    calcCounts: no visible binding for global variable ‘cluster_id’
    calcCounts: no visible binding for global variable ‘sample_id’
    calcMedians: no visible binding for global variable ‘cluster_id’
    calcMedians: no visible binding for global variable ‘sample_id’
    calcMedians: no visible binding for global variable ‘value’
    calcMediansByClusterMarker: no visible binding for global variable
      ‘cluster_id’
    calcMediansByClusterMarker: no visible binding for global variable
      ‘marker’
    calcMediansByClusterMarker: no visible binding for global variable
      ‘value’
    calcMediansBySampleMarker: no visible binding for global variable
      ‘sample_id’
    calcMediansBySampleMarker: no visible binding for global variable
      ‘marker’
    calcMediansBySampleMarker: no visible binding for global variable
      ‘value’
    Undefined global functions or variables:
      cluster_id marker sample_id value
    ```

# DisImpact

Version: 0.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘magrittr’
      All declared Imports should be used.
    ```

# DiversityOccupancy

Version: 1.0.6

## In both

*   checking whether package ‘DiversityOccupancy’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/DiversityOccupancy/new/DiversityOccupancy.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘DiversityOccupancy’ ...
** package ‘DiversityOccupancy’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘rJava’
ERROR: lazy loading failed for package ‘DiversityOccupancy’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/DiversityOccupancy/new/DiversityOccupancy.Rcheck/DiversityOccupancy’

```
### CRAN

```
* installing *source* package ‘DiversityOccupancy’ ...
** package ‘DiversityOccupancy’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘rJava’
ERROR: lazy loading failed for package ‘DiversityOccupancy’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/DiversityOccupancy/old/DiversityOccupancy.Rcheck/DiversityOccupancy’

```
# dodgr

Version: 0.0.3

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Loading dodgr
    Quitting from lines 117-144 (benchmark.Rmd) 
    Error: processing vignette 'benchmark.Rmd' failed with diagnostics:
    polygon edge not found
    Execution halted
    ```

# dplyr

Version: 0.7.5

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.5Mb
      sub-directories of 1Mb or more:
        libs   4.4Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4 marked UTF-8 strings
    ```

# dplyr.teradata

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘bit64’ ‘rstudioapi’ ‘tidyverse’
      All declared Imports should be used.
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

# driftR

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ggplot2’ ‘readr’
      All declared Imports should be used.
    ```

# dtwclust

Version: 5.4.0

## In both

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# dynfrail

Version: 0.5.2

## In both

*   checking whether package ‘dynfrail’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/dynfrail/new/dynfrail.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘dynfrail’ ...
** package ‘dynfrail’ successfully unpacked and MD5 sums checked
** libs
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/dynfrail/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/dynfrail/RcppArmadillo/include" -I/usr/local/include  -fopenmp  -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘dynfrail’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/dynfrail/new/dynfrail.Rcheck/dynfrail’

```
### CRAN

```
* installing *source* package ‘dynfrail’ ...
** package ‘dynfrail’ successfully unpacked and MD5 sums checked
** libs
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/dynfrail/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/dynfrail/RcppArmadillo/include" -I/usr/local/include  -fopenmp  -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘dynfrail’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/dynfrail/old/dynfrail.Rcheck/dynfrail’

```
# ecoengine

Version: 1.11.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘magrittr’
      All declared Imports should be used.
    ```

# eda4treeR

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dae’ ‘dplyr’
      All declared Imports should be used.
    ```

# eechidna

Version: 1.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.3Mb
      sub-directories of 1Mb or more:
        data   4.9Mb
        doc    1.2Mb
    ```

# EFDR

Version: 0.1.1

## In both

*   checking R code for possible problems ... NOTE
    ```
    .gdf : find_loss: no visible global function definition for ‘rnorm’
    .p.values : <anonymous>: no visible global function definition for
      ‘pnorm’
    .relist.dwt: no visible global function definition for ‘relist’
    .relist.dwt: no visible global function definition for ‘as’
    .std.wav.coeff : <anonymous>: no visible global function definition for
      ‘mad’
    regrid: no visible global function definition for ‘predict’
    regrid: no visible global function definition for ‘var’
    regrid: no visible global function definition for ‘medpolish’
    Undefined global functions or variables:
      as mad medpolish pnorm predict relist rnorm var
    Consider adding
      importFrom("methods", "as")
      importFrom("stats", "mad", "medpolish", "pnorm", "predict", "rnorm",
                 "var")
      importFrom("utils", "relist")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# ELMER.data

Version: 2.4.2

## In both

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .travis.yml
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 288.8Mb
      sub-directories of 1Mb or more:
        data  286.3Mb
        doc     2.4Mb
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Description field: should contain one or more complete sentences.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Error in .requirePackage(package) : 
        unable to find required package 'MultiAssayExperiment'
      Calls: <Anonymous> ... getClass -> getClassDef -> .classEnv -> .requirePackage
      Execution halted
    ```

# emil

Version: 2.2.8

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘Rcpp’
      All declared Imports should be used.
    ```

# ENCODExplorer

Version: 2.6.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 73.6Mb
      sub-directories of 1Mb or more:
        data     23.9Mb
        doc       1.5Mb
        extdata  48.0Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘biosample_type’
    step6_control: no visible binding for global variable ‘controls’
    step6_date_released: no visible binding for global variable
      ‘date_released’
    step6_status: no visible binding for global variable ‘status’
    step6_target: no visible binding for global variable ‘target’
    step7: no visible binding for global variable ‘organism’
    step8: no visible binding for global variable ‘investigated_as’
    step8: no visible binding for global variable ‘target’
    step9: no visible binding for global variable ‘organism’
    Undefined global functions or variables:
      . Experiment Value accession antibody_caption
      antibody_characterization antibody_target assay
      biological_replicate_number biosample_name biosample_type col_name
      controls data date_released download.file encode_df file_accession
      file_format href investigated_as lab nucleic_acid_term organism
      platform project replicate_antibody replicate_library server status
      submitted_by target technical_replicate_number treatment ui value
    Consider adding
      importFrom("utils", "data", "download.file")
    to your NAMESPACE file.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 771 marked UTF-8 strings
    ```

# epicontacts

Version: 1.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘colorspace’
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

# ERSA

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘RColorBrewer’ ‘modelr’
      All declared Imports should be used.
    ```

# esc

Version: 0.4.1

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘metafor’
    ```

# etl

Version: 0.3.7

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Loading required package: dplyr
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    pandoc: Could not fetch http://www.r-pkg.org/badges/version/macleish
    TlsException (HandshakeFailed (Error_Protocol ("expecting server hello, got alert : [(AlertLevel_Fatal,HandshakeFailure)]",True,HandshakeFailure)))
    Error: processing vignette 'extending_etl.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 67
    Execution halted
    ```

# eurostat

Version: 3.2.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘curl’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 484 marked UTF-8 strings
    ```

# evaluator

Version: 0.2.3

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      7: rmarkdown::render(system.file("rmd", "risk_dashboard.Rmd", package = "evaluator"), 
             output_options = list(css = styles, favicon = icon, logo = icon), output_file = output_file, 
             intermediates_dir = intermediates_dir, params = list(input_directory = input_directory, 
                 results_directory = results_directory), quiet = quiet, ...)
      8: convert(output_file, run_citeproc)
      9: pandoc_convert(utf8_input, pandoc_to, output_format$pandoc$from, output, citeproc, 
             output_format$pandoc$args, !quiet)
      10: stop("pandoc document conversion failed with error ", result, call. = FALSE)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 83 SKIPPED: 3 FAILED: 1
      1. Error: Risk Dashboard renders (@test-reports.R#62) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# EventStudy

Version: 0.34

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.6Mb
      sub-directories of 1Mb or more:
        doc   5.8Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘curl’ ‘openxlsx’ ‘stringr’
      All declared Imports should be used.
    ```

# exifr

Version: 0.2.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 13.3Mb
      sub-directories of 1Mb or more:
        exiftool  12.3Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# ExPanDaR

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘CodeDepends’ ‘DT’ ‘PKI’ ‘shinycssloaders’ ‘tictoc’
      All declared Imports should be used.
    ```

# factoextra

Version: 1.0.5

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘NbClust’
    ```

# GenomicDataCommons

Version: 1.4.1

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Loading required package: magrittr
    
    Attaching package: 'GenomicDataCommons'
    
    The following object is masked from 'package:stats':
    
        filter
    
    Quitting from lines 125-127 (overview.Rmd) 
    Error: processing vignette 'overview.Rmd' failed with diagnostics:
    failed to rename downloaded file:
    
      from: '/Users/lionel/Library/Caches/GenomicDataCommons/96091213-41a1-40b9-8e7b-297ed5742a60/.partial_download'
      to: '/Users/lionel/Library/Caches/GenomicDataCommons/96091213-41a1-40b9-8e7b-297ed5742a60/094dbc70-aa59-40b1-8ceb-1ce6e5e6bf01.htseq.counts.gz'
      reason:
        cannot rename file
        '/Users/lionel/Library/Caches/GenomicDataCommons/96091213-41a1-40b9-8e7b-297ed5742a60/.partial_download'
        to
        '/Users/lionel/Library/Caches/GenomicDataCommons/96091213-41a1-40b9-8e7b-297ed5742a60/094dbc70-aa59-40b1-8ceb-1ce6e5e6bf01.htseq.counts.gz',
        reason 'No such file or directory'
    Execution halted
    ```

## Newly fixed

*   R CMD check timed out
    

## In both

*   checking R code for possible problems ... NOTE
    ```
    default_fields.character: no visible binding for global variable
      ‘defaults’
    gdc_rnaseq: no visible binding for global variable ‘case_id’
    gdc_rnaseq: no visible binding for global variable ‘file_id’
    Undefined global functions or variables:
      case_id defaults file_id
    ```

# geoparser

Version: 0.1.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc: Could not fetch http://www.ropensci.org/public_images/github_footer.png
    TlsException (HandshakeFailed (Error_Protocol ("expecting server hello, got alert : [(AlertLevel_Fatal,HandshakeFailure)]",True,HandshakeFailure)))
    Error: processing vignette 'geoparser.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 67
    Execution halted
    ```

# GEOquery

Version: 2.48.0

## Newly fixed

*   checking examples ... ERROR
    ```
    ...
    Found 1 file(s)
    GSE10_series_matrix.txt.gz
    trying URL 'https://ftp.ncbi.nlm.nih.gov/geo/series/GSEnnn/GSE10/matrix/GSE10_series_matrix.txt.gz'
    Content type 'application/x-gzip' length 364007 bytes (355 KB)
    ==================================================
    downloaded 355 KB
    
    Parsed with column specification:
    cols(
      TAG = col_character(),
      GSM571 = col_integer(),
      GSM572 = col_integer(),
      GSM573 = col_integer(),
      GSM574 = col_integer()
    )
    Warning in download.file(myurl, destfile, mode = mode, quiet = TRUE, method = getOption("download.file.method.GEOquery")) :
      URL 'https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?targ=self&acc=GPL4&form=text&view=full': status was 'Transferred a partial file'
    Error in download.file(myurl, destfile, mode = mode, quiet = TRUE, method = getOption("download.file.method.GEOquery")) : 
      download from 'https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?targ=self&acc=GPL4&form=text&view=full' failed
    Calls: getGEO ... parseGSEMatrix -> getGEO -> getGEOfile -> download.file
    Execution halted
    ```

## In both

*   R CMD check timed out
    

*   checking installed package size ... NOTE
    ```
      installed size is 13.8Mb
      sub-directories of 1Mb or more:
        extdata  12.8Mb
    ```

*   checking whether the namespace can be loaded with stated dependencies ... NOTE
    ```
    Warning: no function found corresponding to methods exports from ‘GEOquery’ for: ‘show’
    
    A namespace must be able to be loaded with just the base namespace
    loaded: otherwise if the namespace gets loaded by a saved object, the
    session will be unable to start.
    
    Probably some imports need to be declared in the NAMESPACE file.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘httr’
      All declared Imports should be used.
    Package in Depends field not imported from: ‘methods’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    fastTabRead: no visible global function definition for ‘read.delim’
    parseGDS: no visible global function definition for ‘new’
    parseGSE: no visible global function definition for ‘new’
    parseGSEMatrix: no visible global function definition for ‘read.table’
    parseGSEMatrix: no visible binding for global variable ‘.’
    parseGSEMatrix: no visible binding for global variable
      ‘characteristics’
    parseGSEMatrix: no visible binding for global variable ‘kvpair’
    parseGSEMatrix: no visible binding for global variable ‘accession’
    parseGSEMatrix: no visible binding for global variable ‘k’
    parseGSEMatrix: no visible binding for global variable ‘v’
    parseGSEMatrix: no visible global function definition for ‘new’
    parseGSEMatrix: no visible global function definition for ‘as’
    Undefined global functions or variables:
      . MA accession as characteristics k kvpair new read.delim read.table
      v
    Consider adding
      importFrom("methods", "as", "new")
      importFrom("utils", "read.delim", "read.table")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# GerminaR

Version: 1.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DT’ ‘shinydashboard’
      All declared Imports should be used.
    ```

# getCRUCLdata

Version: 0.2.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rappdirs’
      All declared Imports should be used.
    ```

# GetITRData

Version: 0.7

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 99-112 (gitrd-vignette-introduction.Rmd) 
    Error: processing vignette 'gitrd-vignette-introduction.Rmd' failed with diagnostics:
    download from 'http://www.rad.cvm.gov.br/enetconsulta/frmDownloadDocumento.aspx?CodigoInstituicao=2&NumeroSequencialDocumento=5189' failed
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

# ggalt

Version: 0.4.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘plotly’
      All declared Imports should be used.
    ```

# ggdag

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ggforce’ ‘plyr’
      All declared Imports should be used.
    ```

# ggeffects

Version: 0.3.3

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘ordinal’
    ```

# ggenealogy

Version: 0.3.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tibble’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2356 marked UTF-8 strings
    ```

# ggfan

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘colorspace’ ‘grid’ ‘rstan’
      All declared Imports should be used.
    ```

# ggformula

Version: 0.7.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘quantreg’
    ```

# ggguitar

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘gridExtra’ ‘lazyeval’ ‘readr’
      All declared Imports should be used.
    ```

# gglogo

Version: 0.1.3

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Error in re-building vignettes:
      ...
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      The vignette engine knitr::rmarkdown is not available, because the rmarkdown package is not installed. Please install it.
    
    Attaching package: 'gglogo'
    
    The following object is masked from 'package:ggplot2':
    
        fortify
    
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      no font could be found for family "Garamond"
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      no font could be found for family "Garamond"
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      no font could be found for family "Garamond"
    Quitting from lines 46-52 (gglogo-alphabet.Rmd) 
    Error: processing vignette 'gglogo-alphabet.Rmd' failed with diagnostics:
    replacement has 1 row, data has 0
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘knitr’
      All declared Imports should be used.
    ```

# ggmcmc

Version: 1.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
      no font could be found for family "Source Sans Pro"
    Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  :
      no font could be found for family "Source Sans Pro"
    Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  :
      no font could be found for family "Source Sans Pro"
    Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  :
      no font could be found for family "Source Sans Pro"
    Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  :
      no font could be found for family "Source Sans Pro"
    Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  :
      no font could be found for family "Source Sans Pro"
    Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  :
      no font could be found for family "Source Sans Pro"
    Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  :
      no font could be found for family "Source Sans Pro"
    Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  :
      no font could be found for family "Source Sans Pro"
    Quitting from lines 142-143 (using_ggmcmc.Rmd) 
    Error: processing vignette 'using_ggmcmc.Rmd' failed with diagnostics:
    polygon edge not found
    Execution halted
    ```

# ggmosaic

Version: 0.1.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘NHANES’ ‘gridExtra’
      All declared Imports should be used.
    ```

# ggplotAssist

Version: 0.1.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘gcookbook’ ‘ggthemes’ ‘moonBook’ ‘tidyverse’
      All declared Imports should be used.
    ```

# ggpmisc

Version: 0.2.17

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘devtools’ ‘grid’ ‘gridExtra’
      All declared Imports should be used.
    ```

# ggpubr

Version: 0.1.6

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘FactoMineR’
    ```

# ggQQunif

Version: 0.1.5

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# ggquickeda

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DT’ ‘Hmisc’ ‘colourpicker’ ‘dplyr’ ‘ggrepel’ ‘grDevices’ ‘gridExtra’
      ‘lazyeval’ ‘markdown’ ‘plotly’ ‘quantreg’ ‘rlang’ ‘shinyjs’ ‘tidyr’
      All declared Imports should be used.
    ```

# ggRandomForests

Version: 2.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘randomForest’
      All declared Imports should be used.
    ```

# ggraptR

Version: 1.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘RSelenium’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DBI’ ‘GGally’ ‘RColorBrewer’ ‘Rcpp’ ‘assertthat’ ‘backports’
      ‘colorspace’ ‘colourpicker’ ‘evaluate’ ‘futile.options’ ‘gdtools’
      ‘gtable’ ‘htmltools’ ‘htmlwidgets’ ‘httpuv’ ‘labeling’ ‘lambda.r’
      ‘lazyeval’ ‘magrittr’ ‘miniUI’ ‘munsell’ ‘plyr’ ‘reshape’ ‘rprojroot’
      ‘scales’ ‘stringi’ ‘stringr’ ‘svglite’ ‘tibble’ ‘xtable’ ‘yaml’
      All declared Imports should be used.
    ```

# ggridges

Version: 0.5.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 6242 marked UTF-8 strings
    ```

# ggstance

Version: 0.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘lazyeval’
      All declared Imports should be used.
    ```

# ggthemes

Version: 3.5.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘latticeExtra’
    ```

# ggtree

Version: 1.12.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
        rotate
    
    Average angle change [1] 0.205749672759501
    Average angle change [2] 0.0594793517814446
    Average angle change [3] 0.0265103772447353
    Warning in min(y) : no non-missing arguments to min; returning Inf
    Warning in max(y) : no non-missing arguments to max; returning -Inf
    Warning: Removed 1 rows containing missing values (geom_point_g_gtree).
    Warning: Removed 1 rows containing missing values (geom_point_g_gtree).
    Warning: Removed 13 rows containing missing values (geom_text).
    Warning: Removed 12 rows containing missing values (geom_text).
    Scale for 'fill' is already present. Adding another scale for 'fill',
    which will replace the existing scale.
    Warning: The plyr::rename operation has created duplicates for the following name(s): (`size`)
    Loading required package: Biostrings
    Warning in library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE,  :
      there is no package called 'Biostrings'
    Quitting from lines 301-303 (treeAnnotation.Rmd) 
    Error: processing vignette 'treeAnnotation.Rmd' failed with diagnostics:
    object 'readBStringSet' not found
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  9.2Mb
      sub-directories of 1Mb or more:
        doc        4.9Mb
        examples   3.7Mb
    ```

# ggvis

Version: 0.4.3

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘plyr’
    ```

# gogamer

Version: 0.4.3

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(gogamer)
      > 
      > test_check("gogamer")
      Error: segfault from C stack overflow
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 36 SKIPPED: 1 FAILED: 0
      Execution halted
    ```

# grattan

Version: 1.6.0.0

## In both

*   R CMD check timed out
    

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘taxstats’
    ```

# Greg

Version: 1.2.2

## In both

*   checking whether package ‘Greg’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/Greg/new/Greg.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘Greg’ ...
** package ‘Greg’ successfully unpacked and MD5 sums checked
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘quantreg’
ERROR: lazy loading failed for package ‘Greg’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/Greg/new/Greg.Rcheck/Greg’

```
### CRAN

```
* installing *source* package ‘Greg’ ...
** package ‘Greg’ successfully unpacked and MD5 sums checked
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘quantreg’
ERROR: lazy loading failed for package ‘Greg’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/Greg/old/Greg.Rcheck/Greg’

```
# hdme

Version: 0.2.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘Rglpk’
    ```

# hei

Version: 0.1.0

## In both

*   R CMD check timed out
    

*   checking files in ‘vignettes’ ... NOTE
    ```
    Files named as vignettes but with no recognized vignette engine:
       ‘vignettes/Analysis.Rmd’
    (Is a VignetteBuilder field missing?)
    ```

# hiAnnotator

Version: 1.14.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    makeChunks: no visible global function definition for ‘breakInChunks’
    makeChunks: no visible global function definition for ‘detectCores’
    makeChunks : <anonymous>: no visible global function definition for
      ‘keepSeqlevels’
    makeChunks : <anonymous>: no visible global function definition for
      ‘seqlevelsInUse’
    makeGRanges: no visible global function definition for ‘IRanges’
    makeGRanges: no visible global function definition for ‘seqlengths’
    makeGRanges: no visible global function definition for ‘seqlevels<-’
    makeGRanges: no visible global function definition for ‘sortSeqlevels’
    makeGRanges: no visible global function definition for ‘seqlevelsInUse’
    makeGRanges: no visible global function definition for ‘seqlengths<-’
    makeGRanges: no visible global function definition for ‘seqlevels’
    Undefined global functions or variables:
      IRanges breakInChunks countQueryHits detectCores dist featureName
      keepSeqlevels mid n overlapsAny qStrand queryHits seqlengths
      seqlengths<- seqlevels seqlevels<- seqlevelsInUse sortSeqlevels
      subjectHits
    Consider adding
      importFrom("stats", "dist")
    to your NAMESPACE file.
    ```

*   checking files in ‘vignettes’ ... NOTE
    ```
    The following directory looks like a leftover from 'knitr':
      ‘figure’
    Please remove from your package.
    ```

# HiCcompare

Version: 1.2.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.5Mb
      sub-directories of 1Mb or more:
        data   5.6Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    sim.other.methods: no visible binding for global variable ‘IF2’
    sim.other.methods: no visible binding for global variable ‘adj.M’
    sim.other.methods: no visible binding for global variable ‘M’
    sim_matrix: no visible binding for global variable ‘bias.slope’
    total_sum: no visible binding for global variable ‘IF2’
    total_sum: no visible binding for global variable ‘M’
    total_sum: no visible binding for global variable ‘IF1’
    total_sum: no visible binding for global variable ‘chr1’
    volcano: no visible binding for global variable ‘A’
    volcano: no visible binding for global variable ‘adj.IF1’
    volcano: no visible binding for global variable ‘adj.IF2’
    volcano: no visible binding for global variable ‘p.value’
    volcano: no visible binding for global variable ‘D’
    Undefined global functions or variables:
      A D IF IF1 IF2 M Z adj.IF1 adj.IF2 adj.M axis bias.slope bp
      centromere_locations chr1 chr2 count fold.change i j p.adj p.value
      pnorm region1 region2 start1 start2
    Consider adding
      importFrom("graphics", "axis")
      importFrom("stats", "D", "pnorm")
    to your NAMESPACE file.
    ```

# highcharter

Version: 0.5.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 16.5Mb
      sub-directories of 1Mb or more:
        doc          13.7Mb
        htmlwidgets   1.8Mb
    ```

# hiReadsProcessor

Version: 1.16.0

## In both

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .BBSoptions
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘qBaseInsert’
    read.psl : <anonymous>: no visible binding for global variable
      ‘tBaseInsert’
    read.psl: no visible binding for global variable ‘matches’
    read.psl: no visible binding for global variable ‘misMatches’
    read.psl: no visible binding for global variable ‘qBaseInsert’
    read.psl: no visible binding for global variable ‘tBaseInsert’
    read.sampleInfo: no visible global function definition for ‘SimpleList’
    splitSeqsToFiles: no visible global function definition for
      ‘fasta.info’
    vpairwiseAlignSeqs: no visible global function definition for ‘Rle’
    vpairwiseAlignSeqs: no visible global function definition for
      ‘runLength’
    vpairwiseAlignSeqs: no visible global function definition for ‘IRanges’
    vpairwiseAlignSeqs: no visible global function definition for
      ‘runValue’
    Undefined global functions or variables:
      DataFrame IRanges IRangesList Rle ScanBamParam SimpleList
      breakInChunks clusteredValue clusteredValue.freq detectCores
      fasta.info matches mclapply metadata metadata<- misMatches
      qBaseInsert queryHits runLength runValue scanBamFlag tBaseInsert
    ```

# HURDAT

Version: 0.2.0

## In both

*   R CMD check timed out
    

# hurricaneexposure

Version: 0.0.1

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘hurricaneexposuredata’
    ```

# huxtable

Version: 3.0.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘xtable’
    ```

# HydeNet

Version: 0.10.7

## In both

*   checking examples ... ERROR
    ```
    ...
    > 
    > ### ** Examples
    > 
    > data(PE, package="HydeNet")
    > Net <- HydeNetwork(~ wells + 
    +                      pe | wells + 
    +                      d.dimer | pregnant*pe + 
    +                      angio | pe + 
    +                      treat | d.dimer*angio + 
    +                      death | pe*treat,
    +                      data = PE) 
    >   
    >                  
    > compiledNet <- compileJagsModel(Net, n.chains=5)
    Error: .onLoad failed in loadNamespace() for 'rjags', details:
      call: dyn.load(file, DLLpath = DLLpath, ...)
      error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/HydeNet/rjags/libs/rjags.so':
      dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/HydeNet/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
      Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/HydeNet/rjags/libs/rjags.so
      Reason: image not found
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      16: tryCatch(loadNamespace(name), error = function(e) stop(e))
      17: tryCatchList(expr, classes, parentenv, handlers)
      18: tryCatchOne(expr, names, parentenv, handlers[[1L]])
      19: value[[3L]](cond)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 60 SKIPPED: 0 FAILED: 5
      1. Error: (unknown) (@test-HydePosterior.R#11) 
      2. Error: (unknown) (@test-bindPosterior.R#12) 
      3. Error: compileJagsModel returns an object of class 'compiledHydeNetwork' (@test-compileJagsModel.R#14) 
      4. Error: (unknown) (@test-print.HydePosterior.R#11) 
      5. Error: compileDecisionModel (@test_compileDecisionModel.R#14) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      The vignette engine knitr::rmarkdown is not available, because the rmarkdown package is not installed. Please install it.
    Loading required package: nnet
    Quitting from lines 314-325 (DecisionNetworks.Rmd) 
    Error: processing vignette 'DecisionNetworks.Rmd' failed with diagnostics:
    .onLoad failed in loadNamespace() for 'rjags', details:
      call: dyn.load(file, DLLpath = DLLpath, ...)
      error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/HydeNet/rjags/libs/rjags.so':
      dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/HydeNet/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
      Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/HydeNet/rjags/libs/rjags.so
      Reason: image not found
    Execution halted
    ```

# ideal

Version: 1.4.0

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘GO.db’
    
    Packages suggested but not available for checking:
      ‘airway’ ‘org.Hs.eg.db’ ‘TxDb.Hsapiens.UCSC.hg38.knownGene’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# idealstan

Version: 0.2.7

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.0Mb
      sub-directories of 1Mb or more:
        data   3.5Mb
        libs   3.6Mb
    ```

# idefix

Version: 0.2.4

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘bayesm’, ‘ChoiceModelR’, ‘RSGHB’
    ```

# ijtiff

Version: 1.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘grDevices’
      All declared Imports should be used.
    ```

# imager

Version: 0.41.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -Dcimg_r_mode -fpermissive -I/usr/X11R6/include -I/opt/X11/include  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/imager/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/checks.noindex/imager/new/imager.Rcheck/imager/include" -I"/private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/Rtmppwzinv/sourceCpp-x86_64-apple-darwin15.6.0-0.12.17" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c file127ab75e63222.cpp -o file127ab75e63222.o
      clang++ -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o sourceCpp_2.so file127ab75e63222.o -lX11 -L/usr/X11R6/lib -L/opt/X11/include -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
      ── 1. Error: cpp_plugin (@test_cpp_api.R#14)  ──────────────────────────────────
      Error 1 occurred building shared library.
      1: cppFunction(foo.inline, depends = "imager") at testthat/test_cpp_api.R:14
      2: sourceCpp(code = code, env = env, rebuild = rebuild, cacheDir = cacheDir, showOutput = showOutput, 
             verbose = verbose)
      3: stop("Error ", status, " occurred building shared library.")
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 17 SKIPPED: 0 FAILED: 1
      1. Error: cpp_plugin (@test_cpp_api.R#14) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 10.1Mb
      sub-directories of 1Mb or more:
        data      1.4Mb
        doc       1.1Mb
        include   2.8Mb
        libs      3.1Mb
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

# incadata

Version: 0.6.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 568 marked UTF-8 strings
    ```

# incgraph

Version: 1.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘testthat’
      All declared Imports should be used.
    ```

# incR

Version: 1.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘rgeos’
      All declared Imports should be used.
    ```

# inlabru

Version: 2.1.3

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘INLA’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘INLA’
    ```

# ipeaData

Version: 0.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘RCurl’
      All declared Imports should be used.
    ```

# iRF

Version: 2.0.0

## In both

*   checking whether package ‘iRF’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/iRF/new/iRF.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘iRF’ ...
** package ‘iRF’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/iRF/Rcpp/include" -I/usr/local/include  -fopenmp -fPIC  -Wall -g -O2 -c ExportedFunctionsRIT.cpp -o ExportedFunctionsRIT.o
clang: error: unsupported option '-fopenmp'
make: *** [ExportedFunctionsRIT.o] Error 1
ERROR: compilation failed for package ‘iRF’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/iRF/new/iRF.Rcheck/iRF’

```
### CRAN

```
* installing *source* package ‘iRF’ ...
** package ‘iRF’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/iRF/Rcpp/include" -I/usr/local/include  -fopenmp -fPIC  -Wall -g -O2 -c ExportedFunctionsRIT.cpp -o ExportedFunctionsRIT.o
clang: error: unsupported option '-fopenmp'
make: *** [ExportedFunctionsRIT.o] Error 1
ERROR: compilation failed for package ‘iRF’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/iRF/old/iRF.Rcheck/iRF’

```
# IrisSpatialFeatures

Version: 1.3.0

## In both

*   R CMD check timed out
    

# ITNr

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Matrix’ ‘animation’ ‘comtradr’ ‘ndtv’ ‘statnet’ ‘xergm’
      All declared Imports should be used.
    ```

# janeaustenr

Version: 0.1.5

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

# janitor

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘magrittr’
      All declared Imports should be used.
    ```

# jpmesh

Version: 1.1.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 176 marked UTF-8 strings
    ```

# jpndistrict

Version: 0.3.1

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

# KraljicMatrix

Version: 0.2.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘tibble’
      All declared Imports should be used.
    ```

# labelled

Version: 1.1.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘memisc’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘memisc’
    ```

# Lahman

Version: 6.0-0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.4Mb
      sub-directories of 1Mb or more:
        data   7.2Mb
    ```

# leaflet.minicharts

Version: 0.5.3

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
    
    pandoc: Could not fetch /Users/lionel/Desktop/rlang/revdep/library.noindex/leaflet.minicharts/leaflet/htmlwidgets/lib/leaflet/#default#VML
    /Users/lionel/Desktop/rlang/revdep/library.noindex/leaflet.minicharts/leaflet/htmlwidgets/lib/leaflet/: openBinaryFile: inappropriate type (is a directory)
    Error: processing vignette 'introduction.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 67
    Execution halted
    ```

# listarrays

Version: 0.1.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘keras’
    ```

# lmeresampler

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘boot’
      All declared Imports should be used.
    ```

# LocFDRPois

Version: 1.0.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    AnalyticalOptim: no visible global function definition for ‘optim’
    LLConstructor : LL: no visible global function definition for ‘dpois’
    MixtureDensity: no visible global function definition for ‘glm’
    MixtureDensity : f_hat: no visible global function definition for
      ‘predict’
    NullDensity : f0: no visible global function definition for ‘dpois’
    Undefined global functions or variables:
      dpois glm optim predict
    Consider adding
      importFrom("stats", "dpois", "glm", "optim", "predict")
    to your NAMESPACE file.
    ```

# loopr

Version: 1.0.1

## In both

*   checking R code for possible problems ... NOTE
    ```
    amendColumns: no visible global function definition for ‘setNames’
    fillColumns: no visible global function definition for ‘setNames’
    Undefined global functions or variables:
      setNames
    Consider adding
      importFrom("stats", "setNames")
    to your NAMESPACE file.
    ```

# lucid

Version: 1.4

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      The vignette engine knitr::rmarkdown is not available, because the rmarkdown package is not installed. Please install it.
    Loading required package: lucid
    Loading required package: lattice
    Loading required package: rjags
    Loading required package: coda
    Error: package or namespace load failed for 'rjags':
     .onLoad failed in loadNamespace() for 'rjags', details:
      call: dyn.load(file, DLLpath = DLLpath, ...)
      error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/lucid/rjags/libs/rjags.so':
      dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/lucid/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
      Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/lucid/rjags/libs/rjags.so
      Reason: image not found
    Quitting from lines 269-293 (lucid_printing.Rmd) 
    Error: processing vignette 'lucid_printing.Rmd' failed with diagnostics:
    could not find function "jags.model"
    Execution halted
    ```

# mafs

Version: 0.0.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Rcpp’ ‘cmprsk’ ‘colorspace’ ‘etm’ ‘fracdiff’ ‘gtable’ ‘munsell’
      ‘numDeriv’ ‘plyr’ ‘quadprog’ ‘scales’ ‘timeDate’ ‘tseries’ ‘zoo’
      All declared Imports should be used.
    ```

# malariaAtlas

Version: 0.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘grid’
      All declared Imports should be used.
    ```

# MANOVA.RM

Version: 0.2.1

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘RGtk2’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘nparLD’
    ```

# mapedit

Version: 0.4.1

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘geojsonio’
    ```

# mapfuser

Version: 0.1.2

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘LPmerge’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# markmyassignment

Version: 0.8.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘lazyeval’
      All declared Imports should be used.
    ```

# mason

Version: 0.2.5

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘ggplot2’, ‘pander’, ‘pixiedust’
    ```

# mbgraphic

Version: 1.0.0

## In both

*   checking whether package ‘mbgraphic’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/mbgraphic/new/mbgraphic.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘mbgraphic’ ...
** package ‘mbgraphic’ successfully unpacked and MD5 sums checked
** libs
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mbgraphic/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c RcppExports.cpp -o RcppExports.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mbgraphic/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c cmasum.cpp -o cmasum.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mbgraphic/Rcpp/include" -I/usr/local/include   -fPIC  -Wall  -pedantic -O0 -c mbgraphic_init.c -o mbgraphic_init.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mbgraphic/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c variableflip.cpp -o variableflip.o
clang++ -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o mbgraphic.so RcppExports.o cmasum.o mbgraphic_init.o variableflip.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/lionel/Desktop/rlang/revdep/checks.noindex/mbgraphic/new/mbgraphic.Rcheck/mbgraphic/libs
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘rJava’
ERROR: lazy loading failed for package ‘mbgraphic’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/mbgraphic/new/mbgraphic.Rcheck/mbgraphic’

```
### CRAN

```
* installing *source* package ‘mbgraphic’ ...
** package ‘mbgraphic’ successfully unpacked and MD5 sums checked
** libs
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mbgraphic/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c RcppExports.cpp -o RcppExports.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mbgraphic/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c cmasum.cpp -o cmasum.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mbgraphic/Rcpp/include" -I/usr/local/include   -fPIC  -Wall  -pedantic -O0 -c mbgraphic_init.c -o mbgraphic_init.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mbgraphic/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c variableflip.cpp -o variableflip.o
clang++ -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o mbgraphic.so RcppExports.o cmasum.o mbgraphic_init.o variableflip.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/lionel/Desktop/rlang/revdep/checks.noindex/mbgraphic/old/mbgraphic.Rcheck/mbgraphic/libs
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘rJava’
ERROR: lazy loading failed for package ‘mbgraphic’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/mbgraphic/old/mbgraphic.Rcheck/mbgraphic’

```
# mdsr

Version: 0.1.5

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        data   5.4Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2694 marked UTF-8 strings
    ```

# mem

Version: 2.12

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyr’
      All declared Imports should be used.
    ```

# memapp

Version: 2.9

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DT’ ‘RColorBrewer’ ‘RODBC’ ‘dplyr’ ‘formattable’ ‘ggplot2’
      ‘ggthemes’ ‘magrittr’ ‘mem’ ‘openxlsx’ ‘plotly’ ‘readxl’ ‘shinyBS’
      ‘shinydashboard’ ‘shinyjs’ ‘shinythemes’ ‘stringi’ ‘stringr’ ‘tidyr’
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

# metacoder

Version: 0.2.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ggrepel’ ‘reshape’
      All declared Imports should be used.
    ```

# MetaCyto

Version: 1.2.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    collectData: no visible binding for global variable ‘parameter_name’
    collectData: no visible binding for global variable ‘value’
    panelSummary: no visible binding for global variable ‘antibodies’
    panelSummary: no visible binding for global variable ‘value’
    plotGA: no visible binding for global variable ‘lower’
    plotGA: no visible binding for global variable ‘upper’
    searchCluster : <anonymous>: no visible binding for global variable
      ‘triS’
    Undefined global functions or variables:
      antibodies lower parameter_name triS upper value
    ```

# metagene

Version: 2.12.0

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘EnsDb.Hsapiens.v86’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# metagenomeFeatures

Version: 2.0.0

## In both

*   checking whether package ‘metagenomeFeatures’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: subclass "QualityScaledDNAStringSet" of class "DNAStringSet" is not local and cannot be updated for new inheritance information; consider setClassUnion()
      Warning: replacing previous import ‘lazyeval::is_formula’ by ‘purrr::is_formula’ when loading ‘metagenomeFeatures’
      Warning: replacing previous import ‘lazyeval::is_atomic’ by ‘purrr::is_atomic’ when loading ‘metagenomeFeatures’
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/metagenomeFeatures/new/metagenomeFeatures.Rcheck/00install.out’ for details.
    ```

*   checking for missing documentation entries ... WARNING
    ```
    Undocumented S4 methods:
      generic '[' and siglist 'mgFeatures'
    All user-level objects in a package (including S4 classes and methods)
    should have documentation entries.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        extdata   3.5Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    .select: no visible binding for global variable ‘identifier’
    .select.taxa: no visible binding for global variable ‘Keys’
    .select.taxa: no visible binding for global variable ‘.’
    get_gg13.8_85MgDb: no visible binding for global variable ‘metadata’
    Undefined global functions or variables:
      . Keys identifier metadata
    ```

# mfa

Version: 1.2.0

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘mfa-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: calculate_chi
    > ### Title: Calculate posterior chi precision parameters
    > ### Aliases: calculate_chi
    > 
    > ### ** Examples
    > 
    > synth <- create_synthetic(C = 20, G = 5)
    > m <- mfa(synth$X)
    Sampling for 20 cells and 5 genes
    Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
      there is no package called ‘quantreg’
    Calls: mfa ... tryCatch -> tryCatchList -> tryCatchOne -> <Anonymous>
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 7 SKIPPED: 0 FAILED: 9
      1. Error: A call to MFA returns a valid object (@test-general.R#13) 
      2. Error: Conditional distribution of k is correct (@test-mcmc.R#63) 
      3. Error: Conditional distirbution of c is correct (@test-mcmc.R#80) 
      4. Error: Conditional distribution of t is correct (@test-mcmc.R#97) 
      5. Error: Conditional distribution of tau is correct (@test-mcmc.R#114) 
      6. Error: Conditional distribution of theta is correct (@test-mcmc.R#132) 
      7. Error: Conditional distribution of eta is correct (@test-mcmc.R#151) 
      8. Error: Conditional distribution of chi is correct (@test-mcmc.R#169) 
      9. Error: Conditional distribution of pi is correct (@test-mcmc.R#188) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Warning in has_utility("convert", "ImageMagick") :
      ImageMagick not installed or not in PATH
    Sampling for 100 cells and 40 genes
    Quitting from lines 80-82 (introduction_to_mfa.Rmd) 
    Error: processing vignette 'introduction_to_mfa.Rmd' failed with diagnostics:
    there is no package called 'quantreg'
    Execution halted
    ```

# mglR

Version: 0.1.0

## In both

*   R CMD check timed out
    

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘grasp2db’
    ```

# miceFast

Version: 0.2.3

## In both

*   checking whether package ‘miceFast’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/miceFast/new/miceFast.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘miceFast’ ...
** package ‘miceFast’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/miceFast/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/miceFast/RcppArmadillo/include" -I/usr/local/include  -fopenmp -fPIC  -Wall -g -O2 -c R_funs.cpp -o R_funs.o
clang: error: unsupported option '-fopenmp'
make: *** [R_funs.o] Error 1
ERROR: compilation failed for package ‘miceFast’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/miceFast/new/miceFast.Rcheck/miceFast’

```
### CRAN

```
* installing *source* package ‘miceFast’ ...
** package ‘miceFast’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/miceFast/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/miceFast/RcppArmadillo/include" -I/usr/local/include  -fopenmp -fPIC  -Wall -g -O2 -c R_funs.cpp -o R_funs.o
clang: error: unsupported option '-fopenmp'
make: *** [R_funs.o] Error 1
ERROR: compilation failed for package ‘miceFast’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/miceFast/old/miceFast.Rcheck/miceFast’

```
# mixOmics

Version: 6.3.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.8Mb
      sub-directories of 1Mb or more:
        R      1.2Mb
        data   4.0Mb
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

# mlbgameday

Version: 0.1.2

## In both

*   R CMD check timed out
    

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘doParallel’ ‘iterators’ ‘parallel’
      All declared Imports should be used.
    ```

# mleap

Version: 0.1.2

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘rJava’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# MLZ

Version: 0.1.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 15.4Mb
      sub-directories of 1Mb or more:
        libs  15.0Mb
    ```

# modelr

Version: 0.1.2

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘lme4’, ‘rstanarm’
    ```

# momentuHMM

Version: 1.4.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        data   2.1Mb
        doc    1.7Mb
    ```

# MonetDBLite

Version: 0.5.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.0Mb
      sub-directories of 1Mb or more:
        libs   7.6Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# monkeylearn

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ratelimitr’
      All declared Imports should be used.
    ```

# morse

Version: 3.0.0

## In both

*   checking whether package ‘morse’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/morse/new/morse.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘morse’ ...
** package ‘morse’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error : .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/morse/rjags/libs/rjags.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/morse/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/morse/rjags/libs/rjags.so
  Reason: image not found
ERROR: lazy loading failed for package ‘morse’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/morse/new/morse.Rcheck/morse’

```
### CRAN

```
* installing *source* package ‘morse’ ...
** package ‘morse’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error : .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/morse/rjags/libs/rjags.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/morse/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/morse/rjags/libs/rjags.so
  Reason: image not found
ERROR: lazy loading failed for package ‘morse’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/morse/old/morse.Rcheck/morse’

```
# mosaic

Version: 1.2.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘manipulate’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘cubature’
    ```

# mosaicData

Version: 0.16.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 7 marked UTF-8 strings
    ```

# mosaicModel

Version: 0.3.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘MASS’ ‘caret’ ‘ggformula’ ‘knitr’ ‘testthat’ ‘tidyverse’
      All declared Imports should be used.
    ```

# MSnID

Version: 1.14.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    .read_mzIDs.mzR.engine.single.file: no visible binding for global
      variable ‘DatabaseAccess’
    .read_mzIDs.mzR.engine.single.file: no visible binding for global
      variable ‘DatabaseDescription’
    .read_mzIDs.mzR.engine.single.file: no visible binding for global
      variable ‘DBseqLength’
    infer_parsimonious_accessions,MSnID : infer_acc: no visible binding for
      global variable ‘accession’
    infer_parsimonious_accessions,MSnID : infer_acc: no visible binding for
      global variable ‘N’
    infer_parsimonious_accessions,MSnID : infer_acc: no visible binding for
      global variable ‘pepSeq’
    recalibrate,MSnID: no visible global function definition for ‘median’
    recalibrate,MSnID: no visible global function definition for ‘density’
    Undefined global functions or variables:
      DBseqLength DatabaseAccess DatabaseDescription N accession density i
      location mass median modification name optim pepSeq quantile rnorm
      spectrumID
    Consider adding
      importFrom("stats", "density", "median", "optim", "quantile", "rnorm")
    to your NAMESPACE file.
    ```

# MSstatsQC

Version: 1.2.0

## In both

*   checking whether package ‘MSstatsQC’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/MSstatsQC/new/MSstatsQC.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘RforProteomics’
    ```

## Installation

### Devel

```
* installing *source* package ‘MSstatsQC’ ...
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘ff’
ERROR: lazy loading failed for package ‘MSstatsQC’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/MSstatsQC/new/MSstatsQC.Rcheck/MSstatsQC’

```
### CRAN

```
* installing *source* package ‘MSstatsQC’ ...
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘ff’
ERROR: lazy loading failed for package ‘MSstatsQC’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/MSstatsQC/old/MSstatsQC.Rcheck/MSstatsQC’

```
# MSstatsQCgui

Version: 1.0.0

## In both

*   checking whether package ‘MSstatsQCgui’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/MSstatsQCgui/new/MSstatsQCgui.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘MSstatsQCgui’ ...
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘ff’
ERROR: lazy loading failed for package ‘MSstatsQCgui’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/MSstatsQCgui/new/MSstatsQCgui.Rcheck/MSstatsQCgui’

```
### CRAN

```
* installing *source* package ‘MSstatsQCgui’ ...
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘ff’
ERROR: lazy loading failed for package ‘MSstatsQCgui’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/MSstatsQCgui/old/MSstatsQCgui.Rcheck/MSstatsQCgui’

```
# MultiAssayExperiment

Version: 1.6.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Unexported objects imported by ':::' calls:
      ‘BiocGenerics:::replaceSlots’ ‘S4Vectors:::selectSome’
      See the note in ?`:::` about the use of this operator.
    ```

# multiROC

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘boot’ ‘ggplot2’
      All declared Imports should be used.
    ```

# MXM

Version: 1.3.4

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘quantreg’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# myTAI

Version: 0.8.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘biomartr’
      All declared Imports should be used.
    ```

# naniar

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘purrrlyr’
      All declared Imports should be used.
    ```

# ndexr

Version: 1.2.0

## In both

*   R CMD check timed out
    

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
    ndex_rest_GET: no visible global function definition for
      ‘packageVersion’
    ndex_rest_POST: no visible global function definition for
      ‘packageVersion’
    ndex_rest_PUT: no visible global function definition for
      ‘packageVersion’
    rcx_fromRCXgraph: no visible binding for global variable ‘po’
    rcx_toRCXgraph: no visible global function definition for ‘V’
    rcx_toRCXgraph: no visible global function definition for ‘V<-’
    rcx_toRCXgraph: no visible global function definition for ‘E’
    rcx_toRCXgraph: no visible global function definition for ‘E<-’
    rcxgraph_fromRCX: no visible global function definition for ‘V’
    rcxgraph_fromRCX: no visible global function definition for ‘V<-’
    rcxgraph_fromRCX: no visible global function definition for ‘E’
    rcxgraph_fromRCX: no visible global function definition for ‘E<-’
    rcxgraph_toRCX: no visible binding for global variable ‘po’
    Undefined global functions or variables:
      E E<- V V<- packageVersion po tail
    Consider adding
      importFrom("utils", "packageVersion", "tail")
    to your NAMESPACE file.
    ```

# nesRdata

Version: 0.1.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 5 marked UTF-8 strings
    ```

# neuropsychology

Version: 0.5.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘htmlTable’ ‘lme4’ ‘stringi’
      All declared Imports should be used.
    ```

# NFP

Version: 0.99.2

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘NFPdata’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.0Mb
      sub-directories of 1Mb or more:
        data   7.5Mb
    ```

# nitrcbot

Version: 1.0

## In both

*   R CMD check timed out
    

# nlmixr

Version: 0.9.1-2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dparser’ ‘inline’ ‘mvtnorm’ ‘vpc’
      All declared Imports should be used.
    ```

# noaastormevents

Version: 0.1.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘hurricaneexposuredata’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘RColorBrewer’ ‘XML’ ‘choroplethr’ ‘choroplethrMaps’ ‘data.table’
      ‘forcats’ ‘hurricaneexposure’ ‘plyr’
      All declared Imports should be used.
    ```

# NOAAWeather

Version: 0.1.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        data   4.6Mb
    ```

# nos

Version: 1.1.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘bipartite’
    ```

# nucleR

Version: 2.12.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error: processing vignette 'nucleR.Rmd' failed with diagnostics:
    missing value where TRUE/FALSE needed
    Execution halted
    ```

# nullabor

Version: 0.3.1

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    reg_dist: no visible global function definition for ‘lm’
    resid_boot: no visible global function definition for ‘resid’
    resid_pboot: no visible global function definition for ‘rnorm’
    resid_rotate: no visible global function definition for ‘rnorm’
    resid_rotate: no visible global function definition for ‘update’
    resid_rotate: no visible global function definition for ‘resid’
    resid_sigma: no visible global function definition for ‘rnorm’
    rorschach: no visible global function definition for ‘rbinom’
    rss: no visible global function definition for ‘resid’
    sep_dist: no visible global function definition for ‘dist’
    sep_dist: no visible global function definition for ‘cutree’
    sep_dist: no visible global function definition for ‘hclust’
    uni_dist: no visible global function definition for ‘sd’
    Undefined global functions or variables:
      coef cutree dist filter hclust lm predict quantile rbinom resid rnorm
      sd update
    Consider adding
      importFrom("stats", "coef", "cutree", "dist", "filter", "hclust", "lm",
                 "predict", "quantile", "rbinom", "resid", "rnorm", "sd",
                 "update")
    to your NAMESPACE file.
    ```

# nycflights13

Version: 0.2.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.0Mb
      sub-directories of 1Mb or more:
        data   6.9Mb
    ```

# nyctaxi

Version: 0.0.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
         
         taxi <- etl("nyctaxi", dir = "~/Desktop/nyctaxi/")
         taxi %>% 
            etl_extract(years = 2016, months = 1:2, types = c("yellow","green")) %>% 
            etl_transform(years = 2016, months = 1:2, types = c("yellow","green")) %>% 
            etl_load(years = 2016, months = 1:2, types = c("yellow","green")) 
         ## End(Not run)
         
    
    
    Attaching package: 'lubridate'
    
    The following object is masked from 'package:base':
    
        date
    
    pandoc: Could not fetch /Users/lionel/Desktop/rlang/revdep/library.noindex/nyctaxi/leaflet/htmlwidgets/lib/leaflet/#default#VML
    /Users/lionel/Desktop/rlang/revdep/library.noindex/nyctaxi/leaflet/htmlwidgets/lib/leaflet/: openBinaryFile: inappropriate type (is a directory)
    Error: processing vignette 'nyc_taxi.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 67
    Execution halted
    ```

# nzelect

Version: 0.4.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 6409 marked UTF-8 strings
    ```

# observer

Version: 0.1.2

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘ensurer’, ‘validate’
    ```

# oec

Version: 2.7.8

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘readr’
      All declared Imports should be used.
    ```

# OncoSimulR

Version: 2.10.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.6Mb
      sub-directories of 1Mb or more:
        doc   5.4Mb
    ```

# opendotaR

Version: 0.1.4

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# openPrimeR

Version: 1.2.0

## In both

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .travis.yml
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 12.7Mb
      sub-directories of 1Mb or more:
        R         1.3Mb
        extdata  10.2Mb
    ```

# Organism.dplyr

Version: 1.8.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      1: suppressPackageStartupMessages({
             library(TxDb.Hsapiens.UCSC.hg38.knownGene)
         }) at testthat/test-src_organism-select.R:3
      2: withCallingHandlers(expr, packageStartupMessage = function(c) invokeRestart("muffleMessage"))
      3: library(TxDb.Hsapiens.UCSC.hg38.knownGene) at testthat/test-src_organism-select.R:4
      4: stop(txt, domain = NA)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 32 SKIPPED: 0 FAILED: 3
      1. Error: (unknown) (@test-GenomicFeatures-extractors.R#3) 
      2. Error: mouse (@test-src_organism-class.R#54) 
      3. Error: (unknown) (@test-src_organism-select.R#3) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘org.Hs.eg.db’ ‘TxDb.Hsapiens.UCSC.hg38.knownGene’ ‘org.Mm.eg.db’
      ‘TxDb.Mmusculus.UCSC.mm10.ensGene’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported objects imported by ':::' calls:
      ‘AnnotationDbi:::smartKeys’ ‘GenomicFeatures:::.exons_with_3utr’
      ‘GenomicFeatures:::.exons_with_5utr’
      ‘GenomicFeatures:::get_TxDb_seqinfo0’
      ‘S4Vectors:::extract_data_frame_rows’
      See the note in ?`:::` about the use of this operator.
    ```

*   checking R code for possible problems ... NOTE
    ```
    .toGRanges: no visible binding for global variable ‘.’
    intronsByTranscript,src_organism: no visible binding for global
      variable ‘.’
    orgPackageName,src_organism: no visible binding for global variable
      ‘name’
    orgPackageName,src_organism: no visible binding for global variable
      ‘organism’
    orgPackageName,src_organism: no visible binding for global variable
      ‘OrgDb’
    Undefined global functions or variables:
      . OrgDb name organism
    ```

# PakPC2017

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘stats’
      All declared Imports should be used.
    ```

# parlitools

Version: 0.2.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Downloading constituency data
    Warning: Column `party_id` joining character vector and factor, coercing into character vector
    Connecting to API
    Retrieving page 1 of 2
    Retrieving page 2 of 2
    Joining, by = c("pano", "ons_const_id", "constituency_name", "country", "region", "constituency_type")
    `geom_smooth()` using method = 'loess'
    Warning: Removed 60 rows containing non-finite values (stat_smooth).
    Warning: Removed 60 rows containing missing values (geom_point).
    pandoc: Could not fetch /Users/lionel/Desktop/rlang/revdep/library.noindex/parlitools/leaflet/htmlwidgets/lib/leaflet/#default#VML
    /Users/lionel/Desktop/rlang/revdep/library.noindex/parlitools/leaflet/htmlwidgets/lib/leaflet/: openBinaryFile: inappropriate type (is a directory)
    Error: processing vignette 'introduction.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 67
    Execution halted
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 13 marked UTF-8 strings
    ```

# parsemsf

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dbplyr’
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

# patentsview

Version: 0.2.1

## In both

*   R CMD check timed out
    

# patternplot

Version: 0.2

## In both

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# pcaExplorer

Version: 2.6.0

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘GO.db’
    
    Packages suggested but not available for checking: ‘airway’ ‘org.Hs.eg.db’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# pcr

Version: 1.1.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Latexmk: applying rule 'pdflatex'...
    This is pdfTeX, Version 3.14159265-2.6-1.40.17 (TeX Live 2016) (preloaded format=pdflatex)
     restricted \write18 enabled.
    entering extended mode
    Latexmk: Missing input file: 'qpcr_analysis_files/figure-latex/plot_testing data-1' from line
      '! LaTeX Error: File `qpcr_analysis_files/figure-latex/plot_testing data-1' not found.'
    Latexmk: List of undefined refs and citations:
      Reference `fig:fig5' on page 15 undefined on input line 1175
    Latexmk: Summary of warnings:
      Latex failed to resolve 1 reference(s)
    Collected error summary (may duplicate other messages):
      pdflatex: Command for 'pdflatex' gave return code 256
    Latexmk: Use the -f option to force complete processing,
     unless error was exceeding maximum runs of latex/pdflatex.
    Latexmk: Errors, so I did not complete making targets
    ! LaTeX Error: File `qpcr_analysis_files/figure-latex/plot_testing data-1' not 
    found.
    
    Error: processing vignette 'qpcr_analysis.Rmd' failed with diagnostics:
    Failed to compile qpcr_analysis.tex. See qpcr_analysis.log for more info.
    Execution halted
    ```

# PCRedux

Version: 0.2.6

## In both

*   checking whether package ‘PCRedux’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/PCRedux/new/PCRedux.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘PCRedux’ ...
** package ‘PCRedux’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘quantreg’
ERROR: lazy loading failed for package ‘PCRedux’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/PCRedux/new/PCRedux.Rcheck/PCRedux’

```
### CRAN

```
* installing *source* package ‘PCRedux’ ...
** package ‘PCRedux’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘quantreg’
ERROR: lazy loading failed for package ‘PCRedux’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/PCRedux/old/PCRedux.Rcheck/PCRedux’

```
# petro.One

Version: 0.1.3

## In both

*   checking whether package ‘petro.One’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/petro.One/new/petro.One.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘petro.One’ ...
** package ‘petro.One’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘rJava’
ERROR: lazy loading failed for package ‘petro.One’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/petro.One/new/petro.One.Rcheck/petro.One’

```
### CRAN

```
* installing *source* package ‘petro.One’ ...
** package ‘petro.One’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘rJava’
ERROR: lazy loading failed for package ‘petro.One’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/petro.One/old/petro.One.Rcheck/petro.One’

```
# philr

Version: 1.6.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    name.balance: no visible global function definition for ‘as’
    vote.annotation: no visible global function definition for ‘is’
    Undefined global functions or variables:
      as is
    Consider adding
      importFrom("methods", "as", "is")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘compositions’
    ```

# Pi

Version: 1.8.0

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘XGR’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# pitchRx

Version: 1.8.2

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘ggsubplot’
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

# pivottabler

Version: 0.4.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        data   2.0Mb
        doc    2.4Mb
    ```

# pixiedust

Version: 0.8.3

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘Hmisc’
    ```

# pkgdown

Version: 1.0.0

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      31: tryCatchList(expr, classes, parentenv, NULL)
      32: tryCatch(withCallingHandlers({    eval(code, test_env)    if (!handled && !is.null(test)) {        skip_empty()    }}, expectation = handle_expectation, skip = handle_skip, warning = handle_warning,     message = handle_message, error = handle_error), error = handle_fatal,     skip = function(e) {    })
      33: test_code(NULL, exprs, env)
      34: source_file(path, new.env(parent = env), chdir = TRUE, wrap = wrap)
      35: force(code)
      36: with_reporter(reporter = reporter, start_end_reporter = start_end_reporter,     {        lister$start_file(basename(path))        source_file(path, new.env(parent = env), chdir = TRUE,             wrap = wrap)        end_context()    })
      37: FUN(X[[i]], ...)
      38: lapply(paths, test_file, env = env, reporter = current_reporter,     start_end_reporter = FALSE, load_helpers = FALSE, wrap = wrap)
      39: force(code)
      40: with_reporter(reporter = current_reporter, results <- lapply(paths,     test_file, env = env, reporter = current_reporter, start_end_reporter = FALSE,     load_helpers = FALSE, wrap = wrap))
      41: test_files(paths, reporter = reporter, env = env, stop_on_failure = stop_on_failure,     stop_on_warning = stop_on_warning, wrap = wrap)
      42: test_dir(path = test_path, reporter = reporter, env = env, filter = filter,     ..., stop_on_failure = stop_on_failure, stop_on_warning = stop_on_warning,     wrap = wrap)
      43: test_package_dir(package = package, test_path = test_path, filter = filter,     reporter = reporter, ..., stop_on_failure = stop_on_failure,     stop_on_warning = stop_on_warning, wrap = wrap)
      44: test_check("pkgdown")
      An irrecoverable exception occurred. R is aborting now ...
    ```

# PKPDmisc

Version: 2.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘purrr’
      All declared Imports should be used.
    ```

# plotly

Version: 4.7.1

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘RSelenium’
    ```

# plotrr

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘stats’
      All declared Imports should be used.
    ```

# pmc

Version: 1.0.3

## In both

*   R CMD check timed out
    

# PogromcyDanych

Version: 1.5

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.1Mb
      sub-directories of 1Mb or more:
        data   6.0Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 7256 marked UTF-8 strings
    ```

# poio

Version: 0.0-3

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘ISOcodes’
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 8 marked UTF-8 strings
    ```

# PopED

Version: 0.3.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Missing or unexported object: ‘GA::gaMonitor2’
    ```

# poppr

Version: 2.8.0

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
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include  -fopenmp -fPIC  -Wall  -pedantic -O0 -c adjust_missing.c -o adjust_missing.o
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
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include  -fopenmp -fPIC  -Wall  -pedantic -O0 -c adjust_missing.c -o adjust_missing.o
clang: error: unsupported option '-fopenmp'
make: *** [adjust_missing.o] Error 1
ERROR: compilation failed for package ‘poppr’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/poppr/old/poppr.Rcheck/poppr’

```
# PPforest

Version: 0.1.0

## In both

*   checking examples ... ERROR
    ```
    ...
    Warning in PPclassify2(Tree.result = x[[1]], test.data = xnew, Rule = 1) :
      restarting interrupted promise evaluation
    Warning in PPclassify2(Tree.result = x[[1]], test.data = xnew, Rule = 1) :
      restarting interrupted promise evaluation
    Warning in PPclassify2(Tree.result = x[[1]], test.data = xnew, Rule = 1) :
      restarting interrupted promise evaluation
    Warning in PPclassify2(Tree.result = x[[1]], test.data = xnew, Rule = 1) :
      restarting interrupted promise evaluation
    Warning in PPclassify2(Tree.result = x[[1]], test.data = xnew, Rule = 1) :
      restarting interrupted promise evaluation
    Warning in PPclassify2(Tree.result = x[[1]], test.data = xnew, Rule = 1) :
      restarting interrupted promise evaluation
    Warning in PPclassify2(Tree.result = x[[1]], test.data = xnew, Rule = 1) :
      restarting interrupted promise evaluation
    Warning in PPclassify2(Tree.result = x[[1]], test.data = xnew, Rule = 1) :
      restarting interrupted promise evaluation
    Warning in PPclassify2(Tree.result = x[[1]], test.data = xnew, Rule = 1) :
      restarting interrupted promise evaluation
    Error in do.ply(i) : task 1 failed - "object 'Type' not found"
    Calls: PPforest ... trees_pred -> <Anonymous> -> llply -> <Anonymous> -> <Anonymous>
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
        nasa
    
    Loading required package: gridExtra
    
    Attaching package: 'gridExtra'
    
    The following object is masked from 'package:dplyr':
    
        combine
    
    Loading required package: PPtreeViz
    Loading required package: partykit
    Loading required package: grid
    Loading required package: libcoin
    Loading required package: mvtnorm
    Loading required package: rpart
    Quitting from lines 155-160 (PPforest-vignette.Rmd) 
    Error: processing vignette 'PPforest-vignette.Rmd' failed with diagnostics:
    object 'Type' not found
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

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DiagrammeRsvg’ ‘RColorBrewer’ ‘tidyr’
      All declared Imports should be used.
    ```

# progeny

Version: 1.2.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      The vignette engine knitr::rmarkdown is not available, because the rmarkdown package is not installed. Please install it.
    Quitting from lines 42-60 (progeny.Rmd) 
    Error: processing vignette 'progeny.Rmd' failed with diagnostics:
    there is no package called 'airway'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘airway’
    ```

# promises

Version: 1.0.1

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘purrr’
    ```

# proteoQC

Version: 1.16.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error: processing vignette 'proteoQC.Rmd' failed with diagnostics:
    there is no package called ‘prettydoc’
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘RforProteomics’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  7.3Mb
      sub-directories of 1Mb or more:
        doc       2.5Mb
        extdata   3.9Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    plotMS2boxplot: no visible binding for global variable ‘techRep’
    plotMS2boxplot: no visible binding for global variable ‘fraction’
    plotMS2boxplot: no visible binding for global variable ‘MS2QC’
    plotSampleIDResultErrorBar: no visible binding for global variable
      ‘fraction’
    plotSampleIDResultErrorBar: no visible binding for global variable
      ‘val’
    plotSampleIDResultErrorBar: no visible binding for global variable ‘se’
    plotSampleVenn: no visible global function definition for ‘grid.draw’
    plotTechRepVenn : <anonymous>: no visible global function definition
      for ‘grid.draw’
    qcHist: no visible binding for global variable ‘error’
    qcHist: no visible binding for global variable ‘techRep’
    qcHist: no visible binding for global variable ‘bioRep’
    qcHist2: no visible binding for global variable ‘error’
    qcHist2: no visible binding for global variable ‘fractile’
    Undefined global functions or variables:
      ..count.. Intensity MS1QC MS2QC TMT10 TMT6 Tag V1 V2 V3 V4 V5 bioRep
      curenv delta error exprs fractile fraction grid.draw iTRAQ4 iTRAQ8
      label peplength peptide_summary precursorCharge quantify ratio
      readMgfData se techRep val x y
    ```

# proustr

Version: 0.2.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 20105 marked UTF-8 strings
    ```

# psichomics

Version: 1.6.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.6Mb
      sub-directories of 1Mb or more:
        doc   5.6Mb
    ```

*   checking compiled code ... NOTE
    ```
    File ‘psichomics/libs/psichomics.so’:
      Found ‘___stdoutp’, possibly from ‘stdout’ (C)
        Object: ‘psiFastCalc.o’
      Found ‘_printf’, possibly from ‘printf’ (C)
        Object: ‘psiFastCalc.o’
    
    Compiled code should not call entry points which might terminate R nor
    write to stdout/stderr instead of to the console, nor use Fortran I/O
    nor system RNGs.
    
    See ‘Writing portable packages’ in the ‘Writing R Extensions’ manual.
    ```

# PSLM2015

Version: 0.2.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 26 marked Latin-1 strings
    ```

# ptstem

Version: 0.0.3

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        dict   5.1Mb
    ```

# purrr

Version: 0.2.4

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      The following object is masked from 'package:testthat':
      
          is_null
      
      > 
      > test_check("purrr")
      ── 1. Failure: can flatten to a data frame with named lists (@test-flatten.R#77)
      `flatten_dfc(list(1))` did not throw an error.
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 393 SKIPPED: 0 FAILED: 1
      1. Failure: can flatten to a data frame with named lists (@test-flatten.R#77) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# qdap

Version: 2.3.0

## In both

*   checking whether package ‘qdap’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/qdap/new/qdap.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘qdap’ ...
** package ‘qdap’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘rJava’
ERROR: lazy loading failed for package ‘qdap’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/qdap/new/qdap.Rcheck/qdap’

```
### CRAN

```
* installing *source* package ‘qdap’ ...
** package ‘qdap’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘rJava’
ERROR: lazy loading failed for package ‘qdap’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/qdap/old/qdap.Rcheck/qdap’

```
# qqplotr

Version: 0.0.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘knitr’
      All declared Imports should be used.
    ```

# quadmesh

Version: 0.1.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘rgl’
    ```

# quanteda

Version: 1.2.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘wordcloud’
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 71 marked UTF-8 strings
    ```

# QuaternaryProd

Version: 1.14.0

## In both

*   R CMD check timed out
    

*   checking installed package size ... NOTE
    ```
      installed size is 17.0Mb
      sub-directories of 1Mb or more:
        extdata  16.2Mb
    ```

# questionr

Version: 0.6.2

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘Hmisc’
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4145 marked UTF-8 strings
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

# quokar

Version: 0.1.0

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘quantreg’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# r2glmm

Version: 0.1.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘data.table’ ‘dplyr’ ‘lmerTest’
      All declared Imports should be used.
    ```

# radiant.model

Version: 0.8.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2 marked UTF-8 strings
    ```

# radiant.multivariate

Version: 0.8.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

# railtrails

Version: 0.0.3

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc: Could not fetch http://www.r-pkg.org/badges/version/railtrails
    TlsException (HandshakeFailed (Error_Protocol ("expecting server hello, got alert : [(AlertLevel_Fatal,HandshakeFailure)]",True,HandshakeFailure)))
    Error: processing vignette 'intro_to_railtrails.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 67
    Execution halted
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1583 marked UTF-8 strings
    ```

# randomForestExplainer

Version: 0.9

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘MASS’ ‘dtplyr’
      All declared Imports should be used.
    ```

# raptr

Version: 0.1.1

## In both

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking: ‘gurobi’ ‘rgurobi’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.1Mb
      sub-directories of 1Mb or more:
        data   3.6Mb
        doc    1.4Mb
    ```

# rattle

Version: 5.1.0

## In both

*   checking package dependencies ... ERROR
    ```
    Packages required but not available: ‘RGtk2’ ‘cairoDevice’
    
    Packages suggested but not available for checking:
      ‘gWidgetsRGtk2’ ‘odfWeave’ ‘playwith’ ‘rggobi’ ‘RGtk2Extras’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# RBesT

Version: 1.3-3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘lme4’
      All declared Imports should be used.
    ```

# rccmisc

Version: 0.3.7

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# rclimateca

Version: 1.0.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
        |======================================================================| 100%
      
        |                                                                            
        |                                                                      |   0%
        |                                                                            
        |===================================                                   |  50%
        |                                                                            
        |======================================================================| 100%
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 187 SKIPPED: 0 FAILED: 2
      1. Failure: column types for ec_climate_data() are correct (@test-climate_data.R#123) 
      2. Failure: get mudata function for climate data works (@test-climate_data.R#463) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 24 marked UTF-8 strings
    ```

# rcv

Version: 0.2.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 6543 marked UTF-8 strings
    ```

# RCzechia

Version: 1.2.4

## In both

*   R CMD check timed out
    

# RDML

Version: 0.9-9

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      The vignette engine knitr::rmarkdown is not available, because the rmarkdown package is not installed. Please install it.
    Quitting from lines 42-47 (CreateRDML.Rmd) 
    Error: processing vignette 'CreateRDML.Rmd' failed with diagnostics:
    package or namespace load failed for 'chipPCR' in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
     there is no package called 'quantreg'
    Execution halted
    ```

# rdomains

Version: 0.1.7

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘RSelenium’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# rdrop2

Version: 0.8.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘digest’
      All declared Imports should be used.
    ```

# readat

Version: 1.6.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    sfread: no visible binding for global variable ‘header’
    sfread: no visible binding for global variable ‘nrows’
    Undefined global functions or variables:
      header nrows
    ```

# redcapAPI

Version: 2.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘DBI’
      All declared Imports should be used.
    ```

# refuge

Version: 0.1.1

## In both

*   R CMD check timed out
    

# reinforcelearn

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘R6’ ‘nnet’ ‘purrr’
      All declared Imports should be used.
    ```

# replyr

Version: 0.9.5

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘rquery’
    ```

# repurrrsive

Version: 0.1.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 8 marked UTF-8 strings
    ```

# rerddap

Version: 0.4.2

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘taxize’
    ```

# rfbCNPJ

Version: 0.1.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 27 marked UTF-8 strings
    ```

# rfishbase

Version: 2.1.2

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 44 marked UTF-8 strings
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

# RImmPort

Version: 1.8.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        extdata   3.8Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    buildNewSqliteDb: no visible global function definition for
      ‘dbListTables’
    Undefined global functions or variables:
      dbListTables
    ```

# rmapzen

Version: 0.3.5

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 31 marked UTF-8 strings
    ```

# rmcfs

Version: 1.2.11

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘rJava’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# RMCriteria

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# rmsfuns

Version: 0.0.0.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Missing or unexported object: ‘base::shell.exec’
    ```

# Rnightlights

Version: 0.2.1

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘ff’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# rODE

Version: 0.99.6

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘data.table’
      All declared Imports should be used.
    ```

# rolypoly

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘matrixcalc’
      All declared Imports should be used.
    ```

# rpcdsearch

Version: 1.0

## In both

*   checking whether package ‘rpcdsearch’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/rpcdsearch/new/rpcdsearch.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘rpcdsearch’ ...
** package ‘rpcdsearch’ successfully unpacked and MD5 sums checked
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘rJava’
ERROR: lazy loading failed for package ‘rpcdsearch’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/rpcdsearch/new/rpcdsearch.Rcheck/rpcdsearch’

```
### CRAN

```
* installing *source* package ‘rpcdsearch’ ...
** package ‘rpcdsearch’ successfully unpacked and MD5 sums checked
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘rJava’
ERROR: lazy loading failed for package ‘rpcdsearch’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/rpcdsearch/old/rpcdsearch.Rcheck/rpcdsearch’

```
# rpivotTable

Version: 0.3.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘shiny’
    ```

# rPref

Version: 1.2

## In both

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# rprev

Version: 0.2.4

## In both

*   checking whether package ‘rprev’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/rprev/new/rprev.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘rprev’ ...
** package ‘rprev’ successfully unpacked and MD5 sums checked
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘quantreg’
ERROR: lazy loading failed for package ‘rprev’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/rprev/new/rprev.Rcheck/rprev’

```
### CRAN

```
* installing *source* package ‘rprev’ ...
** package ‘rprev’ successfully unpacked and MD5 sums checked
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘quantreg’
ERROR: lazy loading failed for package ‘rprev’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/rprev/old/rprev.Rcheck/rprev’

```
# Rraven

Version: 1.0.3

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘warbleR’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# rrr

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘Rcpp’
      All declared Imports should be used.
    ```

# rsinaica

Version: 0.5.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 467 marked UTF-8 strings
    ```

# rsoi

Version: 0.3.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘utils’
      All declared Imports should be used.
    ```

# RSwissMaps

Version: 0.1.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 18627 marked UTF-8 strings
    ```

# rtable

Version: 0.1.5

## In both

*   checking whether package ‘rtable’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/rtable/new/rtable.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘rtable’ ...
** package ‘rtable’ successfully unpacked and MD5 sums checked
** R
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘ReporteRsjars’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 there is no package called ‘rJava’
Error : package ‘ReporteRsjars’ could not be loaded
ERROR: lazy loading failed for package ‘rtable’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/rtable/new/rtable.Rcheck/rtable’

```
### CRAN

```
* installing *source* package ‘rtable’ ...
** package ‘rtable’ successfully unpacked and MD5 sums checked
** R
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘ReporteRsjars’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 there is no package called ‘rJava’
Error : package ‘ReporteRsjars’ could not be loaded
ERROR: lazy loading failed for package ‘rtable’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/rtable/old/rtable.Rcheck/rtable’

```
# rtimicropem

Version: 1.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘R6’
      All declared Imports should be used.
    ```

# rtrends

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# RtutoR

Version: 1.1

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
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘rJava’
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
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘rJava’
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

# RxODE

Version: 0.7.2-1

## In both

*   checking whether package ‘RxODE’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      ./lsoda_internal.h:21:113: warning: token pasting of ',' and __VA_ARGS__ is a GNU extension [-Wgnu-zero-variadic-macro-arguments]
      lsoda.c:107:13: warning: token pasting of ',' and __VA_ARGS__ is a GNU extension [-Wgnu-zero-variadic-macro-arguments]
      lsoda.c:119:13: warning: token pasting of ',' and __VA_ARGS__ is a GNU extension [-Wgnu-zero-variadic-macro-arguments]
      rxData.cpp:1587:32: warning: comparison of unsigned expression < 0 is always false [-Wtautological-compare]
      rxData.cpp:1592:27: warning: comparison of unsigned expression < 0 is always false [-Wtautological-compare]
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/RxODE/new/RxODE.Rcheck/00install.out’ for details.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘n1qn1’
      All declared Imports should be used.
    ```

# SanFranBeachWater

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tibble’
      All declared Imports should be used.
    ```

# SanzCircos

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘purrr’ ‘tidyr’
      All declared Imports should be used.
    ```

# scfind

Version: 1.2.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error: processing vignette 'scfind.Rmd' failed with diagnostics:
    there is no package called ‘BiocStyle’
    Execution halted
    ```

# scmap

Version: 1.2.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error: processing vignette 'scmap.Rmd' failed with diagnostics:
    there is no package called ‘BiocStyle’
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘Biobase’
      All declared Imports should be used.
    ```

# SEERaBomb

Version: 2018.1

## In both

*   checking whether package ‘SEERaBomb’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/SEERaBomb/new/SEERaBomb.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘SEERaBomb’ ...
** package ‘SEERaBomb’ successfully unpacked and MD5 sums checked
** libs
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/SEERaBomb/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c RcppExports.cpp -o RcppExports.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/SEERaBomb/Rcpp/include" -I/usr/local/include   -fPIC  -Wall  -pedantic -O0 -c SEERaBomb_init.c -o SEERaBomb_init.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/SEERaBomb/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c fillPYM.cpp -o fillPYM.o
clang++ -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o SEERaBomb.so RcppExports.o SEERaBomb_init.o fillPYM.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/lionel/Desktop/rlang/revdep/checks.noindex/SEERaBomb/new/SEERaBomb.Rcheck/SEERaBomb/libs
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘demography’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 there is no package called ‘quantreg’
Error : package ‘demography’ could not be loaded
ERROR: lazy loading failed for package ‘SEERaBomb’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/SEERaBomb/new/SEERaBomb.Rcheck/SEERaBomb’

```
### CRAN

```
* installing *source* package ‘SEERaBomb’ ...
** package ‘SEERaBomb’ successfully unpacked and MD5 sums checked
** libs
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/SEERaBomb/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c RcppExports.cpp -o RcppExports.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/SEERaBomb/Rcpp/include" -I/usr/local/include   -fPIC  -Wall  -pedantic -O0 -c SEERaBomb_init.c -o SEERaBomb_init.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/SEERaBomb/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c fillPYM.cpp -o fillPYM.o
clang++ -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o SEERaBomb.so RcppExports.o SEERaBomb_init.o fillPYM.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/lionel/Desktop/rlang/revdep/checks.noindex/SEERaBomb/old/SEERaBomb.Rcheck/SEERaBomb/libs
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘demography’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 there is no package called ‘quantreg’
Error : package ‘demography’ could not be loaded
ERROR: lazy loading failed for package ‘SEERaBomb’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/SEERaBomb/old/SEERaBomb.Rcheck/SEERaBomb’

```
# segclust2d

Version: 0.1.0

## In both

*   checking whether package ‘segclust2d’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/segclust2d/new/segclust2d.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘segclust2d’ ...
** package ‘segclust2d’ successfully unpacked and MD5 sums checked
** libs
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/segclust2d/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/segclust2d/RcppArmadillo/include" -I/usr/local/include  -fopenmp -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘segclust2d’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/segclust2d/new/segclust2d.Rcheck/segclust2d’

```
### CRAN

```
* installing *source* package ‘segclust2d’ ...
** package ‘segclust2d’ successfully unpacked and MD5 sums checked
** libs
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/segclust2d/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/segclust2d/RcppArmadillo/include" -I/usr/local/include  -fopenmp -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘segclust2d’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/segclust2d/old/segclust2d.Rcheck/segclust2d’

```
# sejmRP

Version: 1.3.4

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘cluster’ ‘factoextra’ ‘tidyr’
      All declared Imports should be used.
    ```

# SemNetCleaner

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘RColorBrewer’ ‘purrr’ ‘tm’
      All declared Imports should be used.
    ```

# seoR

Version: 0.1.0

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘seoR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: lastCached
    > ### Title: Function to get the last Google Cache Date for a URL
    > ### Aliases: lastCached
    > 
    > ### ** Examples
    > 
    > lastCached("https://www.r-project.org/")
    Error in if (is.na(res)) { : argument is of length zero
    Calls: lastCached
    Execution halted
    ```

# sergeant

Version: 0.5.2

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘rJava’
    ```

# sevenbridges

Version: 1.10.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.8Mb
      sub-directories of 1Mb or more:
        R     2.2Mb
        doc   2.9Mb
    ```

# sevenC

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Unexported object imported by a ':::' call: ‘GenomicRanges:::get_out_of_bound_index’
      See the note in ?`:::` about the use of this operator.
    ```

# sf

Version: 0.6-3

## In both

*   checking whether package ‘sf’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/sf/new/sf.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘sf’ ...
** package ‘sf’ successfully unpacked and MD5 sums checked
configure: CC: clang
configure: CXX: clang++
checking for gdal-config... no
no
configure: error: gdal-config not found or not executable.
ERROR: configuration failed for package ‘sf’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/sf/new/sf.Rcheck/sf’

```
### CRAN

```
* installing *source* package ‘sf’ ...
** package ‘sf’ successfully unpacked and MD5 sums checked
configure: CC: clang
configure: CXX: clang++
checking for gdal-config... no
no
configure: error: gdal-config not found or not executable.
ERROR: configuration failed for package ‘sf’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/sf/old/sf.Rcheck/sf’

```
# shazam

Version: 0.1.9

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2 marked UTF-8 strings
    ```

# shiny

Version: 1.1.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.4Mb
      sub-directories of 1Mb or more:
        R     1.1Mb
        www   6.4Mb
    ```

# shiny.semantic

Version: 0.2.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘utils’
      All declared Imports should be used.
    ```

# shinyAce

Version: 0.3.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.8Mb
      sub-directories of 1Mb or more:
        www   7.7Mb
    ```

# shinyaframe

Version: 1.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘shiny’
      All declared Imports should be used.
    ```

# shinyHeatmaply

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘RColorBrewer’ ‘htmlwidgets’ ‘jsonlite’ ‘viridis’
      All declared Imports should be used.
    ```

# SIBER

Version: 2.1.3

## In both

*   checking examples ... ERROR
    ```
    ...
    > ### ** Examples
    > 
    > x <- stats::rnorm(50)
    > y <- stats::rnorm(50)
    > parms <- list()
    > parms$n.iter <- 2 * 10^3
    > parms$n.burnin <- 500
    > parms$n.thin <- 2     
    > parms$n.chains <- 2    
    > priors <- list()
    > priors$R <- 1 * diag(2)
    > priors$k <- 2
    > priors$tau.mu <- 1.0E-3
    > fitEllipse(x, y, parms, priors)
    Error: .onLoad failed in loadNamespace() for 'rjags', details:
      call: dyn.load(file, DLLpath = DLLpath, ...)
      error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/SIBER/rjags/libs/rjags.so':
      dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/SIBER/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
      Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/SIBER/rjags/libs/rjags.so
      Reason: image not found
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      The vignette engine knitr::rmarkdown is not available, because the rmarkdown package is not installed. Please install it.
    Quitting from lines 74-93 (Centroid-Vectors.Rmd) 
    Error: processing vignette 'Centroid-Vectors.Rmd' failed with diagnostics:
    .onLoad failed in loadNamespace() for 'rjags', details:
      call: dyn.load(file, DLLpath = DLLpath, ...)
      error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/SIBER/rjags/libs/rjags.so':
      dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/SIBER/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
      Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/SIBER/rjags/libs/rjags.so
      Reason: image not found
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘ggplot2’ ‘viridis’
      All declared Imports should be used.
    ```

# sicegar

Version: 0.2.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# sidrar

Version: 0.2.4

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# SimDesign

Version: 1.10.1

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘doMPI’
    ```

# simmer

Version: 3.8.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.4Mb
      sub-directories of 1Mb or more:
        doc    1.8Mb
        libs   4.2Mb
    ```

# simputation

Version: 0.2.2

## In both

*   checking whether package ‘simputation’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/simputation/new/simputation.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘simputation’ ...
** package ‘simputation’ successfully unpacked and MD5 sums checked
** libs
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include  -fopenmp -fPIC  -Wall  -pedantic -O0 -c R_register_native.c -o R_register_native.o
clang: error: unsupported option '-fopenmp'
make: *** [R_register_native.o] Error 1
ERROR: compilation failed for package ‘simputation’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/simputation/new/simputation.Rcheck/simputation’

```
### CRAN

```
* installing *source* package ‘simputation’ ...
** package ‘simputation’ successfully unpacked and MD5 sums checked
** libs
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include  -fopenmp -fPIC  -Wall  -pedantic -O0 -c R_register_native.c -o R_register_native.o
clang: error: unsupported option '-fopenmp'
make: *** [R_register_native.o] Error 1
ERROR: compilation failed for package ‘simputation’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/simputation/old/simputation.Rcheck/simputation’

```
# SimRVPedigree

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# sjPlot

Version: 2.4.1

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘plm’
    ```

# skynet

Version: 1.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘maps’
      All declared Imports should be used.
    ```

# solrium

Version: 1.0.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘XML’
    ```

# sophisthse

Version: 0.7.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1320 marked UTF-8 strings
    ```

# sorvi

Version: 0.7.26

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    regression_plot: no visible global function definition for
      ‘colorRampPalette’
    regression_plot: no visible global function definition for
      ‘loess.control’
    regression_plot: no visible global function definition for ‘predict’
    regression_plot : <anonymous>: no visible global function definition
      for ‘quantile’
    regression_plot : <anonymous>: no visible global function definition
      for ‘pnorm’
    regression_plot: no visible global function definition for
      ‘flush.console’
    regression_plot: no visible global function definition for ‘density’
    Undefined global functions or variables:
      colorRampPalette density flush.console loess loess.control pnorm
      predict quantile read.csv
    Consider adding
      importFrom("grDevices", "colorRampPalette")
      importFrom("stats", "density", "loess", "loess.control", "pnorm",
                 "predict", "quantile")
      importFrom("utils", "flush.console", "read.csv")
    to your NAMESPACE file.
    ```

# sourceR

Version: 1.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘gtools’ ‘hashmap’ ‘reshape2’
      All declared Imports should be used.
    ```

# SpaDES.core

Version: 0.1.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      ── 2. Failure: simList object initializes correctly (@test-simList.R#17)  ──────────────────────────
      length(out) not equal to 75.
      1/1 mismatches
      [1] 73 - 75 == -2
      
        Using cached copy of burn event in fireSpread module
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 343 SKIPPED: 33 FAILED: 2
      1. Error: 3 levels of parent and child modules load and show correctly (@test-module-deps-methods.R#201) 
      2. Failure: simList object initializes correctly (@test-simList.R#17) 
      
      Error: testthat unit tests failed
      In addition: Warning message:
      In fun(libname, pkgname) : couldn't connect to display ""
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    The following object is masked from 'package:igraph':
    
        %>%
    
    Warning in fun(libname, pkgname) : couldn't connect to display ""
    Loading required package: RColorBrewer
    Loading required package: raster
    Loading required package: sp
    Loading required package: grid
    
    Attaching package: 'grid'
    
    The following object is masked from 'package:quickPlot':
    
        gpar
    
    Quitting from lines 433-454 (ii-modules.Rmd) 
    Error: processing vignette 'ii-modules.Rmd' failed with diagnostics:
    At optimal_modularity.c:85 : GLPK is not available, Unimplemented function call
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘sp’
      All declared Imports should be used.
    ```

# SpaDES.tools

Version: 0.1.1

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘ff’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# SpatialBall

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘lubridate’
      All declared Imports should be used.
    ```

# SpatialEpiApp

Version: 0.3

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘INLA’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘RColorBrewer’ ‘SpatialEpi’ ‘dplyr’ ‘dygraphs’ ‘ggplot2’
      ‘htmlwidgets’ ‘knitr’ ‘leaflet’ ‘mapproj’ ‘maptools’ ‘rgdal’ ‘rgeos’
      ‘rmarkdown’ ‘shinyjs’ ‘spdep’ ‘xts’
      All declared Imports should be used.
    ```

# spatialwarnings

Version: 1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘moments’ ‘poweRlaw’
      All declared Imports should be used.
    ```

# stacomiR

Version: 0.5.3.1

## In both

*   checking package dependencies ... ERROR
    ```
    Packages required but not available: ‘gWidgetsRGtk2’ ‘RGtk2’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# staRdom

Version: 1.0.5

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘readr’ ‘tools’
      All declared Imports should be used.
    ```

# starmie

Version: 0.1.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.0Mb
      sub-directories of 1Mb or more:
        doc       1.1Mb
        extdata   4.9Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘MCMCpack’
      All declared Imports should be used.
    ```

# states

Version: 0.2.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘lubridate’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 3 marked UTF-8 strings
    ```

# statesRcontiguous

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘magrittr’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 34 marked UTF-8 strings
    ```

# stationaRy

Version: 0.4.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.2Mb
    ```

# statip

Version: 0.1.5

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘distrEx’
    ```

# statsDK

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘ggplot2’ ‘stringr’
      All declared Imports should be used.
    ```

# stminsights

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘huge’ ‘scales’ ‘shinyjs’
      All declared Imports should be used.
    ```

# stplanr

Version: 0.2.4

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘tmap’
    ```

# subSeq

Version: 1.10.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    summary.subsamples: no visible binding for global variable ‘o.padj’
    summary.subsamples: no visible binding for global variable
      ‘significant’
    summary.subsamples: no visible binding for global variable ‘estFDP’
    summary.subsamples: no visible binding for global variable ‘rFDP’
    summary.subsamples: no visible binding for global variable ‘metric’
    summary.subsamples: no visible binding for global variable ‘value’
    summary.subsamples: no visible binding for global variable ‘percent’
    voomLimma: no visible global function definition for ‘model.matrix’
    Undefined global functions or variables:
      . ID average.depth average.value coefficient cor count cov depth
      estFDP method metric model.matrix o.coefficient o.lfdr o.padj
      p.adjust padj percent plot proportion pvalue rFDP rbinom replication
      selectMethod significant valid value var
    Consider adding
      importFrom("graphics", "plot")
      importFrom("methods", "selectMethod")
      importFrom("stats", "cor", "cov", "model.matrix", "p.adjust", "rbinom",
                 "var")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# sunburstR

Version: 2.0.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘treemap’
    ```

# survminer

Version: 0.4.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        doc   5.1Mb
    ```

# sweep

Version: 0.2.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘devtools’ ‘lazyeval’ ‘lubridate’ ‘tidyr’
      All declared Imports should be used.
    ```

# swfdr

Version: 1.6.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error: processing vignette 'swfdrTutorial.Rmd' failed with diagnostics:
    missing value where TRUE/FALSE needed
    Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    lm_pi0: no visible global function definition for ‘glm’
    lm_pi0: no visible binding for global variable ‘binomial’
    Undefined global functions or variables:
      binomial glm
    Consider adding
      importFrom("stats", "binomial", "glm")
    to your NAMESPACE file.
    ```

# switchde

Version: 1.6.0

## In both

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .travis.yml
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

# SWMPrExtension

Version: 0.3.14

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rgeos’
      All declared Imports should be used.
    ```

# synlet

Version: 1.10.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    siRNAPlot: no visible global function definition for ‘pdf’
    siRNAPlot: no visible global function definition for ‘dev.off’
    tTest: no visible global function definition for ‘p.adjust’
    zFactor: no visible binding for global variable ‘condition’
    zFactor: no visible binding for global variable ‘sd’
    zFactor: no visible binding for global variable ‘median’
    zFactor: no visible global function definition for ‘complete.cases’
    Undefined global functions or variables:
      COL_NAME EXPERIMENT_MODIFICATION EXPERIMENT_TYPE MASTER_PLATE PLATE
      READOUT ROW_NAME Var1 WELL_CONTENT_NAME colorRampPalette
      complete.cases condition dev.off experiments is mad median medpolish
      p.adjust pdf phyper rainbow sd siRNA t.test value write.table
    Consider adding
      importFrom("grDevices", "colorRampPalette", "dev.off", "pdf",
                 "rainbow")
      importFrom("methods", "is")
      importFrom("stats", "complete.cases", "mad", "median", "medpolish",
                 "p.adjust", "phyper", "sd", "t.test")
      importFrom("utils", "write.table")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# tabularaster

Version: 0.5.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘methods’
      All declared Imports should be used.
    ```

# tauturri

Version: 0.1.1

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘stringr’
    ```

# taxa

Version: 0.2.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘knitr’ ‘lazyeval’ ‘rlang’ ‘tidyr’
      All declared Imports should be used.
    ```

# tbl2xts

Version: 0.1.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘PerformanceAnalytics’
      All declared Imports should be used.
    ```

# TCGAbiolinks

Version: 2.8.0

## In both

*   checking examples ... ERROR
    ```
    ...
    Downloading: 550 kB     
    Downloading: 550 kB     
    Downloading: 550 kB     
    Downloading: 550 kB     
    Downloading: 560 kB     
    Downloading: 560 kB     
    Downloading: 560 kB     
    Downloading: 560 kB     
    Downloading: 560 kB     
    Downloading: 560 kB     Downloading chunk 2 of 2 (59 files, size = 2.401344 MB) as Wed_May_30_03_38_51_2018_1.tar.gz
    
      |                                                                            
      |                                                                      |   0%
      |                                                                            
      |======================================================================| 100%
    tar: Unrecognized archive format
    tar: Error exit delayed from previous errors.
    Download completed
    Error in if (ret == 1) break : argument is of length zero
    Calls: GDCdownload ... tryCatchList -> tryCatchOne -> <Anonymous> -> GDCdownload.by.chunk
    Execution halted
    ```

*   R CMD check timed out
    

*   checking installed package size ... NOTE
    ```
      installed size is 65.8Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        data   3.5Mb
        doc   60.6Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    TCGAanalyze_networkInference: no visible global function definition for
      ‘c3net’
    TCGAanalyze_networkInference: no visible global function definition for
      ‘minet’
    TCGAquery_recount2: no visible binding for global variable ‘rse_gene’
    TCGAtumor_purity: no visible binding for global variable ‘Tumor.purity’
    TCGAvisualize_SurvivalCoxNET: no visible global function definition for
      ‘dNetInduce’
    TCGAvisualize_SurvivalCoxNET: no visible global function definition for
      ‘dNetPipeline’
    TCGAvisualize_SurvivalCoxNET: no visible global function definition for
      ‘dCommSignif’
    TCGAvisualize_SurvivalCoxNET: no visible global function definition for
      ‘visNet’
    TCGAvisualize_oncoprint: no visible binding for global variable ‘value’
    readTranscriptomeProfiling: no visible binding for global variable
      ‘ignore.case’
    Undefined global functions or variables:
      TabSubtypesCol_merged Tumor.purity barcode c3net clinical dCommSignif
      dNetInduce dNetPipeline ignore.case knnmi.cross limmacontrasts.fit
      limmamakeContrasts minet portions rse_gene value visNet
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Error in .requirePackage(package) : 
        unable to find required package 'maftools'
      Calls: <Anonymous> ... getClass -> getClassDef -> .classEnv -> .requirePackage
      Execution halted
    ```

# TCGAbiolinksGUI.data

Version: 1.0.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 19.7Mb
      sub-directories of 1Mb or more:
        data  18.6Mb
        doc    1.0Mb
    ```

# tcR

Version: 2.2.1.11

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.8Mb
      sub-directories of 1Mb or more:
        data   1.2Mb
        doc    3.9Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘scales’
      All declared Imports should be used.
    ```

*   checking Rd \usage sections ... NOTE
    ```
    S3 methods shown with full name in documentation object 'top.fun':
      ‘slice.fun’
    
    The \usage entries for S3 methods should use the \method markup and not
    their full name.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

# teachingApps

Version: 1.0.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘data.table’ ‘datasets’ ‘stats’
      All declared Imports should be used.
    ```

# tempcyclesdata

Version: 1.0.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.9Mb
      sub-directories of 1Mb or more:
        data   5.9Mb
    ```

# temperatureresponse

Version: 0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘nlme’ ‘tidyr’
      All declared Imports should be used.
    ```

# textmining

Version: 0.0.1

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘rJava’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# textreuse

Version: 0.1.4

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘tm’
    ```

# TFEA.ChIP

Version: 1.0.0

## In both

*   checking package dependencies ... ERROR
    ```
    Packages required but not available:
      ‘TxDb.Hsapiens.UCSC.hg19.knownGene’ ‘org.Hs.eg.db’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# TFutils

Version: 1.0.0

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘TFutils-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: genemodelDF
    > ### Title: use EnsDb to generate an exon-level model of genes identified by
    > ###   symbol
    > ### Aliases: genemodelDF
    > 
    > ### ** Examples
    > 
    > if (requireNamespace("EnsDb.Hsapiens.v75")) {
    +  orm = genemodelDF("ORMDL3", EnsDb.Hsapiens.v75::EnsDb.Hsapiens.v75)
    +  dim(orm)
    + }
    Loading required namespace: EnsDb.Hsapiens.v75
    Failed with error:  'there is no package called 'EnsDb.Hsapiens.v75''
    > head(orm)
    Error in head(orm) : object 'orm' not found
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      4: getExportedValue(pkg, name)
      5: asNamespace(ns)
      6: getNamespace(ns)
      7: tryCatch(loadNamespace(name), error = function(e) stop(e))
      8: tryCatchList(expr, classes, parentenv, handlers)
      9: tryCatchOne(expr, names, parentenv, handlers[[1L]])
      10: value[[3L]](cond)
      
      Failed with error:  'there is no package called 'EnsDb.Hsapiens.v75''
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 3 SKIPPED: 0 FAILED: 1
      1. Error: grabTab returns expected records (@test.R#6) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 26-40 (TFutils.Rmd) 
    Error: processing vignette 'TFutils.Rmd' failed with diagnostics:
    there is no package called 'org.Hs.eg.db'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘Homo.sapiens’ ‘GO.db’ ‘org.Hs.eg.db’ ‘EnsDb.Hsapiens.v75’
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 62 marked UTF-8 strings
    ```

# TH.data

Version: 1.0-8

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        data   1.1Mb
        rda    3.8Mb
    ```

# theseus

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘gridExtra’ ‘splancs’ ‘tidyverse’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘DESeq2’, ‘dada2’
    ```

# thinkr

Version: 0.11

## In both

*   checking whether package ‘thinkr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/thinkr/new/thinkr.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘thinkr’ ...
** package ‘thinkr’ successfully unpacked and MD5 sums checked
** R
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘rJava’
ERROR: lazy loading failed for package ‘thinkr’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/thinkr/new/thinkr.Rcheck/thinkr’

```
### CRAN

```
* installing *source* package ‘thinkr’ ...
** package ‘thinkr’ successfully unpacked and MD5 sums checked
** R
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘rJava’
ERROR: lazy loading failed for package ‘thinkr’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/thinkr/old/thinkr.Rcheck/thinkr’

```
# tidygraph

Version: 1.1.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      14: mutate_as_tbl(.data, !!!dot)
      15: mutate(d_tmp, ...)
      16: mutate.tbl_df(d_tmp, ...)
      17: mutate_impl(.data, dots)
      18: group_optimal()
      19: membership(cluster_optimal(graph = .G(), weights = weights))
      20: cluster_optimal(graph = .G(), weights = weights)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 268 SKIPPED: 0 FAILED: 2
      1. Error: grouping returns integer vector (@test-group.R#14) 
      2. Error: grouping returns integer of correct length (@test-group.R#31) 
      
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
      13: reduce_data(.data, !!enquo(X), numvars = 1, na.rm = na.rm)
      14: tidyselect::vars_select(names(reduced_tab), !!!quos(...))
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

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘tibble’ ‘tidyverse’
      All declared Imports should be used.
    ```

# tidyr

Version: 0.8.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 23 marked UTF-8 strings
    ```

# tidyRSS

Version: 1.2.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘testthat’
      All declared Imports should be used.
    ```

# tidystringdist

Version: 0.1.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘magrittr’
      All declared Imports should be used.
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

Version: 1.0.1

## In both

*   checking compiled code ... WARNING
    ```
    File ‘tidyxl/libs/tidyxl.so’:
      Found ‘_abort’, possibly from ‘abort’ (C)
        Object: ‘xlex.o’
    
    Compiled code should not call entry points which might terminate R nor
    write to stdout/stderr instead of to the console, nor use Fortran I/O
    nor system RNGs.
    
    See ‘Writing portable packages’ in the ‘Writing R Extensions’ manual.
    ```

# tilegramsR

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘sp’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 341 marked UTF-8 strings
    ```

# timelineS

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘base’
      All declared Imports should be used.
    ```

# TimerQuant

Version: 1.10.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘rainbow’
    plotPrimordiumProfile: no visible binding for global variable ‘median’
    plotPrimordiumProfile: no visible binding for global variable ‘mad’
    plotPrimordiumProfile: no visible global function definition for ‘par’
    plotPrimordiumProfile: no visible global function definition for ‘plot’
    plotPrimordiumProfile: no visible global function definition for ‘axis’
    plotPrimordiumProfile: no visible global function definition for
      ‘points’
    plotPrimordiumProfile: no visible global function definition for
      ‘polygon’
    plotPrimordiumProfile: no visible global function definition for ‘rgb’
    simulatedRatio: no visible global function definition for ‘rnorm’
    Undefined global functions or variables:
      approxfun axis mad median optimize par plot points polygon predict
      rainbow rgb rnorm
    Consider adding
      importFrom("grDevices", "rainbow", "rgb")
      importFrom("graphics", "axis", "par", "plot", "points", "polygon")
      importFrom("stats", "approxfun", "mad", "median", "optimize",
                 "predict", "rnorm")
    to your NAMESPACE file.
    ```

# timetk

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘devtools’ ‘forecast’
      All declared Imports should be used.
    ```

# tiobeindexr

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘rvest’ ‘xml2’
      All declared Imports should be used.
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

# TitanCNA

Version: 1.18.0

## In both

*   checking whether package ‘TitanCNA’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘GenomicRanges::shift’ by ‘data.table::shift’ when loading ‘TitanCNA’
      Warning: replacing previous import ‘data.table::first’ by ‘dplyr::first’ when loading ‘TitanCNA’
      Warning: replacing previous import ‘IRanges::slice’ by ‘dplyr::slice’ when loading ‘TitanCNA’
      Warning: replacing previous import ‘data.table::between’ by ‘dplyr::between’ when loading ‘TitanCNA’
      Warning: replacing previous import ‘IRanges::collapse’ by ‘dplyr::collapse’ when loading ‘TitanCNA’
      Warning: replacing previous import ‘GenomeInfoDb::intersect’ by ‘dplyr::intersect’ when loading ‘TitanCNA’
      Warning: replacing previous import ‘data.table::last’ by ‘dplyr::last’ when loading ‘TitanCNA’
      Warning: replacing previous import ‘GenomicRanges::setdiff’ by ‘dplyr::setdiff’ when loading ‘TitanCNA’
      Warning: replacing previous import ‘GenomicRanges::union’ by ‘dplyr::union’ when loading ‘TitanCNA’
      Warning: replacing previous import ‘IRanges::desc’ by ‘dplyr::desc’ when loading ‘TitanCNA’
      Warning: replacing previous import ‘dplyr::select’ by ‘VariantAnnotation::select’ when loading ‘TitanCNA’
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/TitanCNA/new/TitanCNA.Rcheck/00install.out’ for details.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  7.5Mb
      sub-directories of 1Mb or more:
        data      1.7Mb
        extdata   4.9Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      HaplotypeDepth.mean.symmetric HaplotypeDepth.sum
      HaplotypeDepth.sum.symmetric HaplotypeFraction
      HaplotypeFraction.symmetric HaplotypeRatio HaplotypeRatio.1
      HaplotypeRatio.2 Length.snp. LogRatio MajorCN Median_Ratio
      Median_logR MinorCN SNPs Sample Start Start.snp Start.telo
      Start_Position.bp. TITAN_call TITANcall TITANstate abline approxfun
      as axis depth dunif excludeCentromere filterByTargetedSequences
      haplotypeBin head keepChr lines loess logR_Copy_Number lowess mtext
      na.omit nonRef par phaseSet phaseSet.aggr phasedAlleleFraction
      phasedCount phasedCount.haploSymmetric plot points predict queryHits
      read.delim rowRanges rowRanges<- seq.info subjectHits tail tumDepth
      uniroot unstrsplit write.table xtabs
    Consider adding
      importFrom("graphics", "abline", "axis", "lines", "mtext", "par",
                 "plot", "points")
      importFrom("methods", "as")
      importFrom("stats", "approxfun", "dunif", "loess", "lowess", "na.omit",
                 "predict", "uniroot", "xtabs")
      importFrom("utils", "head", "read.delim", "tail", "write.table")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘HMMcopy’, ‘list’
    ```

# tmap

Version: 1.11-2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.7Mb
      sub-directories of 1Mb or more:
        data   1.5Mb
        doc    3.3Mb
    ```

# totalcensus

Version: 0.3.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 42 marked Latin-1 strings
      Note: found 360 marked UTF-8 strings
    ```

# toxplot

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyr’
      All declared Imports should be used.
    ```

# TPP

Version: 3.8.0

## In both

*   checking examples ... ERROR
    ```
    ...
    Removing duplicate identifiers using quality column 'qupm'...
    261 out of 261 rows kept for further analysis.
    Reformating data for input into function 'analyzeTPPCCR' ...
    Done.
    No output directory specified. No result files or plots will be produced.
    Looking for intensity column prefix: 'sumionarea_protein_'
    Computing fold changes...
    Done.
    Found the following column name in attr(data, 'importSettings')$proteinIdCol: 'representative'
    Found the following column name in attr(data, 'importSettings')$fcStr: 'rel_fc_protein_'
    Performing median normalization per temperature...
    Done.
    Looking for unique ID column: 'unique_ID'
    Looking for nonZeroCols: 'qusm'
    Checking which columns in the data table contain the fold change values for fitting and plotting...
    Normalized data columns detected with prefix 'norm_rel_fc_protein_'. Analysis will be based on these values.
    This information was found in the attributes of the input data (access with attr(dataTable, 'importSettings'))
    Performing TPP-CCR dose response curve fitting and generating result table...
    Error in foldChanges[, refCol] : incorrect number of dimensions
    Calls: analyze2DTPP ... withCallingHandlers -> analyzeTPPCCR -> tppccrNormalizeToReference
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 385 SKIPPED: 0 FAILED: 35
      1. Error: allOK (@test_analyze2DTPP.R#14) 
      2. Error: allOK_scientific_drug_concentration_format (@test_analyze2DTPP.R#37) 
      3. Error: warning_deprecated_fct_arg (@test_analyze2DTPP.R#62) 
      4. Error: NPARC_allok (@test_analyzeTPPTR.R#14) 
      5. Error: NPARC_allok_output (@test_analyzeTPPTR.R#34) 
      6. Error: NPARC_allok_plot (@test_analyzeTPPTR.R#61) 
      7. Error: NPARC_allok_files (@test_analyzeTPPTR.R#94) 
      8. Error: meltCurves_allOK_no_conditions (@test_analyzeTPPTR.R#153) 
      9. Error: testApplyCoeffs (@test_applyCoeffs.R#9) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking whether package ‘TPP’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘Biobase::exprs’ by ‘dplyr::exprs’ when loading ‘TPP’
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/TPP/new/TPP.Rcheck/00install.out’ for details.
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    Filtering by annotation column(s) 'qssm' in treatment group: Panobinostat_1
      Column qssm between 4 and Inf-> 333 out of 508 proteins passed.
    
    333 out of 508 proteins passed in total.
    
    Filtering by annotation column(s) 'qssm' in treatment group: Panobinostat_2
      Column qssm between 4 and Inf-> 364 out of 509 proteins passed.
    
    364 out of 509 proteins passed in total.
    
    	2. Find jointP:
    Detecting intersect between treatment groups (jointP).
    -> JointP contains 261 proteins.
    
    	3. Filtering fold changes:
    Filtering fold changes in treatment group: Vehicle_1
    Quitting from lines 73-76 (NPARC_analysis_of_TPP_TR_data.Rnw) 
    Error: processing vignette 'NPARC_analysis_of_TPP_TR_data.Rnw' failed with diagnostics:
    incorrect number of dimensions
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 13.5Mb
      sub-directories of 1Mb or more:
        data           1.9Mb
        example_data   8.0Mb
        test_data      1.9Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported objects imported by ':::' calls:
      ‘doParallel:::.options’ ‘mefa:::rep.data.frame’
      See the note in ?`:::` about the use of this operator.
    ```

*   checking R code for possible problems ... NOTE
    ```
    File ‘TPP/R/TPP.R’:
      .onLoad calls:
        packageStartupMessage(msgText, "\n")
    
    See section ‘Good practice’ in '?.onAttach'.
    
    plot_fSta_distribution: no visible binding for global variable
      ‘..density..’
    plot_pVal_distribution: no visible binding for global variable
      ‘..density..’
    Undefined global functions or variables:
      ..density..
    ```

# trackr

Version: 0.7.5

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘rsolr’
    
    Package suggested but not available for checking: ‘RSelenium’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# TrafficBDE

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘neuralnet’
      All declared Imports should be used.
    ```

# translateSPSS2R

Version: 1.0.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    xpssMeans: no visible global function definition for ‘anova’
    xpssRegression: no visible global function definition for ‘na.omit’
    xpssRegression: no visible global function definition for ‘anova’
    xpssRegression: no visible binding for global variable ‘sd’
    xpssTtest: no visible global function definition for ‘complete.cases’
    xpssTtest: no visible global function definition for ‘t.test’
    xpssTtest: no visible global function definition for ‘na.omit’
    xpssTtest: no visible global function definition for ‘sd’
    xpssTtest: no visible global function definition for ‘var’
    xpssTtest: no visible global function definition for ‘cor.test’
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

# tree.bins

Version: 0.1.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘forcats’
    ```

# tricolore

Version: 1.0.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# tRophicPosition

Version: 0.7.5

## In both

*   checking examples ... ERROR
    ```
    ...
    Running examples in ‘tRophicPosition-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: TPmodel
    > ### Title: Function to create a JAGS-based Bayesian model to calculate
    > ###   trophic position
    > ### Aliases: TPmodel
    > 
    > ### ** Examples
    > 
    > isotopeData <- generateTPData()
    > model.string <- jagsBayesianModel()
    > model <- TPmodel(data = isotopeData, model.string = model.string,
    + n.adapt = 500)
    Error: .onLoad failed in loadNamespace() for 'rjags', details:
      call: dyn.load(file, DLLpath = DLLpath, ...)
      error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/tRophicPosition/rjags/libs/rjags.so':
      dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/tRophicPosition/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
      Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/tRophicPosition/rjags/libs/rjags.so
      Reason: image not found
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 108-110 (Multiple_model_calculation_of_trophic_position_in_R.Rmd) 
    Error: processing vignette 'Multiple_model_calculation_of_trophic_position_in_R.Rmd' failed with diagnostics:
    .onLoad failed in loadNamespace() for 'rjags', details:
      call: dyn.load(file, DLLpath = DLLpath, ...)
      error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/tRophicPosition/rjags/libs/rjags.so':
      dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/tRophicPosition/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
      Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/tRophicPosition/rjags/libs/rjags.so
      Reason: image not found
    Execution halted
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘SIBER’
    ```

# turfR

Version: 0.8-7

## In both

*   checking R code for possible problems ... NOTE
    ```
    turf: no visible global function definition for ‘read.table’
    turf: no visible global function definition for ‘flush.console’
    turf.combos: no visible global function definition for ‘combn’
    Undefined global functions or variables:
      combn flush.console read.table
    Consider adding
      importFrom("utils", "combn", "flush.console", "read.table")
    to your NAMESPACE file.
    ```

# ukbtools

Version: 0.11.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘plyr’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 5 marked UTF-8 strings
    ```

# unvotes

Version: 0.2.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4494 marked UTF-8 strings
    ```

# USAboundaries

Version: 0.3.1

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘USAboundariesData’
    ```

# utilsIPEA

Version: 0.0.6

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘RCurl’ ‘stringdist’ ‘utils’
      All declared Imports should be used.
    ```

# vaersNDvax

Version: 1.0.4

## In both

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking: ‘vaers’ ‘vaersND’
    ```

# vaersvax

Version: 1.0.4

## In both

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking: ‘vaers’ ‘vaersND’
    ```

# valaddin

Version: 0.1.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘purrr’
      All declared Imports should be used.
    ```

# vdmR

Version: 0.2.5

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Rdpack’ ‘maptools’ ‘rgeos’
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

Version: 1.0.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        data   4.0Mb
        doc    1.7Mb
    ```

# VIM

Version: 4.7.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘mvoutlier’, ‘StatDA’, ‘mi’, ‘tkrplot’
    ```

# vinereg

Version: 0.2.0

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
    
    Quitting from lines 158-168 (abalone-example.Rmd) 
    Error: processing vignette 'abalone-example.Rmd' failed with diagnostics:
    there is no package called 'quantreg'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘quantreg’
    ```

# vlad

Version: 0.1.0

## In both

*   checking whether package ‘vlad’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/vlad/new/vlad.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘vlad’ ...
** package ‘vlad’ successfully unpacked and MD5 sums checked
** libs
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/vlad/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/vlad/RcppArmadillo/include" -I/usr/local/include  -fopenmp -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘vlad’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/vlad/new/vlad.Rcheck/vlad’

```
### CRAN

```
* installing *source* package ‘vlad’ ...
** package ‘vlad’ successfully unpacked and MD5 sums checked
** libs
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/vlad/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/vlad/RcppArmadillo/include" -I/usr/local/include  -fopenmp -fPIC  -arch x86_64  -ftemplate-depth-256  -Wall  -pedantic  -O0 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘vlad’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/vlad/old/vlad.Rcheck/vlad’

```
# vqtl

Version: 2.0.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘iterators’ ‘knitr’ ‘purrr’ ‘testthat’
      All declared Imports should be used.
    ```

# waccR

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘lubridate’ ‘tibble’
      All declared Imports should be used.
    ```

# walker

Version: 0.2.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.0Mb
      sub-directories of 1Mb or more:
        doc    2.2Mb
        libs   5.5Mb
    ```

# wbstats

Version: 0.2

## In both

*   R CMD check timed out
    

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1528 marked UTF-8 strings
    ```

# whereport

Version: 0.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4102 marked UTF-8 strings
    ```

# wiggleplotr

Version: 1.4.0

## In both

*   checking examples ... ERROR
    ```
    ...
    Loading required package: IRanges
    
    Attaching package: ‘IRanges’
    
    The following objects are masked from ‘package:dplyr’:
    
        collapse, desc, slice
    
    Loading required package: GenomeInfoDb
    > require("org.Hs.eg.db")
    Loading required package: org.Hs.eg.db
    Warning in library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE,  :
      there is no package called ‘org.Hs.eg.db’
    > require("TxDb.Hsapiens.UCSC.hg38.knownGene")
    Loading required package: TxDb.Hsapiens.UCSC.hg38.knownGene
    Warning in library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE,  :
      there is no package called ‘TxDb.Hsapiens.UCSC.hg38.knownGene’
    > 
    > orgdb = org.Hs.eg.db
    Error: object 'org.Hs.eg.db' not found
    Execution halted
    ```

*   checking for code/documentation mismatches ... WARNING
    ```
    Codoc mismatches from documentation object 'getGenotypePalette':
    getGenotypePalette
      Code: function(old = FALSE)
      Docs: function()
      Argument names in code not in docs:
        old
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 18-28 (wiggleplotr.Rmd) 
    Error: processing vignette 'wiggleplotr.Rmd' failed with diagnostics:
    there is no package called 'EnsDb.Hsapiens.v86'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘EnsDb.Hsapiens.v86’ ‘org.Hs.eg.db’
      ‘TxDb.Hsapiens.UCSC.hg38.knownGene’
    ```

*   checking R code for possible problems ... NOTE
    ```
    plotCoverage: no visible global function definition for ‘is’
    plotTranscripts: no visible global function definition for ‘is’
    Undefined global functions or variables:
      is
    Consider adding
      importFrom("methods", "is")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# windfarmGA

Version: 1.2.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘RandomFields’ ‘data.table’
      All declared Imports should be used.
    ```

# wordbankr

Version: 0.3.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dbplyr’
      All declared Imports should be used.
    ```

# WRTDStidal

Version: 1.1.0

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘quantreg’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# XBSeq

Version: 1.12.0

## In both

*   R CMD check timed out
    

*   checking whether the namespace can be loaded with stated dependencies ... NOTE
    ```
    Warning: no function found corresponding to methods exports from ‘XBSeq’ for: ‘conditions’, ‘conditions<-’, ‘dispTable’
    
    A namespace must be able to be loaded with just the base namespace
    loaded: otherwise if the namespace gets loaded by a saved object, the
    session will be unable to start.
    
    Probably some imports need to be declared in the NAMESPACE file.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    estimateRealCount,XBSeqDataSet: no visible global function definition
      for ‘assay’
    estimateRealCount,XBSeqDataSet: no visible global function definition
      for ‘assay<-’
    estimateSCV,XBSeqDataSet: no visible global function definition for
      ‘conditions’
    estimateSCV,XBSeqDataSet: no visible global function definition for
      ‘dispTable<-’
    Undefined global functions or variables:
      ..count.. DataFrame Gamma Group Sample SummarizedExperiment assay
      assay<- assays baseMean coefficients complete.cases conditions cor
      data ddelap dispTable dispTable<- dnbinom dpois formula glm
      log2FoldChange median optim p.adjust pbeta predict qbeta quantile
      rnbinom scvBiasCorrectionFits
    Consider adding
      importFrom("stats", "Gamma", "coefficients", "complete.cases", "cor",
                 "dnbinom", "dpois", "formula", "glm", "median", "optim",
                 "p.adjust", "pbeta", "predict", "qbeta", "quantile",
                 "rnbinom")
      importFrom("utils", "data")
    to your NAMESPACE file.
    ```

# XKCDdata

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tibble’
      All declared Imports should be used.
    ```

# xtractomatic

Version: 3.4.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# xxIRT

Version: 2.0.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘graphics’
      All declared Imports should be used.
    ```

# Zelig

Version: 5.1.6

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘quantreg’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# ZeligChoice

Version: 0.9-6

## In both

*   checking whether package ‘ZeligChoice’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/ZeligChoice/new/ZeligChoice.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘ZeligChoice’ ...
** package ‘ZeligChoice’ successfully unpacked and MD5 sums checked
** R
** data
** demo
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘quantreg’
ERROR: lazy loading failed for package ‘ZeligChoice’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/ZeligChoice/new/ZeligChoice.Rcheck/ZeligChoice’

```
### CRAN

```
* installing *source* package ‘ZeligChoice’ ...
** package ‘ZeligChoice’ successfully unpacked and MD5 sums checked
** R
** data
** demo
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘quantreg’
ERROR: lazy loading failed for package ‘ZeligChoice’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/ZeligChoice/old/ZeligChoice.Rcheck/ZeligChoice’

```
# ZeligEI

Version: 0.1-2

## In both

*   checking whether package ‘ZeligEI’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/ZeligEI/new/ZeligEI.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘ZeligEI’ ...
** package ‘ZeligEI’ successfully unpacked and MD5 sums checked
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘quantreg’
ERROR: lazy loading failed for package ‘ZeligEI’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/ZeligEI/new/ZeligEI.Rcheck/ZeligEI’

```
### CRAN

```
* installing *source* package ‘ZeligEI’ ...
** package ‘ZeligEI’ successfully unpacked and MD5 sums checked
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘quantreg’
ERROR: lazy loading failed for package ‘ZeligEI’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/ZeligEI/old/ZeligEI.Rcheck/ZeligEI’

```
# zeligverse

Version: 0.1.1

## In both

*   checking whether package ‘zeligverse’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/zeligverse/new/zeligverse.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘zeligverse’ ...
** package ‘zeligverse’ successfully unpacked and MD5 sums checked
** R
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘quantreg’
ERROR: lazy loading failed for package ‘zeligverse’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/zeligverse/new/zeligverse.Rcheck/zeligverse’

```
### CRAN

```
* installing *source* package ‘zeligverse’ ...
** package ‘zeligverse’ successfully unpacked and MD5 sums checked
** R
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘quantreg’
ERROR: lazy loading failed for package ‘zeligverse’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/zeligverse/old/zeligverse.Rcheck/zeligverse’

```
# zFactor

Version: 0.1.7

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rootSolve’
      All declared Imports should be used.
    ```

# zFPKM

Version: 1.2.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    The following object is masked from 'package:S4Vectors':
    
        expand
    
    trying URL 'ftp://ftp.ncbi.nlm.nih.gov/geo/series/GSE94nnn/GSE94802/suppl/GSE94802_Minkina_etal_normalized_FPKM.csv.gz'
    Content type 'unknown' length 800733 bytes (781 KB)
    ==================================================
    trying URL 'ftp://ftp.ncbi.nlm.nih.gov/geo/series/GSE94nnn/GSE94802/suppl/GSE94802_Minkina_etal_raw_counts.csv.gz'
    Content type 'unknown' length 574041 bytes (560 KB)
    ==================================================
    
    Attaching package: 'limma'
    
    The following object is masked from 'package:BiocGenerics':
    
        plotMA
    
    Quitting from lines 108-122 (zFPKM.Rmd) 
    Error: processing vignette 'zFPKM.Rmd' failed with diagnostics:
    statmod package required but is not installed
    Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    PlotGaussianFitDF: no visible global function definition for ‘dnorm’
    PlotGaussianFitDF: no visible binding for global variable ‘density’
    PlotGaussianFitDF: no visible binding for global variable ‘log2fpkm’
    PlotGaussianFitDF: no visible binding for global variable ‘sample_name’
    zFPKMCalc: no visible global function definition for ‘density’
    zFPKMTransform: no visible global function definition for ‘is’
    Undefined global functions or variables:
      density dnorm is log2fpkm sample_name
    Consider adding
      importFrom("methods", "is")
      importFrom("stats", "density", "dnorm")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# ztype

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘ggplot2’ ‘lubridate’
      All declared Imports should be used.
    ```

