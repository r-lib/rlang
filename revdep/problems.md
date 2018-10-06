# abjutils

Version: 0.2.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘httr’ ‘progress’
      All declared Imports should be used.
    ```

# ActisoftR

Version: 0.0.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
      no non-missing arguments to max; returning -Inf
    Warning in min(sleep_time_adj, na.rm = T) :
      no non-missing arguments to min; returning Inf
    Warning in max(sleep_time_adj, na.rm = T) :
      no non-missing arguments to max; returning -Inf
    Warning in min(sleep_time_adj, na.rm = T) :
      no non-missing arguments to min; returning Inf
    Warning in max(sleep_time_adj, na.rm = T) :
      no non-missing arguments to max; returning -Inf
    Warning in min(sleep_time_adj, na.rm = T) :
      no non-missing arguments to min; returning Inf
    Warning in max(sleep_time_adj, na.rm = T) :
      no non-missing arguments to max; returning -Inf
    Warning in min(sleep_time_adj, na.rm = T) :
      no non-missing arguments to min; returning Inf
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'ActisoftR.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# adaptalint

Version: 0.2.3

## In both

*   R CMD check timed out
    

# adaptMT

Version: 1.0.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'adapt_demo.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# adegenet

Version: 2.1.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.7Mb
      sub-directories of 1Mb or more:
        data    1.3Mb
        files   1.7Mb
        R       3.0Mb
    ```

# admixturegraph

Version: 1.0.2

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Loading required package: optimbase
    Loading required package: Matrix
    Loading required package: optimsimplex
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'admixturegraph.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.1Mb
      sub-directories of 1Mb or more:
        data   2.0Mb
        R      3.1Mb
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

Version: 0.22-1

## In both

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
      ‘assertthat’ ‘DBI’ ‘tibble’
      All declared Imports should be used.
    ```

# agridat

Version: 1.16

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Loading required package: RColorBrewer
    Loading required package: multcomp
    Loading required package: mvtnorm
    Loading required package: survival
    Loading required package: TH.data
    Loading required package: MASS
    
    Attaching package: 'TH.data'
    
    The following object is masked from 'package:MASS':
    
        geyser
    
    Loading required package: gridExtra
    Loading required package: maps
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'agridat_examples.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# ahpsurvey

Version: 0.2.2

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'my-vignette.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘knitr’ ‘tidyr’
      All declared Imports should be used.
    ```

# aire.zmvm

Version: 0.6.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 52 marked UTF-8 strings
    ```

# airGR

Version: 1.0.14.1

## In both

*   checking whether package ‘airGR’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/airGR/new/airGR.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘airGR’ ...
** package ‘airGR’ successfully unpacked and MD5 sums checked
** libs
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c airGR.c -o airGR.o
gfortran-7   -fPIC  -g -O2  -c frun_CEMANEIGE.f -o frun_CEMANEIGE.o
make: gfortran-7: No such file or directory
make: *** [frun_CEMANEIGE.o] Error 1
ERROR: compilation failed for package ‘airGR’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/airGR/new/airGR.Rcheck/airGR’

```
### CRAN

```
* installing *source* package ‘airGR’ ...
** package ‘airGR’ successfully unpacked and MD5 sums checked
** libs
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c airGR.c -o airGR.o
gfortran-7   -fPIC  -g -O2  -c frun_CEMANEIGE.f -o frun_CEMANEIGE.o
make: gfortran-7: No such file or directory
make: *** [frun_CEMANEIGE.o] Error 1
ERROR: compilation failed for package ‘airGR’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/airGR/old/airGR.Rcheck/airGR’

```
# alphavantager

Version: 0.1.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      2: stop(content, call. = F) at /Users/lionel/Desktop/rlang/revdep/checks.noindex/alphavantager/new/alphavantager.Rcheck/00_pkg_src/alphavantager/R/av_get.R:103
      
      ── 3. Error: call Technical Indicators (@test_av_get.R#57)  ────────────────────
      Thank you for using Alpha Vantage! Please visit https://www.alphavantage.co/premium/ if you would like to have a higher API call volume.. API parameters used: symbol=MSFT, function=SMA, interval=monthly, time_period=60, series_type=close, apikey=HIDDEN_FOR_YOUR_SAFETY
      1: av_get(symbol, av_fun, interval = interval, time_period = time_period, series_type = series_type) at testthat/test_av_get.R:57
      2: stop(content, call. = F) at /Users/lionel/Desktop/rlang/revdep/checks.noindex/alphavantager/new/alphavantager.Rcheck/00_pkg_src/alphavantager/R/av_get.R:103
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 5 SKIPPED: 0 FAILED: 3
      1. Error: call TIMES_SERIES_INTRADAY (@test_av_get.R#13) 
      2. Error: call SECTOR (@test_av_get.R#38) 
      3. Error: call Technical Indicators (@test_av_get.R#57) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘devtools’
      All declared Imports should be used.
    ```

# alternativeSplicingEvents.hg19

Version: 1.0.1

## In both

*   checking extension type ... ERROR
    ```
    Extensions with Type ‘Annotation package’ cannot be checked.
    ```

# ameco

Version: 0.2.9

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 16.2Mb
      sub-directories of 1Mb or more:
        data  16.1Mb
    ```

# amplican

Version: 1.2.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 13.8Mb
      sub-directories of 1Mb or more:
        doc  12.5Mb
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘CrispRVariants’
    ```

# AMR

Version: 0.4.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘hms’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 67 marked UTF-8 strings
    ```

# amt

Version: 0.0.5.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘magrittr’ ‘Rcpp’
      All declared Imports should be used.
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
Unable to find any JVMs matching version "(null)".
No Java runtime present, try --request to install.
Warning in system("/usr/libexec/java_home", intern = TRUE) :
  running command '/usr/libexec/java_home' had status 1
Error: package or namespace load failed for ‘qdap’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/ANLP/rJava/libs/rJava.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/ANLP/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/ANLP/rJava/libs/rJava.so
  Reason: image not found
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
Unable to find any JVMs matching version "(null)".
No Java runtime present, try --request to install.
Warning in system("/usr/libexec/java_home", intern = TRUE) :
  running command '/usr/libexec/java_home' had status 1
Error: package or namespace load failed for ‘qdap’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/ANLP/rJava/libs/rJava.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/ANLP/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/ANLP/rJava/libs/rJava.so
  Reason: image not found
Error : package ‘qdap’ could not be loaded
ERROR: lazy loading failed for package ‘ANLP’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/ANLP/old/ANLP.Rcheck/ANLP’

```
# annotatr

Version: 1.6.0

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘annotatr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: build_annotations
    > ### Title: A function to build annotations from TxDb.* and AnnotationHub
    > ###   resources
    > ### Aliases: build_annotations
    > 
    > ### ** Examples
    > 
    > # Example with hg19 gene promoters
    > annots = c('hg19_genes_promoters')
    > annots_gr = build_annotations(genome = 'hg19', annotations = annots)
    Error in build_gene_annots(genome = genome, annotations = gene_annotations) : 
      The package TxDb.Hsapiens.UCSC.hg19.knownGene is not installed, please install it via Bioconductor.
    Calls: build_annotations
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    snapshotDate(): 2018-04-30
    Building annotation Gm12878 from AnnotationHub resource AH23256 ...
    require("rtracklayer")
    downloading 0 resources
    loading from cache 
        '/Users/lionel//.AnnotationHub/28684'
    Quitting from lines 153-170 (annotatr-vignette.Rmd) 
    Error: processing vignette 'annotatr-vignette.Rmd' failed with diagnostics:
    The package TxDb.Hsapiens.UCSC.hg19.knownGene is not installed, please install it via Bioconductor.
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘org.Dm.eg.db’ ‘org.Gg.eg.db’ ‘org.Hs.eg.db’ ‘org.Mm.eg.db’
      ‘org.Rn.eg.db’ ‘TxDb.Dmelanogaster.UCSC.dm3.ensGene’
      ‘TxDb.Dmelanogaster.UCSC.dm6.ensGene’
      ‘TxDb.Ggallus.UCSC.galGal5.refGene’
      ‘TxDb.Hsapiens.UCSC.hg19.knownGene’
      ‘TxDb.Hsapiens.UCSC.hg38.knownGene’
      ‘TxDb.Mmusculus.UCSC.mm9.knownGene’
      ‘TxDb.Mmusculus.UCSC.mm10.knownGene’
      ‘TxDb.Rnorvegicus.UCSC.rn4.ensGene’
      ‘TxDb.Rnorvegicus.UCSC.rn5.refGene’
      ‘TxDb.Rnorvegicus.UCSC.rn6.refGene’
    ```

*   checking R code for possible problems ... NOTE
    ```
    plot_coannotations: no visible binding for global variable ‘.’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/annotatr/new/annotatr.Rcheck/00_pkg_src/annotatr/R/visualize.R:176-178)
    plot_numerical_coannotations: no visible binding for global variable
      ‘.’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/annotatr/new/annotatr.Rcheck/00_pkg_src/annotatr/R/visualize.R:463-480)
    plot_numerical_coannotations: no visible binding for global variable
      ‘.’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/annotatr/new/annotatr.Rcheck/00_pkg_src/annotatr/R/visualize.R:466-471)
    plot_numerical_coannotations: no visible binding for global variable
      ‘.’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/annotatr/new/annotatr.Rcheck/00_pkg_src/annotatr/R/visualize.R:473-478)
    Undefined global functions or variables:
      .
    ```

# anomalize

Version: 0.1.1

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > tidyverse_cran_downloads %>%
    +     filter(package == "tidyquant") %>%
    +     ungroup() %>%
    +     time_decompose(count, method = "stl") %>%
    +     anomalize(remainder, method = "iqr") %>%
    +     time_recompose() %>%
    +     plot_anomalies(time_recomposed = TRUE)
    frequency = 7 days
    trend = 91 days
    > 
    > 
    > #### MULTIPLE TIME SERIES ####
    > tidyverse_cran_downloads %>%
    +     time_decompose(count, method = "stl") %>%
    +     anomalize(remainder, method = "iqr") %>%
    +     time_recompose() %>%
    +     plot_anomalies(time_recomposed = TRUE, ncol = 3)
    Error in if (message) message(glue::glue("Converting from {cl} to {class(data)[[1]]}.\n                                    Auto-index message: index = {idx}")) : 
      argument is not interpretable as logical
    Calls: %>% ... time_recompose.grouped_df -> prep_tbl_time -> prep_tbl_time.data.frame
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      6: freduce(value, `_function_list`)
      7: withVisible(function_list[[k]](value))
      8: function_list[[k]](value)
      9: time_recompose(.)
      10: time_recompose.grouped_df(.) at /Users/lionel/Desktop/rlang/revdep/checks.noindex/anomalize/new/anomalize.Rcheck/00_pkg_src/anomalize/R/time_recompose.R:40
      11: prep_tbl_time(data, message = message) at /Users/lionel/Desktop/rlang/revdep/checks.noindex/anomalize/new/anomalize.Rcheck/00_pkg_src/anomalize/R/time_recompose.R:133
      12: prep_tbl_time.data.frame(data, message = message) at /Users/lionel/Desktop/rlang/revdep/checks.noindex/anomalize/new/anomalize.Rcheck/00_pkg_src/anomalize/R/prep_tbl_time.R:27
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 62 SKIPPED: 0 FAILED: 2
      1. Error: returns a ggplot (@test-plot_anomalies.R#8) 
      2. Error: time_recompose works on grouped_tbl_time (@test-time_recompose.R#9) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 68-74 (anomalize_quick_start_guide.Rmd) 
    Error: processing vignette 'anomalize_quick_start_guide.Rmd' failed with diagnostics:
    argument is not interpretable as logical
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
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
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/anomalyDetection/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/anomalyDetection/RcppArmadillo/include" -I/usr/local/include  -fopenmp  -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c RcppExports.cpp -o RcppExports.o
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
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/anomalyDetection/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/anomalyDetection/RcppArmadillo/include" -I/usr/local/include  -fopenmp  -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘anomalyDetection’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/anomalyDetection/old/anomalyDetection.Rcheck/anomalyDetection’

```
# anyflights

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tibble’
      All declared Imports should be used.
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

# auctestr

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyr’
      All declared Imports should be used.
    ```

# augmentedRCBD

Version: 0.1.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘agricolae’
    ```

# auk

Version: 0.3.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 552 marked UTF-8 strings
    ```

# autocogs

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘broom’ ‘diptest’ ‘ggplot2’ ‘hexbin’ ‘MASS’ ‘moments’
      All declared Imports should be used.
    ```

# BALCONY

Version: 0.2.8

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘base’
      All declared Imports should be used.
    ```

# BANEScarparkinglite

Version: 0.1.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘zoo’
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

# BAS

Version: 1.5.1

## In both

*   checking whether package ‘BAS’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/BAS/new/BAS.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘BAS’ ...
** package ‘BAS’ successfully unpacked and MD5 sums checked
** libs
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c CHg_postzbeta.c -o CHg_postzbeta.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c E.ZS.c -o E.ZS.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c ZS.approx.full.np.c -o ZS.approx.full.np.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c ZS.approx.null.np.c -o ZS.approx.null.np.o
ZS.approx.null.np.c:108:30: warning: variable 'mode' is uninitialized when used here [-Wuninitialized]
  REAL(Rtheta)[4] = (double) mode;
                             ^~~~
ZS.approx.null.np.c:90:14: note: initialize the variable 'mode' to silence this warning
  double mode, logmarg, bound=0.0, epsabs, epsrel, result, abserr, *work, *ex;
             ^
              = 0.0
1 warning generated.
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c amcmc.c -o amcmc.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c bayesglm.c -o bayesglm.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c bayesreg.c -o bayesreg.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c betapriorfamily.c -o betapriorfamily.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c cch.c -o cch.o
gfortran-7   -fPIC  -g -O2  -c ch2inv.f -o ch2inv.o
make: gfortran-7: No such file or directory
make: *** [ch2inv.o] Error 1
ERROR: compilation failed for package ‘BAS’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/BAS/new/BAS.Rcheck/BAS’

```
### CRAN

```
* installing *source* package ‘BAS’ ...
** package ‘BAS’ successfully unpacked and MD5 sums checked
** libs
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c CHg_postzbeta.c -o CHg_postzbeta.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c E.ZS.c -o E.ZS.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c ZS.approx.full.np.c -o ZS.approx.full.np.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c ZS.approx.null.np.c -o ZS.approx.null.np.o
ZS.approx.null.np.c:108:30: warning: variable 'mode' is uninitialized when used here [-Wuninitialized]
  REAL(Rtheta)[4] = (double) mode;
                             ^~~~
ZS.approx.null.np.c:90:14: note: initialize the variable 'mode' to silence this warning
  double mode, logmarg, bound=0.0, epsabs, epsrel, result, abserr, *work, *ex;
             ^
              = 0.0
1 warning generated.
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c amcmc.c -o amcmc.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c bayesglm.c -o bayesglm.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c bayesreg.c -o bayesreg.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c betapriorfamily.c -o betapriorfamily.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c cch.c -o cch.o
gfortran-7   -fPIC  -g -O2  -c ch2inv.f -o ch2inv.o
make: gfortran-7: No such file or directory
make: *** [ch2inv.o] Error 1
ERROR: compilation failed for package ‘BAS’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/BAS/old/BAS.Rcheck/BAS’

```
# basecallQC

Version: 1.4.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        doc       1.8Mb
        extdata   2.8Mb
    ```

# basictabler

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# bayesdfa

Version: 0.1.0

## In both

*   R CMD check timed out
    

*   checking installed package size ... NOTE
    ```
      installed size is 11.0Mb
      sub-directories of 1Mb or more:
        libs  10.1Mb
    ```

# bayesplot

Version: 1.6.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.2Mb
      sub-directories of 1Mb or more:
        doc   4.0Mb
        R     2.6Mb
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

# BgeeDB

Version: 2.6.2

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
# bib2df

Version: 1.0.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library("testthat")
      > library("bib2df")
      > test_check("bib2df")
      ── 1. Failure: bib2df() throws error messages (@tests.R#70)  ───────────────────
      `bib2df("https://www.example.com/data/x.bib")` threw an error with unexpected message.
      Expected match: "Invalid URL: File is not readable."
      Actual message: "Could not resolve host: www.example.com"
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 19 SKIPPED: 0 FAILED: 1
      1. Failure: bib2df() throws error messages (@tests.R#70) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# bigQueryR

Version: 0.4.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘bigrquery’
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

# biobroom

Version: 1.12.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    The following object is masked from 'package:dplyr':
    
        count
    
    Loading required package: BiocParallel
    
    Attaching package: 'DelayedArray'
    
    The following objects are masked from 'package:matrixStats':
    
        colMaxs, colMins, colRanges, rowMaxs, rowMins, rowRanges
    
    The following objects are masked from 'package:base':
    
        aperm, apply
    
    Quitting from lines 134-139 (biobroom_vignette.Rmd) 
    Error: processing vignette 'biobroom_vignette.Rmd' failed with diagnostics:
    there is no package called 'airway'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘airway’
    ```

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' call to ‘DESeq2’ in package code.
      Please use :: or requireNamespace() instead.
      See section 'Suggested packages' in the 'Writing R Extensions' manual.
    Missing or unexported object: ‘dplyr::tbl_dt’
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/biobroom/new/biobroom.Rcheck/00_pkg_src/biobroom/R/qvalue_tidiers.R:65-66)
    tidy.RangedSummarizedExperiment: no visible binding for global variable
      ‘value’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/biobroom/new/biobroom.Rcheck/00_pkg_src/biobroom/R/SummarizedExperiment_tidiers.R:43-45)
    tidy.RangedSummarizedExperiment: no visible binding for global variable
      ‘gene’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/biobroom/new/biobroom.Rcheck/00_pkg_src/biobroom/R/SummarizedExperiment_tidiers.R:43-45)
    tidy.RangedSummarizedExperiment: no visible global function definition
      for ‘colData’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/biobroom/new/biobroom.Rcheck/00_pkg_src/biobroom/R/SummarizedExperiment_tidiers.R:48)
    Undefined global functions or variables:
      . calcNormFactors colData counts design DGEList end estimate
      estimateSizeFactors exprs<- fData<- gene gr is lambda model.matrix
      p.adjust pData pData<- pi0 protein rowRanges sample.id seqnames
      setNames smoothed start tbl_dt term value voom voomWithQualityWeights
    Consider adding
      importFrom("methods", "is")
      importFrom("stats", "end", "model.matrix", "p.adjust", "setNames",
                 "start")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports
    field contains 'methods').
    ```

# bioCancer

Version: 1.8.0

## In both

*   checking package dependencies ... ERROR
    ```
    Packages required but not available: ‘org.Hs.eg.db’ ‘reactome.db’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# BiocOncoTK

Version: 1.0.3

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 18-30 (BiocOncoTK.Rmd) 
    Error: processing vignette 'BiocOncoTK.Rmd' failed with diagnostics:
    there is no package called 'org.Hs.eg.db'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘org.Hs.eg.db’ ‘TxDb.Hsapiens.UCSC.hg19.knownGene’
      ‘TxDb.Hsapiens.UCSC.hg18.knownGene’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        data   4.0Mb
        doc    1.7Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    rainfall: no visible global function definition for ‘ylab’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/BiocOncoTK/new/BiocOncoTK.Rcheck/00_pkg_src/BiocOncoTK/R/rainfall3.R:164-168)
    rainfall: no visible global function definition for ‘xlab’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/BiocOncoTK/new/BiocOncoTK.Rcheck/00_pkg_src/BiocOncoTK/R/rainfall3.R:164-168)
    rainfall: no visible global function definition for ‘geom_vline’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/BiocOncoTK/new/BiocOncoTK.Rcheck/00_pkg_src/BiocOncoTK/R/rainfall3.R:169-171)
    rainfall: no visible global function definition for ‘aes’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/BiocOncoTK/new/BiocOncoTK.Rcheck/00_pkg_src/BiocOncoTK/R/rainfall3.R:169-171)
    rainfall: no visible global function definition for
      ‘scale_x_continuous’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/BiocOncoTK/new/BiocOncoTK.Rcheck/00_pkg_src/BiocOncoTK/R/rainfall3.R:169-171)
    rainfall: no visible global function definition for ‘ggtitle’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/BiocOncoTK/new/BiocOncoTK.Rcheck/00_pkg_src/BiocOncoTK/R/rainfall3.R:172)
    rainfall: no visible global function definition for ‘geom_text’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/BiocOncoTK/new/BiocOncoTK.Rcheck/00_pkg_src/BiocOncoTK/R/rainfall3.R:173-174)
    rainfall: no visible global function definition for ‘aes’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/BiocOncoTK/new/BiocOncoTK.Rcheck/00_pkg_src/BiocOncoTK/R/rainfall3.R:173-174)
    Undefined global functions or variables:
      aes BiocFileCache element_blank genome geom_point geom_text
      geom_vline ggplot ggtitle scale_x_continuous seqlengths theme xlab
      ylab
    ```

# biotmle

Version: 1.4.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    .biotmle: no visible global function definition for ‘new’
    Undefined global functions or variables:
      new
    Consider adding
      importFrom("methods", "new")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports
    field contains 'methods').
    ```

# biovizBase

Version: 1.28.2

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

# bisect

Version: 0.9.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'bisect_vignette.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# blkbox

Version: 1.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘bigrf’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unable to find any JVMs matching version "(null)".
    No Java runtime present, try --request to install.
    Namespaces in Imports field not imported from:
      ‘glmnet’ ‘gtools’ ‘knitr’ ‘nnet’ ‘parallel’ ‘reshape’ ‘rJava’
      ‘rmarkdown’ ‘shinyjs’
      All declared Imports should be used.
    Missing or unexported object: ‘xgboost::predict’
    ```

# BloodCancerMultiOmics2017

Version: 1.0.2

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    The following objects are masked from 'package:IRanges':
    
        intersect, setdiff, union
    
    The following objects are masked from 'package:S4Vectors':
    
        intersect, setdiff, union
    
    The following objects are masked from 'package:BiocGenerics':
    
        intersect, setdiff, union
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, union
    
    Quitting from lines 46-92 (BloodCancerMultiOmics2017.Rmd) 
    Error: processing vignette 'BloodCancerMultiOmics2017.Rmd' failed with diagnostics:
    there is no package called 'org.Hs.eg.db'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘org.Hs.eg.db’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 115.7Mb
      sub-directories of 1Mb or more:
        data     80.0Mb
        doc      26.5Mb
        extdata   8.5Mb
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘vsn’
    ```

# blorr

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘descriptr’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘lmtest’
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

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(testthat)
      > library(blscrapeR)
      > 
      > test_check("blscrapeR")
      ── 1. Error: (unknown) (@test_qcew_api.R#8)  ───────────────────────────────────
      cannot open URL 'https://data.bls.gov/cew/data/api/2013/1/industry/5112.csv'
      1: download.file(url, temp, quiet = TRUE) at testthat/test_qcew_api.R:8
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 2 SKIPPED: 0 FAILED: 1
      1. Error: (unknown) (@test_qcew_api.R#8) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# bmlm

Version: 1.3.7

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘methods’
      All declared Imports should be used.
    ```

# BMSC

Version: 0.1

## In both

*   R CMD check timed out
    

*   checking installed package size ... NOTE
    ```
      installed size is 10.8Mb
      sub-directories of 1Mb or more:
        libs  10.5Mb
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

# bootnet

Version: 1.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘psych’
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

# BradleyTerryScalable

Version: 0.1.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:igraph':
    
        as_data_frame, groups, union
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'BradleyTerryScalable.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# BrailleR

Version: 0.29.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'BrailleRHistory.rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘installr’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        doc     1.3Mb
        R       2.1Mb
        Sound   1.0Mb
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

Version: 0.4.3

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Loading required package: Rcpp
      > options(warn = 2)
      > # Only one test per file to avoid hanging 32-bit compile
      > #test_check("breathteststan", filter = "stan_fit")
      > Sys.unsetenv("R_TESTS") # https://github.com/r-lib/testthat/issues/603
      > test_check("breathteststan")
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
      installed size is  7.7Mb
      sub-directories of 1Mb or more:
        libs   7.5Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# broom.mixed

Version: 0.2.2

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘glmmADMB’
    ```

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
      installed size is 29.4Mb
      sub-directories of 1Mb or more:
        data  23.4Mb
        doc    5.3Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    annoByOverlap,Annotate: no visible binding for global variable
      'queryHits'
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/BubbleTree/new/BubbleTree.Rcheck/00_pkg_src/BubbleTree/R/Annotate.R:107)
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

# capm

Version: 0.13.5

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 59 marked UTF-8 strings
    ```

# CARBayesST

Version: 3.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        doc    1.1Mb
        libs   1.1Mb
        R      3.1Mb
    ```

# caret

Version: 6.0-80

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.4Mb
      sub-directories of 1Mb or more:
        data     1.5Mb
        models   2.4Mb
        R        4.1Mb
    ```

# cartools

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘animation’ ‘devtools’ ‘gapminder’ ‘knitr’ ‘rlist’ ‘rmarkdown’
      ‘roxygen2’ ‘sde’ ‘shiny’ ‘tidyverse’ ‘usethis’ ‘utils’
      All declared Imports should be used.
    ```

# CaseBasedReasoning

Version: 0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘cowplot’ ‘dplyr’ ‘ranger’ ‘Rcpp’ ‘rms’ ‘survival’ ‘tidyverse’
      All declared Imports should be used.
    ```

# CATALYST

Version: 1.4.2

## In both

*   R CMD check timed out
    

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .travis.yml
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 10.5Mb
      sub-directories of 1Mb or more:
        data   3.1Mb
        doc    5.1Mb
        R      2.0Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    plotDiffHeatmap,matrix-SummarizedExperiment: no visible binding for
      global variable ‘cluster_id’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/CATALYST/new/CATALYST.Rcheck/00_pkg_src/CATALYST/R/plotDiffHeatmap.R:136)
    plotDiffHeatmap,matrix-SummarizedExperiment: no visible binding for
      global variable ‘sample_id’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/CATALYST/new/CATALYST.Rcheck/00_pkg_src/CATALYST/R/plotDiffHeatmap.R:136)
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

# CausalImpact

Version: 1.2.3

## In both

*   checking R code for possible problems ... NOTE
    ```
    ConstructModel: warning in AddDynamicRegression(ss, formula, data =
      data, sigma.mean.prior = sigma.mean.prior): partial argument match of
      'sigma.mean.prior' to 'sigma.mean.prior.DEPRECATED'
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/CausalImpact/new/CausalImpact.Rcheck/00_pkg_src/CausalImpact/R/impact_model.R:232-233)
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

# cdcfluview

Version: 0.7.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘units’
      All declared Imports should be used.
    ```

# CDECRetrieve

Version: 0.1.2

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘CDECRetrieve-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: cdec_rt
    > ### Title: Get a rating table
    > ### Aliases: cdec_rt
    > 
    > ### ** Examples
    > 
    > cdec_rt("abj") # get the stage to rating curve for ABJ
    Error in open.connection(x, "rb") : 
      Recv failure: Connection reset by peer
    Calls: cdec_rt ... %in% -> cdec_rt_list -> <Anonymous> -> read_html.default
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 36-45 (advanced-queries.Rmd) 
    Error: processing vignette 'advanced-queries.Rmd' failed with diagnostics:
    cannot open URL 'http://cdec.water.ca.gov/cgi-progs/querySHEF?station_id=kwk&sensor_num=25&dur_code=h&start_date=2018-10-04&end_date=2018-10-07&data_wish=Download+SHEF+Data+Now'
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘lazyeval’ ‘purrr’ ‘roxygen2’
      All declared Imports should be used.
    ```

# cellbaseR

Version: 1.4.0

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Attaching package: 'Biostrings'
    
    The following object is masked from 'package:DelayedArray':
    
        type
    
    The following object is masked from 'package:base':
    
        strsplit
    
    
    Attaching package: 'VariantAnnotation'
    
    The following object is masked from 'package:base':
    
        tabulate
    
    Quitting from lines 153-163 (cellbaseR.Rmd) 
    Error: processing vignette 'cellbaseR.Rmd' failed with diagnostics:
    error writing to connection
    Execution halted
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
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/cellscape/new/cellscape.Rcheck/00_pkg_src/cellscape/R/cellscape.R:1134)
    getMutOrder: no visible global function definition for ‘coef’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/cellscape/new/cellscape.Rcheck/00_pkg_src/cellscape/R/cellscape.R:1135)
    getTargetedHeatmapForEachSC: no visible binding for global variable
      ‘single_cell_id’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/cellscape/new/cellscape.Rcheck/00_pkg_src/cellscape/R/cellscape.R:1156)
    getTargetedHeatmapForEachSC: no visible binding for global variable
      ‘chr’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/cellscape/new/cellscape.Rcheck/00_pkg_src/cellscape/R/cellscape.R:1156)
    getTargetedHeatmapForEachSC: no visible binding for global variable
      ‘coord’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/cellscape/new/cellscape.Rcheck/00_pkg_src/cellscape/R/cellscape.R:1156)
    Undefined global functions or variables:
      chr chrom_index coef combn coord copy_number cumsum_values dist
      genotype hclust lm melt mode_cnv n n_gt na.omit px px_width sc_id
      setNames show_warnings single_cell_id site timepoint VAF
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

# childesr

Version: 0.1.0

## In both

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

Version: 1.6.2

## In both

*   checking package dependencies ... ERROR
    ```
    Packages required but not available: ‘org.Hs.eg.db’ ‘org.Mm.eg.db’
    
    Depends: includes the non-default packages:
      ‘Biostrings’ ‘GenomicRanges’ ‘IRanges’ ‘Gviz’ ‘S4Vectors’
      ‘ensembldb’ ‘AnnotationFilter’ ‘data.table’
    Adding so many packages to the search path is excessive and
    importing selectively is preferable.
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# ChIPexoQual

Version: 1.4.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Warning: Removed 14 rows containing non-finite values (stat_binhex).
    Warning: Removed 6 rows containing missing values (geom_hex).
    Warning: Removed 21 rows containing non-finite values (stat_binhex).
    Warning: Removed 1 rows containing missing values (geom_hex).
    convert: profile 'icc': 'RGB ': RGB color space not permitted on grayscale PNG `vignette_files/figure-html/param_dist-1.png' @ warning/png.c/MagickPNGWarningHandler/1656.
    convert: profile 'icc': 'RGB ': RGB color space not permitted on grayscale PNG `vignette_files/figure-html/subsamplots-1.png' @ warning/png.c/MagickPNGWarningHandler/1656.
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'vignette.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Failed with error:  'package 'DelayedArray' could not be loaded'
      Error in .requirePackage(package) : 
        unable to find required package 'ChIPexoQual'
      Calls: <Anonymous> ... getClass -> getClassDef -> .classEnv -> .requirePackage
      Execution halted
    ```

# ChIPseeker

Version: 1.16.1

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘TxDb.Hsapiens.UCSC.hg19.knownGene’
    
    Package suggested but not available for checking: ‘org.Hs.eg.db’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# chorrrds

Version: 0.1.7

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        data   5.0Mb
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
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/chromer/new/chromer.Rcheck/00_pkg_src/chromer/R/clean-data.R:77)
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
        eval, evalq, Filter, Find, get, grep, grepl, intersect,
        is.unsorted, lapply, lengths, Map, mapply, match, mget, order,
        paste, pmax, pmax.int, pmin, pmin.int, Position, rank, rbind,
        Reduce, rowMeans, rownames, rowSums, sapply, setdiff, sort,
        table, tapply, union, unique, unsplit, which, which.max,
        which.min
    
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
      installed size is 18.9Mb
      sub-directories of 1Mb or more:
        data  18.0Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/CINdex/new/CINdex.Rcheck/00_pkg_src/CINdex/R/process.probe.anno.R:21)
    process.probe.anno: no visible binding for global variable ‘ID’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/CINdex/new/CINdex.Rcheck/00_pkg_src/CINdex/R/process.probe.anno.R:31)
    process.reference.genome: no visible binding for global variable
      ‘chrom’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/CINdex/new/CINdex.Rcheck/00_pkg_src/CINdex/R/process.reference.genome.R:21-23)
    process.reference.genome: no visible binding for global variable ‘name’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/CINdex/new/CINdex.Rcheck/00_pkg_src/CINdex/R/process.reference.genome.R:21-23)
    process.reference.genome: no visible binding for global variable
      ‘stain’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/CINdex/new/CINdex.Rcheck/00_pkg_src/CINdex/R/process.reference.genome.R:21-23)
    run.cin.chr: no visible global function definition for ‘is’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/CINdex/new/CINdex.Rcheck/00_pkg_src/CINdex/R/run.cin.chr.R:45-64)
    run.cin.cyto: no visible global function definition for ‘is’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/CINdex/new/CINdex.Rcheck/00_pkg_src/CINdex/R/run.cin.cyto.R:53-84)
    Undefined global functions or variables:
      chrom dataMatrix ID is midpoint name stain
    Consider adding
      importFrom("methods", "is")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports
    field contains 'methods').
    ```

# circumplex

Version: 0.1.2

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
clang++ -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/circumplex/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/circumplex/RcppArmadillo/include" -I/usr/local/include  -fopenmp  -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c RcppExports.cpp -o RcppExports.o
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
clang++ -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/circumplex/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/circumplex/RcppArmadillo/include" -I/usr/local/include  -fopenmp  -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘circumplex’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/circumplex/old/circumplex.Rcheck/circumplex’

```
# civis

Version: 1.5.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.9Mb
      sub-directories of 1Mb or more:
        help   2.1Mb
        R      3.2Mb
    ```

# cleanNLP

Version: 2.0.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Unable to find any JVMs matching version "(null)".
    No Java runtime present, try --request to install.
    ```

# clustermq

Version: 0.8.5

## In both

*   R CMD check timed out
    

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘infuser’ ‘purrr’ ‘R6’
      All declared Imports should be used.
    ```

# clusterProfiler

Version: 3.8.1

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘GO.db’
    
    Packages suggested but not available for checking: ‘KEGG.db’ ‘org.Hs.eg.db’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
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
      installed size is  9.9Mb
      sub-directories of 1Mb or more:
        doc    3.4Mb
        libs   2.4Mb
        R      3.1Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    copyNumber,SingleBatchCopyNumber: no visible binding for global
      variable ‘theta.star’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/CNPBayes/new/CNPBayes.Rcheck/00_pkg_src/CNPBayes/R/copynumber-models.R:148-149)
    copyNumber,SingleBatchCopyNumber: no visible binding for global
      variable ‘theta.star’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/CNPBayes/new/CNPBayes.Rcheck/00_pkg_src/CNPBayes/R/copynumber-models.R:150-151)
    Undefined global functions or variables:
      theta.star
    ```

# coalitions

Version: 0.6.4

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(testthat)
      > library(checkmate)
      > library(coalitions)
      > 
      > test_check("coalitions")
      ── 1. Failure: Federal german scrapers work (@test-scrapers.R#44)  ─────────────
      Check on surveys_by isn't true.
      Must have exactly 6 rows, but has 7 rows
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 95 SKIPPED: 0 FAILED: 1
      1. Failure: Federal german scrapers work (@test-scrapers.R#44) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# cocktailApp

Version: 0.2.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 14661 marked UTF-8 strings
    ```

# codebook

Version: 0.6.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘graphics’ ‘pander’
      All declared Imports should be used.
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

# codified

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘methods’ ‘readr’
      All declared Imports should be used.
    ```

# codyn

Version: 2.0.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'codyn_overview.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# cofeatureR

Version: 1.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tibble’
      All declared Imports should be used.
    ```

# cogena

Version: 1.14.0

## In both

*   checking whether package ‘cogena’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘class::somgrid’ by ‘kohonen::somgrid’ when loading ‘cogena’
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/cogena/new/cogena.Rcheck/00install.out’ for details.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.5Mb
      sub-directories of 1Mb or more:
        doc       1.9Mb
        extdata   3.1Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘legend’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/cogena/new/cogena.Rcheck/00_pkg_src/cogena/R/heatmapCluster.R:151-153)
    heatmapCluster,cogena: no visible global function definition for
      ‘legend’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/cogena/new/cogena.Rcheck/00_pkg_src/cogena/R/heatmapCluster.R:155-157)
    heatmapCluster,cogena: no visible global function definition for
      ‘legend’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/cogena/new/cogena.Rcheck/00_pkg_src/cogena/R/heatmapCluster.R:159-160)
    Undefined global functions or variables:
      abline as.dist axis cor data density dist hist image layout legend
      lines median mtext order.dendrogram p.adjust par phyper plot.new
      rainbow rect reorder sd text title topo.colors
    Consider adding
      importFrom("graphics", "abline", "axis", "hist", "image", "layout",
                 "legend", "lines", "mtext", "par", "plot.new", "rect",
                 "text", "title")
      importFrom("grDevices", "rainbow", "topo.colors")
      importFrom("stats", "as.dist", "cor", "density", "dist", "median",
                 "order.dendrogram", "p.adjust", "phyper", "reorder", "sd")
      importFrom("utils", "data")
    to your NAMESPACE file.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘clValid’
    ```

# CollapsABEL

Version: 0.10.11

## In both

*   checking whether package ‘CollapsABEL’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/CollapsABEL/new/CollapsABEL.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘CollapsABEL’ ...
** package ‘CollapsABEL’ successfully unpacked and MD5 sums checked
** R
** byte-compile and prepare package for lazy loading
Unable to find any JVMs matching version "(null)".
No Java runtime present, try --request to install.
Warning in system("/usr/libexec/java_home", intern = TRUE) :
  running command '/usr/libexec/java_home' had status 1
Error: package or namespace load failed for ‘rJava’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/CollapsABEL/rJava/libs/rJava.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/CollapsABEL/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/CollapsABEL/rJava/libs/rJava.so
  Reason: image not found
Error : package ‘rJava’ could not be loaded
ERROR: lazy loading failed for package ‘CollapsABEL’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/CollapsABEL/new/CollapsABEL.Rcheck/CollapsABEL’

```
### CRAN

```
* installing *source* package ‘CollapsABEL’ ...
** package ‘CollapsABEL’ successfully unpacked and MD5 sums checked
** R
** byte-compile and prepare package for lazy loading
Unable to find any JVMs matching version "(null)".
No Java runtime present, try --request to install.
Warning in system("/usr/libexec/java_home", intern = TRUE) :
  running command '/usr/libexec/java_home' had status 1
Error: package or namespace load failed for ‘rJava’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/CollapsABEL/rJava/libs/rJava.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/CollapsABEL/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/CollapsABEL/rJava/libs/rJava.so
  Reason: image not found
Error : package ‘rJava’ could not be loaded
ERROR: lazy loading failed for package ‘CollapsABEL’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/CollapsABEL/old/CollapsABEL.Rcheck/CollapsABEL’

```
# collapsibleTree

Version: 0.1.7

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      ‘knitr’ ‘shiny’
    ```

# colorednoise

Version: 1.0.3

## In both

*   checking whether package ‘colorednoise’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/colorednoise/new/colorednoise.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘colorednoise’ ...
** package ‘colorednoise’ successfully unpacked and MD5 sums checked
** libs
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/colorednoise/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/colorednoise/RcppArmadillo/include" -I/usr/local/include  -fopenmp -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘colorednoise’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/colorednoise/new/colorednoise.Rcheck/colorednoise’

```
### CRAN

```
* installing *source* package ‘colorednoise’ ...
** package ‘colorednoise’ successfully unpacked and MD5 sums checked
** libs
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/colorednoise/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/colorednoise/RcppArmadillo/include" -I/usr/local/include  -fopenmp -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘colorednoise’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/colorednoise/old/colorednoise.Rcheck/colorednoise’

```
# compareDF

Version: 1.5.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘stringr’
      All declared Imports should be used.
    ```

# COMPASS

Version: 1.18.0

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
      Count id population
    ```

*   checking for unstated dependencies in vignettes ... NOTE
    ```
    'library' or 'require' calls not declared from:
      ‘ggplot2’ ‘readxl’
    ```

# comperes

Version: 0.2.0

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      The following object is masked from 'package:stats':
      
          filter
      
      > 
      > test_check("comperes")
      ── 1. Failure: h2h_funs can be used with !!! (@test-head-to-head.R#216)  ───────
      `... <- NULL` produced warnings.
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 265 SKIPPED: 0 FAILED: 1
      1. Failure: h2h_funs can be used with !!! (@test-head-to-head.R#216) 
      
      Error: testthat unit tests failed
      Execution halted
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
      2: require_xlsx() at /Users/lionel/Desktop/rlang/revdep/checks.noindex/condformat/new/condformat.Rcheck/00_pkg_src/condformat/R/condformat_render.R:111
      3: stop("Please install the xlsx package in order to export to excel") at /Users/lionel/Desktop/rlang/revdep/checks.noindex/condformat/new/condformat.Rcheck/00_pkg_src/condformat/R/condformat_render.R:88
      
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

# conflicted

Version: 1.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘memoise’
      All declared Imports should be used.
    ```

# congressbr

Version: 0.1.3

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘congressbr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: sen_bills
    > ### Title: Downloads and tidies information on the legislation in the
    > ###   Federal Senate
    > ### Aliases: sen_bills
    > 
    > ### ** Examples
    > 
    > pls_5_2010 <- sen_bills(type = "PLS", number = 5, year = 2010)
    Error: Column `bill_indexing` must be a 1d atomic vector or a list
    Execution halted
    ```

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

# countytimezones

Version: 1.0.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    
    Attaching package: 'lubridate'
    
    The following object is masked from 'package:base':
    
        date
    
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'countytimezones.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# countyweather

Version: 0.1.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    
    Attaching package: 'lubridate'
    
    The following object is masked from 'package:base':
    
        date
    
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'countyweather.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

# cowplot

Version: 0.9.3

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘magick’
    ```

# coxed

Version: 0.2.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘mediation’
    ```

# CRANsearcher

Version: 1.0.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 11 marked Latin-1 strings
      Note: found 57 marked UTF-8 strings
    ```

# crawl

Version: 2.2.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘gdistance’ ‘raster’
      All declared Imports should be used.
    ```

# CrossClustering

Version: 4.0.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘glue’
      All declared Imports should be used.
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

# crsra

Version: 0.2.3

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 500 marked UTF-8 strings
    ```

# crypto

Version: 1.0.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘yaml’
      All declared Imports should be used.
    ```

# curatedMetagenomicData

Version: 1.10.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.7Mb
      sub-directories of 1Mb or more:
        doc    1.4Mb
        help   2.7Mb
    ```

# d3r

Version: 0.8.3

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

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        doc                  1.1Mb
        Pied_Flycatchers_1   2.4Mb
        Pied_Flycatchers_2   1.2Mb
    ```

# DAPAR

Version: 1.12.11

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
than is installed on your system (0.12.19). This might lead to errors
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
than is installed on your system (0.12.19). This might lead to errors
when loading mzR. If you encounter such issues, please send a report,
including the output of sessionInfo() to the Bioc support forum at 
https://support.bioconductor.org/. For details see also
https://github.com/sneumann/mzR/wiki/mzR-Rcpp-compiler-linker-issue.
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘DO.db’
ERROR: lazy loading failed for package ‘DAPAR’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/DAPAR/old/DAPAR.Rcheck/DAPAR’

```
# dartR

Version: 1.0.5

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DBI’ ‘Demerelate’ ‘misc3d’ ‘plotly’ ‘quadprog’ ‘rgl’
      All declared Imports should be used.
    ```

# datadr

Version: 0.8.6.1

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘Rhipe’
    ```

# datastructures

Version: 0.2.7

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 13.0Mb
      sub-directories of 1Mb or more:
        doc    1.2Mb
        libs  10.8Mb
    ```

# datasus

Version: 0.4.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘RCurl’
      All declared Imports should be used.
    ```

# DataVisualizations

Version: 1.1.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 60-74 (DataVisualizations.Rmd) 
    Error: processing vignette 'DataVisualizations.Rmd' failed with diagnostics:
    package 'sm' required by 'vioplot' could not be found
    Execution halted
    ```

# DChIPRep

Version: 1.10.0

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
# ddpcr

Version: 1.9

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        sample_data   3.0Mb
    ```

# DeclareDesign

Version: 0.10.0

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
      ── 1. Failure: error if you try to draw POs at a level using a variable that doe
      `my_potential_outcomes_formula(pop)` did not throw an error.
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 436 SKIPPED: 1 FAILED: 1
      1. Failure: error if you try to draw POs at a level using a variable that doesn't exist at that level (@test-potential-outcomes.R#160) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘methods’
      All declared Imports should be used.
    ```

# DeepBlueR

Version: 1.6.0

## In both

*   R CMD check timed out
    

*   checking Rd files ... NOTE
    ```
    prepare_Rd: deepblue_enrich_regions_fast.Rd:35-38: Dropping empty section \examples
    ```

# DEGreport

Version: 1.16.0

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
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/DEGreport/new/DEGreport.Rcheck/00_pkg_src/DEGreport/R/methods.R:274-282)
    degMV: no visible binding for global variable ‘max_sd’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/DEGreport/new/DEGreport.Rcheck/00_pkg_src/DEGreport/R/methods.R:274-282)
    degPatterns: no visible global function definition for ‘rowMedians’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/DEGreport/new/DEGreport.Rcheck/00_pkg_src/DEGreport/R/clustering.R:785-787)
    degPatterns: no visible binding for global variable ‘genes’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/DEGreport/new/DEGreport.Rcheck/00_pkg_src/DEGreport/R/clustering.R:816-821)
    degPlotWide : <anonymous>: no visible binding for global variable
      ‘count’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/DEGreport/new/DEGreport.Rcheck/00_pkg_src/DEGreport/R/genePlots.R:155-158)
    significants,list : <anonymous>: no visible binding for global variable
      ‘gene’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/DEGreport/new/DEGreport.Rcheck/00_pkg_src/DEGreport/R/AllMethods.R:225)
    significants,TopTags: no visible binding for global variable ‘FDR’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/DEGreport/new/DEGreport.Rcheck/00_pkg_src/DEGreport/R/AllMethods.R:147-151)
    significants,TopTags: no visible binding for global variable ‘logFC’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/DEGreport/new/DEGreport.Rcheck/00_pkg_src/DEGreport/R/AllMethods.R:147-151)
    Undefined global functions or variables:
      .x base_mean comp compare count counts covar enrichGO FDR gene genes
      keys log2fc log2FoldChange logFC max_sd min_median ratios rowMedians
      simplify
    ```

# DeLorean

Version: 1.4.1

## Newly broken

*   R CMD check timed out
    

# dendroTools

Version: 1.0.0

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
Unable to find any JVMs matching version "(null)".
No Java runtime present, try --request to install.
Warning in system("/usr/libexec/java_home", intern = TRUE) :
  running command '/usr/libexec/java_home' had status 1
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/dendroTools/rJava/libs/rJava.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/dendroTools/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/dendroTools/rJava/libs/rJava.so
  Reason: image not found
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
Unable to find any JVMs matching version "(null)".
No Java runtime present, try --request to install.
Warning in system("/usr/libexec/java_home", intern = TRUE) :
  running command '/usr/libexec/java_home' had status 1
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/dendroTools/rJava/libs/rJava.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/dendroTools/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/dendroTools/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘dendroTools’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/dendroTools/old/dendroTools.Rcheck/dendroTools’

```
# DEP

Version: 1.2.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        data   1.4Mb
        doc    3.1Mb
        R      1.2Mb
    ```

# DepthProc

Version: 2.0.4

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘sm’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
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

# destiny

Version: 2.10.2

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
      installed size is  7.6Mb
      sub-directories of 1Mb or more:
        doc   4.3Mb
        R     2.0Mb
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

Version: 0.5.1

## In both

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# dexter

Version: 0.8.1

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    $items
     [1] "S1DoCurse"   "S1DoScold"   "S1DoShout"   "S1WantCurse" "S1WantScold"
     [6] "S1WantShout" "S2DoCurse"   "S2DoScold"   "S2DoShout"   "S2WantCurse"
    [11] "S2WantScold" "S2WantShout" "S3DoCurse"   "S3DoScold"   "S3DoShout"  
    [16] "S3WantCurse" "S3WantScold" "S3WantShout" "S4DoCurse"   "S4DoScold"  
    [21] "S4DoShout"   "S4WantCurse" "S4WantScold" "S4WantShout"
    
    $person_properties
    [1] "gender"
    
    $columns_ignored
    [1] "anger"
    
    > add_item_properties(db, verbAggrProperties)
    4 item properties for 24 items added or updated
    > 
    > f=fit_enorm(db)
    Error in fit_enorm_(dataSrc, qtpredicate = qtpredicate, fixed_params = fixed_params,  : 
      One or more items are without score variation
    Calls: fit_enorm -> fit_enorm_
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
             nIterations = nIterations, env = caller_env()) at /Users/lionel/Desktop/rlang/revdep/checks.noindex/dexter/new/dexter.Rcheck/00_pkg_src/dexter/R/enorm.R:34
      3: stop("One or more items are without score variation") at /Users/lionel/Desktop/rlang/revdep/checks.noindex/dexter/new/dexter.Rcheck/00_pkg_src/dexter/R/enorm.R:115
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 33 SKIPPED: 4 FAILED: 5
      1. Error: inconsistencies between data and parms are handled correctly (@test_ability.R#13) 
      2. Error: verbAgg abilities are monotone increasing (@test_ability.R#31) 
      3. Error: calibration of verbal aggression dataset matches oplm results, with fixed and unfixed (@test_enorm.R#16) 
      4. Error: populations work (@test_plausible_values.R#7) 
      5. Error: profile analysis verb agg (@test_profiles.R#25) 
      
      Error: testthat unit tests failed
      In addition: Warning message:
      call dbDisconnect() when finished working with a connection 
      Execution halted
    ```

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
    Quitting from lines 132-134 (dexter.Rmd) 
    Error: processing vignette 'dexter.Rmd' failed with diagnostics:
    vector size cannot be NA/NaN
    Execution halted
    ```

# dextergui

Version: 0.1.4

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Error in re-building vignettes:
      ...
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      The vignette engine knitr::rmarkdown is not available, because the rmarkdown package is not installed. Please install it.
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Loading required package: RSQLite
    no column `person_id` provided, automatically generating unique person id's
    Quitting from lines 151-154 (dextergui.Rmd) 
    Error: processing vignette 'dextergui.Rmd' failed with diagnostics:
    vector size cannot be NA/NaN
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Unexported objects imported by ':::' calls:
      ‘dexter:::get_resp_data’ ‘dexter:::qcolors’
      See the note in ?`:::` about the use of this operator.
    ```

# dexterMST

Version: 0.1.0

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ======
      4 item properties for 24 items added or updated
      ── 1. Error: can import from dexter and calbration comparable to dexter (@test_i
      One or more items are without score variation
      1: fit_enorm(dxdb) at testthat/test_inputs.R:67
      2: fit_enorm_(dataSrc, qtpredicate = qtpredicate, fixed_params = fixed_params, method = method, 
             nIterations = nIterations, env = caller_env())
      3: stop("One or more items are without score variation")
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 10 SKIPPED: 0 FAILED: 1
      1. Error: can import from dexter and calbration comparable to dexter (@test_inputs.R#67) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      The vignette engine knitr::rmarkdown is not available, because the rmarkdown package is not installed. Please install it.
    Quitting from lines 364-400 (multistage_fundamentals.Rmd) 
    Error: processing vignette 'multistage_fundamentals.Rmd' failed with diagnostics:
    One or more items are without score variation
    Execution halted
    ```

# dggridR

Version: 2.0.3

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
     Linking to sp version: 1.3-1 
    Loading required package: ggplot2
    Loading required package: dplyr
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Regions defined for each Polygons
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'dggridR.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
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

# diceR

Version: 0.5.1

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘blockcluster’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
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
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c RcppExports.cpp -o RcppExports.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c bamReader.cpp -o bamReader.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c bedReader.cpp -o bedReader.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c bitBucket.cpp -o bitBucket.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c croi_func.cpp -o croi_func.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c croi_main.cpp -o croi_main.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c densitySet.cpp -o densitySet.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c iBucket.cpp -o iBucket.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c interval.cpp -o interval.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c intervalDensity.cpp -o intervalDensity.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c intervalNode.cpp -o intervalNode.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c intervalSet.cpp -o intervalSet.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c intervalTree.cpp -o intervalTree.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c merge.cpp -o merge.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c mergeOne.c -o mergeOne.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c nodeGroup.cpp -o nodeGroup.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c peakOrder.cpp -o peakOrder.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c reader.cpp -o reader.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c sequence.cpp -o sequence.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c util.cpp -o util.o
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
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c RcppExports.cpp -o RcppExports.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c bamReader.cpp -o bamReader.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c bedReader.cpp -o bedReader.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c bitBucket.cpp -o bitBucket.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c croi_func.cpp -o croi_func.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c croi_main.cpp -o croi_main.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c densitySet.cpp -o densitySet.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c iBucket.cpp -o iBucket.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c interval.cpp -o interval.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c intervalDensity.cpp -o intervalDensity.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c intervalNode.cpp -o intervalNode.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c intervalSet.cpp -o intervalSet.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c intervalTree.cpp -o intervalTree.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c merge.cpp -o merge.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c mergeOne.c -o mergeOne.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c nodeGroup.cpp -o nodeGroup.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c peakOrder.cpp -o peakOrder.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c reader.cpp -o reader.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c sequence.cpp -o sequence.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -D_USE_KNETFILE -DBGZF_CACHE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rsamtools/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/DiffBind/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c util.cpp -o util.o
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

Version: 1.0.10

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 127-144 (diffcyt_workflow.Rmd) 
    Error: processing vignette 'diffcyt_workflow.Rmd' failed with diagnostics:
    there is no package called 'HDCytoData'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘HDCytoData’
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/diffcyt/new/diffcyt.Rcheck/00_pkg_src/diffcyt/R/calcMedians.R:133-136)
    calcMediansByClusterMarker: no visible binding for global variable
      ‘cluster_id’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/diffcyt/new/diffcyt.Rcheck/00_pkg_src/diffcyt/R/calcMediansByClusterMarker.R:123-126)
    calcMediansByClusterMarker: no visible binding for global variable
      ‘marker’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/diffcyt/new/diffcyt.Rcheck/00_pkg_src/diffcyt/R/calcMediansByClusterMarker.R:123-126)
    calcMediansByClusterMarker: no visible binding for global variable
      ‘value’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/diffcyt/new/diffcyt.Rcheck/00_pkg_src/diffcyt/R/calcMediansByClusterMarker.R:123-126)
    calcMediansBySampleMarker: no visible binding for global variable
      ‘sample_id’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/diffcyt/new/diffcyt.Rcheck/00_pkg_src/diffcyt/R/calcMediansBySampleMarker.R:119-122)
    calcMediansBySampleMarker: no visible binding for global variable
      ‘marker’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/diffcyt/new/diffcyt.Rcheck/00_pkg_src/diffcyt/R/calcMediansBySampleMarker.R:119-122)
    calcMediansBySampleMarker: no visible binding for global variable
      ‘value’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/diffcyt/new/diffcyt.Rcheck/00_pkg_src/diffcyt/R/calcMediansBySampleMarker.R:119-122)
    Undefined global functions or variables:
      cluster_id marker sample_id value
    ```

# diffloop

Version: 1.8.0

## In both

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .travis.yml
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

# DisImpact

Version: 0.0.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘magrittr’
      All declared Imports should be used.
    ```

# disto

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘proxy’
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
Unable to find any JVMs matching version "(null)".
No Java runtime present, try --request to install.
Warning in system("/usr/libexec/java_home", intern = TRUE) :
  running command '/usr/libexec/java_home' had status 1
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/DiversityOccupancy/rJava/libs/rJava.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/DiversityOccupancy/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/DiversityOccupancy/rJava/libs/rJava.so
  Reason: image not found
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
Unable to find any JVMs matching version "(null)".
No Java runtime present, try --request to install.
Warning in system("/usr/libexec/java_home", intern = TRUE) :
  running command '/usr/libexec/java_home' had status 1
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/DiversityOccupancy/rJava/libs/rJava.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/DiversityOccupancy/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/DiversityOccupancy/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘DiversityOccupancy’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/DiversityOccupancy/old/DiversityOccupancy.Rcheck/DiversityOccupancy’

```
# DLMtool

Version: 5.2.3

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 10.2Mb
      sub-directories of 1Mb or more:
        data   2.1Mb
        libs   1.1Mb
        R      6.1Mb
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

Version: 0.0.3

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    ```

# dotwhisker

Version: 0.5.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Loading required package: ggplot2
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'dotwhisker-vignette.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# dplyr

Version: 0.7.6

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ── 2. Failure: funs() gives a clear error message (#3368) (@test-funs.R#36)  ───
      `funs(~mp[.])` threw an error with unexpected message.
      Expected match: "`~mp[.]` must be a function name (quoted or unquoted) or an unquoted call, not `~`"
      Actual message: "`~mp[.]` must be a function name (quoted or unquoted) or an unquoted call, not ``~``"
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 2734 SKIPPED: 6 FAILED: 2
      1. Failure: funs() gives a clear error message (#3368) (@test-funs.R#28) 
      2. Failure: funs() gives a clear error message (#3368) (@test-funs.R#36) 
      
      Error: testthat unit tests failed
      In addition: Warning message:
      call dbDisconnect() when finished working with a connection 
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
    Warning: Removed 1 rows containing non-finite values (stat_smooth).
    Warning: Removed 1 rows containing missing values (geom_point).
    Quitting from lines 651-653 (programming.Rmd) 
    Error: processing vignette 'programming.Rmd' failed with diagnostics:
    `UQE()` is defunct. Please use `!!get_expr(x)`
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.2Mb
      sub-directories of 1Mb or more:
        libs   4.4Mb
        R      2.0Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4 marked UTF-8 strings
    ```

# dplyr.teradata

Version: 0.3.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘bit64’ ‘rstudioapi’
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

Version: 1.1.0

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(driftR)
      > 
      > test_check("driftR")
      ── 1. Error: (unknown) (@test_read.R#36)  ──────────────────────────────────────
      Instrument value "Sonde" not acceptable - value should be one of Sonde, EXO, or HOBO
      1: dr_read(system.file("extdata", "rawData.csv", package = "driftR"), instrument = "Sonde", 
             defineVar = TRUE, cleanVar = FALSE) at testthat/test_read.R:36
      2: stop(glue::glue("Instrument value {instrument} not acceptable - value should be one of Sonde, EXO, or HOBO")) at /Users/lionel/Desktop/rlang/revdep/checks.noindex/driftR/new/driftR.Rcheck/00_pkg_src/driftR/R/dr_read.R:73
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 199 SKIPPED: 0 FAILED: 1
      1. Error: (unknown) (@test_read.R#36) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# DSAIDE

Version: 0.7.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'DSAIDE.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        media       2.2Mb
        shinyapps   2.6Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘knitr’ ‘rmarkdown’ ‘utils’
      All declared Imports should be used.
    ```

# DSAIRM

Version: 0.4.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Loading required package: shiny
    Welcome to the DSAIRM package. Type dsairmmenu() to get started.
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'DSAIRM.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘knitr’ ‘rmarkdown’
      All declared Imports should be used.
    ```

# dsr

Version: 0.1.0

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > #US standard population
    > df_ref  <- data.frame(age=c('00-14','15-24','25-44','45-64','65+'),
    +                      pop=c(23961000,15420000,21353000,19601000,10685000))
    > 
    > #Directly Standardized Rate Ratio (per 1000) - 95% log-normal CI's, Alaska as the refernce
    > my_results2 <- dsrr(data=df_study,
    +                    event=deaths,
    +                    fu=fu,
    +                    subgroup=state,
    +                    age,
    +                    refdata=df_ref,
    +                    refgroup="Alaska",
    +                    estimate="ratio",
    +                    sig=0.95,
    +                    mp=1000,
    +                    decimals=4)
    Joining, by = "age"
    Error in mutate_impl(.data, dots) : 
      Column `ratio` must be length 1 (the group size), not 0
    Calls: dsrr ... <Anonymous> -> mutate -> mutate.tbl_df -> mutate_impl
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 100-113 (dsr.Rmd) 
    Error: processing vignette 'dsr.Rmd' failed with diagnostics:
    Column `ratio` must be length 1 (the group size), not 0
    Execution halted
    ```

# dtwclust

Version: 5.5.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.2Mb
      sub-directories of 1Mb or more:
        doc    2.5Mb
        libs   1.1Mb
        R      2.0Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# duawranglr

Version: 0.6.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘digest’ ‘dplyr’
      All declared Imports should be used.
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
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/dynfrail/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/dynfrail/RcppArmadillo/include" -I/usr/local/include  -fopenmp  -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c RcppExports.cpp -o RcppExports.o
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
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/dynfrail/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/dynfrail/RcppArmadillo/include" -I/usr/local/include  -fopenmp  -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘dynfrail’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/dynfrail/old/dynfrail.Rcheck/dynfrail’

```
# dynutils

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘processx’ ‘Rcpp’
      All declared Imports should be used.
    ```

# echarts4r

Version: 0.1.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.3Mb
      sub-directories of 1Mb or more:
        assets        3.4Mb
        htmlwidgets   3.0Mb
    ```

# ecoengine

Version: 1.11.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘magrittr’
      All declared Imports should be used.
    ```

# econet

Version: 0.1.7

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tnet’
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
      installed size is  7.1Mb
      sub-directories of 1Mb or more:
        data   5.6Mb
        doc    1.2Mb
    ```

# eemR

Version: 0.1.5

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
    
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'introduction.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# eesim

Version: 0.1.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
        intersect, setdiff, setequal, union
    
    
    Attaching package: 'gridExtra'
    
    The following object is masked from 'package:dplyr':
    
        combine
    
    This is dlnm 2.3.6. For details: help(dlnm) and vignette('dlnmOverview').
    This function may take a minute or two to run, especially if you
    are creating lots of replications (`n_reps`).
    This function may take a minute or two to run, especially if you
    are creating lots of replications (`n_reps`).
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'eesim.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# EFDR

Version: 0.1.1

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/EFDR/new/EFDR.Rcheck/00_pkg_src/EFDR/R/EFDR_functions.R:686)
    .relist.dwt: no visible global function definition for ‘as’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/EFDR/new/EFDR.Rcheck/00_pkg_src/EFDR/R/EFDR_functions.R:686)
    .std.wav.coeff : <anonymous>: no visible global function definition for
      ‘mad’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/EFDR/new/EFDR.Rcheck/00_pkg_src/EFDR/R/EFDR_functions.R:698)
    regrid: no visible global function definition for ‘predict’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/EFDR/new/EFDR.Rcheck/00_pkg_src/EFDR/R/EFDR_functions.R:391-396)
    regrid: no visible global function definition for ‘var’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/EFDR/new/EFDR.Rcheck/00_pkg_src/EFDR/R/EFDR_functions.R:406)
    regrid: no visible global function definition for ‘medpolish’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/EFDR/new/EFDR.Rcheck/00_pkg_src/EFDR/R/EFDR_functions.R:427)
    Undefined global functions or variables:
      as mad medpolish pnorm predict relist rnorm var
    Consider adding
      importFrom("methods", "as")
      importFrom("stats", "mad", "medpolish", "pnorm", "predict", "rnorm",
                 "var")
      importFrom("utils", "relist")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports
    field contains 'methods').
    ```

# ELMER

Version: 2.4.4

## In both

*   R CMD check timed out
    

*   checking dependencies in R code ... WARNING
    ```
    '::' or ':::' import not declared from: 'data.table'
    ':::' call which should be '::': 'TCGAbiolinks:::TCGAVisualize_volcano'
      See the note in ?`:::` about the use of this operator.
    Unexported objects imported by ':::' calls:
      'TCGAbiolinks:::colDataPrepare' 'TCGAbiolinks:::get.GRCh.bioMart'
      See the note in ?`:::` about the use of this operator.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 45.1Mb
      sub-directories of 1Mb or more:
        doc  44.1Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/ELMER/new/ELMER.Rcheck/00_pkg_src/ELMER/R/motif.TF.Plots.R:148-157)
    scatter: no visible binding for global variable 'value'
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/ELMER/new/ELMER.Rcheck/00_pkg_src/ELMER/R/Scatter.plot.R:217-231)
    scatter: no visible global function definition for 'cor.test'
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/ELMER/new/ELMER.Rcheck/00_pkg_src/ELMER/R/Scatter.plot.R:236-238)
    scatter: no visible binding for global variable 'mae'
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/ELMER/new/ELMER.Rcheck/00_pkg_src/ELMER/R/Scatter.plot.R:250-267)
    TF.rank.plot: no visible binding for global variable 'pvalue'
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/ELMER/new/ELMER.Rcheck/00_pkg_src/ELMER/R/motif.TF.Plots.R:291-304)
    TF.rank.plot: no visible binding for global variable 'label'
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/ELMER/new/ELMER.Rcheck/00_pkg_src/ELMER/R/motif.TF.Plots.R:291-304)
    TF.rank.plot: no visible binding for global variable 'Gene'
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/ELMER/new/ELMER.Rcheck/00_pkg_src/ELMER/R/motif.TF.Plots.R:308-320)
    Undefined global functions or variables:
      cor.test fisher.test Gene GeneID gr hm450.hg38.manifest Hugo_Symbol
      label lowerOR mae motif OR precede Probe pvalue subsetByOverlaps
      Target TF upperOR value write.table x y z
    Consider adding
      importFrom("stats", "cor.test", "fisher.test")
      importFrom("utils", "write.table")
    to your NAMESPACE file.
    ```

# ELMER.data

Version: 2.4.2

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
    
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'vignettes.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

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
      Calls: <Anonymous> ... .extendsForS3 -> extends -> getClassDef -> .requirePackage
      Execution halted
    ```

# emuR

Version: 1.0.0

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

# ENCODExplorer

Version: 2.6.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 74.0Mb
      sub-directories of 1Mb or more:
        data     24.1Mb
        doc       1.5Mb
        extdata  48.0Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    step6_target: no visible binding for global variable ‘target’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/ENCODExplorer/new/ENCODExplorer.Rcheck/00_pkg_src/ENCODExplorer/R/prepare_data.R:354-355)
    step7: no visible binding for global variable ‘organism’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/ENCODExplorer/new/ENCODExplorer.Rcheck/00_pkg_src/ENCODExplorer/R/prepare_data.R:424-425)
    step8: no visible binding for global variable ‘investigated_as’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/ENCODExplorer/new/ENCODExplorer.Rcheck/00_pkg_src/ENCODExplorer/R/prepare_data.R:436-437)
    step8: no visible binding for global variable ‘target’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/ENCODExplorer/new/ENCODExplorer.Rcheck/00_pkg_src/ENCODExplorer/R/prepare_data.R:439-440)
    step9: no visible binding for global variable ‘organism’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/ENCODExplorer/new/ENCODExplorer.Rcheck/00_pkg_src/ENCODExplorer/R/prepare_data.R:449-450)
    Undefined global functions or variables:
      . accession antibody_caption antibody_characterization
      antibody_target assay biological_replicate_number biosample_name
      biosample_type col_name controls data date_released download.file
      encode_df Experiment file_accession file_format href investigated_as
      lab nucleic_acid_term organism platform project replicate_antibody
      replicate_library server status submitted_by target
      technical_replicate_number treatment ui value Value
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

# epiphy

Version: 0.3.4

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'defs-and-eqns.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
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
      installed size is  7.2Mb
      sub-directories of 1Mb or more:
        doc   1.7Mb
        R     4.0Mb
    ```

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘tergm’, ‘ergm.count’, ‘networkDynamic’
    ```

# esc

Version: 0.4.1

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘metafor’
    ```

# estimatr

Version: 0.12

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      ‘broom’ ‘texreg’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.7Mb
      sub-directories of 1Mb or more:
        libs   5.0Mb
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘texreg’
    ```

# eurostat

Version: 3.2.9

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 596 marked UTF-8 strings
    ```

# evaluator

Version: 0.3.0

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── 1. Failure: Simulation summary (@test-summarize.R#6)  ───────────────────────
      as.data.frame(dat) not equivalent to as.data.frame(scenario_summary).
      Component "mean_diff_exceedance": Mean relative difference: 0.8232393
      
      ── 2. Failure: Domain summary (@test-summarize.R#21)  ──────────────────────────
      summarize_domains(simulation_results) not equivalent to `domain_summary`.
      Rows in x but not y: 14, 13, 11, 10, 9, 8, 7, 6, 5, 12, 4[...]. Rows in y but not x: 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4[...]. 
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 85 SKIPPED: 5 FAILED: 2
      1. Failure: Simulation summary (@test-summarize.R#6) 
      2. Failure: Domain summary (@test-summarize.R#21) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# EventStudy

Version: 0.34

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.1Mb
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

Version: 0.2.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 13.0Mb
      sub-directories of 1Mb or more:
        exiftool  12.8Mb
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

# eyetrackingR

Version: 0.1.7

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
      ...
    Warning: Removed 37 rows containing non-finite values (stat_summary).
    Warning: Removed 37 rows containing non-finite values (stat_summary).
    Warning: Removed 37 rows containing non-finite values (stat_summary).
    Warning: Removed 37 rows containing non-finite values (stat_summary).
    Warning: Removed 37 rows containing non-finite values (stat_summary).
    Warning: Removed 37 rows containing non-finite values (stat_summary).
    Warning: Removed 37 rows containing non-finite values (stat_summary).
    Warning: Removed 37 rows containing non-finite values (stat_summary).
    Warning in make_onset_data(response_window_clean, onset_time = 15500, fixation_window_length = 1,  :
      Very few trials have a legitimate first AOI! Possible incorrect onset time?
    Warning in max(SwitchAOI) :
      no non-missing arguments to max; returning -Inf
    Warning in min(SwitchAOI) :
      no non-missing arguments to min; returning Inf
    Warning in max(df_plot$.Time) :
      no non-missing arguments to max; returning -Inf
    Quitting from lines 91-93 (onset_contingent_analysis_vignette.Rmd) 
    Error: processing vignette 'onset_contingent_analysis_vignette.Rmd' failed with diagnostics:
    replacement has 1 row, data has 0
    Execution halted
    ```

# facerec

Version: 0.1.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘magick’
    ```

# factoextra

Version: 1.0.5

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘NbClust’
    ```

# factorMerger

Version: 0.3.6

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘forcats’ ‘formula.tools’
      All declared Imports should be used.
    ```

# fastLink

Version: 0.4.0

## In both

*   checking whether package ‘fastLink’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/fastLink/new/fastLink.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘fastLink’ ...
** package ‘fastLink’ successfully unpacked and MD5 sums checked
** libs
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/fastLink/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/fastLink/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/fastLink/RcppEigen/include" -I/usr/local/include  -fopenmp  -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘fastLink’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/fastLink/new/fastLink.Rcheck/fastLink’

```
### CRAN

```
* installing *source* package ‘fastLink’ ...
** package ‘fastLink’ successfully unpacked and MD5 sums checked
** libs
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/fastLink/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/fastLink/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/fastLink/RcppEigen/include" -I/usr/local/include  -fopenmp  -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘fastLink’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/fastLink/old/fastLink.Rcheck/fastLink’

```
# fastR2

Version: 1.2.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.0Mb
      sub-directories of 1Mb or more:
        snippet   3.7Mb
    ```

# febr

Version: 1.0-0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'febr.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unable to find any JVMs matching version "(null)".
    No Java runtime present, try --request to install.
    Namespaces in Imports field not imported from:
      ‘cellranger’ ‘knitr’
      All declared Imports should be used.
    ```

# fiftystater

Version: 1.0.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 83-87 (fiftystater.Rmd) 
    Error: processing vignette 'fiftystater.Rmd' failed with diagnostics:
    unused argument (output)
    Execution halted
    ```

# filesstrings

Version: 2.7.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘strex’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘dplyr’
    ```

# FindMyFriends

Version: 1.10.0

## In both

*   R CMD check timed out
    

*   checking whether package ‘FindMyFriends’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      ./cdhit-common.h:536:3: warning: 'long long' is a C++11 extension [-Wc++11-long-long]
      ./cdhit-common.h:537:3: warning: 'long long' is a C++11 extension [-Wc++11-long-long]
      cdhit-common.cpp:1785:3: warning: 'long long' is a C++11 extension [-Wc++11-long-long]
      cdhit-common.cpp:1909:9: warning: 'long long' is a C++11 extension [-Wc++11-long-long]
      cdhit-common.cpp:1910:37: warning: 'long long' is a C++11 extension [-Wc++11-long-long]
      cdhit-common.cpp:1945:12: warning: 'long long' is a C++11 extension [-Wc++11-long-long]
      cdhit-common.cpp:1946:40: warning: 'long long' is a C++11 extension [-Wc++11-long-long]
      cdhit-common.cpp:1964:9: warning: 'long long' is a C++11 extension [-Wc++11-long-long]
      cdhit-common.cpp:1965:37: warning: 'long long' is a C++11 extension [-Wc++11-long-long]
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/FindMyFriends/new/FindMyFriends.Rcheck/00install.out’ for details.
    ```

*   checking for code/documentation mismatches ... WARNING
    ```
    Functions or methods with usage in documentation object 'pgVirtual-class' but not in code:
      as
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.8Mb
      sub-directories of 1Mb or more:
        doc       1.5Mb
        extdata   1.8Mb
        libs      1.2Mb
        R         2.1Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported objects imported by ':::' calls:
      ‘gtable:::insert.unit’ ‘gtable:::z_arrange_gtables’
      See the note in ?`:::` about the use of this operator.
    ```

# fingertipscharts

Version: 0.0.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘mapproj’
      All declared Imports should be used.
    ```

# fingertipsR

Version: 0.1.9

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘purrr’
      All declared Imports should be used.
    ```

# fitteR

Version: 0.1.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘ExtDist’
    ```

# flora

Version: 0.3.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.3Mb
      sub-directories of 1Mb or more:
        R   7.2Mb
    ```

# flowWorkspace

Version: 3.28.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 17.4Mb
      sub-directories of 1Mb or more:
        doc    1.4Mb
        lib    8.2Mb
        libs   5.4Mb
        R      2.0Mb
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Versioned 'LinkingTo' values for
      ‘BH’ ‘cytolib’
    are only usable in R >= 3.0.2
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘graphics’ ‘grDevices’ ‘RBGL’
      All declared Imports should be used.
    Unexported objects imported by ':::' calls:
      ‘flowCore:::.estimateLogicle’ ‘flowCore:::checkClass’
      ‘flowCore:::copyFlowSet’ ‘flowCore:::guid’
      ‘flowCore:::logicle_transform’ ‘flowCore:::updateTransformKeywords’
      ‘graph:::.makeEdgeKeys’ ‘lattice:::updateList’
      ‘ncdfFlow:::.isValidSamples’ ‘stats:::.splinefun’
      See the note in ?`:::` about the use of this operator.
    There are ::: calls to the package's namespace in its code. A package
      almost never needs to use ::: for its own objects:
      ‘.cpp_setIndices’ ‘.getNodeInd’
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    show,flowJoWorkspace: no visible binding for global variable
      ‘groupName’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/flowWorkspace/new/flowWorkspace.Rcheck/00_pkg_src/flowWorkspace/R/flowJoWorkspace_Methods.R:66)
    transform,GatingSet: no visible global function definition for ‘is’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/flowWorkspace/new/flowWorkspace.Rcheck/00_pkg_src/flowWorkspace/R/GatingSet_Methods.R:2291-2296)
    transform,GatingSet: no visible global function definition for ‘is’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/flowWorkspace/new/flowWorkspace.Rcheck/00_pkg_src/flowWorkspace/R/GatingSet_Methods.R:2298-2308)
    transform,GatingSet : <anonymous>: no visible global function
      definition for ‘is’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/flowWorkspace/new/flowWorkspace.Rcheck/00_pkg_src/flowWorkspace/R/GatingSet_Methods.R:2301-2302)
    Undefined global functions or variables:
      . .hasSlot as as.formula callNextMethod desc extends gray groupName
      IQR is median new node old openCyto.count parallel sampleName
      selectMethod slot validObject xml.count
    Consider adding
      importFrom("grDevices", "gray")
      importFrom("methods", ".hasSlot", "as", "callNextMethod", "extends",
                 "is", "new", "selectMethod", "slot", "validObject")
      importFrom("stats", "as.formula", "IQR", "median")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports
    field contains 'methods').
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

*   checking compiled code ... NOTE
    ```
    File ‘flowWorkspace/libs/flowWorkspace.so’:
      Found ‘__ZNSt3__14coutE’, possibly from ‘std::cout’ (C++)
        Object: ‘R_GatingSet.o’
    
    Compiled code should not call entry points which might terminate R
    nor write to stdout/stderr instead of to the console, nor use
    Fortran I/O nor system RNGs.
    
    See ‘Writing portable packages’ in the ‘Writing R Extensions’
    manual.
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

# fractional

Version: 0.1.3

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'Theory.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# fredr

Version: 1.0.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

# freqweights

Version: 1.0.4

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘Hmisc’
    ```

# FRK

Version: 0.2.2

## In both

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking: ‘dggrids’ ‘INLA’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  9.8Mb
      sub-directories of 1Mb or more:
        data   5.3Mb
        doc    2.1Mb
        R      2.0Mb
    ```

# FSA

Version: 0.8.20

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘alr4’, ‘prettyR’, ‘RMark’, ‘PMCMR’, ‘pgirmess’, ‘agricolae’
    ```

# FSelectorRcpp

Version: 0.2.1

## In both

*   checking whether package ‘FSelectorRcpp’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/FSelectorRcpp/new/FSelectorRcpp.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘RTCGA.rnaseq’
    ```

## Installation

### Devel

```
* installing *source* package ‘FSelectorRcpp’ ...
** package ‘FSelectorRcpp’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -fopenmp -I../inst/include -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/FSelectorRcpp/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/FSelectorRcpp/BH/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/FSelectorRcpp/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/FSelectorRcpp/testthat/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘FSelectorRcpp’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/FSelectorRcpp/new/FSelectorRcpp.Rcheck/FSelectorRcpp’

```
### CRAN

```
* installing *source* package ‘FSelectorRcpp’ ...
** package ‘FSelectorRcpp’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -fopenmp -I../inst/include -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/FSelectorRcpp/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/FSelectorRcpp/BH/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/FSelectorRcpp/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/FSelectorRcpp/testthat/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘FSelectorRcpp’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/FSelectorRcpp/old/FSelectorRcpp.Rcheck/FSelectorRcpp’

```
# ftDK

Version: 1.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 39 marked UTF-8 strings
    ```

# furniture

Version: 1.8.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘magrittr’
      All declared Imports should be used.
    ```

# furrowSeg

Version: 1.8.0

## In both

*   checking whether package ‘furrowSeg’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘EBImage::abind’ by ‘abind::abind’ when loading ‘furrowSeg’
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/furrowSeg/new/furrowSeg.Rcheck/00install.out’ for details.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 363.7Mb
      sub-directories of 1Mb or more:
        data  362.6Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/furrowSeg/new/furrowSeg.Rcheck/00_pkg_src/furrowSeg/R/featureAnalysis.R:76-77)
    plotFeatureEvolution: no visible global function definition for
      ‘polygon’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/furrowSeg/new/furrowSeg.Rcheck/00_pkg_src/furrowSeg/R/featureAnalysis.R:78-79)
    plotFeatureEvolution: no visible global function definition for ‘rgb’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/furrowSeg/new/furrowSeg.Rcheck/00_pkg_src/furrowSeg/R/featureAnalysis.R:78-79)
    plotFeatureEvolution: no visible global function definition for ‘axis’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/furrowSeg/new/furrowSeg.Rcheck/00_pkg_src/furrowSeg/R/featureAnalysis.R:80)
    plotFeatureEvolution: no visible global function definition for ‘mtext’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/furrowSeg/new/furrowSeg.Rcheck/00_pkg_src/furrowSeg/R/featureAnalysis.R:81)
    plotFeatureEvolution: no visible global function definition for ‘title’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/furrowSeg/new/furrowSeg.Rcheck/00_pkg_src/furrowSeg/R/featureAnalysis.R:84)
    Undefined global functions or variables:
      abline axis median mtext par plot points polygon predict quantile rgb
      title
    Consider adding
      importFrom("graphics", "abline", "axis", "mtext", "par", "plot",
                 "points", "polygon", "title")
      importFrom("grDevices", "rgb")
      importFrom("stats", "median", "predict", "quantile")
    to your NAMESPACE file.
    ```

# futureheatwaves

Version: 1.0.3

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'futureheatwaves.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# GA4GHclient

Version: 1.4.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 14 SKIPPED: 0 FAILED: 82
      1. Error: getBiosample works (@test-getBiosample.R#6) 
      2. Error: getCallSet works (@test-getCallSet.R#6) 
      3. Error: getDataset works (@test-getDataset.R#6) 
      4. Error: getExpressionLevel works (@test-getExpressionLevel.R#6) 
      5. Error: getFeature works (@test-getFeature.R#6) 
      6. Error: getFeatureSet works (@test-getFeatureSet.R#6) 
      7. Error: getIndividual works (@test-getIndividual.R#6) 
      8. Error: getReadGroupSet works (@test-getReadGroupSet.R#6) 
      9. Error: getReference works (@test-getReference.R#6) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    The following objects are masked from 'package:base':
    
        anyDuplicated, append, as.data.frame, basename, cbind, colMeans,
        colnames, colSums, dirname, do.call, duplicated, eval, evalq,
        Filter, Find, get, grep, grepl, intersect, is.unsorted, lapply,
        lengths, Map, mapply, match, mget, order, paste, pmax, pmax.int,
        pmin, pmin.int, Position, rank, rbind, Reduce, rowMeans,
        rownames, rowSums, sapply, setdiff, sort, table, tapply, union,
        unique, unsplit, which, which.max, which.min
    
    
    Attaching package: 'S4Vectors'
    
    The following object is masked from 'package:base':
    
        expand.grid
    
    Quitting from lines 129-133 (GA4GHclient.Rmd) 
    Error: processing vignette 'GA4GHclient.Rmd' failed with diagnostics:
    there is no package called 'org.Hs.eg.db'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘org.Hs.eg.db’ ‘TxDb.Hsapiens.UCSC.hg19.knownGene’
    ```

# GA4GHshiny

Version: 1.2.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      2: stop(txt, domain = NA)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 2 SKIPPED: 0 FAILED: 8
      1. Error: app works (@test-app.R#5) 
      2. Error: getGene works (@test-getGene.R#4) 
      3. Error: getGeneSymbols works (@test-getGeneSymbols.R#4) 
      4. Error: initializeReferences works (@test-initializeReferences.R#6) 
      5. Error: initializeVariantSet works (@test-initializeVariantSet.R#6) 
      6. Error: (unknown) (@test-searchVariantsByGeneSymbol.R#3) 
      7. Error: tidyVariants works with searchVariants output (@test-tidyVariants.R#6) 
      8. Error: tidyVariants works with searchVariantsByGeneSymbol output (@test-tidyVariants.R#16) 
      
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
    Error: processing vignette 'GA4GHshiny.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘org.Hs.eg.db’ ‘TxDb.Hsapiens.UCSC.hg19.knownGene’
    ```

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .travis.yml
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

# gaiah

Version: 0.0.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ggplot2’ ‘maptools’ ‘rgeos’ ‘stringr’ ‘tidyr’
      All declared Imports should be used.
    ```

# gastempt

Version: 0.4.01

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 15.5Mb
      sub-directories of 1Mb or more:
        libs  14.9Mb
    ```

# geex

Version: 1.0.11

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
       -------------------- inline R code fragments -------------------- 
        234:368 format(max(abs(comparison$geex$estimates - comparison$cls$estimates), abs(comparison$geex$vcov - comparison$cls$vcov)), digits = 2)
       ----------------------------------------------------------------- 
    
    label: SB3_results (with options) 
    List of 1
     $ echo: logi FALSE
    
       ~~~~~~~~~~~~~~~~~~~~~~~~~ R code chunk ~~~~~~~~~~~~~~~~~~~~~~~~~~ 
       comparison 
       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    ##------ Sat Oct  6 08:51:52 2018 ------##
    
      ordinary text without R code
    
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'v00_geex_intro.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rootSolve’
      All declared Imports should be used.
    ```

# gender

Version: 0.5.2

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘genderdata’
    ```

# GENESIS

Version: 2.10.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(testthat)
      > library(GENESIS)
      > 
      > test_check("GENESIS")
      ── 1. Error: (unknown) (@test_assocTestMM.R#2)  ────────────────────────────────
      there is no package called 'GWASdata'
      1: library(GWASdata) at testthat/test_assocTestMM.R:2
      2: stop(txt, domain = NA)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 255 SKIPPED: 0 FAILED: 1
      1. Error: (unknown) (@test_assocTestMM.R#2) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘GWASdata’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported object imported by a ':::' call: ‘survey:::saddle’
      See the note in ?`:::` about the use of this operator.
    ```

# geneXtendeR

Version: 1.6.0

## In both

*   checking package dependencies ... ERROR
    ```
    Packages required but not available:
      ‘GO.db’ ‘org.Rn.eg.db’ ‘org.Ag.eg.db’ ‘org.Bt.eg.db’
      ‘org.Ce.eg.db’ ‘org.Cf.eg.db’ ‘org.Dm.eg.db’ ‘org.Dr.eg.db’
      ‘org.Gg.eg.db’ ‘org.Hs.eg.db’ ‘org.Mm.eg.db’ ‘org.Mmu.eg.db’
      ‘org.Pt.eg.db’ ‘org.Sc.sgd.db’ ‘org.Ss.eg.db’ ‘org.Xl.eg.db’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# GenomicDataCommons

Version: 1.4.3

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Loading required package: magrittr
    
    Attaching package: 'GenomicDataCommons'
    
    The following object is masked from 'package:stats':
    
        filter
    
    Quitting from lines 145-146 (overview.Rmd) 
    Error: processing vignette 'overview.Rmd' failed with diagnostics:
    failed to rename downloaded file:
    
      from: '/Users/lionel/Library/Caches/GenomicDataCommons/691ec8c8-17b3-4c26-83f6-bd76817d9238/.partial_download'
      to: '/Users/lionel/Library/Caches/GenomicDataCommons/691ec8c8-17b3-4c26-83f6-bd76817d9238/0315439e-694c-4b14-a27c-29dfa117c764.htseq.counts.gz'
      reason:
        cannot rename file
        '/Users/lionel/Library/Caches/GenomicDataCommons/691ec8c8-17b3-4c26-83f6-bd76817d9238/.partial_download'
        to
        '/Users/lionel/Library/Caches/GenomicDataCommons/691ec8c8-17b3-4c26-83f6-bd76817d9238/0315439e-694c-4b14-a27c-29dfa117c764.htseq.counts.gz',
        reason 'No such file or directory'
    Execution halted
    ```

## In both

*   checking Rd \usage sections ... WARNING
    ```
    Undocumented arguments in documentation object '.htseq_importer'
      ‘fnames’
    
    Functions with \usage entries need to have the appropriate \alias
    entries, and all their arguments documented.
    The \usage entries must correspond to syntactically valid R code.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    default_fields.character: no visible binding for global variable
      ‘defaults’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/GenomicDataCommons/new/GenomicDataCommons.Rcheck/00_pkg_src/GenomicDataCommons/R/fields.R:51)
    gdc_rnaseq: no visible binding for global variable ‘case_id’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/GenomicDataCommons/new/GenomicDataCommons.Rcheck/00_pkg_src/GenomicDataCommons/R/gdc_rnaseq.R:106-107)
    gdc_rnaseq: no visible binding for global variable ‘file_id’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/GenomicDataCommons/new/GenomicDataCommons.Rcheck/00_pkg_src/GenomicDataCommons/R/gdc_rnaseq.R:106-107)
    Undefined global functions or variables:
      case_id defaults file_id
    ```

# GenomicInteractions

Version: 1.14.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.4Mb
      sub-directories of 1Mb or more:
        doc       1.8Mb
        extdata   7.9Mb
    ```

# GenomicMating

Version: 2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# GEOmetadb

Version: 1.42.0

## In both

*   R CMD check timed out
    

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .travis.yml
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    getSQLiteFile: no visible global function definition for
      ‘download.file’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/GEOmetadb/new/GEOmetadb.Rcheck/00_pkg_src/GEOmetadb/R/getSQLiteFile.R:6)
    Undefined global functions or variables:
      download.file
    Consider adding
      importFrom("utils", "download.file")
    to your NAMESPACE file.
    ```

# GEOquery

Version: 2.48.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 14.0Mb
      sub-directories of 1Mb or more:
        extdata  12.8Mb
    ```

*   checking whether the namespace can be loaded with stated dependencies ... NOTE
    ```
    Warning: no function found corresponding to methods exports from ‘GEOquery’ for: ‘show’
    
    A namespace must be able to be loaded with just the base namespace
    loaded: otherwise if the namespace gets loaded by a saved object,
    the session will be unable to start.
    
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
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/GEOquery/new/GEOquery.Rcheck/00_pkg_src/GEOquery/R/parseGEO.R:531-539)
    parseGSEMatrix: no visible binding for global variable ‘accession’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/GEOquery/new/GEOquery.Rcheck/00_pkg_src/GEOquery/R/parseGEO.R:531-539)
    parseGSEMatrix: no visible binding for global variable ‘accession’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/GEOquery/new/GEOquery.Rcheck/00_pkg_src/GEOquery/R/parseGEO.R:541-542)
    parseGSEMatrix: no visible global function definition for ‘new’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/GEOquery/new/GEOquery.Rcheck/00_pkg_src/GEOquery/R/parseGEO.R:568)
    parseGSEMatrix: no visible global function definition for ‘new’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/GEOquery/new/GEOquery.Rcheck/00_pkg_src/GEOquery/R/parseGEO.R:590)
    parseGSEMatrix: no visible global function definition for ‘new’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/GEOquery/new/GEOquery.Rcheck/00_pkg_src/GEOquery/R/parseGEO.R:606-610)
    parseGSEMatrix: no visible global function definition for ‘as’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/GEOquery/new/GEOquery.Rcheck/00_pkg_src/GEOquery/R/parseGEO.R:606-610)
    Undefined global functions or variables:
      . accession as characteristics k kvpair MA new read.delim read.table
      v
    Consider adding
      importFrom("methods", "as", "new")
      importFrom("utils", "read.delim", "read.table")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports
    field contains 'methods').
    ```

# geoSpectral

Version: 0.17.4

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        R           2.1Mb
        test_data   2.9Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unable to find any JVMs matching version "(null)".
    No Java runtime present, try --request to install.
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

# gespeR

Version: 1.12.0

## In both

*   checking for code/documentation mismatches ... WARNING
    ```
    Codoc mismatches from documentation object 'c,Phenotypes-method':
    \S4method{c}{Phenotypes}
      Code: function(x, ...)
      Docs: function(x, ..., recursive = FALSE)
      Argument names in docs not in code:
        recursive
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/gespeR/new/gespeR.Rcheck/00_pkg_src/gespeR/R/gespeR-functions.R:42-43)
    .select.model: no visible global function definition for ‘predict’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/gespeR/new/gespeR.Rcheck/00_pkg_src/gespeR/R/gespeR-functions.R:236)
    concordance: no visible global function definition for ‘cor’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/gespeR/new/gespeR.Rcheck/00_pkg_src/gespeR/R/gespeR-concordance.R:65)
    lasso.rand: no visible global function definition for ‘runif’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/gespeR/new/gespeR.Rcheck/00_pkg_src/gespeR/R/gespeR-functions.R:137-145)
    plot.gespeR: no visible global function definition for ‘hist’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/gespeR/new/gespeR.Rcheck/00_pkg_src/gespeR/R/gespeR-class.R:218)
    stability.selection: no visible global function definition for ‘lm’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/gespeR/new/gespeR.Rcheck/00_pkg_src/gespeR/R/gespeR-functions.R:214)
    Phenotypes,character: no visible global function definition for
      ‘read.delim’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/gespeR/new/gespeR.Rcheck/00_pkg_src/gespeR/R/Phenotypes-class.R:75)
    Undefined global functions or variables:
      coef cor hist lm predict read.delim runif
    Consider adding
      importFrom("graphics", "hist")
      importFrom("stats", "coef", "cor", "lm", "predict", "runif")
      importFrom("utils", "read.delim")
    to your NAMESPACE file.
    ```

# gestalt

Version: 0.1.4

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘gestalt-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: constant
    > ### Title: Values as Functions
    > ### Aliases: constant variable
    > 
    > ### ** Examples
    > 
    > # Function with a constant return value
    > val <- {message("Computing from scratch"); mtcars} %>>>%
    +   split(.$cyl) %>>>%
    +   lapply(function(data) lm(mpg ~ wt, data)) %>>>%
    +   lapply(summary) %>>>%
    +   sapply(`[[`, "r.squared")
    Error: Internal error: Can't find .top pronoun in data mask
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      4: tryCatch(expr, error = function(e) halt(failure, e$message)) at /Users/lionel/Desktop/rlang/revdep/checks.noindex/gestalt/new/gestalt.Rcheck/00_pkg_src/gestalt/R/utils.R:54
      5: tryCatchList(expr, classes, parentenv, handlers)
      6: tryCatchOne(expr, names, parentenv, handlers[[1L]])
      7: value[[3L]](cond)
      8: halt(failure, e$message) at /Users/lionel/Desktop/rlang/revdep/checks.noindex/gestalt/new/gestalt.Rcheck/00_pkg_src/gestalt/R/utils.R:54
      9: stop(sprintf(msg, ...), call. = FALSE) at /Users/lionel/Desktop/rlang/revdep/checks.noindex/gestalt/new/gestalt.Rcheck/00_pkg_src/gestalt/R/utils.R:58
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 814 SKIPPED: 0 FAILED: 3
      1. Error: composition operator obeys magrittr semantics (#39) (@test-compose.R#210) 
      2. Error: parentheses evaluate and group (@test-compose.R#321) 
      3. Error: posure shows composite-function expression (@test-print.R#81) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 42-54 (gestalt.Rmd) 
    Error: processing vignette 'gestalt.Rmd' failed with diagnostics:
    Internal error: Can't find .top pronoun in data mask
    Execution halted
    ```

# GetDFPData

Version: 1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Unable to find any JVMs matching version "(null)".
    No Java runtime present, try --request to install.
    ```

# GetITRData

Version: 0.7

## In both

*   checking dependencies in R code ... NOTE
    ```
    Unable to find any JVMs matching version "(null)".
    No Java runtime present, try --request to install.
    ```

# getProxy

Version: 1.12

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘bitops’ ‘data.table’ ‘dplyr’ ‘httr’
      All declared Imports should be used.
    ```

# getTBinR

Version: 0.5.5

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(getTBinR)
      > 
      > test_check("getTBinR")
      ── 1. Failure: map_tb_burden can have a custom legend specified. (@test-map_tb_b
      plot$labels$fill not equal to `test_label`.
      1/1 mismatches
      x[1]: "`test (test - test)`"
      y[1]: "test (test - test)"
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 75 SKIPPED: 29 FAILED: 1
      1. Failure: map_tb_burden can have a custom legend specified. (@test-map_tb_burden.R#62) 
      
      Error: testthat unit tests failed
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

# ggalluvial

Version: 0.9.0

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    [1] TRUE
    > # Titanic data in lodes format
    > titanic_lodes <- to_lodes_form(titanic_alluvia,
    +                                key = "x", value = "stratum", id = "alluvium",
    +                                axes = 1:4)
    > head(titanic_lodes)
      Freq "alluvium"   "x" "stratum"
    1    0          1 Class       1st
    2    0          2 Class       2nd
    3   35          3 Class       3rd
    4    0          4 Class      Crew
    5    0          5 Class       1st
    6    0          6 Class       2nd
    > is_lodes_form(titanic_lodes,
    +               key = "x", value = "stratum", id = "alluvium",
    +               weight = "Freq")
    Warning: 'glue::collapse' is deprecated.
    Use 'glue_collapse' instead.
    See help("Deprecated") and help("glue-deprecated").
    Error: Strings must match column names. Unknown columns: x
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      The vignette engine knitr::rmarkdown is not available, because the rmarkdown package is not installed. Please install it.
    Loading required package: ggplot2
    Quitting from lines 33-43 (ggalluvial.rmd) 
    Error: processing vignette 'ggalluvial.rmd' failed with diagnostics:
    object 'stratum' not found
    Execution halted
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

# ggbio

Version: 1.28.5

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

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
        expand.grid
    
    Loading required package: IRanges
    Loading required package: GenomeInfoDb
      ordinary text without R code
    
    label: unnamed-chunk-5
    Quitting from lines 143-157 (ggbio.Rnw) 
    Error in library(Homo.sapiens) : 
      there is no package called 'Homo.sapiens'
    Calls: knit2pdf ... withCallingHandlers -> withVisible -> eval -> eval -> library
    In addition: Warning messages:
    1: `panel.margin` is deprecated. Please use `panel.spacing` property instead 
    2: `panel.margin` is deprecated. Please use `panel.spacing` property instead 
    3: `panel.margin` is deprecated. Please use `panel.spacing` property instead 
    4: `panel.margin` is deprecated. Please use `panel.spacing` property instead 
    Execution halted
    make: *** [ggbio.pdf] Error 1
    Error in buildVignettes(dir = "/Users/lionel/Desktop/rlang/revdep/checks.noindex/ggbio/new/ggbio.Rcheck/vign_test/ggbio") : 
      running 'make' failed
    Execution halted
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
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        doc   1.8Mb
        R     3.0Mb
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

Version: 1.8.2

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Warning: Removed 142 rows containing missing values (geom_path).
    Warning: Removed 4 rows containing missing values (geom_hex).
    loading R object...
    loading tree object...
    Done
    Warning: Removed 1 rows containing missing values (geom_hex).
    Warning: Removed 5 rows containing missing values (geom_hex).
    Warning: Removed 11864 rows containing non-finite values (stat_binhex).
    Warning: Removed 15 rows containing missing values (geom_hex).
    Warning: Removed 11892 rows containing non-finite values (stat_binhex).
    Warning: Removed 26 rows containing missing values (geom_hex).
    Warning: Removed 11815 rows containing non-finite values (stat_binhex).
    Warning: Removed 26 rows containing missing values (geom_hex).
    Warning: Removed 11863 rows containing non-finite values (stat_binhex).
    Warning: Removed 27 rows containing missing values (geom_hex).
    Warning: Removed 11858 rows containing non-finite values (stat_binhex).
    Warning: Removed 25 rows containing missing values (geom_hex).
    Quitting from lines 121-124 (Top_features_of_ggcyto.Rmd) 
    Error: processing vignette 'Top_features_of_ggcyto.Rmd' failed with diagnostics:
    can't find "CD4"
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.3Mb
      sub-directories of 1Mb or more:
        doc   5.5Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘scales’
      All declared Imports should be used.
    Unexported objects imported by ':::' calls:
      ‘flowWorkspace:::.mergeGates’ ‘flowWorkspace:::compact’
      ‘flowWorkspace:::fix_y_axis’ ‘flowWorkspace:::isNegated’
      ‘ggplot2:::+.gg’ ‘ggplot2:::add_group’ ‘ggplot2:::as_gg_data_frame’
      ‘ggplot2:::check_aesthetics’ ‘ggplot2:::is_calculated_aes’
      ‘ggplot2:::is.waive’ ‘ggplot2:::make_labels’ ‘ggplot2:::make_scale’
      ‘ggplot2:::plot_clone’ ‘ggplot2:::print.ggplot’
      ‘ggplot2:::scales_add_defaults’ ‘ggplot2:::scales_list’
      ‘ggplot2:::update_theme’
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

# ggdag

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ggforce’ ‘plyr’
      All declared Imports should be used.
    ```

# ggdistribute

Version: 1.0.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
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

Version: 0.5.0

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

Version: 0.1.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘colorspace’ ‘grid’ ‘rstan’
      All declared Imports should be used.
    ```

# ggformula

Version: 0.9.0

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

# gggenes

Version: 0.3.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rlang’
      All declared Imports should be used.
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

# gginnards

Version: 0.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘grid’ ‘tibble’
      All declared Imports should be used.
    ```

# ggiraphExtra

Version: 0.2.9

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ggforce’ ‘webshot’ ‘ztable’
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

# ggmap

Version: 2.6.1

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘ggmap-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: get_map
    > ### Title: Grab a map.
    > ### Aliases: get_map
    > 
    > ### ** Examples
    > 
    > map <- get_map()
    Warning in download.file(url, destfile = tmp, quiet = !messaging, mode = "wb") :
      cannot open URL 'http://maps.googleapis.com/maps/api/staticmap?center=29.763284,-95.363271&zoom=10&size=640x640&scale=2&maptype=terrain&language=en-EN&sensor=false': HTTP status was '403 Forbidden'
    Error in download.file(url, destfile = tmp, quiet = !messaging, mode = "wb") : 
      cannot open URL 'http://maps.googleapis.com/maps/api/staticmap?center=29.763284,-95.363271&zoom=10&size=640x640&scale=2&maptype=terrain&language=en-EN&sensor=false'
    Calls: get_map -> get_googlemap -> download.file
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        data   4.8Mb
    ```

# ggmcmc

Version: 1.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
      no font could be found for family "Source Sans Pro"
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      no font could be found for family "Source Sans Pro"
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      no font could be found for family "Source Sans Pro"
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      no font could be found for family "Source Sans Pro"
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      no font could be found for family "Source Sans Pro"
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      no font could be found for family "Source Sans Pro"
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      no font could be found for family "Source Sans Pro"
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      no font could be found for family "Source Sans Pro"
    Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  :
      no font could be found for family "Source Sans Pro"
    Quitting from lines 142-143 (using_ggmcmc.Rmd) 
    Error: processing vignette 'using_ggmcmc.Rmd' failed with diagnostics:
    polygon edge not found
    Execution halted
    ```

# ggpage

Version: 0.2.2

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggpage-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: paper_shape
    > ### Title: Identify the edges of the paper of each page
    > ### Aliases: paper_shape
    > 
    > ### ** Examples
    > 
    > paper_shape(ggpage_build(tinderbox))
    Error in check_input(x) : 
      Input must be a character vector of any length or a list of character
      vectors, each of which has a length of 1.
    Calls: paper_shape ... tokenfunc -> tf -> tokenize_words.default -> check_input
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      17: function_list[[i]](value)
      18: tidytext::unnest_tokens(., output = "word", input = "text")
      19: unnest_tokens.data.frame(., output = "word", input = "text")
      20: tokenfunc(col, ...)
      21: tf(col, lowercase = FALSE, ...)
      22: tokenize_words.default(col, lowercase = FALSE, ...)
      23: check_input(x)
      24: stop("Input must be a character vector of any length or a list of character\n", "  vectors, each of which has a length of 1.")
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 11 SKIPPED: 0 FAILED: 1
      1. Error: The number of rows and cols are correct (@test-paper_shape.R#4) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Quitting from lines 24-40 (different-features.Rmd) 
    Error: processing vignette 'different-features.Rmd' failed with diagnostics:
    Input must be a character vector of any length or a list of character
      vectors, each of which has a length of 1.
    Execution halted
    ```

# ggplot2

Version: 3.0.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.2Mb
      sub-directories of 1Mb or more:
        doc   1.8Mb
        R     3.8Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘mgcv’ ‘reshape2’ ‘viridisLite’
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

# ggpubr

Version: 0.1.8

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

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘colourpicker’ ‘dplyr’ ‘DT’ ‘Formula’ ‘ggpmisc’ ‘ggrepel’ ‘grDevices’
      ‘gridExtra’ ‘Hmisc’ ‘lazyeval’ ‘markdown’ ‘plotly’ ‘quantreg’ ‘rlang’
      ‘shinyjs’ ‘table1’ ‘tidyr’
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

# ggraph

Version: 1.0.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.9Mb
      sub-directories of 1Mb or more:
        doc   3.0Mb
        R     2.0Mb
    ```

# ggridges

Version: 0.5.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 6242 marked UTF-8 strings
    ```

# ggspatial

Version: 1.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyr’
      All declared Imports should be used.
    ```

# ggstatsplot

Version: 0.0.6

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggstatsplot-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggcoefstats
    > ### Title: Model coefficients for fitted models with the model summary as a
    > ###   caption.
    > ### Aliases: ggcoefstats
    > 
    > ### ** Examples
    > 
    > 
    > set.seed(123)
    > ggcoefstats(x = lm(formula = mpg ~ cyl * am, data = mtcars))
    Error in match.arg(package, unique(paletteer::palettes_d_names$package)) : 
      'arg' should be one of “awtools”, “dichromat”, “dutchmasters”, “ggsci”, “ggpomological”, “ggthemes”, “ghibli”, “grDevices”, “jcolors”, “LaCroixColoR”, “NineteenEightyR”, “nord”, “ochRe”, “palettetown”, “pals”, “Polychrome”, “quickpalette”, “rcartocolor”, “RColorBrewer”, “Redmonder”, “RSkittleBrewer”, “wesanderson”, “yarrr”
    Calls: ggcoefstats -> <Anonymous> -> match.arg
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
        intersect, setdiff, setequal, union
    
    -------------------------------------------------------------------------
    You have loaded plyr after dplyr - this is likely to cause problems.
    If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
    library(plyr); library(dplyr)
    -------------------------------------------------------------------------
    
    Attaching package: 'plyr'
    
    The following objects are masked from 'package:dplyr':
    
        arrange, count, desc, failwith, id, mutate, rename, summarise,
        summarize
    
    Scale for 'y' is already present. Adding another scale for 'y', which
    will replace the existing scale.
    Quitting from lines 79-107 (ggcoefstats.Rmd) 
    Error: processing vignette 'ggcoefstats.Rmd' failed with diagnostics:
    'arg' should be one of "awtools", "dichromat", "dutchmasters", "ggsci", "ggpomological", "ggthemes", "ghibli", "grDevices", "jcolors", "LaCroixColoR", "NineteenEightyR", "nord", "ochRe", "palettetown", "pals", "Polychrome", "quickpalette", "rcartocolor", "RColorBrewer", "Redmonder", "RSkittleBrewer", "wesanderson", "yarrr"
    Execution halted
    ```

## Newly fixed

*   R CMD check timed out
    

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        doc    2.6Mb
        help   2.3Mb
    ```

# ggthemes

Version: 4.0.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 138 marked UTF-8 strings
    ```

# ggtree

Version: 1.12.7

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
      installed size is  9.8Mb
      sub-directories of 1Mb or more:
        doc        4.9Mb
        examples   3.7Mb
    ```

# ggvis

Version: 0.4.4

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘plyr’
    ```

# glmmfields

Version: 0.1.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.8Mb
      sub-directories of 1Mb or more:
        libs   8.0Mb
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

# googledrive

Version: 0.1.1

## In both

*   checking examples ... WARNING
    ```
    Found the following significant warnings:
    
      Warning: 'collapse' is deprecated.
    Deprecated functions may be defunct as soon as of the next release
    of R.
    See ?Deprecated.
    ```

# googlesheets

Version: 0.3.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tibble’
      All declared Imports should be used.
    ```

# gphmm

Version: 0.99.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    
    ----- ----- ----- -----
    
     gphmm version 0.99.0
    
     Biostrings version 2.48.0
    
     jsonlite version 1.5
    
     docopt version 0.6
    
    ----- ----- ----- -----
    
    
    [1] "We are going to : compute"
    [1] "Number of cores used : 1"
    Error in if (param == "") paramgphmm = initializeGphmm() : 
      argument is of length zero
    Execution halted
    Execution halted
    ```

# gQTLstats

Version: 1.12.0

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘Homo.sapiens’
    
    Packages suggested but not available for checking:
      ‘geuvPack’ ‘geuvStore2’ ‘org.Hs.eg.db’
      ‘TxDb.Hsapiens.UCSC.hg19.knownGene’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# graphicalVAR

Version: 0.2.2

## In both

*   checking whether package ‘graphicalVAR’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/graphicalVAR/new/graphicalVAR.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘graphicalVAR’ ...
** package ‘graphicalVAR’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/graphicalVAR/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/graphicalVAR/RcppArmadillo/include" -I/usr/local/include  -fopenmp  -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘graphicalVAR’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/graphicalVAR/new/graphicalVAR.Rcheck/graphicalVAR’

```
### CRAN

```
* installing *source* package ‘graphicalVAR’ ...
** package ‘graphicalVAR’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/graphicalVAR/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/graphicalVAR/RcppArmadillo/include" -I/usr/local/include  -fopenmp  -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘graphicalVAR’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/graphicalVAR/old/graphicalVAR.Rcheck/graphicalVAR’

```
# graphTweets

Version: 0.5.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘utils’
      All declared Imports should be used.
    ```

# grasp2db

Version: 1.1.0

## In both

*   R CMD check timed out
    

*   checking for missing documentation entries ... WARNING
    ```
    Undocumented code objects:
      ‘checkAnti’ ‘getJoinCompatible’ ‘GRASP2’
    Undocumented data sets:
      ‘mml10p_nox’ ‘uniqueGexNames2.0’ ‘uniquePPDnames2.0’
    All user-level objects in a package should have documentation
    entries.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking data for non-ASCII characters ... WARNING
    ```
      Warning: found non-ASCII string
      'Beh<e7>et's disease' in object 'uniquePPDnames2.0'
    ```

*   checking data for ASCII and uncompressed saves ... WARNING
    ```
      
      Note: significantly better compression could be obtained
            by using R CMD build --resave-data
                            old_size new_size compress
      mml10p_nox.rda           7.1Mb    2.8Mb       xz
      uniquePPDnames2.0.rda     17Kb     15Kb    bzip2
    ```

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘AnnotationHubData’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  7.6Mb
      sub-directories of 1Mb or more:
        data   7.1Mb
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    License components with restrictions not permitted:
      Artistic-2.0 + file LICENSE
    ```

*   checking R code for possible problems ... NOTE
    ```
    .grasp2ToAnnotationHub: no visible global function definition for
      ‘outputFile’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/grasp2db/new/grasp2db.Rcheck/00_pkg_src/grasp2db/R/db_AnnotationHub.R:39)
    .grasp2ToAnnotationHub: no visible global function definition for
      ‘outputFile’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/grasp2db/new/grasp2db.Rcheck/00_pkg_src/grasp2db/R/db_AnnotationHub.R:40)
    checkAnti: no visible binding for global variable ‘chr_hg19’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/grasp2db/new/grasp2db.Rcheck/00_pkg_src/grasp2db/R/doanti.R:19-20)
    getJoinCompatible: no visible binding for global variable ‘gwrngs19’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/grasp2db/new/grasp2db.Rcheck/00_pkg_src/grasp2db/R/doanti.R:7)
    Undefined global functions or variables:
      chr_hg19 gwrngs19 outputFile
    ```

# grattan

Version: 1.6.0.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘taxstats’
    ```

# gravity

Version: 0.8.5

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'creating-gravity-datasets.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# Greg

Version: 1.2.2

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘rmeta’
    ```

# gutenbergr

Version: 0.1.4

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 13617 marked UTF-8 strings
    ```

# hdme

Version: 0.2.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Loading required package: lattice
    Loading required package: MASS
    
    Attaching package: 'MASS'
    
    The following object is masked from 'package:dplyr':
    
        select
    
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'hdme-package.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# heatwaveR

Version: 0.3.3

## In both

*   checking whether package ‘heatwaveR’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/heatwaveR/new/heatwaveR.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘heatwaveR’ ...
** package ‘heatwaveR’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/heatwaveR/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/heatwaveR/RcppArmadillo/include" -I/usr/local/include  -fopenmp -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘heatwaveR’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/heatwaveR/new/heatwaveR.Rcheck/heatwaveR’

```
### CRAN

```
* installing *source* package ‘heatwaveR’ ...
** package ‘heatwaveR’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/heatwaveR/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/heatwaveR/RcppArmadillo/include" -I/usr/local/include  -fopenmp -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘heatwaveR’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/heatwaveR/old/heatwaveR.Rcheck/heatwaveR’

```
# heemod

Version: 0.9.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        doc       1.5Mb
        R         2.1Mb
        tabular   1.2Mb
    ```

# hei

Version: 0.1.0

## In both

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
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/hiAnnotator/new/hiAnnotator.Rcheck/00_pkg_src/hiAnnotator/R/hiAnnotator.R:424-428)
    makeGRanges: no visible global function definition for ‘seqlengths’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/hiAnnotator/new/hiAnnotator.Rcheck/00_pkg_src/hiAnnotator/R/hiAnnotator.R:439-440)
    makeGRanges: no visible global function definition for ‘seqlevels<-’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/hiAnnotator/new/hiAnnotator.Rcheck/00_pkg_src/hiAnnotator/R/hiAnnotator.R:464)
    makeGRanges: no visible global function definition for ‘sortSeqlevels’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/hiAnnotator/new/hiAnnotator.Rcheck/00_pkg_src/hiAnnotator/R/hiAnnotator.R:464)
    makeGRanges: no visible global function definition for ‘seqlevelsInUse’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/hiAnnotator/new/hiAnnotator.Rcheck/00_pkg_src/hiAnnotator/R/hiAnnotator.R:464)
    makeGRanges: no visible global function definition for ‘seqlengths<-’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/hiAnnotator/new/hiAnnotator.Rcheck/00_pkg_src/hiAnnotator/R/hiAnnotator.R:465)
    makeGRanges: no visible global function definition for ‘seqlevels’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/hiAnnotator/new/hiAnnotator.Rcheck/00_pkg_src/hiAnnotator/R/hiAnnotator.R:465)
    Undefined global functions or variables:
      breakInChunks countQueryHits detectCores dist featureName IRanges
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

# highcharter

Version: 0.5.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 16.9Mb
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
    vpairwiseAlignSeqs: no visible global function definition for
      ‘runLength’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/hiReadsProcessor/new/hiReadsProcessor.Rcheck/00_pkg_src/hiReadsProcessor/R/hiReadsProcessor.R:1393-1396)
    vpairwiseAlignSeqs: no visible global function definition for ‘Rle’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/hiReadsProcessor/new/hiReadsProcessor.Rcheck/00_pkg_src/hiReadsProcessor/R/hiReadsProcessor.R:1398)
    vpairwiseAlignSeqs: no visible global function definition for
      ‘runLength’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/hiReadsProcessor/new/hiReadsProcessor.Rcheck/00_pkg_src/hiReadsProcessor/R/hiReadsProcessor.R:1399-1402)
    vpairwiseAlignSeqs: no visible global function definition for
      ‘runValue’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/hiReadsProcessor/new/hiReadsProcessor.Rcheck/00_pkg_src/hiReadsProcessor/R/hiReadsProcessor.R:1400-1401)
    vpairwiseAlignSeqs: no visible global function definition for
      ‘runLength’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/hiReadsProcessor/new/hiReadsProcessor.Rcheck/00_pkg_src/hiReadsProcessor/R/hiReadsProcessor.R:1400-1401)
    vpairwiseAlignSeqs: no visible global function definition for ‘IRanges’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/hiReadsProcessor/new/hiReadsProcessor.Rcheck/00_pkg_src/hiReadsProcessor/R/hiReadsProcessor.R:1420-1424)
    Undefined global functions or variables:
      breakInChunks clusteredValue clusteredValue.freq DataFrame
      detectCores fasta.info IRanges IRangesList matches mclapply metadata
      metadata<- misMatches qBaseInsert queryHits Rle runLength runValue
      scanBamFlag ScanBamParam SimpleList tBaseInsert
    ```

# HMMoce

Version: 1.0.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'Using_HMMoce.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# HMP16SData

Version: 1.0.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Attaching package: 'dendextend'
    
    The following object is masked from 'package:stats':
    
        cutree
    
    ========================================
    circlize version 0.4.4
    CRAN page: https://cran.r-project.org/package=circlize
    Github page: https://github.com/jokergoo/circlize
    Documentation: http://jokergoo.github.io/circlize_book/book/
    
    If you use it in published research, please cite:
    Gu, Z. circlize implements and enhances circular visualization 
      in R. Bioinformatics 2014.
    ========================================
    
    Quitting from lines 58-71 (HMP16SData.Rmd) 
    Error: processing vignette 'HMP16SData.Rmd' failed with diagnostics:
    there is no package called 'curatedMetagenomicData'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘curatedMetagenomicData’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 19.1Mb
      sub-directories of 1Mb or more:
        doc       1.5Mb
        extdata  17.4Mb
    ```

# hpiR

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘knitr’
      All declared Imports should be used.
    ```

# HTSSIP

Version: 1.4.0

## In both

*   R CMD check timed out
    

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘igraph’
      All declared Imports should be used.
    ```

# hurricaneexposure

Version: 0.0.1

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘hurricaneexposuredata’
    ```

# HydeNet

Version: 0.10.8

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
      iter  80 value 14.018282
      iter  80 value 14.018282
      iter  90 value 14.017126
      final  value 14.015374 
      converged
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 60 SKIPPED: 0 FAILED: 5
      1. Error: compileDecisionModel (@test_compileDecisionModel.R#14) 
      2. Error: (unknown) (@test-bindPosterior.R#12) 
      3. Error: compileJagsModel returns an object of class 'compiledHydeNetwork' (@test-compileJagsModel.R#14) 
      4. Error: (unknown) (@test-HydePosterior.R#11) 
      5. Error: (unknown) (@test-print.HydePosterior.R#11) 
      
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

*   checking data for non-ASCII characters ... NOTE
    ```
      Error in .requirePackage(package) : 
        unable to find required package 'graph'
      Calls: <Anonymous> ... .extendsForS3 -> extends -> getClassDef -> .requirePackage
      Execution halted
    ```

# hydrolinks

Version: 0.10.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dbplyr’
      All declared Imports should be used.
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

# iCNV

Version: 1.0.0

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
Error : package ‘BSgenome.Hsapiens.UCSC.hg19’ required by ‘CODEX’ could not be found
ERROR: lazy loading failed for package ‘iCNV’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/iCNV/old/iCNV.Rcheck/iCNV’

```
# iCOBRA

Version: 1.8.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        doc       1.2Mb
        extdata   1.7Mb
        R         2.0Mb
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
      installed size is  8.4Mb
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

# IHW

Version: 1.8.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    The following objects are masked from 'package:S4Vectors':
    
        first, intersect, rename, setdiff, setequal, union
    
    The following objects are masked from 'package:BiocGenerics':
    
        combine, intersect, setdiff, union
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Quitting from lines 42-47 (introduction_to_ihw.Rmd) 
    Error: processing vignette 'introduction_to_ihw.Rmd' failed with diagnostics:
    there is no package called 'airway'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘airway’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.3Mb
      sub-directories of 1Mb or more:
        doc   5.8Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/IHW/new/IHW.Rcheck/00_pkg_src/IHW/R/plots.R:101)
    plot_decisionboundary: no visible binding for global variable
      ‘covariate’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/IHW/new/IHW.Rcheck/00_pkg_src/IHW/R/plots.R:110-112)
    plot_decisionboundary: no visible binding for global variable ‘pvalue’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/IHW/new/IHW.Rcheck/00_pkg_src/IHW/R/plots.R:110-112)
    plot_decisionboundary: no visible binding for global variable ‘fold’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/IHW/new/IHW.Rcheck/00_pkg_src/IHW/R/plots.R:110-112)
    thresholds_ihwResult: no visible global function definition for
      ‘na.exclude’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/IHW/new/IHW.Rcheck/00_pkg_src/IHW/R/ihw_class.R:96-97)
    thresholds,ihwResult: no visible global function definition for
      ‘na.exclude’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/IHW/new/IHW.Rcheck/00_pkg_src/IHW/R/ihw_class.R:96-97)
    Undefined global functions or variables:
      covariate fold gurobi mcols mcols<- metadata metadata<- na.exclude
      p.adjust pvalue runif str stratum
    Consider adding
      importFrom("stats", "na.exclude", "p.adjust", "runif")
      importFrom("utils", "str")
    to your NAMESPACE file.
    ```

# IHWpaper

Version: 1.7.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [1] "LSL GBH"
      [1] "TST GBH"
      [1] "SBH"
      [1] "Clfdr"
      [1] "Greedy Indep. Filt."
      [1] "IHW"
      [1] "IHW-Bonferroni E3"
      [1] "Bonferroni"
      [1] "qvalue"
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 1 SKIPPED: 0 FAILED: 1
      1. Error: (unknown) (@test_analyze_datasets.R#4) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 29-65 (BH-explanation.Rmd) 
    Error: processing vignette 'BH-explanation.Rmd' failed with diagnostics:
    Palette not found.
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘airway’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 24.1Mb
      sub-directories of 1Mb or more:
        doc      13.1Mb
        extdata   9.8Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    scott_fdrreg: no visible global function definition for ‘FDRreg’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/IHWpaper/new/IHWpaper.Rcheck/00_pkg_src/IHWpaper/R/covariate_methods.R:88)
    scott_fdrreg: no visible global function definition for ‘getFDR’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/IHWpaper/new/IHWpaper.Rcheck/00_pkg_src/IHWpaper/R/covariate_methods.R:97)
    sim_fun_eval: no visible binding for global variable ‘fdr_method’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/IHWpaper/new/IHWpaper.Rcheck/00_pkg_src/IHWpaper/R/benchmarking.R:61-63)
    sim_fun_eval: no visible binding for global variable ‘fdr_pars’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/IHWpaper/new/IHWpaper.Rcheck/00_pkg_src/IHWpaper/R/benchmarking.R:61-63)
    sim_fun_eval: no visible binding for global variable ‘FDP’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/IHWpaper/new/IHWpaper.Rcheck/00_pkg_src/IHWpaper/R/benchmarking.R:61-63)
    sim_fun_eval: no visible binding for global variable ‘rj_ratio’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/IHWpaper/new/IHWpaper.Rcheck/00_pkg_src/IHWpaper/R/benchmarking.R:61-63)
    sim_fun_eval: no visible binding for global variable ‘FPR’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/IHWpaper/new/IHWpaper.Rcheck/00_pkg_src/IHWpaper/R/benchmarking.R:61-63)
    sim_fun_eval: no visible binding for global variable ‘FWER’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/IHWpaper/new/IHWpaper.Rcheck/00_pkg_src/IHWpaper/R/benchmarking.R:61-63)
    Undefined global functions or variables:
      FDP fdr_method fdr_pars FDRreg FPR FWER getFDR rj_ratio
    ```

# ijtiff

Version: 1.4.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 48-49 (the-imagej-problem.Rmd) 
    Error: processing vignette 'the-imagej-problem.Rmd' failed with diagnostics:
    there is no package called 'magick'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘magick’
    ```

# imager

Version: 0.41.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -Dcimg_r_mode -fpermissive -I/usr/X11R6/include -I/opt/X11/include  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/imager/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/checks.noindex/imager/new/imager.Rcheck/imager/include" -I"/private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/Rtmp0a87LL/sourceCpp-x86_64-apple-darwin15.6.0-0.12.19" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c file270782dfef7.cpp -o file270782dfef7.o
      clang++ -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o sourceCpp_2.so file270782dfef7.o -lX11 -L/usr/X11R6/lib -L/opt/X11/include -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
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
      installed size is 12.2Mb
      sub-directories of 1Mb or more:
        data      1.4Mb
        doc       1.1Mb
        include   2.8Mb
        libs      4.7Mb
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

# ImputeRobust

Version: 1.3

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘gamlss’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# incadata

Version: 0.6.4

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

# INDperform

Version: 0.1.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        data   3.5Mb
        help   1.1Mb
    ```

# inlabru

Version: 2.1.9

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘INLA’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        data   1.1Mb
        misc   1.8Mb
        R      2.0Mb
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘INLA’
    ```

# interplot

Version: 0.2.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Loading required package: ggplot2
    Loading required package: abind
    Loading required package: arm
    Loading required package: MASS
    Loading required package: Matrix
    Loading required package: lme4
    
    arm (Version 1.10-1, built: 2018-4-12)
    
    Working directory is /Users/lionel/Desktop/rlang/revdep/checks.noindex/interplot/new/interplot.Rcheck/vign_test/interplot/vignettes
    
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'interplot-vignette.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘gridExtra’
      All declared Imports should be used.
    ```

# IONiseR

Version: 2.4.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    
    Attaching package: 'GenomicAlignments'
    
    The following object is masked from 'package:dplyr':
    
        last
    
    
    Attaching package: 'ShortRead'
    
    The following object is masked from 'package:dplyr':
    
        id
    
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'IONiseR.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.8Mb
      sub-directories of 1Mb or more:
        doc       3.7Mb
        extdata   1.5Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘idx’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/IONiseR/new/IONiseR.Rcheck/00_pkg_src/IONiseR/R/Methods-subsetting.R:19-21)
    [,Fast5Summary-ANY-ANY-ANY: no visible binding for global variable
      ‘component’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/IONiseR/new/IONiseR.Rcheck/00_pkg_src/IONiseR/R/Methods-subsetting.R:24-26)
    [,Fast5Summary-ANY-ANY-ANY: no visible binding for global variable
      ‘idx’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/IONiseR/new/IONiseR.Rcheck/00_pkg_src/IONiseR/R/Methods-subsetting.R:24-26)
    show,Fast5Summary: no visible binding for global variable ‘full_2D’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/IONiseR/new/IONiseR.Rcheck/00_pkg_src/IONiseR/R/classes.R:70-71)
    show,Fast5Summary: no visible binding for global variable ‘pass’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/IONiseR/new/IONiseR.Rcheck/00_pkg_src/IONiseR/R/classes.R:75)
    show,Fast5Summary: no visible binding for global variable ‘pass’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/IONiseR/new/IONiseR.Rcheck/00_pkg_src/IONiseR/R/classes.R:77)
    Undefined global functions or variables:
      := AAAAA accumulation baseCalledComplement baseCalledTemplate
      bases_called category channel circleFun component duration error freq
      full_2D group hour idx matrixCol matrixRow mean_value meanZValue
      median_signal minute mux name nbases new_reads num_events oddEven
      pass pentamer rbindlist readIDs seq_length start_time time_bin
      time_group TTTTT x y zvalue
    ```

# ipeaData

Version: 0.0.2

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ipeaData-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ipeadata
    > ### Title: Return values from a serie, its metadata and citation.
    > ### Aliases: ipeadata
    > 
    > ### ** Examples
    > 
    >     data <- ipeadata('ADMIS')
    Error in curl::curl_fetch_memory(url, handle = handle) : 
      Timeout was reached: Connection timed out after 10010 milliseconds
    Calls: ipeadata ... request_fetch -> request_fetch.write_memory -> <Anonymous>
    Execution halted
    ```

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      2: basic_call(api_call, type) at /Users/lionel/Desktop/rlang/revdep/checks.noindex/ipeaData/new/ipeaData.Rcheck/00_pkg_src/ipeaData/R/ipeaData.R:139
      3: GET(api) at /Users/lionel/Desktop/rlang/revdep/checks.noindex/ipeaData/new/ipeaData.Rcheck/00_pkg_src/ipeaData/R/ipeaData.R:61
      4: request_perform(req, hu$handle$handle)
      5: request_fetch(req$output, req$url, handle)
      6: request_fetch.write_memory(req$output, req$url, handle)
      7: curl::curl_fetch_memory(url, handle = handle)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 3 SKIPPED: 0 FAILED: 3
      1. Error: basic call (@test_functions_ipea_data.R#12) 
      2. Error: ipeaData (@test_functions_ipea_data.R#17) 
      3. Error: search_serie (@test_functions_ipea_data.R#24) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘RCurl’
      All declared Imports should be used.
    ```

# ipumsr

Version: 0.3.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘R6’
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
clang++ -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/iRF/Rcpp/include" -I/usr/local/include  -fopenmp -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c ExportedFunctionsRIT.cpp -o ExportedFunctionsRIT.o
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
clang++ -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/iRF/Rcpp/include" -I/usr/local/include  -fopenmp -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c ExportedFunctionsRIT.cpp -o ExportedFunctionsRIT.o
clang: error: unsupported option '-fopenmp'
make: *** [ExportedFunctionsRIT.o] Error 1
ERROR: compilation failed for package ‘iRF’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/iRF/old/iRF.Rcheck/iRF’

```
# IrisSpatialFeatures

Version: 1.3.0

## In both

*   R CMD check timed out
    

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        data      2.1Mb
        extdata   1.9Mb
    ```

# iSEE

Version: 1.0.1

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘iSEE-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: annotateEnsembl
    > ### Title: Annotation via ENSEMBL database
    > ### Aliases: annotateEnsembl
    > 
    > ### ** Examples
    > 
    > library(scRNAseq)
    Error in library(scRNAseq) : there is no package called ‘scRNAseq’
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      The following objects are masked from 'package:base':
      
          aperm, apply
      
      Loading required package: SingleCellExperiment
      > 
      > test_check("iSEE")
      Loading required package: scRNAseq
      Error in eval(exprs, env) : require(scRNAseq) is not TRUE
      Calls: test_check ... source_dir -> lapply -> FUN -> eval -> eval -> stopifnot
      In addition: Warning message:
      In library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE,  :
        there is no package called 'scRNAseq'
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    The following objects are masked from 'package:Biobase':
    
        anyMissing, rowMedians
    
    Loading required package: BiocParallel
    
    Attaching package: 'DelayedArray'
    
    The following objects are masked from 'package:matrixStats':
    
        colMaxs, colMins, colRanges, rowMaxs, rowMins, rowRanges
    
    The following objects are masked from 'package:base':
    
        aperm, apply
    
    Loading required package: SingleCellExperiment
    Quitting from lines 89-98 (iSEE_vignette.Rmd) 
    Error: processing vignette 'iSEE_vignette.Rmd' failed with diagnostics:
    there is no package called 'scRNAseq'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘scRNAseq’ ‘org.Mm.eg.db’
    
    Package which this enhances but not available for checking: ‘ExperimentHub’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported object imported by a ':::' call: ‘S4Vectors:::selectSome’
      See the note in ?`:::` about the use of this operator.
    ```

# isomiRs

Version: 1.8.0

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘targetscan.Hs.eg.db’
    
    Package suggested but not available for checking: ‘org.Mm.eg.db’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# ITNr

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘animation’ ‘comtradr’ ‘Matrix’ ‘ndtv’ ‘statnet’ ‘xergm’
      All declared Imports should be used.
    ```

# jaccard

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘magrittr’ ‘Rcpp’
      All declared Imports should be used.
    ```

# janeaustenr

Version: 0.1.5

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
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

# jstor

Version: 0.3.3

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > 
      > test_check("jstor")
      ── 1. Failure: jst_define_import validates input (@test-import-spec.R#17)  ─────
      `jst_define_import(article = jst_get_book)` did not throw an error.
      
      ── 2. Failure: jst_define_import validates input (@test-import-spec.R#18)  ─────
      `jst_define_import(...)` did not throw an error.
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 238 SKIPPED: 3 FAILED: 2
      1. Failure: jst_define_import validates input (@test-import-spec.R#17) 
      2. Failure: jst_define_import validates input (@test-import-spec.R#18) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘curl’ ‘readxl’
      All declared Imports should be used.
    ```

# kableExtra

Version: 0.9.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘magick’
    ```

# kitagawa

Version: 2.2-2

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'Tohoku.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
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
      installed size is  7.6Mb
      sub-directories of 1Mb or more:
        data   7.4Mb
    ```

# landscapemetrics

Version: 0.2

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking: ‘stars’ ‘sf’
    ```

# LexisNexisTools

Version: 0.2.0

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    	...metadata extracted [0.037 secs]
    	...article texts extracted [0.042 secs]
    	...paragraphs extracted [0.051 secs]
    	...superfluous whitespace removed from articles [0.055 secs]
    	...superfluous whitespace removed from paragraphs [0.06 secs]
    Elapsed time: 0.06 secs
    > 
    > docs <- lnt_convert(LNToutput, to = "rDNA")
    > 
    > corpus <- lnt_convert(LNToutput, to = "quanteda")
    > 
    > dbloc <- lnt_convert(LNToutput, to = "lnt2SQLite")
    > 
    > tCorpus <- lnt_convert(LNToutput, to = "corpustools")
    > 
    > tidy <- lnt_convert(LNToutput, to = "tidytext")
    Error in check_input(x) : 
      Input must be a character vector of any length or a list of character
      vectors, each of which has a length of 1.
    Calls: lnt_convert ... tokenfunc -> tf -> tokenize_words.default -> check_input
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      	...processing date 2010-01-09: 0 duplicates found [0.049 secs]. 		
      	...processing date 2010-01-10: 0 duplicates found [0.057 secs]. 		
      	...processing date 2010-01-11: 8 duplicates found [4.01 secs]. 		
      Threshold = 0.99; 4 days processed; 4 duplicates found; in 4.01 secsChecking similiarity for 10 articles over 3 dates...
      	...quanteda dfm construced for similarity comparison [0.057 secs].
      	...processing date 2010-01-08: 0 duplicates found [0.057 secs]. 		
      	...processing date 2010-01-10: 0 duplicates found [0.066 secs]. 		
      	...processing date 2010-01-11: 9 duplicates found [4.10 secs]. 		
      Threshold = 0.99; 3 days processed; 4 duplicates found; in 4.10 secsCreating LNToutput from input 1 files...
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 45 SKIPPED: 0 FAILED: 1
      1. Error: Convert LNToutput to tidytext (@test-lnt_convert.R#47) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    LexisNexisTools Version 0.2.0
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Quitting from lines 164-175 (demo.Rmd) 
    Error: processing vignette 'demo.Rmd' failed with diagnostics:
    Input must be a character vector of any length or a list of character
      vectors, each of which has a length of 1.
    Execution halted
    ```

# lilikoi

Version: 0.1.0

## In both

*   checking whether package ‘lilikoi’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/lilikoi/new/lilikoi.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘lilikoi’ ...
** package ‘lilikoi’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Unable to find any JVMs matching version "(null)".
No Java runtime present, try --request to install.
Warning in system("/usr/libexec/java_home", intern = TRUE) :
  running command '/usr/libexec/java_home' had status 1
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/lilikoi/rJava/libs/rJava.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/lilikoi/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/lilikoi/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘lilikoi’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/lilikoi/new/lilikoi.Rcheck/lilikoi’

```
### CRAN

```
* installing *source* package ‘lilikoi’ ...
** package ‘lilikoi’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Unable to find any JVMs matching version "(null)".
No Java runtime present, try --request to install.
Warning in system("/usr/libexec/java_home", intern = TRUE) :
  running command '/usr/libexec/java_home' had status 1
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/lilikoi/rJava/libs/rJava.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/lilikoi/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/lilikoi/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘lilikoi’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/lilikoi/old/lilikoi.Rcheck/lilikoi’

```
# linguisticsdown

Version: 1.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘tidyr’
      All declared Imports should be used.
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
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/LocFDRPois/new/LocFDRPois.Rcheck/00_pkg_src/LocFDRPois/R/locfdr.R:84)
    LLConstructor : LL: no visible global function definition for ‘dpois’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/LocFDRPois/new/LocFDRPois.Rcheck/00_pkg_src/LocFDRPois/R/locfdr.R:60)
    LLConstructor : LL: no visible global function definition for ‘dpois’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/LocFDRPois/new/LocFDRPois.Rcheck/00_pkg_src/LocFDRPois/R/locfdr.R:63-64)
    MixtureDensity: no visible global function definition for ‘glm’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/LocFDRPois/new/LocFDRPois.Rcheck/00_pkg_src/LocFDRPois/R/locfdr.R:35)
    MixtureDensity : f_hat: no visible global function definition for
      ‘predict’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/LocFDRPois/new/LocFDRPois.Rcheck/00_pkg_src/LocFDRPois/R/locfdr.R:42)
    NullDensity : f0: no visible global function definition for ‘dpois’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/LocFDRPois/new/LocFDRPois.Rcheck/00_pkg_src/LocFDRPois/R/locfdr.R:106)
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
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/loopr/new/loopr.Rcheck/00_pkg_src/loopr/R/loopr.R:96-104)
    fillColumns: no visible global function definition for ‘setNames’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/loopr/new/loopr.Rcheck/00_pkg_src/loopr/R/loopr.R:126-136)
    Undefined global functions or variables:
      setNames
    Consider adding
      importFrom("stats", "setNames")
    to your NAMESPACE file.
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

# lpirfs

Version: 0.1.3

## In both

*   checking whether package ‘lpirfs’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/lpirfs/new/lpirfs.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘lpirfs’ ...
** package ‘lpirfs’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/lpirfs/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/lpirfs/RcppArmadillo/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c RcppExports.cpp -o RcppExports.o
clang++ -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/lpirfs/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/lpirfs/RcppArmadillo/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c get_vals_lagcrit.cpp -o get_vals_lagcrit.o
clang++ -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/lpirfs/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/lpirfs/RcppArmadillo/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c hp_filter.cpp -o hp_filter.o
clang++ -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/lpirfs/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/lpirfs/RcppArmadillo/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c newey_west.cpp -o newey_west.o
clang++ -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/lpirfs/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/lpirfs/RcppArmadillo/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c newey_west_tsls.cpp -o newey_west_tsls.o
clang++ -std=c++11 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o lpirfs.so RcppExports.o get_vals_lagcrit.o hp_filter.o newey_west.o newey_west_tsls.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0 -L/usr/local/gfortran/lib -lgfortran -lquadmath -lm -fopenmp -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
clang: error: unsupported option '-fopenmp'
make: *** [lpirfs.so] Error 1
ERROR: compilation failed for package ‘lpirfs’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/lpirfs/new/lpirfs.Rcheck/lpirfs’

```
### CRAN

```
* installing *source* package ‘lpirfs’ ...
** package ‘lpirfs’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/lpirfs/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/lpirfs/RcppArmadillo/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c RcppExports.cpp -o RcppExports.o
clang++ -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/lpirfs/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/lpirfs/RcppArmadillo/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c get_vals_lagcrit.cpp -o get_vals_lagcrit.o
clang++ -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/lpirfs/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/lpirfs/RcppArmadillo/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c hp_filter.cpp -o hp_filter.o
clang++ -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/lpirfs/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/lpirfs/RcppArmadillo/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c newey_west.cpp -o newey_west.o
clang++ -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/lpirfs/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/lpirfs/RcppArmadillo/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c newey_west_tsls.cpp -o newey_west_tsls.o
clang++ -std=c++11 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o lpirfs.so RcppExports.o get_vals_lagcrit.o hp_filter.o newey_west.o newey_west_tsls.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0 -L/usr/local/gfortran/lib -lgfortran -lquadmath -lm -fopenmp -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
clang: error: unsupported option '-fopenmp'
make: *** [lpirfs.so] Error 1
ERROR: compilation failed for package ‘lpirfs’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/lpirfs/old/lpirfs.Rcheck/lpirfs’

```
# lucid

Version: 1.6

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
    Quitting from lines 271-295 (lucid_examples.Rmd) 
    Error: processing vignette 'lucid_examples.Rmd' failed with diagnostics:
    could not find function "jags.model"
    Execution halted
    ```

# lvnet

Version: 0.3.3

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘OpenMx’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# LymphoSeq

Version: 1.8.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.4Mb
      sub-directories of 1Mb or more:
        doc       2.5Mb
        extdata   5.5Mb
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

Version: 0.0.2

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

# mapedit

Version: 0.4.3

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘geojsonio’
    ```

# mapview

Version: 2.6.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        extdata   1.0Mb
        R         2.0Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘stars’
      All declared Imports should be used.
    ```

# markmyassignment

Version: 0.8.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘lazyeval’
      All declared Imports should be used.
    ```

# mase

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘MASS’
      All declared Imports should be used.
    ```

# mason

Version: 0.2.6

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘ggplot2’, ‘pander’, ‘pixiedust’
    ```

# mathpix

Version: 0.3.0

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘magick’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
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
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mbgraphic/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c RcppExports.cpp -o RcppExports.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mbgraphic/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c cmasum.cpp -o cmasum.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mbgraphic/Rcpp/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c mbgraphic_init.c -o mbgraphic_init.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mbgraphic/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c variableflip.cpp -o variableflip.o
clang++ -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o mbgraphic.so RcppExports.o cmasum.o mbgraphic_init.o variableflip.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/lionel/Desktop/rlang/revdep/checks.noindex/mbgraphic/new/mbgraphic.Rcheck/mbgraphic/libs
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Unable to find any JVMs matching version "(null)".
No Java runtime present, try --request to install.
Warning in system("/usr/libexec/java_home", intern = TRUE) :
  running command '/usr/libexec/java_home' had status 1
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/mbgraphic/rJava/libs/rJava.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/mbgraphic/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/mbgraphic/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘mbgraphic’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/mbgraphic/new/mbgraphic.Rcheck/mbgraphic’

```
### CRAN

```
* installing *source* package ‘mbgraphic’ ...
** package ‘mbgraphic’ successfully unpacked and MD5 sums checked
** libs
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mbgraphic/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c RcppExports.cpp -o RcppExports.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mbgraphic/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c cmasum.cpp -o cmasum.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mbgraphic/Rcpp/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c mbgraphic_init.c -o mbgraphic_init.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mbgraphic/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c variableflip.cpp -o variableflip.o
clang++ -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o mbgraphic.so RcppExports.o cmasum.o mbgraphic_init.o variableflip.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/lionel/Desktop/rlang/revdep/checks.noindex/mbgraphic/old/mbgraphic.Rcheck/mbgraphic/libs
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Unable to find any JVMs matching version "(null)".
No Java runtime present, try --request to install.
Warning in system("/usr/libexec/java_home", intern = TRUE) :
  running command '/usr/libexec/java_home' had status 1
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/mbgraphic/rJava/libs/rJava.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/mbgraphic/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/mbgraphic/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘mbgraphic’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/mbgraphic/old/mbgraphic.Rcheck/mbgraphic’

```
# MCbiclust

Version: 1.4.0

## In both

*   checking package dependencies ... ERROR
    ```
    Packages required but not available: ‘GO.db’ ‘org.Hs.eg.db’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# mdsr

Version: 0.1.6

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        data   5.5Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2694 marked UTF-8 strings
    ```

# mem

Version: 2.13

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘sm’
    
    Package suggested but not available for checking: ‘magick’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# memapp

Version: 2.10

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘magick’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘DT’ ‘foreign’ ‘formattable’ ‘ggplot2’ ‘haven’ ‘magrittr’
      ‘mem’ ‘openxlsx’ ‘plotly’ ‘RColorBrewer’ ‘readxl’ ‘RODBC’ ‘shinyBS’
      ‘shinydashboard’ ‘shinyjs’ ‘shinythemes’ ‘stringi’ ‘stringr’ ‘tidyr’
      All declared Imports should be used.
    ```

# memery

Version: 0.5.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘magick’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘colourpicker’ ‘shinyBS’ ‘shinycssloaders’
      All declared Imports should be used.
    ```

# metacoder

Version: 0.3.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ggrepel’ ‘reshape’ ‘svglite’
      All declared Imports should be used.
    ```

# MetaCyto

Version: 1.2.1

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    collectData: no visible binding for global variable ‘value’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/MetaCyto/new/MetaCyto.Rcheck/00_pkg_src/MetaCyto/R/collectData.R:27)
    panelSummary: no visible binding for global variable ‘antibodies’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/MetaCyto/new/MetaCyto.Rcheck/00_pkg_src/MetaCyto/R/panelSummary.R:34)
    panelSummary: no visible binding for global variable ‘value’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/MetaCyto/new/MetaCyto.Rcheck/00_pkg_src/MetaCyto/R/panelSummary.R:34)
    plotGA: no visible binding for global variable ‘lower’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/MetaCyto/new/MetaCyto.Rcheck/00_pkg_src/MetaCyto/R/plotGA.R:33-39)
    plotGA: no visible binding for global variable ‘upper’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/MetaCyto/new/MetaCyto.Rcheck/00_pkg_src/MetaCyto/R/plotGA.R:33-39)
    searchCluster : <anonymous>: no visible binding for global variable
      ‘triS’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/MetaCyto/new/MetaCyto.Rcheck/00_pkg_src/MetaCyto/R/searchCluster.R:102)
    searchCluster : <anonymous>: no visible binding for global variable
      ‘triS’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/MetaCyto/new/MetaCyto.Rcheck/00_pkg_src/MetaCyto/R/searchCluster.R:103)
    searchCluster : <anonymous>: no visible binding for global variable
      ‘triS’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/MetaCyto/new/MetaCyto.Rcheck/00_pkg_src/MetaCyto/R/searchCluster.R:104)
    Undefined global functions or variables:
      antibodies lower parameter_name triS upper value
    ```

# metaDigitise

Version: 1.0.0

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘magick’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# metagene

Version: 2.12.1

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
    All user-level objects in a package (including S4 classes and
    methods) should have documentation entries.
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
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/metagenomeFeatures/new/metagenomeFeatures.Rcheck/00_pkg_src/metagenomeFeatures/R/mgDb_method_select.R:96-97)
    .select.taxa: no visible binding for global variable ‘Keys’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/metagenomeFeatures/new/metagenomeFeatures.Rcheck/00_pkg_src/metagenomeFeatures/R/mgDb_method_select.R:21)
    .select.taxa: no visible binding for global variable ‘.’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/metagenomeFeatures/new/metagenomeFeatures.Rcheck/00_pkg_src/metagenomeFeatures/R/mgDb_method_select.R:21)
    get_gg13.8_85MgDb: no visible binding for global variable ‘metadata’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/metagenomeFeatures/new/metagenomeFeatures.Rcheck/00_pkg_src/metagenomeFeatures/R/gg13.8_85MgDb.R:23-25)
    Undefined global functions or variables:
      . identifier Keys metadata
    ```

# MetaIntegrator

Version: 2.0.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        data   1.9Mb
        doc    2.2Mb
        R      1.0Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DT’ ‘GEOmetadb’ ‘gplots’ ‘pheatmap’ ‘readr’ ‘RMySQL’ ‘RSQLite’
      All declared Imports should be used.
    ```

# MetamapsDB

Version: 0.0.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Matrix’ ‘shiny’
      All declared Imports should be used.
    ```

# methyvim

Version: 1.2.0

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘methyvim-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: methyheat
    > ### Title: Heatmap for methytmle objects
    > ### Aliases: methyheat
    > 
    > ### ** Examples
    > 
    > suppressMessages(library(SummarizedExperiment))
    > library(methyvimData)
    Error in library(methyvimData) : 
      there is no package called ‘methyvimData’
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── 5. Error: (unknown) (@test-tmle_classic.R#5)  ───────────────────────────────
      there is no package called 'methyvimData'
      1: library(methyvimData) at testthat/test-tmle_classic.R:5
      2: stop(txt, domain = NA)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 15 SKIPPED: 0 FAILED: 5
      1. Error: (unknown) (@test-cluster_sites.R#4) 
      2. Error: (unknown) (@test-methytmle_class.R#5) 
      3. Error: (unknown) (@test-methyvim.R#7) 
      4. Error: (unknown) (@test-screen_limma.R#4) 
      5. Error: (unknown) (@test-tmle_classic.R#5) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 174-177 (using_methyvim.Rmd) 
    Error: processing vignette 'using_methyvim.Rmd' failed with diagnostics:
    there is no package called 'methyvimData'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘minfiData’ ‘methyvimData’
    ```

# mfa

Version: 1.2.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Sampling for 100 cells and 40 genes
    Warning: Computation failed in `stat_smooth()`:
    object 'LOESS' of mode 'function' was not found
    convert: profile 'icc': 'RGB ': RGB color space not permitted on grayscale PNG `introduction_to_mfa_files/figure-html/diagnostics-1.png' @ warning/png.c/MagickPNGWarningHandler/1656.
    convert: profile 'icc': 'RGB ': RGB color space not permitted on grayscale PNG `introduction_to_mfa_files/figure-html/diagnostics-2.png' @ warning/png.c/MagickPNGWarningHandler/1656.
    convert: profile 'icc': 'RGB ': RGB color space not permitted on grayscale PNG `introduction_to_mfa_files/figure-html/plot-chi-1.png' @ warning/png.c/MagickPNGWarningHandler/1656.
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'introduction_to_mfa.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
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
clang++ -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/miceFast/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/miceFast/RcppArmadillo/include" -I/usr/local/include  -fopenmp -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c R_funs.cpp -o R_funs.o
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
clang++ -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/miceFast/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/miceFast/RcppArmadillo/include" -I/usr/local/include  -fopenmp -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c R_funs.cpp -o R_funs.o
clang: error: unsupported option '-fopenmp'
make: *** [R_funs.o] Error 1
ERROR: compilation failed for package ‘miceFast’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/miceFast/old/miceFast.Rcheck/miceFast’

```
# microbiome

Version: 1.2.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Richness
    Observed (richness 0)
    Diversity
    Evenness
    Dominance
    Rarity
    Computing bootsrapped smoothers ...
    Convert to long
    Computing density estimates for the vertical cuts ...
    Vertical cross-sectional density estimate
    Tile approach
    Build ggplot...
    convert: profile 'icc': 'RGB ': RGB color space not permitted on grayscale PNG `vignette_files/figure-html/corevisu-1.png' @ warning/png.c/MagickPNGWarningHandler/1656.
    Warning: Removed 1 rows containing non-finite values (stat_bin).
    Warning: Removed 1 rows containing missing values (geom_bar).
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'vignette.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# Miso

Version: 0.1.5

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# mixOmics

Version: 6.3.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.7Mb
      sub-directories of 1Mb or more:
        data   4.0Mb
        R      5.1Mb
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

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘doParallel’ ‘iterators’ ‘parallel’
      All declared Imports should be used.
    ```

# mleap

Version: 0.1.2

## In both

*   checking whether the package can be loaded ... ERROR
    ```
    Loading this package had a fatal error status code 1
    Loading log:
    Unable to find any JVMs matching version "(null)".
    No Java runtime present, try --request to install.
    Error: package or namespace load failed for ‘mleap’:
     .onLoad failed in loadNamespace() for 'mleap', details:
      call: NULL
      error: .onLoad failed in loadNamespace() for 'rJava', details:
      call: dyn.load(file, DLLpath = DLLpath, ...)
      error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/mleap/rJava/libs/rJava.so':
      dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/mleap/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
      Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/mleap/rJava/libs/rJava.so
      Reason: image not found
    In addition: Warning message:
    In system("/usr/libexec/java_home", intern = TRUE) :
      running command '/usr/libexec/java_home' had status 1
    Execution halted
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

# MLZ

Version: 0.1.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 15.7Mb
      sub-directories of 1Mb or more:
        libs  15.0Mb
    ```

# modelgrid

Version: 1.1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ggplot2’ ‘lattice’
      All declared Imports should be used.
    ```

# modelr

Version: 0.1.2

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘lme4’, ‘rstanarm’
    ```

# momentuHMM

Version: 1.4.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.9Mb
      sub-directories of 1Mb or more:
        data   2.1Mb
        doc    1.7Mb
        R      3.1Mb
    ```

# Momocs

Version: 1.2.9

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        R   3.1Mb
    ```

# MonetDBLite

Version: 0.6.0

## Newly broken

*   R CMD check timed out
    

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.2Mb
      sub-directories of 1Mb or more:
        libs   8.5Mb
    ```

# monkeylearn

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ratelimitr’
      All declared Imports should be used.
    ```

# monocle

Version: 2.8.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘biocViews’ ‘Rcpp’
      All declared Imports should be used.
    Missing or unexported objects:
      ‘scater::newSCESet’ ‘VGAM::gaussianff’
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    importCDS: no visible global function definition for ‘gaussianff’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/monocle/new/monocle.Rcheck/00_pkg_src/monocle/R/cds_conversion.R:235)
    make_canonical: no visible global function definition for ‘nei’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/monocle/new/monocle.Rcheck/00_pkg_src/monocle/R/order_cells.R:297)
    make_canonical: no visible global function definition for ‘nei’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/monocle/new/monocle.Rcheck/00_pkg_src/monocle/R/order_cells.R:298)
    measure_diameter_path: no visible global function definition for ‘nei’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/monocle/new/monocle.Rcheck/00_pkg_src/monocle/R/order_cells.R:470-481)
    orderCells: no visible binding for '<<-' assignment to ‘next_node’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/monocle/new/monocle.Rcheck/00_pkg_src/monocle/R/order_cells.R:1097)
    plot_multiple_branches_pseudotime: no visible binding for global
      variable ‘pseudocount’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/monocle/new/monocle.Rcheck/00_pkg_src/monocle/R/plotting.R:2740)
    plot_multiple_branches_pseudotime: no visible binding for global
      variable ‘Branch’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/monocle/new/monocle.Rcheck/00_pkg_src/monocle/R/plotting.R:2753)
    project2MST: no visible global function definition for ‘nei’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/monocle/new/monocle.Rcheck/00_pkg_src/monocle/R/order_cells.R:1606)
    Undefined global functions or variables:
      Branch gaussianff nei next_node pseudocount Size_Factor
      use_for_ordering
    ```

*   checking files in ‘vignettes’ ... NOTE
    ```
    The following directory looks like a leftover from 'knitr':
      ‘figure’
    Please remove from your package.
    ```

# moonBook

Version: 0.2.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘purrr’
      All declared Imports should be used.
    ```

# morse

Version: 3.1.1

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
# mortAAR

Version: 1.0.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Loading required package: bitops
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'mortAAR_vignette_extended.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# MortalityLaws

Version: 1.6.0

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

# mosaic

Version: 1.4.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘manipulate’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.6Mb
      sub-directories of 1Mb or more:
        doc   1.9Mb
        R     4.2Mb
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘cubature’
    ```

# mosaicData

Version: 0.17.0

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
      ‘caret’ ‘ggformula’ ‘knitr’ ‘MASS’ ‘testthat’ ‘tidyverse’
      All declared Imports should be used.
    ```

# mplot

Version: 1.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Unable to find any JVMs matching version "(null)".
    No Java runtime present, try --request to install.
    ```

# MRFcov

Version: 1.0.35

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘penalized’
    ```

# mrgsolve

Version: 0.8.12

## In both

*   checking whether package ‘mrgsolve’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/mrgsolve/new/mrgsolve.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘mrgsolve’ ...
** package ‘mrgsolve’ successfully unpacked and MD5 sums checked
** libs
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/BH/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c RcppExports.cpp -o RcppExports.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/BH/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c dataobject.cpp -o dataobject.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/BH/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c datarecord.cpp -o datarecord.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/BH/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c devtran.cpp -o devtran.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/BH/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c housemodel-mread-source.cpp -o housemodel-mread-source.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/BH/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c mrgsolve.cpp -o mrgsolve.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/BH/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c mrgsolve_init.cpp -o mrgsolve_init.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/BH/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c odepack_dlsoda.cpp -o odepack_dlsoda.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/BH/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c odeproblem.cpp -o odeproblem.o
gfortran-7   -fPIC  -g -O2  -c opk_dlsoda_mrg.f -o opk_dlsoda_mrg.o
make: gfortran-7: No such file or directory
make: *** [opk_dlsoda_mrg.o] Error 1
ERROR: compilation failed for package ‘mrgsolve’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/mrgsolve/new/mrgsolve.Rcheck/mrgsolve’

```
### CRAN

```
* installing *source* package ‘mrgsolve’ ...
** package ‘mrgsolve’ successfully unpacked and MD5 sums checked
** libs
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/BH/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c RcppExports.cpp -o RcppExports.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/BH/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c dataobject.cpp -o dataobject.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/BH/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c datarecord.cpp -o datarecord.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/BH/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c devtran.cpp -o devtran.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/BH/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c housemodel-mread-source.cpp -o housemodel-mread-source.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/BH/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c mrgsolve.cpp -o mrgsolve.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/BH/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c mrgsolve_init.cpp -o mrgsolve_init.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/BH/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c odepack_dlsoda.cpp -o odepack_dlsoda.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/mrgsolve/BH/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c odeproblem.cpp -o odeproblem.o
gfortran-7   -fPIC  -g -O2  -c opk_dlsoda_mrg.f -o opk_dlsoda_mrg.o
make: gfortran-7: No such file or directory
make: *** [opk_dlsoda_mrg.o] Error 1
ERROR: compilation failed for package ‘mrgsolve’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/mrgsolve/old/mrgsolve.Rcheck/mrgsolve’

```
# msigdbr

Version: 6.1.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        R   5.1Mb
    ```

# MSnID

Version: 1.14.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/MSnID/new/MSnID.Rcheck/00_pkg_src/MSnID/R/MSnID-methods.R:600)
    infer_parsimonious_accessions,MSnID : infer_acc: no visible binding for
      global variable ‘N’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/MSnID/new/MSnID.Rcheck/00_pkg_src/MSnID/R/MSnID-methods.R:600)
    infer_parsimonious_accessions,MSnID : infer_acc: no visible binding for
      global variable ‘accession’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/MSnID/new/MSnID.Rcheck/00_pkg_src/MSnID/R/MSnID-methods.R:601)
    infer_parsimonious_accessions,MSnID : infer_acc: no visible binding for
      global variable ‘pepSeq’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/MSnID/new/MSnID.Rcheck/00_pkg_src/MSnID/R/MSnID-methods.R:603)
    recalibrate,MSnID: no visible global function definition for ‘median’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/MSnID/new/MSnID.Rcheck/00_pkg_src/MSnID/R/MSnID-methods.R:520)
    recalibrate,MSnID: no visible global function definition for ‘density’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/MSnID/new/MSnID.Rcheck/00_pkg_src/MSnID/R/MSnID-methods.R:529)
    Undefined global functions or variables:
      accession DatabaseAccess DatabaseDescription DBseqLength density i
      location mass median modification N name optim pepSeq quantile rnorm
      spectrumID
    Consider adding
      importFrom("stats", "density", "median", "optim", "quantile", "rnorm")
    to your NAMESPACE file.
    ```

# MSstats

Version: 3.12.3

## In both

*   R CMD check timed out
    

*   checking R code for possible problems ... NOTE
    ```
    ...
    SpectronauttoMSstatsFormat: no visible binding for global variable
      ‘missing.col’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/MSstats/new/MSstats.Rcheck/00_pkg_src/MSstats/R/SpectronauttoMSstatsFormat.R:46-47)
    SpectronauttoMSstatsFormat: no visible binding for global variable
      ‘fea’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/MSstats/new/MSstats.Rcheck/00_pkg_src/MSstats/R/SpectronauttoMSstatsFormat.R:188)
    SpectronauttoMSstatsFormat: no visible binding for global variable
      ‘Intensity’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/MSstats/new/MSstats.Rcheck/00_pkg_src/MSstats/R/SpectronauttoMSstatsFormat.R:188)
    SpectronauttoMSstatsFormat: no visible binding for global variable
      ‘PeptideSequence’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/MSstats/new/MSstats.Rcheck/00_pkg_src/MSstats/R/SpectronauttoMSstatsFormat.R:214)
    SpectronauttoMSstatsFormat: no visible binding for global variable
      ‘ProteinName’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/MSstats/new/MSstats.Rcheck/00_pkg_src/MSstats/R/SpectronauttoMSstatsFormat.R:214)
    Undefined global functions or variables:
      ABUNDANCE aggr_Fragment_Annotation aggr_Peak_Area analysis ciw
      datafeature fea FEATURE FRACTION Intensity label LABEL logFC Mean
      missing.col Name ncount ount PeptideSequence Protein Protein_number
      ProteinName residual RUN Selected_fragments Selected_peptides shape
      Train_size weight x y ymax ymin
    ```

# MSstatsQC

Version: 1.2.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘RforProteomics’
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

# multicolor

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘cowsay’
      All declared Imports should be used.
    ```

# multistateutils

Version: 1.2.0

## In both

*   checking examples ... ERROR
    ```
    ...
    + })
    > 
    > # New individual to estimate transition probabilities for
    > newdata <- data.frame(age="20-40", dissub="AML")
    > 
    > # Estimate length of stay in each state after a year, given starting in state 1
    > length_of_stay(models, 
    +                newdata=newdata,
    +                tmat, times=365.25,
    +                start=1)
    
     *** caught illegal operation ***
    address 0x10a08c198, cause 'illegal opcode'
    
    Traceback:
     1: desCpp(transitions, trans_mat, newdata_mat, start_times, start_states -     1, tcovs)
     2: data.table::as.data.table(desCpp(transitions, trans_mat, newdata_mat,     start_times, start_states - 1, tcovs))
     3: run_sim(transition_list, attr_mat, trans_mat, tcovs, start_times,     start_states)
     4: state_occupancy(models, trans_mat, newdata_ext, tcovs, initial_times,     start_states, ci, M, agelimit, agecol, agescale)
     5: length_of_stay(models, newdata = newdata, tmat, times = 365.25,     start = 1)
    An irrecoverable exception occurred. R is aborting now ...
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      26: tryCatchList(expr, classes, parentenv, handlers)
      27: tryCatch(withCallingHandlers({    eval(code, test_env)    if (!handled && !is.null(test)) {        skip_empty()    }}, expectation = handle_expectation, skip = handle_skip, warning = handle_warning,     message = handle_message, error = handle_error), error = handle_fatal,     skip = function(e) {    })
      28: test_code(NULL, exprs, env)
      29: source_file(path, new.env(parent = env), chdir = TRUE, wrap = wrap)
      30: force(code)
      31: with_reporter(reporter = reporter, start_end_reporter = start_end_reporter,     {        lister$start_file(basename(path))        source_file(path, new.env(parent = env), chdir = TRUE,             wrap = wrap)        end_context()    })
      32: FUN(X[[i]], ...)
      33: lapply(paths, test_file, env = env, reporter = current_reporter,     start_end_reporter = FALSE, load_helpers = FALSE, wrap = wrap)
      34: force(code)
      35: with_reporter(reporter = current_reporter, results <- lapply(paths,     test_file, env = env, reporter = current_reporter, start_end_reporter = FALSE,     load_helpers = FALSE, wrap = wrap))
      36: test_files(paths, reporter = reporter, env = env, stop_on_failure = stop_on_failure,     stop_on_warning = stop_on_warning, wrap = wrap)
      37: test_dir(path = test_path, reporter = reporter, env = env, filter = filter,     ..., stop_on_failure = stop_on_failure, stop_on_warning = stop_on_warning,     wrap = wrap)
      38: test_package_dir(package = package, test_path = test_path, filter = filter,     reporter = reporter, ..., stop_on_failure = stop_on_failure,     stop_on_warning = stop_on_warning, wrap = wrap)
      39: test_check("multistateutils")
      An irrecoverable exception occurred. R is aborting now ...
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    11: timing_fn(handle(ev <- withCallingHandlers(withVisible(eval(expr,     envir, enclos)), warning = wHandler, error = eHandler, message = mHandler)))
    12: evaluate_call(expr, parsed$src[[i]], envir = envir, enclos = enclos,     debug = debug, last = i == length(out), use_try = stop_on_error !=         2L, keep_warning = keep_warning, keep_message = keep_message,     output_handler = output_handler, include_timing = include_timing)
    13: evaluate::evaluate(...)
    14: evaluate(code, envir = env, new_device = FALSE, keep_warning = !isFALSE(options$warning),     keep_message = !isFALSE(options$message), stop_on_error = if (options$error &&         options$include) 0L else 2L, output_handler = knit_handlers(options$render,         options))
    15: in_dir(input_dir(), evaluate(code, envir = env, new_device = FALSE,     keep_warning = !isFALSE(options$warning), keep_message = !isFALSE(options$message),     stop_on_error = if (options$error && options$include) 0L else 2L,     output_handler = knit_handlers(options$render, options)))
    16: block_exec(params)
    17: call_block(x)
    18: process_group.block(group)
    19: process_group(group)
    20: withCallingHandlers(if (tangle) process_tangle(group) else process_group(group),     error = function(e) {        setwd(wd)        cat(res, sep = "\n", file = output %n% "")        message("Quitting from lines ", paste(current_lines(i),             collapse = "-"), " (", knit_concord$get("infile"),             ") ")    })
    21: process_file(text, output)
    22: knitr::knit(knit_input, knit_output, envir = envir, quiet = quiet,     encoding = encoding)
    23: rmarkdown::render(file, encoding = encoding, quiet = quiet, envir = globalenv())
    24: vweave_rmarkdown(...)
    25: engine$weave(file, quiet = quiet, encoding = enc)
    26: doTryCatch(return(expr), name, parentenv, handler)
    27: tryCatchOne(expr, names, parentenv, handlers[[1L]])
    28: tryCatchList(expr, classes, parentenv, handlers)
    29: tryCatch({    engine$weave(file, quiet = quiet, encoding = enc)    setwd(startdir)    find_vignette_product(name, by = "weave", engine = engine)}, error = function(e) {    stop(gettextf("processing vignette '%s' failed with diagnostics:\n%s",         file, conditionMessage(e)), domain = NA, call. = FALSE)})
    30: buildVignettes(dir = "/Users/lionel/Desktop/rlang/revdep/checks.noindex/multistateutils/new/multistateutils.Rcheck/vign_test/multistateutils")
    An irrecoverable exception occurred. R is aborting now ...
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Rcpp’ ‘survival’
      All declared Imports should be used.
    ```

# MXM

Version: 1.4.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 12.0Mb
      sub-directories of 1Mb or more:
        doc   1.3Mb
        R    10.1Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘knitr’
      All declared Imports should be used.
    ```

# myTAI

Version: 0.8.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.7Mb
      sub-directories of 1Mb or more:
        data   2.0Mb
        doc    2.4Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘biomartr’
      All declared Imports should be used.
    ```

# naniar

Version: 0.4.0.0

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      8: withVisible(function_list[[k]](value))
      9: function_list[[k]](value)
      10: dplyr::mutate(., any_missing = label_shadow(., !!!quo_vars, missing = missing, complete = complete))
      11: mutate.tbl_df(., any_missing = label_shadow(., !!!quo_vars, missing = missing, complete = complete))
      12: mutate_impl(.data, dots)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 456 SKIPPED: 23 FAILED: 4
      1. Error: add_label_shadow returns a tibble (@test-add-label-shadow.R#7) 
      2. Error: add_label_shadow adds the right number of columns (@test-add-label-shadow.R#13) 
      3. Error: add_label_shadow adds a column with suffix 'any_missing' (@test-add-label-shadow.R#20) 
      4. Error: (unknown) (@test-add-label-shadow.R#45) 
      
      Error: testthat unit tests failed
      Execution halted
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

Version: 1.2.0

## In both

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

# NetworkExtinction

Version: 0.1.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    1
    2
    3
    4
    5
    6
    7
    8
    Simulation 100 of 100 ready
    Warning in chisq.test(x = Hypothesis$DF$AccSecondaryExtinction, y = Nullmodel$sims$AccSecondaryExtinction[1:length(Hypothesis$DF$AccSecondaryExtinction)]) :
      Chi-squared approximation may be incorrect
    Joining, by = c("K", "Cumulative", "Scenario", "Exp")
    Joining, by = c("K", "Cumulative", "Scenario", "Exp", "power")
    Joining, by = c("sigma", "isConv", "finTol", "logLik", "AIC", "BIC", "deviance", "df.residual", "model")
    Joining, by = c("sigma", "isConv", "finTol", "logLik", "AIC", "BIC", "deviance", "df.residual", "model")
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'VignetteNetworkExt.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
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
      installed size is  8.6Mb
      sub-directories of 1Mb or more:
        data   8.0Mb
    ```

# nlmixr

Version: 1.0.0-7

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.9Mb
      sub-directories of 1Mb or more:
        libs   1.7Mb
        R      3.0Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘numDeriv’ ‘PreciseSums’
      All declared Imports should be used.
    ```

# NLMR

Version: 0.3.2

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'getstarted.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# nlshelper

Version: 0.2

## In both

*   checking whether package ‘nlshelper’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/nlshelper/new/nlshelper.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘nlshelper’ ...
** package ‘nlshelper’ successfully unpacked and MD5 sums checked
** R
** byte-compile and prepare package for lazy loading
Error : package ‘sm’ required by ‘magicaxis’ could not be found
ERROR: lazy loading failed for package ‘nlshelper’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/nlshelper/new/nlshelper.Rcheck/nlshelper’

```
### CRAN

```
* installing *source* package ‘nlshelper’ ...
** package ‘nlshelper’ successfully unpacked and MD5 sums checked
** R
** byte-compile and prepare package for lazy loading
Error : package ‘sm’ required by ‘magicaxis’ could not be found
ERROR: lazy loading failed for package ‘nlshelper’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/nlshelper/old/nlshelper.Rcheck/nlshelper’

```
# nlstimedist

Version: 1.1.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(testthat)
      > library(nlstimedist)
      > 
      > test_check("nlstimedist")
      ── 1. Failure: Ensure the glance method is returning expected values (@test-glan
      `out` not equal to `expect`.
      Rows in x but not y: 1. Rows in y but not x: 1. 
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 6 SKIPPED: 0 FAILED: 1
      1. Failure: Ensure the glance method is returning expected values (@test-glance.timedist.R#19) 
      
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
    Error: processing vignette 'nlstimedist.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
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
      ‘choroplethr’ ‘choroplethrMaps’ ‘data.table’ ‘forcats’
      ‘hurricaneexposure’ ‘plyr’ ‘RColorBrewer’ ‘XML’
      All declared Imports should be used.
    ```

# NOAAWeather

Version: 0.1.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        data   5.1Mb
    ```

# nos

Version: 1.1.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘bipartite’
    ```

# nucleR

Version: 2.12.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    The following object is masked from 'package:base':
    
        expand.grid
    
    Loading required package: GenomeInfoDb
    reading file /Users/lionel/Desktop/rlang/revdep/checks.noindex/nucleR/new/nucleR.Rcheck/nucleR/extdata/cellCycleM_chrII_5000-25000.bam
    processing flags
    processing strand +
    processing strand -
    Warning in exp - (ctr - mean(ctr[ctr != 0])) :
      longer object length is not a multiple of shorter object length
    Warning: Removed 1001 rows containing missing values (position_stack).
    [WARNING] Could not parse YAML metadata at line 1 column 1: :2:77: Unexpected '
      '
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'nucleR.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
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

# nycflights13

Version: 1.0.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.1Mb
      sub-directories of 1Mb or more:
        data   7.0Mb
    ```

# nzelect

Version: 0.4.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        data   5.0Mb
    ```

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

*   R CMD check timed out
    

*   checking installed package size ... NOTE
    ```
      installed size is  8.0Mb
      sub-directories of 1Mb or more:
        doc    5.4Mb
        libs   1.2Mb
    ```

# openair

Version: 2.5-0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.1Mb
      sub-directories of 1Mb or more:
        R   4.0Mb
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
      installed size is 15.5Mb
      sub-directories of 1Mb or more:
        extdata  10.2Mb
        R         4.2Mb
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

# Organism.dplyr

Version: 1.8.1

## Newly fixed

*   checking examples ... ERROR
    ```
    ...
       [4]    chr20 44626456-44626599      - |  501393      <NA>         4    ADA
       [5]    chr20 44625569-44625684      - |  501390      <NA>         5    ADA
       ...      ...               ...    ... .     ...       ...       ...    ...
       [8]    chr20 44622829-44622930      - |  501386      <NA>         8    ADA
       [9]    chr20 44622588-44622652      - |  501385      <NA>         9    ADA
      [10]    chr20 44621018-44621147      - |  501384      <NA>        10    ADA
      [11]    chr20 44620299-44620401      - |  501382      <NA>        11    ADA
      [12]    chr20 44619522-44619847      - |  501379      <NA>        12    ADA
    
    ...
    <1 more element>
    -------
    seqinfo: 455 sequences (1 circular) from hg38 genome
    > 
    > ## intronsByTranscript
    > intronsByTranscript(src, filter = SymbolFilter("ADA"))
    Warning: Closing open result set, pending rows
    Error in result_fetch(res@ptr, n = n) : Invalid result set
    Calls: intronsByTranscript ... tryCatchList -> dbFetch -> dbFetch -> .local -> result_fetch
    Warning: Expired, result set already closed
    Execution halted
    ```

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
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/Organism.dplyr/new/Organism.dplyr.Rcheck/00_pkg_src/Organism.dplyr/R/extractors.R:236)
    intronsByTranscript,src_organism: no visible binding for global
      variable ‘.’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/Organism.dplyr/new/Organism.dplyr.Rcheck/00_pkg_src/Organism.dplyr/R/extractor-methods.R:254-255)
    intronsByTranscript,src_organism: no visible binding for global
      variable ‘.’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/Organism.dplyr/new/Organism.dplyr.Rcheck/00_pkg_src/Organism.dplyr/R/extractor-methods.R:264-265)
    orgPackageName,src_organism: no visible binding for global variable
      ‘name’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/Organism.dplyr/new/Organism.dplyr.Rcheck/00_pkg_src/Organism.dplyr/R/src.R:432-433)
    orgPackageName,src_organism: no visible binding for global variable
      ‘organism’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/Organism.dplyr/new/Organism.dplyr.Rcheck/00_pkg_src/Organism.dplyr/R/src.R:434)
    orgPackageName,src_organism: no visible binding for global variable
      ‘OrgDb’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/Organism.dplyr/new/Organism.dplyr.Rcheck/00_pkg_src/Organism.dplyr/R/src.R:434)
    Undefined global functions or variables:
      . name organism OrgDb
    ```

# PakPC2017

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘stats’
      All declared Imports should be used.
    ```

# paletteer

Version: 0.1.0

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘paletteer-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: paletteer_d
    > ### Title: Get discrete palette by package and name
    > ### Aliases: paletteer_d
    > 
    > ### ** Examples
    > 
    > paletteer_d("nord", "frost")
    Error in match.arg(package, unique(paletteer::palettes_d_names$package)) : 
      'arg' should be one of “awtools”, “dichromat”, “dutchmasters”, “ggsci”, “ggpomological”, “ggthemes”, “ghibli”, “grDevices”, “jcolors”, “LaCroixColoR”, “NineteenEightyR”, “nord”, “ochRe”, “palettetown”, “pals”, “Polychrome”, “quickpalette”, “rcartocolor”, “RColorBrewer”, “Redmonder”, “RSkittleBrewer”, “wesanderson”, “yarrr”
    Calls: paletteer_d -> match.arg
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 50 SKIPPED: 0 FAILED: 14
      1. Error: paletteer_c return correct number of colors (@test-paletteer_c.R#4) 
      2. Error: paletteer_c returns characters (@test-paletteer_c.R#9) 
      3. Error: paletteer_c works when package and name are provided as symbols (@test-paletteer_c.R#17) 
      4. Error: direction works correctly in paletteer_c (@test-paletteer_c.R#23) 
      5. Error: paletteer_d return correct number of colors (@test-paletteer_d.R#4) 
      6. Error: paletteer_d returns characters (@test-paletteer_d.R#9) 
      7. Error: paletteer_d returns something when n is unspecified (@test-paletteer_d.R#13) 
      8. Error: paletteer_d return correct amount when type is set to continuous (@test-paletteer_d.R#21) 
      9. Error: paletteer_d works when package and name are provided as symbols (@test-paletteer_d.R#29) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘jcolors’ ‘oompaBase’ ‘palr’ ‘pals’ ‘viridisLite’
      All declared Imports should be used.
    ```

# parlitools

Version: 0.2.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 13 marked UTF-8 strings
    ```

# parsemsf

Version: 0.1.1

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
    
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'introduction.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

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

# PathwaySplice

Version: 1.4.0

## In both

*   checking package dependencies ... ERROR
    ```
    Packages required but not available: ‘org.Hs.eg.db’ ‘org.Mm.eg.db’ ‘GO.db’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# patternplot

Version: 0.2.1

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

Version: 1.1.2

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Joining, by = c("group", "gene")
    Joining, by = "gene"
    Joining, by = c("group", "gene")
    Joining, by = c("group", "gene")
    Joining, by = "gene"
    Joining, by = c("group", "gene")
    Joining, by = c("group", "gene")
    Parsed with column specification:
    cols(
      ref = col_double(),
      target = col_double()
    )
    Joining, by = c("group", "gene")
    Joining, by = c("group", "gene", "calibrated")
    Joining, by = c("group", "gene")
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'qpcr_analysis.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# PCRedux

Version: 0.2.6-1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'PCRedux.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.7Mb
      sub-directories of 1Mb or more:
        doc   3.9Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘caret’
      All declared Imports should be used.
    ```

# pdp

Version: 0.7.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘mlbench’, ‘ICEbox’
    ```

# perturbatr

Version: 1.0.0

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
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        data   3.1Mb
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
Unable to find any JVMs matching version "(null)".
No Java runtime present, try --request to install.
Warning in system("/usr/libexec/java_home", intern = TRUE) :
  running command '/usr/libexec/java_home' had status 1
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/petro.One/rJava/libs/rJava.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/petro.One/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/petro.One/rJava/libs/rJava.so
  Reason: image not found
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
Unable to find any JVMs matching version "(null)".
No Java runtime present, try --request to install.
Warning in system("/usr/libexec/java_home", intern = TRUE) :
  running command '/usr/libexec/java_home' had status 1
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/petro.One/rJava/libs/rJava.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/petro.One/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/petro.One/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘petro.One’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/petro.One/old/petro.One.Rcheck/petro.One’

```
# phenopath

Version: 1.4.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    convert: profile 'icc': 'RGB ': RGB color space not permitted on grayscale PNG `introduction_to_phenopath_files/figure-html/plot-elbo-1.png' @ warning/png.c/MagickPNGWarningHandler/1656.
    convert: profile 'icc': 'RGB ': RGB color space not permitted on grayscale PNG `introduction_to_phenopath_files/figure-html/plot-results-1.png' @ warning/png.c/MagickPNGWarningHandler/1656.
    convert: profile 'icc': 'RGB ': RGB color space not permitted on grayscale PNG `introduction_to_phenopath_files/figure-html/plot-results-2.png' @ warning/png.c/MagickPNGWarningHandler/1656.
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'introduction_to_phenopath.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# PhenotypeSimulator

Version: 0.2.2

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'Sample-scripts-external-genotype-simulation.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# philr

Version: 1.6.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    name.balance: no visible global function definition for ‘as’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/philr/new/philr.Rcheck/00_pkg_src/philr/R/name_balances.R:57)
    vote.annotation: no visible global function definition for ‘is’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/philr/new/philr.Rcheck/00_pkg_src/philr/R/name_balances.R:127-131)
    Undefined global functions or variables:
      as is
    Consider adding
      importFrom("methods", "as", "is")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports
    field contains 'methods').
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘compositions’
    ```

# physiology

Version: 1.0.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'Everest.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
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
      installed size is  7.6Mb
      sub-directories of 1Mb or more:
        data   2.0Mb
        doc    2.4Mb
        R      3.0Mb
    ```

# pixiedust

Version: 0.8.5

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘Hmisc’
    ```

# pkgdown

Version: 1.1.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘magick’
    ```

# pkggraph

Version: 0.2.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.0Mb
      sub-directories of 1Mb or more:
        doc   4.4Mb
    ```

# PKPDmisc

Version: 2.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘purrr’
      All declared Imports should be used.
    ```

# platetools

Version: 0.1.1

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘S4Vectors’, ‘data.table’
    ```

# plotly

Version: 4.8.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.1Mb
      sub-directories of 1Mb or more:
        htmlwidgets   3.1Mb
        R             2.3Mb
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

# plyranges

Version: 1.0.3

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### Aliases: chop_by_introns chop_by_gaps
    > 
    > ### ** Examples
    > 
    > if (require(pasillaBamSubset)) {
    +    bamfile <- untreated1_chr4()
    +    # define a region of interest
    +    roi <- data.frame(seqnames = "chr4", start = 5e5, end = 7e5) %>%
    +             as_granges()
    +    # results in a grouped ranges object
    +    rng <- read_bam(bamfile) %>% 
    +             filter_by_overlaps(roi) %>%
    +             chop_by_gaps()
    +    # to find ranges that have gaps use filter with `n()`
    +    rng %>% filter(n() > 2)
    +   
    + }
    Loading required package: pasillaBamSubset
    Error in FUN(X[[i]], ...) : object '"gaps"' not found
    Calls: %>% ... new_grouped_gr -> make_group_inx -> as -> .class1 -> lapply -> FUN
    Execution halted
    ```

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
         }, "could not find function \"WIGFile\"", quote(WIGFile(test_wig))) at testthat/test-io-wig.R:24
      2: eval(code, test_env)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 267 SKIPPED: 0 FAILED: 7
      1. Error: grouping allows character names (@test-groups.R#22) 
      2. Error: overlaps works as expected (@test-io-bam.R#30) 
      3. Error: read_bed returns correct GRanges (@test-io-bed.R#67) 
      4. Error: read_bed_graph returns correct GRanges (@test-io-bedGraph.R#39) 
      5. Error: reading/ writing bigwig files returns correct GRanges (@test-io-bw.R#19) 
      6. Error: reading GFF files returns correct GRanges (@test-io-gff.R#87) 
      7. Error: reading WIG files (@test-io-wig.R#24) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘BSgenome.Hsapiens.UCSC.hg19’
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
      installed size is  7.2Mb
      sub-directories of 1Mb or more:
        data   7.0Mb
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

Version: 0.4.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyr’
      All declared Imports should be used.
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
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include  -fopenmp -fPIC   -Wall -pedantic -O0 -c adjust_missing.c -o adjust_missing.o
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
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include  -fopenmp -fPIC   -Wall -pedantic -O0 -c adjust_missing.c -o adjust_missing.o
clang: error: unsupported option '-fopenmp'
make: *** [adjust_missing.o] Error 1
ERROR: compilation failed for package ‘poppr’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/poppr/old/poppr.Rcheck/poppr’

```
# powerlmm

Version: 0.4.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'technical.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# PPforest

Version: 0.1.1

## In both

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
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'PPforest-vignette.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# princurve

Version: 2.1.2.1

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
    
    Quitting from lines 23-54 (intro.Rmd) 
    Error: processing vignette 'intro.Rmd' failed with diagnostics:
    there is no package called 'magick'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘magick’
    ```

# prisonbrief

Version: 0.1.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2 marked UTF-8 strings
    ```

# processanimateR

Version: 0.3.0

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(processanimateR)
      > 
      > test_check("processanimateR")
      ── 1. Failure: patients example works (@test-issue-4-order.R#6)  ───────────────
      `animate_process(patients)` produced warnings.
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 0 SKIPPED: 0 FAILED: 1
      1. Failure: patients example works (@test-issue-4-order.R#6) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘scales’
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

# profile

Version: 1.0

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘profile-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: read_rprof
    > ### Title: File I/O for profiler data
    > ### Aliases: read_rprof read_pprof write_rprof write_pprof
    > 
    > ### ** Examples
    > 
    > rprof_file <- system.file("samples/rprof/1.out", package = "profile")
    > ds <- read_rprof(rprof_file)
    > ds
    Profile data: 22 samples
    > pprof_file <- tempfile("profile", fileext = ".pb.gz")
    > write_pprof(ds, pprof_file)
    Error: Package RProtoBuf is required to read pprof files.
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘RProtoBuf’
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

# pRoloc

Version: 1.20.2

## In both

*   checking examples ... ERROR
    ```
    ...
    > ### Title: Class '"ClustDist"'
    > ### Aliases: ClustDist class:ClustDist ClustDist-class
    > ###   plot,ClustDist,MSnSet-method show,ClustDist-method
    > ### Keywords: classes
    > 
    > ### ** Examples
    > 
    >   showClass("ClustDist")
    Class "ClustDist" [package "pRoloc"]
    
    Slots:
                                                                            
    Name:           k       dist       term         id       nrow    clustsz
    Class:    numeric       list  character  character    numeric       list
                                
    Name:  components       fcol
    Class:     vector  character
    >   
    >   library('pRolocdata')
    Error in library("pRolocdata") : there is no package called ‘pRolocdata’
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      This is pRoloc version 1.20.2 
        Visit https://lgatto.github.io/pRoloc/ to get started.
      
      Warning messages:
      1: In fun(libname, pkgname) :
        mzR has been built against a different Rcpp version (0.12.16)
      than is installed on your system (0.12.19). This might lead to errors
      when loading mzR. If you encounter such issues, please send a report,
      including the output of sessionInfo() to the Bioc support forum at 
      https://support.bioconductor.org/. For details see also
      https://github.com/sneumann/mzR/wiki/mzR-Rcpp-compiler-linker-issue.
      2: replacing previous import 'BiocGenerics::var' by 'stats::var' when loading 'MLInterfaces' 
      > library("pRolocdata")
      Error in library("pRolocdata") : there is no package called 'pRolocdata'
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    The following object is masked from 'package:tools':
    
        toHTML
    
    
    Attaching package: 'annotate'
    
    The following object is masked from 'package:mzR':
    
        nChrom
    
    Loading required package: cluster
    Warning: replacing previous import 'BiocGenerics::var' by 'stats::var' when loading 'MLInterfaces'
    
    This is pRoloc version 1.20.2 
      Visit https://lgatto.github.io/pRoloc/ to get started.
    
    Quitting from lines 87-93 (pRoloc-goannotations.Rmd) 
    Error: processing vignette 'pRoloc-goannotations.Rmd' failed with diagnostics:
    there is no package called 'pRolocdata'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking: ‘pRolocdata’ ‘GO.db’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 15.1Mb
      sub-directories of 1Mb or more:
        doc  10.6Mb
        R     3.0Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported objects imported by ':::' calls:
      ‘caret:::predict.plsda’ ‘MLInterfaces:::.macroF1’
      ‘MLInterfaces:::.precision’ ‘MLInterfaces:::.recall’
      ‘MLInterfaces:::es2df’
      See the note in ?`:::` about the use of this operator.
    There are ::: calls to the package's namespace in its code. A package
      almost never needs to use ::: for its own objects:
      ‘opt’
    ```

*   checking R code for possible problems ... NOTE
    ```
    Found the following possibly unsafe calls:
    File ‘pRoloc/R/annotation.R’:
      unlockBinding("params", .pRolocEnv)
    ```

# pRolocGUI

Version: 1.14.0

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘pRolocGUI-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: pRolocVis
    > ### Title: Interactive visualisation of spatial proteomics data
    > ### Aliases: pRolocVis pRolocVis_aggregate pRolocVis_classify
    > ###   pRolocVis_compare pRolocVis_pca
    > 
    > ### ** Examples
    > 
    > library("pRoloc")
    > library("pRolocdata")
    Error in library("pRolocdata") : there is no package called ‘pRolocdata’
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    
    Attaching package: 'annotate'
    
    The following object is masked from 'package:mzR':
    
        nChrom
    
    Loading required package: cluster
    Warning: replacing previous import 'BiocGenerics::var' by 'stats::var' when loading 'MLInterfaces'
    
    This is pRoloc version 1.20.2 
      Visit https://lgatto.github.io/pRoloc/ to get started.
    
    
    This is pRolocGUI version 1.14.0
    
    Quitting from lines 77-79 (pRolocGUI.Rmd) 
    Error: processing vignette 'pRolocGUI.Rmd' failed with diagnostics:
    there is no package called 'pRolocdata'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘pRolocdata’
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Authors@R field gives more than one person with maintainer role:
      Lisa Breckels <lms79@cam.ac.uk> [aut, cre]
      Laurent Gatto <lg390@cam.ac.uk> [aut, cre]
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported object imported by a ':::' call: ‘pRoloc:::remap’
      See the note in ?`:::` about the use of this operator.
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
      installed size is  7.7Mb
      sub-directories of 1Mb or more:
        doc       2.5Mb
        extdata   3.9Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    qcHist: no visible binding for global variable ‘techRep’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/proteoQC/new/proteoQC.Rcheck/00_pkg_src/proteoQC/R/visualization.R:406-416)
    qcHist: no visible binding for global variable ‘bioRep’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/proteoQC/new/proteoQC.Rcheck/00_pkg_src/proteoQC/R/visualization.R:406-416)
    qcHist2: no visible binding for global variable ‘error’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/proteoQC/new/proteoQC.Rcheck/00_pkg_src/proteoQC/R/visualization.R:357-365)
    qcHist2: no visible binding for global variable ‘fractile’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/proteoQC/new/proteoQC.Rcheck/00_pkg_src/proteoQC/R/visualization.R:357-365)
    qcHist2: no visible binding for global variable ‘fractile’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/proteoQC/new/proteoQC.Rcheck/00_pkg_src/proteoQC/R/visualization.R:367-369)
    qcHist2: no visible binding for global variable ‘error’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/proteoQC/new/proteoQC.Rcheck/00_pkg_src/proteoQC/R/visualization.R:377-385)
    qcHist2: no visible binding for global variable ‘fractile’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/proteoQC/new/proteoQC.Rcheck/00_pkg_src/proteoQC/R/visualization.R:377-385)
    qcHist2: no visible binding for global variable ‘fractile’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/proteoQC/new/proteoQC.Rcheck/00_pkg_src/proteoQC/R/visualization.R:389-391)
    Undefined global functions or variables:
      ..count.. bioRep curenv delta error exprs fractile fraction grid.draw
      Intensity iTRAQ4 iTRAQ8 label MS1QC MS2QC peplength peptide_summary
      precursorCharge quantify ratio readMgfData se Tag techRep TMT10 TMT6
      V1 V2 V3 V4 V5 val x y
    ```

# proustr

Version: 0.2.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 20105 marked UTF-8 strings
    ```

# prozor

Version: 0.2.11

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
    
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'TargetDecoyFDR_Example.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.7Mb
      sub-directories of 1Mb or more:
        data      1.7Mb
        extdata   2.3Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘readr’
      All declared Imports should be used.
    ```

# psichomics

Version: 1.6.2

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
      |                                        |   0% 
      |                                        |   0% 
      |                                        |   0% 
      |                                        |   0% 
      |                                        |   0% 
      |========================================| 100% 
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'AS_events_preparation.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  9.8Mb
      sub-directories of 1Mb or more:
        doc   5.6Mb
        R     3.0Mb
    ```

*   checking compiled code ... NOTE
    ```
    File ‘psichomics/libs/psichomics.so’:
      Found ‘___stdoutp’, possibly from ‘stdout’ (C)
        Object: ‘psiFastCalc.o’
      Found ‘_printf’, possibly from ‘printf’ (C)
        Object: ‘psiFastCalc.o’
    
    Compiled code should not call entry points which might terminate R
    nor write to stdout/stderr instead of to the console, nor use
    Fortran I/O nor system RNGs.
    
    See ‘Writing portable packages’ in the ‘Writing R Extensions’
    manual.
    ```

# PSLM2015

Version: 0.2.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        data   5.1Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 26 marked Latin-1 strings
    ```

# psychmeta

Version: 2.2.0

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
        R   7.0Mb
    ```

# psycho

Version: 0.3.7

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        doc   4.3Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘methods’
      All declared Imports should be used.
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

# pysd2r

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘knitr’
      All declared Imports should be used.
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
Unable to find any JVMs matching version "(null)".
No Java runtime present, try --request to install.
Warning in system("/usr/libexec/java_home", intern = TRUE) :
  running command '/usr/libexec/java_home' had status 1
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/qdap/rJava/libs/rJava.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/qdap/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/qdap/rJava/libs/rJava.so
  Reason: image not found
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
Unable to find any JVMs matching version "(null)".
No Java runtime present, try --request to install.
Warning in system("/usr/libexec/java_home", intern = TRUE) :
  running command '/usr/libexec/java_home' had status 1
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/qdap/rJava/libs/rJava.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/qdap/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/qdap/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘qdap’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/qdap/old/qdap.Rcheck/qdap’

```
# qqplotr

Version: 0.0.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘knitr’ ‘purrr’ ‘rmarkdown’
      All declared Imports should be used.
    ```

# qsort

Version: 0.2.2

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 6 marked UTF-8 strings
    ```

# quadmesh

Version: 0.1.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘rgl’
    ```

# qualmap

Version: 0.1.1

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > stl <- stLouis
    > stl <- dplyr::mutate(stl, TRACTCE = as.numeric(TRACTCE))
    > 
    > # create clusters
    > cluster1 <- qm_define(118600, 119101, 119300)
    > cluster2 <- qm_define(119300, 121200, 121100)
    > 
    > # create cluster objects
    > cluster_obj1 <- qm_create(ref = stl, key = TRACTCE, value = cluster1,
    +     rid = 1, cid = 1, category = "positive")
    > cluster_obj2 <- qm_create(ref = stl, key = TRACTCE, value = cluster2,
    +     rid = 1, cid = 2, category = "positive")
    > 
    > # combine cluster objects
    > clusters <- qm_combine(cluster_obj1, cluster_obj2)
    > 
    > # summarize cluster objects
    > positive1 <- qm_summarize(ref = stl, key = TRACTCE, clusters = clusters, category = "positive")
    Error in qm_summarize(ref = stl, key = TRACTCE, clusters = clusters, category = "positive") : 
      The specified category "positive" cannot be found in the clusters data.
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      2: stop(glue("The specified category {categoryVarQ} cannot be found in the clusters data.")) at /Users/lionel/Desktop/rlang/revdep/checks.noindex/qualmap/new/qualmap.Rcheck/00_pkg_src/qualmap/R/qm_summarize.R:179
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 55 SKIPPED: 0 FAILED: 8
      1. Failure: (unknown) (@test_2_qm_validate.R#36) 
      2. Failure: (unknown) (@test_2_qm_validate.R#49) 
      3. Error: (unknown) (@test_2_qm_validate.R#54) 
      4. Error: (unknown) (@test_4_qm_create.R#110) 
      5. Failure: (unknown) (@test_6_qm_summarize.R#40) 
      6. Failure: (unknown) (@test_6_qm_summarize.R#44) 
      7. Failure: (unknown) (@test_6_qm_summarize.R#62) 
      8. Error: (unknown) (@test_6_qm_summarize.R#77) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# quanteda

Version: 1.3.4

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.0Mb
      sub-directories of 1Mb or more:
        data   1.2Mb
        libs   2.7Mb
        R      3.1Mb
    ```

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

Version: 0.6.3

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

*   checking whether package ‘quokar’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/quokar/new/quokar.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘quokar’ ...
** package ‘quokar’ successfully unpacked and MD5 sums checked
** libs
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c init.c -o init.o
gfortran-7   -fPIC  -g -O2  -c rqbr.f -o rqbr.o
make: gfortran-7: No such file or directory
make: *** [rqbr.o] Error 1
ERROR: compilation failed for package ‘quokar’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/quokar/new/quokar.Rcheck/quokar’

```
### CRAN

```
* installing *source* package ‘quokar’ ...
** package ‘quokar’ successfully unpacked and MD5 sums checked
** libs
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c init.c -o init.o
gfortran-7   -fPIC  -g -O2  -c rqbr.f -o rqbr.o
make: gfortran-7: No such file or directory
make: *** [rqbr.o] Error 1
ERROR: compilation failed for package ‘quokar’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/quokar/old/quokar.Rcheck/quokar’

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

# r4lineups

Version: 0.1.1

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘magick’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# r511

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# radiant.data

Version: 0.9.7

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘shinyFiles’
      All declared Imports should be used.
    ```

# radiant.model

Version: 0.9.5

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘radiant.model-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: write.coeff
    > ### Title: Write coefficient table for linear and logistic regression
    > ### Aliases: write.coeff
    > 
    > ### ** Examples
    > 
    > regress(diamonds, rvar = "price", evar = c("carat", "clarity", "x"), check = "standardize") %>%
    +   write.coeff(sort = TRUE) %>%
    +   format_df(dec = 3)
    Standardized coefficients selected
    
    Warning: Unknown or uninitialised column: 'OR'.
    Warning: Unknown or uninitialised column: 'coeff'.
    Error in abs(object$coeff) : 
      non-numeric argument to mathematical function
    Calls: %>% ... eval -> _fseq -> freduce -> <Anonymous> -> write.coeff
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      x[1]: "2 \"pclass|2nd \" 0.409 -0.893      0.208     -4.290  < .001  ***"
      y[1]: "sex|male    0.080      -2.522     0.163 -15.447  < .001 ***"
      
      2nd female      0.779 0.712 0.833
      1st female      0.896 0.856 0.926
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 13 SKIPPED: 0 FAILED: 5
      1. Failure: regress (@test_stats.R#13) 
      2. Failure: regress (@test_stats.R#17) 
      3. Failure: regress (@test_stats.R#23) 
      4. Failure: logistic (@test_stats.R#45) 
      5. Failure: logistic (@test_stats.R#49) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rstudioapi’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2 marked UTF-8 strings
    ```

# radiant.multivariate

Version: 0.9.5

## In both

*   checking examples ... ERROR
    ```
    ...
     Price        $100         -6.833
     Price        $150        -33.833
     Shape        Circular      0.000
     Shape        Rectangular -27.833
     Shape        Square      -13.333
     Base utility ~            58.111
    
    Conjoint importance weights:
     Attributes    IW
         Memory 0.280
         Radio  0.058
         Size   0.080
         Price  0.319
         Shape  0.263
    
    Conjoint regression results:
    
    Error in sprintf(paste0("%.", dec, "f"), .) : 
      (list) object cannot be coerced to type 'double'
    Calls: summary ... _fseq -> freduce -> withVisible -> <Anonymous> -> sprintf
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      10: withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      11: eval(quote(`_fseq`(`_lhs`)), env, env)
      12: eval(quote(`_fseq`(`_lhs`)), env, env)
      13: `_fseq`(`_lhs`)
      14: freduce(value, `_function_list`)
      15: withVisible(function_list[[k]](value))
      16: function_list[[k]](value)
      17: sprintf(paste0("%.", dec, "f"), .) at /Users/lionel/Desktop/rlang/revdep/checks.noindex/radiant.multivariate/new/radiant.multivariate.Rcheck/00_pkg_src/radiant.multivariate/R/conjoint.R:169
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 6 SKIPPED: 0 FAILED: 1
      1. Error: Conjoint on mp3 data (@test_stats.R#109) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.0Mb
      sub-directories of 1Mb or more:
        app   4.5Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rstudioapi’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

# railtrails

Version: 0.1.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1557 marked UTF-8 strings
    ```

# randomForestExplainer

Version: 0.9

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dtplyr’ ‘MASS’
      All declared Imports should be used.
    ```

# raptr

Version: 0.1.2

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘gurobi’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  7.9Mb
      sub-directories of 1Mb or more:
        data   3.6Mb
        libs   2.0Mb
        R      1.0Mb
    ```

# Rariant

Version: 1.16.0

## In both

*   checking examples ... ERROR
    ```
    ...
    > ### Aliases: tallyPlot
    > 
    > ### ** Examples
    > 
    >   library(ggbio)
    Loading required package: ggplot2
    Need specific help about ggbio? try mailing 
     the maintainer or visit http://tengfei.github.com/ggbio/
    
    Attaching package: 'ggbio'
    
    The following objects are masked from 'package:ggplot2':
    
        geom_bar, geom_rect, geom_segment, ggsave, stat_bin, stat_identity,
        xlim
    
    >   library(GenomicRanges)
    >   library(BSgenome.Hsapiens.UCSC.hg19)
    Error in library(BSgenome.Hsapiens.UCSC.hg19) : 
      there is no package called 'BSgenome.Hsapiens.UCSC.hg19'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘BSgenome.Hsapiens.UCSC.hg19’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.0Mb
      sub-directories of 1Mb or more:
        doc       2.3Mb
        extdata   5.2Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    tallyBamRegion: no visible global function definition for 'PileupParam'
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/Rariant/new/Rariant.Rcheck/00_pkg_src/Rariant/R/tally.R:101-110)
    tallyBamRegion: no visible global function definition for
      'ScanBamParam'
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/Rariant/new/Rariant.Rcheck/00_pkg_src/Rariant/R/tally.R:112)
    tallyBamRegion: no visible global function definition for 'pileup'
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/Rariant/new/Rariant.Rcheck/00_pkg_src/Rariant/R/tally.R:114)
    Undefined global functions or variables:
      pileup PileupParam ScanBamParam
    ```

*   checking installed files from ‘inst/doc’ ... NOTE
    ```
    The following files should probably not be installed:
      ‘rariant-inspect-ci.png’, ‘rariant-inspect-shift.png’
    
    Consider the use of a .Rinstignore file: see ‘Writing R Extensions’,
    or move the vignette sources from ‘inst/doc’ to ‘vignettes’.
    ```

# rattle

Version: 5.2.0

## In both

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘cairoDevice’ ‘gWidgetsRGtk2’ ‘playwith’ ‘rggobi’ ‘RGtk2’
      ‘RGtk2Extras’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 10.9Mb
      sub-directories of 1Mb or more:
        data   3.0Mb
        etc    1.9Mb
        po     1.2Mb
        R      4.0Mb
    ```

# RBesT

Version: 1.3-3

## In both

*   R CMD check timed out
    

*   checking installed package size ... NOTE
    ```
      installed size is  8.5Mb
      sub-directories of 1Mb or more:
        libs   6.3Mb
        R      1.1Mb
    ```

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

Version: 1.0.2

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 24 marked UTF-8 strings
    ```

# rcongresso

Version: 0.4.6

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      16: withVisible(function_list[[k]](value))
      17: function_list[[k]](value)
      18: dplyr::do(., .congresso_api(API_path, .))
      19: do.rowwise_df(., .congresso_api(API_path, .))
      20: overscope_eval_next(overscope, args[[j]])
      21: .congresso_api(API_path, .) at /private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/RtmpbSBfQ2/R.INSTALL38e96c47fc7c/rlang/R/lifecycle-retired.R:794
      22: .get_from_api(path, query) at /Users/lionel/Desktop/rlang/revdep/checks.noindex/rcongresso/new/rcongresso.Rcheck/00_pkg_src/rcongresso/R/utils.R:55
      23: stop(.ERRO_RETORNO_JSON, call. = FALSE) at /Users/lionel/Desktop/rlang/revdep/checks.noindex/rcongresso/new/rcongresso.Rcheck/00_pkg_src/rcongresso/R/utils.R:38
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 71 SKIPPED: 0 FAILED: 1
      1. Error: Quantidade de itens por requisição (@test_proposicoes.R#81) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Quitting from lines 150-156 (utilizando-fetch-proposicao.Rmd) 
    Error: processing vignette 'utilizando-fetch-proposicao.Rmd' failed with diagnostics:
    API did not return json
    Execution halted
    ```

# rcv

Version: 0.2.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        data   5.0Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 6543 marked UTF-8 strings
    ```

# RDML

Version: 0.9-9

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.5Mb
      sub-directories of 1Mb or more:
        doc   2.4Mb
        R     3.0Mb
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

Version: 1.1.0

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
# rdrop2

Version: 0.8.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘digest’
      All declared Imports should be used.
    ```

# readabs

Version: 0.2.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘sjmisc’ ‘stringr’
      All declared Imports should be used.
    ```

# readat

Version: 1.6.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    sfread: no visible binding for global variable ‘header’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/readat/new/readat.Rcheck/00_pkg_src/readat/R/sfread.R:54)
    sfread: no visible binding for global variable ‘nrows’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/readat/new/readat.Rcheck/00_pkg_src/readat/R/sfread.R:54)
    Undefined global functions or variables:
      header nrows
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

# reproducible

Version: 0.2.3

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      15: .local(x, ...)
      16: .readRasterLayerValues(x, 1, x@nrows)
      17: rgdal::GDAL.open(object@file@name, silent = TRUE)
      18: new("GDALReadOnlyDataset", filename, silent = silent, allowedDrivers = allowedDrivers, 
             options = options)
      19: initialize(value, ...)
      20: initialize(value, ...)
      21: .local(.Object, ...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 169 SKIPPED: 12 FAILED: 1
      1. Error: test Copy (@test-copy.R#73) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘gdalUtils’
      All declared Imports should be used.
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

# restfulSE

Version: 1.2.3

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘GO.db’
    
    Packages suggested but not available for checking:
      ‘org.Mm.eg.db’ ‘org.Hs.eg.db’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# rfacebookstat

Version: 1.8.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘bitops’
      All declared Imports should be used.
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

# RGMQL

Version: 1.0.2

## In both

*   checking whether package ‘RGMQL’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/RGMQL/new/RGMQL.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘RGMQL’ ...
** R
** inst
** byte-compile and prepare package for lazy loading
Unable to find any JVMs matching version "(null)".
No Java runtime present, try --request to install.
Warning in system("/usr/libexec/java_home", intern = TRUE) :
  running command '/usr/libexec/java_home' had status 1
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/RGMQL/rJava/libs/rJava.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/RGMQL/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/RGMQL/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘RGMQL’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/RGMQL/new/RGMQL.Rcheck/RGMQL’

```
### CRAN

```
* installing *source* package ‘RGMQL’ ...
** R
** inst
** byte-compile and prepare package for lazy loading
Unable to find any JVMs matching version "(null)".
No Java runtime present, try --request to install.
Warning in system("/usr/libexec/java_home", intern = TRUE) :
  running command '/usr/libexec/java_home' had status 1
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/RGMQL/rJava/libs/rJava.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/RGMQL/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/RGMQL/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘RGMQL’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/RGMQL/old/RGMQL.Rcheck/RGMQL’

```
# rhierbaps

Version: 1.0.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
      failed to tidy R code in chunk <unnamed-chunk-9>
    reason: Error in loadNamespace(name) : there is no package called 'formatR'
    
    Warning in block_exec(params) :
      failed to tidy R code in chunk <unnamed-chunk-10>
    reason: Error in loadNamespace(name) : there is no package called 'formatR'
    
    Warning in block_exec(params) :
      failed to tidy R code in chunk <unnamed-chunk-11>
    reason: Error in loadNamespace(name) : there is no package called 'formatR'
    
    Warning in block_exec(params) :
      failed to tidy R code in chunk <unnamed-chunk-12>
    reason: Error in loadNamespace(name) : there is no package called 'formatR'
    
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'introduction.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
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
      installed size is  5.9Mb
      sub-directories of 1Mb or more:
        extdata   3.8Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    buildNewSqliteDb: no visible global function definition for
      ‘dbListTables’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/RImmPort/new/RImmPort.Rcheck/00_pkg_src/RImmPort/R/ImmPortSqlite.R:1890)
    Undefined global functions or variables:
      dbListTables
    ```

# rmapzen

Version: 0.4.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 31 marked UTF-8 strings
    ```

# rmcfs

Version: 1.2.14

## In both

*   checking whether package ‘rmcfs’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/rmcfs/new/rmcfs.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘rmcfs’ ...
** package ‘rmcfs’ successfully unpacked and MD5 sums checked
** R
** inst
** byte-compile and prepare package for lazy loading
Unable to find any JVMs matching version "(null)".
No Java runtime present, try --request to install.
Warning in system("/usr/libexec/java_home", intern = TRUE) :
  running command '/usr/libexec/java_home' had status 1
Error: package or namespace load failed for ‘rJava’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/rmcfs/rJava/libs/rJava.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/rmcfs/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/rmcfs/rJava/libs/rJava.so
  Reason: image not found
Error : package ‘rJava’ could not be loaded
ERROR: lazy loading failed for package ‘rmcfs’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/rmcfs/new/rmcfs.Rcheck/rmcfs’

```
### CRAN

```
* installing *source* package ‘rmcfs’ ...
** package ‘rmcfs’ successfully unpacked and MD5 sums checked
** R
** inst
** byte-compile and prepare package for lazy loading
Unable to find any JVMs matching version "(null)".
No Java runtime present, try --request to install.
Warning in system("/usr/libexec/java_home", intern = TRUE) :
  running command '/usr/libexec/java_home' had status 1
Error: package or namespace load failed for ‘rJava’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/rmcfs/rJava/libs/rJava.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/rmcfs/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/rmcfs/rJava/libs/rJava.so
  Reason: image not found
Error : package ‘rJava’ could not be loaded
ERROR: lazy loading failed for package ‘rmcfs’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/rmcfs/old/rmcfs.Rcheck/rmcfs’

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

# rmytarget

Version: 1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘jsonlite’
      All declared Imports should be used.
    ```

# RNeXML

Version: 2.1.2

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Retrieving data for taxon 'Trachypithecus_pileatus'
    
    No ENTREZ API key provided
     Get one via use_entrez()
    See https://ncbiinsights.ncbi.nlm.nih.gov/2017/11/02/new-api-keys-for-the-e-utilities/
    
    Retrieving data for taxon 'Trachypithecus_vetulus'
    
    No ENTREZ API key provided
     Get one via use_entrez()
    See https://ncbiinsights.ncbi.nlm.nih.gov/2017/11/02/new-api-keys-for-the-e-utilities/
    
    Retrieving data for taxon 'Varecia_variegata'
    
    Loading required package: maps
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'simmap.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# Rnightlights

Version: 0.2.2

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      7: sort(unlist(grep("adm", rgdal::ogrListLayers(getPolyFnamePath(ctryCode = ctryCode, 
             gadmVersion = gadmVersion, custPolyPath = custPolyPath)), value = T))) at /Users/lionel/Desktop/rlang/revdep/checks.noindex/Rnightlights/new/Rnightlights.Rcheck/00_pkg_src/Rnightlights/R/polygons.R:305
      8: unlist(grep("adm", rgdal::ogrListLayers(getPolyFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, 
             custPolyPath = custPolyPath)), value = T)) at /Users/lionel/Desktop/rlang/revdep/checks.noindex/Rnightlights/new/Rnightlights.Rcheck/00_pkg_src/Rnightlights/R/polygons.R:305
      9: grep("adm", rgdal::ogrListLayers(getPolyFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, 
             custPolyPath = custPolyPath)), value = T) at /Users/lionel/Desktop/rlang/revdep/checks.noindex/Rnightlights/new/Rnightlights.Rcheck/00_pkg_src/Rnightlights/R/polygons.R:305
      10: rgdal::ogrListLayers(getPolyFnamePath(ctryCode = ctryCode, gadmVersion = gadmVersion, 
             custPolyPath = custPolyPath)) at /Users/lionel/Desktop/rlang/revdep/checks.noindex/Rnightlights/new/Rnightlights.Rcheck/00_pkg_src/Rnightlights/R/polygons.R:305
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 50 SKIPPED: 1 FAILED: 1
      1. Error: admLevel lookups work (@test-admlevels.R#10) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# roahd

Version: 1.4.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'Roahd.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        data   2.9Mb
        doc    1.6Mb
    ```

# robotstxt

Version: 0.6.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘future’
      All declared Imports should be used.
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

# roxygen2

Version: 6.1.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 524 SKIPPED: 0 FAILED: 10
      1.  Failure: commonmark picks up the various link references (@test-rd-markdown-links.R#97) 
      2.  Failure: commonmark picks up the various link references (@test-rd-markdown-links.R#97) 
      3.  Failure: commonmark picks up the various link references (@test-rd-markdown-links.R#97) 
      4.  Failure: commonmark picks up the various link references (@test-rd-markdown-links.R#97) 
      5.  Failure: commonmark picks up the various link references (@test-rd-markdown-links.R#97) 
      6.  Failure: commonmark picks up the various link references (@test-rd-markdown-links.R#97) 
      7.  Failure: commonmark picks up the various link references (@test-rd-markdown-links.R#97) 
      8.  Failure: commonmark picks up the various link references (@test-rd-markdown-links.R#97) 
      9.  Failure: commonmark picks up the various link references (@test-rd-markdown-links.R#97) 
      10. Failure: commonmark picks up the various link references (@test-rd-markdown-links.R#97) 
      
      Error: testthat unit tests failed
      Execution halted
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
Unable to find any JVMs matching version "(null)".
No Java runtime present, try --request to install.
Warning in system("/usr/libexec/java_home", intern = TRUE) :
  running command '/usr/libexec/java_home' had status 1
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/rpcdsearch/rJava/libs/rJava.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/rpcdsearch/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/rpcdsearch/rJava/libs/rJava.so
  Reason: image not found
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
Unable to find any JVMs matching version "(null)".
No Java runtime present, try --request to install.
Warning in system("/usr/libexec/java_home", intern = TRUE) :
  running command '/usr/libexec/java_home' had status 1
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/rpcdsearch/rJava/libs/rJava.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/rpcdsearch/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/rpcdsearch/rJava/libs/rJava.so
  Reason: image not found
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

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘doParallel’ ‘foreach’
      All declared Imports should be used.
    ```

# rrr

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘Rcpp’
      All declared Imports should be used.
    ```

# rscorecard

Version: 0.10.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyselect’
      All declared Imports should be used.
    ```

# RSDA

Version: 2.0.7

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘randomcoloR’
      All declared Imports should be used.
    ```

# rsimsum

Version: 0.3.3

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘ggthemes’
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

# Rspotify

Version: 0.1.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘magrittr’
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

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘ReporteRs’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# RTCGA

Version: 1.10.0

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘RTCGA-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: boxplotTCGA
    > ### Title: Create Boxplots for TCGA Datasets
    > ### Aliases: boxplotTCGA
    > 
    > ### ** Examples
    > 
    > library(RTCGA.rnaseq)
    Error in library(RTCGA.rnaseq) : 
      there is no package called ‘RTCGA.rnaseq’
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(RTCGA)
      Welcome to the RTCGA (version: 1.10.0).
      > library(RTCGA.rnaseq)
      Error in library(RTCGA.rnaseq) : 
        there is no package called 'RTCGA.rnaseq'
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘RTCGA.rnaseq’ ‘RTCGA.clinical’ ‘RTCGA.mutations’ ‘RTCGA.RPPA’
      ‘RTCGA.mRNA’ ‘RTCGA.miRNASeq’ ‘RTCGA.methylation’ ‘RTCGA.CNV’
      ‘RTCGA.PANCAN12’
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/RTCGA/new/RTCGA.Rcheck/00_pkg_src/RTCGA/R/ggbiplot.R:157-161)
    ggbiplot: no visible binding for global variable ‘xvar’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/RTCGA/new/RTCGA.Rcheck/00_pkg_src/RTCGA/R/ggbiplot.R:157-161)
    ggbiplot: no visible binding for global variable ‘yvar’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/RTCGA/new/RTCGA.Rcheck/00_pkg_src/RTCGA/R/ggbiplot.R:157-161)
    ggbiplot: no visible binding for global variable ‘angle’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/RTCGA/new/RTCGA.Rcheck/00_pkg_src/RTCGA/R/ggbiplot.R:157-161)
    ggbiplot: no visible binding for global variable ‘hjust’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/RTCGA/new/RTCGA.Rcheck/00_pkg_src/RTCGA/R/ggbiplot.R:157-161)
    read.mutations: no visible binding for global variable ‘.’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/RTCGA/new/RTCGA.Rcheck/00_pkg_src/RTCGA/R/readTCGA.R:383)
    read.mutations: no visible binding for global variable ‘.’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/RTCGA/new/RTCGA.Rcheck/00_pkg_src/RTCGA/R/readTCGA.R:386)
    read.rnaseq: no visible binding for global variable ‘.’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/RTCGA/new/RTCGA.Rcheck/00_pkg_src/RTCGA/R/readTCGA.R:372-375)
    survivalTCGA: no visible binding for global variable ‘times’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/RTCGA/new/RTCGA.Rcheck/00_pkg_src/RTCGA/R/survivalTCGA.R:101-137)
    whichDateToUse: no visible binding for global variable ‘.’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/RTCGA/new/RTCGA.Rcheck/00_pkg_src/RTCGA/R/downloadTCGA.R:167-168)
    Undefined global functions or variables:
      . angle hjust muted times varname xvar yvar
    ```

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘RTCGA.rnaseq’, ‘RTCGA.clinical’, ‘RTCGA.mutations’, ‘RTCGA.CNV’, ‘RTCGA.RPPA’, ‘RTCGA.mRNA’, ‘RTCGA.miRNASeq’, ‘RTCGA.methylation’
    ```

# rtimicropem

Version: 1.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘R6’
      All declared Imports should be used.
    ```

# rtrek

Version: 0.1.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 992 marked UTF-8 strings
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

*   R CMD check timed out
    

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ggplot2’
      All declared Imports should be used.
    ```

# ruler

Version: 0.1.3

## Newly broken

*   checking examples ... ERROR
    ```
    ...
     1. dplyr::transmute_at(., c("disp", "qsec"), rules(z_score = abs(. -     mean(.))/sd(.) > 1))
    
    Use 'functions' to extract the individual functions. 
    
    > 
    > # Dealing with one column edge case
    > improper_pack <- . %>% dplyr::transmute_at(
    +   dplyr::vars(vs),
    +   rules(improper_is_neg = . < 0)
    + )
    > 
    > proper_pack <- . %>% dplyr::transmute_at(
    +   dplyr::vars(vs = vs),
    +   rules(proper_is_neg = . < 0)
    + )
    > 
    > mtcars[1:2, ] %>%
    +   expose(cell_packs(improper_pack, proper_pack)) %>%
    +   get_report()
    Error: Expected a list of quosures
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      2. Error: expose preserves pack names (@test-expose.R#218) 
      3. Error: expose accounts for rule separator (@test-expose.R#250) 
      4. Error: expose guesses (@test-expose.R#269) 
      5. Error: expose_single.default guesses col pack (@test-expose.R#309) 
      6. Error: expose_single.default guesses cell pack (@test-expose.R#335) 
      7. Error: expose_single.col_pack works (@test-expose.R#402) 
      8. Error: expose_single.cell_pack works (@test-expose.R#453) 
      9. Error: rules works (@test-rules.R#6) 
      
      Error: testthat unit tests failed
      In addition: Warning message:
      `list_len()` is soft-deprecated as of rlang 0.2.0.
      Please use `new_list()` instead
      This warning is displayed once per session. 
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 117-125 (validation.Rmd) 
    Error: processing vignette 'validation.Rmd' failed with diagnostics:
    Expected a list of quosures
    Execution halted
    ```

# rwavelet

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# RxODE

Version: 0.8.0-8

## In both

*   checking whether package ‘RxODE’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/RxODE/new/RxODE.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘RxODE’ ...
** package ‘RxODE’ successfully unpacked and MD5 sums checked
checking for gcc... clang
checking whether the C compiler works... yes
checking for C compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... no
checking for suffix of object files... o
checking whether we are using the GNU C compiler... yes
checking whether clang accepts -g... yes
checking for clang option to accept ISO C89... none needed
checking for clang option to support OpenMP... unsupported
configure: creating ./config.status
config.status: creating src/Makevars
--------[begin src/Makevars]--------
# -*- mode: makefile-gmake -*-
CXX_STD     = CXX11

################################################################################
## Package library flags.
PKG_LIBS    = $(LAPACK_LIBS) $(BLAS_LIBS) $(FLIBS) 
## add -lprofiler for access to gperftools;  You also have to have -g
## https://stackoverflow.com/questions/13224322/profiling-rcpp-code-on-os-x
## -Wl,--no-as-needed -lprofiler -Wl,--as-needed
## Clang UBSAN requires -lubsan
# PKG_LIBS    = $(LAPACK_LIBS) $(BLAS_LIBS) $(FLIBS)  -lubsan

################################################################################
## Compiling flags

## fsanitize=address = ASAN debugging with gcc
# PKG_FFLAGS  = -g -ggdb -O0 -fno-inline-functions -fsanitize=address,undefined,bounds-strict -fno-omit-frame-pointer -Wall -Wextra $(FPICFLAGS) 
# PKG_FCFLAGS = -g -ggdb -O0 -fno-inline-functions -fsanitize=address,undefined,bounds-strict -fno-omit-frame-pointer -Wall -Wextra $(FPICFLAGS)
# PKG_CFLAGS  = -g -ggdb -O0 -fno-inline-functions -fsanitize=address,undefined,bounds-strict -fno-omit-frame-pointer -Wall -Wextra 
# PKG_CPPFLAGS = -g -ggdb -O0 -fno-inline-functions -fsanitize=address,undefined,bounds-strict -fno-omit-frame-pointer -Wall -Wextra 

# Clang UBSAN debugging
# PKG_FFLAGS  = -g -fsanitize=address,undefined -fno-sanitize=float-divide-by-zero -fno-omit-frame-pointer $(FPICFLAGS) 
# PKG_FCFLAGS = -g -fsanitize=address,undefined -fno-sanitize=float-divide-by-zero -fno-omit-frame-pointer -frtti $(FPICFLAGS)
# PKG_CFLAGS  = -g -fsanitize=address,undefined -fno-sanitize=float-divide-by-zero -fno-omit-frame-pointer -frtti 
# PKG_CPPFLAGS = -g -fsanitize=address,undefined -fno-sanitize=float-divide-by-zero -fno-omit-frame-pointer -frtti 

## Standard debug for gdb and valgrind
# PKG_FFLAGS  = -g -ggdb -O0 -Wall -Wextra $(FPICFLAGS) 
# PKG_FCFLAGS = -g -ggdb -O0 -Wall -Wextra $(FPICFLAGS)
# PKG_CFLAGS  = -g -ggdb -O0 -Wall -Wextra 
# PKG_CPPFLAGS = -g -ggdb -O0 -Wall -Wextra 

# Release options
PKG_FFLAGS  = $(FPICFLAGS) 
PKG_FCFLAGS = $(FPICFLAGS)
PKG_CFLAGS  =  
PKG_CPPFLAGS = 

SOURCES_C = call_dvode.c dop853.c tran.c omegaChol.c init.c par_solve.c cfode.c common.c corfailure.c correction.c daxpy.c ddot.c dgefa.c dgesl.c dscal.c fnorm.c idamax.c intdy.c lsoda.c methodswitch.c orderswitch.c prja.c scaleh.c solsy.c stoda.c vmnorm.c strdup_printf.c rprintf.c box.c lbfgsR.c lincmt.c
SOURCES_CPP = RcppExports.cpp WinDrive.cpp rxInv.cpp rxData.cpp eventTable.cpp inner.cpp
SOURCES_FORTAN = dlsoda.f opkda1_abbr.f opkda2.f

OBJECTS = $(SOURCES_C:.c=.o) $(SOURCES_FORTAN:.f=.o) $(SOURCES_CPP:.cpp=.o)


--------[end src/Makevars]--------
** libs
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c call_dvode.c -o call_dvode.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c dop853.c -o dop853.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c tran.c -o tran.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c omegaChol.c -o omegaChol.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c init.c -o init.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c par_solve.c -o par_solve.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c cfode.c -o cfode.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c common.c -o common.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c corfailure.c -o corfailure.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c correction.c -o correction.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c daxpy.c -o daxpy.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c ddot.c -o ddot.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c dgefa.c -o dgefa.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c dgesl.c -o dgesl.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c dscal.c -o dscal.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c fnorm.c -o fnorm.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c idamax.c -o idamax.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c intdy.c -o intdy.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c lsoda.c -o lsoda.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c methodswitch.c -o methodswitch.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c orderswitch.c -o orderswitch.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c prja.c -o prja.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c scaleh.c -o scaleh.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c solsy.c -o solsy.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c stoda.c -o stoda.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c vmnorm.c -o vmnorm.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c strdup_printf.c -o strdup_printf.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c rprintf.c -o rprintf.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c box.c -o box.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c lbfgsR.c -o lbfgsR.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c lincmt.c -o lincmt.o
gfortran-7  -fPIC  -fPIC  -g -O2  -c dlsoda.f -o dlsoda.o
make: gfortran-7: No such file or directory
make: *** [dlsoda.o] Error 1
ERROR: compilation failed for package ‘RxODE’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/RxODE/new/RxODE.Rcheck/RxODE’

```
### CRAN

```
* installing *source* package ‘RxODE’ ...
** package ‘RxODE’ successfully unpacked and MD5 sums checked
checking for gcc... clang
checking whether the C compiler works... yes
checking for C compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... no
checking for suffix of object files... o
checking whether we are using the GNU C compiler... yes
checking whether clang accepts -g... yes
checking for clang option to accept ISO C89... none needed
checking for clang option to support OpenMP... unsupported
configure: creating ./config.status
config.status: creating src/Makevars
--------[begin src/Makevars]--------
# -*- mode: makefile-gmake -*-
CXX_STD     = CXX11

################################################################################
## Package library flags.
PKG_LIBS    = $(LAPACK_LIBS) $(BLAS_LIBS) $(FLIBS) 
## add -lprofiler for access to gperftools;  You also have to have -g
## https://stackoverflow.com/questions/13224322/profiling-rcpp-code-on-os-x
## -Wl,--no-as-needed -lprofiler -Wl,--as-needed
## Clang UBSAN requires -lubsan
# PKG_LIBS    = $(LAPACK_LIBS) $(BLAS_LIBS) $(FLIBS)  -lubsan

################################################################################
## Compiling flags

## fsanitize=address = ASAN debugging with gcc
# PKG_FFLAGS  = -g -ggdb -O0 -fno-inline-functions -fsanitize=address,undefined,bounds-strict -fno-omit-frame-pointer -Wall -Wextra $(FPICFLAGS) 
# PKG_FCFLAGS = -g -ggdb -O0 -fno-inline-functions -fsanitize=address,undefined,bounds-strict -fno-omit-frame-pointer -Wall -Wextra $(FPICFLAGS)
# PKG_CFLAGS  = -g -ggdb -O0 -fno-inline-functions -fsanitize=address,undefined,bounds-strict -fno-omit-frame-pointer -Wall -Wextra 
# PKG_CPPFLAGS = -g -ggdb -O0 -fno-inline-functions -fsanitize=address,undefined,bounds-strict -fno-omit-frame-pointer -Wall -Wextra 

# Clang UBSAN debugging
# PKG_FFLAGS  = -g -fsanitize=address,undefined -fno-sanitize=float-divide-by-zero -fno-omit-frame-pointer $(FPICFLAGS) 
# PKG_FCFLAGS = -g -fsanitize=address,undefined -fno-sanitize=float-divide-by-zero -fno-omit-frame-pointer -frtti $(FPICFLAGS)
# PKG_CFLAGS  = -g -fsanitize=address,undefined -fno-sanitize=float-divide-by-zero -fno-omit-frame-pointer -frtti 
# PKG_CPPFLAGS = -g -fsanitize=address,undefined -fno-sanitize=float-divide-by-zero -fno-omit-frame-pointer -frtti 

## Standard debug for gdb and valgrind
# PKG_FFLAGS  = -g -ggdb -O0 -Wall -Wextra $(FPICFLAGS) 
# PKG_FCFLAGS = -g -ggdb -O0 -Wall -Wextra $(FPICFLAGS)
# PKG_CFLAGS  = -g -ggdb -O0 -Wall -Wextra 
# PKG_CPPFLAGS = -g -ggdb -O0 -Wall -Wextra 

# Release options
PKG_FFLAGS  = $(FPICFLAGS) 
PKG_FCFLAGS = $(FPICFLAGS)
PKG_CFLAGS  =  
PKG_CPPFLAGS = 

SOURCES_C = call_dvode.c dop853.c tran.c omegaChol.c init.c par_solve.c cfode.c common.c corfailure.c correction.c daxpy.c ddot.c dgefa.c dgesl.c dscal.c fnorm.c idamax.c intdy.c lsoda.c methodswitch.c orderswitch.c prja.c scaleh.c solsy.c stoda.c vmnorm.c strdup_printf.c rprintf.c box.c lbfgsR.c lincmt.c
SOURCES_CPP = RcppExports.cpp WinDrive.cpp rxInv.cpp rxData.cpp eventTable.cpp inner.cpp
SOURCES_FORTAN = dlsoda.f opkda1_abbr.f opkda2.f

OBJECTS = $(SOURCES_C:.c=.o) $(SOURCES_FORTAN:.f=.o) $(SOURCES_CPP:.cpp=.o)


--------[end src/Makevars]--------
** libs
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c call_dvode.c -o call_dvode.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c dop853.c -o dop853.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c tran.c -o tran.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c omegaChol.c -o omegaChol.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c init.c -o init.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c par_solve.c -o par_solve.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c cfode.c -o cfode.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c common.c -o common.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c corfailure.c -o corfailure.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c correction.c -o correction.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c daxpy.c -o daxpy.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c ddot.c -o ddot.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c dgefa.c -o dgefa.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c dgesl.c -o dgesl.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c dscal.c -o dscal.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c fnorm.c -o fnorm.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c idamax.c -o idamax.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c intdy.c -o intdy.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c lsoda.c -o lsoda.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c methodswitch.c -o methodswitch.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c orderswitch.c -o orderswitch.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c prja.c -o prja.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c scaleh.c -o scaleh.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c solsy.c -o solsy.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c stoda.c -o stoda.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c vmnorm.c -o vmnorm.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c strdup_printf.c -o strdup_printf.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c rprintf.c -o rprintf.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c box.c -o box.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c lbfgsR.c -o lbfgsR.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/dparser/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/RcppArmadillo/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/RxODE/PreciseSums/include" -I/usr/local/include   -fPIC   -Wall -pedantic -O0 -c lincmt.c -o lincmt.o
gfortran-7  -fPIC  -fPIC  -g -O2  -c dlsoda.f -o dlsoda.o
make: gfortran-7: No such file or directory
make: *** [dlsoda.o] Error 1
ERROR: compilation failed for package ‘RxODE’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/RxODE/old/RxODE.Rcheck/RxODE’

```
# rzeit2

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ggplot2’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 841 marked UTF-8 strings
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

# sars

Version: 1.0.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        refs   4.3Mb
    ```

# scanstatistics

Version: 1.0.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'introduction.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# scater

Version: 1.8.4

## In both

*   checking examples ... WARNING
    ```
    Found the following significant warnings:
    
      Warning: 'read10xResults' is deprecated.
      Warning: 'downsampleCounts' is deprecated.
      Warning: 'normalizeExprs' is deprecated.
      Warning: 'normalizeExprs' is deprecated.
      Warning: 'normalizeExprs' is deprecated.
      Warning: 'normalizeExprs' is deprecated.
      Warning: 'normalizeExprs' is deprecated.
      Warning: 'normalizeExprs' is deprecated.
      Warning: 'read10xResults' is deprecated.
    Deprecated functions may be defunct as soon as of the next release
    of R.
    See ?Deprecated.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 15.5Mb
      sub-directories of 1Mb or more:
        doc       5.4Mb
        extdata   2.9Mb
        libs      4.9Mb
    ```

# scFeatureFilter

Version: 1.0.0

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

# sdcTable

Version: 0.24

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
# sdStaf

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘rgdal’ ‘rgeos’ ‘tidyr’
      All declared Imports should be used.
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
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/segclust2d/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/segclust2d/RcppArmadillo/include" -I/usr/local/include  -fopenmp -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c RcppExports.cpp -o RcppExports.o
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
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/segclust2d/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/segclust2d/RcppArmadillo/include" -I/usr/local/include  -fopenmp -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c RcppExports.cpp -o RcppExports.o
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

# seoR

Version: 0.1.0

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘seoR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: getBingResults
    > ### Title: Function to retrive data from Google Suggest for a specific
    > ###   keyword
    > ### Aliases: getBingResults
    > 
    > ### ** Examples
    > 
    > getBingResults("R Project")
    Error in data.frame(..., check.names = FALSE) : 
      arguments imply differing number of rows: 9, 8
    Calls: getBingResults -> as.data.frame -> cbind -> cbind -> data.frame
    Execution halted
    ```

# seqCAT

Version: 1.2.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Reading VCF file ...
    Creating SNV profile ...
    Created and stored SNV profile for "HKE3" in [hke3_profile.txt].
    Reading profile for HKE3 in file hke3_profile.txt ...
    Comparing HCT116 and HKE3 ...
    Comparing HCT116 and HCT116 [1 / 6]
    Comparing HCT116 and HKE3 [2 / 6]
    Comparing HCT116 and RKO [3 / 6]
    Comparing HKE3 and HKE3 [4 / 6]
    Comparing HKE3 and RKO [5 / 6]
    Comparing RKO and RKO [6 / 6]
    Comparing COSMIC.HCT116 and COSMIC.HCT116 ...
    Comparing COSMIC.HCT116 and HCT116 [1 / 3]
    Comparing COSMIC.HCT116 and HKE3 [2 / 3]
    Comparing COSMIC.HCT116 and RKO [3 / 3]
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'seqCAT.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# seqcombo

Version: 1.2.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    Attaching package: 'igraph'
    
    The following object is masked from 'package:tibble':
    
        as_data_frame
    
    The following objects are masked from 'package:stats':
    
        decompose, spectrum
    
    The following object is masked from 'package:base':
    
        union
    
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'reassortment.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# sergeant

Version: 0.5.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Unable to find any JVMs matching version "(null)".
    No Java runtime present, try --request to install.
    ```

# Seurat

Version: 2.3.4

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘loomR’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        libs   1.4Mb
        R      3.0Mb
    ```

# sevenbridges

Version: 1.10.4

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.7Mb
      sub-directories of 1Mb or more:
        doc   2.9Mb
        R     4.1Mb
    ```

# sevenC

Version: 1.0.0

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
    
    convert: profile 'icc': 'RGB ': RGB color space not permitted on grayscale PNG `sevenC_files/figure-html/unnamed-chunk-21-1.png' @ warning/png.c/MagickPNGWarningHandler/1656.
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
checking for gdal-config... /usr/local/bin/gdal-config
checking gdal-config usability... yes
configure: GDAL: 2.3.1
checking GDAL version >= 2.0.0... yes
checking for gcc... clang
checking whether the C compiler works... yes
checking for C compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... no
checking for suffix of object files... o
checking whether we are using the GNU C compiler... yes
checking whether clang accepts -g... yes
checking for clang option to accept ISO C89... none needed
checking how to run the C preprocessor... clang -E
checking for grep that handles long lines and -e... /usr/bin/grep
checking for egrep... /usr/bin/grep -E
checking for ANSI C header files... yes
checking for sys/types.h... yes
checking for sys/stat.h... yes
checking for stdlib.h... yes
checking for string.h... yes
checking for memory.h... yes
checking for strings.h... yes
checking for inttypes.h... yes
checking for stdint.h... yes
checking for unistd.h... yes
checking gdal.h usability... yes
checking gdal.h presence... yes
checking for gdal.h... yes
checking GDAL: linking with --libs only... no
checking GDAL: linking with --libs and --dep-libs... no
In file included from gdal_test.cpp:1:
In file included from /usr/local/include/gdal.h:45:
/usr/local/include/cpl_port.h:187:6: error: Must have C++11 or newer.
#    error Must have C++11 or newer.
     ^
1 error generated.
In file included from gdal_test.cpp:1:
In file included from /usr/local/include/gdal.h:45:
/usr/local/include/cpl_port.h:187:6: error: Must have C++11 or newer.
#    error Must have C++11 or newer.
     ^
1 error generated.
configure: Install failure: compilation and/or linkage problems.
configure: error: GDALAllRegister not found in libgdal.
ERROR: configuration failed for package ‘sf’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/sf/new/sf.Rcheck/sf’

```
### CRAN

```
* installing *source* package ‘sf’ ...
** package ‘sf’ successfully unpacked and MD5 sums checked
configure: CC: clang
configure: CXX: clang++
checking for gdal-config... /usr/local/bin/gdal-config
checking gdal-config usability... yes
configure: GDAL: 2.3.1
checking GDAL version >= 2.0.0... yes
checking for gcc... clang
checking whether the C compiler works... yes
checking for C compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... no
checking for suffix of object files... o
checking whether we are using the GNU C compiler... yes
checking whether clang accepts -g... yes
checking for clang option to accept ISO C89... none needed
checking how to run the C preprocessor... clang -E
checking for grep that handles long lines and -e... /usr/bin/grep
checking for egrep... /usr/bin/grep -E
checking for ANSI C header files... yes
checking for sys/types.h... yes
checking for sys/stat.h... yes
checking for stdlib.h... yes
checking for string.h... yes
checking for memory.h... yes
checking for strings.h... yes
checking for inttypes.h... yes
checking for stdint.h... yes
checking for unistd.h... yes
checking gdal.h usability... yes
checking gdal.h presence... yes
checking for gdal.h... yes
checking GDAL: linking with --libs only... no
checking GDAL: linking with --libs and --dep-libs... no
In file included from gdal_test.cpp:1:
In file included from /usr/local/include/gdal.h:45:
/usr/local/include/cpl_port.h:187:6: error: Must have C++11 or newer.
#    error Must have C++11 or newer.
     ^
1 error generated.
In file included from gdal_test.cpp:1:
In file included from /usr/local/include/gdal.h:45:
/usr/local/include/cpl_port.h:187:6: error: Must have C++11 or newer.
#    error Must have C++11 or newer.
     ^
1 error generated.
configure: Install failure: compilation and/or linkage problems.
configure: error: GDALAllRegister not found in libgdal.
ERROR: configuration failed for package ‘sf’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/sf/old/sf.Rcheck/sf’

```
# shiny

Version: 1.1.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.4Mb
      sub-directories of 1Mb or more:
        R     4.0Mb
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

Version: 0.3.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.9Mb
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
      ‘htmlwidgets’ ‘jsonlite’ ‘RColorBrewer’ ‘viridis’
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

# sigmajs

Version: 0.1.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.2Mb
      sub-directories of 1Mb or more:
        doc   6.5Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 28 marked UTF-8 strings
    ```

# SimDesign

Version: 1.11

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘doMPI’
    ```

# simmer

Version: 4.0.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.6Mb
      sub-directories of 1Mb or more:
        doc    2.1Mb
        libs   4.4Mb
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
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include  -fopenmp -fPIC   -Wall -pedantic -O0 -c R_register_native.c -o R_register_native.o
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
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/usr/local/include  -fopenmp -fPIC   -Wall -pedantic -O0 -c R_register_native.c -o R_register_native.o
clang: error: unsupported option '-fopenmp'
make: *** [R_register_native.o] Error 1
ERROR: compilation failed for package ‘simputation’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/simputation/old/simputation.Rcheck/simputation’

```
# SimRVPedigree

Version: 0.3.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
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

Version: 1.0.0

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘singscore-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: projectScoreLandscape
    > ### Title: Project data on the landscape plot obtained from
    > ###   'plotScoreLandscape()'
    > ### Aliases: projectScoreLandscape
    > 
    > ### ** Examples
    > 
    > ranked <- rankGenes(toy_expr_se)
    > scoredf1 <- simpleScore(ranked, upSet = toy_gs_up, downSet = toy_gs_dn)
    > scoredf2 <- simpleScore(ranked, upSet = toy_gs_up)
    > psl <- plotScoreLandscape(scoredf1, scoredf2)
    > projectScoreLandscape(psl,scoredf1, scoredf2)
    Warning: Ignoring unknown aesthetics: text
    Error in FUN(X[[i]], ...) : object 'Signature 1' not found
    Calls: <Anonymous> ... ggplot_build.ggplot -> by_layer -> f -> <Anonymous> -> f -> lapply -> FUN
    Execution halted
    ```

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    Loading required package: XML
    
    Attaching package: 'XML'
    
    The following object is masked from 'package:tools':
    
        toHTML
    
    Loading required package: graph
    
    Attaching package: 'graph'
    
    The following object is masked from 'package:XML':
    
        addNode
    
    Quitting from lines 174-182 (singscore.Rmd) 
    Error: processing vignette 'singscore.Rmd' failed with diagnostics:
    object 'tcga-EPI' not found
    Execution halted
    ```

# sjmisc

Version: 2.7.5

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘sjmisc-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: move_columns
    > ### Title: Move columns to other positions in a data frame
    > ### Aliases: move_columns
    > 
    > ### ** Examples
    > 
    > data(iris)
    > 
    > iris %>%
    +   move_columns(Sepal.Width, .after = "Species") %>%
    +   head()
    Error in if (pos.after < 1) { : argument is of length zero
    Calls: %>% ... eval -> _fseq -> freduce -> <Anonymous> -> move_columns
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      10: select.data.frame(., !!group_name)
      11: tidyselect::vars_select(names(.data), !!!quos(...))
      12: vars_select_eval(.vars, quos)
      13: map_if(ind_list, is_character, match_strings, names = TRUE)
      14: map(.x[sel], .f, ...)
      15: .f(.x[[i]], ...)
      16: bad_unknown_vars(vars, unknown)
      17: abort(glue("Unknown { thing } { fmt_args(unknown) } "))
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 96 SKIPPED: 6 FAILED: 1
      1. Error: de_mean (@test-demean.R#12) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# sjstats

Version: 0.17.1

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘arm’
    ```

# skynet

Version: 1.2.2

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
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/sorvi/new/sorvi.Rcheck/00_pkg_src/sorvi/R/regression_plot.R:115)
    regression_plot : <anonymous>: no visible global function definition
      for ‘pnorm’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/sorvi/new/sorvi.Rcheck/00_pkg_src/sorvi/R/regression_plot.R:115)
    regression_plot: no visible global function definition for
      ‘flush.console’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/sorvi/new/sorvi.Rcheck/00_pkg_src/sorvi/R/regression_plot.R:138)
    regression_plot: no visible global function definition for ‘density’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/sorvi/new/sorvi.Rcheck/00_pkg_src/sorvi/R/regression_plot.R:147)
    regression_plot: no visible global function definition for
      ‘flush.console’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/sorvi/new/sorvi.Rcheck/00_pkg_src/sorvi/R/regression_plot.R:194)
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

Version: 0.2.2

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      At optimal_modularity.c:85 : GLPK is not available, Unimplemented function call
      1: moduleGraph(mySim, FALSE) at testthat/test-module-deps-methods.R:195
      2: moduleGraph(mySim, FALSE) at /Users/lionel/Desktop/rlang/revdep/checks.noindex/SpaDES.core/new/SpaDES.core.Rcheck/00_pkg_src/SpaDES.core/R/plotting-diagrams.R:434
      3: cluster_optimal(grph) at /Users/lionel/Desktop/rlang/revdep/checks.noindex/SpaDES.core/new/SpaDES.core.Rcheck/00_pkg_src/SpaDES.core/R/plotting-diagrams.R:453
      
        Using cached copy of .inputObjects event in child6 module. Adding to memoised copy.
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 306 SKIPPED: 35 FAILED: 2
      1. Error: test checkpointing with disk-backed raster (@test-checkpoint.R#96) 
      2. Error: 3 levels of parent and child modules load and show correctly (@test-module-deps-methods.R#195) 
      
      Error: testthat unit tests failed
      In addition: Warning message:
      In fun(libname, pkgname) : couldn't connect to display ""
      Execution halted
    ```

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'moduleCoverage.Rd':
      ‘[covr]{shine}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'i-introduction.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        doc   1.2Mb
        R     3.0Mb
    ```

# sparklyr

Version: 0.9.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.1Mb
      sub-directories of 1Mb or more:
        java   1.9Mb
        R      4.0Mb
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
      ‘dplyr’ ‘dygraphs’ ‘ggplot2’ ‘htmlwidgets’ ‘knitr’ ‘leaflet’
      ‘mapproj’ ‘maptools’ ‘RColorBrewer’ ‘rgdal’ ‘rgeos’ ‘rmarkdown’
      ‘shinyjs’ ‘SpatialEpi’ ‘spdep’ ‘xts’
      All declared Imports should be used.
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

# splashr

Version: 0.4.1

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘magick’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# sport

Version: 0.1.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 6863 marked UTF-8 strings
    ```

# ss3sim

Version: 0.9.5

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'introduction.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# stacomiR

Version: 0.5.3.1

## In both

*   checking package dependencies ... ERROR
    ```
    Packages required but not available:
      ‘stacomirtools’ ‘gWidgetsRGtk2’ ‘RGtk2’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# staRdom

Version: 1.0.8

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Writing 18 Bibtex entries ... OK
    Results written to file 'references.bib'
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'Basic_analysis_of_DOM_samples.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

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

Version: 0.1-1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.7Mb
      sub-directories of 1Mb or more:
        doc   3.5Mb
        nc    2.9Mb
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
      installed size is  8.4Mb
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

Version: 0.2.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘huge’ ‘readr’ ‘scales’ ‘shinyjs’
      All declared Imports should be used.
    ```

# stormwindmodel

Version: 0.1.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    
    Attaching package: 'gridExtra'
    
    The following object is masked from 'package:dplyr':
    
        combine
    
    Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=georgia&zoom=5&size=640x640&scale=2&maptype=terrain&language=en-EN&sensor=false
    Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=georgia&sensor=false
    Warning: geocode failed with status OVER_QUERY_LIMIT, location = "georgia"
    Quitting from lines 220-233 (Details.Rmd) 
    Error: processing vignette 'Details.Rmd' failed with diagnostics:
    arguments imply differing number of rows: 0, 1
    Execution halted
    ```

# stplanr

Version: 0.2.5

## In both

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

# STRMPS

Version: 0.5.8

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘STRaitRazoR’
    ```

# SubgrPlots

Version: 0.1.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        paper   2.3Mb
        R       3.1Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘alluvial’ ‘geoR’ ‘gridBase’ ‘UpSetR’
      All declared Imports should be used.
    ```

# subSeq

Version: 1.10.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/subSeq/new/subSeq.Rcheck/00_pkg_src/subSeq/R/summary.subsamples.R:127-129)
    summary.subsamples: no visible binding for global variable ‘percent’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/subSeq/new/subSeq.Rcheck/00_pkg_src/subSeq/R/summary.subsamples.R:127-129)
    summary.subsamples: no visible binding for global variable ‘proportion’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/subSeq/new/subSeq.Rcheck/00_pkg_src/subSeq/R/summary.subsamples.R:127-129)
    summary.subsamples: no visible binding for global variable ‘method’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/subSeq/new/subSeq.Rcheck/00_pkg_src/subSeq/R/summary.subsamples.R:127-129)
    voomLimma: no visible global function definition for ‘model.matrix’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/subSeq/new/subSeq.Rcheck/00_pkg_src/subSeq/R/handlers.R:41)
    Undefined global functions or variables:
      . average.depth average.value coefficient cor count cov depth estFDP
      ID method metric model.matrix o.coefficient o.lfdr o.padj p.adjust
      padj percent plot proportion pvalue rbinom replication rFDP
      selectMethod significant valid value var
    Consider adding
      importFrom("graphics", "plot")
      importFrom("methods", "selectMethod")
      importFrom("stats", "cor", "cov", "model.matrix", "p.adjust", "rbinom",
                 "var")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports
    field contains 'methods').
    ```

# sugrrants

Version: 0.1.5

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Error in re-building vignettes:
      ...
    
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

Version: 1.0.4

## In both

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
      ‘dplyr’
    Adding so many packages to the search path is excessive and
    importing selectively is preferable.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 12.8Mb
      sub-directories of 1Mb or more:
        data   9.0Mb
        doc    3.3Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported object imported by a ':::' call: ‘BiocGenerics:::replaceSlots’
      See the note in ?`:::` about the use of this operator.
    ```

*   checking R code for possible problems ... NOTE
    ```
    .list2mat : <anonymous>: no visible binding for global variable
      ‘.method’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/SummarizedBenchmark/new/SummarizedBenchmark.Rcheck/00_pkg_src/SummarizedBenchmark/R/buildBench.R:275)
    .list2mat : <anonymous>: no visible binding for global variable ‘.val’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/SummarizedBenchmark/new/SummarizedBenchmark.Rcheck/00_pkg_src/SummarizedBenchmark/R/buildBench.R:275)
    .list2mat : <anonymous>: no visible binding for global variable ‘.id’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/SummarizedBenchmark/new/SummarizedBenchmark.Rcheck/00_pkg_src/SummarizedBenchmark/R/buildBench.R:276-277)
    plotROC: no visible binding for global variable ‘FDR’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/SummarizedBenchmark/new/SummarizedBenchmark.Rcheck/00_pkg_src/SummarizedBenchmark/R/PlottingFunctions.R:81-82)
    plotROC: no visible binding for global variable ‘TPR’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/SummarizedBenchmark/new/SummarizedBenchmark.Rcheck/00_pkg_src/SummarizedBenchmark/R/PlottingFunctions.R:81-82)
    plotROC: no visible binding for global variable ‘method’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/SummarizedBenchmark/new/SummarizedBenchmark.Rcheck/00_pkg_src/SummarizedBenchmark/R/PlottingFunctions.R:81-82)
    Undefined global functions or variables:
      .id .method .val FDR method TPR
    ```

# sunburstR

Version: 2.1.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘treemap’
    ```

# survminer

Version: 0.4.3

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.1Mb
      sub-directories of 1Mb or more:
        doc   5.1Mb
    ```

# survsup

Version: 0.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘gridExtra’ ‘stats’ ‘survival’ ‘utils’
      All declared Imports should be used.
    ```

# survtmle

Version: 1.1.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'survtmle_intro.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# sweep

Version: 0.2.1.1

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
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'swfdrTutorial.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    lm_pi0: no visible global function definition for ‘glm’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/swfdr/new/swfdr.Rcheck/00_pkg_src/swfdr/R/lm_pi0.R:56)
    lm_pi0: no visible binding for global variable ‘binomial’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/swfdr/new/swfdr.Rcheck/00_pkg_src/swfdr/R/lm_pi0.R:56)
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

# synlet

Version: 1.10.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    zFactor: no visible binding for global variable ‘sd’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/synlet/new/synlet.Rcheck/00_pkg_src/synlet/R/zFactor.R:37-38)
    zFactor: no visible binding for global variable ‘median’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/synlet/new/synlet.Rcheck/00_pkg_src/synlet/R/zFactor.R:37-38)
    zFactor: no visible global function definition for ‘complete.cases’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/synlet/new/synlet.Rcheck/00_pkg_src/synlet/R/zFactor.R:50)
    Undefined global functions or variables:
      COL_NAME colorRampPalette complete.cases condition dev.off
      EXPERIMENT_MODIFICATION EXPERIMENT_TYPE experiments is mad
      MASTER_PLATE median medpolish p.adjust pdf phyper PLATE rainbow
      READOUT ROW_NAME sd siRNA t.test value Var1 WELL_CONTENT_NAME
      write.table
    Consider adding
      importFrom("grDevices", "colorRampPalette", "dev.off", "pdf",
                 "rainbow")
      importFrom("methods", "is")
      importFrom("stats", "complete.cases", "mad", "median", "medpolish",
                 "p.adjust", "phyper", "sd", "t.test")
      importFrom("utils", "write.table")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports
    field contains 'methods').
    ```

# syuzhet

Version: 1.0.4

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.8Mb
      sub-directories of 1Mb or more:
        extdata   3.1Mb
        R         2.1Mb
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
        data   1.1Mb
        doc    1.7Mb
        R      2.1Mb
    ```

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

Version: 2.8.4

## In both

*   R CMD check timed out
    

*   checking dependencies in R code ... WARNING
    ```
    '::' or ':::' import not declared from: ‘tidyr’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 74.2Mb
      sub-directories of 1Mb or more:
        data   3.5Mb
        doc   66.4Mb
        R      4.1Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    TCGAtumor_purity: no visible binding for global variable ‘Tumor.purity’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/TCGAbiolinks/new/TCGAbiolinks.Rcheck/00_pkg_src/TCGAbiolinks/R/clinical.R:639-640)
    TCGAvisualize_oncoprint: no visible binding for global variable ‘value’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/TCGAbiolinks/new/TCGAbiolinks.Rcheck/00_pkg_src/TCGAbiolinks/R/visualize.R:944)
    TCGAvisualize_SurvivalCoxNET: no visible global function definition for
      ‘dNetInduce’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/TCGAbiolinks/new/TCGAbiolinks.Rcheck/00_pkg_src/TCGAbiolinks/R/visualize.R:156-157)
    TCGAvisualize_SurvivalCoxNET: no visible global function definition for
      ‘dNetPipeline’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/TCGAbiolinks/new/TCGAbiolinks.Rcheck/00_pkg_src/TCGAbiolinks/R/visualize.R:161-162)
    TCGAvisualize_SurvivalCoxNET: no visible global function definition for
      ‘dCommSignif’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/TCGAbiolinks/new/TCGAbiolinks.Rcheck/00_pkg_src/TCGAbiolinks/R/visualize.R:174)
    TCGAvisualize_SurvivalCoxNET: no visible global function definition for
      ‘visNet’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/TCGAbiolinks/new/TCGAbiolinks.Rcheck/00_pkg_src/TCGAbiolinks/R/visualize.R:184-189)
    Undefined global functions or variables:
      barcode c3net clinical coordinates dCommSignif dNetInduce
      dNetPipeline exon knnmi.cross limmacontrasts.fit limmamakeContrasts
      minet portions rse_gene TabSubtypesCol_merged Tumor.purity value
      visNet
    ```

# TCGAbiolinksGUI

Version: 1.6.1

## In both

*   checking package dependencies ... ERROR
    ```
    Packages required but not available:
      ‘IlluminaHumanMethylation450kanno.ilmn12.hg19’
      ‘IlluminaHumanMethylation450kmanifest’
      ‘IlluminaHumanMethylation27kmanifest’
      ‘IlluminaHumanMethylation27kanno.ilmn12.hg19’
      ‘IlluminaHumanMethylationEPICanno.ilm10b2.hg19’
      ‘IlluminaHumanMethylationEPICmanifest’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# TCGAbiolinksGUI.data

Version: 1.0.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'vignettes.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 19.7Mb
      sub-directories of 1Mb or more:
        data  18.6Mb
        doc    1.0Mb
    ```

# tcR

Version: 2.2.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.8Mb
      sub-directories of 1Mb or more:
        data   1.4Mb
        doc    3.9Mb
        R      2.0Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘scales’
      All declared Imports should be used.
    ```

# telefit

Version: 1.0.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        data   2.4Mb
        libs   2.5Mb
    ```

# tempcyclesdata

Version: 1.0.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.2Mb
      sub-directories of 1Mb or more:
        data   6.1Mb
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

# testassay

Version: 0.1.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'GIAValidationExample.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

# textmining

Version: 0.0.1

## In both

*   checking whether package ‘textmining’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/textmining/new/textmining.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘textmining’ ...
** package ‘textmining’ successfully unpacked and MD5 sums checked
** R
** byte-compile and prepare package for lazy loading
Warning in fun(libname, pkgname) : couldn't connect to display ""
Unable to find any JVMs matching version "(null)".
No Java runtime present, try --request to install.
Warning in system("/usr/libexec/java_home", intern = TRUE) :
  running command '/usr/libexec/java_home' had status 1
Error : .onLoad failed in loadNamespace() for 'mallet', details:
  call: NULL
  error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/textmining/rJava/libs/rJava.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/textmining/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/textmining/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘textmining’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/textmining/new/textmining.Rcheck/textmining’

```
### CRAN

```
* installing *source* package ‘textmining’ ...
** package ‘textmining’ successfully unpacked and MD5 sums checked
** R
** byte-compile and prepare package for lazy loading
Warning in fun(libname, pkgname) : couldn't connect to display ""
Unable to find any JVMs matching version "(null)".
No Java runtime present, try --request to install.
Warning in system("/usr/libexec/java_home", intern = TRUE) :
  running command '/usr/libexec/java_home' had status 1
Error : .onLoad failed in loadNamespace() for 'mallet', details:
  call: NULL
  error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep/library.noindex/textmining/rJava/libs/rJava.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep/library.noindex/textmining/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/lionel/Desktop/rlang/revdep/library.noindex/textmining/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘textmining’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/textmining/old/textmining.Rcheck/textmining’

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

Version: 1.0-9

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.5Mb
      sub-directories of 1Mb or more:
        data   1.1Mb
        rda    7.0Mb
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

# tibbletime

Version: 0.1.1

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      8: rlang::quo_name(get_index_quo(.tbl_time)) at /Users/lionel/Desktop/rlang/revdep/checks.noindex/tibbletime/new/tibbletime.Rcheck/00_pkg_src/tibbletime/R/getters.R:28
      9: quo_squash(quo) at /private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/RtmpbSBfQ2/R.INSTALL38e96c47fc7c/rlang/R/quo.R:401
      10: is_quosure(quo) at /private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/RtmpbSBfQ2/R.INSTALL38e96c47fc7c/rlang/R/quo.R:346
      11: get_index_quo(.tbl_time) at /Users/lionel/Desktop/rlang/revdep/checks.noindex/tibbletime/new/tibbletime.Rcheck/00_pkg_src/tibbletime/R/getters.R:28
      12: glue_stop("Object is not of class `tbl_time`.") at /Users/lionel/Desktop/rlang/revdep/checks.noindex/tibbletime/new/tibbletime.Rcheck/00_pkg_src/tibbletime/R/getters.R:12
      13: stop(glue::glue(..., .sep, .envir = parent.frame()), call. = FALSE) at /Users/lionel/Desktop/rlang/revdep/checks.noindex/tibbletime/new/tibbletime.Rcheck/00_pkg_src/tibbletime/R/util.R:28
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 126 SKIPPED: 0 FAILED: 3
      1. Failure: nest() with index creates tbl_df (@test_compat-tidyr.R#25) 
      2. Failure: unnest() with index goes back to tbl_time (@test_compat-tidyr.R#40) 
      3. Error: unnest() with index goes back to tbl_time (@test_compat-tidyr.R#41) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# tidybayes

Version: 1.0.1

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
      OK: 236 SKIPPED: 2 FAILED: 1
      1. Error: tidy_draws works with runjags (@test.tidy_draws.R#83) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# tidygraph

Version: 1.1.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      14: mutate_as_tbl(.data, !!!dot) at /Users/lionel/Desktop/rlang/revdep/checks.noindex/tidygraph/new/tidygraph.Rcheck/00_pkg_src/tidygraph/R/mutate.R:7
      15: mutate(d_tmp, ...) at /Users/lionel/Desktop/rlang/revdep/checks.noindex/tidygraph/new/tidygraph.Rcheck/00_pkg_src/tidygraph/R/mutate.R:43
      16: mutate.tbl_df(d_tmp, ...)
      17: mutate_impl(.data, dots)
      18: group_optimal()
      19: membership(cluster_optimal(graph = .G(), weights = weights)) at /Users/lionel/Desktop/rlang/revdep/checks.noindex/tidygraph/new/tidygraph.Rcheck/00_pkg_src/tidygraph/R/group.R:124
      20: cluster_optimal(graph = .G(), weights = weights) at /Users/lionel/Desktop/rlang/revdep/checks.noindex/tidygraph/new/tidygraph.Rcheck/00_pkg_src/tidygraph/R/group.R:124
      
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

Version: 0.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘broom’ ‘ggplot2’ ‘infer’ ‘pillar’ ‘recipes’ ‘rsample’
      ‘tidyposterior’ ‘tidypredict’ ‘tidytext’ ‘yardstick’
      All declared Imports should be used.
    ```

# tidyquant

Version: 0.5.5

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        doc   4.1Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unable to find any JVMs matching version "(null)".
    No Java runtime present, try --request to install.
    ```

# tidyr

Version: 0.8.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 23 marked UTF-8 strings
    ```

# tidyRSS

Version: 1.2.6

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

# tidytext

Version: 0.1.9

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      15: stop("Input must be a character vector of any length or a list of character\n", "  vectors, each of which has a length of 1.")
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 203 SKIPPED: 0 FAILED: 8
      1. Error: Can cast tables into a sparse Matrix (@test-sparse-casters.R#13) 
      2. Error: Can cast_sparse with tidyeval (@test-sparse-casters.R#40) 
      3. Error: Can cast tables into a sparse DocumentTermMatrix (@test-sparse-casters.R#48) 
      4. Error: Can cast tables into a sparse TermDocumentMatrix (@test-sparse-casters.R#61) 
      5. Error: Can cast tables into a sparse dfm (@test-sparse-casters.R#74) 
      6. Error: can augment an stm output (@test-stm-tidiers.R#61) 
      7. Error: tf-idf with tidyeval works (@test-tf-idf.R#69) 
      8. Error: tokenizing with tidyeval works (@test-unnest-tokens.R#150) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
        intersect, setdiff, setequal, union
    
    Joining, by = "book"
    `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    Warning: Removed 896 rows containing non-finite values (stat_bin).
    Loading required package: NLP
    
    Attaching package: 'NLP'
    
    The following object is masked from 'package:ggplot2':
    
        annotate
    
    Warning: Trying to compute distinct() for variables not found in the data:
    - `row_col`, `column_col`
    This is an error, but only a warning is raised for compatibility reasons.
    The operation will return the input unchanged.
    Quitting from lines 93-113 (tidying_casting.Rmd) 
    Error: processing vignette 'tidying_casting.Rmd' failed with diagnostics:
    not yet implemented for matrix with typeof NULL
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Unable to find any JVMs matching version "(null)".
    No Java runtime present, try --request to install.
    ```

# tidytidbits

Version: 0.1.0

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tidytidbits-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: cross_tabulate
    > ### Title: Create cross table from a tibble
    > ### Aliases: cross_tabulate
    > 
    > ### ** Examples
    > 
    > library(magrittr)
    > if (requireNamespace("survival", quietly = TRUE))
    + {
    +    survival::bladder1 %>%
    +       cross_tabulate(treatment, recur) %>%
    +       chisq.test()
    + }
    Error: Expected a list of quosures
    Execution halted
    ```

# tidytransit

Version: 0.3.4

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
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
    plotPrimordiumProfile: no visible global function definition for
      ‘points’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/TimerQuant/new/TimerQuant.Rcheck/00_pkg_src/TimerQuant/R/plotPrimordiumProfile.R:24)
    plotPrimordiumProfile: no visible global function definition for
      ‘polygon’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/TimerQuant/new/TimerQuant.Rcheck/00_pkg_src/TimerQuant/R/plotPrimordiumProfile.R:26-27)
    plotPrimordiumProfile: no visible global function definition for ‘rgb’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/TimerQuant/new/TimerQuant.Rcheck/00_pkg_src/TimerQuant/R/plotPrimordiumProfile.R:26-27)
    simulatedRatio: no visible global function definition for ‘rnorm’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/TimerQuant/new/TimerQuant.Rcheck/00_pkg_src/TimerQuant/R/SAPSstochastic.R:4)
    simulatedRatio: no visible global function definition for ‘rnorm’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/TimerQuant/new/TimerQuant.Rcheck/00_pkg_src/TimerQuant/R/SAPSstochastic.R:5)
    Undefined global functions or variables:
      approxfun axis mad median optimize par plot points polygon predict
      rainbow rgb rnorm
    Consider adding
      importFrom("graphics", "axis", "par", "plot", "points", "polygon")
      importFrom("grDevices", "rainbow", "rgb")
      importFrom("stats", "approxfun", "mad", "median", "optimize",
                 "predict", "rnorm")
    to your NAMESPACE file.
    ```

# timescape

Version: 1.4.0

## In both

*   checking Rd \usage sections ... WARNING
    ```
    Duplicated \argument entries in documentation object 'timescapeOutput':
      ‘width’ ‘height’ ‘mutations’ ‘height’ ‘width’ ‘clonal_prev’
      ‘tree_edges’ ‘alpha’ ‘clonal_prev’ ‘tree_edges’ ‘genotype_position’
      ‘clone_colours’ ‘perturbations’ ‘mutations’ ‘tree_edges’
      ‘clonal_prev’ ‘clonal_prev’ ‘tree_edges’ ‘clone_colours’ ‘mutations’
    
    Functions with \usage entries need to have the appropriate \alias
    entries, and all their arguments documented.
    The \usage entries must correspond to syntactically valid R code.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .vscode
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘gtools’
      All declared Imports should be used.
    ```

*   checking R code for possible problems ... NOTE
    ```
    getMutationsData: no visible binding for global variable
      ‘show_warnings’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/timescape/new/timescape.Rcheck/00_pkg_src/timescape/R/timescape.R:653-657)
    Undefined global functions or variables:
      show_warnings
    ```

# timetk

Version: 0.1.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘devtools’ ‘forecast’
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

# TissueEnrich

Version: 1.0.6

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    Loading required package: graph
    
    Attaching package: 'graph'
    
    The following object is masked from 'package:XML':
    
        addNode
    
    Read 97 items
    No background list provided. Using all the
                    genes as background.
    Read 97 items
    No background list provided. Using all the
                    genes as background.
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'TissueEnrich.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
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
      installed size is  8.0Mb
      sub-directories of 1Mb or more:
        data      1.7Mb
        extdata   4.9Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      filterByTargetedSequences haplotypeBin HaplotypeBinDepth.mean
      HaplotypeBinDepth.sum HaplotypeDepth.mean
      HaplotypeDepth.mean.symmetric HaplotypeDepth.sum
      HaplotypeDepth.sum.symmetric HaplotypeFraction
      HaplotypeFraction.symmetric HaplotypeRatio HaplotypeRatio.1
      HaplotypeRatio.2 head keepChr Length.snp. lines loess
      logR_Copy_Number LogRatio lowess MajorCN Median_logR Median_Ratio
      MinorCN mtext na.omit nonRef par phasedAlleleFraction phasedCount
      phasedCount.haploSymmetric phaseSet phaseSet.aggr plot points predict
      queryHits read.delim rowRanges rowRanges<- Sample seq.info SNPs Start
      Start_Position.bp. Start.snp Start.telo subjectHits tail TITAN_call
      TITANcall TITANstate tumDepth uniroot unstrsplit write.table xtabs
    Consider adding
      importFrom("graphics", "abline", "axis", "lines", "mtext", "par",
                 "plot", "points")
      importFrom("methods", "as")
      importFrom("stats", "approxfun", "dunif", "loess", "lowess", "na.omit",
                 "predict", "uniroot", "xtabs")
      importFrom("utils", "head", "read.delim", "tail", "write.table")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports
    field contains 'methods').
    ```

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘HMMcopy’, ‘list’
    ```

# tmap

Version: 2.1-1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.8Mb
      sub-directories of 1Mb or more:
        data   1.4Mb
        doc    2.1Mb
        R      3.0Mb
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

Version: 3.8.4

## In both

*   R CMD check timed out
    

*   checking installed package size ... NOTE
    ```
      installed size is 15.1Mb
      sub-directories of 1Mb or more:
        data           1.9Mb
        example_data   8.0Mb
        R              2.1Mb
        test_data      1.9Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘broom’
      All declared Imports should be used.
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
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/TPP/new/TPP.Rcheck/00_pkg_src/TPP/R/plot_fSta_distribution.R:19-28)
    plot_pVal_distribution: no visible binding for global variable
      ‘..density..’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/TPP/new/TPP.Rcheck/00_pkg_src/TPP/R/plot_pVal_distribution.R:22-31)
    Undefined global functions or variables:
      ..density..
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

# tree.bins

Version: 0.1.1

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘forcats’
    ```

# treeio

Version: 1.4.3

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

# trelliscope

Version: 0.9.8

## In both

*   checking dependencies in R code ... NOTE
    ```
    Unable to find any JVMs matching version "(null)".
    No Java runtime present, try --request to install.
    ```

# trialr

Version: 0.0.3

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Loading required package: Rcpp
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'BEBOP.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 14.5Mb
      sub-directories of 1Mb or more:
        libs  12.7Mb
    ```

# tricolore

Version: 1.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 88 marked UTF-8 strings
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

# tropr

Version: 0.1.2

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Failed to find tvtrope content
      1: as.data.frame(trope_content(.url)) at testthat/test-tropr.R:26
      2: trope_content(.url)
      3: stop("Failed to find tvtrope content") at /Users/lionel/Desktop/rlang/revdep/checks.noindex/tropr/new/tropr.Rcheck/00_pkg_src/tropr/R/tropr.R:40
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 3 SKIPPED: 0 FAILED: 5
      1. Error: trope_data (@test-data.R#6) 
      2. Error: trope_cache (@test-data.R#19) 
      3. Error: trope_content, as.data.frame (@test-tropr.R#5) 
      4. Error: trope_history, aggr (@test-tropr.R#14) 
      5. Error: More cases (@test-tropr.R#26) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      The vignette engine knitr::rmarkdown is not available, because the rmarkdown package is not installed. Please install it.
    Quitting from lines 41-47 (quickstart.Rmd) 
    Error: processing vignette 'quickstart.Rmd' failed with diagnostics:
    Failed to find tvtrope content
    Execution halted
    ```

# trread

Version: 0.2.5

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘here’ ‘htmltools’ ‘magrittr’ ‘scales’ ‘stringr’
      All declared Imports should be used.
    ```

# tsibble

Version: 0.5.2

## Newly broken

*   checking examples ... ERROR
    ```
    ...
         Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
    1949 112 118 132 129 121 135 148 148 136 119 104 118
    1950 115 126 141 135 125 149 170 170 158 133 114 140
    1951 145 150 178 163 172 178 199 199 184 162 146 166
    1952 171 180 193 181 183 218 230 242 209 191 172 194
    1953 196 196 236 235 229 243 264 272 237 211 180 201
    1954 204 188 235 227 234 264 302 293 259 229 203 229
    1955 242 233 267 269 270 315 364 347 312 274 237 278
    1956 284 277 317 313 318 374 413 405 355 306 271 306
    1957 315 301 356 348 355 422 465 467 404 347 305 336
    1958 340 318 362 348 363 435 491 505 404 359 310 337
    1959 360 342 406 396 420 472 548 559 463 407 362 405
    1960 417 391 419 461 472 535 622 606 508 461 390 432
    > 
    > # equally spaced over trading days, not smart enough to guess frequency ----
    > x2 <- as_tsibble(EuStockMarkets)
    > head(as.ts(x2, frequency = 260))
    Warning: The `tbl_ts` is not sorted by `c("~", "key")`, and `index`.
    Warning: The `tbl_ts` is not sorted by `index`.
    Error: Invalid tsibble: identical data entries from `index`. Use `find_duplicates()` to check the duplicated rows.
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 618 SKIPPED: 1 FAILED: 10
      1.  Failure: nest() (@test-tidyr.R#68) 
      2.  Failure: nest() (@test-tidyr.R#69) 
      3.  Failure: nest() (@test-tidyr.R#74) 
      4.  Error: nest() (@test-tidyr.R#78) 
      5.  Error: unnest() (@test-tidyr.R#89) 
      6.  Failure: dplyr verbs for lst_ts (@test-tidyr.R#98) 
      7.  Error: dplyr verbs for lst_ts (@test-tidyr.R#102) 
      8.  Failure: Space in index variable (@test-tsibble.R#87) 
      9.  Error: Spectial characters in column names (@test-tsibble.R#352) 
      10. Error: a tsibble with more than one measured vars (@test-tsibble2ts.R#45) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

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

# TSstudio

Version: 0.1.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘colormap’
      All declared Imports should be used.
    ```

# turfR

Version: 0.8-7

## In both

*   checking R code for possible problems ... NOTE
    ```
    turf: no visible global function definition for ‘read.table’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/turfR/new/turfR.Rcheck/00_pkg_src/turfR/R/turfR_0.8-7.R:12)
    turf: no visible global function definition for ‘flush.console’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/turfR/new/turfR.Rcheck/00_pkg_src/turfR/R/turfR_0.8-7.R:102)
    turf.combos: no visible global function definition for ‘combn’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/turfR/new/turfR.Rcheck/00_pkg_src/turfR/R/turfR_0.8-7.R:158)
    Undefined global functions or variables:
      combn flush.console read.table
    Consider adding
      importFrom("utils", "combn", "flush.console", "read.table")
    to your NAMESPACE file.
    ```

# ubci

Version: 0.0.3

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      10: httr::content(.)
      11: parse_auto(raw, type, encoding, ...)
      12: parser(x = content, type = type, encoding = encoding, ...)
      13: need_package("xml2")
      14: stop("Please install ", pkg, " package", call. = FALSE)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 12 SKIPPED: 0 FAILED: 4
      1. Error: index colume chk (@test-ubci_index_options.R#9) 
      2. Error: lang ko chk (@test-ubci_index_options.R#15) 
      3. Error: index check 2 (@test-ubci_index.R#17) 
      4. Error: index check 3 (@test-ubci_index.R#24) 
      
      Error: testthat unit tests failed
      Execution halted
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

# useful

Version: 1.2.5

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      x[5]: "8,750,038h"
      y[5]: "8,750,037.80h"
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 738 SKIPPED: 0 FAILED: 6
      1. Failure: All functions return correct output (@test-formatters.r#80) 
      2. Failure: All functions return correct output (@test-formatters.r#84) 
      3. Failure: All functions return correct output (@test-formatters.r#89) 
      4. Failure: All functions return correct output (@test-formatters.r#93) 
      5. Failure: All functions return correct output (@test-formatters.r#102) 
      6. Failure: All functions return correct output (@test-formatters.r#104) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# usethis

Version: 1.4.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘magick’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘magick’
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

Version: 1.0.5

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

# vapour

Version: 0.1.0

## In both

*   checking whether package ‘vapour’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/vapour/new/vapour.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘vapour’ ...
** package ‘vapour’ successfully unpacked and MD5 sums checked
configure: CC: clang
configure: CXX: clang++
checking for gdal-config... /usr/local/bin/gdal-config
checking gdal-config usability... yes
configure: GDAL: 2.3.1
checking GDAL version >= 2.0.0... yes
checking for gcc... clang
checking whether the C compiler works... yes
checking for C compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... no
checking for suffix of object files... o
checking whether we are using the GNU C compiler... yes
checking whether clang accepts -g... yes
checking for clang option to accept ISO C89... none needed
checking how to run the C preprocessor... clang -E
checking for grep that handles long lines and -e... /usr/bin/grep
checking for egrep... /usr/bin/grep -E
checking for ANSI C header files... yes
checking for sys/types.h... yes
checking for sys/stat.h... yes
checking for stdlib.h... yes
checking for string.h... yes
checking for memory.h... yes
checking for strings.h... yes
checking for inttypes.h... yes
checking for stdint.h... yes
checking for unistd.h... yes
checking gdal.h usability... yes
checking gdal.h presence... yes
checking for gdal.h... yes
checking GDAL: linking with --libs only... no
checking GDAL: linking with --libs and --dep-libs... no
In file included from gdal_test.cpp:1:
In file included from /usr/local/include/gdal.h:45:
/usr/local/include/cpl_port.h:187:6: error: Must have C++11 or newer.
#    error Must have C++11 or newer.
     ^
1 error generated.
In file included from gdal_test.cpp:1:
In file included from /usr/local/include/gdal.h:45:
/usr/local/include/cpl_port.h:187:6: error: Must have C++11 or newer.
#    error Must have C++11 or newer.
     ^
1 error generated.
configure: Install failure: compilation and/or linkage problems.
configure: error: GDALAllRegister not found in libgdal.
ERROR: configuration failed for package ‘vapour’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/vapour/new/vapour.Rcheck/vapour’

```
### CRAN

```
* installing *source* package ‘vapour’ ...
** package ‘vapour’ successfully unpacked and MD5 sums checked
configure: CC: clang
configure: CXX: clang++
checking for gdal-config... /usr/local/bin/gdal-config
checking gdal-config usability... yes
configure: GDAL: 2.3.1
checking GDAL version >= 2.0.0... yes
checking for gcc... clang
checking whether the C compiler works... yes
checking for C compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... no
checking for suffix of object files... o
checking whether we are using the GNU C compiler... yes
checking whether clang accepts -g... yes
checking for clang option to accept ISO C89... none needed
checking how to run the C preprocessor... clang -E
checking for grep that handles long lines and -e... /usr/bin/grep
checking for egrep... /usr/bin/grep -E
checking for ANSI C header files... yes
checking for sys/types.h... yes
checking for sys/stat.h... yes
checking for stdlib.h... yes
checking for string.h... yes
checking for memory.h... yes
checking for strings.h... yes
checking for inttypes.h... yes
checking for stdint.h... yes
checking for unistd.h... yes
checking gdal.h usability... yes
checking gdal.h presence... yes
checking for gdal.h... yes
checking GDAL: linking with --libs only... no
checking GDAL: linking with --libs and --dep-libs... no
In file included from gdal_test.cpp:1:
In file included from /usr/local/include/gdal.h:45:
/usr/local/include/cpl_port.h:187:6: error: Must have C++11 or newer.
#    error Must have C++11 or newer.
     ^
1 error generated.
In file included from gdal_test.cpp:1:
In file included from /usr/local/include/gdal.h:45:
/usr/local/include/cpl_port.h:187:6: error: Must have C++11 or newer.
#    error Must have C++11 or newer.
     ^
1 error generated.
configure: Install failure: compilation and/or linkage problems.
configure: error: GDALAllRegister not found in libgdal.
ERROR: configuration failed for package ‘vapour’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/vapour/old/vapour.Rcheck/vapour’

```
# vdmR

Version: 0.2.6

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘maptools’ ‘Rdpack’ ‘rgeos’
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
      installed size is  6.2Mb
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

# visdat

Version: 0.5.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘plotly’ ‘rlang’
      All declared Imports should be used.
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
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/vlad/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/vlad/RcppArmadillo/include" -I/usr/local/include  -fopenmp -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c RcppExports.cpp -o RcppExports.o
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
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/vlad/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/vlad/RcppArmadillo/include" -I/usr/local/include  -fopenmp -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘vlad’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/vlad/old/vlad.Rcheck/vlad’

```
# voxel

Version: 1.3.5

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘methods’
      All declared Imports should be used.
    ```

# vqtl

Version: 2.0.4

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘iterators’ ‘knitr’ ‘purrr’ ‘testthat’
      All declared Imports should be used.
    ```

# vsn

Version: 3.48.1

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘affydata’
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    ...
    The following objects are masked from 'package:base':
    
        anyDuplicated, append, as.data.frame, basename, cbind, colMeans,
        colnames, colSums, dirname, do.call, duplicated, eval, evalq,
        Filter, Find, get, grep, grepl, intersect, is.unsorted, lapply,
        lengths, Map, mapply, match, mget, order, paste, pmax, pmax.int,
        pmin, pmin.int, Position, rank, rbind, Reduce, rowMeans,
        rownames, rowSums, sapply, setdiff, sort, table, tapply, union,
        unique, unsplit, which, which.max, which.min
    
    Welcome to Bioconductor
    
        Vignettes contain introductory material; view with
        'browseVignettes()'. To cite Bioconductor, see
        'citation("Biobase")', and for packages 'citation("pkgname")'.
    
    convert: profile 'icc': 'RGB ': RGB color space not permitted on grayscale PNG `A-vsn_files/figure-html/nkid-scp-1.png' @ warning/png.c/MagickPNGWarningHandler/1656.
    Quitting from lines 256-259 (A-vsn.Rmd) 
    Error: processing vignette 'A-vsn.Rmd' failed with diagnostics:
    there is no package called 'affydata'
    Execution halted
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

*   R CMD check timed out
    

*   checking installed package size ... NOTE
    ```
      installed size is 14.2Mb
      sub-directories of 1Mb or more:
        doc    2.2Mb
        libs  11.6Mb
    ```

# wallace

Version: 1.0.5

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dismo’ ‘dplyr’ ‘DT’ ‘ENMeval’ ‘leaflet.extras’ ‘maptools’ ‘raster’
      ‘RColorBrewer’ ‘rgdal’ ‘rgeos’ ‘rmarkdown’ ‘shinyjs’ ‘shinythemes’
      ‘spocc’ ‘spThin’ ‘testthat’ ‘XML’
      All declared Imports should be used.
    ```

# wand

Version: 0.2.0

## In both

*   checking whether package ‘wand’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/wand/new/wand.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘wand’ ...
** package ‘wand’ successfully unpacked and MD5 sums checked
Checking to see if libmagic is available...
** libs
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -L/usr/local/include -L/opt/local/include -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/wand/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c RcppExports.cpp -o RcppExports.o
clang: warning: argument unused during compilation: '-L/usr/local/include' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-L/opt/local/include' [-Wunused-command-line-argument]
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -L/usr/local/include -L/opt/local/include -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/wand/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c wand.cpp -o wand.o
clang: warning: argument unused during compilation: '-L/usr/local/include' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-L/opt/local/include' [-Wunused-command-line-argument]
In file included from wand.cpp:13:
./magic.h:123:2: warning: extra ';' outside of a function is a C++11 extension [-Wc++11-extra-semi]
};
 ^
1 warning generated.
clang++ -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o wand.so RcppExports.o wand.o -L/usr/local/lib -L/opt/local/lib -L/usr/lib -lmagic -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
ld: library not found for -lmagic
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [wand.so] Error 1
ERROR: compilation failed for package ‘wand’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/wand/new/wand.Rcheck/wand’

```
### CRAN

```
* installing *source* package ‘wand’ ...
** package ‘wand’ successfully unpacked and MD5 sums checked
Checking to see if libmagic is available...
** libs
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -L/usr/local/include -L/opt/local/include -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/wand/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c RcppExports.cpp -o RcppExports.o
clang: warning: argument unused during compilation: '-L/usr/local/include' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-L/opt/local/include' [-Wunused-command-line-argument]
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -L/usr/local/include -L/opt/local/include -I"/Users/lionel/Desktop/rlang/revdep/library.noindex/wand/Rcpp/include" -I/usr/local/include   -fPIC  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -O0 -c wand.cpp -o wand.o
clang: warning: argument unused during compilation: '-L/usr/local/include' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-L/opt/local/include' [-Wunused-command-line-argument]
In file included from wand.cpp:13:
./magic.h:123:2: warning: extra ';' outside of a function is a C++11 extension [-Wc++11-extra-semi]
};
 ^
1 warning generated.
clang++ -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o wand.so RcppExports.o wand.o -L/usr/local/lib -L/opt/local/lib -L/usr/lib -lmagic -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
ld: library not found for -lmagic
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [wand.so] Error 1
ERROR: compilation failed for package ‘wand’
* removing ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/wand/old/wand.Rcheck/wand’

```
# wbstats

Version: 0.2

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1528 marked UTF-8 strings
    ```

# weathercan

Version: 0.2.7

## In both

*   checking examples ... ERROR
    ```
    ...
    The error most likely occurred in:
    
    > ### Name: tz_calc
    > ### Title: Get timezone from lat/lon
    > ### Aliases: tz_calc get_tz
    > 
    > ### ** Examples
    > 
    > 
    > # Daylight savings
    > tz_calc(lat = 53.881857, lon = -122.786271)
    character(0)
    > tz_calc(coords = c(53.881857, -122.786271))
    character(0)
    > 
    > # No daylight savings
    > tz_calc(lat = 53.881857, lon = -122.786271, etc = TRUE)
    Error in if (tz <= 0) tz <- paste0("+", abs(tz)) else tz <- paste0("-",  : 
      argument is of length zero
    Calls: tz_calc
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 417 SKIPPED: 5 FAILED: 13
      1. Failure: tz_calc() returns the correct tz (@test_02_utils.R#4) 
      2. Error: tz_calc() returns the correct tz (@test_02_utils.R#6) 
      3. Error: weather (hour) returns a data frame (@test_06_weather_dl.R#6) 
      4. Error: weather (hour) formats timezone display (@test_06_weather_dl.R#34) 
      5. Error: weather (hour) formats timezone to UTC with multiple zones (@test_06_weather_dl.R#44) 
      6. Error: weather (hour) gets all (@test_06_weather_dl.R#61) 
      7. Error: weather (hour) trims NAs (@test_06_weather_dl.R#73) 
      8. Error: weather (hour) multiple stations (@test_06_weather_dl.R#80) 
      9. Error: weather (hour) no data fails nicely (@test_06_weather_dl.R#95) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Quitting from lines 87-90 (weathercan.Rmd) 
    Error: processing vignette 'weathercan.Rmd' failed with diagnostics:
    argument is of length zero
    Execution halted
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 25 marked UTF-8 strings
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
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/wiggleplotr/new/wiggleplotr.Rcheck/00_pkg_src/wiggleplotr/R/wiggleplotr.R:184)
    plotCoverage: no visible global function definition for ‘is’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/wiggleplotr/new/wiggleplotr.Rcheck/00_pkg_src/wiggleplotr/R/wiggleplotr.R:185)
    plotTranscripts: no visible global function definition for ‘is’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/wiggleplotr/new/wiggleplotr.Rcheck/00_pkg_src/wiggleplotr/R/wiggleplotr.R:33)
    plotTranscripts: no visible global function definition for ‘is’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/wiggleplotr/new/wiggleplotr.Rcheck/00_pkg_src/wiggleplotr/R/wiggleplotr.R:34)
    Undefined global functions or variables:
      is
    Consider adding
      importFrom("methods", "is")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports
    field contains 'methods').
    ```

# windfarmGA

Version: 1.2.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘data.table’ ‘RandomFields’
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

# XBSeq

Version: 1.12.0

## In both

*   R CMD check timed out
    

*   checking whether the namespace can be loaded with stated dependencies ... NOTE
    ```
    Warning: no function found corresponding to methods exports from ‘XBSeq’ for: ‘conditions’, ‘conditions<-’, ‘dispTable’
    
    A namespace must be able to be loaded with just the base namespace
    loaded: otherwise if the namespace gets loaded by a saved object,
    the session will be unable to start.
    
    Probably some imports need to be declared in the NAMESPACE file.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘conditions’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/XBSeq/new/XBSeq.Rcheck/00_pkg_src/XBSeq/R/core_functions.R:106)
    estimateSCV,XBSeqDataSet: no visible global function definition for
      ‘conditions’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/XBSeq/new/XBSeq.Rcheck/00_pkg_src/XBSeq/R/core_functions.R:107)
    estimateSCV,XBSeqDataSet: no visible global function definition for
      ‘dispTable<-’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/XBSeq/new/XBSeq.Rcheck/00_pkg_src/XBSeq/R/core_functions.R:108)
    Undefined global functions or variables:
      ..count.. assay assay<- assays baseMean coefficients complete.cases
      conditions cor data DataFrame ddelap dispTable dispTable<- dnbinom
      dpois formula Gamma glm Group log2FoldChange median optim p.adjust
      pbeta predict qbeta quantile rnbinom Sample scvBiasCorrectionFits
      SummarizedExperiment
    Consider adding
      importFrom("stats", "coefficients", "complete.cases", "cor", "dnbinom",
                 "dpois", "formula", "Gamma", "glm", "median", "optim",
                 "p.adjust", "pbeta", "predict", "qbeta", "quantile",
                 "rnbinom")
      importFrom("utils", "data")
    to your NAMESPACE file.
    ```

# XGR

Version: 1.1.4

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.8Mb
      sub-directories of 1Mb or more:
        data   1.1Mb
        R      4.0Mb
    ```

# XKCDdata

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tibble’
      All declared Imports should be used.
    ```

# xpose4

Version: 4.6.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        R   4.0Mb
    ```

# xtractomatic

Version: 3.4.2

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    date	lon	lat	lowLon	higLon	lowLat	higLat
    4/23/2003	203.899	19.664	203.899	203.899	19.664	19.664
    4/24/2003	204.151	19.821	203.912597	204.389403	18.78051934	20.86148066
    4/30/2003	203.919	20.351	203.6793669	204.1586331	18.79728188	21.90471812
    5/1/2003	204.229	20.305	203.9943343	204.4636657	18.90440013	21.70559987
    Quitting from lines 818-843 (Usingxtractomatic.Rmd) 
    Error: processing vignette 'Usingxtractomatic.Rmd' failed with diagnostics:
    (converted from warning) Removed 4070 rows containing missing values (geom_raster).
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# Zelig

Version: 5.1.6

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.1Mb
      sub-directories of 1Mb or more:
        R   6.0Mb
    ```

# zeligverse

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Amelia’ ‘MatchIt’ ‘WhatIf’
      All declared Imports should be used.
    ```

# zFactor

Version: 0.1.7

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
    
    pandoc-citeproc: when expecting a product (:*:), encountered Object instead
    Error running filter /usr/local/bin/pandoc-citeproc:
    Filter returned error status 1
    Error: processing vignette 'Ann10.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 83
    Execution halted
    ```

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
    ...
    PlotGaussianFitDF: no visible binding for global variable ‘density’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/zFPKM/new/zFPKM.Rcheck/00_pkg_src/zFPKM/R/zfpkm.R:223)
    PlotGaussianFitDF: no visible binding for global variable ‘log2fpkm’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/zFPKM/new/zFPKM.Rcheck/00_pkg_src/zFPKM/R/zfpkm.R:223)
    PlotGaussianFitDF: no visible binding for global variable ‘sample_name’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/zFPKM/new/zFPKM.Rcheck/00_pkg_src/zFPKM/R/zfpkm.R:223)
    PlotGaussianFitDF: no visible binding for global variable ‘log2fpkm’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/zFPKM/new/zFPKM.Rcheck/00_pkg_src/zFPKM/R/zfpkm.R:227-233)
    PlotGaussianFitDF: no visible binding for global variable ‘density’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/zFPKM/new/zFPKM.Rcheck/00_pkg_src/zFPKM/R/zfpkm.R:227-233)
    zFPKMCalc: no visible global function definition for ‘density’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/zFPKM/new/zFPKM.Rcheck/00_pkg_src/zFPKM/R/zfpkm.R:162)
    zFPKMTransform: no visible global function definition for ‘is’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/zFPKM/new/zFPKM.Rcheck/00_pkg_src/zFPKM/R/zfpkm.R:125-127)
    Undefined global functions or variables:
      density dnorm is log2fpkm sample_name
    Consider adding
      importFrom("methods", "is")
      importFrom("stats", "density", "dnorm")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports
    field contains 'methods').
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

# ztype

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘ggplot2’ ‘lubridate’
      All declared Imports should be used.
    ```

