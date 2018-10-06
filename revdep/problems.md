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
# datadr

Version: 0.8.6.1

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

# hpiR

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘knitr’
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

# kokudosuuchi

Version: 0.4.2

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 52458 marked UTF-8 strings
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

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘futile.options’ ‘ggfortify’ ‘grDevices’ ‘stats’
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

# markmyassignment

Version: 0.8.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘lazyeval’
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

# MlBayesOpt

Version: 0.3.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘data.table’ ‘foreach’
      All declared Imports should be used.
    ```

# modelr

Version: 0.1.2

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘lme4’, ‘rstanarm’
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

# openair

Version: 2.5-0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.1Mb
      sub-directories of 1Mb or more:
        R   4.0Mb
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

# particles

Version: 0.2.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
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

# pkgdown

Version: 1.1.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘magick’
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

Version: 4.8.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.1Mb
      sub-directories of 1Mb or more:
        htmlwidgets   3.1Mb
        R             2.3Mb
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

Version: 0.4.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 31 marked UTF-8 strings
    ```

# RSDA

Version: 2.0.7

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

# visdat

Version: 0.5.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘plotly’ ‘rlang’
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

# wordbankr

Version: 0.3.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dbplyr’
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

