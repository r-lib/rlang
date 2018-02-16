# amt

Version: 0.0.2.0

## In both

*   checking whether package ‘amt’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: No Rd macros in package 'Rdpack'.
    See ‘/Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/checks.noindex/amt/new/amt.Rcheck/00install.out’ for details.
    ```

*   checking Rd files ... WARNING
    ```
    Warning: No Rd macros in package 'Rdpack'.
    prepare_Rd: adjust_param.Rd:84: unknown macro '\insertRef'
    prepare_Rd: movement_metrics.Rd:51: unknown macro '\insertRef'
    prepare_Rd: movement_metrics.Rd:52: unknown macro '\insertRef'
    prepare_Rd: movement_metrics.Rd:53: unknown macro '\insertRef'
    prepare_Rd: sim_ud.Rd:58: unknown macro '\insertRef'
    prepare_Rd: sim_ud.Rd:59: unknown macro '\insertRef'
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘Rcpp’
      All declared Imports should be used.
    ```

# auk

Version: 0.1.0

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘auk-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: auk_country
    > ### Title: Filter the eBird data by country
    > ### Aliases: auk_country
    > 
    > ### ** Examples
    > 
    > # country names and ISO2 codes can be mixed
    > # not case sensitive
    > country <- c("CA", "United States", "mexico")
    > system.file("extdata/ebd-sample.txt", package = "auk") %>%
    +   auk_ebd() %>%
    +   auk_country(country)
    Error: 'countrycode_data' is not an exported object from 'namespace:countrycode'
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      9: auk_country(., country)
      10: auk_country.auk_ebd(., country)
      11: tolower(countrycode::countrycode_data$iso2c)
      12: countrycode::countrycode_data
      13: getExportedValue(pkg, name)
      14: stop(gettextf("'%s' is not an exported object from 'namespace:%s'", name, getNamespaceName(ns)), 
             call. = FALSE, domain = NA)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 199 SKIPPED: 12 FAILED: 2
      1. Error: auk_ebd prints method (@test_auk-ebd.r#66) 
      2. Error: auk_country (@test_filters.r#36) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 173-190 (auk.Rmd) 
    Error: processing vignette 'auk.Rmd' failed with diagnostics:
    'countrycode_data' is not an exported object from 'namespace:countrycode'
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Missing or unexported object: ‘countrycode::countrycode_data’
    ```

# banR

Version: 0.2.0

## Newly broken

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

# bayesplot

Version: 1.4.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.7Mb
      sub-directories of 1Mb or more:
        R     1.6Mb
        doc   3.6Mb
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

# dbplot

Version: 0.2.0

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      > library(dplyr)
      > library(ggplot2)
      > 
      > test_check("dbplot")
      ── 1. Failure: Correct binwidth formula is returned  ───────────────────────────
      db_bin(var, binwidth = 10) not equal to rlang::expr(...).
      target, current do not match when deparsed
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 11 SKIPPED: 0 FAILED: 1
      1. Failure: Correct binwidth formula is returned 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# dbplyr

Version: 1.2.0

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 526 SKIPPED: 11 FAILED: 8
      1. Failure: filter and mutate translate is.na correctly (@test-translate-mssql.r#80) 
      2. Failure: custom scalar translated correctly (@test-translate-postgresql.R#13) 
      3. Failure: custom scalar translated correctly (@test-translate-postgresql.R#14) 
      4. Failure: custom scalar translated correctly (@test-translate-postgresql.R#15) 
      5. Failure: is.na and is.null are equivalent (@test-translate.r#22) 
      6. Failure: is.na and is.null are equivalent (@test-translate.r#23) 
      7. Failure: is.na and is.null are equivalent (@test-translate.r#25) 
      8. Failure: is.na and is.null are equivalent (@test-translate.r#26) 
      
      Error: testthat unit tests failed
      In addition: Warning message:
      call dbDisconnect() when finished working with a connection 
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tibble’
      All declared Imports should be used.
    ```

# detrendr

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ijtiff’
      All declared Imports should be used.
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# dexter

Version: 0.6.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 109 marked UTF-8 strings
    ```

# DiagrammeR

Version: 0.9.2

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > #> [1] 2
    > 
    > # Add edges by specifying node `label`
    > # values; note that all nodes must have
    > # unique `label` values to use this option
    > graph <-
    +   graph %>%
    +   add_edge(
    +     from = "three",
    +     to = "four",
    +     rel = "L") %>%
    +   add_edge(
    +     from = "four",
    +     to = "one",
    +     rel = "L")
    > 
    > # Use the `get_edges()` function to verify
    > # that the edges were added
    > get_edges(graph)
    Error: `UQ()` can only be used within a quasiquoted argument
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 884 SKIPPED: 0 FAILED: 113
      1. Error: forward edges can be added given a selection of edges (@test-add_forward_reverse_edges.R#20) 
      2. Error: reverse edges can be added given a selection of edges (@test-add_forward_reverse_edges.R#65) 
      3. Error: Adding a balanced tree is possible (@test-add_graphs.R#26) 
      4. Error: Adding a cycle is possible (@test-add_graphs.R#119) 
      5. Error: Adding a path is possible (@test-add_graphs.R#264) 
      6. Error: Adding a prism is possible (@test-add_graphs.R#409) 
      7. Error: Adding a star is possible (@test-add_graphs.R#554) 
      8. Error: Adding a full graph is possible (@test-add_graphs.R#703) 
      9. Error: adding a node to a graph is possible (@test-add_node_edge.R#72) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

# dplyr

Version: 0.7.4

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      3: eval_bare(get_expr(quo), get_env(quo))
      4: summarise(gdf, out = 1:2)
      5: summarise.tbl_df(gdf, out = 1:2)
      6: summarise_impl(.data, dots)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 2670 SKIPPED: 6 FAILED: 3
      1. Error: mutate() supports unquoted values (@test-mutate.r#721) 
      2. Failure: summarise() supports unquoted values (@test-summarise.r#980) 
      3. Error: summarise() supports unquoted values (@test-summarise.r#985) 
      
      Error: testthat unit tests failed
      In addition: Warning message:
      call dbDisconnect() when finished working with a connection 
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.3Mb
      sub-directories of 1Mb or more:
        libs   4.5Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4 marked UTF-8 strings
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

# epitable

Version: 0.1.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘magrittr’ ‘readr’
      All declared Imports should be used.
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

# ggeffects

Version: 0.3.1

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘ordinal’
    ```

# idealstan

Version: 0.2.7

## In both

*   R CMD check timed out
    

*   checking installed package size ... NOTE
    ```
      installed size is  7.9Mb
      sub-directories of 1Mb or more:
        data   3.5Mb
        libs   3.6Mb
    ```

# implyr

Version: 0.2.2

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(RJDBC)
      Loading required package: rJava
      Unable to find any JVMs matching version "(null)".
      No Java runtime present, try --request to install.
      Error: package or namespace load failed for 'rJava':
       .onLoad failed in loadNamespace() for 'rJava', details:
        call: dyn.load(file, DLLpath = DLLpath, ...)
        error: unable to load shared object '/Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/library.noindex/implyr/rJava/libs/rJava.so':
        dlopen(/Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/library.noindex/implyr/rJava/libs/rJava.so, 6): Library not loaded: @rpath/libjvm.dylib
        Referenced from: /Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/library.noindex/implyr/rJava/libs/rJava.so
        Reason: image not found
      Error: package 'rJava' could not be loaded
      In addition: Warning message:
      running command '/usr/libexec/java_home' had status 1 
      Execution halted
    ```

# jpmesh

Version: 1.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘readr’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 176 marked UTF-8 strings
    ```

# jpndistrict

Version: 0.3.0

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

# markmyassignment

Version: 0.8.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘lazyeval’
      All declared Imports should be used.
    ```

# metacoder

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ggrepel’ ‘reshape’
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

# mudata2

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘hms’ ‘methods’
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

# nonmemica

Version: 0.7.9

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      The vignette engine knitr::rmarkdown is not available, because the rmarkdown package is not installed. Please install it.
    Unable to find any JVMs matching version "(null)".
    No Java runtime present, try --request to install.
    Quitting from lines 197-201 (parameter-table.Rmd) 
    Error: processing vignette 'parameter-table.Rmd' failed with diagnostics:
    package 'ReporteRsjars' could not be loaded
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unable to find any JVMs matching version "(null)".
    No Java runtime present, try --request to install.
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
    
    pandoc: Could not fetch /Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/library.noindex/nyctaxi/leaflet/htmlwidgets/lib/leaflet/#default#VML
    /Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/library.noindex/nyctaxi/leaflet/htmlwidgets/lib/leaflet/: openBinaryFile: inappropriate type (is a directory)
    Error: processing vignette 'nyc_taxi.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 67
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

# pointblank

Version: 0.1

## In both

*   checking whether package ‘pointblank’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/checks.noindex/pointblank/new/pointblank.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘pointblank’ ...
** package ‘pointblank’ successfully unpacked and MD5 sums checked
** R
** inst
** preparing package for lazy loading
Unable to find any JVMs matching version "(null)".
No Java runtime present, try --request to install.
Warning: running command '/usr/libexec/java_home' had status 1
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/library.noindex/pointblank/rJava/libs/rJava.so':
  dlopen(/Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/library.noindex/pointblank/rJava/libs/rJava.so, 6): Library not loaded: @rpath/libjvm.dylib
  Referenced from: /Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/library.noindex/pointblank/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘pointblank’
* removing ‘/Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/checks.noindex/pointblank/new/pointblank.Rcheck/pointblank’

```
### CRAN

```
* installing *source* package ‘pointblank’ ...
** package ‘pointblank’ successfully unpacked and MD5 sums checked
** R
** inst
** preparing package for lazy loading
Unable to find any JVMs matching version "(null)".
No Java runtime present, try --request to install.
Warning: running command '/usr/libexec/java_home' had status 1
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/library.noindex/pointblank/rJava/libs/rJava.so':
  dlopen(/Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/library.noindex/pointblank/rJava/libs/rJava.so, 6): Library not loaded: @rpath/libjvm.dylib
  Referenced from: /Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/library.noindex/pointblank/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘pointblank’
* removing ‘/Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/checks.noindex/pointblank/old/pointblank.Rcheck/pointblank’

```
# poppr

Version: 2.6.1

## In both

*   checking whether package ‘poppr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/checks.noindex/poppr/new/poppr.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘poppr’ ...
** package ‘poppr’ successfully unpacked and MD5 sums checked
** libs
clang -I/Library/Frameworks/R.framework/Resources/include -DNDEBUG   -I/usr/local/include  -fopenmp -fPIC  -Wall  -pedantic -O0 -c bitwise_distance.c -o bitwise_distance.o
clang: error: unsupported option '-fopenmp'
make: *** [bitwise_distance.o] Error 1
ERROR: compilation failed for package ‘poppr’
* removing ‘/Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/checks.noindex/poppr/new/poppr.Rcheck/poppr’

```
### CRAN

```
* installing *source* package ‘poppr’ ...
** package ‘poppr’ successfully unpacked and MD5 sums checked
** libs
clang -I/Library/Frameworks/R.framework/Resources/include -DNDEBUG   -I/usr/local/include  -fopenmp -fPIC  -Wall  -pedantic -O0 -c bitwise_distance.c -o bitwise_distance.o
clang: error: unsupported option '-fopenmp'
make: *** [bitwise_distance.o] Error 1
ERROR: compilation failed for package ‘poppr’
* removing ‘/Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/checks.noindex/poppr/old/poppr.Rcheck/poppr’

```
# prisonbrief

Version: 0.1.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2 marked UTF-8 strings
    ```

# proustr

Version: 0.2.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 20105 marked UTF-8 strings
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

# rclimateca

Version: 1.0.0

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

# rhmmer

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
    See ‘/Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/checks.noindex/RtutoR/new/RtutoR.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘RtutoR’ ...
** package ‘RtutoR’ successfully unpacked and MD5 sums checked
** R
** inst
** preparing package for lazy loading
Unable to find any JVMs matching version "(null)".
No Java runtime present, try --request to install.
Warning: running command '/usr/libexec/java_home' had status 1
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/library.noindex/RtutoR/rJava/libs/rJava.so':
  dlopen(/Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/library.noindex/RtutoR/rJava/libs/rJava.so, 6): Library not loaded: @rpath/libjvm.dylib
  Referenced from: /Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/library.noindex/RtutoR/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘RtutoR’
* removing ‘/Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/checks.noindex/RtutoR/new/RtutoR.Rcheck/RtutoR’

```
### CRAN

```
* installing *source* package ‘RtutoR’ ...
** package ‘RtutoR’ successfully unpacked and MD5 sums checked
** R
** inst
** preparing package for lazy loading
Unable to find any JVMs matching version "(null)".
No Java runtime present, try --request to install.
Warning: running command '/usr/libexec/java_home' had status 1
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/library.noindex/RtutoR/rJava/libs/rJava.so':
  dlopen(/Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/library.noindex/RtutoR/rJava/libs/rJava.so, 6): Library not loaded: @rpath/libjvm.dylib
  Referenced from: /Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/library.noindex/RtutoR/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘RtutoR’
* removing ‘/Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/checks.noindex/RtutoR/old/RtutoR.Rcheck/RtutoR’

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

Version: 0.1.1

## Newly broken

*   checking examples ... ERROR
    ```
    ...
     3 group_pack..1 vs_one 13.3.3.84      0 F    
     4 group_pack..1 vs_one 14.3.3.57      0 F    
     5 group_pack..1 vs_one 14.7.5.345     0 F    
     6 group_pack..1 vs_one 15.3.57        0 F    
     7 group_pack..1 vs_one 15.2.3.435     0 F    
     8 group_pack..1 vs_one 15.2.3.78      0 F    
     9 group_pack..1 vs_one 15.5.3.52      0 F    
    10 group_pack..1 vs_one 15.8.3.17      0 F    
    # ... with 54 more rows
    > 
    > # Results should have in column 'id' value 1 and not 0.
    > mtcars %>% dplyr::slice(1) %>% expose(row_rule_pack) %>% get_report()
    Tidy data validation report:
    # A tibble: 1 x 5
      pack         rule        var      id value
      <chr>        <chr>       <chr> <int> <lgl>
    1 data_pack..1 neg_row_sum .all      0 F    
    > 
    > mtcars %>% dplyr::slice(1) %>% expose(cell_rule_pack) %>% get_report()
    Error: `UQS()` can only be used within a quasiquoted argument
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 269 SKIPPED: 0 FAILED: 9
      1. Error: expose works (@test-expose.R#162) 
      2. Error: expose preserves pack names (@test-expose.R#218) 
      3. Error: expose accounts for rule separator (@test-expose.R#250) 
      4. Error: expose guesses (@test-expose.R#269) 
      5. Error: expose_single.default guesses col pack (@test-expose.R#309) 
      6. Error: expose_single.default guesses cell pack (@test-expose.R#335) 
      7. Error: expose_single.col_pack works (@test-expose.R#402) 
      8. Error: expose_single.cell_pack works (@test-expose.R#453) 
      9. Error: rules works (@test-rules.R#6) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 117-125 (validation.Rmd) 
    Error: processing vignette 'validation.Rmd' failed with diagnostics:
    `UQS()` can only be used within a quasiquoted argument
    Execution halted
    ```

# seplyr

Version: 0.5.3

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 47-50 (rename_se.Rmd) 
    Error: processing vignette 'rename_se.Rmd' failed with diagnostics:
    All arguments must be named
    Execution halted
    ```

# sf

Version: 0.6-0

## In both

*   checking whether package ‘sf’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/checks.noindex/sf/new/sf.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘sf’ ...
** package ‘sf’ successfully unpacked and MD5 sums checked
configure: CC: clang
configure: CXX: ccache clang++
checking for gdal-config... no
no
configure: error: gdal-config not found or not executable.
ERROR: configuration failed for package ‘sf’
* removing ‘/Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/checks.noindex/sf/new/sf.Rcheck/sf’

```
### CRAN

```
* installing *source* package ‘sf’ ...
** package ‘sf’ successfully unpacked and MD5 sums checked
configure: CC: clang
configure: CXX: ccache clang++
checking for gdal-config... no
no
configure: error: gdal-config not found or not executable.
ERROR: configuration failed for package ‘sf’
* removing ‘/Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/checks.noindex/sf/old/sf.Rcheck/sf’

```
# sjPlot

Version: 2.4.1

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘plm’
    ```

# sjstats

Version: 0.14.1

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘MuMIn’, ‘piecewiseSEM’
    ```

# skimr

Version: 1.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘knitr’
      All declared Imports should be used.
    ```

# stplanr

Version: 0.2.2

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘tmap’
    ```

# taxa

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘knitr’ ‘lazyeval’ ‘rlang’
      All declared Imports should be used.
    ```

# tidyinftheo

Version: 0.2.1

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      13: reduce_data(.data, !(!enquo(X)), numvars = 1, na.rm = na.rm)
      14: tidyselect::vars_select(names(reduced_tab), !(!(!quos(...))))
      15: vars_select_eval(.vars, quos)
      16: map_lgl(quos, quo_is_helper)
      17: .f(.x[[i]], ...)
      18: extract_expr(quo)
      19: is_lang(expr, paren_sym)
      20: is_call(x, name, n, ns)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 37 SKIPPED: 0 FAILED: 1
      1. Error: right number of columns given (@test-errors.R#10) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘tibble’ ‘tidyverse’
      All declared Imports should be used.
    ```

# tidyLPA

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘purrr’
      All declared Imports should be used.
    ```

# tidyr

Version: 0.8.0

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

Version: 0.1.6

## In both

*   checking dependencies in R code ... NOTE
    ```
    Unable to find any JVMs matching version "(null)".
    No Java runtime present, try --request to install.
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

Version: 1.0.0

## In both

*   checking compiled code ... WARNING
    ```
    File ‘tidyxl/libs/tidyxl.so’:
      Found ‘_abort’, possibly from ‘abort’ (C)
        Object: ‘xlex.o’
    
    Compiled code should not call entry points which might terminate R nor
    write to stdout/stderr instead of to the console, nor the system RNG.
    
    See ‘Writing portable packages’ in the ‘Writing R Extensions’ manual.
    ```

# VWPre

Version: 1.0.1

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    There are 50 samples per bin.
    One data point every 50 millisecond(s)
    Preparing ELogit output...
    Number of Observations equal to Number of Samples. 
     Calculation will be based on Number of Samples.
    Grand average of Proportion Looks calculated using Event means.
    Grand average of Proportion Looks calculated using Event means.
    Grand average of Proportion Looks calculated using Event means.
    Grand average of Proportion Looks calculated using Event means.
    Grand average of Proportion Looks calculated using Event means.
    Grand average of Proportion Looks calculated using Event means.
    Grand average of Proportion Looks calculated using Event means.
    Grand average of Proportion Looks calculated using Event means.
    Grand average of Proportion Looks calculated using Event means.
    Grand average of Proportion Looks calculated using Subject means.
    Grand average of Difference calculated using Event means.
    Quitting from lines 204-218 (SR_Plotting.Rmd) 
    Error: processing vignette 'SR_Plotting.Rmd' failed with diagnostics:
    `UQ()` can only be used within a quasiquoted argument
    Execution halted
    ```

