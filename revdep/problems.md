# banR

Version: 0.2.0

## Newly fixed

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 57-66 (geocode.Rmd) 
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

# bomrang

Version: 0.0.8

## In both

*   R CMD check timed out
    

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
      1. Error: condformat2excel generates a file (@test_rendering.R#38) -------------
      Please install the xlsx package in order to export to excel
      1: condformat2excel(condformat(head(iris, n = rows_to_write)), filename = filename) at testthat/test_rendering.R:38
      2: require_xlsx()
      3: stop("Please install the xlsx package in order to export to excel")
      
      testthat results ================================================================
      OK: 115 SKIPPED: 0 FAILED: 1
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

# dbplyr

Version: 1.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tibble’
      All declared Imports should be used.
    ```

# dexter

Version: 0.5.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 109 marked UTF-8 strings
    ```

# dplyr

Version: 0.7.4

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.3Mb
      sub-directories of 1Mb or more:
        libs   4.4Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4 marked UTF-8 strings
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

# GSODR

Version: 1.1.0

## In both

*   R CMD check timed out
    

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘rgeos’
    ```

# implyr

Version: 0.2.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Loading required package: DBI
      Loading required package: rJava
      Unable to find any JVMs matching version "(null)".
      No Java runtime present, try --request to install.
      Error: package or namespace load failed for 'rJava':
       .onLoad failed in loadNamespace() for 'rJava', details:
        call: dyn.load(file, DLLpath = DLLpath, ...)
        error: unable to load shared object '/Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/library/implyr/rJava/libs/rJava.so':
        dlopen(/Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/library/implyr/rJava/libs/rJava.so, 6): Library not loaded: @rpath/libjvm.dylib
        Referenced from: /Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/library/implyr/rJava/libs/rJava.so
        Reason: image not found
      Error: package 'rJava' could not be loaded
      In addition: Warning message:
      running command '/usr/libexec/java_home' had status 1 
      Execution halted
    ```

# kokudosuuchi

Version: 0.4.1

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘sf’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
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

# nonmemica

Version: 0.7.6

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
    
    pandoc: Could not fetch /Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/library/nyctaxi/leaflet/htmlwidgets/lib/leaflet/#default#VML
    /Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/library/nyctaxi/leaflet/htmlwidgets/lib/leaflet/: openBinaryFile: inappropriate type (is a directory)
    Error: processing vignette 'nyc_taxi.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 67
    Execution halted
    ```

# PKPDmisc

Version: 2.0.0

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
    See ‘/Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/checks/pointblank/new/pointblank.Rcheck/00install.out’ for details.
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
  error: unable to load shared object '/Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/library/pointblank/rJava/libs/rJava.so':
  dlopen(/Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/library/pointblank/rJava/libs/rJava.so, 6): Library not loaded: @rpath/libjvm.dylib
  Referenced from: /Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/library/pointblank/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘pointblank’
* removing ‘/Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/checks/pointblank/new/pointblank.Rcheck/pointblank’

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
  error: unable to load shared object '/Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/library/pointblank/rJava/libs/rJava.so':
  dlopen(/Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/library/pointblank/rJava/libs/rJava.so, 6): Library not loaded: @rpath/libjvm.dylib
  Referenced from: /Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/library/pointblank/rJava/libs/rJava.so
  Reason: image not found
ERROR: lazy loading failed for package ‘pointblank’
* removing ‘/Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/checks/pointblank/old/pointblank.Rcheck/pointblank’

```
# poppr

Version: 2.5.0

## In both

*   checking whether package ‘poppr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/checks/poppr/new/poppr.Rcheck/00install.out’ for details.
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
* removing ‘/Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/checks/poppr/new/poppr.Rcheck/poppr’

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
* removing ‘/Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/checks/poppr/old/poppr.Rcheck/poppr’

```
# prisonbrief

Version: 0.1.0

## In both

*   checking whether package ‘prisonbrief’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/checks/prisonbrief/new/prisonbrief.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘prisonbrief’ ...
** package ‘prisonbrief’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** inst
** preparing package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘sf’
ERROR: lazy loading failed for package ‘prisonbrief’
* removing ‘/Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/checks/prisonbrief/new/prisonbrief.Rcheck/prisonbrief’

```
### CRAN

```
* installing *source* package ‘prisonbrief’ ...
** package ‘prisonbrief’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** inst
** preparing package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘sf’
ERROR: lazy loading failed for package ‘prisonbrief’
* removing ‘/Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/checks/prisonbrief/old/prisonbrief.Rcheck/prisonbrief’

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

# seplyr

Version: 0.1.5

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘datasets’
      All declared Imports should be used.
    ```

# sf

Version: 0.5-5

## In both

*   checking whether package ‘sf’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/checks/sf/new/sf.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘rgeos’
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
* removing ‘/Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/checks/sf/new/sf.Rcheck/sf’

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
* removing ‘/Users/lionel/Dropbox/Projects/R/hadley/rlang/revdep/checks/sf/old/sf.Rcheck/sf’

```
# sjPlot

Version: 2.4.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘prediction’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘plm’
    ```

# sjstats

Version: 0.12.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘sjPlot’, ‘MuMIn’, ‘piecewiseSEM’
    ```

# sparklyr

Version: 0.6.3

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(testthat)
      > library(sparklyr)
      > 
      > if (identical(Sys.getenv("NOT_CRAN"), "true")) {
      +   test_check("sparklyr")
      +   on.exit({ spark_disconnect_all() ; livy_service_stop() })
      + }
      Error in validate_java_version_line(master, version) : 
        Java version detected but couldn't parse version from No Java runtime present, requesting install.
      Calls: test_check ... validate_java_version -> validate_java_version_line
      In addition: Warning message:
      running command ''/usr/bin/java' -version 2>&1' had status 1 
      testthat results ================================================================
      OK: 0 SKIPPED: 0 FAILED: 0
      Execution halted
    ```

# stplanr

Version: 0.1.9

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘rgeos’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# tatoo

Version: 1.0.8

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘crayon’ ‘utils’
      All declared Imports should be used.
    ```

# taxa

Version: 0.1.0

## Newly broken

*   checking examples ... ERROR
    ```
    ...
      4 data sets:
        info:
          # A tibble: 4 x 4
              name n_legs dangerous taxon_id
            <fctr>  <dbl>     <lgl>    <chr>
          1    cat      4     FALSE        n
          2   mole      4     FALSE        o
          3 tomato      0     FALSE        q
          # ... with 1 more rows
        phylopic_ids:  e148eabb-f138-43c6-b1e4-5cda2180485a ... 63604565-0406-460b-8cb8-1abe954b3f3a
        foods: a list with 6 items
        And 1 more data sets: abund
      1 functions:
     reaction
    > 
    > # Remove taxa whose obserservation were filtered out
    > filter_obs(ex_taxmap, "info", dangerous == FALSE, drop_taxa = TRUE)
    Error in names(selection) <- self$taxon_ids() : 
      'names' attribute [17] must be the same length as the vector [2]
    Calls: filter_obs ... filter_obs.Taxmap -> <Anonymous> -> <Anonymous> -> <Anonymous>
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      testthat results ================================================================
      OK: 317 SKIPPED: 0 FAILED: 31
      1. Error: NSE values can be found (@test--taxmap.R#214) 
      2. Error: All valid NSE values can be found (@test--taxmap.R#224) 
      3. Error: Mapping between table observations and the edge list works (@test--taxmap.R#233) 
      4. Error: Mapping between a subset of observations and the edge list works (@test--taxmap.R#242) 
      5. Error: Mapping non-recursivly between observations and the edge list works (@test--taxmap.R#247) 
      6. Error: Mapping simplification between observations and the edge list works (@test--taxmap.R#253) 
      7. Error: Mapping observations in external tables (@test--taxmap.R#259) 
      8. Error: Default taxon filtering works (@test--taxmap.R#271) 
      9. Error: Subtaxa can be included when filtering taxa (@test--taxmap.R#279) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 258-259 (taxa-vignette.Rmd) 
    Error: processing vignette 'taxa-vignette.Rmd' failed with diagnostics:
    'names' attribute [14] must be the same length as the vector [2]
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘knitr’ ‘lazyeval’ ‘rlang’
      All declared Imports should be used.
    ```

# tidyr

Version: 0.7.2

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 23 marked UTF-8 strings
    ```

# tidytext

Version: 0.1.4

## In both

*   checking dependencies in R code ... NOTE
    ```
    Unable to find any JVMs matching version "(null)".
    No Java runtime present, try --request to install.
    ```

