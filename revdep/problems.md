# abjutils

Version: 0.2.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘httr’ ‘progress’
      All declared Imports should be used.
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

Version: 0.1.2

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

# cutpointr

Version: 0.7.4

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      `print\(scp\)` does not match "accuracy_oob 0.8201".
      Actual value: "Method: oc_youden_normal \\nPredictor: dsi \\nOutcome: suicide \\nDirection: >= \\nSubgroups: female, male \\nNr\. of bootstraps: 10 \\n\\nSubgroup: female \\n-------------------------------------------------------------------------------- \\n optimal_cutpoint accuracy    acc sensitivity specificity    AUC n_pos n_neg\\n           2\.4778   0\.8954 0\.8954      0\.8148      0\.9014 0\.9446    27   365\\n\\nCutpoint 2\.47775393352595:\\n          observation\\nprediction yes  no\\n       yes  22  36\\n       no    5 329\\n\\n\\nPredictor summary: \\n Min\. 5% 1st Qu\. Median   Mean 3rd Qu\. 95% Max\.     SD\\n    0  0       0      0 0\.8393       1   5   10 1\.7452\\n\\nPredictor summary per class: \\n    Min\.  5% 1st Qu\. Median   Mean 3rd Qu\. 95% Max     SD\\nno     0 0\.0       0      0 0\.5479       0   4  10 1\.3181\\nyes    0 1\.3       4      5 4\.7778       6   7   9 2\.0444\\n\\nBootstrap summary: \\n# A tibble: 13 x 10\\n   Variable       Min\.  `5%` `1st Qu\.` Median  Mean `3rd Qu\.` `95%`  Max\.     SD\\n   <chr>         <dbl> <dbl>     <dbl>  <dbl> <dbl>     <dbl> <dbl> <dbl>  <dbl>\\n 1 optimal_cutp… 2\.18  2\.23      2\.33   2\.43  2\.47      2\.51  2\.83  2\.94  0\.218 \\n 2 AUC_b         0\.941 0\.943     0\.950  0\.964 0\.960     0\.967 0\.974 0\.976 0\.0119\\n 3 AUC_oob       0\.894 0\.894     0\.912  0\.924 0\.925     0\.939 0\.955 0\.956 0\.0222\\n 4 accuracy_b    0\.860 0\.871     0\.888  0\.908 0\.904     0\.923 0\.927 0\.929 0\.0226\\n 5 accuracy_oob  0\.820 0\.838     0\.873  0\.876 0\.880     0\.901 0\.912 0\.914 0\.0278\\n 6 acc_b         0\.860 0\.871     0\.888  0\.908 0\.904     0\.923 0\.927 0\.929 0\.0226\\n 7 acc_oob       0\.820 0\.838     0\.873  0\.876 0\.880     0\.901 0\.912 0\.914 0\.0278\\n 8 sensitivity_b 0\.708 0\.737     0\.779  0\.823 0\.826     0\.851 0\.940 0\.954 0\.0728\\n 9 sensitivity_… 0\.625 0\.644     0\.762  0\.809 0\.800     0\.872 0\.913 0\.923 0\.0971\\n10 specificity_b 0\.870 0\.875     0\.894  0\.915 0\.909     0\.927 0\.931 0\.932 0\.0223\\n11 specificity_… 0\.835 0\.845     0\.876  0\.880 0\.886     0\.912 0\.921 0\.922 0\.0283\\n12 kappa_b       0\.321 0\.329     0\.423  0\.509 0\.485     0\.562 0\.590 0\.610 0\.0995\\n13 kappa_oob     0\.305 0\.324     0\.368  0\.420 0\.444     0\.511 0\.608 0\.631 0\.106 \\n\\nSubgroup: male \\n-------------------------------------------------------------------------------- \\n optimal_cutpoint accuracy    acc sensitivity specificity    AUC n_pos n_neg\\n           3\.1723   0\.8643 0\.8643      0\.6667      0\.8779 0\.8617     9   131\\n\\nCutpoint 3\.17225507835137:\\n          observation\\nprediction yes  no\\n       yes   6  16\\n       no    3 115\\n\\n\\nPredictor summary: \\n Min\. 5% 1st Qu\. Median Mean 3rd Qu\. 95% Max\.     SD\\n    0  0       0      0 1\.15       1   6   11 2\.1151\\n\\nPredictor summary per class: \\n    Min\.  5% 1st Qu\. Median   Mean 3rd Qu\.  95% Max     SD\\nno     0 0\.0       0      0 0\.8702       1  5\.0   6 1\.6286\\nyes    0 0\.4       3      4 5\.2222       8 10\.6  11 3\.8333\\n\\nBootstrap summary: \\n# A tibble: 13 x 10\\n   Variable       Min\.  `5%` `1st Qu\.` Median  Mean `3rd Qu\.` `95%`  Max\.     SD\\n   <chr>         <dbl> <dbl>     <dbl>  <dbl> <dbl>     <dbl> <dbl> <dbl>  <dbl>\\n 1 optimal_cutp… 2\.82  2\.84      2\.92   3\.27  3\.26      3\.55  3\.82  3\.90  0\.387 \\n 2 AUC_b         0\.758 0\.787     0\.825  0\.879 0\.871     0\.904 0\.959 0\.968 0\.0641\\n 3 AUC_oob       0\.631 0\.691     0\.792  0\.885 0\.859     0\.943 0\.972 0\.977 0\.109 \\n 4 accuracy_b    0\.807 0\.814     0\.834  0\.864 0\.852     0\.871 0\.871 0\.871 0\.0243\\n 5 accuracy_oob  0\.822 0\.823     0\.839  0\.871 0\.866     0\.896 0\.905 0\.906 0\.0327\\n 6 acc_b         0\.807 0\.814     0\.834  0\.864 0\.852     0\.871 0\.871 0\.871 0\.0243\\n 7 acc_oob       0\.822 0\.823     0\.839  0\.871 0\.866     0\.896 0\.905 0\.906 0\.0327\\n 8 sensitivity_b 0\.556 0\.582     0\.667  0\.703 0\.735     0\.794 0\.936 1     0\.129 \\n 9 sensitivity_… 0\.333 0\.363     0\.5    0\.667 0\.707     1     1     1     0\.272 \\n10 specificity_b 0\.817 0\.825     0\.846  0\.867 0\.862     0\.875 0\.892 0\.898 0\.0246\\n11 specificity_… 0\.818 0\.826     0\.853  0\.887 0\.877     0\.898 0\.917 0\.918 0\.0342\\n12 kappa_b       0\.210 0\.220     0\.243  0\.338 0\.319     0\.380 0\.407 0\.411 0\.0757\\n13 kappa_oob     0\.118 0\.145     0\.208  0\.306 0\.310     0\.398 0\.497 0\.570 0\.139 "
      
      ── 3. Failure: summary is printed correctly (@test-cutpointr.R#1211)  ──────────
      `print\(scp\)` does not match "accuracy_oob 0.8163".
      Actual value: "Method: oc_youden_normal \\nPredictor: x \\nOutcome: class \\nDirection: >= \\nSubgroups: female, male \\nNr\. of bootstraps: 10 \\n\\nSubgroup: female \\n-------------------------------------------------------------------------------- \\n optimal_cutpoint accuracy    acc sensitivity specificity    AUC n_pos n_neg\\n           2\.4778   0\.8954 0\.8954      0\.8148      0\.9014 0\.9446    27   365\\n\\nCutpoint 2\.47775393352595:\\n          observation\\nprediction yes  no\\n       yes  22  36\\n       no    5 329\\n\\n\\nPredictor summary: \\n Min\. 5% 1st Qu\. Median   Mean 3rd Qu\. 95% Max\.     SD\\n    0  0       0      0 0\.8393       1   5   10 1\.7452\\n\\nPredictor summary per class: \\n    Min\.  5% 1st Qu\. Median   Mean 3rd Qu\. 95% Max     SD\\nno     0 0\.0       0      0 0\.5479       0   4  10 1\.3181\\nyes    0 1\.3       4      5 4\.7778       6   7   9 2\.0444\\n\\nBootstrap summary: \\n# A tibble: 13 x 10\\n   Variable       Min\.  `5%` `1st Qu\.` Median  Mean `3rd Qu\.` `95%`  Max\.     SD\\n   <chr>         <dbl> <dbl>     <dbl>  <dbl> <dbl>     <dbl> <dbl> <dbl>  <dbl>\\n 1 optimal_cutp… 2\.02  2\.12      2\.32   2\.40  2\.40      2\.54  2\.62  2\.66  0\.185 \\n 2 AUC_b         0\.907 0\.910     0\.92   0\.950 0\.940     0\.958 0\.965 0\.966 0\.0227\\n 3 AUC_oob       0\.898 0\.905     0\.931  0\.953 0\.947     0\.968 0\.978 0\.980 0\.0274\\n 4 accuracy_b    0\.878 0\.878     0\.895  0\.902 0\.900     0\.908 0\.916 0\.921 0\.0138\\n 5 accuracy_oob  0\.865 0\.868     0\.879  0\.888 0\.891     0\.906 0\.914 0\.917 0\.0176\\n 6 acc_b         0\.878 0\.878     0\.895  0\.902 0\.900     0\.908 0\.916 0\.921 0\.0138\\n 7 acc_oob       0\.865 0\.868     0\.879  0\.888 0\.891     0\.906 0\.914 0\.917 0\.0176\\n 8 sensitivity_b 0\.66  0\.689     0\.759  0\.786 0\.796     0\.849 0\.896 0\.917 0\.076 \\n 9 sensitivity_… 0\.7   0\.712     0\.8    0\.847 0\.861     0\.972 1     1     0\.112 \\n10 specificity_b 0\.878 0\.881     0\.901  0\.913 0\.910     0\.922 0\.934 0\.939 0\.019 \\n11 specificity_… 0\.864 0\.867     0\.882  0\.892 0\.895     0\.909 0\.925 0\.926 0\.0216\\n12 kappa_b       0\.362 0\.410     0\.475  0\.528 0\.514     0\.566 0\.582 0\.585 0\.0692\\n13 kappa_oob     0\.160 0\.214     0\.391  0\.420 0\.404     0\.475 0\.524 0\.539 0\.112 \\n\\nSubgroup: male \\n-------------------------------------------------------------------------------- \\n optimal_cutpoint accuracy    acc sensitivity specificity    AUC n_pos n_neg\\n           3\.1723   0\.8643 0\.8643      0\.6667      0\.8779 0\.8617     9   131\\n\\nCutpoint 3\.17225507835137:\\n          observation\\nprediction yes  no\\n       yes   6  16\\n       no    3 115\\n\\n\\nPredictor summary: \\n Min\. 5% 1st Qu\. Median Mean 3rd Qu\. 95% Max\.     SD\\n    0  0       0      0 1\.15       1   6   11 2\.1151\\n\\nPredictor summary per class: \\n    Min\.  5% 1st Qu\. Median   Mean 3rd Qu\.  95% Max     SD\\nno     0 0\.0       0      0 0\.8702       1  5\.0   6 1\.6286\\nyes    0 0\.4       3      4 5\.2222       8 10\.6  11 3\.8333\\n\\nBootstrap summary: \\n# A tibble: 13 x 10\\n   Variable       Min\.  `5%` `1st Qu\.` Median  Mean `3rd Qu\.` `95%`  Max\.     SD\\n   <chr>         <dbl> <dbl>     <dbl>  <dbl> <dbl>     <dbl> <dbl> <dbl>  <dbl>\\n 1 optimal_cutp… 2\.14  2\.26      2\.93   3\.05  2\.97      3\.28  3\.35  3\.36  0\.403 \\n 2 AUC_b         0\.738 0\.760     0\.823  0\.848 0\.852     0\.904 0\.925 0\.929 0\.0611\\n 3 AUC_oob       0\.806 0\.815     0\.838  0\.901 0\.899     0\.958 0\.990 1     0\.0688\\n 4 accuracy_b    0\.8   0\.8       0\.848  0\.868 0\.854     0\.871 0\.875 0\.879 0\.0298\\n 5 accuracy_oob  0\.816 0\.820     0\.835  0\.87  0\.862     0\.877 0\.899 0\.917 0\.031 \\n 6 acc_b         0\.8   0\.8       0\.848  0\.868 0\.854     0\.871 0\.875 0\.879 0\.0298\\n 7 acc_oob       0\.816 0\.820     0\.835  0\.87  0\.862     0\.877 0\.899 0\.917 0\.031 \\n 8 sensitivity_b 0\.333 0\.376     0\.542  0\.690 0\.656     0\.744 0\.9   1     0\.192 \\n 9 sensitivity_… 0\.5   0\.545     0\.617  0\.8   0\.777     0\.95  1     1     0\.183 \\n10 specificity_b 0\.806 0\.807     0\.865  0\.876 0\.864     0\.879 0\.894 0\.903 0\.0316\\n11 specificity_… 0\.808 0\.823     0\.852  0\.874 0\.870     0\.886 0\.909 0\.909 0\.031 \\n12 kappa_b       0\.133 0\.135     0\.154  0\.264 0\.264     0\.364 0\.416 0\.436 0\.116 \\n13 kappa_oob     0\.140 0\.192     0\.318  0\.448 0\.405     0\.493 0\.575 0\.625 0\.143 "
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 369 SKIPPED: 0 FAILED: 3
      1. Failure: summary is printed correctly (@test-cutpointr.R#1179) 
      2. Failure: summary is printed correctly (@test-cutpointr.R#1195) 
      3. Failure: summary is printed correctly (@test-cutpointr.R#1211) 
      
      Error: testthat unit tests failed
      Execution halted
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
      Warning: package ‘estimatr’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/DeclareDesign/new/DeclareDesign.Rcheck/00install.out’ for details.
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

Version: 0.3.8

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

*   checking package dependencies ... NOTE
    ```
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

Version: 0.9.0

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

*   checking whether package ‘ggbio’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: subclass "GeneNameFilter" of class "AnnotationFilter" is not local and cannot be updated for new inheritance information; consider setClassUnion()
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/ggbio/new/ggbio.Rcheck/00install.out’ for details.
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
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
    3: package 'GenomeInfoDb' was built under R version 3.5.2 
    4: `panel.margin` is deprecated. Please use `panel.spacing` property instead 
    5: `panel.margin` is deprecated. Please use `panel.spacing` property instead 
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
      Warning: package ‘RcppArmadillo’ was built under R version 3.5.2
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

# ggeffects

Version: 0.8.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘ordinal’
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
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        doc   2.8Mb
        R     2.0Mb
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
        doc   1.8Mb
        R     3.8Mb
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

Version: 0.0.9

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        help   4.1Mb
        R      1.1Mb
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
      Warning: package ‘Matrix’ was built under R version 3.5.2
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

Version: 1.7.0.0

## In both

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
      installed size is  8.8Mb
      sub-directories of 1Mb or more:
        help   1.2Mb
        R      7.1Mb
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

Version: 4.4.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Fontconfig error: "/Users/lionel/Desktop/rlang/revdep/library.noindex/huxtable/magick/etc/fonts/conf.d/90-synthetic.conf", line 6: invalid attribute 'version'
      Fontconfig error: Cannot load default config file
      ── 1. Failure: huxreg copes with different models (@test-huxreg.R#31)  ─────────
      `hr <- huxreg(lm1, lm2, glm1)` produced warnings.
      
      ── 2. Failure: Data written in appropriate format (@test-openxlsx.R#101)  ──────
      `openxlsx::saveWorkbook(wb, file = "test-xlsx.xlsx", overwrite = TRUE)` produced messages.
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 888 SKIPPED: 57 FAILED: 2
      1. Failure: huxreg copes with different models (@test-huxreg.R#31) 
      2. Failure: Data written in appropriate format (@test-openxlsx.R#101) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        doc   2.9Mb
        R     2.1Mb
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

*   checking installed package size ... NOTE
    ```
      installed size is  6.8Mb
      sub-directories of 1Mb or more:
        libs   5.1Mb
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
      installed size is  5.8Mb
      sub-directories of 1Mb or more:
        data   3.5Mb
        help   1.2Mb
        R      1.1Mb
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

# jtools

Version: 2.0.0

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.0Mb
      sub-directories of 1Mb or more:
        doc   2.2Mb
        R     2.1Mb
    ```

## In both

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

# MazamaSpatialUtils

Version: 0.6.1

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        data   4.0Mb
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
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        R   4.0Mb
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
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        data   3.1Mb
    ```

# photobiology

Version: 0.9.26

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.0Mb
      sub-directories of 1Mb or more:
        R   3.1Mb
    ```

## In both

*   checking whether package ‘photobiology’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘tibble’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/photobiology/new/photobiology.Rcheck/00install.out’ for details.
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
      installed size is  7.1Mb
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
        R   7.1Mb
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
      OK: 1125 SKIPPED: 9 FAILED: 1
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

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘rlang’ ‘spatstat’
      All declared Imports should be used.
    ```

# replyr

Version: 0.9.9

## In both

*   checking whether package ‘replyr’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘wrapr’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/replyr/new/replyr.Rcheck/00install.out’ for details.
    ```

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

# seplyr

Version: 0.8.3

## In both

*   checking whether package ‘seplyr’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘wrapr’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/seplyr/new/seplyr.Rcheck/00install.out’ for details.
    ```

# SeqVarTools

Version: 1.20.2

## In both

*   checking whether package ‘SeqVarTools’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘SeqArray’ was built under R version 3.5.2
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/SeqVarTools/new/SeqVarTools.Rcheck/00install.out’ for details.
    ```

# sf

Version: 0.7-3

## In both

*   checking tests ...
    ```
    ...
    201,202d200
    < Warning message:
    < package 'stars' was built under R version 3.5.2 
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Reading layer `nospatial' from data source `/Users/lionel/Desktop/rlang/revdep/checks.noindex/sf/new/sf.Rcheck/sf/gpkg/nospatial.gpkg' using driver `GPKG'
      OGR: Unsupported geometry type
      Reading layer `nc' from data source `/Users/lionel/Desktop/rlang/revdep/checks.noindex/sf/new/sf.Rcheck/sf/shape/nc.shp' using driver `ESRI Shapefile'
      Simple feature collection with 100 features and 14 fields
      geometry type:  MULTIPOLYGON
      dimension:      XY
      bbox:           xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
      epsg (SRID):    4267
      proj4string:    +proj=longlat +datum=NAD27 +no_defs
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 621 SKIPPED: 59 FAILED: 1
      1. Error: gdal_subdatasets works (@test_gdal.R#50) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 17.5Mb
      sub-directories of 1Mb or more:
        doc     11.2Mb
        R        2.0Mb
        sqlite   1.5Mb
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
        R      4.0Mb
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
      installed size is 15.6Mb
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

Version: 0.2.7

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
      installed size is 15.8Mb
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

Version: 0.3.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.3Mb
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

Version: 0.4.3

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      3: compare.tbl_df(act$val, exp$val)
      4: NextMethod("compare") at /private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/RtmpvD5aJZ/R.INSTALL97c27da5a21/readr/R/utils.R:51
      5: compare.default(act$val, exp$val)
      6: all.equal(x, y, ...)
      7: all.equal.tbl_df(x, y, ...)
      8: equal_data_frame(target, current, ignore_col_order = ignore_col_order, ignore_row_order = ignore_row_order, 
             convert = convert)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 522 SKIPPED: 6 FAILED: 2
      1. Failure: dot arguments are properly passed to readr (@test-read_nm_tables.R#57) 
      2. Error: vpc_data works properly with xpdb tables (@test-vpc.R#43) 
      
      Error: testthat unit tests failed
      Execution halted
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

