# anglr

<details>

* Version: 0.7.0
* Source code: https://github.com/cran/anglr
* URL: https://github.com/hypertidy/anglr
* BugReports: https://github.com/hypertidy/anglr/issues
* Date/Publication: 2020-07-29 08:10:03 UTC
* Number of recursive dependencies: 123

Run `cloud_details(, "anglr")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/anglr/new/anglr.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘anglr/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘anglr’ version ‘0.7.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘anglr’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... ERROR
  Running ‘testthat.R’
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
   14. base::asNamespace(ns)
   15. base::getNamespace(ns)
   16. base::loadNamespace(name)
   17. base::withRestarts(stop(cond), retry_loadNamespace = function() NULL)
   18. base:::withOneRestart(expr, restarts[[1L]])
   19. base:::doWithOneRestart(return(expr), restart)
  
  ══ testthat results  ═══════════════════════════════════════════════════════════
  [ OK: 60 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 3 ]
  1. Error: copy down works (@test-copy-down.R#5) 
  2. Error: mesh_plot works (@test-mesh_plot.R#42) 
  3. Error: sanity prevails 4 (@test-sanity.R#27) 
  
  Error: testthat unit tests failed
  Execution halted
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘anglr.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 1 ERROR






```
### CRAN

```
* using log directory ‘/tmp/workdir/anglr/old/anglr.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘anglr/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘anglr’ version ‘0.7.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘anglr’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... ERROR
  Running ‘testthat.R’
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
   14. base::asNamespace(ns)
   15. base::getNamespace(ns)
   16. base::loadNamespace(name)
   17. base::withRestarts(stop(cond), retry_loadNamespace = function() NULL)
   18. base:::withOneRestart(expr, restarts[[1L]])
   19. base:::doWithOneRestart(return(expr), restart)
  
  ══ testthat results  ═══════════════════════════════════════════════════════════
  [ OK: 60 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 3 ]
  1. Error: copy down works (@test-copy-down.R#5) 
  2. Error: mesh_plot works (@test-mesh_plot.R#42) 
  3. Error: sanity prevails 4 (@test-sanity.R#27) 
  
  Error: testthat unit tests failed
  Execution halted
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘anglr.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 1 ERROR






```
# butcher

<details>

* Version: 0.1.2
* Source code: https://github.com/cran/butcher
* URL: https://tidymodels.github.io/butcher, https://github.com/tidymodels/butcher
* BugReports: https://github.com/tidymodels/butcher/issues
* Date/Publication: 2020-01-23 22:40:02 UTC
* Number of recursive dependencies: 184

Run `cloud_details(, "butcher")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/butcher/new/butcher.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘butcher/DESCRIPTION’ ... OK
* this is package ‘butcher’ version ‘0.1.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package suggested but not available: ‘NMF’

The suggested packages are required for a complete check.
Checking can be attempted without them by setting the environment
variable _R_CHECK_FORCE_SUGGESTS_ to a false value.

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR






```
### CRAN

```
* using log directory ‘/tmp/workdir/butcher/old/butcher.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘butcher/DESCRIPTION’ ... OK
* this is package ‘butcher’ version ‘0.1.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package suggested but not available: ‘NMF’

The suggested packages are required for a complete check.
Checking can be attempted without them by setting the environment
variable _R_CHECK_FORCE_SUGGESTS_ to a false value.

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR






```
# ceramic

<details>

* Version: 0.6.0
* Source code: https://github.com/cran/ceramic
* URL: https://github.com/hypertidy/ceramic
* BugReports: https://github.com/hypertidy/ceramic/issues
* Date/Publication: 2019-07-20 09:00:02 UTC
* Number of recursive dependencies: 95

Run `cloud_details(, "ceramic")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/ceramic/new/ceramic.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘ceramic/DESCRIPTION’ ... OK
* this is package ‘ceramic’ version ‘0.6.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘ceramic’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking R/sysdata.rda ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘spelling.R’
  Running ‘testthat.R’
* DONE
Status: OK






```
### CRAN

```
* using log directory ‘/tmp/workdir/ceramic/old/ceramic.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘ceramic/DESCRIPTION’ ... OK
* this is package ‘ceramic’ version ‘0.6.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘ceramic’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking R/sysdata.rda ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘spelling.R’
  Running ‘testthat.R’
* DONE
Status: OK






```
# clustree

<details>

* Version: 0.4.3
* Source code: https://github.com/cran/clustree
* URL: https://github.com/lazappi/clustree
* BugReports: https://github.com/lazappi/clustree/issues
* Date/Publication: 2020-06-14 16:40:02 UTC
* Number of recursive dependencies: 176

Run `cloud_details(, "clustree")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/clustree/new/clustree.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘clustree/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘clustree’ version ‘0.4.3’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages suggested but not available:
  'SingleCellExperiment', 'SummarizedExperiment'

The suggested packages are required for a complete check.
Checking can be attempted without them by setting the environment
variable _R_CHECK_FORCE_SUGGESTS_ to a false value.

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR






```
### CRAN

```
* using log directory ‘/tmp/workdir/clustree/old/clustree.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘clustree/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘clustree’ version ‘0.4.3’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages suggested but not available:
  'SingleCellExperiment', 'SummarizedExperiment'

The suggested packages are required for a complete check.
Checking can be attempted without them by setting the environment
variable _R_CHECK_FORCE_SUGGESTS_ to a false value.

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR






```
# conflicted

<details>

* Version: 1.0.4
* Source code: https://github.com/cran/conflicted
* URL: https://github.com/r-lib/conflicted
* BugReports: https://github.com/r-lib/conflicted/issues
* Date/Publication: 2019-06-21 22:40:03 UTC
* Number of recursive dependencies: 67

Run `cloud_details(, "conflicted")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/conflicted/new/conflicted.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘conflicted/DESCRIPTION’ ... OK
* this is package ‘conflicted’ version ‘1.0.4’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘conflicted’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... NOTE
Namespace in Imports field not imported from: ‘memoise’
  All declared Imports should be used.
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* DONE
Status: 1 NOTE






```
### CRAN

```
* using log directory ‘/tmp/workdir/conflicted/old/conflicted.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘conflicted/DESCRIPTION’ ... OK
* this is package ‘conflicted’ version ‘1.0.4’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘conflicted’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... NOTE
Namespace in Imports field not imported from: ‘memoise’
  All declared Imports should be used.
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* DONE
Status: 1 NOTE






```
# cowplot

<details>

* Version: 1.1.0
* Source code: https://github.com/cran/cowplot
* URL: https://wilkelab.org/cowplot/
* BugReports: https://github.com/wilkelab/cowplot/issues
* Date/Publication: 2020-09-08 07:30:10 UTC
* Number of recursive dependencies: 127

Run `cloud_details(, "cowplot")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/cowplot/new/cowplot.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘cowplot/DESCRIPTION’ ... OK
* this is package ‘cowplot’ version ‘1.1.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package suggested but not available: ‘ragg’

The suggested packages are required for a complete check.
Checking can be attempted without them by setting the environment
variable _R_CHECK_FORCE_SUGGESTS_ to a false value.

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR






```
### CRAN

```
* using log directory ‘/tmp/workdir/cowplot/old/cowplot.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘cowplot/DESCRIPTION’ ... OK
* this is package ‘cowplot’ version ‘1.1.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package suggested but not available: ‘ragg’

The suggested packages are required for a complete check.
Checking can be attempted without them by setting the environment
variable _R_CHECK_FORCE_SUGGESTS_ to a false value.

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR






```
# crossmap

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/crossmap
* URL: https://crossmap.rossellhayes.com, https://github.com/rossellhayes/crossmap
* BugReports: https://github.com/rossellhayes/crossmap/issues
* Date/Publication: 2020-09-24 07:30:02 UTC
* Number of recursive dependencies: 55

Run `cloud_details(, "crossmap")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```
* using log directory ‘/tmp/workdir/crossmap/old/crossmap.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘crossmap/DESCRIPTION’ ... OK
* this is package ‘crossmap’ version ‘0.2.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘crossmap’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ...





```
# ctDNAtools

<details>

* Version: 0.4.0
* Source code: https://github.com/cran/ctDNAtools
* URL: https://github.com/alkodsi/ctDNAtools
* BugReports: https://github.com/alkodsi/ctDNAtools/issues
* Date/Publication: 2020-03-04 18:10:02 UTC
* Number of recursive dependencies: 133

Run `cloud_details(, "ctDNAtools")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/ctDNAtools/new/ctDNAtools.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘ctDNAtools/DESCRIPTION’ ... OK
* this is package ‘ctDNAtools’ version ‘0.4.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available:
  'Rsamtools', 'GenomicRanges', 'IRanges', 'GenomeInfoDb',
  'BiocGenerics', 'BSgenome', 'GenomicAlignments', 'Biostrings',
  'VariantAnnotation'

Package suggested but not available: ‘BSgenome.Hsapiens.UCSC.hg19’

The suggested packages are required for a complete check.
Checking can be attempted without them by setting the environment
variable _R_CHECK_FORCE_SUGGESTS_ to a false value.

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR






```
### CRAN

```
* using log directory ‘/tmp/workdir/ctDNAtools/old/ctDNAtools.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘ctDNAtools/DESCRIPTION’ ... OK
* this is package ‘ctDNAtools’ version ‘0.4.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available:
  'Rsamtools', 'GenomicRanges', 'IRanges', 'GenomeInfoDb',
  'BiocGenerics', 'BSgenome', 'GenomicAlignments', 'Biostrings',
  'VariantAnnotation'

Package suggested but not available: ‘BSgenome.Hsapiens.UCSC.hg19’

The suggested packages are required for a complete check.
Checking can be attempted without them by setting the environment
variable _R_CHECK_FORCE_SUGGESTS_ to a false value.

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR






```
# devtools

<details>

* Version: 2.3.2
* Source code: https://github.com/cran/devtools
* URL: https://devtools.r-lib.org/, https://github.com/r-lib/devtools
* BugReports: https://github.com/r-lib/devtools/issues
* Date/Publication: 2020-09-18 07:20:11 UTC
* Number of recursive dependencies: 106

Run `cloud_details(, "devtools")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/devtools/new/devtools.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘devtools/DESCRIPTION’ ... OK
* this is package ‘devtools’ version ‘2.3.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘devtools’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... ERROR
  Running ‘spelling.R’
  Running ‘testthat.R’
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  callr subprocess failed: Vignette re-building failed.
  Backtrace:
   1. devtools::build_vignettes(pkg)
   2. callr::r(...)
   3. callr:::get_result(output = out, options)
   4. throw(newerr, parent = remerr[[2]])
  
  ══ testthat results  ═══════════════════════════════════════════════════════════
  [ OK: 54 | SKIPPED: 10 | WARNINGS: 0 | FAILED: 3 ]
  1. Error: Sweave vignettes copied into doc (@test-vignettes.R#13) 
  2. Error: Built files are updated (@test-vignettes.R#30) 
  3. Error: Extra files copied and removed (@test-vignettes.R#84) 
  
  Error: testthat unit tests failed
  Execution halted
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘dependencies.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 1 ERROR






```
### CRAN

```
* using log directory ‘/tmp/workdir/devtools/old/devtools.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘devtools/DESCRIPTION’ ... OK
* this is package ‘devtools’ version ‘2.3.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘devtools’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... ERROR
  Running ‘spelling.R’
  Running ‘testthat.R’
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  callr subprocess failed: Vignette re-building failed.
  Backtrace:
   1. devtools::build_vignettes(pkg)
   2. callr::r(...)
   3. callr:::get_result(output = out, options)
   4. throw(newerr, parent = remerr[[2]])
  
  ══ testthat results  ═══════════════════════════════════════════════════════════
  [ OK: 54 | SKIPPED: 10 | WARNINGS: 0 | FAILED: 3 ]
  1. Error: Sweave vignettes copied into doc (@test-vignettes.R#13) 
  2. Error: Built files are updated (@test-vignettes.R#30) 
  3. Error: Extra files copied and removed (@test-vignettes.R#84) 
  
  Error: testthat unit tests failed
  Execution halted
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘dependencies.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 1 ERROR






```
# dmdScheme

<details>

* Version: 1.2.6
* Source code: https://github.com/cran/dmdScheme
* URL: https://exp-micro-ecol-hub.github.io/dmdScheme/, https://github.com/Exp-Micro-Ecol-Hub/dmdScheme/
* BugReports: https://github.com/Exp-Micro-Ecol-Hub/dmdScheme/issues/
* Date/Publication: 2020-09-16 08:10:08 UTC
* Number of recursive dependencies: 93

Run `cloud_details(, "dmdScheme")` for more info

</details>

## In both

*   checking whether package ‘dmdScheme’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/dmdScheme/new/dmdScheme.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘dmdScheme’ ...
** package ‘dmdScheme’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/opt/R/4.0.2/lib/R/library/jqr/libs/jqr.so':
  libjq.so.1: cannot open shared object file: No such file or directory
Calls: <Anonymous> ... asNamespace -> loadNamespace -> library.dynam -> dyn.load
Execution halted
ERROR: lazy loading failed for package ‘dmdScheme’
* removing ‘/tmp/workdir/dmdScheme/new/dmdScheme.Rcheck/dmdScheme’

```
### CRAN

```
* installing *source* package ‘dmdScheme’ ...
** package ‘dmdScheme’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/opt/R/4.0.2/lib/R/library/jqr/libs/jqr.so':
  libjq.so.1: cannot open shared object file: No such file or directory
Calls: <Anonymous> ... asNamespace -> loadNamespace -> library.dynam -> dyn.load
Execution halted
ERROR: lazy loading failed for package ‘dmdScheme’
* removing ‘/tmp/workdir/dmdScheme/old/dmdScheme.Rcheck/dmdScheme’

```
# DSAIRM

<details>

* Version: 0.8.6
* Source code: https://github.com/cran/DSAIRM
* URL: https://ahgroup.github.io/DSAIRM, https://github.com/ahgroup/DSAIRM/
* BugReports: https://github.com/ahgroup/DSAIRM/issues
* Date/Publication: 2020-07-14 11:50:03 UTC
* Number of recursive dependencies: 119

Run `cloud_details(, "DSAIRM")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/DSAIRM/new/DSAIRM.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘DSAIRM/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘DSAIRM’ version ‘0.8.6’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘DSAIRM’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... NOTE
Namespace in Imports field not imported from: ‘tidyr’
  All declared Imports should be used.
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘DSAIRM.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 1 NOTE






```
### CRAN

```
* using log directory ‘/tmp/workdir/DSAIRM/old/DSAIRM.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘DSAIRM/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘DSAIRM’ version ‘0.8.6’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘DSAIRM’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... NOTE
Namespace in Imports field not imported from: ‘tidyr’
  All declared Imports should be used.
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘DSAIRM.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 1 NOTE






```
# fingertipscharts

<details>

* Version: 0.0.11
* Source code: https://github.com/cran/fingertipscharts
* BugReports: https://github.com/PublicHealthEngland/fingertipscharts/issues
* Date/Publication: 2020-06-05 08:30:02 UTC
* Number of recursive dependencies: 139

Run `cloud_details(, "fingertipscharts")` for more info

</details>

## In both

*   checking whether package ‘fingertipscharts’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/fingertipscharts/new/fingertipscharts.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘fingertipscharts’ ...
** package ‘fingertipscharts’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/opt/R/4.0.2/lib/R/library/jqr/libs/jqr.so':
  libjq.so.1: cannot open shared object file: No such file or directory
Calls: <Anonymous> ... asNamespace -> loadNamespace -> library.dynam -> dyn.load
Execution halted
ERROR: lazy loading failed for package ‘fingertipscharts’
* removing ‘/tmp/workdir/fingertipscharts/new/fingertipscharts.Rcheck/fingertipscharts’

```
### CRAN

```
* installing *source* package ‘fingertipscharts’ ...
** package ‘fingertipscharts’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/opt/R/4.0.2/lib/R/library/jqr/libs/jqr.so':
  libjq.so.1: cannot open shared object file: No such file or directory
Calls: <Anonymous> ... asNamespace -> loadNamespace -> library.dynam -> dyn.load
Execution halted
ERROR: lazy loading failed for package ‘fingertipscharts’
* removing ‘/tmp/workdir/fingertipscharts/old/fingertipscharts.Rcheck/fingertipscharts’

```
# fishualize

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/fishualize
* URL: https://github.com/nschiett/fishualize
* BugReports: https://github.com/nschiett/fishualize/issues
* Date/Publication: 2020-04-20 19:10:10 UTC
* Number of recursive dependencies: 101

Run `cloud_details(, "fishualize")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/fishualize/new/fishualize.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘fishualize/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘fishualize’ version ‘0.2.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘fishualize’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘contribute.Rmd’ using ‘UTF-8’... OK
  ‘overview_colors.Rmd’ using ‘UTF-8’... OK
  ‘overview_shapes.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: OK






```
### CRAN

```
* using log directory ‘/tmp/workdir/fishualize/old/fishualize.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘fishualize/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘fishualize’ version ‘0.2.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘fishualize’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘contribute.Rmd’ using ‘UTF-8’... OK
  ‘overview_colors.Rmd’ using ‘UTF-8’... OK
  ‘overview_shapes.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: OK






```
# getTBinR

<details>

* Version: 0.7.1
* Source code: https://github.com/cran/getTBinR
* URL: https://www.samabbott.co.uk/getTBinR, https://github.com/seabbs/getTBinR
* BugReports: https://github.com/seabbs/getTBinR/issues
* Date/Publication: 2020-06-11 05:00:02 UTC
* Number of recursive dependencies: 139

Run `cloud_details(, "getTBinR")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/getTBinR/new/getTBinR.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘getTBinR/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘getTBinR’ version ‘0.7.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘getTBinR’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘spelling.R’
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘case_fatality_rate.Rmd’ using ‘UTF-8’... OK
  ‘case_study_global_trends.Rmd’ using ‘UTF-8’... OK
  ‘intro.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: OK






```
### CRAN

```
* using log directory ‘/tmp/workdir/getTBinR/old/getTBinR.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘getTBinR/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘getTBinR’ version ‘0.7.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘getTBinR’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘spelling.R’
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘case_fatality_rate.Rmd’ using ‘UTF-8’... OK
  ‘case_study_global_trends.Rmd’ using ‘UTF-8’... OK
  ‘intro.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: OK






```
# ggdist

<details>

* Version: 2.2.0
* Source code: https://github.com/cran/ggdist
* URL: http://mjskay.github.io/ggdist, https://github.com/mjskay/ggdist
* BugReports: https://github.com/mjskay/ggdist/issues/new
* Date/Publication: 2020-07-12 05:30:02 UTC
* Number of recursive dependencies: 125

Run `cloud_details(, "ggdist")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/ggdist/new/ggdist.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘ggdist/DESCRIPTION’ ... OK
* this is package ‘ggdist’ version ‘2.2.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘ggdist’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘freq-uncertainty-vis.Rmd’ using ‘UTF-8’... OK
  ‘slabinterval.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: OK






```
### CRAN

```
* using log directory ‘/tmp/workdir/ggdist/old/ggdist.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘ggdist/DESCRIPTION’ ... OK
* this is package ‘ggdist’ version ‘2.2.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘ggdist’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘freq-uncertainty-vis.Rmd’ using ‘UTF-8’... OK
  ‘slabinterval.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: OK






```
# golem

<details>

* Version: 0.2.1
* Source code: https://github.com/cran/golem
* URL: https://github.com/ThinkR-open/golem
* BugReports: https://github.com/ThinkR-open/golem/issues
* Date/Publication: 2020-03-05 15:00:02 UTC
* Number of recursive dependencies: 98

Run `cloud_details(, "golem")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/golem/new/golem.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘golem/DESCRIPTION’ ... OK
* this is package ‘golem’ version ‘0.2.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘golem’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘manualtests.R’
  Running ‘spelling.R’
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘a_start.Rmd’ using ‘UTF-8’... OK
  ‘b_dev.Rmd’ using ‘UTF-8’... OK
  ‘c_deploy.Rmd’ using ‘UTF-8’... OK
  ‘d_js.Rmd’ using ‘UTF-8’... OK
  ‘e_config.Rmd’ using ‘UTF-8’... OK
  ‘z_golem_cheatsheet.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: OK






```
### CRAN

```
* using log directory ‘/tmp/workdir/golem/old/golem.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘golem/DESCRIPTION’ ... OK
* this is package ‘golem’ version ‘0.2.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘golem’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘manualtests.R’
  Running ‘spelling.R’
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘a_start.Rmd’ using ‘UTF-8’... OK
  ‘b_dev.Rmd’ using ‘UTF-8’... OK
  ‘c_deploy.Rmd’ using ‘UTF-8’... OK
  ‘d_js.Rmd’ using ‘UTF-8’... OK
  ‘e_config.Rmd’ using ‘UTF-8’... OK
  ‘z_golem_cheatsheet.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: OK






```
# gtsummary

<details>

* Version: 1.3.5
* Source code: https://github.com/cran/gtsummary
* URL: https://github.com/ddsjoberg/gtsummary, http://www.danieldsjoberg.com/gtsummary/
* BugReports: https://github.com/ddsjoberg/gtsummary/issues
* Date/Publication: 2020-09-29 21:00:02 UTC
* Number of recursive dependencies: 165

Run `cloud_details(, "gtsummary")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/gtsummary/new/gtsummary.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘gtsummary/DESCRIPTION’ ... OK
* this is package ‘gtsummary’ version ‘1.3.5’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘gtsummary’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking R/sysdata.rda ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘spelling.R’
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘gallery.Rmd’ using ‘UTF-8’... OK
  ‘global_options.Rmd’ using ‘UTF-8’... OK
  ‘gtsummary_definition.Rmd’ using ‘UTF-8’... OK
  ‘inline_text.Rmd’ using ‘UTF-8’... OK
  ‘rmarkdown.Rmd’ using ‘UTF-8’... OK
  ‘tbl_regression.Rmd’ using ‘UTF-8’... OK
  ‘tbl_summary.Rmd’ using ‘UTF-8’... OK
  ‘themes.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: OK






```
### CRAN

```
* using log directory ‘/tmp/workdir/gtsummary/old/gtsummary.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘gtsummary/DESCRIPTION’ ... OK
* this is package ‘gtsummary’ version ‘1.3.5’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘gtsummary’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking R/sysdata.rda ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘spelling.R’
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘gallery.Rmd’ using ‘UTF-8’... OK
  ‘global_options.Rmd’ using ‘UTF-8’... OK
  ‘gtsummary_definition.Rmd’ using ‘UTF-8’... OK
  ‘inline_text.Rmd’ using ‘UTF-8’... OK
  ‘rmarkdown.Rmd’ using ‘UTF-8’... OK
  ‘tbl_regression.Rmd’ using ‘UTF-8’... OK
  ‘tbl_summary.Rmd’ using ‘UTF-8’... OK
  ‘themes.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: OK






```
# htmltools

<details>

* Version: 0.5.0
* Source code: https://github.com/cran/htmltools
* URL: https://github.com/rstudio/htmltools
* BugReports: https://github.com/rstudio/htmltools/issues
* Date/Publication: 2020-06-16 14:10:10 UTC
* Number of recursive dependencies: 33

Run `cloud_details(, "htmltools")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/htmltools/new/htmltools.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘htmltools/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘htmltools’ version ‘0.5.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package suggested but not available: ‘ragg’

Package which this enhances but not available for checking: ‘knitr’

The suggested packages are required for a complete check.
Checking can be attempted without them by setting the environment
variable _R_CHECK_FORCE_SUGGESTS_ to a false value.

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR






```
### CRAN

```
* using log directory ‘/tmp/workdir/htmltools/old/htmltools.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘htmltools/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘htmltools’ version ‘0.5.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package suggested but not available: ‘ragg’

Package which this enhances but not available for checking: ‘knitr’

The suggested packages are required for a complete check.
Checking can be attempted without them by setting the environment
variable _R_CHECK_FORCE_SUGGESTS_ to a false value.

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR






```
# immunarch

<details>

* Version: 0.6.5
* Source code: https://github.com/cran/immunarch
* URL: https://immunarch.com/, https://github.com/immunomind/immunarch
* BugReports: https://github.com/immunomind/immunarch/issues
* Date/Publication: 2020-06-14 17:40:06 UTC
* Number of recursive dependencies: 172

Run `cloud_details(, "immunarch")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/immunarch/new/immunarch.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘immunarch/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘immunarch’ version ‘0.6.5’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘immunarch’ can be installed ... OK
* checking installed package size ... NOTE
  installed size is  8.2Mb
  sub-directories of 1Mb or more:
    data   4.3Mb
    doc    1.6Mb
* checking package directory ... OK
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... NOTE
Namespace in Imports field not imported from: ‘dbplyr’
  All declared Imports should be used.
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking line endings in C/C++/Fortran sources/headers ... OK
* checking compiled code ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘v1_introduction.Rmd’ using ‘UTF-8’... OK
  ‘v2_data.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 2 NOTEs






```
### CRAN

```
* using log directory ‘/tmp/workdir/immunarch/old/immunarch.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘immunarch/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘immunarch’ version ‘0.6.5’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘immunarch’ can be installed ... OK
* checking installed package size ... NOTE
  installed size is  8.2Mb
  sub-directories of 1Mb or more:
    data   4.3Mb
    doc    1.6Mb
* checking package directory ... OK
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... NOTE
Namespace in Imports field not imported from: ‘dbplyr’
  All declared Imports should be used.
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking line endings in C/C++/Fortran sources/headers ... OK
* checking compiled code ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘v1_introduction.Rmd’ using ‘UTF-8’... OK
  ‘v2_data.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 2 NOTEs






```
# jstor

<details>

* Version: 0.3.9
* Source code: https://github.com/cran/jstor
* URL: https://github.com/ropensci/jstor, https://docs.ropensci.org/jstor
* BugReports: https://github.com/ropensci/jstor/issues
* Date/Publication: 2020-06-04 04:50:03 UTC
* Number of recursive dependencies: 69

Run `cloud_details(, "jstor")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```
* using log directory ‘/tmp/workdir/jstor/old/jstor.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘jstor/DESCRIPTION’ ... OK
* this is package ‘jstor’ version ‘0.3.9’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘jstor’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking R/sysdata.rda ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ...





```
# lobstr

<details>

* Version: 1.1.1
* Source code: https://github.com/cran/lobstr
* URL: https://github.com/r-lib/lobstr
* BugReports: https://github.com/r-lib/lobstr/issues
* Date/Publication: 2019-07-02 23:10:04 UTC
* Number of recursive dependencies: 65

Run `cloud_details(, "lobstr")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/lobstr/new/lobstr.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘lobstr/DESCRIPTION’ ... OK
* this is package ‘lobstr’ version ‘1.1.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘lobstr’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking line endings in C/C++/Fortran sources/headers ... OK
* checking compiled code ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* DONE
Status: OK






```
### CRAN

```
* using log directory ‘/tmp/workdir/lobstr/old/lobstr.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘lobstr/DESCRIPTION’ ... OK
* this is package ‘lobstr’ version ‘1.1.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘lobstr’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking line endings in C/C++/Fortran sources/headers ... OK
* checking compiled code ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* DONE
Status: OK






```
# metagam

<details>

* Version: 0.1.2
* Source code: https://github.com/cran/metagam
* URL: https://lifebrain.github.io/metagam/, https://github.com/Lifebrain/metagam
* BugReports: https://github.com/Lifebrain/metagam/issues
* Date/Publication: 2020-06-22 18:30:03 UTC
* Number of recursive dependencies: 144

Run `cloud_details(, "metagam")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/metagam/new/metagam.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘metagam/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘metagam’ version ‘0.1.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘metap’

Package suggested but not available: ‘multtest’

The suggested packages are required for a complete check.
Checking can be attempted without them by setting the environment
variable _R_CHECK_FORCE_SUGGESTS_ to a false value.

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR






```
### CRAN

```
* using log directory ‘/tmp/workdir/metagam/old/metagam.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘metagam/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘metagam’ version ‘0.1.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘metap’

Package suggested but not available: ‘multtest’

The suggested packages are required for a complete check.
Checking can be attempted without them by setting the environment
variable _R_CHECK_FORCE_SUGGESTS_ to a false value.

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR






```
# pkgdown

<details>

* Version: 1.6.1
* Source code: https://github.com/cran/pkgdown
* URL: https://pkgdown.r-lib.org, https://github.com/r-lib/pkgdown
* BugReports: https://github.com/r-lib/pkgdown/issues
* Date/Publication: 2020-09-12 05:50:26 UTC
* Number of recursive dependencies: 91

Run `cloud_details(, "pkgdown")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/pkgdown/new/pkgdown.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘pkgdown/DESCRIPTION’ ... OK
* this is package ‘pkgdown’ version ‘1.6.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘ragg’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR






```
### CRAN

```
* using log directory ‘/tmp/workdir/pkgdown/old/pkgdown.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘pkgdown/DESCRIPTION’ ... OK
* this is package ‘pkgdown’ version ‘1.6.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘ragg’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR






```
# portalr

<details>

* Version: 0.3.5
* Source code: https://github.com/cran/portalr
* URL: https://weecology.github.io/portalr/, https://github.com/weecology/portalr
* BugReports: https://github.com/weecology/portalr/issues
* Date/Publication: 2020-08-28 11:00:07 UTC
* Number of recursive dependencies: 107

Run `cloud_details(, "portalr")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/portalr/new/portalr.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘portalr/DESCRIPTION’ ... OK
* this is package ‘portalr’ version ‘0.3.5’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘portalr’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘portal_researcher_examples.Rmd’ using ‘UTF-8’... OK
  ‘rodent-abundance-demo.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: OK






```
### CRAN

```
* using log directory ‘/tmp/workdir/portalr/old/portalr.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘portalr/DESCRIPTION’ ... OK
* this is package ‘portalr’ version ‘0.3.5’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘portalr’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘portal_researcher_examples.Rmd’ using ‘UTF-8’... OK
  ‘rodent-abundance-demo.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: OK






```
# radiant.data

<details>

* Version: 1.3.10
* Source code: https://github.com/cran/radiant.data
* URL: https://github.com/radiant-rstats/radiant.data/, https://radiant-rstats.github.io/radiant.data/, https://radiant-rstats.github.io/docs/
* BugReports: https://github.com/radiant-rstats/radiant.data/issues
* Date/Publication: 2020-08-07 05:20:12 UTC
* Number of recursive dependencies: 151

Run `cloud_details(, "radiant.data")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/radiant.data/new/radiant.data.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘radiant.data/DESCRIPTION’ ... OK
* this is package ‘radiant.data’ version ‘1.3.10’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘radiant.data’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* DONE
Status: OK






```
### CRAN

```
* using log directory ‘/tmp/workdir/radiant.data/old/radiant.data.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘radiant.data/DESCRIPTION’ ... OK
* this is package ‘radiant.data’ version ‘1.3.10’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘radiant.data’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* DONE
Status: OK






```
# radiant.model

<details>

* Version: 1.3.14
* Source code: https://github.com/cran/radiant.model
* URL: https://github.com/radiant-rstats/radiant.model/, https://radiant-rstats.github.io/radiant.model/, https://radiant-rstats.github.io/docs/
* BugReports: https://github.com/radiant-rstats/radiant.model/issues/
* Date/Publication: 2020-08-26 22:50:03 UTC
* Number of recursive dependencies: 164

Run `cloud_details(, "radiant.model")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/radiant.model/new/radiant.model.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘radiant.model/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘radiant.model’ version ‘1.3.14’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘radiant.model’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* DONE
Status: OK






```
### CRAN

```
* using log directory ‘/tmp/workdir/radiant.model/old/radiant.model.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘radiant.model/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘radiant.model’ version ‘1.3.14’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘radiant.model’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* DONE
Status: OK






```
# radiant.multivariate

<details>

* Version: 1.3.5
* Source code: https://github.com/cran/radiant.multivariate
* URL: https://github.com/radiant-rstats/radiant.multivariate, https://radiant-rstats.github.io/radiant.multivariate, https://radiant-rstats.github.io/docs
* BugReports: https://github.com/radiant-rstats/radiant.multivariate/issues
* Date/Publication: 2020-03-25 07:40:02 UTC
* Number of recursive dependencies: 168

Run `cloud_details(, "radiant.multivariate")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/radiant.multivariate/new/radiant.multivariate.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘radiant.multivariate/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘radiant.multivariate’ version ‘1.3.5’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘radiant.multivariate’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* DONE
Status: OK






```
### CRAN

```
* using log directory ‘/tmp/workdir/radiant.multivariate/old/radiant.multivariate.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘radiant.multivariate/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘radiant.multivariate’ version ‘1.3.5’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘radiant.multivariate’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* DONE
Status: OK






```
# rbgm

<details>

* Version: 0.1.0
* Source code: https://github.com/cran/rbgm
* URL: https://research.csiro.au/atlantis/
* BugReports: https://github.com/AustralianAntarcticDivision/rbgm/issues/
* Date/Publication: 2020-04-12 05:30:04 UTC
* Number of recursive dependencies: 71

Run `cloud_details(, "rbgm")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/rbgm/new/rbgm.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘rbgm/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘rbgm’ version ‘0.1.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘rbgm’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘BGM_Spatial.Rmd’ using ‘UTF-8’... OK
  ‘BGM_examplefiles.Rmd’ using ‘UTF-8’... OK
  ‘BGMfiles.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: OK






```
### CRAN

```
* using log directory ‘/tmp/workdir/rbgm/old/rbgm.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘rbgm/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘rbgm’ version ‘0.1.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘rbgm’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘BGM_Spatial.Rmd’ using ‘UTF-8’... OK
  ‘BGM_examplefiles.Rmd’ using ‘UTF-8’... OK
  ‘BGMfiles.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: OK






```
# RFishBC

<details>

* Version: 0.2.3
* Source code: https://github.com/cran/RFishBC
* URL: http://derekogle.com/RFishBC
* BugReports: https://github.com/droglenc/RFishBC/issues
* Date/Publication: 2019-12-12 05:30:02 UTC
* Number of recursive dependencies: 122

Run `cloud_details(, "RFishBC")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/RFishBC/new/RFishBC.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘RFishBC/DESCRIPTION’ ... OK
* this is package ‘RFishBC’ version ‘0.2.3’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘RFishBC’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘test-all.R’
* DONE
Status: OK






```
### CRAN

```
* using log directory ‘/tmp/workdir/RFishBC/old/RFishBC.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘RFishBC/DESCRIPTION’ ... OK
* this is package ‘RFishBC’ version ‘0.2.3’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘RFishBC’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘test-all.R’
* DONE
Status: OK






```
# rmdpartials

<details>

* Version: 0.5.8
* Source code: https://github.com/cran/rmdpartials
* URL: https://github.com/rubenarslan/rmdpartials
* BugReports: https://github.com/rubenarslan/rmdpartials/issues
* Date/Publication: 2020-06-04 14:10:13 UTC
* Number of recursive dependencies: 94

Run `cloud_details(, "rmdpartials")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/rmdpartials/new/rmdpartials.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘rmdpartials/DESCRIPTION’ ... OK
* this is package ‘rmdpartials’ version ‘0.5.8’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘rmdpartials’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘rmdpartials.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: OK






```
### CRAN

```
* using log directory ‘/tmp/workdir/rmdpartials/old/rmdpartials.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘rmdpartials/DESCRIPTION’ ... OK
* this is package ‘rmdpartials’ version ‘0.5.8’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘rmdpartials’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘rmdpartials.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: OK






```
# sccore

<details>

* Version: 0.1.0
* Source code: https://github.com/cran/sccore
* URL: https://github.com/kharchenkolab/sccore
* BugReports: https://github.com/kharchenkolab/sccore/issues
* Date/Publication: 2020-09-30 10:00:02 UTC
* Number of recursive dependencies: 77

Run `cloud_details(, "sccore")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/sccore/new/sccore.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘sccore/DESCRIPTION’ ... OK
* this is package ‘sccore’ version ‘0.1.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package suggested but not available: ‘ggrastr’

The suggested packages are required for a complete check.
Checking can be attempted without them by setting the environment
variable _R_CHECK_FORCE_SUGGESTS_ to a false value.

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR






```
### CRAN

```
* using log directory ‘/tmp/workdir/sccore/old/sccore.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘sccore/DESCRIPTION’ ... OK
* this is package ‘sccore’ version ‘0.1.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package suggested but not available: ‘ggrastr’

The suggested packages are required for a complete check.
Checking can be attempted without them by setting the environment
variable _R_CHECK_FORCE_SUGGESTS_ to a false value.

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR






```
# SDMtune

<details>

* Version: 1.1.2
* Source code: https://github.com/cran/SDMtune
* URL: https://consbiol-unibern.github.io/SDMtune/, https://github.com/ConsBiol-unibern/SDMtune
* BugReports: https://github.com/ConsBiol-unibern/SDMtune/issues
* Date/Publication: 2020-10-02 11:12:06 UTC
* Number of recursive dependencies: 123

Run `cloud_details(, "SDMtune")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/SDMtune/new/SDMtune.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘SDMtune/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘SDMtune’ version ‘1.1.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘SDMtune’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking R/sysdata.rda ... OK
* checking line endings in C/C++/Fortran sources/headers ... OK
* checking compiled code ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘basic-use.Rmd’ using ‘UTF-8’... OK
  ‘hyper-tuning.Rmd’ using ‘UTF-8’... OK
  ‘presence-absence.Rmd’ using ‘UTF-8’... OK
  ‘var-selection.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: OK






```
### CRAN

```
* using log directory ‘/tmp/workdir/SDMtune/old/SDMtune.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘SDMtune/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘SDMtune’ version ‘1.1.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘SDMtune’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking R/sysdata.rda ... OK
* checking line endings in C/C++/Fortran sources/headers ... OK
* checking compiled code ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘basic-use.Rmd’ using ‘UTF-8’... OK
  ‘hyper-tuning.Rmd’ using ‘UTF-8’... OK
  ‘presence-absence.Rmd’ using ‘UTF-8’... OK
  ‘var-selection.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: OK






```
# Seurat

<details>

* Version: 3.2.2
* Source code: https://github.com/cran/Seurat
* URL: https://satijalab.org/seurat, https://github.com/satijalab/seurat
* BugReports: https://github.com/satijalab/seurat/issues
* Date/Publication: 2020-09-26 04:30:12 UTC
* Number of recursive dependencies: 224

Run `cloud_details(, "Seurat")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/Seurat/new/Seurat.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘Seurat/DESCRIPTION’ ... OK
* this is package ‘Seurat’ version ‘3.2.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages suggested but not available:
  'S4Vectors', 'SummarizedExperiment', 'SingleCellExperiment', 'MAST',
  'DESeq2', 'BiocGenerics', 'GenomicRanges', 'GenomeInfoDb', 'IRanges',
  'rtracklayer', 'monocle', 'Biobase', 'limma', 'metap'

The suggested packages are required for a complete check.
Checking can be attempted without them by setting the environment
variable _R_CHECK_FORCE_SUGGESTS_ to a false value.

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR






```
### CRAN

```
* using log directory ‘/tmp/workdir/Seurat/old/Seurat.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘Seurat/DESCRIPTION’ ... OK
* this is package ‘Seurat’ version ‘3.2.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages suggested but not available:
  'S4Vectors', 'SummarizedExperiment', 'SingleCellExperiment', 'MAST',
  'DESeq2', 'BiocGenerics', 'GenomicRanges', 'GenomeInfoDb', 'IRanges',
  'rtracklayer', 'monocle', 'Biobase', 'limma', 'metap'

The suggested packages are required for a complete check.
Checking can be attempted without them by setting the environment
variable _R_CHECK_FORCE_SUGGESTS_ to a false value.

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR






```
# shiny

<details>

* Version: 1.5.0
* Source code: https://github.com/cran/shiny
* URL: http://shiny.rstudio.com
* BugReports: https://github.com/rstudio/shiny/issues
* Date/Publication: 2020-06-23 13:30:03 UTC
* Number of recursive dependencies: 98

Run `cloud_details(, "shiny")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/shiny/new/shiny.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘shiny/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘shiny’ version ‘1.5.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package suggested but not available: ‘ragg’

The suggested packages are required for a complete check.
Checking can be attempted without them by setting the environment
variable _R_CHECK_FORCE_SUGGESTS_ to a false value.

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR






```
### CRAN

```
* using log directory ‘/tmp/workdir/shiny/old/shiny.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘shiny/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘shiny’ version ‘1.5.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package suggested but not available: ‘ragg’

The suggested packages are required for a complete check.
Checking can be attempted without them by setting the environment
variable _R_CHECK_FORCE_SUGGESTS_ to a false value.

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR






```
# silicate

<details>

* Version: 0.6.1
* Source code: https://github.com/cran/silicate
* URL: https://github.com/hypertidy/silicate
* BugReports: https://github.com/hypertidy/silicate/issues
* Date/Publication: 2020-05-12 07:00:02 UTC
* Number of recursive dependencies: 138

Run `cloud_details(, "silicate")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/silicate/new/silicate.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘silicate/DESCRIPTION’ ... OK
* this is package ‘silicate’ version ‘0.6.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘silicate’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking R/sysdata.rda ... OK
* checking include directives in Makefiles ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘silicate-data-model.Rmd’ using ‘UTF-8’... OK
  ‘silicate_topology_01.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: OK






```
### CRAN

```
* using log directory ‘/tmp/workdir/silicate/old/silicate.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘silicate/DESCRIPTION’ ... OK
* this is package ‘silicate’ version ‘0.6.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘silicate’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking R/sysdata.rda ... OK
* checking include directives in Makefiles ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘silicate-data-model.Rmd’ using ‘UTF-8’... OK
  ‘silicate_topology_01.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: OK






```
# simhelpers

<details>

* Version: 0.1.0
* Source code: https://github.com/cran/simhelpers
* URL: https://meghapsimatrix.github.io/simhelpers/index.html
* BugReports: https://github.com/meghapsimatrix/simhelpers/issues
* Date/Publication: 2020-03-31 15:00:06 UTC
* Number of recursive dependencies: 98

Run `cloud_details(, "simhelpers")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/simhelpers/new/simhelpers.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘simhelpers/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘simhelpers’ version ‘0.1.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘simhelpers’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... NOTE
Namespaces in Imports field not imported from:
  ‘future’ ‘magrittr’ ‘purrr’ ‘rlang’ ‘utils’
  All declared Imports should be used.
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘MCSE.Rmd’ using ‘UTF-8’... OK
  ‘simulation_workflow.Rmd’ using ‘UTF-8’... OK
  ‘visualization.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 1 NOTE






```
### CRAN

```
* using log directory ‘/tmp/workdir/simhelpers/old/simhelpers.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘simhelpers/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘simhelpers’ version ‘0.1.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘simhelpers’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... NOTE
Namespaces in Imports field not imported from:
  ‘future’ ‘magrittr’ ‘purrr’ ‘rlang’ ‘utils’
  All declared Imports should be used.
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘MCSE.Rmd’ using ‘UTF-8’... OK
  ‘simulation_workflow.Rmd’ using ‘UTF-8’... OK
  ‘visualization.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 1 NOTE






```
# spbabel

<details>

* Version: 0.5.5
* Source code: https://github.com/cran/spbabel
* URL: https://mdsumner.github.io/spbabel/
* BugReports: https://github.com/mdsumner/spbabel/issues
* Date/Publication: 2020-09-15 09:10:03 UTC
* Number of recursive dependencies: 102

Run `cloud_details(, "spbabel")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/spbabel/new/spbabel.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘spbabel/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘spbabel’ version ‘0.5.5’
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘spbabel’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘spbabel.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: OK






```
### CRAN

```
* using log directory ‘/tmp/workdir/spbabel/old/spbabel.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘spbabel/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘spbabel’ version ‘0.5.5’
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘spbabel’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘spbabel.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: OK






```
# tidybayes

<details>

* Version: 2.1.1
* Source code: https://github.com/cran/tidybayes
* URL: http://mjskay.github.io/tidybayes, https://github.com/mjskay/tidybayes
* BugReports: https://github.com/mjskay/tidybayes/issues/new
* Date/Publication: 2020-06-19 05:50:03 UTC
* Number of recursive dependencies: 204

Run `cloud_details(, "tidybayes")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/tidybayes/new/tidybayes.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘tidybayes/DESCRIPTION’ ... OK
* this is package ‘tidybayes’ version ‘2.1.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘tidybayes’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... NOTE
Namespaces in Imports field not imported from:
  ‘HDInterval’ ‘grid’ ‘scales’
  All declared Imports should be used.
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘tidy-brms.Rmd’ using ‘UTF-8’... OK
  ‘tidy-rstanarm.Rmd’ using ‘UTF-8’... OK
  ‘tidybayes-residuals.Rmd’ using ‘UTF-8’... OK
  ‘tidybayes.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 1 NOTE






```
### CRAN

```
* using log directory ‘/tmp/workdir/tidybayes/old/tidybayes.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘tidybayes/DESCRIPTION’ ... OK
* this is package ‘tidybayes’ version ‘2.1.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘tidybayes’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... NOTE
Namespaces in Imports field not imported from:
  ‘HDInterval’ ‘grid’ ‘scales’
  All declared Imports should be used.
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘tidy-brms.Rmd’ using ‘UTF-8’... OK
  ‘tidy-rstanarm.Rmd’ using ‘UTF-8’... OK
  ‘tidybayes-residuals.Rmd’ using ‘UTF-8’... OK
  ‘tidybayes.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 1 NOTE






```
# trackr

<details>

* Version: 0.10.6
* Source code: https://github.com/cran/trackr
* Date/Publication: 2020-02-29 05:40:07 UTC
* Number of recursive dependencies: 107

Run `cloud_details(, "trackr")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/trackr/new/trackr.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘trackr/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘trackr’ version ‘0.10.6’
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available:
  'histry', 'CodeDepends', 'rsolr', 'roprov'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR






```
### CRAN

```
* using log directory ‘/tmp/workdir/trackr/old/trackr.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘trackr/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘trackr’ version ‘0.10.6’
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available:
  'histry', 'CodeDepends', 'rsolr', 'roprov'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR






```
# usethis

<details>

* Version: 1.6.3
* Source code: https://github.com/cran/usethis
* URL: https://usethis.r-lib.org, https://github.com/r-lib/usethis
* BugReports: https://github.com/r-lib/usethis/issues
* Date/Publication: 2020-09-17 17:00:03 UTC
* Number of recursive dependencies: 80

Run `cloud_details(, "usethis")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/usethis/new/usethis.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘usethis/DESCRIPTION’ ... OK
* this is package ‘usethis’ version ‘1.6.3’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘usethis’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking line endings in Makefiles ... OK
* checking for GNU extensions in Makefiles ... OK
* checking include directives in Makefiles ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘spelling.R’
  Running ‘testthat.R’
* DONE
Status: OK






```
### CRAN

```
* using log directory ‘/tmp/workdir/usethis/old/usethis.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘usethis/DESCRIPTION’ ... OK
* this is package ‘usethis’ version ‘1.6.3’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘usethis’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking line endings in Makefiles ... OK
* checking for GNU extensions in Makefiles ... OK
* checking include directives in Makefiles ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘spelling.R’
  Running ‘testthat.R’
* DONE
Status: OK






```
# vctrs

<details>

* Version: 0.3.4
* Source code: https://github.com/cran/vctrs
* URL: https://vctrs.r-lib.org/
* BugReports: https://github.com/r-lib/vctrs/issues
* Date/Publication: 2020-08-29 07:40:02 UTC
* Number of recursive dependencies: 71

Run `cloud_details(, "vctrs")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/vctrs/new/vctrs.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘vctrs/DESCRIPTION’ ... OK
* this is package ‘vctrs’ version ‘0.3.4’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘vctrs’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking line endings in C/C++/Fortran sources/headers ... OK
* checking line endings in Makefiles ... OK
* checking compilation flags in Makevars ... OK
* checking for GNU extensions in Makefiles ... OK
* checking for portable use of $(BLAS_LIBS) and $(LAPACK_LIBS) ... OK
* checking use of PKG_*FLAGS in Makefiles ... OK
* checking compiled code ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘pillar.Rmd’ using ‘UTF-8’... OK
  ‘s3-vector.Rmd’ using ‘UTF-8’... OK
  ‘stability.Rmd’ using ‘UTF-8’... OK
  ‘type-size.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: OK






```
### CRAN

```
* using log directory ‘/tmp/workdir/vctrs/old/vctrs.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘vctrs/DESCRIPTION’ ... OK
* this is package ‘vctrs’ version ‘0.3.4’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘vctrs’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking line endings in C/C++/Fortran sources/headers ... OK
* checking line endings in Makefiles ... OK
* checking compilation flags in Makevars ... OK
* checking for GNU extensions in Makefiles ... OK
* checking for portable use of $(BLAS_LIBS) and $(LAPACK_LIBS) ... OK
* checking use of PKG_*FLAGS in Makefiles ... OK
* checking compiled code ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘pillar.Rmd’ using ‘UTF-8’... OK
  ‘s3-vector.Rmd’ using ‘UTF-8’... OK
  ‘stability.Rmd’ using ‘UTF-8’... OK
  ‘type-size.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: OK






```
# ymlthis

<details>

* Version: 0.1.2
* Source code: https://github.com/cran/ymlthis
* URL: https://ymlthis.r-lib.org, https://github.com/r-lib/ymlthis
* BugReports: https://github.com/r-lib/ymlthis/issues
* Date/Publication: 2020-02-03 23:00:02 UTC
* Number of recursive dependencies: 91

Run `cloud_details(, "ymlthis")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/ymlthis/new/ymlthis.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘ymlthis/DESCRIPTION’ ... OK
* this is package ‘ymlthis’ version ‘0.1.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘ymlthis’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... NOTE
Namespaces in Imports field not imported from:
  ‘miniUI’ ‘shinyBS’
  All declared Imports should be used.
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘spelling.R’
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘introduction-to-ymlthis.Rmd’ using ‘UTF-8’... OK
  ‘yaml-fieldguide.Rmd’ using ‘UTF-8’... OK
  ‘yaml-overview.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 1 NOTE






```
### CRAN

```
* using log directory ‘/tmp/workdir/ymlthis/old/ymlthis.Rcheck’
* using R version 4.0.2 (2020-06-22)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘ymlthis/DESCRIPTION’ ... OK
* this is package ‘ymlthis’ version ‘0.1.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘ymlthis’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking dependencies in R code ... NOTE
Namespaces in Imports field not imported from:
  ‘miniUI’ ‘shinyBS’
  All declared Imports should be used.
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘spelling.R’
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘introduction-to-ymlthis.Rmd’ using ‘UTF-8’... OK
  ‘yaml-fieldguide.Rmd’ using ‘UTF-8’... OK
  ‘yaml-overview.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 1 NOTE






```
