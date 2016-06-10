# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.3.0 (2016-05-03) |
|system   |x86_64, darwin13.4.0         |
|ui       |X11                          |
|language |(EN)                         |
|collate  |en_US.UTF-8                  |
|tz       |America/Chicago              |
|date     |2016-06-10                   |

## Packages

|package   |*  |version     |date       |source                     |
|:---------|:--|:-----------|:----------|:--------------------------|
|covr      |   |2.0.1       |2016-04-06 |CRAN (R 3.3.0)             |
|knitr     |   |1.13        |2016-05-09 |CRAN (R 3.3.0)             |
|lazyeval  |   |0.1.10.9000 |2016-06-10 |local (hadley/lazyeval@NA) |
|rmarkdown |   |0.9.6       |2016-05-01 |CRAN (R 3.3.0)             |
|testthat  |*  |1.0.2.9000  |2016-06-10 |github (hadley/testthat)   |

# Check results
55 packages

## alakazam (0.2.3)
Maintainer: Jason Vander Heiden <jason.vanderheiden@yale.edu>  
Bug reports: https://bitbucket.org/kleinstein/alakazam/issues

0 errors | 0 warnings | 0 notes

## assertr (1.0.0)
Maintainer: Tony Fischetti <tony.fischetti@gmail.com>  
Bug reports: https://github.com/tonyfischetti/assertr/issues

0 errors | 0 warnings | 1 note 

```
checking R code for possible problems ... NOTE
maha_dist: no visible global function definition for ‘cov’
within_n_mads : <anonymous>: no visible global function definition for
  ‘mad’
within_n_mads : <anonymous>: no visible global function definition for
  ‘median’
within_n_sds : <anonymous>: no visible global function definition for
  ‘sd’
Undefined global functions or variables:
  cov mad median sd
Consider adding
  importFrom("stats", "cov", "mad", "median", "sd")
to your NAMESPACE file.
```

## binomen (0.1.0)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: https://github.com/ropensci/binomen/issues

0 errors | 0 warnings | 0 notes

## bioinactivation (1.1.2)
Maintainer: Alberto Garre <garre.alberto@gmail.com>

0 errors | 0 warnings | 0 notes

## chunked (0.2.1)
Maintainer: Edwin de Jonge <edwindjonge@gmail.com>  
Bug reports: https://github.com/edwindj/chunked/issues

0 errors | 0 warnings | 0 notes

## cofeatureR (1.0.1)
Maintainer: Fong Chun Chan <fongchunchan@gmail.com>  
Bug reports: https://github.com/tinyheero/cofeatureR/issues

0 errors | 0 warnings | 0 notes

## condformat (0.2.0)
Maintainer: Sergio Oller Moreno <sergioller@gmail.com>  
Bug reports: http://github.com/zeehio/condformat/issues

0 errors | 0 warnings | 0 notes

## datastepr (0.0.1)
Maintainer: Brandon Taylor <brandon.taylor221@gmail.com>

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
          recursively.

_E_x_a_m_p_l_e_s:

     step = dataStepClass$new()
     
     frame = data.frame(x = 1:10)
... 8 lines ...
     }
     
     stairs()
     
     step$results
     

Quitting from lines 55-84 (datastepping.Rmd) 
Error: processing vignette 'datastepping.Rmd' failed with diagnostics:
incorrect length (2), expecting: 10
Execution halted
```

## ddpcr (1.3)
Maintainer: Dean Attali <daattali@gmail.com>  
Bug reports: https://github.com/daattali/ddpcr/issues

0 errors | 0 warnings | 0 notes

## dplyr (0.4.3)
Maintainer: Hadley Wickham <hadley@rstudio.com>  
Bug reports: https://github.com/hadley/dplyr/issues

0 errors | 0 warnings | 0 notes

## easyformatr (0.1.1)
Maintainer: Brandon Taylor <brandon.taylor221@gmail.com>  
Bug reports: https://github.com/bramtayl/easyformatr/issues

0 errors | 0 warnings | 0 notes

## elpatron (0.0.2)
Maintainer: Jordan Mackie <jmackie@protonmail.com>

0 errors | 0 warnings | 0 notes

## emil (2.2.3)
Maintainer: Christofer Backlin <emil@christofer.backlin.se>  
Bug reports: https://github.com/Molmed/emil/issues

0 errors | 0 warnings | 0 notes

## eyetrackingR (0.1.6)
Maintainer: Jacob Dink <jacobwdink@gmail.com>  
Bug reports: https://github.com/jwdink/eyetrackingR/issues

0 errors | 0 warnings | 0 notes

## forestmodel (0.4.0)
Maintainer: Nick Kennedy <r@nick-kennedy.com>

0 errors | 0 warnings | 0 notes

## ggvis (0.4.2)
Maintainer: Winston Chang <winston@rstudio.com>

1 error  | 0 warnings | 1 note 

```
checking tests ... ERROR
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  [8] 7 - 2 == 5
  [9] 7 - 2 == 5
  ...
  
  
  testthat results ================================================================
  OK: 444 SKIPPED: 0 FAILED: 1
  1. Failure: Automatic width (@test-compute-bin.r#143) 
  
  Error: testthat unit tests failed
  In addition: Warning message:
  In rbind_all(out[[1]]) : Unequal factor levels: coercing to character
  Execution halted

checking R code for possible problems ... NOTE
adjust_breaks: no visible global function definition for ‘median’
bin_params.POSIXct: no visible global function definition for ‘is’
bin_vector.POSIXct: no visible global function definition for ‘is’
combine_data_props: no visible global function definition for
  ‘setNames’
combine_data_props : <anonymous>: no visible global function definition
  for ‘setNames’
compute_boxplot.data.frame: no visible global function definition for
  ‘quantile’
... 24 lines ...
  ‘packageVersion’
Undefined global functions or variables:
  complete.cases formula is median na.omit packageVersion predict qt
  quantile runif setNames terms
Consider adding
  importFrom("methods", "is")
  importFrom("stats", "complete.cases", "formula", "median", "na.omit",
             "predict", "qt", "quantile", "runif", "setNames", "terms")
  importFrom("utils", "packageVersion")
to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
contains 'methods').
```

## gutenbergr (0.1.1)
Maintainer: David Robinson <admiral.david@gmail.com>  
Bug reports: http://github.com/ropenscilabs/gutenbergr/issues

0 errors | 0 warnings | 0 notes

## heemod (0.3.1)
Maintainer: Antoine Filipovic-Pierucci <pierucci@gmail.com>  
Bug reports: https://github.com/pierucci/heemod/issues

0 errors | 0 warnings | 0 notes

## IAT (0.3)
Maintainer: Dan Martin <dpmartin42@gmail.com>

0 errors | 0 warnings | 0 notes

## jqr (0.2.3)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: https://github.com/ropensci/jqr/issues

0 errors | 0 warnings | 0 notes

## loopr (1.0.1)
Maintainer: Brandon Taylor <Brandon.Taylor221@gmail.com>

0 errors | 0 warnings | 1 note 

```
checking R code for possible problems ... NOTE
amendColumns: no visible global function definition for ‘setNames’
fillColumns: no visible global function definition for ‘setNames’
Undefined global functions or variables:
  setNames
Consider adding
  importFrom("stats", "setNames")
to your NAMESPACE file.
```

## markmyassignment (0.6.0)
Maintainer: Mans Magnusson <mons.magnusson@gmail.com>

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  8: make_label(object, label)
  9: label %||% label(object)
  10: label(object)
  11: paste(deparse(as.call(list(x[[1]], quote(...)))), collapse = "\n")
  12: deparse(as.call(list(x[[1]], quote(...))))
  
  testthat results ================================================================
  OK: 53 SKIPPED: 1 FAILED: 2
  1. Error: expect_function_self_contained() (@test-expectation.R#8) 
  2. Error: expect_function_arguments() (@test-expectation.R#22) 
  
  Error: testthat unit tests failed
  Execution halted
```

## mosaic (0.13.0)
Maintainer: Randall Pruim <rpruim@calvin.edu>  
Bug reports: https://github.com/ProjectMOSAIC/mosaic/issues

1 error  | 0 warnings | 0 notes

```
checking examples ... ERROR
Running examples in ‘mosaic-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: MSPE
> ### Title: Mean Squared Prediction Error
> ### Aliases: MSPE
> 
> ### ** Examples
> 
> HELP <- HELPrct %>% sample_frac(.3)
> MSPE( gwm( age ~ sex, data = HELP), HELPrct)
[1] 59.70464
> MSPE( gwm( age ~ 1, data = HELP), HELPrct)
[1] 59.4482
> MSPE( gwm( age ~ sex + homeless, data = HELP), HELPrct)
[1] 59.43545
> MSPE( gwm( sex ~ 1, data = HELP), HELPrct)
Error in lazyeval::lazy(x) : Promise has already been forced
Calls: MSPE -> gwm -> data.frame -> tally -> <Anonymous> -> .Call
Execution halted
```

## ncappc (0.2.1.1)
Maintainer: Chayan Acharya <chayan.acharya@farmbio.uu.se>

0 errors | 0 warnings | 0 notes

## networkreporting (0.1.0)
Maintainer: Dennis M. Feehan <feehan@berkeley.edu>

0 errors | 0 warnings | 0 notes

## openair (1.8-2)
Maintainer: David Carslaw <david.carslaw@york.ac.uk>  
Bug reports: https://github.com/davidcarslaw/openair/issues

0 errors | 0 warnings | 0 notes

## photobiologyInOut (0.4.4)
Maintainer: Pedro J. Aphalo <pedro.aphalo@helsinki.fi>  
Bug reports: https://bitbucket.org/aphalo/photobiologyinout/

0 errors | 0 warnings | 0 notes

## platetools (0.0.1)
Maintainer: Scott Warchal <s.warchal@sms.ed.ac.uk>

0 errors | 0 warnings | 0 notes

## poplite (0.99.16)
Maintainer: Daniel Bottomly <bottomly@ohsu.edu>

0 errors | 1 warning  | 2 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...

    select

The following object is masked from ‘package:stats’:

    filter

... 8 lines ...
Loading required package: DBI
Starting gender
Starting clinical
Starting samples
Starting dna

Error: processing vignette 'poplite.Rnw' failed with diagnostics:
 chunk 15 
Error in library(VariantAnnotation) : 
  there is no package called ‘VariantAnnotation’
Execution halted

checking package dependencies ... NOTE
Package suggested but not available for checking: ‘VariantAnnotation’

checking R code for possible problems ... NOTE
filter_.Database: no visible global function definition for ‘stack’
get.starting.point : <anonymous>: no visible global function definition
  for ‘na.omit’
select_.Database: no visible global function definition for ‘stack’
tsl.to.graph: no visible global function definition for ‘stack’
join,Database: no visible global function definition for ‘stack’
join,Database : .get.select.cols: no visible global function definition
  for ‘setNames’
join,Database: no visible binding for global variable ‘new.ancil’
join,Database: no visible global function definition for ‘setNames’
Undefined global functions or variables:
  na.omit new.ancil setNames stack
Consider adding
  importFrom("stats", "na.omit", "setNames")
  importFrom("utils", "stack")
to your NAMESPACE file.
```

## purrr (0.2.1)
Maintainer: Hadley Wickham <hadley@rstudio.com>  
Bug reports: https://github.com/hadley/purrr/issues

0 errors | 0 warnings | 0 notes

## R6Frame (0.1.0)
Maintainer: Kristian D. Olsen <kristian@doingit.no>

0 errors | 0 warnings | 0 notes

## rbokeh (0.4.2)
Maintainer: Ryan Hafen <rhafen@gmail.com>  
Bug reports: https://github.com/hafen/rbokeh/issues

0 errors | 0 warnings | 0 notes

## request (0.1.0)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: https://github.com/sckott/request/issues

0 errors | 0 warnings | 0 notes

## rex (1.1.1)
Maintainer: Jim Hester <james.f.hester@gmail.com>  
Bug reports: https://github.com/kevinushey/rex/issues

0 errors | 0 warnings | 0 notes

## rfishbase (2.1.0)
Maintainer: Carl Boettiger <cboettig@ropensci.org>  
Bug reports: https://github.com/ropensci/rfishbase/issues

0 errors | 0 warnings | 0 notes

## rgho (0.0.1)
Maintainer: Antoine Filipovic-Pierucci <pierucci@gmail.com>  
Bug reports: https://github.com/pierucci/rgho/issues

0 errors | 0 warnings | 0 notes

## riem (0.1.0)
Maintainer: Maëlle Salmon <maelle.salmon@yahoo.se>  
Bug reports: http://github.com/ropenscilabs/riem/issues

0 errors | 0 warnings | 1 note 

```
checking DESCRIPTION meta-information ... NOTE
Authors@R field gives persons with no valid roles:
  Brooke Anderson [rev] (Brooke Anderson reviewed the package for rOpenSci, see https://github.com/ropensci/onboarding/issues/39.)
```

## RmarineHeatWaves (0.13.1)
Maintainer: Albertus J. Smit <albertus.smit@gmail.com>

0 errors | 0 warnings | 0 notes

## RNeXML (2.0.6)
Maintainer: Carl Boettiger <cboettig@gmail.com>  
Bug reports: https://github.com/ropensci/RNeXML/issues

0 errors | 0 warnings | 1 note 

```
checking package dependencies ... NOTE
Packages suggested but not available for checking: ‘rrdf’ ‘Sxslt’
```

## rprev (0.1.0)
Maintainer: Stuart Lacy <stuart.lacy@york.ac.uk>

0 errors | 0 warnings | 0 notes

## rscorecard (0.2.5)
Maintainer: Benjamin Skinner <b.skinner@vanderbilt.edu>  
Bug reports: http://github.com/btskinner/rscorecard/issues

0 errors | 0 warnings | 0 notes

## scrubr (0.1.1)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: https://github.com/ropenscilabs/scrubr/issues

0 errors | 0 warnings | 0 notes

## shazam (0.1.2)
Maintainer: Jason Vander Heiden <jason.vanderheiden@yale.edu>  
Bug reports: https://bitbucket.org/kleinstein/shazam/issues

0 errors | 0 warnings | 0 notes

## simPH (1.3.9)
Maintainer: Christopher Gandrud <christopher.gandrud@gmail.com>  
Bug reports: https://github.com/christophergandrud/simPH/issues

0 errors | 0 warnings | 0 notes

## SpaDES (1.1.4)
Maintainer: Alex M Chubaty <alexander.chubaty@canada.ca>  
Bug reports: https://github.com/PredictiveEcology/SpaDES/issues

0 errors | 0 warnings | 1 note 

```
checking package dependencies ... NOTE
Package suggested but not available for checking: ‘fastshp’
```

## sprintfr (0.1.0)
Maintainer: Brandon Taylor <brandon.taylor221@gmail.com>  
Bug reports: https://github.com/bramtayl/sprintfr/issues

0 errors | 0 warnings | 0 notes

## srvyr (0.1.1)
Maintainer: Greg Freedman <greg.freedman@gmail.com>  
Bug reports: https://github.com/gergness/srvyr/issues

0 errors | 0 warnings | 0 notes

## statar (0.6.1)
Maintainer: Matthieu Gomez <mattg@princeton.edu>  
Bug reports: https://github.com/matthieugomez/statar/issues

0 errors | 0 warnings | 0 notes

## strptimer (0.1.0)
Maintainer: Brandon Taylor <brandon.taylor221@gmail.com>

0 errors | 0 warnings | 0 notes

## tadaatoolbox (0.9.0)
Maintainer: Lukas Burk <lukas@quantenbrot.de>  
Bug reports: https://github.com/tadaadata/tadaatoolbox/issues

0 errors | 0 warnings | 0 notes

## tibble (1.0)
Maintainer: Kirill Müller <krlmlr+r@mailbox.org>  
Bug reports: https://github.com/hadley/tibble/issues

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  4. Failure: trunc_mat output matches known output (@test-trunc-mat.r#48) -------
  attr(knit, "knit_cacheable") isn't true.
  
  
  testthat results ================================================================
  OK: 190 SKIPPED: 0 FAILED: 4
  1. Failure: trunc_mat output matches known output (@test-trunc-mat.r#37) 
  2. Failure: trunc_mat output matches known output (@test-trunc-mat.r#41) 
  3. Failure: trunc_mat output matches known output (@test-trunc-mat.r#44) 
  4. Failure: trunc_mat output matches known output (@test-trunc-mat.r#48) 
  
  Error: testthat unit tests failed
  Execution halted
```

## tidyr (0.4.1)
Maintainer: Hadley Wickham <hadley@rstudio.com>  
Bug reports: https://github.com/hadley/tidyr/issues

0 errors | 0 warnings | 0 notes

## treeplyr (0.1.1)
Maintainer: Josef Uyeda <josef.uyeda@gmail.com>

0 errors | 0 warnings | 0 notes

## VWPre (0.5.0)
Maintainer: Vincent Porretta <vincentporretta@gmail.com>

0 errors | 0 warnings | 0 notes

## wrswoR.benchmark (0.1-1)
Maintainer: Kirill Müller <krlmlr+r@mailbox.org>  
Bug reports: https://github.com/krlmlr/wrswoR.benchmark/issues

0 errors | 0 warnings | 0 notes

