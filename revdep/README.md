# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.3.0 (2016-05-03) |
|system   |x86_64, darwin13.4.0         |
|ui       |RStudio (0.99.1186)          |
|language |(EN)                         |
|collate  |en_US.UTF-8                  |
|tz       |America/Chicago              |
|date     |2016-05-25                   |

## Packages

|package   |*  |version     |date       |source                          |
|:---------|:--|:-----------|:----------|:-------------------------------|
|covr      |   |2.0.1       |2016-04-06 |CRAN (R 3.3.0)                  |
|knitr     |   |1.13        |2016-05-09 |CRAN (R 3.3.0)                  |
|lazyeval  |   |0.1.10.9000 |2016-05-25 |local (hadley/lazyeval@bce211b) |
|rmarkdown |   |0.9.6       |2016-05-01 |CRAN (R 3.3.0)                  |
|testthat  |*  |1.0.2       |2016-04-23 |CRAN (R 3.3.0)                  |

# Check results
50 packages

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

0 errors | 0 warnings | 0 notes

## dplyr (0.4.3)
Maintainer: Hadley Wickham <hadley@rstudio.com>  
Bug reports: https://github.com/hadley/dplyr/issues

1 error  | 0 warnings | 0 notes

```
checking package dependencies ... ERROR
Package required but not available: ‘BH’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```

## easyformatr (0.1.1)
Maintainer: Brandon Taylor <brandon.taylor221@gmail.com>  
Bug reports: https://github.com/bramtayl/easyformatr/issues

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

1 error  | 0 warnings | 0 notes

```
checking package dependencies ... ERROR
Package required but not available: ‘BH’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```

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

0 errors | 0 warnings | 0 notes

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

## multiplyr (0.1.0)
Maintainer: Jim Blundell <james.blundell@keble.ox.ac.uk>

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  1. Error: arrange() sorts on a single cluster (@test-arrange.R#7) 
  2. Error: arrange() can sort with multiple levels (@test-arrange.R#27) 
  3. Error: arrange() sorts on a cluster of size 2 (@test-arrange.R#39) 
  4. Error: arrange() maintains groups when sorting a grouped data frame (@test-arrange.R#64) 
  5. Error: arrange() throws an error with undefined columns (@test-arrange.R#78) 
  6. Error: arrange() can deal with an empty data frame (@test-arrange.R#84) 
  7. Error: arrange() with no parameters returns data frame unchanged (@test-arrange.R#91) 
  8. Error: arrange() returns a data frame (@test-arrange.R#97) 
  9. Error: Multiplyr(x=...) creates the appropriate structure (@test-class.R#8) 
  1. ...
  
  Error: testthat unit tests failed
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

1 error  | 0 warnings | 0 notes

```
checking package dependencies ... ERROR
Package required but not available: ‘BH’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```

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

## RNeXML (2.0.6)
Maintainer: Carl Boettiger <cboettig@gmail.com>  
Bug reports: https://github.com/ropensci/RNeXML/issues

0 errors | 0 warnings | 1 note 

```
checking package dependencies ... NOTE
Packages suggested but not available for checking: ‘rrdf’ ‘Sxslt’
```

## rscorecard (0.2.5)
Maintainer: Benjamin Skinner <b.skinner@vanderbilt.edu>  
Bug reports: http://github.com/btskinner/rscorecard/issues

0 errors | 0 warnings | 0 notes

## SciencesPo (1.3.9)
Maintainer: Daniel Marcelino <dmarcelino@live.com>  
Bug reports: http://github.com/danielmarcelino/SciencesPo/issues

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Units                 Measurement System Units
bhodrick93            Bekaert's and Hodrick's (1993) Data
cathedrals            Cathedrals
cgreene76             Christensen's and Greene's (1976) Data
galton                Galton's Family Data on Human Stature.
griliches76           Griliches's (1976) Data
ltaylor96             Lothian's and Taylor's (1996) Data Set
... 8 lines ...
turnout               Turnout Data
twins                 Burt's twin data
words                 Word frequencies from Mosteller and Wallace

Loading required package: SciencesPo
initializing ... done

Quitting from lines 395-399 (SciencesPo.Rmd) 
Error: processing vignette 'SciencesPo.Rmd' failed with diagnostics:
polygon edge not found
Execution halted
```

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

