## Test environments
* local OS X install, R 3.3.0
* ubuntu 12.04 (on travis-ci), R 3.3.0
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

I ran `R CMD check` on all 55 reverse dependencies. (https://github.com/hadley/dplyr/tree/master/revdep/). 

I saw the following problems:

* datastepr: checking re-building of vignette outputs ... WARNING
  New vignette failure - not obvious why.

* ggvis: checking tests ... ERROR
  Existing CRAN failure (I'll get this fixed next week)

* markmyassignment: checking tests ... ERROR
  New test failure - not obvious why.

* mosaic: checking examples ... ERROR
  Author is aware of problem and planning submission ASAP.

* poplite: checking re-building of vignette outputs ... WARNING

* tibble: checking tests ... ERROR
  Existing CRAN failure

I notified all authors about problems on on May 27 and again today.

