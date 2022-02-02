## Test environments

* local OS X install: release
* ci: r-devel and 5 last R versions
* win-builder: devel and release
* rchk: unbuntu-rchk platform on R-hub


## R CMD check results

0 errors | 0 warnings | 0 notes


## Reverse dependencies

We checked 1534 reverse dependencies (1531 from CRAN + 3 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 1 new problems
 * We failed to check 3 packages

Issues with CRAN packages are summarised below.

### New problems
(This reports the first line of each new failure)

* galah
  checking tests ... ERROR

  -> Seems unrelated and I can't reproduce.

### Failed to check

* bayesmodels (NA)
* conos       (NA)
* loon.ggplot (NA)
> 


## CRAN checks

Fixed a performance regression that caused excessive checking time with packages throwing unhandled warnings in tight loops during checks

Fixed Rd cross-references NOTE on r-devel-linux-x86_64-fedora-clang
