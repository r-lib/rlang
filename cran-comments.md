
Fixes the rchk problems. However, rchk warnings remain after the fix which AFAICS are false positives. I have contacted Tomas Kalibera about it. Refactoring the code to work around the warnings would be too dangerous for a quick release (dots capture in the tidyverse relies on it).

## Test environments

* local OS X install: release
* travis-ci: 3.1, 3.3, 3.4, release, and devel
* win-builder: devel and release
* rchk: unbuntu-rchk platform on R-hub


## R CMD check results

0 errors | 0 warnings | 0 notes


## Reverse dependencies

I have run R CMD check on the 444 downstream dependencies. (Summary at https://github.com/r-lib/rlang/blob/release-0.3.2/revdep/README.md).

There were no issues.
