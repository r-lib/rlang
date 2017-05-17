## Bugfix release

The last rlang release was sent recently, but we have discovered one
issue with the bytecode compiler that causes downstream failures in
the reverse dependencies of the next dplyr version. This release
should prevent many R CMD check failures for compiled packages
depending on dplyr.


## Test environments

* local OS X install, R 3.4.0
* ubuntu 12.04 (on travis-ci), R 3.4.0
* win-builder (devel and release)


## R CMD check results

0 errors | 0 warnings | 0 notes


## Reverse dependencies

I have run R CMD check on the 2 downstream dependencies. (Summary at
https://github.com/tidyverse/rlang/tree/master/revdep).

There were no problems.
