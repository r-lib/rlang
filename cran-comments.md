
Fixes a bug in knitted markdown documents which caused backtraces to be displayed when they shouldn't.

Also fixes one valgrind issue (I can't reproduce the uninitialised value warning locally).

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
