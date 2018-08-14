
This release fixes the protection issues revealed by rchk.

## Test environments

* local OS X install: release
* travis-ci: 3.1, oldrel, release, and devel
* win-builder: devel and release


## R CMD check results

0 errors | 0 warnings | 0 notes


## Reverse dependencies

I have run R CMD check on the 285 downstream dependencies. (Summary at https://github.com/r-lib/rlang/tree/release-0.2.2/revdep).

I couldn't install 8 packages. 1 package failed (Nmisc) but it doesn't seem like the failure is due to rlang. I have informed the author about the failure.
