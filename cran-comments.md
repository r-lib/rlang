
## Test environments

* local OS X install: release
* travis-ci: 3.1, oldrel, release, and devel
* win-builder: devel and release


## R CMD check results

0 errors | 0 warnings | 0 notes


## Reverse dependencies

I have run R CMD check on the 219 downstream dependencies. (Summary at
https://github.com/r-lib/rlang/tree/release-0.2.1/revdep).

I couldn't install 21 packages. 1 package failed (pkgdown) but I
couldn't reproduce this failure manually.

In addition I have run the revdeps of dplyr, tidyr, and purrr. There
was only one failure (GenomicDataCommons) which seemed unrelated (file
copy failure).
