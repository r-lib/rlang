
## Test environments

* local OS X install: release
* travis-ci: 3.1, 3.3, 3.4, release, and devel
* win-builder: devel and release
* rchk: unbuntu-rchk platform on R-hub


## R CMD check results

0 errors | 0 warnings | 0 notes


## Reverse dependencies

I have run R CMD check on the 325 downstream dependencies. (Summary at
https://github.com/tidyverse/rlang/tree/master/revdep).

I couldn't install 15 packages. There were 6 broken packages and I
have sent emails and fixes to the authors 2 weeks ago. 2 of these
failures were fixed in the mean time, leaving 4 broken packages. The
author of the naniar package will send a fix shortly and we will send
a patch release for dplyr as well (the check failure is caused by
assumptions in the tests that are no longer true rather than broken
behaviour).

In addition, I have tested the reverse dependencies of dplyr, tidyr,
and purrr (1245 packages). I couldn't check 47 packages. I found 3
failures in dplyr's revdeps, 1 in purrr's revdeps and 1 in tidyr's
revdeps. The authors of these packages were informed two weeks ago as
well.
