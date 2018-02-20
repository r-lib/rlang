
## Test environments

* local OS X install: 3.4.3
* travis-ci: 3.1, 3.3, 3.4.3, and devel
* win-builder: devel and release


## R CMD check results

0 errors | 0 warnings | 0 notes


## Reverse dependencies

I have run R CMD check on the 134 downstream dependencies. (Summary at
https://github.com/tidyverse/rlang/tree/master/revdep).

I couldn't install 8 packages:

  idealstan, pointblank, poppr, RtutoR, rubias, sf, infer, ipumsr

There were 9 broken packages. I have sent emails one month ago and
again today to inform the authors. Updates for dplyr and dbplot will
be sent shortly to CRAN to fix the issues. dbplyr should already be
fixed.
