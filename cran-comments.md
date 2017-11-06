
The rchk warnings should now be fixed.


## Test environments

* local OS X install, R 3.4.2
* ubuntu 12.04 (on travis-ci), R 3.4.2
* win-builder (devel and release)


## R CMD check results

0 errors | 0 warnings | 0 notes


## Reverse dependencies

I have run R CMD check on the 72 downstream dependencies.

There were 6 packages I couldn't check:

- bomrang
- GSODR
- pointblank
- poppr
- prisonbrief
- sf


There was 1 broken package :

- taxa

This is due to an intentional fix within rlang. I have sent a patch to
`taxa` authors to fix this issue and they will send a CRAN update
shortly.
