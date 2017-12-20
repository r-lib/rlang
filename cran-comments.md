
This point release switches from `SET_NAMED()` to
`MARK_NOT_MUTABLE()`.


## Test environments

* local OS X install, R 3.4.3
* ubuntu 12.04 (on travis-ci), R 3.4.3
* win-builder (devel and release)


## R CMD check results

0 errors | 0 warnings | 0 notes


## Reverse dependencies

The only change is the use of `MARK_NOT_MUTABLE()` so I have not run
the reverse dependencies.
