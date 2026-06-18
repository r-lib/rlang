# a5R

<details>

* Version: 0.4.0
* GitHub: https://github.com/belian-earth/a5R
* Source code: https://github.com/cran/a5R
* Date/Publication: 2026-05-14 13:00:07 UTC
* Number of recursive dependencies: 67

Run `revdepcheck::cloud_details(, "a5R")` for more info

</details>

## In both

*   checking whether package ‘a5R’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/a5R/new/a5R.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘a5R’ ...
** package ‘a5R’ successfully unpacked and MD5 sums checked
** using staged installation
Using cargo 1.75.0
Using rustc 1.75.0 (82e1608df 2023-12-21) (built from a source tarball)
Building for CRAN.
Writing `src/Makevars`.
`tools/config.R` has finished.
** libs
using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0’
...

Caused by:
  feature `edition2024` is required

  The package requires the Cargo feature called `edition2024`, but that feature is not stabilized in this version of Cargo (1.75.0).
  Consider trying a more recent nightly release.
  See https://doc.rust-lang.org/nightly/cargo/reference/unstable.html#edition-2024 for more information about the status of this feature.
make: *** [Makevars:26: rust/target/release/liba5R.a] Error 101
ERROR: compilation failed for package ‘a5R’
* removing ‘/tmp/workdir/a5R/new/a5R.Rcheck/a5R’


```
### CRAN

```
* installing *source* package ‘a5R’ ...
** package ‘a5R’ successfully unpacked and MD5 sums checked
** using staged installation
Using cargo 1.75.0
Using rustc 1.75.0 (82e1608df 2023-12-21) (built from a source tarball)
Building for CRAN.
Writing `src/Makevars`.
`tools/config.R` has finished.
** libs
using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0’
...

Caused by:
  feature `edition2024` is required

  The package requires the Cargo feature called `edition2024`, but that feature is not stabilized in this version of Cargo (1.75.0).
  Consider trying a more recent nightly release.
  See https://doc.rust-lang.org/nightly/cargo/reference/unstable.html#edition-2024 for more information about the status of this feature.
make: *** [Makevars:26: rust/target/release/liba5R.a] Error 101
ERROR: compilation failed for package ‘a5R’
* removing ‘/tmp/workdir/a5R/old/a5R.Rcheck/a5R’


```
# AbSolution

<details>

* Version: 1.0.1
* GitHub: https://github.com/EDS-Bioinformatics-Laboratory/AbSolution
* Source code: https://github.com/cran/AbSolution
* Date/Publication: 2026-04-27 08:50:09 UTC
* Number of recursive dependencies: 217

Run `revdepcheck::cloud_details(, "AbSolution")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/AbSolution/new/AbSolution.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘AbSolution/DESCRIPTION’ ... OK
...
* this is package ‘AbSolution’ version ‘1.0.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘alakazam’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/AbSolution/old/AbSolution.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘AbSolution/DESCRIPTION’ ... OK
...
* this is package ‘AbSolution’ version ‘1.0.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘alakazam’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# arcgisplaces

<details>

* Version: 0.1.2
* GitHub: NA
* Source code: https://github.com/cran/arcgisplaces
* Date/Publication: 2025-04-10 16:20:02 UTC
* Number of recursive dependencies: 30

Run `revdepcheck::cloud_details(, "arcgisplaces")` for more info

</details>

## In both

*   checking whether package ‘arcgisplaces’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/arcgisplaces/new/arcgisplaces.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘arcgisplaces’ ...
** package ‘arcgisplaces’ successfully unpacked and MD5 sums checked
** using staged installation
Using cargo 1.75.0
Using rustc 1.75.0 (82e1608df 2023-12-21) (built from a source tarball)
Building for CRAN.
Writing `src/Makevars`.
`tools/config.R` has finished.
** libs
using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0’
...
export CARGO_HOME=/tmp/workdir/arcgisplaces/new/arcgisplaces.Rcheck/00_pkg_src/arcgisplaces/src/.cargo && \
export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/tmp/home/.cargo/bin" && \
RUSTFLAGS=" --print=native-static-libs" cargo build -j 2 --offline --lib --release --manifest-path=./rust/Cargo.toml --target-dir ./rust/target
error: package `native-tls v0.2.14` cannot be built because it requires rustc 1.80.0 or newer, while the currently active rustc version is 1.75.0
Either upgrade to rustc 1.80.0 or newer, or use
cargo update native-tls@0.2.14 --precise ver
where `ver` is the latest version of `native-tls` supporting rustc 1.75.0
make: *** [Makevars:28: rust/target/release/libarcgisplaces.a] Error 101
ERROR: compilation failed for package ‘arcgisplaces’
* removing ‘/tmp/workdir/arcgisplaces/new/arcgisplaces.Rcheck/arcgisplaces’


```
### CRAN

```
* installing *source* package ‘arcgisplaces’ ...
** package ‘arcgisplaces’ successfully unpacked and MD5 sums checked
** using staged installation
Using cargo 1.75.0
Using rustc 1.75.0 (82e1608df 2023-12-21) (built from a source tarball)
Building for CRAN.
Writing `src/Makevars`.
`tools/config.R` has finished.
** libs
using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0’
...
export CARGO_HOME=/tmp/workdir/arcgisplaces/old/arcgisplaces.Rcheck/00_pkg_src/arcgisplaces/src/.cargo && \
export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/tmp/home/.cargo/bin" && \
RUSTFLAGS=" --print=native-static-libs" cargo build -j 2 --offline --lib --release --manifest-path=./rust/Cargo.toml --target-dir ./rust/target
error: package `native-tls v0.2.14` cannot be built because it requires rustc 1.80.0 or newer, while the currently active rustc version is 1.75.0
Either upgrade to rustc 1.80.0 or newer, or use
cargo update native-tls@0.2.14 --precise ver
where `ver` is the latest version of `native-tls` supporting rustc 1.75.0
make: *** [Makevars:28: rust/target/release/libarcgisplaces.a] Error 101
ERROR: compilation failed for package ‘arcgisplaces’
* removing ‘/tmp/workdir/arcgisplaces/old/arcgisplaces.Rcheck/arcgisplaces’


```
# bayesdfa

<details>

* Version: 1.3.4
* GitHub: https://github.com/fate-ewi/bayesdfa
* Source code: https://github.com/cran/bayesdfa
* Date/Publication: 2025-03-22 20:30:21 UTC
* Number of recursive dependencies: 86

Run `revdepcheck::cloud_details(, "bayesdfa")` for more info

</details>

## In both

*   checking whether package ‘bayesdfa’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/bayesdfa/new/bayesdfa.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘bayesdfa’ ...
** package ‘bayesdfa’ successfully unpacked and MD5 sums checked
** using staged installation
Error in loadNamespace(x) : there is no package called ‘rstantools’
Calls: loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: configuration failed for package ‘bayesdfa’
* removing ‘/tmp/workdir/bayesdfa/new/bayesdfa.Rcheck/bayesdfa’


```
### CRAN

```
* installing *source* package ‘bayesdfa’ ...
** package ‘bayesdfa’ successfully unpacked and MD5 sums checked
** using staged installation
Error in loadNamespace(x) : there is no package called ‘rstantools’
Calls: loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: configuration failed for package ‘bayesdfa’
* removing ‘/tmp/workdir/bayesdfa/old/bayesdfa.Rcheck/bayesdfa’


```
# bmm

<details>

* Version: 1.3.1
* GitHub: https://github.com/venpopov/bmm
* Source code: https://github.com/cran/bmm
* Date/Publication: 2026-06-05 07:40:02 UTC
* Number of recursive dependencies: 138

Run `revdepcheck::cloud_details(, "bmm")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/bmm/new/bmm.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘bmm/DESCRIPTION’ ... OK
...
* this is package ‘bmm’ version ‘1.3.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘rtdists’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/bmm/old/bmm.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘bmm/DESCRIPTION’ ... OK
...
* this is package ‘bmm’ version ‘1.3.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘rtdists’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# brms

<details>

* Version: 2.23.0
* GitHub: https://github.com/paul-buerkner/brms
* Source code: https://github.com/cran/brms
* Date/Publication: 2025-09-09 10:50:02 UTC
* Number of recursive dependencies: 219

Run `revdepcheck::cloud_details(, "brms")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/brms/new/brms.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘brms/DESCRIPTION’ ... OK
...
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘rstan’

Packages suggested but not available for checking: 'shinystan', 'rtdists'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/brms/old/brms.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘brms/DESCRIPTION’ ... OK
...
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘rstan’

Packages suggested but not available for checking: 'shinystan', 'rtdists'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# bullseye

<details>

* Version: 1.0.1
* GitHub: https://github.com/cbhurley/bullseye
* Source code: https://github.com/cran/bullseye
* Date/Publication: 2025-09-15 11:00:02 UTC
* Number of recursive dependencies: 163

Run `revdepcheck::cloud_details(, "bullseye")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/bullseye/new/bullseye.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘bullseye/DESCRIPTION’ ... OK
...
--- failed re-building ‘vis_pairwise.Rmd’

SUMMARY: processing the following files failed:
  ‘calc_pairwise.Rmd’ ‘integrating.Rmd’ ‘vis_pairwise.Rmd’

Error: Vignette re-building failed.
Execution halted

* DONE
Status: 2 ERRORs, 2 NOTEs





```
### CRAN

```
* using log directory ‘/tmp/workdir/bullseye/old/bullseye.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘bullseye/DESCRIPTION’ ... OK
...
--- failed re-building ‘vis_pairwise.Rmd’

SUMMARY: processing the following files failed:
  ‘calc_pairwise.Rmd’ ‘integrating.Rmd’ ‘vis_pairwise.Rmd’

Error: Vignette re-building failed.
Execution halted

* DONE
Status: 2 ERRORs, 2 NOTEs





```
# ClustImpute

<details>

* Version: 0.2.4
* GitHub: NA
* Source code: https://github.com/cran/ClustImpute
* Date/Publication: 2021-05-31 07:40:11 UTC
* Number of recursive dependencies: 115

Run `revdepcheck::cloud_details(, "ClustImpute")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/ClustImpute/new/ClustImpute.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘ClustImpute/DESCRIPTION’ ... OK
...
* this is package ‘ClustImpute’ version ‘0.2.4’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘copula’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/ClustImpute/old/ClustImpute.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘ClustImpute/DESCRIPTION’ ... OK
...
* this is package ‘ClustImpute’ version ‘0.2.4’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘copula’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# copulaSim

<details>

* Version: 0.0.1
* GitHub: https://github.com/psyen0824/copulaSim
* Source code: https://github.com/cran/copulaSim
* Date/Publication: 2022-08-19 12:10:02 UTC
* Number of recursive dependencies: 73

Run `revdepcheck::cloud_details(, "copulaSim")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/copulaSim/new/copulaSim.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘copulaSim/DESCRIPTION’ ... OK
...
--- failed re-building ‘introduction.Rmd’

SUMMARY: processing the following file failed:
  ‘introduction.Rmd’

Error: Vignette re-building failed.
Execution halted

* DONE
Status: 1 ERROR, 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/copulaSim/old/copulaSim.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘copulaSim/DESCRIPTION’ ... OK
...
--- failed re-building ‘introduction.Rmd’

SUMMARY: processing the following file failed:
  ‘introduction.Rmd’

Error: Vignette re-building failed.
Execution halted

* DONE
Status: 1 ERROR, 1 NOTE





```
# cylcop

<details>

* Version: 0.2.1
* GitHub: https://github.com/floo66/cylcop
* Source code: https://github.com/cran/cylcop
* Date/Publication: 2025-10-01 11:10:02 UTC
* Number of recursive dependencies: 95

Run `revdepcheck::cloud_details(, "cylcop")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/cylcop/new/cylcop.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘cylcop/DESCRIPTION’ ... OK
...
* this is package ‘cylcop’ version ‘0.2.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘copula’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/cylcop/old/cylcop.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘cylcop/DESCRIPTION’ ... OK
...
* this is package ‘cylcop’ version ‘0.2.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘copula’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# detourr

<details>

* Version: 0.2.0
* GitHub: https://github.com/casperhart/detourr
* Source code: https://github.com/cran/detourr
* Date/Publication: 2025-08-28 03:00:02 UTC
* Number of recursive dependencies: 110

Run `revdepcheck::cloud_details(, "detourr")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/detourr/new/detourr.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘detourr/DESCRIPTION’ ... OK
...
* this is package ‘detourr’ version ‘0.2.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘tourr’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/detourr/old/detourr.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘detourr/DESCRIPTION’ ... OK
...
* this is package ‘detourr’ version ‘0.2.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘tourr’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# dymo

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/dymo
* Number of recursive dependencies: 39

Run `revdepcheck::cloud_details(, "dymo")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# dySEM

<details>

* Version: 1.4.1
* GitHub: https://github.com/jsakaluk/dySEM
* Source code: https://github.com/cran/dySEM
* Date/Publication: 2025-12-22 20:00:02 UTC
* Number of recursive dependencies: 195

Run `revdepcheck::cloud_details(, "dySEM")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/dySEM/new/dySEM.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘dySEM/DESCRIPTION’ ... OK
...
* this is package ‘dySEM’ version ‘1.4.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'EGAnet', 'semPlot'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/dySEM/old/dySEM.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘dySEM/DESCRIPTION’ ... OK
...
* this is package ‘dySEM’ version ‘1.4.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'EGAnet', 'semPlot'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# ecorisk

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/ecorisk
* Number of recursive dependencies: 145

Run `revdepcheck::cloud_details(, "ecorisk")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# enderecobr

<details>

* Version: 0.5.0
* GitHub: https://github.com/ipeaGIT/enderecobr
* Source code: https://github.com/cran/enderecobr
* Date/Publication: 2026-01-10 07:51:43 UTC
* Number of recursive dependencies: 52

Run `revdepcheck::cloud_details(, "enderecobr")` for more info

</details>

## In both

*   checking whether package ‘enderecobr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/enderecobr/new/enderecobr.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘enderecobr’ ...
** package ‘enderecobr’ successfully unpacked and MD5 sums checked
** using staged installation
Using cargo 1.75.0
Using rustc 1.75.0 (82e1608df 2023-12-21) (built from a source tarball)
Building for CRAN.
Writing `src/Makevars`.
`tools/config.R` has finished.
** libs
using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0’
...
export CARGO_HOME=/tmp/workdir/enderecobr/new/enderecobr.Rcheck/00_pkg_src/enderecobr/src/.cargo && \
export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/tmp/home/.cargo/bin" && \
RUSTFLAGS=" --print=native-static-libs" cargo build -j 2 --offline --lib --release --manifest-path=./rust/Cargo.toml --target-dir ./rust/target 
error: failed to parse lock file at: /tmp/workdir/enderecobr/new/enderecobr.Rcheck/00_pkg_src/enderecobr/src/rust/Cargo.lock

Caused by:
  lock file version 4 requires `-Znext-lockfile-bump`
make: *** [Makevars:26: rust/target/release/libenderecobr.a] Error 101
ERROR: compilation failed for package ‘enderecobr’
* removing ‘/tmp/workdir/enderecobr/new/enderecobr.Rcheck/enderecobr’


```
### CRAN

```
* installing *source* package ‘enderecobr’ ...
** package ‘enderecobr’ successfully unpacked and MD5 sums checked
** using staged installation
Using cargo 1.75.0
Using rustc 1.75.0 (82e1608df 2023-12-21) (built from a source tarball)
Building for CRAN.
Writing `src/Makevars`.
`tools/config.R` has finished.
** libs
using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0’
...
export CARGO_HOME=/tmp/workdir/enderecobr/old/enderecobr.Rcheck/00_pkg_src/enderecobr/src/.cargo && \
export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/tmp/home/.cargo/bin" && \
RUSTFLAGS=" --print=native-static-libs" cargo build -j 2 --offline --lib --release --manifest-path=./rust/Cargo.toml --target-dir ./rust/target 
error: failed to parse lock file at: /tmp/workdir/enderecobr/old/enderecobr.Rcheck/00_pkg_src/enderecobr/src/rust/Cargo.lock

Caused by:
  lock file version 4 requires `-Znext-lockfile-bump`
make: *** [Makevars:26: rust/target/release/libenderecobr.a] Error 101
ERROR: compilation failed for package ‘enderecobr’
* removing ‘/tmp/workdir/enderecobr/old/enderecobr.Rcheck/enderecobr’


```
# eye

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/eye
* Number of recursive dependencies: 59

Run `revdepcheck::cloud_details(, "eye")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# FAfA

<details>

* Version: 1.1
* GitHub: https://github.com/AFarukKILIC/FAfA
* Source code: https://github.com/cran/FAfA
* Date/Publication: 2026-05-03 14:50:07 UTC
* Number of recursive dependencies: 271

Run `revdepcheck::cloud_details(, "FAfA")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/FAfA/new/FAfA.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘FAfA/DESCRIPTION’ ... OK
...
* this is package ‘FAfA’ version ‘1.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'EGAnet', 'MBESS', 'semPlot'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/FAfA/old/FAfA.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘FAfA/DESCRIPTION’ ... OK
...
* this is package ‘FAfA’ version ‘1.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'EGAnet', 'MBESS', 'semPlot'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# fastplyr

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/fastplyr
* Number of recursive dependencies: 42

Run `revdepcheck::cloud_details(, "fastplyr")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# ferrn

<details>

* Version: 0.3.0
* GitHub: https://github.com/huizezhang-sherry/ferrn
* Source code: https://github.com/cran/ferrn
* Date/Publication: 2025-11-20 22:10:10 UTC
* Number of recursive dependencies: 150

Run `revdepcheck::cloud_details(, "ferrn")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/ferrn/new/ferrn.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘ferrn/DESCRIPTION’ ... OK
...
* this is package ‘ferrn’ version ‘0.3.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘tourr’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/ferrn/old/ferrn.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘ferrn/DESCRIPTION’ ... OK
...
* this is package ‘ferrn’ version ‘0.3.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘tourr’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# ffp

<details>

* Version: 0.2.2
* GitHub: https://github.com/Reckziegel/FFP
* Source code: https://github.com/cran/ffp
* Date/Publication: 2022-09-29 15:10:06 UTC
* Number of recursive dependencies: 105

Run `revdepcheck::cloud_details(, "ffp")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/ffp/new/ffp.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘ffp/DESCRIPTION’ ... OK
...
--- failed re-building ‘views.Rmd’

SUMMARY: processing the following file failed:
  ‘views.Rmd’

Error: Vignette re-building failed.
Execution halted

* DONE
Status: 1 ERROR, 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/ffp/old/ffp.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘ffp/DESCRIPTION’ ... OK
...
--- failed re-building ‘views.Rmd’

SUMMARY: processing the following file failed:
  ‘views.Rmd’

Error: Vignette re-building failed.
Execution halted

* DONE
Status: 1 ERROR, 1 NOTE





```
# finnts

<details>

* Version: 0.6.0
* GitHub: https://github.com/microsoft/finnts
* Source code: https://github.com/cran/finnts
* Date/Publication: 2025-09-04 17:00:09 UTC
* Number of recursive dependencies: 247

Run `revdepcheck::cloud_details(, "finnts")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/finnts/new/finnts.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘finnts/DESCRIPTION’ ... OK
...
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/finnts/old/finnts.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘finnts/DESCRIPTION’ ... OK
...
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 NOTE





```
# fio

<details>

* Version: 1.1.0
* GitHub: https://github.com/albersonmiranda/fio
* Source code: https://github.com/cran/fio
* Date/Publication: 2026-06-07 06:10:02 UTC
* Number of recursive dependencies: 88

Run `revdepcheck::cloud_details(, "fio")` for more info

</details>

## In both

*   checking whether package ‘fio’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/fio/new/fio.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘fio’ ...
** package ‘fio’ successfully unpacked and MD5 sums checked
** using staged installation
Error in eval(ei, envir) : 
------------------ [UNSUPPORTED RUST VERSION]------------------
- Minimum supported Rust version is 1.84.
- Installed Rust version is 1.75.0.
---------------------------------------------------------------
Calls: source -> withVisible -> eval -> eval
Execution halted
ERROR: configuration failed for package ‘fio’
* removing ‘/tmp/workdir/fio/new/fio.Rcheck/fio’


```
### CRAN

```
* installing *source* package ‘fio’ ...
** package ‘fio’ successfully unpacked and MD5 sums checked
** using staged installation
Error in eval(ei, envir) : 
------------------ [UNSUPPORTED RUST VERSION]------------------
- Minimum supported Rust version is 1.84.
- Installed Rust version is 1.75.0.
---------------------------------------------------------------
Calls: source -> withVisible -> eval -> eval
Execution halted
ERROR: configuration failed for package ‘fio’
* removing ‘/tmp/workdir/fio/old/fio.Rcheck/fio’


```
# genekitr

<details>

* Version: 1.2.8
* GitHub: https://github.com/GangLiLab/genekitr
* Source code: https://github.com/cran/genekitr
* Date/Publication: 2024-09-06 13:00:06 UTC
* Number of recursive dependencies: 202

Run `revdepcheck::cloud_details(, "genekitr")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/genekitr/new/genekitr.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘genekitr/DESCRIPTION’ ... OK
...
* this is package ‘genekitr’ version ‘1.2.8’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘clusterProfiler’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/genekitr/old/genekitr.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘genekitr/DESCRIPTION’ ... OK
...
* this is package ‘genekitr’ version ‘1.2.8’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘clusterProfiler’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# ggsem

<details>

* Version: 1.0.0.1
* GitHub: https://github.com/smin95/ggsem
* Source code: https://github.com/cran/ggsem
* Date/Publication: 2026-06-02 02:30:02 UTC
* Number of recursive dependencies: 228

Run `revdepcheck::cloud_details(, "ggsem")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/ggsem/new/ggsem.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘ggsem/DESCRIPTION’ ... OK
...
* this is package ‘ggsem’ version ‘1.0.0.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘semPlot’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/ggsem/old/ggsem.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘ggsem/DESCRIPTION’ ... OK
...
* this is package ‘ggsem’ version ‘1.0.0.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘semPlot’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# ggsql

<details>

* Version: 0.3.3
* GitHub: https://github.com/posit-dev/ggsql-r
* Source code: https://github.com/cran/ggsql
* Date/Publication: 2026-06-03 08:20:23 UTC
* Number of recursive dependencies: 69

Run `revdepcheck::cloud_details(, "ggsql")` for more info

</details>

## In both

*   checking whether package ‘ggsql’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/ggsql/new/ggsql.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘ggsql’ ...
** package ‘ggsql’ successfully unpacked and MD5 sums checked
** using staged installation
2026-06-18 09:33:50 URL:https://release-assets.githubusercontent.com/github-production-release-asset/1213346768/5a7c0166-a4e7-4312-87c2-4ba66d673298?sp=r&sv=2018-11-09&sr=b&spr=https&se=2026-06-18T10%3A28%3A35Z&rscd=attachment%3B+filename%3Dvendor.tar.xz.sha256&rsct=application%2Foctet-stream&skoid=96c2d410-5711-43a1-aedd-ab1947aa7ab0&sktid=398a6654-997b-47e9-b12b-9515b896b4de&skt=2026-06-18T09%3A28%3A01Z&ske=2026-06-18T10%3A28%3A35Z&sks=b&skv=2018-11-09&sig=Dj9zCoGuXuSTX0mUjwwecMsPwUEDdQb2qRiTLyQbt2c%3D&jwt=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJnaXRodWIuY29tIiwiYXVkIjoicmVsZWFzZS1hc3NldHMuZ2l0aHVidXNlcmNvbnRlbnQuY29tIiwia2V5Ijoia2V5MSIsImV4cCI6MTc4MTc3NTUzMCwibmJmIjoxNzgxNzc1MjMwLCJwYXRoIjoicmVsZWFzZWFzc2V0cHJvZHVjdGlvbi5ibG9iLmNvcmUud2luZG93cy5uZXQifQ.0-qLAHfqoXT0_Ymob0Pd_3Y9sZEA7xIsFSPLlDgdiSU&response-content-disposition=attachment%3B%20filename%3Dvendor.tar.xz.sha256&response-content-type=application%2Foctet-stream [65/65] -> "/tmp/RtmpsxvsvV/file9aa7bd4ec6b.sha256" [1]
Downloading vendor archive from https://github.com/posit-dev/ggsql-r/releases/download/v0.3.3/vendor.tar.xz
2026-06-18 09:33:50 URL:https://release-assets.githubusercontent.com/github-production-release-asset/1213346768/95bf3db6-701d-4f73-8242-343e76c4fb40?sp=r&sv=2018-11-09&sr=b&spr=https&se=2026-06-18T10%3A29%3A26Z&rscd=attachment%3B+filename%3Dvendor.tar.xz&rsct=application%2Foctet-stream&skoid=96c2d410-5711-43a1-aedd-ab1947aa7ab0&sktid=398a6654-997b-47e9-b12b-9515b896b4de&skt=2026-06-18T09%3A29%3A22Z&ske=2026-06-18T10%3A29%3A26Z&sks=b&skv=2018-11-09&sig=NxxGrdowGF1p78eal%2BrwtWIeM6V77fKLDh%2BRppKbrKU%3D&jwt=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJnaXRodWIuY29tIiwiYXVkIjoicmVsZWFzZS1hc3NldHMuZ2l0aHVidXNlcmNvbnRlbnQuY29tIiwia2V5Ijoia2V5MSIsImV4cCI6MTc4MTc3NzAzMCwibmJmIjoxNzgxNzc1MjMwLCJwYXRoIjoicmVsZWFzZWFzc2V0cHJvZHVjdGlvbi5ibG9iLmNvcmUud2luZG93cy5uZXQifQ.3IL4sH-W5x4e4RbTxyDrczdMEWs4E5Q9v9Dw34olVYk&response-content-disposition=attachment%3B%20filename%3Dvendor.tar.xz&response-content-type=application%2Foctet-stream [38545072/38545072] -> "/tmp/RtmpsxvsvV/file9aa20d3e031.tar.xz" [1]
Error: 'sha256sum' is not an exported object from 'namespace:tools'
Execution halted
ERROR: configuration failed for package ‘ggsql’
* removing ‘/tmp/workdir/ggsql/new/ggsql.Rcheck/ggsql’


```
### CRAN

```
* installing *source* package ‘ggsql’ ...
** package ‘ggsql’ successfully unpacked and MD5 sums checked
** using staged installation
2026-06-18 09:33:27 URL:https://release-assets.githubusercontent.com/github-production-release-asset/1213346768/5a7c0166-a4e7-4312-87c2-4ba66d673298?sp=r&sv=2018-11-09&sr=b&spr=https&se=2026-06-18T10%3A32%3A12Z&rscd=attachment%3B+filename%3Dvendor.tar.xz.sha256&rsct=application%2Foctet-stream&skoid=96c2d410-5711-43a1-aedd-ab1947aa7ab0&sktid=398a6654-997b-47e9-b12b-9515b896b4de&skt=2026-06-18T09%3A31%3A32Z&ske=2026-06-18T10%3A32%3A12Z&sks=b&skv=2018-11-09&sig=d6AXc00G5X1jk99OI%2FtWd9F2kS0K5189e5GlgBW77uM%3D&jwt=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJnaXRodWIuY29tIiwiYXVkIjoicmVsZWFzZS1hc3NldHMuZ2l0aHVidXNlcmNvbnRlbnQuY29tIiwia2V5Ijoia2V5MSIsImV4cCI6MTc4MTc3NTUwNywibmJmIjoxNzgxNzc1MjA3LCJwYXRoIjoicmVsZWFzZWFzc2V0cHJvZHVjdGlvbi5ibG9iLmNvcmUud2luZG93cy5uZXQifQ.RhkfufVGLECe9ubldxHnDEBAyOx9GGEH-IQyorOXRHg&response-content-disposition=attachment%3B%20filename%3Dvendor.tar.xz.sha256&response-content-type=application%2Foctet-stream [65/65] -> "/tmp/RtmpWEH67L/file7be1c510d4e.sha256" [1]
Downloading vendor archive from https://github.com/posit-dev/ggsql-r/releases/download/v0.3.3/vendor.tar.xz
2026-06-18 09:33:27 URL:https://release-assets.githubusercontent.com/github-production-release-asset/1213346768/95bf3db6-701d-4f73-8242-343e76c4fb40?sp=r&sv=2018-11-09&sr=b&spr=https&se=2026-06-18T10%3A29%3A45Z&rscd=attachment%3B+filename%3Dvendor.tar.xz&rsct=application%2Foctet-stream&skoid=96c2d410-5711-43a1-aedd-ab1947aa7ab0&sktid=398a6654-997b-47e9-b12b-9515b896b4de&skt=2026-06-18T09%3A29%3A15Z&ske=2026-06-18T10%3A29%3A45Z&sks=b&skv=2018-11-09&sig=tqM2zPx9P58602RhczUk4YHfzXINpuCWkC6btsoImwY%3D&jwt=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJnaXRodWIuY29tIiwiYXVkIjoicmVsZWFzZS1hc3NldHMuZ2l0aHVidXNlcmNvbnRlbnQuY29tIiwia2V5Ijoia2V5MSIsImV4cCI6MTc4MTc3NzAwNywibmJmIjoxNzgxNzc1MjA3LCJwYXRoIjoicmVsZWFzZWFzc2V0cHJvZHVjdGlvbi5ibG9iLmNvcmUud2luZG93cy5uZXQifQ.1xSaXTKKj3OOFNXseb37U6SeTJlfZmgmvxK_Ejsj9IM&response-content-disposition=attachment%3B%20filename%3Dvendor.tar.xz&response-content-type=application%2Foctet-stream [38545072/38545072] -> "/tmp/RtmpWEH67L/file7be3f7d2fa3.tar.xz" [1]
Error: 'sha256sum' is not an exported object from 'namespace:tools'
Execution halted
ERROR: configuration failed for package ‘ggsql’
* removing ‘/tmp/workdir/ggsql/old/ggsql.Rcheck/ggsql’


```
# gMOIP

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/gMOIP
* Number of recursive dependencies: 108

Run `revdepcheck::cloud_details(, "gMOIP")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# grandR

<details>

* Version: 0.2.7
* GitHub: https://github.com/erhard-lab/grandR
* Source code: https://github.com/cran/grandR
* Date/Publication: 2026-01-14 11:10:02 UTC
* Number of recursive dependencies: 250

Run `revdepcheck::cloud_details(, "grandR")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/grandR/new/grandR.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘grandR/DESCRIPTION’ ... OK
...
* checking line endings in C/C++/Fortran sources/headers ... OK
* checking compiled code ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 2 NOTEs





```
### CRAN

```
* using log directory ‘/tmp/workdir/grandR/old/grandR.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘grandR/DESCRIPTION’ ... OK
...
* checking line endings in C/C++/Fortran sources/headers ... OK
* checking compiled code ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 2 NOTEs





```
# gratia

<details>

* Version: 0.11.2
* GitHub: https://github.com/gavinsimpson/gratia
* Source code: https://github.com/cran/gratia
* Date/Publication: 2026-02-07 06:10:36 UTC
* Number of recursive dependencies: 178

Run `revdepcheck::cloud_details(, "gratia")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/gratia/new/gratia.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘gratia/DESCRIPTION’ ... OK
...
> detaching 'package:mgcv', 'package:GJRM'
> 
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘test-all.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 2 NOTEs





```
### CRAN

```
* using log directory ‘/tmp/workdir/gratia/old/gratia.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘gratia/DESCRIPTION’ ... OK
...
> detaching 'package:mgcv', 'package:GJRM'
> 
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘test-all.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 2 NOTEs





```
# gvcR

<details>

* Version: 0.4.0
* GitHub: https://github.com/myaseen208/gvcR
* Source code: https://github.com/cran/gvcR
* Date/Publication: 2024-10-01 06:50:02 UTC
* Number of recursive dependencies: 142

Run `revdepcheck::cloud_details(, "gvcR")` for more info

</details>

## In both

*   checking whether package ‘gvcR’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/gvcR/new/gvcR.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘gvcR’ ...
** package ‘gvcR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘HRW’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘gvcR’
* removing ‘/tmp/workdir/gvcR/new/gvcR.Rcheck/gvcR’


```
### CRAN

```
* installing *source* package ‘gvcR’ ...
** package ‘gvcR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘HRW’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘gvcR’
* removing ‘/tmp/workdir/gvcR/old/gvcR.Rcheck/gvcR’


```
# h3o

<details>

* Version: 0.3.0
* GitHub: https://github.com/extendr/h3o
* Source code: https://github.com/cran/h3o
* Date/Publication: 2025-08-29 11:00:02 UTC
* Number of recursive dependencies: 17

Run `revdepcheck::cloud_details(, "h3o")` for more info

</details>

## In both

*   checking whether package ‘h3o’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/h3o/new/h3o.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘h3o’ ...
** package ‘h3o’ successfully unpacked and MD5 sums checked
** using staged installation
Using cargo 1.75.0
Using rustc 1.75.0 (82e1608df 2023-12-21) (built from a source tarball)
Building for CRAN.
Writing `src/Makevars`.
`tools/config.R` has finished.
** libs
using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0’
...
export CARGO_HOME=/tmp/workdir/h3o/new/h3o.Rcheck/00_pkg_src/h3o/src/.cargo && \
export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/tmp/home/.cargo/bin" && \
RUSTFLAGS=" --print=native-static-libs" cargo build -j 2 --offline --lib --release --manifest-path=./rust/Cargo.toml --target-dir ./rust/target 
error: failed to parse lock file at: /tmp/workdir/h3o/new/h3o.Rcheck/00_pkg_src/h3o/src/rust/Cargo.lock

Caused by:
  lock file version 4 requires `-Znext-lockfile-bump`
make: *** [Makevars:26: rust/target/release/libh3o.a] Error 101
ERROR: compilation failed for package ‘h3o’
* removing ‘/tmp/workdir/h3o/new/h3o.Rcheck/h3o’


```
### CRAN

```
* installing *source* package ‘h3o’ ...
** package ‘h3o’ successfully unpacked and MD5 sums checked
** using staged installation
Using cargo 1.75.0
Using rustc 1.75.0 (82e1608df 2023-12-21) (built from a source tarball)
Building for CRAN.
Writing `src/Makevars`.
`tools/config.R` has finished.
** libs
using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0’
...
export CARGO_HOME=/tmp/workdir/h3o/old/h3o.Rcheck/00_pkg_src/h3o/src/.cargo && \
export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/tmp/home/.cargo/bin" && \
RUSTFLAGS=" --print=native-static-libs" cargo build -j 2 --offline --lib --release --manifest-path=./rust/Cargo.toml --target-dir ./rust/target 
error: failed to parse lock file at: /tmp/workdir/h3o/old/h3o.Rcheck/00_pkg_src/h3o/src/rust/Cargo.lock

Caused by:
  lock file version 4 requires `-Znext-lockfile-bump`
make: *** [Makevars:26: rust/target/release/libh3o.a] Error 101
ERROR: compilation failed for package ‘h3o’
* removing ‘/tmp/workdir/h3o/old/h3o.Rcheck/h3o’


```
# idealstan

<details>

* Version: 1.0
* GitHub: https://github.com/saudiwin/idealstan
* Source code: https://github.com/cran/idealstan
* Date/Publication: 2026-05-12 19:20:18 UTC
* Number of recursive dependencies: 148

Run `revdepcheck::cloud_details(, "idealstan")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/idealstan/new/idealstan.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘idealstan/DESCRIPTION’ ... OK
...
* checking LazyData ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 2 NOTEs





```
### CRAN

```
* using log directory ‘/tmp/workdir/idealstan/old/idealstan.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘idealstan/DESCRIPTION’ ... OK
...
* checking LazyData ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 2 NOTEs





```
# IOBR

<details>

* Version: 2.2.3
* GitHub: https://github.com/IOBR/IOBR
* Source code: https://github.com/cran/IOBR
* Date/Publication: 2026-05-30 15:30:02 UTC
* Number of recursive dependencies: 426

Run `revdepcheck::cloud_details(, "IOBR")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/IOBR/new/IOBR.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘IOBR/DESCRIPTION’ ... OK
...
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘GSVA’

Packages suggested but not available for checking: 'DESeq2', 'easier'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/IOBR/old/IOBR.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘IOBR/DESCRIPTION’ ... OK
...
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘GSVA’

Packages suggested but not available for checking: 'DESeq2', 'easier'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# ivolcano

<details>

* Version: 0.0.6
* GitHub: https://github.com/YuLab-SMU/ivolcano
* Source code: https://github.com/cran/ivolcano
* Date/Publication: 2026-06-05 06:50:02 UTC
* Number of recursive dependencies: 140

Run `revdepcheck::cloud_details(, "ivolcano")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/ivolcano/new/ivolcano.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘ivolcano/DESCRIPTION’ ... OK
...
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 2 NOTEs





```
### CRAN

```
* using log directory ‘/tmp/workdir/ivolcano/old/ivolcano.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘ivolcano/DESCRIPTION’ ... OK
...
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 2 NOTEs





```
# lcsm

<details>

* Version: 0.3.2
* GitHub: https://github.com/milanwiedemann/lcsm
* Source code: https://github.com/cran/lcsm
* Date/Publication: 2023-02-25 23:40:02 UTC
* Number of recursive dependencies: 137

Run `revdepcheck::cloud_details(, "lcsm")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/lcsm/new/lcsm.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘lcsm/DESCRIPTION’ ... OK
...
* this is package ‘lcsm’ version ‘0.3.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘semPlot’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/lcsm/old/lcsm.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘lcsm/DESCRIPTION’ ... OK
...
* this is package ‘lcsm’ version ‘0.3.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘semPlot’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# LMMstar

<details>

* Version: 1.1.0
* GitHub: https://github.com/bozenne/LMMstar
* Source code: https://github.com/cran/LMMstar
* Date/Publication: 2024-05-12 21:43:11 UTC
* Number of recursive dependencies: 171

Run `revdepcheck::cloud_details(, "LMMstar")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/LMMstar/new/LMMstar.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘LMMstar/DESCRIPTION’ ... OK
...
* this is package ‘LMMstar’ version ‘1.1.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘copula’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/LMMstar/old/LMMstar.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘LMMstar/DESCRIPTION’ ... OK
...
* this is package ‘LMMstar’ version ‘1.1.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘copula’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# manydist

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/manydist
* Number of recursive dependencies: 190

Run `revdepcheck::cloud_details(, "manydist")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# marginaleffects

<details>

* Version: 0.32.0
* GitHub: https://github.com/vincentarelbundock/marginaleffects
* Source code: https://github.com/cran/marginaleffects
* Date/Publication: 2026-02-14 06:40:08 UTC
* Number of recursive dependencies: 489

Run `revdepcheck::cloud_details(, "marginaleffects")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/marginaleffects/new/marginaleffects.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘marginaleffects/DESCRIPTION’ ... OK
...
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘spelling.R’
  Running ‘tinytest.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/marginaleffects/old/marginaleffects.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘marginaleffects/DESCRIPTION’ ... OK
...
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘spelling.R’
  Running ‘tinytest.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 NOTE





```
# measureR

<details>

* Version: 0.0.3
* GitHub: https://github.com/hdmeasure/measureR
* Source code: https://github.com/cran/measureR
* Date/Publication: 2026-05-15 08:40:02 UTC
* Number of recursive dependencies: 234

Run `revdepcheck::cloud_details(, "measureR")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/measureR/new/measureR.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘measureR/DESCRIPTION’ ... OK
...
* this is package ‘measureR’ version ‘0.0.3’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'semPlot', 'semptools'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/measureR/old/measureR.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘measureR/DESCRIPTION’ ... OK
...
* this is package ‘measureR’ version ‘0.0.3’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'semPlot', 'semptools'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# metrica

<details>

* Version: 2.1.1
* GitHub: https://github.com/adriancorrendo/metrica
* Source code: https://github.com/cran/metrica
* Date/Publication: 2026-03-18 06:12:46 UTC
* Number of recursive dependencies: 86

Run `revdepcheck::cloud_details(, "metrica")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/metrica/new/metrica.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘metrica/DESCRIPTION’ ... OK
...
* this is package ‘metrica’ version ‘2.1.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘energy’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/metrica/old/metrica.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘metrica/DESCRIPTION’ ... OK
...
* this is package ‘metrica’ version ‘2.1.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘energy’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# mineSweepR

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/mineSweepR
* Date/Publication: 2023-12-02 11:50:10 UTC
* Number of recursive dependencies: 98

Run `revdepcheck::cloud_details(, "mineSweepR")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/mineSweepR/new/mineSweepR.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘mineSweepR/DESCRIPTION’ ... OK
...
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking examples ... OK
* DONE
Status: OK





```
### CRAN

```
* using log directory ‘/tmp/workdir/mineSweepR/old/mineSweepR.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘mineSweepR/DESCRIPTION’ ... OK
...
* checking Rd metadata ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking examples ... OK
* DONE
Status: OK





```
# MiscMetabar

<details>

* Version: 0.16.8
* GitHub: https://github.com/adrientaudiere/MiscMetabar
* Source code: https://github.com/cran/MiscMetabar
* Date/Publication: 2026-06-08 14:30:02 UTC
* Number of recursive dependencies: 470

Run `revdepcheck::cloud_details(, "MiscMetabar")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/MiscMetabar/new/MiscMetabar.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘MiscMetabar/DESCRIPTION’ ... OK
...
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘spelling.R’
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 2 NOTEs





```
### CRAN

```
* using log directory ‘/tmp/workdir/MiscMetabar/old/MiscMetabar.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘MiscMetabar/DESCRIPTION’ ... OK
...
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘spelling.R’
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 2 NOTEs





```
# MixMashNet

<details>

* Version: 1.1.0
* GitHub: https://github.com/ARCbiostat/MixMashNet
* Source code: https://github.com/cran/MixMashNet
* Date/Publication: 2026-06-16 20:40:02 UTC
* Number of recursive dependencies: 201

Run `revdepcheck::cloud_details(, "MixMashNet")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/MixMashNet/new/MixMashNet.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘MixMashNet/DESCRIPTION’ ... OK
...
* this is package ‘MixMashNet’ version ‘1.1.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘EGAnet’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/MixMashNet/old/MixMashNet.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘MixMashNet/DESCRIPTION’ ... OK
...
* this is package ‘MixMashNet’ version ‘1.1.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘EGAnet’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# morepls

<details>

* Version: 0.2.1
* GitHub: NA
* Source code: https://github.com/cran/morepls
* Date/Publication: 2025-05-29 10:10:02 UTC
* Number of recursive dependencies: 40

Run `revdepcheck::cloud_details(, "morepls")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/morepls/new/morepls.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘morepls/DESCRIPTION’ ... OK
...
+            ncomp = 5,
+            data = yarn,
+            validation = "CV",
+            method = "oscorespls")
> plo_vip(pls)
Error in plo_vip(pls) : 
  plsVarSel package should be installed to use this type of plot
Execution halted
* DONE
Status: 1 ERROR, 2 NOTEs





```
### CRAN

```
* using log directory ‘/tmp/workdir/morepls/old/morepls.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘morepls/DESCRIPTION’ ... OK
...
+            ncomp = 5,
+            data = yarn,
+            validation = "CV",
+            method = "oscorespls")
> plo_vip(pls)
Error in plo_vip(pls) : 
  plsVarSel package should be installed to use this type of plot
Execution halted
* DONE
Status: 1 ERROR, 2 NOTEs





```
# multinma

<details>

* Version: 0.9.1
* GitHub: https://github.com/dmphillippo/multinma
* Source code: https://github.com/cran/multinma
* Date/Publication: 2026-04-16 23:10:09 UTC
* Number of recursive dependencies: 153

Run `revdepcheck::cloud_details(, "multinma")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/multinma/new/multinma.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘multinma/DESCRIPTION’ ... OK
...
* this is package ‘multinma’ version ‘0.9.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘copula’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/multinma/old/multinma.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘multinma/DESCRIPTION’ ... OK
...
* this is package ‘multinma’ version ‘0.9.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘copula’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# mvalpha

<details>

* Version: 0.6.0
* GitHub: https://github.com/therealcfdrake/mvalpha
* Source code: https://github.com/cran/mvalpha
* Date/Publication: 2026-06-16 02:50:02 UTC
* Number of recursive dependencies: 32

Run `revdepcheck::cloud_details(, "mvalpha")` for more info

</details>

## In both

*   checking whether package ‘mvalpha’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/mvalpha/new/mvalpha.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘mvalpha’ ...
** package ‘mvalpha’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0’
g++ -std=gnu++17 -I"/opt/R/4.4.0/lib/R/include" -DNDEBUG  -I'/tmp/r-deps/Rcpp/include' -I/usr/local/include   -fopenmp -fpic  -g -O2   -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.4.0/lib/R/include" -DNDEBUG  -I'/tmp/r-deps/Rcpp/include' -I/usr/local/include   -fopenmp -fpic  -g -O2   -c mvalpha_internal_functions.cpp -o mvalpha_internal_functions.o
g++ -std=gnu++17 -shared -L/opt/R/4.4.0/lib/R/lib -L/usr/local/lib -o mvalpha.so RcppExports.o mvalpha_internal_functions.o -fopenmp -lomp -L/opt/R/4.4.0/lib/R/lib -lR
/usr/bin/ld: cannot find -lomp: No such file or directory
collect2: error: ld returned 1 exit status
make: *** [/opt/R/4.4.0/lib/R/share/make/shlib.mk:10: mvalpha.so] Error 1
ERROR: compilation failed for package ‘mvalpha’
* removing ‘/tmp/workdir/mvalpha/new/mvalpha.Rcheck/mvalpha’


```
### CRAN

```
* installing *source* package ‘mvalpha’ ...
** package ‘mvalpha’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0’
g++ -std=gnu++17 -I"/opt/R/4.4.0/lib/R/include" -DNDEBUG  -I'/tmp/r-deps/Rcpp/include' -I/usr/local/include   -fopenmp -fpic  -g -O2   -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.4.0/lib/R/include" -DNDEBUG  -I'/tmp/r-deps/Rcpp/include' -I/usr/local/include   -fopenmp -fpic  -g -O2   -c mvalpha_internal_functions.cpp -o mvalpha_internal_functions.o
g++ -std=gnu++17 -shared -L/opt/R/4.4.0/lib/R/lib -L/usr/local/lib -o mvalpha.so RcppExports.o mvalpha_internal_functions.o -fopenmp -lomp -L/opt/R/4.4.0/lib/R/lib -lR
/usr/bin/ld: cannot find -lomp: No such file or directory
collect2: error: ld returned 1 exit status
make: *** [/opt/R/4.4.0/lib/R/share/make/shlib.mk:10: mvalpha.so] Error 1
ERROR: compilation failed for package ‘mvalpha’
* removing ‘/tmp/workdir/mvalpha/old/mvalpha.Rcheck/mvalpha’


```
# OlinkAnalyze

<details>

* Version: 5.0.0
* GitHub: https://github.com/Olink-Proteomics/OlinkRPackage
* Source code: https://github.com/cran/OlinkAnalyze
* Date/Publication: 2026-03-28 12:00:02 UTC
* Number of recursive dependencies: 234

Run `revdepcheck::cloud_details(, "OlinkAnalyze")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/OlinkAnalyze/new/OlinkAnalyze.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘OlinkAnalyze/DESCRIPTION’ ... OK
...
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/OlinkAnalyze/old/OlinkAnalyze.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘OlinkAnalyze/DESCRIPTION’ ... OK
...
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 NOTE





```
# omicsTools

<details>

* Version: 1.1.7
* GitHub: NA
* Source code: https://github.com/cran/omicsTools
* Date/Publication: 2025-12-16 15:40:02 UTC
* Number of recursive dependencies: 230

Run `revdepcheck::cloud_details(, "omicsTools")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/omicsTools/new/omicsTools.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘omicsTools/DESCRIPTION’ ... OK
...
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/omicsTools/old/omicsTools.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘omicsTools/DESCRIPTION’ ... OK
...
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 NOTE





```
# outstandR

<details>

* Version: 2.0.0
* GitHub: https://github.com/StatisticsHealthEconomics/outstandR
* Source code: https://github.com/cran/outstandR
* Date/Publication: 2026-06-10 08:40:08 UTC
* Number of recursive dependencies: 160

Run `revdepcheck::cloud_details(, "outstandR")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/outstandR/new/outstandR.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘outstandR/DESCRIPTION’ ... OK
...
* this is package ‘outstandR’ version ‘2.0.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘copula’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/outstandR/old/outstandR.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘outstandR/DESCRIPTION’ ... OK
...
* this is package ‘outstandR’ version ‘2.0.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘copula’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# pandemonium

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/pandemonium
* Date/Publication: 2026-05-21 04:20:02 UTC
* Number of recursive dependencies: 217

Run `revdepcheck::cloud_details(, "pandemonium")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/pandemonium/new/pandemonium.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘pandemonium/DESCRIPTION’ ... OK
...
* this is package ‘pandemonium’ version ‘1.0.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘tourr’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/pandemonium/old/pandemonium.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘pandemonium/DESCRIPTION’ ... OK
...
* this is package ‘pandemonium’ version ‘1.0.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘tourr’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# prefviz

<details>

* Version: 0.1.2
* GitHub: https://github.com/numbats/prefviz
* Source code: https://github.com/cran/prefviz
* Date/Publication: 2026-05-01 11:00:08 UTC
* Number of recursive dependencies: 111

Run `revdepcheck::cloud_details(, "prefviz")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/prefviz/new/prefviz.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘prefviz/DESCRIPTION’ ... OK
...
--- finished re-building ‘transform_raw_data.Rmd’

SUMMARY: processing the following files failed:
  ‘add_ordered_path.Rmd’ ‘draw_ternary_plot.Rmd’

Error: Vignette re-building failed.
Execution halted

* DONE
Status: 1 ERROR, 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/prefviz/old/prefviz.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘prefviz/DESCRIPTION’ ... OK
...
--- finished re-building ‘transform_raw_data.Rmd’

SUMMARY: processing the following files failed:
  ‘add_ordered_path.Rmd’ ‘draw_ternary_plot.Rmd’

Error: Vignette re-building failed.
Execution halted

* DONE
Status: 1 ERROR, 1 NOTE





```
# projectLSA

<details>

* Version: 0.0.9
* GitHub: https://github.com/hdmeasure/projectLSA
* Source code: https://github.com/cran/projectLSA
* Date/Publication: 2026-05-09 11:30:02 UTC
* Number of recursive dependencies: 263

Run `revdepcheck::cloud_details(, "projectLSA")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/projectLSA/new/projectLSA.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘projectLSA/DESCRIPTION’ ... OK
...
* this is package ‘projectLSA’ version ‘0.0.9’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'semPlot', 'semptools'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/projectLSA/old/projectLSA.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘projectLSA/DESCRIPTION’ ... OK
...
* this is package ‘projectLSA’ version ‘0.0.9’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'semPlot', 'semptools'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# projpred

<details>

* Version: 2.10.0
* GitHub: https://github.com/stan-dev/projpred
* Source code: https://github.com/cran/projpred
* Date/Publication: 2025-12-06 13:30:02 UTC
* Number of recursive dependencies: 163

Run `revdepcheck::cloud_details(, "projpred")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/projpred/new/projpred.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘projpred/DESCRIPTION’ ... OK
...
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 2 NOTEs





```
### CRAN

```
* using log directory ‘/tmp/workdir/projpred/old/projpred.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘projpred/DESCRIPTION’ ... OK
...
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 2 NOTEs





```
# psychonetrics

<details>

* Version: 0.15
* GitHub: https://github.com/SachaEpskamp/psychonetrics
* Source code: https://github.com/cran/psychonetrics
* Date/Publication: 2026-02-27 06:40:02 UTC
* Number of recursive dependencies: 137

Run `revdepcheck::cloud_details(, "psychonetrics")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/psychonetrics/new/psychonetrics.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘psychonetrics/DESCRIPTION’ ... OK
...
* checking line endings in C/C++/Fortran sources/headers ... OK
* checking line endings in Makefiles ... OK
* checking compilation flags in Makevars ... OK
* checking for GNU extensions in Makefiles ... OK
* checking for portable use of $(BLAS_LIBS) and $(LAPACK_LIBS) ... OK
* checking use of PKG_*FLAGS in Makefiles ... OK
* checking compiled code ... OK
* checking examples ... OK
* DONE
Status: 2 NOTEs





```
### CRAN

```
* using log directory ‘/tmp/workdir/psychonetrics/old/psychonetrics.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘psychonetrics/DESCRIPTION’ ... OK
...
* checking line endings in C/C++/Fortran sources/headers ... OK
* checking line endings in Makefiles ... OK
* checking compilation flags in Makevars ... OK
* checking for GNU extensions in Makefiles ... OK
* checking for portable use of $(BLAS_LIBS) and $(LAPACK_LIBS) ... OK
* checking use of PKG_*FLAGS in Makefiles ... OK
* checking compiled code ... OK
* checking examples ... OK
* DONE
Status: 2 NOTEs





```
# RegDDM

<details>

* Version: 1.1
* GitHub: https://github.com/biorabbit/RegDDM
* Source code: https://github.com/cran/RegDDM
* Date/Publication: 2025-07-01 18:40:02 UTC
* Number of recursive dependencies: 73

Run `revdepcheck::cloud_details(, "RegDDM")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/RegDDM/new/RegDDM.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘RegDDM/DESCRIPTION’ ... OK
...
* this is package ‘RegDDM’ version ‘1.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘rtdists’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/RegDDM/old/RegDDM.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘RegDDM/DESCRIPTION’ ... OK
...
* this is package ‘RegDDM’ version ‘1.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘rtdists’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# RGraphSpace

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/RGraphSpace
* Number of recursive dependencies: 169

Run `revdepcheck::cloud_details(, "RGraphSpace")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# rollout

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/rollout
* Number of recursive dependencies: 74

Run `revdepcheck::cloud_details(, "rollout")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# SCpubr

<details>

* Version: 3.0.1
* GitHub: https://github.com/enblacar/SCpubr
* Source code: https://github.com/cran/SCpubr
* Date/Publication: 2026-01-09 15:10:18 UTC
* Number of recursive dependencies: 291

Run `revdepcheck::cloud_details(, "SCpubr")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/SCpubr/new/SCpubr.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘SCpubr/DESCRIPTION’ ... OK
...
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/SCpubr/old/SCpubr.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘SCpubr/DESCRIPTION’ ... OK
...
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 NOTE





```
# semptools

<details>

* Version: 0.3.3
* GitHub: https://github.com/sfcheung/semptools
* Source code: https://github.com/cran/semptools
* Date/Publication: 2026-03-17 12:20:02 UTC
* Number of recursive dependencies: 122

Run `revdepcheck::cloud_details(, "semptools")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/semptools/new/semptools.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘semptools/DESCRIPTION’ ... OK
...
* this is package ‘semptools’ version ‘0.3.3’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘semPlot’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/semptools/old/semptools.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘semptools/DESCRIPTION’ ... OK
...
* this is package ‘semptools’ version ‘0.3.3’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘semPlot’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# shinybrms

<details>

* Version: 1.8.1
* GitHub: https://github.com/fweber144/shinybrms
* Source code: https://github.com/cran/shinybrms
* Date/Publication: 2025-12-01 22:30:02 UTC
* Number of recursive dependencies: 149

Run `revdepcheck::cloud_details(, "shinybrms")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/shinybrms/new/shinybrms.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘shinybrms/DESCRIPTION’ ... OK
...
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘rstan’

Packages suggested but not available for checking: 'shinystan', 'rstanarm'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/shinybrms/old/shinybrms.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘shinybrms/DESCRIPTION’ ... OK
...
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘rstan’

Packages suggested but not available for checking: 'shinystan', 'rstanarm'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# Signac

<details>

* Version: 1.17.1
* GitHub: https://github.com/stuart-lab/signac
* Source code: https://github.com/cran/Signac
* Date/Publication: 2026-04-06 18:10:07 UTC
* Number of recursive dependencies: 227

Run `revdepcheck::cloud_details(, "Signac")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/Signac/new/Signac.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘Signac/DESCRIPTION’ ... OK
...
* checking for GNU extensions in Makefiles ... OK
* checking for portable use of $(BLAS_LIBS) and $(LAPACK_LIBS) ... OK
* checking use of PKG_*FLAGS in Makefiles ... OK
* checking compiled code ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* DONE
Status: 3 NOTEs





```
### CRAN

```
* using log directory ‘/tmp/workdir/Signac/old/Signac.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘Signac/DESCRIPTION’ ... OK
...
* checking for GNU extensions in Makefiles ... OK
* checking for portable use of $(BLAS_LIBS) and $(LAPACK_LIBS) ... OK
* checking use of PKG_*FLAGS in Makefiles ... OK
* checking compiled code ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* DONE
Status: 3 NOTEs





```
# SISIR

<details>

* Version: 0.2.4
* GitHub: NA
* Source code: https://github.com/cran/SISIR
* Date/Publication: 2026-05-14 17:10:02 UTC
* Number of recursive dependencies: 116

Run `revdepcheck::cloud_details(, "SISIR")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/SISIR/new/SISIR.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘SISIR/DESCRIPTION’ ... OK
...
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* DONE
Status: 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/SISIR/old/SISIR.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘SISIR/DESCRIPTION’ ... OK
...
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* DONE
Status: 1 NOTE





```
# spareg

<details>

* Version: 1.1.1
* GitHub: https://github.com/lauravana/spareg
* Source code: https://github.com/cran/spareg
* Date/Publication: 2025-08-19 08:30:02 UTC
* Number of recursive dependencies: 128

Run `revdepcheck::cloud_details(, "spareg")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/spareg/new/spareg.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘spareg/DESCRIPTION’ ... OK
...
--- failed re-building 'spareg.Rnw'

SUMMARY: processing the following file failed:
  'spareg.Rnw'

Error: Vignette re-building failed.
Execution halted

* DONE
Status: 2 ERRORs





```
### CRAN

```
* using log directory ‘/tmp/workdir/spareg/old/spareg.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘spareg/DESCRIPTION’ ... OK
...
--- failed re-building 'spareg.Rnw'

SUMMARY: processing the following file failed:
  'spareg.Rnw'

Error: Vignette re-building failed.
Execution halted

* DONE
Status: 2 ERRORs





```
# sparsevar

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/sparsevar
* Number of recursive dependencies: 73

Run `revdepcheck::cloud_details(, "sparsevar")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# spinebil

<details>

* Version: 1.0.5
* GitHub: https://github.com/uschiLaa/spinebil
* Source code: https://github.com/cran/spinebil
* Date/Publication: 2025-10-17 06:00:02 UTC
* Number of recursive dependencies: 113

Run `revdepcheck::cloud_details(, "spinebil")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/spinebil/new/spinebil.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘spinebil/DESCRIPTION’ ... OK
...
* this is package ‘spinebil’ version ‘1.0.5’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘tourr’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/spinebil/old/spinebil.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘spinebil/DESCRIPTION’ ... OK
...
* this is package ‘spinebil’ version ‘1.0.5’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘tourr’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# spqrp

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/spqrp
* Number of recursive dependencies: 179

Run `revdepcheck::cloud_details(, "spqrp")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# SRscore

<details>

* Version: 0.1.2
* GitHub: NA
* Source code: https://github.com/cran/SRscore
* Date/Publication: 2026-01-08 18:50:19 UTC
* Number of recursive dependencies: 167

Run `revdepcheck::cloud_details(, "SRscore")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/SRscore/new/SRscore.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘SRscore/DESCRIPTION’ ... OK
...
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/SRscore/old/SRscore.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘SRscore/DESCRIPTION’ ... OK
...
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 NOTE





```
# SSVS

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/SSVS
* Number of recursive dependencies: 155

Run `revdepcheck::cloud_details(, "SSVS")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# tepr

<details>

* Version: 1.1.15
* GitHub: NA
* Source code: https://github.com/cran/tepr
* Date/Publication: 2026-03-06 10:20:02 UTC
* Number of recursive dependencies: 124

Run `revdepcheck::cloud_details(, "tepr")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/tepr/new/tepr.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘tepr/DESCRIPTION’ ... OK
...
* this is package ‘tepr’ version ‘1.1.15’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘rtracklayer’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/tepr/old/tepr.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘tepr/DESCRIPTION’ ... OK
...
* this is package ‘tepr’ version ‘1.1.15’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘rtracklayer’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# tidybins

<details>

* Version: 0.1.2
* GitHub: https://github.com/Harrison4192/tidybins
* Source code: https://github.com/cran/tidybins
* Date/Publication: 2026-03-05 12:30:02 UTC
* Number of recursive dependencies: 224

Run `revdepcheck::cloud_details(, "tidybins")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/tidybins/new/tidybins.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘tidybins/DESCRIPTION’ ... OK
...
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 3 NOTEs





```
### CRAN

```
* using log directory ‘/tmp/workdir/tidybins/old/tidybins.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘tidybins/DESCRIPTION’ ... OK
...
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 3 NOTEs





```
# tidycomm

<details>

* Version: 0.4.2
* GitHub: https://github.com/tidycomm/tidycomm
* Source code: https://github.com/cran/tidycomm
* Date/Publication: 2025-08-27 12:00:02 UTC
* Number of recursive dependencies: 149

Run `revdepcheck::cloud_details(, "tidycomm")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/tidycomm/new/tidycomm.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘tidycomm/DESCRIPTION’ ... OK
...
* this is package ‘tidycomm’ version ‘0.4.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘MBESS’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/tidycomm/old/tidycomm.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘tidycomm/DESCRIPTION’ ... OK
...
* this is package ‘tidycomm’ version ‘0.4.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘MBESS’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# tidylearn

<details>

* Version: 0.3.1
* GitHub: https://github.com/ces0491/tidylearn
* Source code: https://github.com/cran/tidylearn
* Date/Publication: 2026-05-19 09:20:09 UTC
* Number of recursive dependencies: 255

Run `revdepcheck::cloud_details(, "tidylearn")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/tidylearn/new/tidylearn.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘tidylearn/DESCRIPTION’ ... OK
...
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/tidylearn/old/tidylearn.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘tidylearn/DESCRIPTION’ ... OK
...
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 NOTE





```
# TransProR

<details>

* Version: 1.0.7
* GitHub: https://github.com/SSSYDYSSS/TransProR
* Source code: https://github.com/cran/TransProR
* Date/Publication: 2025-09-12 12:10:02 UTC
* Number of recursive dependencies: 201

Run `revdepcheck::cloud_details(, "TransProR")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/TransProR/new/TransProR.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘TransProR/DESCRIPTION’ ... OK
...
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘ggtree’

Package suggested but not available for checking: ‘ggtreeExtra’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/TransProR/old/TransProR.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘TransProR/DESCRIPTION’ ... OK
...
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘ggtree’

Package suggested but not available for checking: ‘ggtreeExtra’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# univariateML

<details>

* Version: 1.5.0
* GitHub: https://github.com/JonasMoss/univariateML
* Source code: https://github.com/cran/univariateML
* Date/Publication: 2025-03-04 13:30:02 UTC
* Number of recursive dependencies: 105

Run `revdepcheck::cloud_details(, "univariateML")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/univariateML/new/univariateML.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘univariateML/DESCRIPTION’ ... OK
...
--- finished re-building ‘overview.Rmd’

SUMMARY: processing the following file failed:
  ‘copula.Rmd’

Error: Vignette re-building failed.
Execution halted

* DONE
Status: 1 ERROR, 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/univariateML/old/univariateML.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘univariateML/DESCRIPTION’ ... OK
...
--- finished re-building ‘overview.Rmd’

SUMMARY: processing the following file failed:
  ‘copula.Rmd’

Error: Vignette re-building failed.
Execution halted

* DONE
Status: 1 ERROR, 1 NOTE





```
# valr

<details>

* Version: 0.9.1
* GitHub: https://github.com/rnabioco/valr
* Source code: https://github.com/cran/valr
* Date/Publication: 2026-01-11 06:10:02 UTC
* Number of recursive dependencies: 139

Run `revdepcheck::cloud_details(, "valr")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/valr/new/valr.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘valr/DESCRIPTION’ ... OK
...
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘cpp11bigwig’

Package suggested but not available for checking: ‘GenomicRanges’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/valr/old/valr.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘valr/DESCRIPTION’ ... OK
...
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘cpp11bigwig’

Package suggested but not available for checking: ‘GenomicRanges’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# wjake

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/wjake
* Number of recursive dependencies: 118

Run `revdepcheck::cloud_details(, "wjake")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# wmwAUC

<details>

* Version: 0.2.0
* GitHub: https://github.com/grendar/wmwAUC
* Source code: https://github.com/cran/wmwAUC
* Date/Publication: 2025-12-19 14:20:02 UTC
* Number of recursive dependencies: 145

Run `revdepcheck::cloud_details(, "wmwAUC")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/wmwAUC/new/wmwAUC.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘wmwAUC/DESCRIPTION’ ... OK
...
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* DONE
Status: OK





```
### CRAN

```
* using log directory ‘/tmp/workdir/wmwAUC/old/wmwAUC.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘wmwAUC/DESCRIPTION’ ... OK
...
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* DONE
Status: OK





```
