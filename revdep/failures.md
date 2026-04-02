# a5R

<details>

* Version: 0.3.1
* GitHub: https://github.com/belian-earth/a5R
* Source code: https://github.com/cran/a5R
* Date/Publication: 2026-03-26 12:30:02 UTC
* Number of recursive dependencies: 65

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
using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’
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
using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’
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
# AnanseSeurat

<details>

* Version: 1.2.0
* GitHub: https://github.com/JGASmits/AnanseSeurat
* Source code: https://github.com/cran/AnanseSeurat
* Date/Publication: 2023-11-11 21:43:17 UTC
* Number of recursive dependencies: 212

Run `revdepcheck::cloud_details(, "AnanseSeurat")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/AnanseSeurat/new/AnanseSeurat.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘AnanseSeurat/DESCRIPTION’ ... OK
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
* using log directory ‘/tmp/workdir/AnanseSeurat/old/AnanseSeurat.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘AnanseSeurat/DESCRIPTION’ ... OK
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
# apa

<details>

* Version: 0.3.5
* GitHub: https://github.com/dgromer/apa
* Source code: https://github.com/cran/apa
* Date/Publication: 2026-02-22 15:30:07 UTC
* Number of recursive dependencies: 124

Run `revdepcheck::cloud_details(, "apa")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/apa/new/apa.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘apa/DESCRIPTION’ ... OK
...
* this is package ‘apa’ version ‘0.3.5’
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
* using log directory ‘/tmp/workdir/apa/old/apa.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘apa/DESCRIPTION’ ... OK
...
* this is package ‘apa’ version ‘0.3.5’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘MBESS’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# apaTables

<details>

* Version: 2.0.8
* GitHub: https://github.com/dstanley4/apaTables
* Source code: https://github.com/cran/apaTables
* Date/Publication: 2021-01-04 19:00:02 UTC
* Number of recursive dependencies: 106

Run `revdepcheck::cloud_details(, "apaTables")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/apaTables/new/apaTables.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘apaTables/DESCRIPTION’ ... OK
* this is package ‘apaTables’ version ‘2.0.8’
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
* using log directory ‘/tmp/workdir/apaTables/old/apaTables.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘apaTables/DESCRIPTION’ ... OK
* this is package ‘apaTables’ version ‘2.0.8’
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘MBESS’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# apaText

<details>

* Version: 0.1.7
* GitHub: NA
* Source code: https://github.com/cran/apaText
* Date/Publication: 2023-05-23 14:02:03 UTC
* Number of recursive dependencies: 88

Run `revdepcheck::cloud_details(, "apaText")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/apaText/new/apaText.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘apaText/DESCRIPTION’ ... OK
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
* using log directory ‘/tmp/workdir/apaText/old/apaText.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘apaText/DESCRIPTION’ ... OK
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
# apsimx

<details>

* Version: 2.8.235
* GitHub: https://github.com/femiguez/apsimx
* Source code: https://github.com/cran/apsimx
* Date/Publication: 2025-03-10 05:40:02 UTC
* Number of recursive dependencies: 184

Run `revdepcheck::cloud_details(, "apsimx")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/apsimx/new/apsimx.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘apsimx/DESCRIPTION’ ... OK
...
  Running ‘test_soil_properties.R’
  Running ‘test_sr_ks.R’
  Running ‘test_ssurgo2sp.R’
  Running ‘test_vignette.R’
  Running ‘test_worldmodeler.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 2 NOTEs





```
### CRAN

```
* using log directory ‘/tmp/workdir/apsimx/old/apsimx.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘apsimx/DESCRIPTION’ ... OK
...
  Running ‘test_soil_properties.R’
  Running ‘test_sr_ks.R’
  Running ‘test_ssurgo2sp.R’
  Running ‘test_vignette.R’
  Running ‘test_worldmodeler.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 2 NOTEs





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
using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’
...
export CARGO_HOME=/tmp/workdir/arcgisplaces/new/arcgisplaces.Rcheck/00_pkg_src/arcgisplaces/src/.cargo && \
export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/root/.cargo/bin" && \
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
using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’
...
export CARGO_HOME=/tmp/workdir/arcgisplaces/old/arcgisplaces.Rcheck/00_pkg_src/arcgisplaces/src/.cargo && \
export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/root/.cargo/bin" && \
RUSTFLAGS=" --print=native-static-libs" cargo build -j 2 --offline --lib --release --manifest-path=./rust/Cargo.toml --target-dir ./rust/target
error: package `native-tls v0.2.14` cannot be built because it requires rustc 1.80.0 or newer, while the currently active rustc version is 1.75.0
Either upgrade to rustc 1.80.0 or newer, or use
cargo update native-tls@0.2.14 --precise ver
where `ver` is the latest version of `native-tls` supporting rustc 1.75.0
make: *** [Makevars:28: rust/target/release/libarcgisplaces.a] Error 101
ERROR: compilation failed for package ‘arcgisplaces’
* removing ‘/tmp/workdir/arcgisplaces/old/arcgisplaces.Rcheck/arcgisplaces’


```
# aridagri

<details>

* Version: 2.0.3
* GitHub: https://github.com/lalitrolaniya/aridagri
* Source code: https://github.com/cran/aridagri
* Date/Publication: 2026-02-24 19:20:08 UTC
* Number of recursive dependencies: 180

Run `revdepcheck::cloud_details(, "aridagri")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/aridagri/new/aridagri.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘aridagri/DESCRIPTION’ ... OK
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
Status: 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/aridagri/old/aridagri.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘aridagri/DESCRIPTION’ ... OK
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
Status: 1 NOTE





```
# AssumpSure

<details>

* Version: 1.1.4
* GitHub: https://github.com/Ahmedbargheet/AssumpSure
* Source code: https://github.com/cran/AssumpSure
* Date/Publication: 2025-11-25 11:20:18 UTC
* Number of recursive dependencies: 261

Run `revdepcheck::cloud_details(, "AssumpSure")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/AssumpSure/new/AssumpSure.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘AssumpSure/DESCRIPTION’ ... OK
...
* this is package ‘AssumpSure’ version ‘1.1.4’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘MVN’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/AssumpSure/old/AssumpSure.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘AssumpSure/DESCRIPTION’ ... OK
...
* this is package ‘AssumpSure’ version ‘1.1.4’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘MVN’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# autovi

<details>

* Version: 0.4.1
* GitHub: https://github.com/TengMCing/autovi
* Source code: https://github.com/cran/autovi
* Date/Publication: 2024-11-18 04:40:12 UTC
* Number of recursive dependencies: 98

Run `revdepcheck::cloud_details(, "autovi")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/autovi/new/autovi.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘autovi/DESCRIPTION’ ... OK
...
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* DONE
Status: OK





```
### CRAN

```
* using log directory ‘/tmp/workdir/autovi/old/autovi.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘autovi/DESCRIPTION’ ... OK
...
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* DONE
Status: OK





```
# auxvecLASSO

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/auxvecLASSO
* Number of recursive dependencies: 71

Run `revdepcheck::cloud_details(, "auxvecLASSO")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# AVGAS

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/AVGAS
* Date/Publication: 2023-12-20 16:20:08 UTC
* Number of recursive dependencies: 49

Run `revdepcheck::cloud_details(, "AVGAS")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/AVGAS/new/AVGAS.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘AVGAS/DESCRIPTION’ ... OK
...
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘AVGAS’ can be installed ... ERROR
Installation failed.
See ‘/tmp/workdir/AVGAS/new/AVGAS.Rcheck/00install.out’ for details.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/AVGAS/old/AVGAS.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘AVGAS/DESCRIPTION’ ... OK
...
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘AVGAS’ can be installed ... ERROR
Installation failed.
See ‘/tmp/workdir/AVGAS/old/AVGAS.Rcheck/00install.out’ for details.
* DONE
Status: 1 ERROR





```
# bayesdfa

<details>

* Version: 1.3.4
* GitHub: https://github.com/fate-ewi/bayesdfa
* Source code: https://github.com/cran/bayesdfa
* Date/Publication: 2025-03-22 20:30:21 UTC
* Number of recursive dependencies: 85

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
# BayesPET

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/BayesPET
* Date/Publication: 2026-02-12 08:00:26 UTC
* Number of recursive dependencies: 71

Run `revdepcheck::cloud_details(, "BayesPET")` for more info

</details>

## In both

*   checking whether package ‘BayesPET’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/BayesPET/new/BayesPET.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘BayesPET’ ...
** package ‘BayesPET’ successfully unpacked and MD5 sums checked
** using staged installation
Error in loadNamespace(x) : there is no package called ‘rstantools’
Calls: loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: configuration failed for package ‘BayesPET’
* removing ‘/tmp/workdir/BayesPET/new/BayesPET.Rcheck/BayesPET’


```
### CRAN

```
* installing *source* package ‘BayesPET’ ...
** package ‘BayesPET’ successfully unpacked and MD5 sums checked
** using staged installation
Error in loadNamespace(x) : there is no package called ‘rstantools’
Calls: loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: configuration failed for package ‘BayesPET’
* removing ‘/tmp/workdir/BayesPET/old/BayesPET.Rcheck/BayesPET’


```
# BayesVolcano

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/BayesVolcano
* Number of recursive dependencies: 103

Run `revdepcheck::cloud_details(, "BayesVolcano")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# benthos

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/benthos
* Number of recursive dependencies: 72

Run `revdepcheck::cloud_details(, "benthos")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# bfboinet

<details>

* Version: 0.4.0
* GitHub: NA
* Source code: https://github.com/cran/bfboinet
* Date/Publication: 2025-06-03 09:50:02 UTC
* Number of recursive dependencies: 28

Run `revdepcheck::cloud_details(, "bfboinet")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/bfboinet/new/bfboinet.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘bfboinet/DESCRIPTION’ ... OK
...
* this is package ‘bfboinet’ version ‘0.4.0’
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
* using log directory ‘/tmp/workdir/bfboinet/old/bfboinet.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘bfboinet/DESCRIPTION’ ... OK
...
* this is package ‘bfboinet’ version ‘0.4.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘copula’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# BGGM

<details>

* Version: 2.1.6
* GitHub: https://github.com/rast-lab/BGGM
* Source code: https://github.com/cran/BGGM
* Date/Publication: 2025-12-02 07:40:02 UTC
* Number of recursive dependencies: 209

Run `revdepcheck::cloud_details(, "BGGM")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/BGGM/new/BGGM.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘BGGM/DESCRIPTION’ ... OK
...
* this is package ‘BGGM’ version ‘2.1.6’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘BFpack’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/BGGM/old/BGGM.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘BGGM/DESCRIPTION’ ... OK
...
* this is package ‘BGGM’ version ‘2.1.6’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘BFpack’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# BGmisc

<details>

* Version: 1.6.0.1
* GitHub: https://github.com/R-Computing-Lab/BGmisc
* Source code: https://github.com/cran/BGmisc
* Date/Publication: 2026-03-13 17:00:07 UTC
* Number of recursive dependencies: 133

Run `revdepcheck::cloud_details(, "BGmisc")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/BGmisc/new/BGmisc.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘BGmisc/DESCRIPTION’ ... OK
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
* using log directory ‘/tmp/workdir/BGmisc/old/BGmisc.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘BGmisc/DESCRIPTION’ ... OK
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
# bipd

<details>

* Version: 0.3
* GitHub: NA
* Source code: https://github.com/cran/bipd
* Date/Publication: 2022-06-05 16:10:05 UTC
* Number of recursive dependencies: 153

Run `revdepcheck::cloud_details(, "bipd")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/bipd/new/bipd.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘bipd/DESCRIPTION’ ... OK
...
--- failed re-building ‘Imputing-missing-values-in-IPD.Rmd’

SUMMARY: processing the following file failed:
  ‘Imputing-missing-values-in-IPD.Rmd’

Error: Vignette re-building failed.
Execution halted

* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/bipd/old/bipd.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘bipd/DESCRIPTION’ ... OK
...
--- failed re-building ‘Imputing-missing-values-in-IPD.Rmd’

SUMMARY: processing the following file failed:
  ‘Imputing-missing-values-in-IPD.Rmd’

Error: Vignette re-building failed.
Execution halted

* DONE
Status: 1 ERROR





```
# bmm

<details>

* Version: 1.3.0
* GitHub: https://github.com/venpopov/bmm
* Source code: https://github.com/cran/bmm
* Date/Publication: 2026-03-30 11:00:09 UTC
* Number of recursive dependencies: 137

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
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘bmm/DESCRIPTION’ ... OK
...
* this is package ‘bmm’ version ‘1.3.0’
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
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘bmm/DESCRIPTION’ ... OK
...
* this is package ‘bmm’ version ‘1.3.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘rtdists’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# boinet

<details>

* Version: 1.5.0
* GitHub: NA
* Source code: https://github.com/cran/boinet
* Date/Publication: 2025-10-26 19:20:02 UTC
* Number of recursive dependencies: 111

Run `revdepcheck::cloud_details(, "boinet")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/boinet/new/boinet.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘boinet/DESCRIPTION’ ... OK
...
* this is package ‘boinet’ version ‘1.5.0’
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
* using log directory ‘/tmp/workdir/boinet/old/boinet.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘boinet/DESCRIPTION’ ... OK
...
* this is package ‘boinet’ version ‘1.5.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘copula’

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
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘brms/DESCRIPTION’ ... OK
...
  
  [ FAIL 1 | WARN 0 | SKIP 11 | PASS 2731 ]
  Error:
  ! Test failures.
  Execution halted
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 ERROR, 3 NOTEs





```
### CRAN

```
* using log directory ‘/tmp/workdir/brms/old/brms.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘brms/DESCRIPTION’ ... OK
...
  
  [ FAIL 1 | WARN 0 | SKIP 11 | PASS 2731 ]
  Error:
  ! Test failures.
  Execution halted
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 ERROR, 3 NOTEs





```
# BSL

<details>

* Version: 3.2.5
* GitHub: NA
* Source code: https://github.com/cran/BSL
* Date/Publication: 2022-11-03 09:00:07 UTC
* Number of recursive dependencies: 98

Run `revdepcheck::cloud_details(, "BSL")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/BSL/new/BSL.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘BSL/DESCRIPTION’ ... OK
...
* this is package ‘BSL’ version ‘3.2.5’
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
* using log directory ‘/tmp/workdir/BSL/old/BSL.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘BSL/DESCRIPTION’ ... OK
...
* this is package ‘BSL’ version ‘3.2.5’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘copula’

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
* running under: Ubuntu 24.04.3 LTS
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
* running under: Ubuntu 24.04.3 LTS
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
# cases

<details>

* Version: 0.2.0
* GitHub: https://github.com/maxwestphal/cases
* Source code: https://github.com/cran/cases
* Date/Publication: 2025-01-09 17:50:02 UTC
* Number of recursive dependencies: 118

Run `revdepcheck::cloud_details(, "cases")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/cases/new/cases.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘cases/DESCRIPTION’ ... OK
...
* this is package ‘cases’ version ‘0.2.0’
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
* using log directory ‘/tmp/workdir/cases/old/cases.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘cases/DESCRIPTION’ ... OK
...
* this is package ‘cases’ version ‘0.2.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘copula’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# cassowaryr

<details>

* Version: 2.0.2
* GitHub: https://github.com/numbats/cassowaryr
* Source code: https://github.com/cran/cassowaryr
* Date/Publication: 2024-09-13 07:20:02 UTC
* Number of recursive dependencies: 102

Run `revdepcheck::cloud_details(, "cassowaryr")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/cassowaryr/new/cassowaryr.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘cassowaryr/DESCRIPTION’ ... OK
...
* this is package ‘cassowaryr’ version ‘2.0.2’
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
* using log directory ‘/tmp/workdir/cassowaryr/old/cassowaryr.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘cassowaryr/DESCRIPTION’ ... OK
...
* this is package ‘cassowaryr’ version ‘2.0.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘energy’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# cdcatR

<details>

* Version: 1.0.7
* GitHub: https://github.com/miguel-sorrel/cdcatR
* Source code: https://github.com/cran/cdcatR
* Date/Publication: 2026-02-11 09:30:02 UTC
* Number of recursive dependencies: 128

Run `revdepcheck::cloud_details(, "cdcatR")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/cdcatR/new/cdcatR.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘cdcatR/DESCRIPTION’ ... OK
...
* this is package ‘cdcatR’ version ‘1.0.7’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘cdmTools’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/cdcatR/old/cdcatR.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘cdcatR/DESCRIPTION’ ... OK
...
* this is package ‘cdcatR’ version ‘1.0.7’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘cdmTools’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# cdmTools

<details>

* Version: 1.0.6
* GitHub: https://github.com/pablo-najera/cdmTools
* Source code: https://github.com/cran/cdmTools
* Date/Publication: 2025-05-19 08:10:02 UTC
* Number of recursive dependencies: 123

Run `revdepcheck::cloud_details(, "cdmTools")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/cdmTools/new/cdmTools.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘cdmTools/DESCRIPTION’ ... OK
...
* this is package ‘cdmTools’ version ‘1.0.6’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘fungible’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/cdmTools/old/cdmTools.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘cdmTools/DESCRIPTION’ ... OK
...
* this is package ‘cdmTools’ version ‘1.0.6’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘fungible’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# cheem

<details>

* Version: 0.4.2
* GitHub: https://github.com/nspyrison/cheem
* Source code: https://github.com/cran/cheem
* Date/Publication: 2025-09-17 22:10:08 UTC
* Number of recursive dependencies: 171

Run `revdepcheck::cloud_details(, "cheem")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/cheem/new/cheem.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘cheem/DESCRIPTION’ ... OK
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
Status: OK





```
### CRAN

```
* using log directory ‘/tmp/workdir/cheem/old/cheem.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘cheem/DESCRIPTION’ ... OK
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
Status: OK





```
# ChillModels

<details>

* Version: 1.0.2
* GitHub: NA
* Source code: https://github.com/cran/ChillModels
* Date/Publication: 2020-01-16 16:50:06 UTC
* Number of recursive dependencies: 106

Run `revdepcheck::cloud_details(, "ChillModels")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/ChillModels/new/ChillModels.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘ChillModels/DESCRIPTION’ ... OK
...
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking LazyData ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking examples ... OK
* DONE
Status: 2 NOTEs





```
### CRAN

```
* using log directory ‘/tmp/workdir/ChillModels/old/ChillModels.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘ChillModels/DESCRIPTION’ ... OK
...
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking LazyData ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking examples ... OK
* DONE
Status: 2 NOTEs





```
# chouca

<details>

* Version: 0.1.99
* GitHub: https://github.com/alexgenin/chouca
* Source code: https://github.com/cran/chouca
* Date/Publication: 2024-03-07 15:00:02 UTC
* Number of recursive dependencies: 78

Run `revdepcheck::cloud_details(, "chouca")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/chouca/new/chouca.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘chouca/DESCRIPTION’ ... OK
...
--- failed re-building ‘chouca-package.Rmd’

SUMMARY: processing the following file failed:
  ‘chouca-package.Rmd’

Error: Vignette re-building failed.
Execution halted

* DONE
Status: 2 ERRORs, 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/chouca/old/chouca.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘chouca/DESCRIPTION’ ... OK
...
--- failed re-building ‘chouca-package.Rmd’

SUMMARY: processing the following file failed:
  ‘chouca-package.Rmd’

Error: Vignette re-building failed.
Execution halted

* DONE
Status: 2 ERRORs, 1 NOTE





```
# cinaR

<details>

* Version: 0.2.6
* GitHub: https://github.com/eonurk/cinaR
* Source code: https://github.com/cran/cinaR
* Date/Publication: 2026-02-04 14:50:02 UTC
* Number of recursive dependencies: 234

Run `revdepcheck::cloud_details(, "cinaR")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/cinaR/new/cinaR.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘cinaR/DESCRIPTION’ ... OK
...
> cp.normalized <- normalizeConsensus(cp, norm.method = "quantile")
Error in preprocessCore::normalize.quantiles(cp) : 
  ERROR; return code from pthread_create() is 22
Calls: normalizeConsensus -> <Anonymous>
Execution halted
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 ERROR, 2 NOTEs





```
### CRAN

```
* using log directory ‘/tmp/workdir/cinaR/old/cinaR.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘cinaR/DESCRIPTION’ ... OK
...
> cp.normalized <- normalizeConsensus(cp, norm.method = "quantile")
Error in preprocessCore::normalize.quantiles(cp) : 
  ERROR; return code from pthread_create() is 22
Calls: normalizeConsensus -> <Anonymous>
Execution halted
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 ERROR, 2 NOTEs





```
# ClustAssess

<details>

* Version: 1.1.0
* GitHub: https://github.com/Core-Bioinformatics/ClustAssess
* Source code: https://github.com/cran/ClustAssess
* Date/Publication: 2025-05-27 23:00:30 UTC
* Number of recursive dependencies: 249

Run `revdepcheck::cloud_details(, "ClustAssess")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/ClustAssess/new/ClustAssess.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘ClustAssess/DESCRIPTION’ ... OK
...
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking line endings in C/C++/Fortran sources/headers ... OK
* checking compiled code ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* DONE
Status: 2 NOTEs





```
### CRAN

```
* using log directory ‘/tmp/workdir/ClustAssess/old/ClustAssess.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘ClustAssess/DESCRIPTION’ ... OK
...
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking line endings in C/C++/Fortran sources/headers ... OK
* checking compiled code ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* DONE
Status: 2 NOTEs





```
# ClusterGVis

<details>

* Version: 0.1.4
* GitHub: https://github.com/junjunlab/ClusterGVis
* Source code: https://github.com/cran/ClusterGVis
* Date/Publication: 2025-07-19 06:10:02 UTC
* Number of recursive dependencies: 300

Run `revdepcheck::cloud_details(, "ClusterGVis")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/ClusterGVis/new/ClusterGVis.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘ClusterGVis/DESCRIPTION’ ... OK
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
Status: 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/ClusterGVis/old/ClusterGVis.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘ClusterGVis/DESCRIPTION’ ... OK
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
Status: 1 NOTE





```
# clusterMI

<details>

* Version: 1.5
* GitHub: NA
* Source code: https://github.com/cran/clusterMI
* Date/Publication: 2025-02-24 17:20:08 UTC
* Number of recursive dependencies: 266

Run `revdepcheck::cloud_details(, "clusterMI")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/clusterMI/new/clusterMI.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘clusterMI/DESCRIPTION’ ... OK
...
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘clusterMI’ can be installed ... ERROR
Installation failed.
See ‘/tmp/workdir/clusterMI/new/clusterMI.Rcheck/00install.out’ for details.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/clusterMI/old/clusterMI.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘clusterMI/DESCRIPTION’ ... OK
...
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘clusterMI’ can be installed ... ERROR
Installation failed.
See ‘/tmp/workdir/clusterMI/old/clusterMI.Rcheck/00install.out’ for details.
* DONE
Status: 1 ERROR





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
* running under: Ubuntu 24.04.3 LTS
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
* running under: Ubuntu 24.04.3 LTS
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
# colleyRstats

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/colleyRstats
* Number of recursive dependencies: 225

Run `revdepcheck::cloud_details(, "colleyRstats")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# CompAREdesign

<details>

* Version: 2.4.0
* GitHub: NA
* Source code: https://github.com/cran/CompAREdesign
* Date/Publication: 2025-03-05 03:40:02 UTC
* Number of recursive dependencies: 107

Run `revdepcheck::cloud_details(, "CompAREdesign")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/CompAREdesign/new/CompAREdesign.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘CompAREdesign/DESCRIPTION’ ... OK
...
* this is package ‘CompAREdesign’ version ‘2.4.0’
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
* using log directory ‘/tmp/workdir/CompAREdesign/old/CompAREdesign.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘CompAREdesign/DESCRIPTION’ ... OK
...
* this is package ‘CompAREdesign’ version ‘2.4.0’
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
* Number of recursive dependencies: 72

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
* running under: Ubuntu 24.04.3 LTS
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
* running under: Ubuntu 24.04.3 LTS
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
# correlation

<details>

* Version: 0.8.8
* GitHub: https://github.com/easystats/correlation
* Source code: https://github.com/cran/correlation
* Date/Publication: 2025-07-08 15:40:02 UTC
* Number of recursive dependencies: 186

Run `revdepcheck::cloud_details(, "correlation")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/correlation/new/correlation.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘correlation/DESCRIPTION’ ... OK
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
* using log directory ‘/tmp/workdir/correlation/old/correlation.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘correlation/DESCRIPTION’ ... OK
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
# coveR2

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/coveR2
* Date/Publication: 2023-10-17 09:50:05 UTC
* Number of recursive dependencies: 64

Run `revdepcheck::cloud_details(, "coveR2")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/coveR2/new/coveR2.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘coveR2/DESCRIPTION’ ... OK
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
* using log directory ‘/tmp/workdir/coveR2/old/coveR2.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘coveR2/DESCRIPTION’ ... OK
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
# cowbell

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/cowbell
* Date/Publication: 2017-05-05 21:01:45 UTC
* Number of recursive dependencies: 55

Run `revdepcheck::cloud_details(, "cowbell")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# cry

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/cry
* Number of recursive dependencies: 43

Run `revdepcheck::cloud_details(, "cry")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# ctsem

<details>

* Version: 3.10.6
* GitHub: https://github.com/cdriveraus/ctsem
* Source code: https://github.com/cran/ctsem
* Date/Publication: 2026-01-27 13:30:02 UTC
* Number of recursive dependencies: 170

Run `revdepcheck::cloud_details(, "ctsem")` for more info

</details>

## In both

*   checking whether package ‘ctsem’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/ctsem/new/ctsem.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘arules’
    ```

## Installation

### Devel

```
* installing *source* package ‘ctsem’ ...
** package ‘ctsem’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’
using C++17


g++ -std=gnu++17 -I"/opt/R/4.4.0/lib/R/include" -DNDEBUG -I"../inst/include" -I"/tmp/r-deps/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -DUSE_STANC3 -D_HAS_AUTO_PTR_ETC=0 -I'/tmp/r-deps/BH/include' -I'/tmp/r-deps/Rcpp/include' -I'/tmp/r-deps/RcppEigen/include' -I'/tmp/r-deps/RcppParallel/include' -I'/tmp/r-deps/rstan/include' -I'/tmp/r-deps/StanHeaders/include' -I/usr/local/include    -I'/tmp/r-deps/RcppParallel/include' -D_REENTRANT -DSTAN_THREADS   -fpic  -g -O2   -c RcppExports.cpp -o RcppExports.o
In file included from /tmp/r-deps/RcppEigen/include/Eigen/Core:205,
...
/tmp/r-deps/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:0:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_ctsm_namespace::model_ctsm; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/tmp/r-deps/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:21:0:   required from here
/tmp/r-deps/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:654:74: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__m128d’} [-Wignored-attributes]
  654 |   return internal::first_aligned<int(unpacket_traits<DefaultPacketType>::alignment),Derived>(m);
      |                                                                          ^~~~~~~~~
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.4.0/lib/R/etc/Makeconf:202: stanExports_ctsm.o] Error 1
ERROR: compilation failed for package ‘ctsem’
* removing ‘/tmp/workdir/ctsem/new/ctsem.Rcheck/ctsem’


```
### CRAN

```
* installing *source* package ‘ctsem’ ...
** package ‘ctsem’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’
using C++17


g++ -std=gnu++17 -I"/opt/R/4.4.0/lib/R/include" -DNDEBUG -I"../inst/include" -I"/tmp/r-deps/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -DUSE_STANC3 -D_HAS_AUTO_PTR_ETC=0 -I'/tmp/r-deps/BH/include' -I'/tmp/r-deps/Rcpp/include' -I'/tmp/r-deps/RcppEigen/include' -I'/tmp/r-deps/RcppParallel/include' -I'/tmp/r-deps/rstan/include' -I'/tmp/r-deps/StanHeaders/include' -I/usr/local/include    -I'/tmp/r-deps/RcppParallel/include' -D_REENTRANT -DSTAN_THREADS   -fpic  -g -O2   -c RcppExports.cpp -o RcppExports.o
In file included from /tmp/r-deps/RcppEigen/include/Eigen/Core:205,
...
/tmp/r-deps/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:0:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_ctsm_namespace::model_ctsm; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/tmp/r-deps/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:21:0:   required from here
/tmp/r-deps/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:654:74: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__m128d’} [-Wignored-attributes]
  654 |   return internal::first_aligned<int(unpacket_traits<DefaultPacketType>::alignment),Derived>(m);
      |                                                                          ^~~~~~~~~
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.4.0/lib/R/etc/Makeconf:202: stanExports_ctsm.o] Error 1
ERROR: compilation failed for package ‘ctsem’
* removing ‘/tmp/workdir/ctsem/old/ctsem.Rcheck/ctsem’


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
* running under: Ubuntu 24.04.3 LTS
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
* running under: Ubuntu 24.04.3 LTS
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
# daltoolbox

<details>

* Version: 1.3.727
* GitHub: https://github.com/cefet-rj-dal/daltoolbox
* Source code: https://github.com/cran/daltoolbox
* Date/Publication: 2026-03-12 06:10:19 UTC
* Number of recursive dependencies: 161

Run `revdepcheck::cloud_details(, "daltoolbox")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/daltoolbox/new/daltoolbox.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘daltoolbox/DESCRIPTION’ ... OK
...
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'arules', 'arulesSequences'

Package suggested but not available for checking: ‘arulesViz’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/daltoolbox/old/daltoolbox.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘daltoolbox/DESCRIPTION’ ... OK
...
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'arules', 'arulesSequences'

Package suggested but not available for checking: ‘arulesViz’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# DataSimilarity

<details>

* Version: 0.3.0
* GitHub: NA
* Source code: https://github.com/cran/DataSimilarity
* Date/Publication: 2026-02-27 11:20:02 UTC
* Number of recursive dependencies: 191

Run `revdepcheck::cloud_details(, "DataSimilarity")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/DataSimilarity/new/DataSimilarity.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘DataSimilarity/DESCRIPTION’ ... OK
...
--- failed re-building ‘GettingStarted.Rnw’

SUMMARY: processing the following files failed:
  ‘Details.Rnw’ ‘GettingStarted.Rnw’

Error: Vignette re-building failed.
Execution halted

* DONE
Status: 1 ERROR, 1 WARNING, 3 NOTEs





```
### CRAN

```
* using log directory ‘/tmp/workdir/DataSimilarity/old/DataSimilarity.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘DataSimilarity/DESCRIPTION’ ... OK
...
--- failed re-building ‘GettingStarted.Rnw’

SUMMARY: processing the following files failed:
  ‘Details.Rnw’ ‘GettingStarted.Rnw’

Error: Vignette re-building failed.
Execution halted

* DONE
Status: 1 ERROR, 1 WARNING, 3 NOTEs





```
# dbrobust

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/dbrobust
* Number of recursive dependencies: 128

Run `revdepcheck::cloud_details(, "dbrobust")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# decisionSupport

<details>

* Version: 1.115
* GitHub: NA
* Source code: https://github.com/cran/decisionSupport
* Date/Publication: 2025-08-19 18:00:02 UTC
* Number of recursive dependencies: 179

Run `revdepcheck::cloud_details(, "decisionSupport")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/decisionSupport/new/decisionSupport.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘decisionSupport/DESCRIPTION’ ... OK
...
* this is package ‘decisionSupport’ version ‘1.115’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘chillR’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/decisionSupport/old/decisionSupport.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘decisionSupport/DESCRIPTION’ ... OK
...
* this is package ‘decisionSupport’ version ‘1.115’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘chillR’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# deepSTRAPP

<details>

* Version: 1.0.0
* GitHub: https://github.com/MaelDore/deepSTRAPP
* Source code: https://github.com/cran/deepSTRAPP
* Date/Publication: 2026-01-19 17:30:02 UTC
* Number of recursive dependencies: 97

Run `revdepcheck::cloud_details(, "deepSTRAPP")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/deepSTRAPP/new/deepSTRAPP.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘deepSTRAPP/DESCRIPTION’ ... OK
...
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required and available but unsuitable version: ‘methods’

Package suggested but not available for checking: ‘BioGeoBEARS’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/deepSTRAPP/old/deepSTRAPP.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘deepSTRAPP/DESCRIPTION’ ... OK
...
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required and available but unsuitable version: ‘methods’

Package suggested but not available for checking: ‘BioGeoBEARS’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# degradr

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/degradr
* Number of recursive dependencies: 44

Run `revdepcheck::cloud_details(, "degradr")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# detourr

<details>

* Version: 0.2.0
* GitHub: https://github.com/casperhart/detourr
* Source code: https://github.com/cran/detourr
* Date/Publication: 2025-08-28 03:00:02 UTC
* Number of recursive dependencies: 107

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
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘detourr/DESCRIPTION’ ... OK
...
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking LazyData ... OK
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
* using log directory ‘/tmp/workdir/detourr/old/detourr.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘detourr/DESCRIPTION’ ... OK
...
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking LazyData ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* DONE
Status: 1 NOTE





```
# DFD

<details>

* Version: 0.4.0
* GitHub: https://github.com/MohmedSoudy/DFD
* Source code: https://github.com/cran/DFD
* Date/Publication: 2025-12-22 10:10:02 UTC
* Number of recursive dependencies: 220

Run `revdepcheck::cloud_details(, "DFD")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/DFD/new/DFD.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘DFD/DESCRIPTION’ ... OK
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
Status: 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/DFD/old/DFD.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘DFD/DESCRIPTION’ ... OK
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
Status: 1 NOTE





```
# dimRed

<details>

* Version: 0.2.7
* GitHub: https://github.com/gdkrmr/dimRed
* Source code: https://github.com/cran/dimRed
* Date/Publication: 2025-04-23 15:00:02 UTC
* Number of recursive dependencies: 131

Run `revdepcheck::cloud_details(, "dimRed")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/dimRed/new/dimRed.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘dimRed/DESCRIPTION’ ... OK
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
* using log directory ‘/tmp/workdir/dimRed/old/dimRed.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘dimRed/DESCRIPTION’ ... OK
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
# dMrs

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/dMrs
* Date/Publication: 2025-01-21 15:40:05 UTC
* Number of recursive dependencies: 138

Run `revdepcheck::cloud_details(, "dMrs")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/dMrs/new/dMrs.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘dMrs/DESCRIPTION’ ... OK
...
* this is package ‘dMrs’ version ‘1.0.0’
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
* using log directory ‘/tmp/workdir/dMrs/old/dMrs.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘dMrs/DESCRIPTION’ ... OK
...
* this is package ‘dMrs’ version ‘1.0.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘copula’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# DstarM

<details>

* Version: 0.5.0
* GitHub: https://github.com/vandenman/DstarM
* Source code: https://github.com/cran/DstarM
* Date/Publication: 2025-04-01 09:00:02 UTC
* Number of recursive dependencies: 52

Run `revdepcheck::cloud_details(, "DstarM")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/DstarM/new/DstarM.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘DstarM/DESCRIPTION’ ... OK
...
* this is package ‘DstarM’ version ‘0.5.0’
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
* using log directory ‘/tmp/workdir/DstarM/old/DstarM.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘DstarM/DESCRIPTION’ ... OK
...
* this is package ‘DstarM’ version ‘0.5.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘rtdists’

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
* Number of recursive dependencies: 38

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
* Number of recursive dependencies: 194

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
* running under: Ubuntu 24.04.3 LTS
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
* running under: Ubuntu 24.04.3 LTS
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
# easybgm

<details>

* Version: 0.3.1
* GitHub: https://github.com/KarolineHuth/easybgm
* Source code: https://github.com/cran/easybgm
* Date/Publication: 2025-12-08 12:10:07 UTC
* Number of recursive dependencies: 180

Run `revdepcheck::cloud_details(, "easybgm")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/easybgm/new/easybgm.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘easybgm/DESCRIPTION’ ... OK
...
* this is package ‘easybgm’ version ‘0.3.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘BGGM’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/easybgm/old/easybgm.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘easybgm/DESCRIPTION’ ... OK
...
* this is package ‘easybgm’ version ‘0.3.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘BGGM’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# easyEWAS

<details>

* Version: 1.0.1
* GitHub: https://github.com/ytwangZero/easyEWAS
* Source code: https://github.com/cran/easyEWAS
* Date/Publication: 2026-03-16 19:10:14 UTC
* Number of recursive dependencies: 282

Run `revdepcheck::cloud_details(, "easyEWAS")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/easyEWAS/new/easyEWAS.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘easyEWAS/DESCRIPTION’ ... OK
...
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking LazyData ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking examples ... OK
* DONE
Status: 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/easyEWAS/old/easyEWAS.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘easyEWAS/DESCRIPTION’ ... OK
...
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking LazyData ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking examples ... OK
* DONE
Status: 1 NOTE





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
# eda4treeR

<details>

* Version: 1.1.0
* GitHub: https://github.com/myaseen208/eda4treeR
* Source code: https://github.com/cran/eda4treeR
* Date/Publication: 2024-09-13 21:50:02 UTC
* Number of recursive dependencies: 141

Run `revdepcheck::cloud_details(, "eda4treeR")` for more info

</details>

## In both

*   checking whether package ‘eda4treeR’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/eda4treeR/new/eda4treeR.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘eda4treeR’ ...
** package ‘eda4treeR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘HRW’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘eda4treeR’
* removing ‘/tmp/workdir/eda4treeR/new/eda4treeR.Rcheck/eda4treeR’


```
### CRAN

```
* installing *source* package ‘eda4treeR’ ...
** package ‘eda4treeR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘HRW’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘eda4treeR’
* removing ‘/tmp/workdir/eda4treeR/old/eda4treeR.Rcheck/eda4treeR’


```
# EFDR

<details>

* Version: 1.3
* GitHub: https://github.com/andrewzm/EFDR
* Source code: https://github.com/cran/EFDR
* Date/Publication: 2023-08-22 22:20:02 UTC
* Number of recursive dependencies: 103

Run `revdepcheck::cloud_details(, "EFDR")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/EFDR/new/EFDR.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘EFDR/DESCRIPTION’ ... OK
...
* this is package ‘EFDR’ version ‘1.3’
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
* using log directory ‘/tmp/workdir/EFDR/old/EFDR.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘EFDR/DESCRIPTION’ ... OK
...
* this is package ‘EFDR’ version ‘1.3’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘copula’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# effectcheck

<details>

* Version: 0.2.3
* GitHub: https://github.com/giladfeldman/escicheck
* Source code: https://github.com/cran/effectcheck
* Date/Publication: 2026-03-25 10:50:02 UTC
* Number of recursive dependencies: 127

Run `revdepcheck::cloud_details(, "effectcheck")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/effectcheck/new/effectcheck.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘effectcheck/DESCRIPTION’ ... OK
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
* using log directory ‘/tmp/workdir/effectcheck/old/effectcheck.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘effectcheck/DESCRIPTION’ ... OK
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
# EGAnet

<details>

* Version: 2.4.0
* GitHub: https://github.com/hfgolino/EGAnet
* Source code: https://github.com/cran/EGAnet
* Date/Publication: 2025-12-06 16:30:08 UTC
* Number of recursive dependencies: 167

Run `revdepcheck::cloud_details(, "EGAnet")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/EGAnet/new/EGAnet.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘EGAnet/DESCRIPTION’ ... OK
...
* this is package ‘EGAnet’ version ‘2.4.0’
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
* using log directory ‘/tmp/workdir/EGAnet/old/EGAnet.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘EGAnet/DESCRIPTION’ ... OK
...
* this is package ‘EGAnet’ version ‘2.4.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘semPlot’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# enderecobr

<details>

* Version: 0.5.0
* GitHub: https://github.com/ipeaGIT/enderecobr
* Source code: https://github.com/cran/enderecobr
* Date/Publication: 2026-01-10 07:51:43 UTC
* Number of recursive dependencies: 51

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
using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’
...
export CARGO_HOME=/tmp/workdir/enderecobr/new/enderecobr.Rcheck/00_pkg_src/enderecobr/src/.cargo && \
export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/root/.cargo/bin" && \
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
using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’
...
export CARGO_HOME=/tmp/workdir/enderecobr/old/enderecobr.Rcheck/00_pkg_src/enderecobr/src/.cargo && \
export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/root/.cargo/bin" && \
RUSTFLAGS=" --print=native-static-libs" cargo build -j 2 --offline --lib --release --manifest-path=./rust/Cargo.toml --target-dir ./rust/target 
error: failed to parse lock file at: /tmp/workdir/enderecobr/old/enderecobr.Rcheck/00_pkg_src/enderecobr/src/rust/Cargo.lock

Caused by:
  lock file version 4 requires `-Znext-lockfile-bump`
make: *** [Makevars:26: rust/target/release/libenderecobr.a] Error 101
ERROR: compilation failed for package ‘enderecobr’
* removing ‘/tmp/workdir/enderecobr/old/enderecobr.Rcheck/enderecobr’


```
# ER

<details>

* Version: 1.1.2
* GitHub: NA
* Source code: https://github.com/cran/ER
* Date/Publication: 2025-07-28 22:10:08 UTC
* Number of recursive dependencies: 40

Run `revdepcheck::cloud_details(, "ER")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/ER/new/ER.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘ER/DESCRIPTION’ ... OK
...
* this is package ‘ER’ version ‘1.1.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘plsVarSel’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/ER/old/ER.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘ER/DESCRIPTION’ ... OK
...
* this is package ‘ER’ version ‘1.1.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘plsVarSel’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# eye

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/eye
* Number of recursive dependencies: 58

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

* Version: 0.5
* GitHub: https://github.com/AFarukKILIC/FAfA
* Source code: https://github.com/cran/FAfA
* Date/Publication: 2025-12-15 13:00:02 UTC
* Number of recursive dependencies: 273

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
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘FAfA/DESCRIPTION’ ... OK
...
* this is package ‘FAfA’ version ‘0.5’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'EGAnet', 'MBESS', 'energy', 'semPlot'

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
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘FAfA/DESCRIPTION’ ... OK
...
* this is package ‘FAfA’ version ‘0.5’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'EGAnet', 'MBESS', 'energy', 'semPlot'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# fastei

<details>

* Version: 0.0.0.12
* GitHub: https://github.com/DanielHermosilla/ecological-inference-elections
* Source code: https://github.com/cran/fastei
* Date/Publication: 2026-01-10 03:10:02 UTC
* Number of recursive dependencies: 57

Run `revdepcheck::cloud_details(, "fastei")` for more info

</details>

## In both

*   checking whether package ‘fastei’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/fastei/new/fastei.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘fastei’ ...
** package ‘fastei’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’
using C++ compiler: ‘g++ (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’
gcc -I"/opt/R/4.4.0/lib/R/include" -DNDEBUG  -I'/tmp/r-deps/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c LP.c -o LP.o
gcc -I"/opt/R/4.4.0/lib/R/include" -DNDEBUG  -I'/tmp/r-deps/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c MCMC.c -o MCMC.o
In file included from stb_ds.h:398,
                 from MCMC.c:25:
...
      |                                        ^~~~~~~~~~
stb_ds.h:422:17: note: in expansion of macro ‘stbds_arrfree’
  422 | #define arrfree stbds_arrfree
      |                 ^~~~~~~~~~~~~
MCMC.c:375:13: note: in expansion of macro ‘arrfree’
  375 |             arrfree(hashTable[i].value);
      |             ^~~~~~~
make: *** [/opt/R/4.4.0/lib/R/etc/Makeconf:195: MCMC.o] Error 1
ERROR: compilation failed for package ‘fastei’
* removing ‘/tmp/workdir/fastei/new/fastei.Rcheck/fastei’


```
### CRAN

```
* installing *source* package ‘fastei’ ...
** package ‘fastei’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’
using C++ compiler: ‘g++ (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’
gcc -I"/opt/R/4.4.0/lib/R/include" -DNDEBUG  -I'/tmp/r-deps/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c LP.c -o LP.o
gcc -I"/opt/R/4.4.0/lib/R/include" -DNDEBUG  -I'/tmp/r-deps/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c MCMC.c -o MCMC.o
In file included from stb_ds.h:398,
                 from MCMC.c:25:
...
      |                                        ^~~~~~~~~~
stb_ds.h:422:17: note: in expansion of macro ‘stbds_arrfree’
  422 | #define arrfree stbds_arrfree
      |                 ^~~~~~~~~~~~~
MCMC.c:375:13: note: in expansion of macro ‘arrfree’
  375 |             arrfree(hashTable[i].value);
      |             ^~~~~~~
make: *** [/opt/R/4.4.0/lib/R/etc/Makeconf:195: MCMC.o] Error 1
ERROR: compilation failed for package ‘fastei’
* removing ‘/tmp/workdir/fastei/old/fastei.Rcheck/fastei’


```
# fastqrs

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/fastqrs
* Date/Publication: 2025-04-16 20:10:02 UTC
* Number of recursive dependencies: 104

Run `revdepcheck::cloud_details(, "fastqrs")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/fastqrs/new/fastqrs.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘fastqrs/DESCRIPTION’ ... OK
...
* this is package ‘fastqrs’ version ‘1.0.0’
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
* using log directory ‘/tmp/workdir/fastqrs/old/fastqrs.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘fastqrs/DESCRIPTION’ ... OK
...
* this is package ‘fastqrs’ version ‘1.0.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘copula’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# FCPS

<details>

* Version: 1.3.6
* GitHub: https://github.com/Mthrun/FCPS
* Source code: https://github.com/cran/FCPS
* Date/Publication: 2026-03-26 11:10:02 UTC
* Number of recursive dependencies: 266

Run `revdepcheck::cloud_details(, "FCPS")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/FCPS/new/FCPS.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘FCPS/DESCRIPTION’ ... OK
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
* using log directory ‘/tmp/workdir/FCPS/old/FCPS.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘FCPS/DESCRIPTION’ ... OK
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
# fddm

<details>

* Version: 1.0-2
* GitHub: https://github.com/rtdists/fddm
* Source code: https://github.com/cran/fddm
* Date/Publication: 2024-07-02 16:00:07 UTC
* Number of recursive dependencies: 88

Run `revdepcheck::cloud_details(, "fddm")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/fddm/new/fddm.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘fddm/DESCRIPTION’ ... OK
...
--- failed re-building ‘validity.Rmd’

SUMMARY: processing the following file failed:
  ‘validity.Rmd’

Error: Vignette re-building failed.
Execution halted

* DONE
Status: 1 ERROR, 2 NOTEs





```
### CRAN

```
* using log directory ‘/tmp/workdir/fddm/old/fddm.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘fddm/DESCRIPTION’ ... OK
...
--- failed re-building ‘validity.Rmd’

SUMMARY: processing the following file failed:
  ‘validity.Rmd’

Error: Vignette re-building failed.
Execution halted

* DONE
Status: 1 ERROR, 2 NOTEs





```
# FeatureImpCluster

<details>

* Version: 0.1.5
* GitHub: NA
* Source code: https://github.com/cran/FeatureImpCluster
* Date/Publication: 2021-10-20 16:20:02 UTC
* Number of recursive dependencies: 95

Run `revdepcheck::cloud_details(, "FeatureImpCluster")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/FeatureImpCluster/new/FeatureImpCluster.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘FeatureImpCluster/DESCRIPTION’ ... OK
...
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* DONE
Status: OK





```
### CRAN

```
* using log directory ‘/tmp/workdir/FeatureImpCluster/old/FeatureImpCluster.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘FeatureImpCluster/DESCRIPTION’ ... OK
...
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* DONE
Status: OK





```
# ferrn

<details>

* Version: 0.3.0
* GitHub: https://github.com/huizezhang-sherry/ferrn
* Source code: https://github.com/cran/ferrn
* Date/Publication: 2025-11-20 22:10:10 UTC
* Number of recursive dependencies: 146

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
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘ferrn/DESCRIPTION’ ... OK
...
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking LazyData ... OK
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
* using log directory ‘/tmp/workdir/ferrn/old/ferrn.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘ferrn/DESCRIPTION’ ... OK
...
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking LazyData ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* DONE
Status: OK





```
# ffp

<details>

* Version: 0.2.2
* GitHub: https://github.com/Reckziegel/FFP
* Source code: https://github.com/cran/ffp
* Date/Publication: 2022-09-29 15:10:06 UTC
* Number of recursive dependencies: 104

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
* running under: Ubuntu 24.04.3 LTS
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
* running under: Ubuntu 24.04.3 LTS
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
# fio

<details>

* Version: 1.0.0
* GitHub: https://github.com/albersonmiranda/fio
* Source code: https://github.com/cran/fio
* Date/Publication: 2026-02-27 08:40:02 UTC
* Number of recursive dependencies: 89

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
# fitteR

<details>

* Version: 0.2.0
* GitHub: NA
* Source code: https://github.com/cran/fitteR
* Date/Publication: 2022-02-22 12:00:02 UTC
* Number of recursive dependencies: 249

Run `revdepcheck::cloud_details(, "fitteR")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/fitteR/new/fitteR.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘fitteR/DESCRIPTION’ ... OK
...
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking R/sysdata.rda ... OK
* checking examples ... OK
* DONE
Status: 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/fitteR/old/fitteR.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘fitteR/DESCRIPTION’ ... OK
...
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking R/sysdata.rda ... OK
* checking examples ... OK
* DONE
Status: 1 NOTE





```
# fozziejoin

<details>

* Version: 0.0.13
* GitHub: https://github.com/fozzieverse/fozziejoin
* Source code: https://github.com/cran/fozziejoin
* Date/Publication: 2026-03-09 16:20:02 UTC
* Number of recursive dependencies: 63

Run `revdepcheck::cloud_details(, "fozziejoin")` for more info

</details>

## In both

*   checking whether package ‘fozziejoin’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/fozziejoin/new/fozziejoin.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘fozziejoin’ ...
** package ‘fozziejoin’ successfully unpacked and MD5 sums checked
** using staged installation
Using cargo 1.75.0
Using rustc 1.75.0 (82e1608df 2023-12-21) (built from a source tarball)
Building for CRAN.
Writing `src/Makevars`.
`tools/config.R` has finished.
** libs
using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’
...
export CARGO_HOME=/tmp/workdir/fozziejoin/new/fozziejoin.Rcheck/00_pkg_src/fozziejoin/src/.cargo && \
export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/root/.cargo/bin" && \
RUSTFLAGS=" --print=native-static-libs" cargo build -j 2 --offline --lib --release --manifest-path=./rust/Cargo.toml --target-dir ./rust/target 
error: failed to parse lock file at: /tmp/workdir/fozziejoin/new/fozziejoin.Rcheck/00_pkg_src/fozziejoin/src/rust/Cargo.lock

Caused by:
  lock file version 4 requires `-Znext-lockfile-bump`
make: *** [Makevars:26: rust/target/release/libfozziejoin.a] Error 101
ERROR: compilation failed for package ‘fozziejoin’
* removing ‘/tmp/workdir/fozziejoin/new/fozziejoin.Rcheck/fozziejoin’


```
### CRAN

```
* installing *source* package ‘fozziejoin’ ...
** package ‘fozziejoin’ successfully unpacked and MD5 sums checked
** using staged installation
Using cargo 1.75.0
Using rustc 1.75.0 (82e1608df 2023-12-21) (built from a source tarball)
Building for CRAN.
Writing `src/Makevars`.
`tools/config.R` has finished.
** libs
using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’
...
export CARGO_HOME=/tmp/workdir/fozziejoin/old/fozziejoin.Rcheck/00_pkg_src/fozziejoin/src/.cargo && \
export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/root/.cargo/bin" && \
RUSTFLAGS=" --print=native-static-libs" cargo build -j 2 --offline --lib --release --manifest-path=./rust/Cargo.toml --target-dir ./rust/target 
error: failed to parse lock file at: /tmp/workdir/fozziejoin/old/fozziejoin.Rcheck/00_pkg_src/fozziejoin/src/rust/Cargo.lock

Caused by:
  lock file version 4 requires `-Znext-lockfile-bump`
make: *** [Makevars:26: rust/target/release/libfozziejoin.a] Error 101
ERROR: compilation failed for package ‘fozziejoin’
* removing ‘/tmp/workdir/fozziejoin/old/fozziejoin.Rcheck/fozziejoin’


```
# gasmodel

<details>

* Version: 0.6.2
* GitHub: https://github.com/vladimirholy/gasmodel
* Source code: https://github.com/cran/gasmodel
* Date/Publication: 2025-08-18 20:50:08 UTC
* Number of recursive dependencies: 85

Run `revdepcheck::cloud_details(, "gasmodel")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/gasmodel/new/gasmodel.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘gasmodel/DESCRIPTION’ ... OK
...
* this is package ‘gasmodel’ version ‘0.6.2’
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
* using log directory ‘/tmp/workdir/gasmodel/old/gasmodel.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘gasmodel/DESCRIPTION’ ... OK
...
* this is package ‘gasmodel’ version ‘0.6.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘copula’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# GDINA

<details>

* Version: 2.9.12
* GitHub: https://github.com/Wenchao-Ma/GDINA
* Source code: https://github.com/cran/GDINA
* Date/Publication: 2025-07-02 17:00:02 UTC
* Number of recursive dependencies: 149

Run `revdepcheck::cloud_details(, "GDINA")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/GDINA/new/GDINA.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘GDINA/DESCRIPTION’ ... OK
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
* using log directory ‘/tmp/workdir/GDINA/old/GDINA.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘GDINA/DESCRIPTION’ ... OK
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
# gemR

<details>

* Version: 1.2.2
* GitHub: NA
* Source code: https://github.com/cran/gemR
* Date/Publication: 2025-09-04 20:00:02 UTC
* Number of recursive dependencies: 94

Run `revdepcheck::cloud_details(, "gemR")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/gemR/new/gemR.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘gemR/DESCRIPTION’ ... OK
...
* this is package ‘gemR’ version ‘1.2.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘plsVarSel’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/gemR/old/gemR.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘gemR/DESCRIPTION’ ... OK
...
* this is package ‘gemR’ version ‘1.2.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘plsVarSel’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





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
* running under: Ubuntu 24.04.3 LTS
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
* running under: Ubuntu 24.04.3 LTS
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
# GeneralizedUmatrixGPU

<details>

* Version: 0.1.14
* GitHub: NA
* Source code: https://github.com/cran/GeneralizedUmatrixGPU
* Date/Publication: 2026-01-27 15:30:02 UTC
* Number of recursive dependencies: 85

Run `revdepcheck::cloud_details(, "GeneralizedUmatrixGPU")` for more info

</details>

## In both

*   checking whether package ‘GeneralizedUmatrixGPU’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/GeneralizedUmatrixGPU/new/GeneralizedUmatrixGPU.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘GeneralizedUmatrixGPU’ ...
** package ‘GeneralizedUmatrixGPU’ successfully unpacked and MD5 sums checked
** using staged installation
configure: building makevars
System detected: Linux
End of configure
** libs
using C++ compiler: ‘g++ (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’
using C++17
g++ -std=gnu++17 -I"/opt/R/4.4.0/lib/R/include" -DNDEBUG  -I'/tmp/r-deps/Rcpp/include' -I'/tmp/r-deps/RcppArmadillo/include' -I'/tmp/r-deps/RcppParallel/include' -I/usr/local/include    -fpic  -g -O2   -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.4.0/lib/R/include" -DNDEBUG  -I'/tmp/r-deps/Rcpp/include' -I'/tmp/r-deps/RcppArmadillo/include' -I'/tmp/r-deps/RcppParallel/include' -I/usr/local/include    -fpic  -g -O2   -c trainSESOM.cpp -o trainSESOM.o
In file included from trainSESOM.cpp:2:
ocl.h:9:10: fatal error: CL/cl.h: No such file or directory
    9 | #include <CL/cl.h>
      |          ^~~~~~~~~
compilation terminated.
make: *** [/opt/R/4.4.0/lib/R/etc/Makeconf:204: trainSESOM.o] Error 1
ERROR: compilation failed for package ‘GeneralizedUmatrixGPU’
* removing ‘/tmp/workdir/GeneralizedUmatrixGPU/new/GeneralizedUmatrixGPU.Rcheck/GeneralizedUmatrixGPU’


```
### CRAN

```
* installing *source* package ‘GeneralizedUmatrixGPU’ ...
** package ‘GeneralizedUmatrixGPU’ successfully unpacked and MD5 sums checked
** using staged installation
configure: building makevars
System detected: Linux
End of configure
** libs
using C++ compiler: ‘g++ (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’
using C++17
g++ -std=gnu++17 -I"/opt/R/4.4.0/lib/R/include" -DNDEBUG  -I'/tmp/r-deps/Rcpp/include' -I'/tmp/r-deps/RcppArmadillo/include' -I'/tmp/r-deps/RcppParallel/include' -I/usr/local/include    -fpic  -g -O2   -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.4.0/lib/R/include" -DNDEBUG  -I'/tmp/r-deps/Rcpp/include' -I'/tmp/r-deps/RcppArmadillo/include' -I'/tmp/r-deps/RcppParallel/include' -I/usr/local/include    -fpic  -g -O2   -c trainSESOM.cpp -o trainSESOM.o
In file included from trainSESOM.cpp:2:
ocl.h:9:10: fatal error: CL/cl.h: No such file or directory
    9 | #include <CL/cl.h>
      |          ^~~~~~~~~
compilation terminated.
make: *** [/opt/R/4.4.0/lib/R/etc/Makeconf:204: trainSESOM.o] Error 1
ERROR: compilation failed for package ‘GeneralizedUmatrixGPU’
* removing ‘/tmp/workdir/GeneralizedUmatrixGPU/old/GeneralizedUmatrixGPU.Rcheck/GeneralizedUmatrixGPU’


```
# ggpicrust2

<details>

* Version: 2.5.10
* GitHub: https://github.com/cafferychen777/ggpicrust2
* Source code: https://github.com/cran/ggpicrust2
* Date/Publication: 2026-02-12 16:40:16 UTC
* Number of recursive dependencies: 317

Run `revdepcheck::cloud_details(, "ggpicrust2")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/ggpicrust2/new/ggpicrust2.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘ggpicrust2/DESCRIPTION’ ... OK
...
  
  [ FAIL 8 | WARN 2 | SKIP 9 | PASS 151 ]
  Error:
  ! Test failures.
  Execution halted
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 ERROR, 2 NOTEs





```
### CRAN

```
* using log directory ‘/tmp/workdir/ggpicrust2/old/ggpicrust2.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘ggpicrust2/DESCRIPTION’ ... OK
...
  
  [ FAIL 8 | WARN 2 | SKIP 9 | PASS 151 ]
  Error:
  ! Test failures.
  Execution halted
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 ERROR, 2 NOTEs





```
# ggsem

<details>

* Version: 0.9.9
* GitHub: https://github.com/smin95/ggsem
* Source code: https://github.com/cran/ggsem
* Date/Publication: 2026-01-28 07:40:02 UTC
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
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘ggsem/DESCRIPTION’ ... OK
...
* this is package ‘ggsem’ version ‘0.9.9’
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
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘ggsem/DESCRIPTION’ ... OK
...
* this is package ‘ggsem’ version ‘0.9.9’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘semPlot’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# GJRM

<details>

* Version: 0.2-6.8
* GitHub: NA
* Source code: https://github.com/cran/GJRM
* Date/Publication: 2025-06-23 21:20:02 UTC
* Number of recursive dependencies: 58

Run `revdepcheck::cloud_details(, "GJRM")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/GJRM/new/GJRM.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘GJRM/DESCRIPTION’ ... OK
...
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘copula’

Package which this enhances but not available for checking: ‘sp’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/GJRM/old/GJRM.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘GJRM/DESCRIPTION’ ... OK
...
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘copula’

Package which this enhances but not available for checking: ‘sp’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





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
* running under: Ubuntu 24.04.3 LTS
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
* running under: Ubuntu 24.04.3 LTS
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
* Number of recursive dependencies: 176

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
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘gratia/DESCRIPTION’ ... OK
...
  'soap-films/draw-smooth-estimates-so-soap-film-bndry.svg', and
  'soap-films/draw-smooth-estimates-so-soap-film.svg'
  Error:
  ! Test failures.
  Execution halted
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/gratia/old/gratia.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘gratia/DESCRIPTION’ ... OK
...
  'soap-films/draw-smooth-estimates-so-soap-film-bndry.svg', and
  'soap-films/draw-smooth-estimates-so-soap-film.svg'
  Error:
  ! Test failures.
  Execution halted
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 ERROR





```
# GRIDCOPULA

<details>

* Version: 1.1.0
* GitHub: NA
* Source code: https://github.com/cran/GRIDCOPULA
* Date/Publication: 2025-09-13 22:40:12 UTC
* Number of recursive dependencies: 44

Run `revdepcheck::cloud_details(, "GRIDCOPULA")` for more info

</details>

## In both

*   checking whether package ‘GRIDCOPULA’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/GRIDCOPULA/new/GRIDCOPULA.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘GRIDCOPULA’ ...
** package ‘GRIDCOPULA’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘gsl’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘GRIDCOPULA’
* removing ‘/tmp/workdir/GRIDCOPULA/new/GRIDCOPULA.Rcheck/GRIDCOPULA’


```
### CRAN

```
* installing *source* package ‘GRIDCOPULA’ ...
** package ‘GRIDCOPULA’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘gsl’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘GRIDCOPULA’
* removing ‘/tmp/workdir/GRIDCOPULA/old/GRIDCOPULA.Rcheck/GRIDCOPULA’


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
* Number of recursive dependencies: 18

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
using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’
...
export CARGO_HOME=/tmp/workdir/h3o/new/h3o.Rcheck/00_pkg_src/h3o/src/.cargo && \
export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/root/.cargo/bin" && \
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
using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’
...
export CARGO_HOME=/tmp/workdir/h3o/old/h3o.Rcheck/00_pkg_src/h3o/src/.cargo && \
export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/root/.cargo/bin" && \
RUSTFLAGS=" --print=native-static-libs" cargo build -j 2 --offline --lib --release --manifest-path=./rust/Cargo.toml --target-dir ./rust/target 
error: failed to parse lock file at: /tmp/workdir/h3o/old/h3o.Rcheck/00_pkg_src/h3o/src/rust/Cargo.lock

Caused by:
  lock file version 4 requires `-Znext-lockfile-bump`
make: *** [Makevars:26: rust/target/release/libh3o.a] Error 101
ERROR: compilation failed for package ‘h3o’
* removing ‘/tmp/workdir/h3o/old/h3o.Rcheck/h3o’


```
# harbinger

<details>

* Version: 1.2.767
* GitHub: https://github.com/cefet-rj-dal/harbinger
* Source code: https://github.com/cran/harbinger
* Date/Publication: 2026-02-11 01:20:02 UTC
* Number of recursive dependencies: 189

Run `revdepcheck::cloud_details(, "harbinger")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/harbinger/new/harbinger.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘harbinger/DESCRIPTION’ ... OK
...
* this is package ‘harbinger’ version ‘1.2.767’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'tspredit', 'daltoolbox'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/harbinger/old/harbinger.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘harbinger/DESCRIPTION’ ... OK
...
* this is package ‘harbinger’ version ‘1.2.767’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'tspredit', 'daltoolbox'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# hbsaems

<details>

* Version: 0.1.1
* GitHub: https://github.com/madsyair/hbsaems
* Source code: https://github.com/cran/hbsaems
* Date/Publication: 2025-07-18 14:40:02 UTC
* Number of recursive dependencies: 178

Run `revdepcheck::cloud_details(, "hbsaems")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/hbsaems/new/hbsaems.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘hbsaems/DESCRIPTION’ ... OK
...
* this is package ‘hbsaems’ version ‘0.1.1’
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
* using log directory ‘/tmp/workdir/hbsaems/old/hbsaems.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘hbsaems/DESCRIPTION’ ... OK
...
* this is package ‘hbsaems’ version ‘0.1.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘energy’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# heimdall

<details>

* Version: 1.2.727
* GitHub: https://github.com/cefet-rj-dal/heimdall
* Source code: https://github.com/cran/heimdall
* Date/Publication: 2026-03-10 18:40:02 UTC
* Number of recursive dependencies: 122

Run `revdepcheck::cloud_details(, "heimdall")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/heimdall/new/heimdall.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘heimdall/DESCRIPTION’ ... OK
...
* this is package ‘heimdall’ version ‘1.2.727’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘daltoolbox’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/heimdall/old/heimdall.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘heimdall/DESCRIPTION’ ... OK
...
* this is package ‘heimdall’ version ‘1.2.727’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘daltoolbox’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# iccCompare

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/iccCompare
* Number of recursive dependencies: 48

Run `revdepcheck::cloud_details(, "iccCompare")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# imt

<details>

* Version: 1.0.0
* GitHub: https://github.com/google/imt
* Source code: https://github.com/cran/imt
* Date/Publication: 2024-09-02 21:40:10 UTC
* Number of recursive dependencies: 131

Run `revdepcheck::cloud_details(, "imt")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```
* using log directory ‘/tmp/workdir/imt/old/imt.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘imt/DESCRIPTION’ ... OK
...
GNU make is a SystemRequirements.
* checking for portable use of $(BLAS_LIBS) and $(LAPACK_LIBS) ... OK
* checking use of PKG_*FLAGS in Makefiles ... OK
* checking compiled code ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* DONE
Status: 2 NOTEs





```
# IndGenErrors

<details>

* Version: 0.1.6
* GitHub: NA
* Source code: https://github.com/cran/IndGenErrors
* Date/Publication: 2025-01-31 14:30:04 UTC
* Number of recursive dependencies: 41

Run `revdepcheck::cloud_details(, "IndGenErrors")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/IndGenErrors/new/IndGenErrors.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘IndGenErrors/DESCRIPTION’ ... OK
...
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘IndGenErrors’ can be installed ... ERROR
Installation failed.
See ‘/tmp/workdir/IndGenErrors/new/IndGenErrors.Rcheck/00install.out’ for details.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/IndGenErrors/old/IndGenErrors.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘IndGenErrors/DESCRIPTION’ ... OK
...
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘IndGenErrors’ can be installed ... ERROR
Installation failed.
See ‘/tmp/workdir/IndGenErrors/old/IndGenErrors.Rcheck/00install.out’ for details.
* DONE
Status: 1 ERROR





```
# intervalpsych

<details>

* Version: 0.1.0
* GitHub: https://github.com/matthiaskloft/intervalpsych
* Source code: https://github.com/cran/intervalpsych
* Date/Publication: 2025-07-08 09:20:02 UTC
* Number of recursive dependencies: 64

Run `revdepcheck::cloud_details(, "intervalpsych")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```
* using log directory ‘/tmp/workdir/intervalpsych/old/intervalpsych.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘intervalpsych/DESCRIPTION’ ... OK
...
GNU make is a SystemRequirements.
* checking for portable use of $(BLAS_LIBS) and $(LAPACK_LIBS) ... OK
* checking use of PKG_*FLAGS in Makefiles ... OK
* checking compiled code ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* DONE
Status: 2 NOTEs





```
# irt

<details>

* Version: 0.2.9
* GitHub: https://github.com/egonulates/irt
* Source code: https://github.com/cran/irt
* Date/Publication: 2024-02-20 20:40:02 UTC
* Number of recursive dependencies: 47

Run `revdepcheck::cloud_details(, "irt")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```
* using log directory ‘/tmp/workdir/irt/old/irt.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘irt/DESCRIPTION’ ... OK
...
* checking compiled code ... OK
* checking usage of KIND in Fortran files ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 NOTE





```
# irtQ

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/irtQ
* Date/Publication: 2025-07-17 04:00:02 UTC
* Number of recursive dependencies: 96

Run `revdepcheck::cloud_details(, "irtQ")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```
* using log directory ‘/tmp/workdir/irtQ/old/irtQ.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘irtQ/DESCRIPTION’ ... OK
...
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking LazyData ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking examples ... OK
* DONE
Status: 1 NOTE





```
# Kcop

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/Kcop
* Date/Publication: 2022-11-16 15:00:20 UTC
* Number of recursive dependencies: 30

Run `revdepcheck::cloud_details(, "Kcop")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/Kcop/new/Kcop.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘Kcop/DESCRIPTION’ ... OK
...
* this is package ‘Kcop’ version ‘1.0.0’
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
* using log directory ‘/tmp/workdir/Kcop/old/Kcop.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘Kcop/DESCRIPTION’ ... OK
...
* this is package ‘Kcop’ version ‘1.0.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘copula’

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
* Number of recursive dependencies: 170

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
* running under: Ubuntu 24.04.3 LTS
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
* running under: Ubuntu 24.04.3 LTS
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
# LongDecompHE

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/LongDecompHE
* Date/Publication: 2025-07-03 15:20:02 UTC
* Number of recursive dependencies: 43

Run `revdepcheck::cloud_details(, "LongDecompHE")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/LongDecompHE/new/LongDecompHE.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘LongDecompHE/DESCRIPTION’ ... OK
...
* this is package ‘LongDecompHE’ version ‘0.1.0’
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
* using log directory ‘/tmp/workdir/LongDecompHE/old/LongDecompHE.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘LongDecompHE/DESCRIPTION’ ... OK
...
* this is package ‘LongDecompHE’ version ‘0.1.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘copula’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# MBAnalysis

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/MBAnalysis
* Number of recursive dependencies: 19

Run `revdepcheck::cloud_details(, "MBAnalysis")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# MD2sample

<details>

* Version: 1.2.0
* GitHub: NA
* Source code: https://github.com/cran/MD2sample
* Date/Publication: 2026-03-28 06:50:06 UTC
* Number of recursive dependencies: 85

Run `revdepcheck::cloud_details(, "MD2sample")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/MD2sample/new/MD2sample.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘MD2sample/DESCRIPTION’ ... OK
...
* this is package ‘MD2sample’ version ‘1.2.0’
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
* using log directory ‘/tmp/workdir/MD2sample/old/MD2sample.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘MD2sample/DESCRIPTION’ ... OK
...
* this is package ‘MD2sample’ version ‘1.2.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘copula’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# MDgof

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/MDgof
* Date/Publication: 2026-02-12 20:40:03 UTC
* Number of recursive dependencies: 84

Run `revdepcheck::cloud_details(, "MDgof")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/MDgof/new/MDgof.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘MDgof/DESCRIPTION’ ... OK
...
* this is package ‘MDgof’ version ‘1.0.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'copula', 'MD2sample'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/MDgof/old/MDgof.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘MDgof/DESCRIPTION’ ... OK
...
* this is package ‘MDgof’ version ‘1.0.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'copula', 'MD2sample'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# MiscMetabar

<details>

* Version: 0.14.4
* GitHub: https://github.com/adrientaudiere/MiscMetabar
* Source code: https://github.com/cran/MiscMetabar
* Date/Publication: 2025-09-30 15:30:02 UTC
* Number of recursive dependencies: 424

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
* running under: Ubuntu 24.04.3 LTS
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
Status: 3 NOTEs





```
### CRAN

```
* using log directory ‘/tmp/workdir/MiscMetabar/old/MiscMetabar.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
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
Status: 3 NOTEs





```
# MixedIndTests

<details>

* Version: 1.2.0
* GitHub: NA
* Source code: https://github.com/cran/MixedIndTests
* Date/Publication: 2024-02-14 00:04:09 UTC
* Number of recursive dependencies: 40

Run `revdepcheck::cloud_details(, "MixedIndTests")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/MixedIndTests/new/MixedIndTests.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘MixedIndTests/DESCRIPTION’ ... OK
...
* this is package ‘MixedIndTests’ version ‘1.2.0’
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
* using log directory ‘/tmp/workdir/MixedIndTests/old/MixedIndTests.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘MixedIndTests/DESCRIPTION’ ... OK
...
* this is package ‘MixedIndTests’ version ‘1.2.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘copula’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# MixMashNet

<details>

* Version: 0.6.0
* GitHub: https://github.com/ARCbiostat/MixMashNet
* Source code: https://github.com/cran/MixMashNet
* Date/Publication: 2026-03-03 10:40:09 UTC
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
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘MixMashNet/DESCRIPTION’ ... OK
...
* this is package ‘MixMashNet’ version ‘0.6.0’
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
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘MixMashNet/DESCRIPTION’ ... OK
...
* this is package ‘MixMashNet’ version ‘0.6.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘EGAnet’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# ModStatR

<details>

* Version: 1.4.1
* GitHub: https://github.com/fbertran/ModStatR
* Source code: https://github.com/cran/ModStatR
* Date/Publication: 2025-09-12 22:50:26 UTC
* Number of recursive dependencies: 310

Run `revdepcheck::cloud_details(, "ModStatR")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/ModStatR/new/ModStatR.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘ModStatR/DESCRIPTION’ ... OK
...
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘gsl’

Packages suggested but not available for checking: 'MBESS', 'MVN'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/ModStatR/old/ModStatR.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘ModStatR/DESCRIPTION’ ... OK
...
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘gsl’

Packages suggested but not available for checking: 'MBESS', 'MVN'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# MultANOVA

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/MultANOVA
* Number of recursive dependencies: 20

Run `revdepcheck::cloud_details(, "MultANOVA")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# MultiResponseR

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/MultiResponseR
* Number of recursive dependencies: 69

Run `revdepcheck::cloud_details(, "MultiResponseR")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# MVN

<details>

* Version: 6.3
* GitHub: https://github.com/selcukorkmaz/MVN
* Source code: https://github.com/cran/MVN
* Date/Publication: 2026-01-13 06:10:41 UTC
* Number of recursive dependencies: 159

Run `revdepcheck::cloud_details(, "MVN")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/MVN/new/MVN.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘MVN/DESCRIPTION’ ... OK
...
* this is package ‘MVN’ version ‘6.3’
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
* using log directory ‘/tmp/workdir/MVN/old/MVN.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘MVN/DESCRIPTION’ ... OK
...
* this is package ‘MVN’ version ‘6.3’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘energy’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# OlinkAnalyze

<details>

* Version: 5.0.0
* GitHub: https://github.com/Olink-Proteomics/OlinkRPackage
* Source code: https://github.com/cran/OlinkAnalyze
* Date/Publication: 2026-03-28 12:00:02 UTC
* Number of recursive dependencies: 221

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
* running under: Ubuntu 24.04.3 LTS
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
* running under: Ubuntu 24.04.3 LTS
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
# OpenMx

<details>

* Version: 2.22.11
* GitHub: https://github.com/OpenMx/OpenMx
* Source code: https://github.com/cran/OpenMx
* Date/Publication: 2026-03-08 06:10:55 UTC
* Number of recursive dependencies: 99

Run `revdepcheck::cloud_details(, "OpenMx")` for more info

</details>

## In both

*   checking whether package ‘OpenMx’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/OpenMx/new/OpenMx.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘OpenMx’ ...
** package ‘OpenMx’ successfully unpacked and MD5 sums checked
** using staged installation
NOTE: ./configure is not an autoconf generated script.
Change default C/C++ compiler and default compile flags by editing ~/.R/Makevars
** libs
using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’
using Fortran compiler: ‘GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’
using C++ compiler: ‘g++ (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’
using C++17
...
      |                                                          ^~~~
/tmp/r-deps/RcppEigen/include/Eigen/src/Core/products/SelfadjointMatrixMatrix.h:51:58: warning: ignoring attributes on template argument ‘Eigen::internal::unpacket_traits<__vector(2) double>::half’ {aka ‘__m128d’} [-Wignored-attributes]
/tmp/r-deps/RcppEigen/include/Eigen/src/Core/products/SelfadjointMatrixMatrix.h:52:64: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__m128d’} [-Wignored-attributes]
   52 |            QuarterPacketSize = unpacket_traits<QuarterPacket>::size,
      |                                                                ^~~~
/tmp/r-deps/RcppEigen/include/Eigen/src/Core/products/SelfadjointMatrixMatrix.h:52:64: warning: ignoring attributes on template argument ‘Eigen::internal::unpacket_traits<__vector(2) double>::half’ {aka ‘__m128d’} [-Wignored-attributes]
/tmp/r-deps/RcppEigen/include/Eigen/src/Core/products/SelfadjointMatrixMatrix.h:52:64: warning: ignoring attributes on template argument ‘Eigen::internal::unpacket_traits<__vector(2) double>::half’ {aka ‘__m128d’} [-Wignored-attributes]
make: *** [/opt/R/4.4.0/lib/R/etc/Makeconf:204: omxData.o] Error 1
ERROR: compilation failed for package ‘OpenMx’
* removing ‘/tmp/workdir/OpenMx/new/OpenMx.Rcheck/OpenMx’


```
### CRAN

```
* installing *source* package ‘OpenMx’ ...
** package ‘OpenMx’ successfully unpacked and MD5 sums checked
** using staged installation
NOTE: ./configure is not an autoconf generated script.
Change default C/C++ compiler and default compile flags by editing ~/.R/Makevars
** libs
using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’
using Fortran compiler: ‘GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’
using C++ compiler: ‘g++ (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’
using C++17
...
      |                                                          ^~~~
/tmp/r-deps/RcppEigen/include/Eigen/src/Core/products/SelfadjointMatrixMatrix.h:51:58: warning: ignoring attributes on template argument ‘Eigen::internal::unpacket_traits<__vector(2) double>::half’ {aka ‘__m128d’} [-Wignored-attributes]
/tmp/r-deps/RcppEigen/include/Eigen/src/Core/products/SelfadjointMatrixMatrix.h:52:64: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__m128d’} [-Wignored-attributes]
   52 |            QuarterPacketSize = unpacket_traits<QuarterPacket>::size,
      |                                                                ^~~~
/tmp/r-deps/RcppEigen/include/Eigen/src/Core/products/SelfadjointMatrixMatrix.h:52:64: warning: ignoring attributes on template argument ‘Eigen::internal::unpacket_traits<__vector(2) double>::half’ {aka ‘__m128d’} [-Wignored-attributes]
/tmp/r-deps/RcppEigen/include/Eigen/src/Core/products/SelfadjointMatrixMatrix.h:52:64: warning: ignoring attributes on template argument ‘Eigen::internal::unpacket_traits<__vector(2) double>::half’ {aka ‘__m128d’} [-Wignored-attributes]
make: *** [/opt/R/4.4.0/lib/R/etc/Makeconf:204: omxData.o] Error 1
ERROR: compilation failed for package ‘OpenMx’
* removing ‘/tmp/workdir/OpenMx/old/OpenMx.Rcheck/OpenMx’


```
# PPbigdata

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/PPbigdata
* Date/Publication: 2024-04-30 11:42:50 UTC
* Number of recursive dependencies: 140

Run `revdepcheck::cloud_details(, "PPbigdata")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/PPbigdata/new/PPbigdata.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘PPbigdata/DESCRIPTION’ ... OK
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
* using log directory ‘/tmp/workdir/PPbigdata/old/PPbigdata.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘PPbigdata/DESCRIPTION’ ... OK
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
# QFASA

<details>

* Version: 1.2.1
* GitHub: https://github.com/cstewartGH/QFASA
* Source code: https://github.com/cran/QFASA
* Date/Publication: 2024-08-27 19:40:02 UTC
* Number of recursive dependencies: 138

Run `revdepcheck::cloud_details(, "QFASA")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/QFASA/new/QFASA.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘QFASA/DESCRIPTION’ ... OK
...
* this is package ‘QFASA’ version ‘1.2.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘Compositional’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/QFASA/old/QFASA.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘QFASA/DESCRIPTION’ ... OK
...
* this is package ‘QFASA’ version ‘1.2.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘Compositional’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# QuadratiK

<details>

* Version: 1.1.3
* GitHub: https://github.com/ropensci/QuadratiK
* Source code: https://github.com/cran/QuadratiK
* Date/Publication: 2025-02-04 21:10:02 UTC
* Number of recursive dependencies: 151

Run `revdepcheck::cloud_details(, "QuadratiK")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/QuadratiK/new/QuadratiK.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘QuadratiK/DESCRIPTION’ ... OK
...
--- finished re-building ‘wireless_clustering.Rmd’

SUMMARY: processing the following file failed:
  ‘uniformity.Rmd’

Error: Vignette re-building failed.
Execution halted

* DONE
Status: 1 ERROR, 2 NOTEs





```
### CRAN

```
* using log directory ‘/tmp/workdir/QuadratiK/old/QuadratiK.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘QuadratiK/DESCRIPTION’ ... OK
...
--- finished re-building ‘wireless_clustering.Rmd’

SUMMARY: processing the following file failed:
  ‘uniformity.Rmd’

Error: Vignette re-building failed.
Execution halted

* DONE
Status: 1 ERROR, 2 NOTEs





```
# RCTrep

<details>

* Version: 1.2.0
* GitHub: https://github.com/duolajiang/RCTrep
* Source code: https://github.com/cran/RCTrep
* Date/Publication: 2023-11-02 14:40:02 UTC
* Number of recursive dependencies: 183

Run `revdepcheck::cloud_details(, "RCTrep")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/RCTrep/new/RCTrep.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘RCTrep/DESCRIPTION’ ... OK
...
* this is package ‘RCTrep’ version ‘1.2.0’
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
* using log directory ‘/tmp/workdir/RCTrep/old/RCTrep.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘RCTrep/DESCRIPTION’ ... OK
...
* this is package ‘RCTrep’ version ‘1.2.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘copula’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# RegDDM

<details>

* Version: 1.1
* GitHub: https://github.com/biorabbit/RegDDM
* Source code: https://github.com/cran/RegDDM
* Date/Publication: 2025-07-01 18:40:02 UTC
* Number of recursive dependencies: 72

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
* running under: Ubuntu 24.04.3 LTS
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
* running under: Ubuntu 24.04.3 LTS
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
# ReporterScore

<details>

* Version: 0.2.5
* GitHub: https://github.com/Asa12138/ReporterScore
* Source code: https://github.com/cran/ReporterScore
* Date/Publication: 2026-02-20 14:50:14 UTC
* Number of recursive dependencies: 261

Run `revdepcheck::cloud_details(, "ReporterScore")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/ReporterScore/new/ReporterScore.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘ReporterScore/DESCRIPTION’ ... OK
...
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/ReporterScore/old/ReporterScore.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘ReporterScore/DESCRIPTION’ ... OK
...
* checking data for non-ASCII characters ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 NOTE





```
# Revticulate

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/Revticulate
* Date/Publication: 2022-03-15 23:30:07 UTC
* Number of recursive dependencies: 176

Run `revdepcheck::cloud_details(, "Revticulate")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/Revticulate/new/Revticulate.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘Revticulate/DESCRIPTION’ ... OK
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
Status: 2 NOTEs





```
### CRAN

```
* using log directory ‘/tmp/workdir/Revticulate/old/Revticulate.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘Revticulate/DESCRIPTION’ ... OK
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
Status: 2 NOTEs





```
# RGENERATEPREC

<details>

* Version: 1.3.2
* GitHub: NA
* Source code: https://github.com/cran/RGENERATEPREC
* Date/Publication: 2025-07-30 16:10:02 UTC
* Number of recursive dependencies: 120

Run `revdepcheck::cloud_details(, "RGENERATEPREC")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/RGENERATEPREC/new/RGENERATEPREC.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘RGENERATEPREC/DESCRIPTION’ ... OK
...
* checking extension type ... Package
* this is package ‘RGENERATEPREC’ version ‘1.3.2’
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
* using log directory ‘/tmp/workdir/RGENERATEPREC/old/RGENERATEPREC.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘RGENERATEPREC/DESCRIPTION’ ... OK
...
* checking extension type ... Package
* this is package ‘RGENERATEPREC’ version ‘1.3.2’
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘copula’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# RMOPI

<details>

* Version: 1.1
* GitHub: NA
* Source code: https://github.com/cran/RMOPI
* Date/Publication: 2022-08-22 12:50:08 UTC
* Number of recursive dependencies: 95

Run `revdepcheck::cloud_details(, "RMOPI")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/RMOPI/new/RMOPI.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘RMOPI/DESCRIPTION’ ... OK
...
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘RMOPI’ can be installed ... ERROR
Installation failed.
See ‘/tmp/workdir/RMOPI/new/RMOPI.Rcheck/00install.out’ for details.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/RMOPI/old/RMOPI.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘RMOPI/DESCRIPTION’ ... OK
...
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘RMOPI’ can be installed ... ERROR
Installation failed.
See ‘/tmp/workdir/RMOPI/old/RMOPI.Rcheck/00install.out’ for details.
* DONE
Status: 1 ERROR





```
# RSP

<details>

* Version: 0.5
* GitHub: NA
* Source code: https://github.com/cran/RSP
* Date/Publication: 2026-01-21 08:00:26 UTC
* Number of recursive dependencies: 256

Run `revdepcheck::cloud_details(, "RSP")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/RSP/new/RSP.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘RSP/DESCRIPTION’ ... OK
...
* this is package ‘RSP’ version ‘0.5’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'MVN', 'semPlot'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/RSP/old/RSP.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘RSP/DESCRIPTION’ ... OK
...
* this is package ‘RSP’ version ‘0.5’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'MVN', 'semPlot'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# RulesTools

<details>

* Version: 0.1.1
* GitHub: https://github.com/nikolett0203/RulesTools
* Source code: https://github.com/cran/RulesTools
* Date/Publication: 2025-01-28 15:50:06 UTC
* Number of recursive dependencies: 114

Run `revdepcheck::cloud_details(, "RulesTools")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/RulesTools/new/RulesTools.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘RulesTools/DESCRIPTION’ ... OK
...
* this is package ‘RulesTools’ version ‘0.1.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'arules', 'eulerr'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/RulesTools/old/RulesTools.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘RulesTools/DESCRIPTION’ ... OK
...
* this is package ‘RulesTools’ version ‘0.1.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'arules', 'eulerr'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





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
* running under: Ubuntu 24.04.3 LTS
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
* running under: Ubuntu 24.04.3 LTS
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
# SeuratExplorer

<details>

* Version: 0.1.4
* GitHub: https://github.com/fentouxungui/SeuratExplorer
* Source code: https://github.com/cran/SeuratExplorer
* Date/Publication: 2026-03-31 03:20:03 UTC
* Number of recursive dependencies: 208

Run `revdepcheck::cloud_details(, "SeuratExplorer")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/SeuratExplorer/new/SeuratExplorer.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘SeuratExplorer/DESCRIPTION’ ... OK
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
Status: 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/SeuratExplorer/old/SeuratExplorer.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘SeuratExplorer/DESCRIPTION’ ... OK
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
Status: 1 NOTE





```
# Signac

<details>

* Version: 1.17.0
* GitHub: https://github.com/stuart-lab/signac
* Source code: https://github.com/cran/Signac
* Date/Publication: 2026-04-01 10:00:03 UTC
* Number of recursive dependencies: 228

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
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘Signac/DESCRIPTION’ ... OK
...
* checking package dependencies ... ERROR
Package required but not available: ‘Seqinfo’

Packages suggested but not available for checking:
  'TFBSTools', 'motifmatchr'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/Signac/old/Signac.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘Signac/DESCRIPTION’ ... OK
...
* checking package dependencies ... ERROR
Package required but not available: ‘Seqinfo’

Packages suggested but not available for checking:
  'TFBSTools', 'motifmatchr'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# SimDesign

<details>

* Version: 2.25
* GitHub: https://github.com/philchalmers/SimDesign
* Source code: https://github.com/cran/SimDesign
* Date/Publication: 2026-03-31 05:10:37 UTC
* Number of recursive dependencies: 147

Run `revdepcheck::cloud_details(, "SimDesign")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/SimDesign/new/SimDesign.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘SimDesign/DESCRIPTION’ ... OK
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
* using log directory ‘/tmp/workdir/SimDesign/old/SimDesign.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘SimDesign/DESCRIPTION’ ... OK
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
# Spower

<details>

* Version: 0.6
* GitHub: https://github.com/philchalmers/Spower
* Source code: https://github.com/cran/Spower
* Date/Publication: 2026-02-13 17:40:10 UTC
* Number of recursive dependencies: 166

Run `revdepcheck::cloud_details(, "Spower")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/Spower/new/Spower.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘Spower/DESCRIPTION’ ... OK
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
Status: 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/Spower/old/Spower.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘Spower/DESCRIPTION’ ... OK
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
Status: 1 NOTE





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
* running under: Ubuntu 24.04.3 LTS
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
* running under: Ubuntu 24.04.3 LTS
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
# StatTeacherAssistant

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/StatTeacherAssistant
* Number of recursive dependencies: 123

Run `revdepcheck::cloud_details(, "StatTeacherAssistant")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# STCCGEV

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/STCCGEV
* Date/Publication: 2025-03-27 17:30:04 UTC
* Number of recursive dependencies: 72

Run `revdepcheck::cloud_details(, "STCCGEV")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/STCCGEV/new/STCCGEV.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘STCCGEV/DESCRIPTION’ ... OK
...
* this is package ‘STCCGEV’ version ‘1.0.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'bsts', 'copula'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/STCCGEV/old/STCCGEV.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘STCCGEV/DESCRIPTION’ ... OK
...
* this is package ‘STCCGEV’ version ‘1.0.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'bsts', 'copula'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# STCYP

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/STCYP
* Date/Publication: 2025-09-09 14:40:23 UTC
* Number of recursive dependencies: 71

Run `revdepcheck::cloud_details(, "STCYP")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/STCYP/new/STCYP.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘STCYP/DESCRIPTION’ ... OK
...
* this is package ‘STCYP’ version ‘1.0.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'bsts', 'copula'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/STCYP/old/STCYP.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘STCYP/DESCRIPTION’ ... OK
...
* this is package ‘STCYP’ version ‘1.0.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'bsts', 'copula'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# SurprisalAnalysis

<details>

* Version: 3.0.0
* GitHub: NA
* Source code: https://github.com/cran/SurprisalAnalysis
* Date/Publication: 2026-01-08 03:30:02 UTC
* Number of recursive dependencies: 217

Run `revdepcheck::cloud_details(, "SurprisalAnalysis")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/SurprisalAnalysis/new/SurprisalAnalysis.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘SurprisalAnalysis/DESCRIPTION’ ... OK
...
* this is package ‘SurprisalAnalysis’ version ‘3.0.0’
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
* using log directory ‘/tmp/workdir/SurprisalAnalysis/old/SurprisalAnalysis.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘SurprisalAnalysis/DESCRIPTION’ ... OK
...
* this is package ‘SurprisalAnalysis’ version ‘3.0.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘clusterProfiler’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# Surrogate

<details>

* Version: 3.4.1
* GitHub: https://github.com/florianstijven/Surrogate-development
* Source code: https://github.com/cran/Surrogate
* Date/Publication: 2025-04-29 04:40:02 UTC
* Number of recursive dependencies: 193

Run `revdepcheck::cloud_details(, "Surrogate")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/Surrogate/new/Surrogate.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘Surrogate/DESCRIPTION’ ... OK
...
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘MBESS’

Package suggested but not available for checking: ‘copula’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/Surrogate/old/Surrogate.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘Surrogate/DESCRIPTION’ ... OK
...
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘MBESS’

Package suggested but not available for checking: ‘copula’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# TCIU

<details>

* Version: 1.2.8
* GitHub: https://github.com/SOCR/TCIU
* Source code: https://github.com/cran/TCIU
* Date/Publication: 2026-01-26 03:30:02 UTC
* Number of recursive dependencies: 161

Run `revdepcheck::cloud_details(, "TCIU")` for more info

</details>

## In both

*   checking whether package ‘TCIU’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/TCIU/new/TCIU.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘TCIU’ ...
** package ‘TCIU’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’
using Fortran compiler: ‘GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’
gcc -I"/opt/R/4.4.0/lib/R/include" -DNDEBUG   -I/usr/local/include    -fpic  -g -O2  -c GLS.c -o GLS.o
gcc -I"/opt/R/4.4.0/lib/R/include" -DNDEBUG   -I/usr/local/include    -fpic  -g -O2  -c RC_interface.c -o RC_interface.o
gcc -I"/opt/R/4.4.0/lib/R/include" -DNDEBUG   -I/usr/local/include    -fpic  -g -O2  -c complex_Sig_gen.c -o complex_Sig_gen.o
gcc -I"/opt/R/4.4.0/lib/R/include" -DNDEBUG   -I/usr/local/include    -fpic  -g -O2  -c complex_Sig_sig2I.c -o complex_Sig_sig2I.o
...
*** moving datasets to lazyload DB
** demo
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘gsl’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘TCIU’
* removing ‘/tmp/workdir/TCIU/new/TCIU.Rcheck/TCIU’


```
### CRAN

```
* installing *source* package ‘TCIU’ ...
** package ‘TCIU’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’
using Fortran compiler: ‘GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’
gcc -I"/opt/R/4.4.0/lib/R/include" -DNDEBUG   -I/usr/local/include    -fpic  -g -O2  -c GLS.c -o GLS.o
gcc -I"/opt/R/4.4.0/lib/R/include" -DNDEBUG   -I/usr/local/include    -fpic  -g -O2  -c RC_interface.c -o RC_interface.o
gcc -I"/opt/R/4.4.0/lib/R/include" -DNDEBUG   -I/usr/local/include    -fpic  -g -O2  -c complex_Sig_gen.c -o complex_Sig_gen.o
gcc -I"/opt/R/4.4.0/lib/R/include" -DNDEBUG   -I/usr/local/include    -fpic  -g -O2  -c complex_Sig_sig2I.c -o complex_Sig_sig2I.o
...
*** moving datasets to lazyload DB
** demo
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘gsl’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘TCIU’
* removing ‘/tmp/workdir/TCIU/old/TCIU.Rcheck/TCIU’


```
# TDAkit

<details>

* Version: 0.1.3
* GitHub: NA
* Source code: https://github.com/cran/TDAkit
* Date/Publication: 2025-09-21 22:10:13 UTC
* Number of recursive dependencies: 82

Run `revdepcheck::cloud_details(, "TDAkit")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/TDAkit/new/TDAkit.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘TDAkit/DESCRIPTION’ ... OK
...
* this is package ‘TDAkit’ version ‘0.1.3’
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
* using log directory ‘/tmp/workdir/TDAkit/old/TDAkit.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘TDAkit/DESCRIPTION’ ... OK
...
* this is package ‘TDAkit’ version ‘0.1.3’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘energy’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# TELP

<details>

* Version: 1.0.3
* GitHub: NA
* Source code: https://github.com/cran/TELP
* Date/Publication: 2025-05-09 15:00:05 UTC
* Number of recursive dependencies: 109

Run `revdepcheck::cloud_details(, "TELP")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/TELP/new/TELP.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘TELP/DESCRIPTION’ ... OK
...
* checking extension type ... Package
* this is package ‘TELP’ version ‘1.0.3’
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'arules', 'arulesViz'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/TELP/old/TELP.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘TELP/DESCRIPTION’ ... OK
...
* checking extension type ... Package
* this is package ‘TELP’ version ‘1.0.3’
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'arules', 'arulesViz'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# TestAnaAPP

<details>

* Version: 1.1.2
* GitHub: https://github.com/jiangyouxiang/TestAnaAPP
* Source code: https://github.com/cran/TestAnaAPP
* Date/Publication: 2024-11-09 04:00:02 UTC
* Number of recursive dependencies: 274

Run `revdepcheck::cloud_details(, "TestAnaAPP")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/TestAnaAPP/new/TestAnaAPP.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘TestAnaAPP/DESCRIPTION’ ... OK
...
* this is package ‘TestAnaAPP’ version ‘1.1.2’
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
* using log directory ‘/tmp/workdir/TestAnaAPP/old/TestAnaAPP.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘TestAnaAPP/DESCRIPTION’ ... OK
...
* this is package ‘TestAnaAPP’ version ‘1.1.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘semPlot’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





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
* running under: Ubuntu 24.04.3 LTS
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
* running under: Ubuntu 24.04.3 LTS
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
# TriDimRegression

<details>

* Version: 1.0.3
* GitHub: https://github.com/alexander-pastukhov/tridim-regression
* Source code: https://github.com/cran/TriDimRegression
* Date/Publication: 2025-10-09 16:50:02 UTC
* Number of recursive dependencies: 91

Run `revdepcheck::cloud_details(, "TriDimRegression")` for more info

</details>

## In both

*   checking whether package ‘TriDimRegression’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/TriDimRegression/new/TriDimRegression.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘TriDimRegression’ ...
** package ‘TriDimRegression’ successfully unpacked and MD5 sums checked
** using staged installation
Error in loadNamespace(x) : there is no package called ‘rstantools’
Calls: loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: configuration failed for package ‘TriDimRegression’
* removing ‘/tmp/workdir/TriDimRegression/new/TriDimRegression.Rcheck/TriDimRegression’


```
### CRAN

```
* installing *source* package ‘TriDimRegression’ ...
** package ‘TriDimRegression’ successfully unpacked and MD5 sums checked
** using staged installation
Error in loadNamespace(x) : there is no package called ‘rstantools’
Calls: loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: configuration failed for package ‘TriDimRegression’
* removing ‘/tmp/workdir/TriDimRegression/old/TriDimRegression.Rcheck/TriDimRegression’


```
# VecDep

<details>

* Version: 0.1.3
* GitHub: https://github.com/StevenDeKeyser98/VecDep
* Source code: https://github.com/cran/VecDep
* Date/Publication: 2024-11-14 13:50:19 UTC
* Number of recursive dependencies: 94

Run `revdepcheck::cloud_details(, "VecDep")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/VecDep/new/VecDep.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘VecDep/DESCRIPTION’ ... OK
...
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘HAC’

Package suggested but not available for checking: ‘copula’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/VecDep/old/VecDep.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘VecDep/DESCRIPTION’ ... OK
...
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘HAC’

Package suggested but not available for checking: ‘copula’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# WINS

<details>

* Version: 1.5.1
* GitHub: NA
* Source code: https://github.com/cran/WINS
* Date/Publication: 2025-07-02 06:30:02 UTC
* Number of recursive dependencies: 109

Run `revdepcheck::cloud_details(, "WINS")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/WINS/new/WINS.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘WINS/DESCRIPTION’ ... OK
...
* this is package ‘WINS’ version ‘1.5.1’
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
* using log directory ‘/tmp/workdir/WINS/old/WINS.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘WINS/DESCRIPTION’ ... OK
...
* this is package ‘WINS’ version ‘1.5.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘copula’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# WQM

<details>

* Version: 0.1.4
* GitHub: NA
* Source code: https://github.com/cran/WQM
* Date/Publication: 2024-10-11 08:00:18 UTC
* Number of recursive dependencies: 76

Run `revdepcheck::cloud_details(, "WQM")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/WQM/new/WQM.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘WQM/DESCRIPTION’ ... OK
...
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘WQM’ can be installed ... ERROR
Installation failed.
See ‘/tmp/workdir/WQM/new/WQM.Rcheck/00install.out’ for details.
* DONE
Status: 1 ERROR, 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/WQM/old/WQM.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘WQM/DESCRIPTION’ ... OK
...
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘WQM’ can be installed ... ERROR
Installation failed.
See ‘/tmp/workdir/WQM/old/WQM.Rcheck/00install.out’ for details.
* DONE
Status: 1 ERROR, 1 NOTE





```
# XYomics

<details>

* Version: 0.1.3
* GitHub: NA
* Source code: https://github.com/cran/XYomics
* Date/Publication: 2026-01-08 19:30:09 UTC
* Number of recursive dependencies: 213

Run `revdepcheck::cloud_details(, "XYomics")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/XYomics/new/XYomics.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘XYomics/DESCRIPTION’ ... OK
...
* this is package ‘XYomics’ version ‘0.1.3’
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
* using log directory ‘/tmp/workdir/XYomics/old/XYomics.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
* running under: Ubuntu 24.04.3 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘XYomics/DESCRIPTION’ ... OK
...
* this is package ‘XYomics’ version ‘0.1.3’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘clusterProfiler’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
