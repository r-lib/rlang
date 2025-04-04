# arealDB

<details>

* Version: 0.9.4
* GitHub: https://github.com/luckinet/arealDB
* Source code: https://github.com/cran/arealDB
* Date/Publication: 2025-01-20 13:40:05 UTC
* Number of recursive dependencies: 110

Run `revdepcheck::cloud_details(, "arealDB")` for more info

</details>

## In both

*   checking whether package ‘arealDB’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/arealDB/new/arealDB.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘arealDB’ ...
** package ‘arealDB’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/usr/local/lib/R/site-library/redland/libs/redland.so':
  librdf.so.0: cannot open shared object file: No such file or directory
Calls: <Anonymous> ... asNamespace -> loadNamespace -> library.dynam -> dyn.load
Execution halted
ERROR: lazy loading failed for package ‘arealDB’
* removing ‘/tmp/workdir/arealDB/new/arealDB.Rcheck/arealDB’


```
### CRAN

```
* installing *source* package ‘arealDB’ ...
** package ‘arealDB’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/usr/local/lib/R/site-library/redland/libs/redland.so':
  librdf.so.0: cannot open shared object file: No such file or directory
Calls: <Anonymous> ... asNamespace -> loadNamespace -> library.dynam -> dyn.load
Execution halted
ERROR: lazy loading failed for package ‘arealDB’
* removing ‘/tmp/workdir/arealDB/old/arealDB.Rcheck/arealDB’


```
# bayesdfa

<details>

* Version: 1.3.4
* GitHub: https://github.com/fate-ewi/bayesdfa
* Source code: https://github.com/cran/bayesdfa
* Date/Publication: 2025-03-22 20:30:21 UTC
* Number of recursive dependencies: 87

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
# ctsem

<details>

* Version: 3.10.2
* GitHub: https://github.com/cdriveraus/ctsem
* Source code: https://github.com/cran/ctsem
* Date/Publication: 2025-01-13 11:00:08 UTC
* Number of recursive dependencies: 160

Run `revdepcheck::cloud_details(, "ctsem")` for more info

</details>

## In both

*   checking whether package ‘ctsem’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/ctsem/new/ctsem.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘ctsem’ ...
** package ‘ctsem’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
using C++17


g++ -std=gnu++17 -I"/opt/R/4.4.0/lib/R/include" -DNDEBUG -I"../inst/include" -I"/usr/local/lib/R/site-library/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -DUSE_STANC3 -D_HAS_AUTO_PTR_ETC=0 -I'/usr/local/lib/R/site-library/BH/include' -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppEigen/include' -I'/usr/local/lib/R/site-library/RcppParallel/include' -I'/usr/local/lib/R/site-library/rstan/include' -I'/usr/local/lib/R/site-library/StanHeaders/include' -I/usr/local/include    -I'/usr/local/lib/R/site-library/RcppParallel/include' -D_REENTRANT -DSTAN_THREADS   -fpic  -g -O2   -c RcppExports.cpp -o RcppExports.o
In file included from /usr/local/lib/R/site-library/RcppEigen/include/Eigen/Core:205,
...
/usr/local/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:0:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_ctsm_namespace::model_ctsm; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/usr/local/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:21:0:   required from here
/usr/local/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:654:74: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__m128d’} [-Wignored-attributes]
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
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
using C++17


g++ -std=gnu++17 -I"/opt/R/4.4.0/lib/R/include" -DNDEBUG -I"../inst/include" -I"/usr/local/lib/R/site-library/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -DUSE_STANC3 -D_HAS_AUTO_PTR_ETC=0 -I'/usr/local/lib/R/site-library/BH/include' -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppEigen/include' -I'/usr/local/lib/R/site-library/RcppParallel/include' -I'/usr/local/lib/R/site-library/rstan/include' -I'/usr/local/lib/R/site-library/StanHeaders/include' -I/usr/local/include    -I'/usr/local/lib/R/site-library/RcppParallel/include' -D_REENTRANT -DSTAN_THREADS   -fpic  -g -O2   -c RcppExports.cpp -o RcppExports.o
In file included from /usr/local/lib/R/site-library/RcppEigen/include/Eigen/Core:205,
...
/usr/local/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:0:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_ctsm_namespace::model_ctsm; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/usr/local/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:21:0:   required from here
/usr/local/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:654:74: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__m128d’} [-Wignored-attributes]
  654 |   return internal::first_aligned<int(unpacket_traits<DefaultPacketType>::alignment),Derived>(m);
      |                                                                          ^~~~~~~~~
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.4.0/lib/R/etc/Makeconf:202: stanExports_ctsm.o] Error 1
ERROR: compilation failed for package ‘ctsem’
* removing ‘/tmp/workdir/ctsem/old/ctsem.Rcheck/ctsem’


```
# EcoEnsemble

<details>

* Version: 1.1.2
* GitHub: https://github.com/CefasRepRes/EcoEnsemble
* Source code: https://github.com/cran/EcoEnsemble
* Date/Publication: 2025-03-18 18:20:02 UTC
* Number of recursive dependencies: 90

Run `revdepcheck::cloud_details(, "EcoEnsemble")` for more info

</details>

## In both

*   checking whether package ‘EcoEnsemble’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/EcoEnsemble/new/EcoEnsemble.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘EcoEnsemble’ ...
** package ‘EcoEnsemble’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
using C++17


g++ -std=gnu++17 -I"/opt/R/4.4.0/lib/R/include" -DNDEBUG -I"../inst/include" -I"/usr/local/lib/R/site-library/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -DUSE_STANC3 -D_HAS_AUTO_PTR_ETC=0 -I'/usr/local/lib/R/site-library/BH/include' -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppEigen/include' -I'/usr/local/lib/R/site-library/RcppParallel/include' -I'/usr/local/lib/R/site-library/rstan/include' -I'/usr/local/lib/R/site-library/StanHeaders/include' -I/usr/local/include    -I'/usr/local/lib/R/site-library/RcppParallel/include' -D_REENTRANT -DSTAN_THREADS   -fpic  -g -O2   -c KF_back.cpp -o KF_back.o
In file included from /usr/local/lib/R/site-library/RcppEigen/include/Eigen/Core:205,
...
/usr/local/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:0:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_ensemble_model_hierarchical_namespace::model_ensemble_model_hierarchical; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/usr/local/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:21:0:   required from here
/usr/local/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:654:74: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__m128d’} [-Wignored-attributes]
  654 |   return internal::first_aligned<int(unpacket_traits<DefaultPacketType>::alignment),Derived>(m);
      |                                                                          ^~~~~~~~~
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.4.0/lib/R/etc/Makeconf:202: stanExports_ensemble_model_hierarchical.o] Error 1
ERROR: compilation failed for package ‘EcoEnsemble’
* removing ‘/tmp/workdir/EcoEnsemble/new/EcoEnsemble.Rcheck/EcoEnsemble’


```
### CRAN

```
* installing *source* package ‘EcoEnsemble’ ...
** package ‘EcoEnsemble’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
using C++17


g++ -std=gnu++17 -I"/opt/R/4.4.0/lib/R/include" -DNDEBUG -I"../inst/include" -I"/usr/local/lib/R/site-library/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -DUSE_STANC3 -D_HAS_AUTO_PTR_ETC=0 -I'/usr/local/lib/R/site-library/BH/include' -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppEigen/include' -I'/usr/local/lib/R/site-library/RcppParallel/include' -I'/usr/local/lib/R/site-library/rstan/include' -I'/usr/local/lib/R/site-library/StanHeaders/include' -I/usr/local/include    -I'/usr/local/lib/R/site-library/RcppParallel/include' -D_REENTRANT -DSTAN_THREADS   -fpic  -g -O2   -c KF_back.cpp -o KF_back.o
In file included from /usr/local/lib/R/site-library/RcppEigen/include/Eigen/Core:205,
...
/usr/local/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:0:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_ensemble_model_hierarchical_namespace::model_ensemble_model_hierarchical; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/usr/local/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:21:0:   required from here
/usr/local/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:654:74: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__m128d’} [-Wignored-attributes]
  654 |   return internal::first_aligned<int(unpacket_traits<DefaultPacketType>::alignment),Derived>(m);
      |                                                                          ^~~~~~~~~
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.4.0/lib/R/etc/Makeconf:202: stanExports_ensemble_model_hierarchical.o] Error 1
ERROR: compilation failed for package ‘EcoEnsemble’
* removing ‘/tmp/workdir/EcoEnsemble/old/EcoEnsemble.Rcheck/EcoEnsemble’


```
# FAIRmaterials

<details>

* Version: 0.4.2.1
* GitHub: NA
* Source code: https://github.com/cran/FAIRmaterials
* Date/Publication: 2024-06-27 15:40:02 UTC
* Number of recursive dependencies: 93

Run `revdepcheck::cloud_details(, "FAIRmaterials")` for more info

</details>

## In both

*   checking whether package ‘FAIRmaterials’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/FAIRmaterials/new/FAIRmaterials.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘FAIRmaterials’ ...
** package ‘FAIRmaterials’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/usr/local/lib/R/site-library/redland/libs/redland.so':
  librdf.so.0: cannot open shared object file: No such file or directory
Calls: <Anonymous> ... asNamespace -> loadNamespace -> library.dynam -> dyn.load
Execution halted
ERROR: lazy loading failed for package ‘FAIRmaterials’
* removing ‘/tmp/workdir/FAIRmaterials/new/FAIRmaterials.Rcheck/FAIRmaterials’


```
### CRAN

```
* installing *source* package ‘FAIRmaterials’ ...
** package ‘FAIRmaterials’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/usr/local/lib/R/site-library/redland/libs/redland.so':
  librdf.so.0: cannot open shared object file: No such file or directory
Calls: <Anonymous> ... asNamespace -> loadNamespace -> library.dynam -> dyn.load
Execution halted
ERROR: lazy loading failed for package ‘FAIRmaterials’
* removing ‘/tmp/workdir/FAIRmaterials/old/FAIRmaterials.Rcheck/FAIRmaterials’


```
# fio

<details>

* Version: 0.1.5
* GitHub: https://github.com/albersonmiranda/fio
* Source code: https://github.com/cran/fio
* Date/Publication: 2025-04-02 21:50:09 UTC
* Number of recursive dependencies: 90

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
Using cargo 1.75.0
Using rustc 1.75.0 (82e1608df 2023-12-21) (built from a source tarball)
Building for CRAN.
Writing `src/Makevars`.
`tools/config.R` has finished.
** libs
using C compiler: ‘gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
...
   Compiling thiserror-impl v2.0.12
   Compiling crossbeam-deque v0.8.6
error: unsupported output in build script of `pulp v0.21.4`: `cargo::rustc-check-cfg=cfg(libpulp_const)`
Found a `cargo::key=value` build directive which is reserved for future use.
Either change the directive to `cargo:key=value` syntax (note the single `:`) or upgrade your version of Rust.
See https://doc.rust-lang.org/cargo/reference/build-scripts.html#outputs-of-the-build-script for more information about build script outputs.
warning: build failed, waiting for other jobs to finish...
make: *** [Makevars:27: rust/target/release/libfio.a] Error 101
ERROR: compilation failed for package ‘fio’
* removing ‘/tmp/workdir/fio/new/fio.Rcheck/fio’


```
### CRAN

```
* installing *source* package ‘fio’ ...
** package ‘fio’ successfully unpacked and MD5 sums checked
** using staged installation
Using cargo 1.75.0
Using rustc 1.75.0 (82e1608df 2023-12-21) (built from a source tarball)
Building for CRAN.
Writing `src/Makevars`.
`tools/config.R` has finished.
** libs
using C compiler: ‘gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
...
   Compiling pulp v0.21.4
   Compiling crossbeam-deque v0.8.6
error: unsupported output in build script of `pulp v0.21.4`: `cargo::rustc-check-cfg=cfg(libpulp_const)`
Found a `cargo::key=value` build directive which is reserved for future use.
Either change the directive to `cargo:key=value` syntax (note the single `:`) or upgrade your version of Rust.
See https://doc.rust-lang.org/cargo/reference/build-scripts.html#outputs-of-the-build-script for more information about build script outputs.
warning: build failed, waiting for other jobs to finish...
make: *** [Makevars:27: rust/target/release/libfio.a] Error 101
ERROR: compilation failed for package ‘fio’
* removing ‘/tmp/workdir/fio/old/fio.Rcheck/fio’


```
# loon.shiny

<details>

* Version: 1.0.3
* GitHub: NA
* Source code: https://github.com/cran/loon.shiny
* Date/Publication: 2022-10-08 15:30:02 UTC
* Number of recursive dependencies: 136

Run `revdepcheck::cloud_details(, "loon.shiny")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/loon.shiny/new/loon.shiny.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘loon.shiny/DESCRIPTION’ ... OK
...
* this is package ‘loon.shiny’ version ‘1.0.3’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'loon', 'loon.ggplot'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/loon.shiny/old/loon.shiny.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘loon.shiny/DESCRIPTION’ ... OK
...
* this is package ‘loon.shiny’ version ‘1.0.3’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'loon', 'loon.ggplot'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# metabolic

<details>

* Version: 0.1.2
* GitHub: https://github.com/fmmattioni/metabolic
* Source code: https://github.com/cran/metabolic
* Date/Publication: 2023-10-10 07:40:02 UTC
* Number of recursive dependencies: 137

Run `revdepcheck::cloud_details(, "metabolic")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/metabolic/new/metabolic.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘metabolic/DESCRIPTION’ ... OK
...
* checking data for non-ASCII characters ... NOTE
  Note: found 37 marked UTF-8 strings
* checking LazyData ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘spelling.R’
* DONE
Status: 2 NOTEs





```
### CRAN

```
* using log directory ‘/tmp/workdir/metabolic/old/metabolic.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘metabolic/DESCRIPTION’ ... OK
...
* checking data for non-ASCII characters ... NOTE
  Note: found 37 marked UTF-8 strings
* checking LazyData ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘spelling.R’
* DONE
Status: 2 NOTEs





```
# metajam

<details>

* Version: 0.3.1
* GitHub: https://github.com/NCEAS/metajam
* Source code: https://github.com/cran/metajam
* Date/Publication: 2024-08-16 17:50:02 UTC
* Number of recursive dependencies: 90

Run `revdepcheck::cloud_details(, "metajam")` for more info

</details>

## In both

*   checking whether package ‘metajam’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/metajam/new/metajam.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘metajam’ ...
** package ‘metajam’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/usr/local/lib/R/site-library/redland/libs/redland.so':
  librdf.so.0: cannot open shared object file: No such file or directory
Calls: <Anonymous> ... namespaceImport -> loadNamespace -> library.dynam -> dyn.load
Execution halted
ERROR: lazy loading failed for package ‘metajam’
* removing ‘/tmp/workdir/metajam/new/metajam.Rcheck/metajam’


```
### CRAN

```
* installing *source* package ‘metajam’ ...
** package ‘metajam’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/usr/local/lib/R/site-library/redland/libs/redland.so':
  librdf.so.0: cannot open shared object file: No such file or directory
Calls: <Anonymous> ... namespaceImport -> loadNamespace -> library.dynam -> dyn.load
Execution halted
ERROR: lazy loading failed for package ‘metajam’
* removing ‘/tmp/workdir/metajam/old/metajam.Rcheck/metajam’


```
# multinma

<details>

* Version: 0.8.0
* GitHub: https://github.com/dmphillippo/multinma
* Source code: https://github.com/cran/multinma
* Date/Publication: 2025-03-25 22:50:16 UTC
* Number of recursive dependencies: 150

Run `revdepcheck::cloud_details(, "multinma")` for more info

</details>

## In both

*   checking whether package ‘multinma’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/multinma/new/multinma.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘multinma’ ...
** package ‘multinma’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
using C++17


g++ -std=gnu++17 -I"/opt/R/4.4.0/lib/R/include" -DNDEBUG -I"../inst/include" -I"/usr/local/lib/R/site-library/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -DUSE_STANC3 -D_HAS_AUTO_PTR_ETC=0 -I'/usr/local/lib/R/site-library/BH/include' -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppEigen/include' -I'/usr/local/lib/R/site-library/RcppParallel/include' -I'/usr/local/lib/R/site-library/rstan/include' -I'/usr/local/lib/R/site-library/StanHeaders/include' -I/usr/local/include    -I'/usr/local/lib/R/site-library/RcppParallel/include' -D_REENTRANT -DSTAN_THREADS   -fpic  -g -O2   -c RcppExports.cpp -o RcppExports.o
In file included from /usr/local/lib/R/site-library/RcppEigen/include/Eigen/Core:205,
...
/usr/local/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:0:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_survival_param_namespace::model_survival_param; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/usr/local/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:21:0:   required from here
/usr/local/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:654:74: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__m128d’} [-Wignored-attributes]
  654 |   return internal::first_aligned<int(unpacket_traits<DefaultPacketType>::alignment),Derived>(m);
      |                                                                          ^~~~~~~~~
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.4.0/lib/R/etc/Makeconf:202: stanExports_survival_param.o] Error 1
ERROR: compilation failed for package ‘multinma’
* removing ‘/tmp/workdir/multinma/new/multinma.Rcheck/multinma’


```
### CRAN

```
* installing *source* package ‘multinma’ ...
** package ‘multinma’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
using C++17


g++ -std=gnu++17 -I"/opt/R/4.4.0/lib/R/include" -DNDEBUG -I"../inst/include" -I"/usr/local/lib/R/site-library/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -DUSE_STANC3 -D_HAS_AUTO_PTR_ETC=0 -I'/usr/local/lib/R/site-library/BH/include' -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppEigen/include' -I'/usr/local/lib/R/site-library/RcppParallel/include' -I'/usr/local/lib/R/site-library/rstan/include' -I'/usr/local/lib/R/site-library/StanHeaders/include' -I/usr/local/include    -I'/usr/local/lib/R/site-library/RcppParallel/include' -D_REENTRANT -DSTAN_THREADS   -fpic  -g -O2   -c RcppExports.cpp -o RcppExports.o
In file included from /usr/local/lib/R/site-library/RcppEigen/include/Eigen/Core:205,
...
/usr/local/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:0:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_survival_param_namespace::model_survival_param; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/usr/local/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:21:0:   required from here
/usr/local/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:654:74: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__m128d’} [-Wignored-attributes]
  654 |   return internal::first_aligned<int(unpacket_traits<DefaultPacketType>::alignment),Derived>(m);
      |                                                                          ^~~~~~~~~
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.4.0/lib/R/etc/Makeconf:202: stanExports_survival_param.o] Error 1
ERROR: compilation failed for package ‘multinma’
* removing ‘/tmp/workdir/multinma/old/multinma.Rcheck/multinma’


```
# nesRdata

<details>

* Version: 0.3.1
* GitHub: https://github.com/jsta/nesRdata
* Source code: https://github.com/cran/nesRdata
* Date/Publication: 2020-04-30 17:20:02 UTC
* Number of recursive dependencies: 66

Run `revdepcheck::cloud_details(, "nesRdata")` for more info

</details>

## In both

*   checking whether package ‘nesRdata’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/nesRdata/new/nesRdata.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘nesRdata’ ...
** package ‘nesRdata’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/usr/local/lib/R/site-library/redland/libs/redland.so':
  librdf.so.0: cannot open shared object file: No such file or directory
Calls: <Anonymous> ... namespaceImport -> loadNamespace -> library.dynam -> dyn.load
Execution halted
ERROR: lazy loading failed for package ‘nesRdata’
* removing ‘/tmp/workdir/nesRdata/new/nesRdata.Rcheck/nesRdata’


```
### CRAN

```
* installing *source* package ‘nesRdata’ ...
** package ‘nesRdata’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/usr/local/lib/R/site-library/redland/libs/redland.so':
  librdf.so.0: cannot open shared object file: No such file or directory
Calls: <Anonymous> ... namespaceImport -> loadNamespace -> library.dynam -> dyn.load
Execution halted
ERROR: lazy loading failed for package ‘nesRdata’
* removing ‘/tmp/workdir/nesRdata/old/nesRdata.Rcheck/nesRdata’


```
# ontologics

<details>

* Version: 0.7.4
* GitHub: https://github.com/luckinet/ontologics
* Source code: https://github.com/cran/ontologics
* Date/Publication: 2025-01-17 16:50:02 UTC
* Number of recursive dependencies: 80

Run `revdepcheck::cloud_details(, "ontologics")` for more info

</details>

## In both

*   checking whether package ‘ontologics’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/ontologics/new/ontologics.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘ontologics’ ...
** package ‘ontologics’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/usr/local/lib/R/site-library/redland/libs/redland.so':
  librdf.so.0: cannot open shared object file: No such file or directory
Calls: <Anonymous> ... asNamespace -> loadNamespace -> library.dynam -> dyn.load
Execution halted
ERROR: lazy loading failed for package ‘ontologics’
* removing ‘/tmp/workdir/ontologics/new/ontologics.Rcheck/ontologics’


```
### CRAN

```
* installing *source* package ‘ontologics’ ...
** package ‘ontologics’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/usr/local/lib/R/site-library/redland/libs/redland.so':
  librdf.so.0: cannot open shared object file: No such file or directory
Calls: <Anonymous> ... asNamespace -> loadNamespace -> library.dynam -> dyn.load
Execution halted
ERROR: lazy loading failed for package ‘ontologics’
* removing ‘/tmp/workdir/ontologics/old/ontologics.Rcheck/ontologics’


```
# rdflib

<details>

* Version: 0.2.9
* GitHub: https://github.com/ropensci/rdflib
* Source code: https://github.com/cran/rdflib
* Date/Publication: 2024-08-17 06:00:05 UTC
* Number of recursive dependencies: 93

Run `revdepcheck::cloud_details(, "rdflib")` for more info

</details>

## In both

*   checking whether package ‘rdflib’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/rdflib/new/rdflib.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘rdflib’ ...
** package ‘rdflib’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/usr/local/lib/R/site-library/redland/libs/redland.so':
  librdf.so.0: cannot open shared object file: No such file or directory
Calls: <Anonymous> ... asNamespace -> loadNamespace -> library.dynam -> dyn.load
Execution halted
ERROR: lazy loading failed for package ‘rdflib’
* removing ‘/tmp/workdir/rdflib/new/rdflib.Rcheck/rdflib’


```
### CRAN

```
* installing *source* package ‘rdflib’ ...
** package ‘rdflib’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/usr/local/lib/R/site-library/redland/libs/redland.so':
  librdf.so.0: cannot open shared object file: No such file or directory
Calls: <Anonymous> ... asNamespace -> loadNamespace -> library.dynam -> dyn.load
Execution halted
ERROR: lazy loading failed for package ‘rdflib’
* removing ‘/tmp/workdir/rdflib/old/rdflib.Rcheck/rdflib’


```
# rmsb

<details>

* Version: 1.1-1
* GitHub: NA
* Source code: https://github.com/cran/rmsb
* Date/Publication: 2024-07-08 11:10:03 UTC
* Number of recursive dependencies: 138

Run `revdepcheck::cloud_details(, "rmsb")` for more info

</details>

## In both

*   checking whether package ‘rmsb’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/rmsb/new/rmsb.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘rmsb’ ...
** package ‘rmsb’ successfully unpacked and MD5 sums checked
** using staged installation
Error in loadNamespace(x) : there is no package called ‘rstantools’
Calls: loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: configuration failed for package ‘rmsb’
* removing ‘/tmp/workdir/rmsb/new/rmsb.Rcheck/rmsb’


```
### CRAN

```
* installing *source* package ‘rmsb’ ...
** package ‘rmsb’ successfully unpacked and MD5 sums checked
** using staged installation
Error in loadNamespace(x) : there is no package called ‘rstantools’
Calls: loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: configuration failed for package ‘rmsb’
* removing ‘/tmp/workdir/rmsb/old/rmsb.Rcheck/rmsb’


```
# rstanarm

<details>

* Version: 2.32.1
* GitHub: https://github.com/stan-dev/rstanarm
* Source code: https://github.com/cran/rstanarm
* Date/Publication: 2024-01-18 23:00:03 UTC
* Number of recursive dependencies: 141

Run `revdepcheck::cloud_details(, "rstanarm")` for more info

</details>

## In both

*   checking whether package ‘rstanarm’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/rstanarm/new/rstanarm.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘rstanarm’ ...
** package ‘rstanarm’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
using C++17
"/opt/R/4.4.0/lib/R/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/bernoulli.stan
Wrote C++ file "stan_files/bernoulli.cc"


...
/usr/local/lib/R/site-library/StanHeaders/include/stan/math/rev/fun/quad_form.hpp:88:0:   required from here
/usr/local/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:654:74: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__m128d’} [-Wignored-attributes]
  654 |   return internal::first_aligned<int(unpacket_traits<DefaultPacketType>::alignment),Derived>(m);
      |                                                                          ^~~~~~~~~
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.4.0/lib/R/etc/Makeconf:202: stan_files/continuous.o] Error 1
rm stan_files/bernoulli.cc stan_files/binomial.cc stan_files/continuous.cc
ERROR: compilation failed for package ‘rstanarm’
* removing ‘/tmp/workdir/rstanarm/new/rstanarm.Rcheck/rstanarm’


```
### CRAN

```
* installing *source* package ‘rstanarm’ ...
** package ‘rstanarm’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
using C++17
"/opt/R/4.4.0/lib/R/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/bernoulli.stan
Wrote C++ file "stan_files/bernoulli.cc"


...
/usr/local/lib/R/site-library/StanHeaders/include/stan/math/rev/fun/quad_form.hpp:88:0:   required from here
/usr/local/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:654:74: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__m128d’} [-Wignored-attributes]
  654 |   return internal::first_aligned<int(unpacket_traits<DefaultPacketType>::alignment),Derived>(m);
      |                                                                          ^~~~~~~~~
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.4.0/lib/R/etc/Makeconf:202: stan_files/continuous.o] Error 1
rm stan_files/bernoulli.cc stan_files/binomial.cc stan_files/continuous.cc
ERROR: compilation failed for package ‘rstanarm’
* removing ‘/tmp/workdir/rstanarm/old/rstanarm.Rcheck/rstanarm’


```
# streamDAG

<details>

* Version: 1.5-9
* GitHub: NA
* Source code: https://github.com/cran/streamDAG
* Date/Publication: 2025-01-14 21:50:02 UTC
* Number of recursive dependencies: 139

Run `revdepcheck::cloud_details(, "streamDAG")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/streamDAG/new/streamDAG.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘streamDAG/DESCRIPTION’ ... OK
* this is package ‘streamDAG’ version ‘1.5-9’
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘asbio’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/streamDAG/old/streamDAG.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘streamDAG/DESCRIPTION’ ... OK
* this is package ‘streamDAG’ version ‘1.5-9’
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘asbio’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# TriDimRegression

<details>

* Version: 1.0.2
* GitHub: https://github.com/alexander-pastukhov/tridim-regression
* Source code: https://github.com/cran/TriDimRegression
* Date/Publication: 2023-09-13 14:10:03 UTC
* Number of recursive dependencies: 98

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
