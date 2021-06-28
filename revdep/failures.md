# apisensr

<details>

* Version: 0.3.0
* GitHub: https://github.com/dhaine/apisensr
* Source code: https://github.com/cran/apisensr
* Date/Publication: 2021-03-15 11:20:09 UTC
* Number of recursive dependencies: 128

Run `cloud_details(, "apisensr")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/apisensr/new/apisensr.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘apisensr/DESCRIPTION’ ... OK
* this is package ‘apisensr’ version ‘0.3.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘episensr’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/apisensr/old/apisensr.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘apisensr/DESCRIPTION’ ... OK
* this is package ‘apisensr’ version ‘0.3.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘episensr’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# bayesdfa

<details>

* Version: 1.1.0
* GitHub: https://github.com/fate-ewi/bayesdfa
* Source code: https://github.com/cran/bayesdfa
* Date/Publication: 2021-05-28 18:10:05 UTC
* Number of recursive dependencies: 81

Run `cloud_details(, "bayesdfa")` for more info

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
** libs


g++ -std=gnu++14 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG -I"../inst/include" -I"/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I'/opt/R/4.0.3/lib/R/site-library/BH/include' -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppEigen/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppParallel/include' -I'/opt/R/4.0.3/lib/R/site-library/rstan/include' -I'/opt/R/4.0.3/lib/R/site-library/StanHeaders/include' -I/usr/local/include   -I'/opt/R/4.0.3/lib/R/site-library/RcppParallel/include' -D_REENTRANT -DSTAN_THREADS   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
In file included from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/Core:397,
                 from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/Dense:1,
                 from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/RcppEigenForward.h:30,
...
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/ProductEvaluators.h:35:90:   required from ‘Eigen::internal::evaluator<Eigen::Product<Lhs, Rhs, Option> >::evaluator(const XprType&) [with Lhs = Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>; Rhs = Eigen::Matrix<double, -1, 1>; int Options = 0; Eigen::internal::evaluator<Eigen::Product<Lhs, Rhs, Option> >::XprType = Eigen::Product<Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>, Eigen::Matrix<double, -1, 1>, 0>]’
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/Product.h:132:22:   required from ‘Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::operator const Scalar() const [with Lhs = Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>; Rhs = Eigen::Matrix<double, -1, 1>; int Option = 0; Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::Scalar = double]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:23:56:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_dfa_namespace::model_dfa; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:10:   required from here
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:55:30: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__vector(2) double’} [-Wignored-attributes]
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.0.3/lib/R/etc/Makeconf:179: stanExports_dfa.o] Error 1
ERROR: compilation failed for package ‘bayesdfa’
* removing ‘/tmp/workdir/bayesdfa/new/bayesdfa.Rcheck/bayesdfa’


```
### CRAN

```
* installing *source* package ‘bayesdfa’ ...
** package ‘bayesdfa’ successfully unpacked and MD5 sums checked
** using staged installation
** libs


g++ -std=gnu++14 -I"/opt/R/4.0.3/lib/R/include" -DNDEBUG -I"../inst/include" -I"/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I'/opt/R/4.0.3/lib/R/site-library/BH/include' -I'/opt/R/4.0.3/lib/R/site-library/Rcpp/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppEigen/include' -I'/opt/R/4.0.3/lib/R/site-library/RcppParallel/include' -I'/opt/R/4.0.3/lib/R/site-library/rstan/include' -I'/opt/R/4.0.3/lib/R/site-library/StanHeaders/include' -I/usr/local/include   -I'/opt/R/4.0.3/lib/R/site-library/RcppParallel/include' -D_REENTRANT -DSTAN_THREADS   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
In file included from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/Core:397,
                 from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/Dense:1,
                 from /opt/R/4.0.3/lib/R/site-library/RcppEigen/include/RcppEigenForward.h:30,
...
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/ProductEvaluators.h:35:90:   required from ‘Eigen::internal::evaluator<Eigen::Product<Lhs, Rhs, Option> >::evaluator(const XprType&) [with Lhs = Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>; Rhs = Eigen::Matrix<double, -1, 1>; int Options = 0; Eigen::internal::evaluator<Eigen::Product<Lhs, Rhs, Option> >::XprType = Eigen::Product<Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>, Eigen::Matrix<double, -1, 1>, 0>]’
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/Product.h:132:22:   required from ‘Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::operator const Scalar() const [with Lhs = Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>; Rhs = Eigen::Matrix<double, -1, 1>; int Option = 0; Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::Scalar = double]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:23:56:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_dfa_namespace::model_dfa; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:10:   required from here
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:55:30: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__vector(2) double’} [-Wignored-attributes]
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.0.3/lib/R/etc/Makeconf:179: stanExports_dfa.o] Error 1
ERROR: compilation failed for package ‘bayesdfa’
* removing ‘/tmp/workdir/bayesdfa/old/bayesdfa.Rcheck/bayesdfa’


```
# NA

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/NA
* Number of recursive dependencies: 0

Run `cloud_details(, "NA")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# NA

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/NA
* Number of recursive dependencies: 0

Run `cloud_details(, "NA")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# NA

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/NA
* Number of recursive dependencies: 0

Run `cloud_details(, "NA")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# metagam

<details>

* Version: 0.2.0
* GitHub: https://github.com/Lifebrain/metagam
* Source code: https://github.com/cran/metagam
* Date/Publication: 2020-11-12 08:10:02 UTC
* Number of recursive dependencies: 145

Run `cloud_details(, "metagam")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/metagam/new/metagam.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘metagam/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘metagam’ version ‘0.2.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘metap’

Package suggested but not available for checking: ‘multtest’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/metagam/old/metagam.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘metagam/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘metagam’ version ‘0.2.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘metap’

Package suggested but not available for checking: ‘multtest’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# rabhit

<details>

* Version: 0.1.5
* GitHub: NA
* Source code: https://github.com/cran/rabhit
* Date/Publication: 2020-07-11 22:40:02 UTC
* Number of recursive dependencies: 123

Run `cloud_details(, "rabhit")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/rabhit/new/rabhit.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘rabhit/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘rabhit’ version ‘0.1.5’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'alakazam', 'tigger'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/rabhit/old/rabhit.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘rabhit/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘rabhit’ version ‘0.1.5’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'alakazam', 'tigger'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# NA

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/NA
* Number of recursive dependencies: 0

Run `cloud_details(, "NA")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# NA

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/NA
* Number of recursive dependencies: 0

Run `cloud_details(, "NA")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# scoper

<details>

* Version: 1.1.0
* GitHub: NA
* Source code: https://github.com/cran/scoper
* Date/Publication: 2020-08-10 21:50:02 UTC
* Number of recursive dependencies: 115

Run `cloud_details(, "scoper")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/scoper/new/scoper.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘scoper/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘scoper’ version ‘1.1.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'alakazam', 'shazam'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/scoper/old/scoper.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘scoper/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘scoper’ version ‘1.1.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'alakazam', 'shazam'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# NA

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/NA
* Number of recursive dependencies: 0

Run `cloud_details(, "NA")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# SynthETIC

<details>

* Version: 1.0.1
* GitHub: https://github.com/agi-lab/SynthETIC
* Source code: https://github.com/cran/SynthETIC
* Date/Publication: 2021-06-26 15:50:02 UTC
* Number of recursive dependencies: 106

Run `cloud_details(, "SynthETIC")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/SynthETIC/new/SynthETIC.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘SynthETIC/DESCRIPTION’ ... OK
* this is package ‘SynthETIC’ version ‘1.0.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... NOTE
...
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘SynthETIC-demo.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/SynthETIC/old/SynthETIC.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘SynthETIC/DESCRIPTION’ ... OK
* this is package ‘SynthETIC’ version ‘1.0.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... NOTE
...
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘SynthETIC-demo.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... SKIPPED
* DONE
Status: 1 NOTE





```
# NA

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/NA
* Number of recursive dependencies: 0

Run `cloud_details(, "NA")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# tigger

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/tigger
* Date/Publication: 2020-05-13 05:10:03 UTC
* Number of recursive dependencies: 116

Run `cloud_details(, "tigger")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/tigger/new/tigger.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘tigger/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘tigger’ version ‘1.0.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'alakazam', 'shazam'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/tigger/old/tigger.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘tigger/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘tigger’ version ‘1.0.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'alakazam', 'shazam'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# trackr

<details>

* Version: 0.10.7
* GitHub: NA
* Source code: https://github.com/cran/trackr
* Date/Publication: 2021-05-24 14:50:02 UTC
* Number of recursive dependencies: 99

Run `cloud_details(, "trackr")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/trackr/new/trackr.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘trackr/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘trackr’ version ‘0.10.7’
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available:
  'histry', 'CodeDepends', 'rsolr', 'roprov'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/trackr/old/trackr.Rcheck’
* using R version 4.0.3 (2020-10-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using options ‘--no-manual --no-build-vignettes’
* checking for file ‘trackr/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘trackr’ version ‘0.10.7’
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available:
  'histry', 'CodeDepends', 'rsolr', 'roprov'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
