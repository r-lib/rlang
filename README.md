# rlang

[![Build Status](https://travis-ci.org/tidyverse/rlang.svg?branch=master)](https://travis-ci.org/tidyverse/rlang)
[![Coverage Status](https://img.shields.io/codecov/c/github/tidyverse/rlang/master.svg)](https://codecov.io/github/tidyverse/rlang?branch=master)

## Overview

The rlang package provides tools to work with core language features
of R and the tidyverse:

- Tidy evaluation.
- Base types: vectors, expressions, and environments.
- Call and context stacks.
- The condition system.


## Installation

rlang is not automatically installed with the `tidyverse` package, but
you can install it with:

```r
install.packages("rlang")
library("rlang")
```

Or install the development version from github with:

```r
# install.packages("devtools")
devtools::install_github("tidyverse/rlang", build_vignettes = TRUE)
```
