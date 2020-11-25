# alfred

<details>

* Version: 0.1.8
* Source code: https://github.com/cran/alfred
* URL: https://github.com/onnokleen/alfred/
* BugReports: https://github.com/onnokleen/alfred/issues
* Date/Publication: 2020-09-11 14:50:03 UTC
* Number of recursive dependencies: 77

Run `cloud_details(, "alfred")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── ERROR (test_download.R:2:3): Downloaded data is as expected ─────────────────
      Error: Download of specified time-series failed - did you misspell the identifier?
      Backtrace:
          █
       1. ├─testthat::expect_equal(...) test_download.R:2:2
       2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. └─alfred::get_alfred_series(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      ERROR (test_download.R:2:3): Downloaded data is as expected
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

# highcharter

<details>

* Version: 0.8.2
* Source code: https://github.com/cran/highcharter
* URL: http://jkunst.com/highcharter, https://github.com/jbkunst/highcharter
* BugReports: https://github.com/jbkunst/highcharter/issues
* Date/Publication: 2020-07-26 08:50:12 UTC
* Number of recursive dependencies: 135

Run `cloud_details(, "highcharter")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    >   
    > 
    > # Retrieve stock data to plot.
    > aapl <- quantmod::getSymbols("AAPL", 
    +   src = "yahoo",
    +   from = "2020-01-01",
    +   auto.assign = FALSE
    + )
    ‘getSymbols’ currently uses auto.assign=TRUE by default, but will
    use auto.assign=FALSE in 0.5-0. You will still be able to use
    ‘loadSymbols’ to automatically load data. getOption("getSymbols.env")
    and getOption("getSymbols.auto.assign") will still be checked for
    alternate defaults.
    
    This message is shown once per session and may be disabled by setting 
    options("getSymbols.warning4.0"=FALSE). See ?getSymbols for details.
    
    Error in curl::curl_fetch_memory(cu, handle = h) : 
      Timeout was reached: [finance.yahoo.com] Operation timed out after 10001 milliseconds with 0 out of 0 bytes received
    Calls: <Anonymous> ... getSymbols.yahoo -> .getHandle -> new.session -> <Anonymous>
    Execution halted
    ```

# torch

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/torch
* URL: https://torch.mlverse.org/docs, https://github.com/mlverse/torch
* BugReports: https://github.com/mlverse/torch/issues
* Date/Publication: 2020-10-20 21:10:02 UTC
* Number of recursive dependencies: 58

Run `cloud_details(, "torch")` for more info

</details>

## Newly broken

*   checking whether package ‘torch’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/torch/new/torch.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking installed package size ... NOTE
    ```
      installed size is 157.6Mb
      sub-directories of 1Mb or more:
        R       3.1Mb
        help    2.8Mb
        libs  151.2Mb
    ```

## Installation

### Devel

```
* installing *source* package ‘torch’ ...
** package ‘torch’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c autograd.cpp -o autograd.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c contrib.cpp -o contrib.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c cuda.cpp -o cuda.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c device.cpp -o device.o
In file included from device.cpp:2:
torch_types.h:7: warning: "LANTERN_HOST_HANDLER" redefined
    7 | #define LANTERN_HOST_HANDLER lantern_host_handler();
      | 
In file included from device.cpp:1:
lantern/lantern.h:35: note: this is the location of the previous definition
   35 | #define LANTERN_HOST_HANDLER
      | 
In file included from device.cpp:1:
lantern/lantern.h: In function ‘bool lanternInit(const string&, std::string*)’:
lantern/lantern.h:4097:6: note: variable tracking size limit exceeded with ‘-fvar-tracking-assignments’, retrying without
 4097 | bool lanternInit(const std::string &libPath, std::string *pError)
      |      ^~~~~~~~~~~
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c dimname_list.cpp -o dimname_list.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c dtype.cpp -o dtype.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c gen-namespace.cpp -o gen-namespace.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c generator.cpp -o generator.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c indexing.cpp -o indexing.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c lantern.cpp -o lantern.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c layout.cpp -o layout.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c memory_format.cpp -o memory_format.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c nn_utils_rnn.cpp -o nn_utils_rnn.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c qscheme.cpp -o qscheme.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c quantization.cpp -o quantization.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c reduction.cpp -o reduction.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c save.cpp -o save.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c scalar.cpp -o scalar.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c storage.cpp -o storage.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c tensor.cpp -o tensor.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c tensor_list.cpp -o tensor_list.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c tensor_options.cpp -o tensor_options.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c utils.cpp -o utils.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c variable_list.cpp -o variable_list.o
g++ -std=gnu++11 -shared -L/usr/lib/R/lib -Wl,-Bsymbolic-functions -Wl,-z,relro -o torch.so RcppExports.o autograd.o contrib.o cuda.o device.o dimname_list.o dtype.o gen-namespace.o generator.o indexing.o lantern.o layout.o memory_format.o nn_utils_rnn.o qscheme.o quantization.o reduction.o save.o scalar.o storage.o tensor.o tensor_list.o tensor_options.o utils.o variable_list.o -L/usr/lib/R/lib -lR
Renaming torch lib to torchpkg
"/usr/lib/R/bin/Rscript" "../tools/renamelib.R"
installing to /tmp/workdir/torch/new/torch.Rcheck/00LOCK-torch/00new/torch/libs
** R
Warning in dir.create(outCodeDir) :
  cannot create dir '/tmp/workdir/torch/new/torch.Rcheck/00LOCK-torch/00new/torch/R', reason 'No space left on device'
Error in .install_package_code_files(".", instdir) : 
  cannot open directory '/tmp/workdir/torch/new/torch.Rcheck/00LOCK-torch/00new/torch/R'
ERROR: unable to collate and parse R files for package ‘torch’
* removing ‘/tmp/workdir/torch/new/torch.Rcheck/torch’

```
### CRAN

```
* installing *source* package ‘torch’ ...
** package ‘torch’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c autograd.cpp -o autograd.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c contrib.cpp -o contrib.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c cuda.cpp -o cuda.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c device.cpp -o device.o
In file included from device.cpp:2:
torch_types.h:7: warning: "LANTERN_HOST_HANDLER" redefined
    7 | #define LANTERN_HOST_HANDLER lantern_host_handler();
      | 
In file included from device.cpp:1:
lantern/lantern.h:35: note: this is the location of the previous definition
   35 | #define LANTERN_HOST_HANDLER
      | 
In file included from device.cpp:1:
lantern/lantern.h: In function ‘bool lanternInit(const string&, std::string*)’:
lantern/lantern.h:4097:6: note: variable tracking size limit exceeded with ‘-fvar-tracking-assignments’, retrying without
 4097 | bool lanternInit(const std::string &libPath, std::string *pError)
      |      ^~~~~~~~~~~
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c dimname_list.cpp -o dimname_list.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c dtype.cpp -o dtype.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c gen-namespace.cpp -o gen-namespace.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c generator.cpp -o generator.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c indexing.cpp -o indexing.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c lantern.cpp -o lantern.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c layout.cpp -o layout.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c memory_format.cpp -o memory_format.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c nn_utils_rnn.cpp -o nn_utils_rnn.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c qscheme.cpp -o qscheme.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c quantization.cpp -o quantization.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c reduction.cpp -o reduction.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c save.cpp -o save.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c scalar.cpp -o scalar.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c storage.cpp -o storage.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c tensor.cpp -o tensor.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c tensor_list.cpp -o tensor_list.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c tensor_options.cpp -o tensor_options.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c utils.cpp -o utils.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG  -I"/usr/local/lib/R/site-library/Rcpp/include"   -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c variable_list.cpp -o variable_list.o
g++ -std=gnu++11 -shared -L/usr/lib/R/lib -Wl,-Bsymbolic-functions -Wl,-z,relro -o torch.so RcppExports.o autograd.o contrib.o cuda.o device.o dimname_list.o dtype.o gen-namespace.o generator.o indexing.o lantern.o layout.o memory_format.o nn_utils_rnn.o qscheme.o quantization.o reduction.o save.o scalar.o storage.o tensor.o tensor_list.o tensor_options.o utils.o variable_list.o -L/usr/lib/R/lib -lR
Renaming torch lib to torchpkg
"/usr/lib/R/bin/Rscript" "../tools/renamelib.R"
installing to /tmp/workdir/torch/old/torch.Rcheck/00LOCK-torch/00new/torch/libs
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (torch)

```
