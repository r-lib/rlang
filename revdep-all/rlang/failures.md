# arrow

<details>

* Version: 0.16.0.2
* Source code: https://github.com/cran/arrow
* URL: https://github.com/apache/arrow/, https://arrow.apache.org/docs/r
* BugReports: https://issues.apache.org/jira/projects/ARROW/issues
* Date/Publication: 2020-02-14 12:20:05 UTC
* Number of recursive dependencies: 52

Run `revdep_details(,"arrow")` for more info

</details>

## In both

*   checking whether package ‘arrow’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/arrow/new/arrow.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘arrow’ ...
** package ‘arrow’ successfully unpacked and MD5 sums checked
** using staged installation
Downloading apache-arrow
rm: fts_read: No such file or directory
Thu Feb 27 19:37:48 CET 2020: Auto-brewing apache-arrow in /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow...
/System/Library/Frameworks/Ruby.framework/Versions/2.6/usr/lib/ruby/2.6.0/rubygems/core_ext/kernel_require.rb:54:in `require': cannot load such file -- plist (LoadError)
	from /System/Library/Frameworks/Ruby.framework/Versions/2.6/usr/lib/ruby/2.6.0/rubygems/core_ext/kernel_require.rb:54:in `require'
	from /private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/build-apache-arrow/Library/Homebrew/system_command.rb:3:in `<top (required)>'
	from /System/Library/Frameworks/Ruby.framework/Versions/2.6/usr/lib/ruby/2.6.0/rubygems/core_ext/kernel_require.rb:54:in `require'
	from /System/Library/Frameworks/Ruby.framework/Versions/2.6/usr/lib/ruby/2.6.0/rubygems/core_ext/kernel_require.rb:54:in `require'
	from /private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/build-apache-arrow/Library/Homebrew/global.rb:14:in `<top (required)>'
	from /private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/build-apache-arrow/Library/Homebrew/brew.rb:15:in `require_relative'
	from /private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/build-apache-arrow/Library/Homebrew/brew.rb:15:in `<main>'
cp: /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/Cellar/*/*/lib/*.a: No such file or directory
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrew.a
PKG_CFLAGS=-I/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/include -DARROW_R_WITH_ARROW
PKG_LIBS=-L/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/lib -L/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib -lbrewparquet -lbrewarrow_dataset -lbrewarrow -lbrewthrift -lbrewlz4 -lbrewboost_system -lbrewboost_filesystem -lbrewboost_regex -lbrewsnappy
** libs
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/include -DARROW_R_WITH_ARROW -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/arrow/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c array.cpp -o array.o
In file included from array.cpp:18:
././arrow_types.h:198:10: fatal error: 'arrow/api.h' file not found
#include <arrow/api.h>
         ^~~~~~~~~~~~~
1 error generated.
make: *** [array.o] Error 1
ERROR: compilation failed for package ‘arrow’
* removing ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/arrow/new/arrow.Rcheck/arrow’

```
### CRAN

```
* installing *source* package ‘arrow’ ...
** package ‘arrow’ successfully unpacked and MD5 sums checked
** using staged installation
Downloading apache-arrow
Thu Feb 27 19:37:48 CET 2020: Auto-brewing apache-arrow in /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow...
/System/Library/Frameworks/Ruby.framework/Versions/2.6/usr/lib/ruby/2.6.0/rubygems/core_ext/kernel_require.rb:54:in `require': cannot load such file -- plist (LoadError)
	from /System/Library/Frameworks/Ruby.framework/Versions/2.6/usr/lib/ruby/2.6.0/rubygems/core_ext/kernel_require.rb:54:in `require'
	from /private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/build-apache-arrow/Library/Homebrew/system_command.rb:3:in `<top (required)>'
	from /System/Library/Frameworks/Ruby.framework/Versions/2.6/usr/lib/ruby/2.6.0/rubygems/core_ext/kernel_require.rb:54:in `require'
	from /System/Library/Frameworks/Ruby.framework/Versions/2.6/usr/lib/ruby/2.6.0/rubygems/core_ext/kernel_require.rb:54:in `require'
	from /private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/build-apache-arrow/Library/Homebrew/global.rb:14:in `<top (required)>'
	from /private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/build-apache-arrow/Library/Homebrew/brew.rb:15:in `require_relative'
	from /private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/build-apache-arrow/Library/Homebrew/brew.rb:15:in `<main>'
cp: /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/Cellar/*/*/lib/*.a: No such file or directory
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrew.a
PKG_CFLAGS=-I/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/include -DARROW_R_WITH_ARROW
PKG_LIBS=-L/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/lib -L/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib -lbrewparquet -lbrewarrow_dataset -lbrewarrow -lbrewthrift -lbrewlz4 -lbrewboost_system -lbrewboost_filesystem -lbrewboost_regex -lbrewsnappy
** libs
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/include -DARROW_R_WITH_ARROW -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/arrow/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c array.cpp -o array.o
In file included from array.cpp:18:
././arrow_types.h:198:10: fatal error: 'arrow/api.h' file not found
#include <arrow/api.h>
         ^~~~~~~~~~~~~
1 error generated.
make: *** [array.o] Error 1
ERROR: compilation failed for package ‘arrow’
* removing ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/arrow/old/arrow.Rcheck/arrow’

```
# BayesPostEst

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/BayesPostEst
* URL: https://github.com/ShanaScogin/BayesPostEst
* BugReports: https://github.com/ShanaScogin/BayesPostEst/issues
* Date/Publication: 2019-12-14 23:10:02 UTC
* Number of recursive dependencies: 140

Run `revdep_details(,"BayesPostEst")` for more info

</details>

## In both

*   checking whether package ‘BayesPostEst’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/BayesPostEst/new/BayesPostEst.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘BayesPostEst’ ...
** package ‘BayesPostEst’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
Warning: namespace ‘rjags’ is not available and has been replaced
by .GlobalEnv when processing object ‘jags_interactive’
Warning: namespace ‘rjags’ is not available and has been replaced
by .GlobalEnv when processing object ‘jags_interactive’
Warning: namespace ‘rjags’ is not available and has been replaced
by .GlobalEnv when processing object ‘jags_interactive_cat’
Warning: namespace ‘rjags’ is not available and has been replaced
by .GlobalEnv when processing object ‘jags_interactive_cat’
Warning: namespace ‘rjags’ is not available and has been replaced
by .GlobalEnv when processing object ‘jags_logit’
Warning: namespace ‘rjags’ is not available and has been replaced
by .GlobalEnv when processing object ‘jags_logit’
Warning: namespace ‘rjags’ is not available and has been replaced
by .GlobalEnv when processing object ‘jags_probit’
Warning: namespace ‘rjags’ is not available and has been replaced
by .GlobalEnv when processing object ‘jags_probit’
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/BayesPostEst/rjags/libs/rjags.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/BayesPostEst/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/BayesPostEst/rjags/libs/rjags.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘BayesPostEst’
* removing ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/BayesPostEst/new/BayesPostEst.Rcheck/BayesPostEst’

```
### CRAN

```
* installing *source* package ‘BayesPostEst’ ...
** package ‘BayesPostEst’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
Warning: namespace ‘rjags’ is not available and has been replaced
by .GlobalEnv when processing object ‘jags_interactive’
Warning: namespace ‘rjags’ is not available and has been replaced
by .GlobalEnv when processing object ‘jags_interactive’
Warning: namespace ‘rjags’ is not available and has been replaced
by .GlobalEnv when processing object ‘jags_interactive_cat’
Warning: namespace ‘rjags’ is not available and has been replaced
by .GlobalEnv when processing object ‘jags_interactive_cat’
Warning: namespace ‘rjags’ is not available and has been replaced
by .GlobalEnv when processing object ‘jags_logit’
Warning: namespace ‘rjags’ is not available and has been replaced
by .GlobalEnv when processing object ‘jags_logit’
Warning: namespace ‘rjags’ is not available and has been replaced
by .GlobalEnv when processing object ‘jags_probit’
Warning: namespace ‘rjags’ is not available and has been replaced
by .GlobalEnv when processing object ‘jags_probit’
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/BayesPostEst/rjags/libs/rjags.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/BayesPostEst/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/BayesPostEst/rjags/libs/rjags.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘BayesPostEst’
* removing ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/BayesPostEst/old/BayesPostEst.Rcheck/BayesPostEst’

```
# circumplex

<details>

* Version: 0.3.5
* Source code: https://github.com/cran/circumplex
* URL: https://github.com/jmgirard/circumplex
* BugReports: https://github.com/jmgirard/circumplex/issues
* Date/Publication: 2020-01-10 01:10:08 UTC
* Number of recursive dependencies: 94

Run `revdep_details(,"circumplex")` for more info

</details>

## In both

*   checking whether package ‘circumplex’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/circumplex/new/circumplex.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘circumplex’ ...
** package ‘circumplex’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/circumplex/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/circumplex/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘circumplex’
* removing ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/circumplex/new/circumplex.Rcheck/circumplex’

```
### CRAN

```
* installing *source* package ‘circumplex’ ...
** package ‘circumplex’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/circumplex/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/circumplex/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘circumplex’
* removing ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/circumplex/old/circumplex.Rcheck/circumplex’

```
# collector

<details>

* Version: 0.1.3
* Source code: https://github.com/cran/collector
* URL: https://collector.tidyrisk.org
* BugReports: https://github.com/davidski/collector/issues
* Date/Publication: 2020-02-18 00:10:02 UTC
* Number of recursive dependencies: 125

Run `revdep_details(,"collector")` for more info

</details>

## In both

*   checking whether package ‘collector’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/collector/new/collector.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘collector’ ...
** package ‘collector’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘spacyr’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘collector’
* removing ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/collector/new/collector.Rcheck/collector’

```
### CRAN

```
* installing *source* package ‘collector’ ...
** package ‘collector’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘spacyr’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘collector’
* removing ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/collector/old/collector.Rcheck/collector’

```
# detrendr

<details>

* Version: 0.6.4
* Source code: https://github.com/cran/detrendr
* URL: https://rorynolan.github.io/detrendr, https://www.github.com/rorynolan/detrendr
* BugReports: https://www.github.com/rorynolan/detrendr/issues
* Date/Publication: 2019-07-08 16:40:03 UTC
* Number of recursive dependencies: 92

Run `revdep_details(,"detrendr")` for more info

</details>

## In both

*   R CMD check timed out
    

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# dexter

<details>

* Version: 1.0.5
* Source code: https://github.com/cran/dexter
* URL: http://dexterities.netlify.com
* BugReports: https://github.com/jessekps/dexter/issues
* Date/Publication: 2020-02-14 15:30:08 UTC
* Number of recursive dependencies: 78

Run `revdep_details(,"dexter")` for more info

</details>

## In both

*   checking whether package ‘dexter’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/dexter/new/dexter.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘dexter’ ...
** package ‘dexter’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/dexter/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/dexter/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘dexter’
* removing ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/dexter/new/dexter.Rcheck/dexter’

```
### CRAN

```
* installing *source* package ‘dexter’ ...
** package ‘dexter’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/dexter/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/dexter/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘dexter’
* removing ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/dexter/old/dexter.Rcheck/dexter’

```
# ggiraph

<details>

* Version: 0.7.0
* Source code: https://github.com/cran/ggiraph
* URL: https://davidgohel.github.io/ggiraph
* BugReports: https://github.com/davidgohel/ggiraph/issues
* Date/Publication: 2019-10-31 18:20:02 UTC
* Number of recursive dependencies: 89

Run `revdep_details(,"ggiraph")` for more info

</details>

## In both

*   checking whether package ‘ggiraph’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/ggiraph/new/ggiraph.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘ggiraph’ ...
** package ‘ggiraph’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/ggiraph/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/ggiraph/gdtools/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c RcppExports.cpp -o RcppExports.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/ggiraph/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/ggiraph/gdtools/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c a_color.cpp -o a_color.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/ggiraph/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/ggiraph/gdtools/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c dsvg.cpp -o dsvg.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/ggiraph/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/ggiraph/gdtools/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c fonts.cpp -o fonts.o
clang++ -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o ggiraph.so RcppExports.o a_color.o dsvg.o fonts.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/ggiraph/new/ggiraph.Rcheck/00LOCK-ggiraph/00new/ggiraph/libs
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
Error: package or namespace load failed for ‘ggiraph’ in dyn.load(file, DLLpath = DLLpath, ...):
 unable to load shared object '/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/ggiraph/new/ggiraph.Rcheck/00LOCK-ggiraph/00new/ggiraph/libs/ggiraph.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/ggiraph/new/ggiraph.Rcheck/00LOCK-ggiraph/00new/ggiraph/libs/ggiraph.so, 6): Symbol not found: __ZN12CairoContextD1Ev
  Referenced from: /Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/ggiraph/new/ggiraph.Rcheck/00LOCK-ggiraph/00new/ggiraph/libs/ggiraph.so
  Expected in: flat namespace
 in /Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/ggiraph/new/ggiraph.Rcheck/00LOCK-ggiraph/00new/ggiraph/libs/ggiraph.so
Error: loading failed
Execution halted
ERROR: loading failed
* removing ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/ggiraph/new/ggiraph.Rcheck/ggiraph’

```
### CRAN

```
* installing *source* package ‘ggiraph’ ...
** package ‘ggiraph’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/ggiraph/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/ggiraph/gdtools/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c RcppExports.cpp -o RcppExports.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/ggiraph/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/ggiraph/gdtools/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c a_color.cpp -o a_color.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/ggiraph/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/ggiraph/gdtools/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c dsvg.cpp -o dsvg.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/ggiraph/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/ggiraph/gdtools/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c fonts.cpp -o fonts.o
clang++ -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o ggiraph.so RcppExports.o a_color.o dsvg.o fonts.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/ggiraph/old/ggiraph.Rcheck/00LOCK-ggiraph/00new/ggiraph/libs
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
Error: package or namespace load failed for ‘ggiraph’ in dyn.load(file, DLLpath = DLLpath, ...):
 unable to load shared object '/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/ggiraph/old/ggiraph.Rcheck/00LOCK-ggiraph/00new/ggiraph/libs/ggiraph.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/ggiraph/old/ggiraph.Rcheck/00LOCK-ggiraph/00new/ggiraph/libs/ggiraph.so, 6): Symbol not found: __ZN12CairoContextD1Ev
  Referenced from: /Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/ggiraph/old/ggiraph.Rcheck/00LOCK-ggiraph/00new/ggiraph/libs/ggiraph.so
  Expected in: flat namespace
 in /Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/ggiraph/old/ggiraph.Rcheck/00LOCK-ggiraph/00new/ggiraph/libs/ggiraph.so
Error: loading failed
Execution halted
ERROR: loading failed
* removing ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/ggiraph/old/ggiraph.Rcheck/ggiraph’

```
# iCNV

<details>

* Version: 1.4.0
* Source code: https://github.com/cran/iCNV
* Date/Publication: 2019-05-02
* Number of recursive dependencies: 91

Run `revdep_details(,"iCNV")` for more info

</details>

## In both

*   checking whether package ‘iCNV’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/iCNV/new/iCNV.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘iCNV’ ...
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package ‘BSgenome.Hsapiens.UCSC.hg19’ required by ‘CODEX’ could not be found
Execution halted
ERROR: lazy loading failed for package ‘iCNV’
* removing ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/iCNV/new/iCNV.Rcheck/iCNV’

```
### CRAN

```
* installing *source* package ‘iCNV’ ...
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package ‘BSgenome.Hsapiens.UCSC.hg19’ required by ‘CODEX’ could not be found
Execution halted
ERROR: lazy loading failed for package ‘iCNV’
* removing ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/iCNV/old/iCNV.Rcheck/iCNV’

```
# JointAI

<details>

* Version: 0.6.1
* Source code: https://github.com/cran/JointAI
* URL: https://nerler.github.io/JointAI
* BugReports: https://github.com/nerler/JointAI/issues
* Date/Publication: 2020-02-12 09:30:02 UTC
* Number of recursive dependencies: 86

Run `revdep_details(,"JointAI")` for more info

</details>

## In both

*   checking whether package ‘JointAI’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/JointAI/new/JointAI.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘JointAI’ ...
** package ‘JointAI’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rjags’:
 .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/JointAI/rjags/libs/rjags.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/JointAI/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/JointAI/rjags/libs/rjags.so
  Reason: image not found
Error: package ‘rjags’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘JointAI’
* removing ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/JointAI/new/JointAI.Rcheck/JointAI’

```
### CRAN

```
* installing *source* package ‘JointAI’ ...
** package ‘JointAI’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rjags’:
 .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/JointAI/rjags/libs/rjags.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/JointAI/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.4.dylib
  Referenced from: /Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/JointAI/rjags/libs/rjags.so
  Reason: image not found
Error: package ‘rjags’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘JointAI’
* removing ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/JointAI/old/JointAI.Rcheck/JointAI’

```
# PAST

<details>

* Version: 1.0.1
* Source code: https://github.com/cran/PAST
* URL: https://github.com/IGBB/past
* BugReports: https://github.com/IGBB/past/issues
* Date/Publication: 2019-08-07
* Number of recursive dependencies: 85

Run `revdep_details(,"PAST")` for more info

</details>

## In both

*   R CMD check timed out
    

*   checking top-level files ... NOTE
    ```
    File
      LICENSE
    is not mentioned in the DESCRIPTION file.
    ```

*   checking R code for possible problems ... NOTE
    ```
    assign_SNPs_to_genes: no visible binding for global variable
      ‘Marker_original.x’
      (/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/PAST/new/PAST.Rcheck/00_pkg_src/PAST/R/assign_SNPs_to_genes.R:467-490)
    find_pathway_significance: no visible binding for global variable
      ‘gene_id’
      (/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/PAST/new/PAST.Rcheck/00_pkg_src/PAST/R/find_pathway_significance.R:126-128)
    load_GWAS_data: no visible binding for global variable ‘Chr’
      (/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/PAST/new/PAST.Rcheck/00_pkg_src/PAST/R/load_GWAS_data.R:35-50)
    load_GWAS_data: no visible binding for global variable ‘Pos’
      (/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/PAST/new/PAST.Rcheck/00_pkg_src/PAST/R/load_GWAS_data.R:35-50)
    Undefined global functions or variables:
      Chr gene_id Marker_original.x Pos
    ```

# poppr

<details>

* Version: 2.8.5
* Source code: https://github.com/cran/poppr
* URL: https://grunwaldlab.github.io/poppr, https://github.com/grunwaldlab/poppr, https://grunwaldlab.github.io/Population_Genetics_in_R/
* BugReports: https://github.com/grunwaldlab/poppr/issues
* Date/Publication: 2020-02-25 10:10:02 UTC
* Number of recursive dependencies: 111

Run `revdep_details(,"poppr")` for more info

</details>

## In both

*   checking whether package ‘poppr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/poppr/new/poppr.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘poppr’ ...
** package ‘poppr’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c adjust_missing.c -o adjust_missing.o
clang: error: unsupported option '-fopenmp'
make: *** [adjust_missing.o] Error 1
ERROR: compilation failed for package ‘poppr’
* removing ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/poppr/new/poppr.Rcheck/poppr’

```
### CRAN

```
* installing *source* package ‘poppr’ ...
** package ‘poppr’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c adjust_missing.c -o adjust_missing.o
clang: error: unsupported option '-fopenmp'
make: *** [adjust_missing.o] Error 1
ERROR: compilation failed for package ‘poppr’
* removing ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/poppr/old/poppr.Rcheck/poppr’

```
# Rdrools

<details>

* Version: 1.1.1
* Source code: https://github.com/cran/Rdrools
* Date/Publication: 2018-12-08 15:00:13 UTC
* Number of recursive dependencies: 79

Run `revdep_details(,"Rdrools")` for more info

</details>

## In both

*   checking whether package ‘Rdrools’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/Rdrools/new/Rdrools.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘Rdrools’ ...
** package ‘Rdrools’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rJava’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/Rdrools/rJava/libs/rJava.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/Rdrools/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/Rdrools/rJava/libs/rJava.so
  Reason: image not found
Error: package ‘rJava’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘Rdrools’
* removing ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/Rdrools/new/Rdrools.Rcheck/Rdrools’

```
### CRAN

```
* installing *source* package ‘Rdrools’ ...
** package ‘Rdrools’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rJava’:
 .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/Rdrools/rJava/libs/rJava.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/Rdrools/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/Rdrools/rJava/libs/rJava.so
  Reason: image not found
Error: package ‘rJava’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘Rdrools’
* removing ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/Rdrools/old/Rdrools.Rcheck/Rdrools’

```
# RtutoR

<details>

* Version: 1.2
* Source code: https://github.com/cran/RtutoR
* Date/Publication: 2018-09-14 07:50:07 UTC
* Number of recursive dependencies: 121

Run `revdep_details(,"RtutoR")` for more info

</details>

## In both

*   checking whether package ‘RtutoR’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/RtutoR/new/RtutoR.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘RtutoR’ ...
** package ‘RtutoR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/RtutoR/rJava/libs/rJava.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/RtutoR/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/RtutoR/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘RtutoR’
* removing ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/RtutoR/new/RtutoR.Rcheck/RtutoR’

```
### CRAN

```
* installing *source* package ‘RtutoR’ ...
** package ‘RtutoR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/RtutoR/rJava/libs/rJava.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/RtutoR/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/RtutoR/rJava/libs/rJava.so
  Reason: image not found
Execution halted
ERROR: lazy loading failed for package ‘RtutoR’
* removing ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/RtutoR/old/RtutoR.Rcheck/RtutoR’

```
# rvg

<details>

* Version: 0.2.4
* Source code: https://github.com/cran/rvg
* URL: https://davidgohel.github.io/rvg
* BugReports: https://github.com/davidgohel/rvg/issues
* Date/Publication: 2020-02-17 22:10:03 UTC
* Number of recursive dependencies: 50

Run `revdep_details(,"rvg")` for more info

</details>

## In both

*   checking whether package ‘rvg’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/rvg/new/rvg.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘rvg’ ...
** package ‘rvg’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/gdtools/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c RcppExports.cpp -o RcppExports.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/gdtools/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c a_color.cpp -o a_color.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/gdtools/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c a_prstgeom.cpp -o a_prstgeom.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/gdtools/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c clipper.cpp -o clipper.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/gdtools/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c docx.cpp -o docx.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/gdtools/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c fonts.cpp -o fonts.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/gdtools/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c line_style.cpp -o line_style.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/gdtools/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c main_tree.cpp -o main_tree.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/gdtools/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c nv_pr.cpp -o nv_pr.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/gdtools/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c ppr.cpp -o ppr.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/gdtools/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c pptx.cpp -o pptx.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/gdtools/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c rpr.cpp -o rpr.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/gdtools/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c utils.cpp -o utils.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/gdtools/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c xfrm.cpp -o xfrm.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/gdtools/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c xlsx.cpp -o xlsx.o
clang++ -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o rvg.so RcppExports.o a_color.o a_prstgeom.o clipper.o docx.o fonts.o line_style.o main_tree.o nv_pr.o ppr.o pptx.o rpr.o utils.o xfrm.o xlsx.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/rvg/new/rvg.Rcheck/00LOCK-rvg/00new/rvg/libs
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
Error: package or namespace load failed for ‘rvg’ in dyn.load(file, DLLpath = DLLpath, ...):
 unable to load shared object '/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/rvg/new/rvg.Rcheck/00LOCK-rvg/00new/rvg/libs/rvg.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/rvg/new/rvg.Rcheck/00LOCK-rvg/00new/rvg/libs/rvg.so, 6): Symbol not found: __ZN12CairoContextD1Ev
  Referenced from: /Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/rvg/new/rvg.Rcheck/00LOCK-rvg/00new/rvg/libs/rvg.so
  Expected in: flat namespace
 in /Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/rvg/new/rvg.Rcheck/00LOCK-rvg/00new/rvg/libs/rvg.so
Error: loading failed
Execution halted
ERROR: loading failed
* removing ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/rvg/new/rvg.Rcheck/rvg’

```
### CRAN

```
* installing *source* package ‘rvg’ ...
** package ‘rvg’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/gdtools/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c RcppExports.cpp -o RcppExports.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/gdtools/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c a_color.cpp -o a_color.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/gdtools/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c a_prstgeom.cpp -o a_prstgeom.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/gdtools/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c clipper.cpp -o clipper.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/gdtools/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c docx.cpp -o docx.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/gdtools/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c fonts.cpp -o fonts.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/gdtools/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c line_style.cpp -o line_style.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/gdtools/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c main_tree.cpp -o main_tree.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/gdtools/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c nv_pr.cpp -o nv_pr.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/gdtools/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c ppr.cpp -o ppr.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/gdtools/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c pptx.cpp -o pptx.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/gdtools/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c rpr.cpp -o rpr.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/gdtools/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c utils.cpp -o utils.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/gdtools/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c xfrm.cpp -o xfrm.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/rvg/gdtools/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c xlsx.cpp -o xlsx.o
clang++ -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o rvg.so RcppExports.o a_color.o a_prstgeom.o clipper.o docx.o fonts.o line_style.o main_tree.o nv_pr.o ppr.o pptx.o rpr.o utils.o xfrm.o xlsx.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/rvg/old/rvg.Rcheck/00LOCK-rvg/00new/rvg/libs
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
Error: package or namespace load failed for ‘rvg’ in dyn.load(file, DLLpath = DLLpath, ...):
 unable to load shared object '/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/rvg/old/rvg.Rcheck/00LOCK-rvg/00new/rvg/libs/rvg.so':
  dlopen(/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/rvg/old/rvg.Rcheck/00LOCK-rvg/00new/rvg/libs/rvg.so, 6): Symbol not found: __ZN12CairoContextD1Ev
  Referenced from: /Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/rvg/old/rvg.Rcheck/00LOCK-rvg/00new/rvg/libs/rvg.so
  Expected in: flat namespace
 in /Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/rvg/old/rvg.Rcheck/00LOCK-rvg/00new/rvg/libs/rvg.so
Error: loading failed
Execution halted
ERROR: loading failed
* removing ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/rvg/old/rvg.Rcheck/rvg’

```
# sdcTable

<details>

* Version: 0.30
* Source code: https://github.com/cran/sdcTable
* URL: https://github.com/sdcTools/sdcTable
* BugReports: https://github.com/sdcTools/userSupport/issues
* Date/Publication: 2019-09-19 13:10:02 UTC
* Number of recursive dependencies: 56

Run `revdep_details(,"sdcTable")` for more info

</details>

## In both

*   checking whether package ‘sdcTable’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/sdcTable/new/sdcTable.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘sdcTable’ ...
** package ‘sdcTable’ successfully unpacked and MD5 sums checked
** using staged installation
GLPK is not available
ERROR: configuration failed for package ‘sdcTable’
* removing ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/sdcTable/new/sdcTable.Rcheck/sdcTable’

```
### CRAN

```
* installing *source* package ‘sdcTable’ ...
** package ‘sdcTable’ successfully unpacked and MD5 sums checked
** using staged installation
GLPK is not available
ERROR: configuration failed for package ‘sdcTable’
* removing ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/sdcTable/old/sdcTable.Rcheck/sdcTable’

```
# sismonr

<details>

* Version: 2.1.0
* Source code: https://github.com/cran/sismonr
* URL: https://oliviaab.github.io/sismonr/
* BugReports: https://github.com/oliviaAB/sismonr/issues
* Date/Publication: 2020-02-11 06:50:02 UTC
* Number of recursive dependencies: 79

Run `revdep_details(,"sismonr")` for more info

</details>

## In both

*   R CMD check timed out
    

*   checking S3 generic/method consistency ... WARNING
    ```
    ┌ Warning: Module StatsBase with build ID 394827600233716 is missing from the cache.
    │ This may mean StatsBase [2913bbd2-ae8a-5f71-8c99-4fb6c76f3a91] does not support precompilation but is imported by a module that does.
    └ @ Base loading.jl:947
    See section ‘Generic functions and methods’ in the ‘Writing R Extensions’
    manual.
    ```

*   checking replacement functions ... WARNING
    ```
    ┌ Warning: Module StatsBase with build ID 394827600233716 is missing from the cache.
    │ This may mean StatsBase [2913bbd2-ae8a-5f71-8c99-4fb6c76f3a91] does not support precompilation but is imported by a module that does.
    └ @ Base loading.jl:947
    The argument of a replacement function which corresponds to the right hand side
    must be named ‘value’.
    ```

*   checking dependencies in R code ... NOTE
    ```
    ┌ Warning: Module StatsBase with build ID 394827600233716 is missing from the cache.
    │ This may mean StatsBase [2913bbd2-ae8a-5f71-8c99-4fb6c76f3a91] does not support precompilation but is imported by a module that does.
    └ @ Base loading.jl:947
    ```

*   checking foreign function calls ... NOTE
    ```
    ┌ Warning: Module StatsBase with build ID 394827600233716 is missing from the cache.
    │ This may mean StatsBase [2913bbd2-ae8a-5f71-8c99-4fb6c76f3a91] does not support precompilation but is imported by a module that does.
    └ @ Base loading.jl:947
    See chapter ‘System and foreign language interfaces’ in the ‘Writing R
    Extensions’ manual.
    ```

# SpaDES.core

<details>

* Version: 1.0.0
* Source code: https://github.com/cran/SpaDES.core
* URL: https://spades-core.predictiveecology.org/, https://github.com/PredictiveEcology/SpaDES.core
* BugReports: https://github.com/PredictiveEcology/SpaDES.core/issues
* Date/Publication: 2020-02-21 13:30:09 UTC
* Number of recursive dependencies: 165

Run `revdep_details(,"SpaDES.core")` for more info

</details>

## In both

*   R CMD check timed out
    

# superml

<details>

* Version: 0.5.2
* Source code: https://github.com/cran/superml
* URL: https://github.com/saraswatmks/superml
* BugReports: https://github.com/saraswatmks/superml/issues
* Date/Publication: 2020-02-24 17:40:02 UTC
* Number of recursive dependencies: 103

Run `revdep_details(,"superml")` for more info

</details>

## In both

*   checking whether package ‘superml’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/superml/new/superml.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘superml’ ...
** package ‘superml’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/superml/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/superml/BH/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c RcppExports.cpp -o RcppExports.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/superml/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/superml/BH/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c utils.cpp -o utils.o
utils.cpp:45:68: error: a space is required between consecutive right angle brackets (use '> >')
                std::vector<std::string> rx = as<std::vector<string>>(r);
                                                                   ^~
                                                                   > >
utils.cpp:63:9: warning: 'auto' type specifier is a C++11 extension [-Wc++11-extensions]
    for(auto i: string){
        ^
utils.cpp:63:15: warning: range-based for loop is a C++11 extension [-Wc++11-extensions]
    for(auto i: string){
              ^
2 warnings and 1 error generated.
make: *** [utils.o] Error 1
ERROR: compilation failed for package ‘superml’
* removing ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/superml/new/superml.Rcheck/superml’

```
### CRAN

```
* installing *source* package ‘superml’ ...
** package ‘superml’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/superml/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/superml/BH/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c RcppExports.cpp -o RcppExports.o
clang++ -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/superml/Rcpp/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/superml/BH/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c utils.cpp -o utils.o
utils.cpp:45:68: error: a space is required between consecutive right angle brackets (use '> >')
                std::vector<std::string> rx = as<std::vector<string>>(r);
                                                                   ^~
                                                                   > >
utils.cpp:63:9: warning: 'auto' type specifier is a C++11 extension [-Wc++11-extensions]
    for(auto i: string){
        ^
utils.cpp:63:15: warning: range-based for loop is a C++11 extension [-Wc++11-extensions]
    for(auto i: string){
              ^
2 warnings and 1 error generated.
make: *** [utils.o] Error 1
ERROR: compilation failed for package ‘superml’
* removing ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/superml/old/superml.Rcheck/superml’

```
# trialr

<details>

* Version: 0.1.3
* Source code: https://github.com/cran/trialr
* URL: https://github.com/brockk/trialr
* BugReports: https://github.com/brockk/trialr/issues
* Date/Publication: 2020-01-08 22:30:10 UTC
* Number of recursive dependencies: 102

Run `revdep_details(,"trialr")` for more info

</details>

## In both

*   R CMD check timed out
    

# vctrs

<details>

* Version: 0.2.3
* Source code: https://github.com/cran/vctrs
* URL: https://github.com/r-lib/vctrs
* BugReports: https://github.com/r-lib/vctrs/issues
* Date/Publication: 2020-02-20 18:50:02 UTC
* Number of recursive dependencies: 63

Run `revdep_details(,"vctrs")` for more info

</details>

## In both

*   checking whether package ‘vctrs’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/vctrs/new/vctrs.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘vctrs’ ...
** package ‘vctrs’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c altrep-rle.c -o altrep-rle.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c arg-counter.c -o arg-counter.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c arg.c -o arg.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c bind.c -o bind.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c c.c -o c.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c cast-dispatch.c -o cast-dispatch.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c cast.c -o cast.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c compare.c -o compare.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c conditions.c -o conditions.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c dictionary.c -o dictionary.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c equal.c -o equal.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c fields.c -o fields.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c group.c -o group.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c growable.c -o growable.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c hash.c -o hash.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c init.c -o init.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c names.c -o names.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c proxy-restore.c -o proxy-restore.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c proxy.c -o proxy.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c ptype2-dispatch.c -o ptype2-dispatch.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c size-common.c -o size-common.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c size.c -o size.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c slice-array.c -o slice-array.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c slice-assign.c -o slice-assign.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c slice-chop.c -o slice-chop.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c slice.c -o slice.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c split.c -o split.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c subscript-loc.c -o subscript-loc.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c translate.c -o translate.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c type-data-frame.c -o type-data-frame.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c type-date-time.c -o type-date-time.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c type-factor.c -o type-factor.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c type-info.c -o type-info.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c type-list-of.c -o type-list-of.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c type-tibble.c -o type-tibble.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c type.c -o type.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c type2.c -o type2.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c typeof2-s3.c -o typeof2-s3.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c typeof2.c -o typeof2.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c unspecified.c -o unspecified.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c utils-dispatch.c -o utils-dispatch.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c utils.c -o utils.o
clang -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o vctrs.so altrep-rle.o arg-counter.o arg.o bind.o c.o cast-dispatch.o cast.o compare.o conditions.o dictionary.o equal.o fields.o group.o growable.o hash.o init.o names.o proxy-restore.o proxy.o ptype2-dispatch.o size-common.o size.o slice-array.o slice-assign.o slice-chop.o slice.o split.o subscript-loc.o translate.o type-data-frame.o type-date-time.o type-factor.o type-info.o type-list-of.o type-tibble.o type.o type2.o typeof2-s3.o typeof2.o unspecified.o utils-dispatch.o utils.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
duplicate symbol _altrep_rle_class in:
    altrep-rle.o
    init.o
ld: 1 duplicate symbol for architecture x86_64
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [vctrs.so] Error 1
ERROR: compilation failed for package ‘vctrs’
* removing ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/vctrs/new/vctrs.Rcheck/vctrs’

```
### CRAN

```
* installing *source* package ‘vctrs’ ...
** package ‘vctrs’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c altrep-rle.c -o altrep-rle.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c arg-counter.c -o arg-counter.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c arg.c -o arg.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c bind.c -o bind.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c c.c -o c.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c cast-dispatch.c -o cast-dispatch.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c cast.c -o cast.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c compare.c -o compare.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c conditions.c -o conditions.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c dictionary.c -o dictionary.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c equal.c -o equal.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c fields.c -o fields.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c group.c -o group.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c growable.c -o growable.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c hash.c -o hash.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c init.c -o init.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c names.c -o names.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c proxy-restore.c -o proxy-restore.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c proxy.c -o proxy.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c ptype2-dispatch.c -o ptype2-dispatch.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c size-common.c -o size-common.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c size.c -o size.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c slice-array.c -o slice-array.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c slice-assign.c -o slice-assign.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c slice-chop.c -o slice-chop.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c slice.c -o slice.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c split.c -o split.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c subscript-loc.c -o subscript-loc.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c translate.c -o translate.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c type-data-frame.c -o type-data-frame.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c type-date-time.c -o type-date-time.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c type-factor.c -o type-factor.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c type-info.c -o type-info.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c type-list-of.c -o type-list-of.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c type-tibble.c -o type-tibble.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c type.c -o type.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c type2.c -o type2.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c typeof2-s3.c -o typeof2-s3.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c typeof2.c -o typeof2.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c unspecified.c -o unspecified.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c utils-dispatch.c -o utils-dispatch.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c utils.c -o utils.o
clang -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o vctrs.so altrep-rle.o arg-counter.o arg.o bind.o c.o cast-dispatch.o cast.o compare.o conditions.o dictionary.o equal.o fields.o group.o growable.o hash.o init.o names.o proxy-restore.o proxy.o ptype2-dispatch.o size-common.o size.o slice-array.o slice-assign.o slice-chop.o slice.o split.o subscript-loc.o translate.o type-data-frame.o type-date-time.o type-factor.o type-info.o type-list-of.o type-tibble.o type.o type2.o typeof2-s3.o typeof2.o unspecified.o utils-dispatch.o utils.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
duplicate symbol _altrep_rle_class in:
    altrep-rle.o
    init.o
ld: 1 duplicate symbol for architecture x86_64
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [vctrs.so] Error 1
ERROR: compilation failed for package ‘vctrs’
* removing ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/vctrs/old/vctrs.Rcheck/vctrs’

```
# vroom

<details>

* Version: 1.2.0
* Source code: https://github.com/cran/vroom
* URL: https://github.com/r-lib/vroom
* BugReports: https://github.com/r-lib/vroom/issues
* Date/Publication: 2020-01-13 22:40:02 UTC
* Number of recursive dependencies: 89

Run `revdep_details(,"vroom")` for more info

</details>

## In both

*   checking whether package ‘vroom’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/vroom/new/vroom.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘vroom’ ...
** package ‘vroom’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/progress/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -Imio/include -DWIN32_LEAN_AND_MEAN -Ispdlog/include -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c Iconv.cpp -o Iconv.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/progress/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -Imio/include -DWIN32_LEAN_AND_MEAN -Ispdlog/include -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c LocaleInfo.cpp -o LocaleInfo.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/progress/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -Imio/include -DWIN32_LEAN_AND_MEAN -Ispdlog/include -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c RcppExports.cpp -o RcppExports.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/progress/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -Imio/include -DWIN32_LEAN_AND_MEAN -Ispdlog/include -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c altrep.cc -o altrep.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/progress/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -Imio/include -DWIN32_LEAN_AND_MEAN -Ispdlog/include -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c delimited_index.cc -o delimited_index.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/progress/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -Imio/include -DWIN32_LEAN_AND_MEAN -Ispdlog/include -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c delimited_index_connection.cc -o delimited_index_connection.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/progress/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -Imio/include -DWIN32_LEAN_AND_MEAN -Ispdlog/include -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c fixed_width_index_connection.cc -o fixed_width_index_connection.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/progress/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -Imio/include -DWIN32_LEAN_AND_MEAN -Ispdlog/include -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c gen.cc -o gen.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/progress/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c grisu3.c -o grisu3.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/progress/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -Imio/include -DWIN32_LEAN_AND_MEAN -Ispdlog/include -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c guess_type.cc -o guess_type.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/progress/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -Imio/include -DWIN32_LEAN_AND_MEAN -Ispdlog/include -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c index_collection.cc -o index_collection.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/progress/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c localtime.c -o localtime.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/progress/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -Imio/include -DWIN32_LEAN_AND_MEAN -Ispdlog/include -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c vroom.cc -o vroom.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/progress/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -Imio/include -DWIN32_LEAN_AND_MEAN -Ispdlog/include -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c vroom_big_int.cc -o vroom_big_int.o
vroom_big_int.cc:23:13: error: use of undeclared identifier 'LONG_LONG_MAX'
  if (val > LONG_LONG_MAX) {
            ^
1 error generated.
make: *** [vroom_big_int.o] Error 1
ERROR: compilation failed for package ‘vroom’
* removing ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/vroom/new/vroom.Rcheck/vroom’

```
### CRAN

```
* installing *source* package ‘vroom’ ...
** package ‘vroom’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/progress/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -Imio/include -DWIN32_LEAN_AND_MEAN -Ispdlog/include -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c Iconv.cpp -o Iconv.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/progress/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -Imio/include -DWIN32_LEAN_AND_MEAN -Ispdlog/include -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c LocaleInfo.cpp -o LocaleInfo.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/progress/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -Imio/include -DWIN32_LEAN_AND_MEAN -Ispdlog/include -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c RcppExports.cpp -o RcppExports.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/progress/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -Imio/include -DWIN32_LEAN_AND_MEAN -Ispdlog/include -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c altrep.cc -o altrep.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/progress/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -Imio/include -DWIN32_LEAN_AND_MEAN -Ispdlog/include -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c delimited_index.cc -o delimited_index.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/progress/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -Imio/include -DWIN32_LEAN_AND_MEAN -Ispdlog/include -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c delimited_index_connection.cc -o delimited_index_connection.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/progress/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -Imio/include -DWIN32_LEAN_AND_MEAN -Ispdlog/include -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c fixed_width_index_connection.cc -o fixed_width_index_connection.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/progress/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -Imio/include -DWIN32_LEAN_AND_MEAN -Ispdlog/include -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c gen.cc -o gen.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/progress/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c grisu3.c -o grisu3.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/progress/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -Imio/include -DWIN32_LEAN_AND_MEAN -Ispdlog/include -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c guess_type.cc -o guess_type.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/progress/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -Imio/include -DWIN32_LEAN_AND_MEAN -Ispdlog/include -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c index_collection.cc -o index_collection.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/progress/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2   -Wall -pedantic -fno-common -g -O0 -c localtime.c -o localtime.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/progress/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -Imio/include -DWIN32_LEAN_AND_MEAN -Ispdlog/include -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c vroom.cc -o vroom.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/progress/include" -I"/Users/lionel/Desktop/rlang/revdep-all/rlang/library.noindex/vroom/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -Imio/include -DWIN32_LEAN_AND_MEAN -Ispdlog/include -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c vroom_big_int.cc -o vroom_big_int.o
vroom_big_int.cc:23:13: error: use of undeclared identifier 'LONG_LONG_MAX'
  if (val > LONG_LONG_MAX) {
            ^
1 error generated.
make: *** [vroom_big_int.o] Error 1
ERROR: compilation failed for package ‘vroom’
* removing ‘/Users/lionel/Desktop/rlang/revdep-all/rlang/checks.noindex/vroom/old/vroom.Rcheck/vroom’

```
