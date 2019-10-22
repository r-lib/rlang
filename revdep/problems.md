# arrow

<details>

* Version: 0.15.0
* Source code: https://github.com/cran/arrow
* URL: https://github.com/apache/arrow/, https://arrow.apache.org/docs/r
* BugReports: https://issues.apache.org/jira/projects/ARROW/issues
* Date/Publication: 2019-10-07 19:00:02 UTC
* Number of recursive dependencies: 50

Run `revdep_details(,"arrow")` for more info

</details>

## Newly broken

*   R CMD check timed out
    

## Newly fixed

*   checking whether package ‘arrow’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/arrow/old/arrow.Rcheck/00install.out’ for details.
    ```

# GenomicDataCommons

<details>

* Version: 1.8.0
* Source code: https://github.com/cran/GenomicDataCommons
* URL: https://bioconductor.org/packages/GenomicDataCommons, http://github.com/Bioconductor/GenomicDataCommons
* BugReports: https://github.com/Bioconductor/GenomicDataCommons/issues/new
* Date/Publication: 2019-05-02
* Number of recursive dependencies: 104

Run `revdep_details(,"GenomicDataCommons")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > # get some example file uuids
    > uuids <- files() %>%
    +     filter(~ access == 'open' & file_size < 100000) %>%
    +     results(size = 3) %>%
    +     ids()
    > 
    > # and get the data, placing it into the gdc_cache() directory
    > fpaths <- gdcdata(uuids, use_cached=TRUE)
    Error in .gdc_file_rename(destfile, to, overwrite) : 
      failed to rename downloaded file:
    
      from: '/Users/lionel/Library/Caches/GenomicDataCommons/663d87cd-8291-4db1-b6ec-4fde3b55ffda/.partial_download'
      to: '/Users/lionel/Library/Caches/GenomicDataCommons/663d87cd-8291-4db1-b6ec-4fde3b55ffda/nationwidechildrens.org_clinical.TCGA-BP-4343.xml'
      reason:
        cannot rename file
        '/Users/lionel/Library/Caches/GenomicDataCommons/663d87cd-8291-4db1-b6ec-4fde3b55ffda/.partial_download'
        to
        '/Users/lionel/Library/Caches/GenomicDataCommons/663d87cd-8291-4db1-b6ec-4fde3b55ffda/nationwidechildrens.org_clinical.TCGA-BP-4343.xml',
        reason 'No such file or directory'
    Calls: gdcdata -> mapply -> <Anonymous> -> .gdc_file_rename
    Execution halted
    ```

## In both

*   checking Rd \usage sections ... WARNING
    ```
    Undocumented arguments in documentation object '.htseq_importer'
      ‘fnames’
    
    Functions with \usage entries need to have the appropriate \alias
    entries, and all their arguments documented.
    The \usage entries must correspond to syntactically valid R code.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    default_fields.character: no visible binding for global variable
      ‘defaults’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/GenomicDataCommons/new/GenomicDataCommons.Rcheck/00_pkg_src/GenomicDataCommons/R/fields.R:51)
    gdc_rnaseq: no visible binding for global variable ‘case_id’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/GenomicDataCommons/new/GenomicDataCommons.Rcheck/00_pkg_src/GenomicDataCommons/R/gdc_rnaseq.R:106-107)
    gdc_rnaseq: no visible binding for global variable ‘file_id’
      (/Users/lionel/Desktop/rlang/revdep/checks.noindex/GenomicDataCommons/new/GenomicDataCommons.Rcheck/00_pkg_src/GenomicDataCommons/R/gdc_rnaseq.R:106-107)
    Undefined global functions or variables:
      case_id defaults file_id
    ```

# weathercan

<details>

* Version: 0.3.1
* Source code: https://github.com/cran/weathercan
* URL: https://github.com/ropensci/weathercan
* BugReports: https://github.com/ropensci/weathercan/issues
* Date/Publication: 2019-09-29 16:00:02 UTC
* Number of recursive dependencies: 131

Run `revdep_details(,"weathercan")` for more info

</details>

## Newly broken

*   R CMD check timed out
    

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 72 marked UTF-8 strings
    ```

