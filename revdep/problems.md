# polmineR

<details>

* Version: 0.8.9
* GitHub: https://github.com/PolMine/polmineR
* Source code: https://github.com/cran/polmineR
* Date/Publication: 2023-10-29 21:50:02 UTC
* Number of recursive dependencies: 97

Run `revdepcheck::cloud_details(, "polmineR")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        R         2.0Mb
        extdata   1.9Mb
    ```

# rpaleoclim

<details>

* Version: 1.1.0
* GitHub: https://github.com/joeroe/rpaleoclim
* Source code: https://github.com/cran/rpaleoclim
* Date/Publication: 2025-09-30 09:00:02 UTC
* Number of recursive dependencies: 54

Run `revdepcheck::cloud_details(, "rpaleoclim")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘rpaleoclim-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: load_paleoclim
    > ### Title: Load data from PaleoClim
    > ### Aliases: load_paleoclim
    > 
    > ### ** Examples
    > 
    > file <- system.file("testdata", "LH_v1_10m_cropped.zip",
    ...
    
     *** caught segfault ***
    address 0x270, cause 'memory not mapped'
    
    Traceback:
     1: dir_map(old, identity, all, recurse, type, fail)
     2: fs::dir_ls(tmpdir, recurse = TRUE, glob = "*.tif")
     3: load_paleoclim(file)
    An irrecoverable exception occurred. R is aborting now ...
    Segmentation fault (core dumped)
    ```

