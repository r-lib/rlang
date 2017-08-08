# banR

Version: 0.2.0

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    If file is larger than 8 MB, it must be splitted
    Size is : 61 bytes
    SuccessOKSuccess: (200) OK
    # A tibble: 3 x 16
          y          z                     x latitude longitude
      <chr>      <dbl>                 <chr>    <dbl>     <dbl>
    1 75015 -0.6264538 39 quai Andre Citroen 48.84683  2.279092
    2 75012  0.1836433     64 Allee de Bercy 48.84255  2.375933
    3 75007 -0.8356286    20 avenue de Segur 48.85032  2.308332
    # ... with 11 more variables: result_label <chr>, result_score <dbl>,
    #   result_type <chr>, result_id <chr>, result_housenumber <int>,
    #   result_name <chr>, result_street <chr>, result_postcode <int>,
    #   result_city <chr>, result_context <chr>, result_citycode <chr>
    > geocode_tbl(tbl = table_test, adresse = x, code_postal = y)
    Writing tempfile to.../var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//RtmpTIqQ1D/file970d7b3ad7f0.csv
    If file is larger than 8 MB, it must be splitted
    Size is : 81 bytes
    Server errorService UnavailableServer error: (503) Service Unavailable
    Error in geocode_tbl(tbl = table_test, adresse = x, code_postal = y) : 
      The API sent back an error 503
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘stringr’
      All declared Imports should be used.
    ```

