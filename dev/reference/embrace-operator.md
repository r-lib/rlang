# Embrace operator `{{`

The embrace operator `{{` is used to create functions that call other
[data-masking](https://rlang.r-lib.org/dev/reference/topic-data-mask.md)
functions. It transports a data-masked argument (an argument that can
refer to columns of a data frame) from one function to another.

    my_mean <- function(data, var) {
      dplyr::summarise(data, mean = mean({{ var }}))
    }

## Under the hood

`{{` combines
[`enquo()`](https://rlang.r-lib.org/dev/reference/enquo.md) and
[`!!`](https://rlang.r-lib.org/dev/reference/injection-operator.md) in
one step. The snippet above is equivalent to:

    my_mean <- function(data, var) {
      var <- enquo(var)
      dplyr::summarise(data, mean = mean(!!var))
    }

## See also

- [What is data-masking and why do I need
  {{?](https://rlang.r-lib.org/dev/reference/topic-data-mask.md)

- [Data mask programming
  patterns](https://rlang.r-lib.org/dev/reference/topic-data-mask-programming.md)
