# Show injected expression

`qq_show()` helps examining [injected
expressions](https://rlang.r-lib.org/dev/reference/topic-inject.md)
inside a function. This is useful for learning about injection and for
debugging injection code.

## Arguments

- expr:

  An expression involving [injection
  operators](https://rlang.r-lib.org/dev/reference/topic-inject.md).

## Examples

`qq_show()` shows the intermediary expression before it is evaluated by
R:

    list2(!!!1:3)
    #> [[1]]
    #> [1] 1
    #>
    #> [[2]]
    #> [1] 2
    #>
    #> [[3]]
    #> [1] 3

    qq_show(list2(!!!1:3))
    #> list2(1L, 2L, 3L)

It is especially useful inside functions to reveal what an injected
expression looks like:

    my_mean <- function(data, var) {
      qq_show(data %>% dplyr::summarise(mean({{ var }})))
    }

    mtcars %>% my_mean(cyl)
    #> data %>% dplyr::summarise(mean(^cyl))

## See also

- [Injecting with !!, !!!, and glue
  syntax](https://rlang.r-lib.org/dev/reference/topic-inject.md)
