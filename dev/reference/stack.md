# Get properties of the current or caller frame

These accessors retrieve properties of frames on the call stack. The
prefix indicates for which frame a property should be accessed:

- From the current frame with `current_` accessors.

- From a calling frame with `caller_` accessors.

- From a matching frame with `frame_` accessors.

The suffix indicates which property to retrieve:

- `_fn` accessors return the function running in the frame.

- `_call` accessors return the defused call with which the function
  running in the frame was invoked.

- `_env` accessors return the execution environment of the function
  running in the frame.

## Usage

``` r
current_call()

current_fn()

current_env()

caller_call(n = 1)

caller_fn(n = 1)

caller_env(n = 1)

frame_call(frame = caller_env())

frame_fn(frame = caller_env())
```

## Arguments

- n:

  The number of callers to go back.

- frame:

  A frame environment of a currently running function, as returned by
  `caller_env()`. `NULL` is returned if the environment does not exist
  on the stack.

## See also

`caller_env()` and `current_env()`
