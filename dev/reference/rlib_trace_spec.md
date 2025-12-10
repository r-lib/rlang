# Backtrace specification

**\[experimental\]**

## Structure

An r-lib backtrace is a data frame that contains the following columns:

- `call`: List of calls. These may carry `srcref` objects.

- `visible`: Logical vector. If `FALSE`, the corresponding call will be
  hidden from simplified backtraces.

- `parent`: Integer vector of parent references (see
  [`sys.parents()`](https://rdrr.io/r/base/sys.parent.html)) as row
  numbers. 0 is global.

- `namespace`: Character vector of namespaces. `NA` for global or no
  namespace

- `scope`: Character vector of strings taking values `"::"`, `":::"`,
  `"global"`, or `"local"`.

A backtrace data frame may contain extra columns. If you add additional
columns, make sure to prefix their names with the name of your package
or organisation to avoid potential conflicts with future extensions of
this spec, e.g. `"mypkg_column"`.

## Operations

- **Length**. The length of the backtrace is the number of rows of the
  underlying data.

- **Concatenation**. Performed by row-binding two backtraces. The
  `parent` column of the RHS is shifted by `nrow(LHS)` so that the last
  call of the LHS takes place of the global frame of the RHS.

- **Subsetting**. Performed by slicing the backtrace. After the data
  frame is sliced, the `parent` column is adjusted to the new row
  indices. Any `parent` value that no longer exists in the sliced
  backtrace is set to 0 (the global frame).
