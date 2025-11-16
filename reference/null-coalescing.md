# NULL-coalescing operator

Returns the right-hand side if the left-hand side is NULL, otherwise
returns the left-hand side.

## Usage

``` r
x %||% y
```

## Arguments

- x:

  Left-hand side value

- y:

  Right-hand side value

## Value

x if x is not NULL, otherwise y
