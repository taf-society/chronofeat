# Custom User Function

Apply user-provided filling function.

## Usage

``` r
.fill_custom(y, dates, params = list())
```

## Arguments

- y:

  Numeric vector

- dates:

  Date vector

- params:

  List with:

  - fn: User function with signature fn(y, dates, params)

## Value

Filled numeric vector
