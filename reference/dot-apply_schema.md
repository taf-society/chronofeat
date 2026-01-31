# Apply Schema and Predictor Selection

Harmonizes a prediction row to match the training schema and selects
only the predictors needed by the model.

## Usage

``` r
.apply_schema(new_row, schema = NULL, predictors = NULL)
```

## Arguments

- new_row:

  Single-row tibble with raw features

- schema:

  Named list of column types from training (NULL to skip)

- predictors:

  Character vector of predictor names (NULL to skip)

## Value

Single-row tibble, harmonized and filtered
