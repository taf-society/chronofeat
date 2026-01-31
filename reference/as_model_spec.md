# Convert Model Function to Model Specification

Converts a model function (e.g., `lm`, `glm`) to a model specification
list with `fit` and `predict` functions compatible with the chronofeat
API.

## Usage

``` r
as_model_spec(model_fn, ...)
```

## Arguments

- model_fn:

  A model function that accepts a formula and data argument

- ...:

  Additional arguments passed to the model function during fitting

## Value

A list with `fit` and `predict` functions:

- `fit(y, X, ...)` - Fits the model with response y and predictors X

- `predict(object, newdata, ...)` - Generates predictions from fitted
  model

## Details

This function provides backward compatibility for users who want to pass
model functions directly (e.g., `model = lm`) instead of model
specifications.

The wrapper creates a formula-based interface internally, combining y
and X into a temporary data frame and fitting the model using the
standard R formula interface.

Supported model functions include any that accept the standard
formula/data interface: `lm`, `glm`,
[`randomForest::randomForest`](https://rdrr.io/pkg/randomForest/man/randomForest.html),
`ranger::ranger`, etc.

## Examples

``` r
if (FALSE) { # \dontrun{
# Convert lm to model spec
lm_spec <- as_model_spec(lm)

# Convert glm with family argument
glm_spec <- as_model_spec(glm, family = poisson())

# Use directly in fit()
m <- fit(value ~ p(12), data = df, date = "date", model = lm)
} # }
```
