# Helper: Simple linear model for tests
# This replaces the removed model_lm() wrapper

test_model_lm <- function() {
 list(
   fit = function(y, X, ...) {
     if (ncol(X) > 0) {
       predictor_names <- colnames(X)
       formula_rhs <- paste(predictor_names, collapse = " + ")
       X_formula <- stats::as.formula(paste("~ ", formula_rhs))
       X_mat <- stats::model.matrix(X_formula, data = X)
     } else {
       X_mat <- matrix(1, nrow = nrow(X), ncol = 1)
       colnames(X_mat) <- "(Intercept)"
     }
     stats::lm.fit(x = X_mat, y = y)
   },
   predict = function(object, newdata, ...) {
     if (is.null(newdata) || nrow(newdata) == 0) return(numeric(0))
     if (ncol(newdata) > 0) {
       predictor_names <- colnames(newdata)
       formula_rhs <- paste(predictor_names, collapse = " + ")
       X_formula <- stats::as.formula(paste("~ ", formula_rhs))
       X_new <- stats::model.matrix(X_formula, data = newdata)
     } else {
       X_new <- matrix(1, nrow = nrow(newdata), ncol = 1)
       colnames(X_new) <- "(Intercept)"
     }
     as.numeric(X_new %*% object$coefficients)
   }
 )
}
