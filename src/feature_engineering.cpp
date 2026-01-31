#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::depends(RcppArmadillo)]]

//' Compute lag features efficiently
//'
//' @param x Numeric vector to compute lags from
//' @param lags Integer vector of lag positions
//' @return Matrix where each column is a lag
// [[Rcpp::export]]
NumericMatrix compute_lags_cpp(NumericVector x, IntegerVector lags) {
  int n = x.size();
  int n_lags = lags.size();
  NumericMatrix result(n, n_lags);

  for (int j = 0; j < n_lags; j++) {
    int lag = lags[j];
    for (int i = 0; i < n; i++) {
      if (i < lag) {
        result(i, j) = NA_REAL;
      } else {
        result(i, j) = x[i - lag];
      }
    }
  }

  return result;
}

//' Compute moving averages efficiently
//'
//' @param x Numeric vector
//' @param windows Integer vector of window sizes
//' @return Matrix where each column is an MA with different window
// [[Rcpp::export]]
NumericMatrix compute_ma_cpp(NumericVector x, IntegerVector windows) {
  int n = x.size();
  int n_windows = windows.size();
  NumericMatrix result(n, n_windows);

  for (int j = 0; j < n_windows; j++) {
    int w = windows[j];
    for (int i = 0; i < n; i++) {
      if (i < w - 1) {
        result(i, j) = NA_REAL;
      } else {
        double sum = 0.0;
        int count = 0;
        for (int k = 0; k < w; k++) {
          if (!NumericVector::is_na(x[i - k])) {
            sum += x[i - k];
            count++;
          }
        }
        result(i, j) = count > 0 ? sum / count : NA_REAL;
      }
    }
  }

  return result;
}

//' Compute rolling sum efficiently
//'
//' @param x Numeric vector
//' @param windows Integer vector of window sizes
//' @return Matrix where each column is a rolling sum
//' @details Returns NA if the window is incomplete OR if all values in the window are NA.
//'          This matches the R behavior where sum of all-NA with na.rm handling returns NA.
// [[Rcpp::export]]
NumericMatrix compute_rollsum_cpp(NumericVector x, IntegerVector windows) {
  int n = x.size();
  int n_windows = windows.size();
  NumericMatrix result(n, n_windows);

  for (int j = 0; j < n_windows; j++) {
    int w = windows[j];
    for (int i = 0; i < n; i++) {
      if (i < w - 1) {
        result(i, j) = NA_REAL;
      } else {
        double sum = 0.0;
        int count = 0;
        for (int k = 0; k < w; k++) {
          if (!NumericVector::is_na(x[i - k])) {
            sum += x[i - k];
            count++;
          }
        }
        // Return NA if all values in window are NA
        result(i, j) = (count > 0) ? sum : NA_REAL;
      }
    }
  }

  return result;
}

//' Compute rolling standard deviation
//'
//' @param x Numeric vector
//' @param windows Integer vector of window sizes
//' @return Matrix where each column is a rolling SD
// [[Rcpp::export]]
NumericMatrix compute_rollsd_cpp(NumericVector x, IntegerVector windows) {
  int n = x.size();
  int n_windows = windows.size();
  NumericMatrix result(n, n_windows);

  for (int j = 0; j < n_windows; j++) {
    int w = windows[j];
    for (int i = 0; i < n; i++) {
      if (i < w - 1) {
        result(i, j) = NA_REAL;
      } else {
        double sum = 0.0;
        double sum_sq = 0.0;
        int count = 0;

        for (int k = 0; k < w; k++) {
          if (!NumericVector::is_na(x[i - k])) {
            double val = x[i - k];
            sum += val;
            sum_sq += val * val;
            count++;
          }
        }

        if (count < 2) {
          result(i, j) = NA_REAL;
        } else {
          double mean = sum / count;
          double variance = (sum_sq - count * mean * mean) / (count - 1);
          result(i, j) = sqrt(variance);
        }
      }
    }
  }

  return result;
}

//' Compute rolling min/max
//'
//' @param x Numeric vector
//' @param windows Integer vector of window sizes
//' @param compute_min Logical, if TRUE compute min, else max
//' @return Matrix where each column is rolling min or max
// [[Rcpp::export]]
NumericMatrix compute_rollminmax_cpp(NumericVector x, IntegerVector windows, bool compute_min = true) {
  int n = x.size();
  int n_windows = windows.size();
  NumericMatrix result(n, n_windows);

  for (int j = 0; j < n_windows; j++) {
    int w = windows[j];
    for (int i = 0; i < n; i++) {
      if (i < w - 1) {
        result(i, j) = NA_REAL;
      } else {
        double extreme = compute_min ? R_PosInf : R_NegInf;
        bool found = false;

        for (int k = 0; k < w; k++) {
          if (!NumericVector::is_na(x[i - k])) {
            found = true;
            if (compute_min) {
              extreme = std::min(extreme, (double)x[i - k]);
            } else {
              extreme = std::max(extreme, (double)x[i - k]);
            }
          }
        }
        result(i, j) = found ? extreme : NA_REAL;
      }
    }
  }

  return result;
}

//' Compute rolling linear trend slope
//'
//' @param x Numeric vector
//' @param windows Integer vector of window sizes
//' @return Matrix where each column is rolling slope
// [[Rcpp::export]]
NumericMatrix compute_rollslope_cpp(NumericVector x, IntegerVector windows) {
  int n = x.size();
  int n_windows = windows.size();
  NumericMatrix result(n, n_windows);

  for (int j = 0; j < n_windows; j++) {
    int w = windows[j];
    for (int i = 0; i < n; i++) {
      if (i < w - 1) {
        result(i, j) = NA_REAL;
      } else {
        // Collect valid observations
        std::vector<double> y_vals;
        std::vector<double> x_vals;

        for (int k = 0; k < w; k++) {
          if (!NumericVector::is_na(x[i - k])) {
            y_vals.push_back(x[i - k]);
            x_vals.push_back(w - k);  // Time index
          }
        }

        if (y_vals.size() < 2) {
          result(i, j) = NA_REAL;
        } else {
          // Compute linear regression slope: beta = cov(x,y) / var(x)
          double mean_x = 0.0, mean_y = 0.0;
          int count = y_vals.size();

          for (int k = 0; k < count; k++) {
            mean_x += x_vals[k];
            mean_y += y_vals[k];
          }
          mean_x /= count;
          mean_y /= count;

          double cov = 0.0, var_x = 0.0;
          for (int k = 0; k < count; k++) {
            double dx = x_vals[k] - mean_x;
            double dy = y_vals[k] - mean_y;
            cov += dx * dy;
            var_x += dx * dx;
          }

          result(i, j) = var_x > 0 ? cov / var_x : NA_REAL;
        }
      }
    }
  }

  return result;
}

//' Create target features for a single time point (used in recursive forecasting)
//'
//' @param y_hist Numeric vector of historical target values
//' @param p Integer, number of lags
//' @param q Integer vector of MA windows
//' @param roll_windows Integer vector of rolling stat windows
//' @param roll_stats Character vector of stats to compute
//' @param trend_windows Integer vector for rolling slopes
//' @return Named list of feature values
// [[Rcpp::export]]
List make_target_feats_cpp(NumericVector y_hist,
                           Nullable<int> p = R_NilValue,
                           Nullable<IntegerVector> q = R_NilValue,
                           Nullable<IntegerVector> roll_windows = R_NilValue,
                           Nullable<CharacterVector> roll_stats = R_NilValue,
                           Nullable<IntegerVector> trend_windows = R_NilValue) {
  List result;
  int n = y_hist.size();

  // Lags
  if (p.isNotNull()) {
    int n_lags = as<int>(p);
    for (int lag = 1; lag <= n_lags; lag++) {
      String name = "lag_" + std::to_string(lag);
      double val = (n >= lag) ? y_hist[n - lag] : NA_REAL;
      result[name] = val;
    }
  }

  // Moving averages
  if (q.isNotNull()) {
    IntegerVector windows = as<IntegerVector>(q);
    for (int i = 0; i < windows.size(); i++) {
      int w = windows[i];
      String name = "ma_" + std::to_string(w);

      if (n >= w) {
        double sum = 0.0;
        int count = 0;
        for (int k = 0; k < w; k++) {
          if (!NumericVector::is_na(y_hist[n - 1 - k])) {
            sum += y_hist[n - 1 - k];
            count++;
          }
        }
        result[name] = count > 0 ? sum / count : NA_REAL;
      } else {
        result[name] = NA_REAL;
      }
    }
  }

  // Rolling statistics
  // CRITICAL: Match R training behavior (.complete = TRUE) - return NA for incomplete windows
  if (roll_windows.isNotNull() && roll_stats.isNotNull()) {
    IntegerVector windows = as<IntegerVector>(roll_windows);
    CharacterVector stats = as<CharacterVector>(roll_stats);

    for (int i = 0; i < windows.size(); i++) {
      int w = windows[i];

      // Return NA for all stats if window is incomplete (matches .complete = TRUE)
      if (n < w) {
        for (int s = 0; s < stats.size(); s++) {
          String stat = stats[s];
          std::string name = std::string("roll") + stat.get_cstring() + "_" + std::to_string(w);
          result[name] = NA_REAL;
        }
        continue;
      }

      // Full window available - collect values
      std::vector<double> vals;
      for (int k = 0; k < w; k++) {
        if (!NumericVector::is_na(y_hist[n - 1 - k])) {
          vals.push_back(y_hist[n - 1 - k]);
        }
      }

      // Compute stats (return NA if all values in window are NA)
      for (int s = 0; s < stats.size(); s++) {
        String stat = stats[s];
        std::string name = std::string("roll") + stat.get_cstring() + "_" + std::to_string(w);

        if (vals.size() == 0) {
          result[name] = NA_REAL;
        } else if (stat == "sum") {
          double sum = 0.0;
          for (double v : vals) sum += v;
          result[name] = sum;
        } else if (stat == "sd") {
          if (vals.size() >= 2) {
            double mean = 0.0;
            for (double v : vals) mean += v;
            mean /= vals.size();
            double var = 0.0;
            for (double v : vals) var += (v - mean) * (v - mean);
            result[name] = sqrt(var / (vals.size() - 1));
          } else {
            result[name] = NA_REAL;
          }
        } else if (stat == "min") {
          double min_val = R_PosInf;
          for (double v : vals) min_val = std::min(min_val, v);
          result[name] = min_val;
        } else if (stat == "max") {
          double max_val = R_NegInf;
          for (double v : vals) max_val = std::max(max_val, v);
          result[name] = max_val;
        }
      }
    }
  }

  // Trend slopes
  // CRITICAL: Match R training behavior (.complete = TRUE) - return NA for incomplete windows
  if (trend_windows.isNotNull()) {
    IntegerVector windows = as<IntegerVector>(trend_windows);

    for (int i = 0; i < windows.size(); i++) {
      int w = windows[i];
      String name = "rollslope_" + std::to_string(w);

      // Return NA if window is incomplete (matches .complete = TRUE)
      if (n < w) {
        result[name] = NA_REAL;
        continue;
      }

      // Full window available - collect non-NA values
      std::vector<double> y_vals;
      std::vector<double> x_vals;

      for (int k = 0; k < w; k++) {
        if (!NumericVector::is_na(y_hist[n - w + k])) {
          y_vals.push_back(y_hist[n - w + k]);
          x_vals.push_back(k + 1);
        }
      }

      // Need at least 2 points to compute slope
      if (y_vals.size() >= 2) {
        double mean_x = 0.0, mean_y = 0.0;
        for (size_t k = 0; k < y_vals.size(); k++) {
          mean_x += x_vals[k];
          mean_y += y_vals[k];
        }
        mean_x /= y_vals.size();
        mean_y /= y_vals.size();

        double cov = 0.0, var_x = 0.0;
        for (size_t k = 0; k < y_vals.size(); k++) {
          double dx = x_vals[k] - mean_x;
          double dy = y_vals[k] - mean_y;
          cov += dx * dy;
          var_x += dx * dx;
        }

        result[name] = var_x > 0 ? cov / var_x : NA_REAL;
      } else {
        result[name] = NA_REAL;
      }
    }
  }

  return result;
}
