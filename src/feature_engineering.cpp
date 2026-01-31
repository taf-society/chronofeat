#include <cpp11.hpp>
#include <vector>
#include <cmath>
#include <algorithm>
#include <string>

using namespace cpp11;

// Helper: check if value is NA
inline bool is_na(double x) {
  return ISNA(x) || ISNAN(x);
}

//' Compute lag features efficiently
//'
//' @param x Numeric vector to compute lags from
//' @param lags Integer vector of lag positions
//' @return Matrix where each column is a lag
[[cpp11::register]]
doubles_matrix<> compute_lags_cpp(doubles x, integers lags) {
  R_xlen_t n = x.size();
  R_xlen_t n_lags = lags.size();

  writable::doubles_matrix<> result(n, n_lags);

  for (R_xlen_t j = 0; j < n_lags; j++) {
    int lag = lags[j];
    for (R_xlen_t i = 0; i < n; i++) {
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
[[cpp11::register]]
doubles_matrix<> compute_ma_cpp(doubles x, integers windows) {
  R_xlen_t n = x.size();
  R_xlen_t n_windows = windows.size();

  writable::doubles_matrix<> result(n, n_windows);

  for (R_xlen_t j = 0; j < n_windows; j++) {
    int w = windows[j];
    for (R_xlen_t i = 0; i < n; i++) {
      if (i < w - 1) {
        result(i, j) = NA_REAL;
      } else {
        double sum = 0.0;
        int count = 0;
        for (int k = 0; k < w; k++) {
          if (!is_na(x[i - k])) {
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
[[cpp11::register]]
doubles_matrix<> compute_rollsum_cpp(doubles x, integers windows) {
  R_xlen_t n = x.size();
  R_xlen_t n_windows = windows.size();

  writable::doubles_matrix<> result(n, n_windows);

  for (R_xlen_t j = 0; j < n_windows; j++) {
    int w = windows[j];
    for (R_xlen_t i = 0; i < n; i++) {
      if (i < w - 1) {
        result(i, j) = NA_REAL;
      } else {
        double sum = 0.0;
        int count = 0;
        for (int k = 0; k < w; k++) {
          if (!is_na(x[i - k])) {
            sum += x[i - k];
            count++;
          }
        }
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
[[cpp11::register]]
doubles_matrix<> compute_rollsd_cpp(doubles x, integers windows) {
  R_xlen_t n = x.size();
  R_xlen_t n_windows = windows.size();

  writable::doubles_matrix<> result(n, n_windows);

  for (R_xlen_t j = 0; j < n_windows; j++) {
    int w = windows[j];
    for (R_xlen_t i = 0; i < n; i++) {
      if (i < w - 1) {
        result(i, j) = NA_REAL;
      } else {
        double sum = 0.0;
        double sum_sq = 0.0;
        int count = 0;

        for (int k = 0; k < w; k++) {
          if (!is_na(x[i - k])) {
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
          result(i, j) = std::sqrt(variance);
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
[[cpp11::register]]
doubles_matrix<> compute_rollminmax_cpp(doubles x, integers windows, bool compute_min) {
  R_xlen_t n = x.size();
  R_xlen_t n_windows = windows.size();

  writable::doubles_matrix<> result(n, n_windows);

  for (R_xlen_t j = 0; j < n_windows; j++) {
    int w = windows[j];
    for (R_xlen_t i = 0; i < n; i++) {
      if (i < w - 1) {
        result(i, j) = NA_REAL;
      } else {
        double extreme = compute_min ? R_PosInf : R_NegInf;
        bool found = false;

        for (int k = 0; k < w; k++) {
          if (!is_na(x[i - k])) {
            found = true;
            if (compute_min) {
              extreme = std::min(extreme, x[i - k]);
            } else {
              extreme = std::max(extreme, x[i - k]);
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
[[cpp11::register]]
doubles_matrix<> compute_rollslope_cpp(doubles x, integers windows) {
  R_xlen_t n = x.size();
  R_xlen_t n_windows = windows.size();

  writable::doubles_matrix<> result(n, n_windows);

  for (R_xlen_t j = 0; j < n_windows; j++) {
    int w = windows[j];
    for (R_xlen_t i = 0; i < n; i++) {
      if (i < w - 1) {
        result(i, j) = NA_REAL;
      } else {
        std::vector<double> y_vals;
        std::vector<double> x_vals;

        for (int k = 0; k < w; k++) {
          if (!is_na(x[i - k])) {
            y_vals.push_back(x[i - k]);
            x_vals.push_back(w - k);
          }
        }

        if (y_vals.size() < 2) {
          result(i, j) = NA_REAL;
        } else {
          double mean_x = 0.0, mean_y = 0.0;
          size_t count = y_vals.size();

          for (size_t k = 0; k < count; k++) {
            mean_x += x_vals[k];
            mean_y += y_vals[k];
          }
          mean_x /= count;
          mean_y /= count;

          double cov = 0.0, var_x = 0.0;
          for (size_t k = 0; k < count; k++) {
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
//' @param p_sexp Integer vector of lag indices (NULL for none)
//' @param q_sexp Integer vector of MA windows (NULL for none)
//' @param roll_windows_sexp Integer vector of rolling stat windows (NULL for none)
//' @param roll_stats_sexp Character vector of stats to compute (NULL for none)
//' @param trend_windows_sexp Integer vector for rolling slopes (NULL for none)
//' @return Named list of feature values
[[cpp11::register]]
list make_target_feats_cpp(doubles y_hist,
                           SEXP p,
                           SEXP q,
                           SEXP roll_windows,
                           SEXP roll_stats,
                           SEXP trend_windows) {
  writable::list result;
  R_xlen_t n = y_hist.size();

  // Lags - p is now a vector of specific lag indices
  if (p != R_NilValue) {
    integers p_vec = as_cpp<integers>(p);
    for (R_xlen_t i = 0; i < p_vec.size(); i++) {
      int lag = p_vec[i];
      std::string name = "lag_" + std::to_string(lag);
      double val = (n >= lag) ? y_hist[n - lag] : NA_REAL;
      result.push_back(named_arg(name.c_str()) = val);
    }
  }

  // Moving averages
  if (q != R_NilValue) {
    integers q_vec = as_cpp<integers>(q);
    for (R_xlen_t i = 0; i < q_vec.size(); i++) {
      int w = q_vec[i];
      std::string name = "ma_" + std::to_string(w);

      if (n >= w) {
        double sum = 0.0;
        int count = 0;
        for (int k = 0; k < w; k++) {
          if (!is_na(y_hist[n - 1 - k])) {
            sum += y_hist[n - 1 - k];
            count++;
          }
        }
        result.push_back(named_arg(name.c_str()) = (count > 0 ? sum / count : NA_REAL));
      } else {
        result.push_back(named_arg(name.c_str()) = NA_REAL);
      }
    }
  }

  // Rolling statistics
  if (roll_windows != R_NilValue && roll_stats != R_NilValue) {
    integers roll_windows_vec = as_cpp<integers>(roll_windows);
    strings roll_stats_vec = as_cpp<strings>(roll_stats);

    for (R_xlen_t i = 0; i < roll_windows_vec.size(); i++) {
      int w = roll_windows_vec[i];

      if (n < w) {
        for (R_xlen_t s = 0; s < roll_stats_vec.size(); s++) {
          std::string stat(roll_stats_vec[s]);
          std::string name = "roll" + stat + "_" + std::to_string(w);
          result.push_back(named_arg(name.c_str()) = NA_REAL);
        }
        continue;
      }

      std::vector<double> vals;
      for (int k = 0; k < w; k++) {
        if (!is_na(y_hist[n - 1 - k])) {
          vals.push_back(y_hist[n - 1 - k]);
        }
      }

      for (R_xlen_t s = 0; s < roll_stats_vec.size(); s++) {
        std::string stat(roll_stats_vec[s]);
        std::string name = "roll" + stat + "_" + std::to_string(w);

        if (vals.empty()) {
          result.push_back(named_arg(name.c_str()) = NA_REAL);
        } else if (stat == "sum") {
          double sum = 0.0;
          for (double v : vals) sum += v;
          result.push_back(named_arg(name.c_str()) = sum);
        } else if (stat == "sd") {
          if (vals.size() >= 2) {
            double mean = 0.0;
            for (double v : vals) mean += v;
            mean /= vals.size();
            double var = 0.0;
            for (double v : vals) var += (v - mean) * (v - mean);
            result.push_back(named_arg(name.c_str()) = std::sqrt(var / (vals.size() - 1)));
          } else {
            result.push_back(named_arg(name.c_str()) = NA_REAL);
          }
        } else if (stat == "min") {
          double min_val = R_PosInf;
          for (double v : vals) min_val = std::min(min_val, v);
          result.push_back(named_arg(name.c_str()) = min_val);
        } else if (stat == "max") {
          double max_val = R_NegInf;
          for (double v : vals) max_val = std::max(max_val, v);
          result.push_back(named_arg(name.c_str()) = max_val);
        } else {
          result.push_back(named_arg(name.c_str()) = NA_REAL);
        }
      }
    }
  }

  // Trend slopes
  if (trend_windows != R_NilValue) {
    integers trend_windows_vec = as_cpp<integers>(trend_windows);

    for (R_xlen_t i = 0; i < trend_windows_vec.size(); i++) {
      int w = trend_windows_vec[i];
      std::string name = "rollslope_" + std::to_string(w);

      if (n < w) {
        result.push_back(named_arg(name.c_str()) = NA_REAL);
        continue;
      }

      std::vector<double> y_vals;
      std::vector<double> x_vals;

      for (int k = 0; k < w; k++) {
        if (!is_na(y_hist[n - w + k])) {
          y_vals.push_back(y_hist[n - w + k]);
          x_vals.push_back(k + 1);
        }
      }

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

        result.push_back(named_arg(name.c_str()) = (var_x > 0 ? cov / var_x : NA_REAL));
      } else {
        result.push_back(named_arg(name.c_str()) = NA_REAL);
      }
    }
  }

  return result;
}
