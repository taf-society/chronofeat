#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::depends(RcppArmadillo)]]

//' Fast recursive forecasting engine (no calendar/xreg version)
//'
//' This is the core forecasting loop optimized for speed.
//' Handles the case with only target-based features (lags, MAs, rolling stats).
//'
//' @param y_hist Numeric vector of historical target values
//' @param model_coef Numeric vector of model coefficients
//' @param has_intercept Logical, if TRUE first coef is intercept
//' @param h Integer, forecast horizon
//' @param p Integer vector of specific lag indices (e.g., c(1, 4, 6, 12))
//' @param q Integer vector of MA windows
//' @param roll_windows Integer vector of rolling stat windows
//' @param roll_stats Character vector of stats to compute
//' @param trend_windows Integer vector for rolling slopes
//' @param trend_degrees Integer vector for polynomial trends
//' @return Numeric vector of forecasts
// [[Rcpp::export]]
NumericVector recursive_forecast_simple_cpp(
    NumericVector y_hist,
    NumericVector model_coef,
    bool has_intercept,
    int h,
    Nullable<IntegerVector> p = R_NilValue,
    Nullable<IntegerVector> q = R_NilValue,
    Nullable<IntegerVector> roll_windows = R_NilValue,
    Nullable<CharacterVector> roll_stats = R_NilValue,
    Nullable<IntegerVector> trend_windows = R_NilValue,
    Nullable<IntegerVector> trend_degrees = R_NilValue) {

  NumericVector forecasts(h);
  NumericVector y_extended = clone(y_hist);
  int history_len = y_hist.size();

  // Build feature name order to match coefficient order
  std::vector<String> feature_names;
  if (has_intercept) {
    feature_names.push_back("(Intercept)");
  }

  // Lags - p is now a vector of specific lag indices
  if (p.isNotNull()) {
    IntegerVector lag_indices = as<IntegerVector>(p);
    for (int i = 0; i < lag_indices.size(); i++) {
      feature_names.push_back("lag_" + std::to_string(lag_indices[i]));
    }
  }

  // MAs
  if (q.isNotNull()) {
    IntegerVector windows = as<IntegerVector>(q);
    for (int i = 0; i < windows.size(); i++) {
      feature_names.push_back("ma_" + std::to_string(windows[i]));
    }
  }

  // Trends
  if (trend_degrees.isNotNull()) {
    IntegerVector degrees = as<IntegerVector>(trend_degrees);
    for (int i = 0; i < degrees.size(); i++) {
      feature_names.push_back("trend" + std::to_string(degrees[i]));
    }
  }

  // Rolling stats
  if (roll_windows.isNotNull() && roll_stats.isNotNull()) {
    IntegerVector windows = as<IntegerVector>(roll_windows);
    CharacterVector stats = as<CharacterVector>(roll_stats);

    for (int i = 0; i < windows.size(); i++) {
      for (int s = 0; s < stats.size(); s++) {
        String stat = stats[s];
        std::string name = std::string("roll") + stat.get_cstring() + "_" + std::to_string(windows[i]);
        feature_names.push_back(name);
      }
    }
  }

  // Trend slopes
  if (trend_windows.isNotNull()) {
    IntegerVector windows = as<IntegerVector>(trend_windows);
    for (int i = 0; i < windows.size(); i++) {
      feature_names.push_back("rollslope_" + std::to_string(windows[i]));
    }
  }

  // Main forecasting loop
  for (int t = 0; t < h; t++) {
    NumericVector features(feature_names.size());
    int feat_idx = 0;

    // Intercept
    if (has_intercept) {
      features[feat_idx++] = 1.0;
    }

    int n = y_extended.size();

    // Lags - iterate over specific lag indices
    if (p.isNotNull()) {
      IntegerVector lag_indices = as<IntegerVector>(p);
      for (int i = 0; i < lag_indices.size(); i++) {
        int lag = lag_indices[i];
        features[feat_idx++] = (n >= lag) ? y_extended[n - lag] : NA_REAL;
      }
    }

    // MAs
    if (q.isNotNull()) {
      IntegerVector windows = as<IntegerVector>(q);
      for (int i = 0; i < windows.size(); i++) {
        int w = windows[i];
        if (n >= w) {
          double sum = 0.0;
          int count = 0;
          for (int k = 0; k < w; k++) {
            if (!NumericVector::is_na(y_extended[n - 1 - k])) {
              sum += y_extended[n - 1 - k];
              count++;
            }
          }
          features[feat_idx++] = count > 0 ? sum / count : NA_REAL;
        } else {
          features[feat_idx++] = NA_REAL;
        }
      }
    }

    // Trend features
    if (trend_degrees.isNotNull()) {
      IntegerVector degrees = as<IntegerVector>(trend_degrees);
      int trend_idx = history_len + t + 1;
      for (int i = 0; i < degrees.size(); i++) {
        int d = degrees[i];
        features[feat_idx++] = pow(trend_idx, d);
      }
    }

    // Rolling stats
    if (roll_windows.isNotNull() && roll_stats.isNotNull()) {
      IntegerVector windows = as<IntegerVector>(roll_windows);
      CharacterVector stats = as<CharacterVector>(roll_stats);

      for (int i = 0; i < windows.size(); i++) {
        int w = windows[i];

        // Match training behavior: return NA if window is incomplete
        if (n < w) {
          for (int s = 0; s < stats.size(); s++) {
            features[feat_idx++] = NA_REAL;
          }
          continue;
        }

        std::vector<double> vals;
        for (int k = 0; k < w; k++) {
          if (!NumericVector::is_na(y_extended[n - 1 - k])) {
            vals.push_back(y_extended[n - 1 - k]);
          }
        }

        for (int s = 0; s < stats.size(); s++) {
          String stat = stats[s];

          if (vals.size() > 0) {
            if (stat == "sum") {
              double sum = 0.0;
              for (double v : vals) sum += v;
              features[feat_idx++] = sum;
            } else if (stat == "sd") {
              if (vals.size() >= 2) {
                double mean = 0.0;
                for (double v : vals) mean += v;
                mean /= vals.size();
                double var = 0.0;
                for (double v : vals) var += (v - mean) * (v - mean);
                features[feat_idx++] = sqrt(var / (vals.size() - 1));
              } else {
                features[feat_idx++] = NA_REAL;
              }
            } else if (stat == "min") {
              double min_val = R_PosInf;
              for (double v : vals) min_val = std::min(min_val, v);
              features[feat_idx++] = min_val;
            } else if (stat == "max") {
              double max_val = R_NegInf;
              for (double v : vals) max_val = std::max(max_val, v);
              features[feat_idx++] = max_val;
            } else {
              features[feat_idx++] = NA_REAL;
            }
          } else {
            features[feat_idx++] = NA_REAL;
          }
        }
      }
    }

    // Trend slopes
    if (trend_windows.isNotNull()) {
      IntegerVector windows = as<IntegerVector>(trend_windows);

      for (int i = 0; i < windows.size(); i++) {
        int w = windows[i];

        // Match training behavior: return NA if window is incomplete
        if (n < w) {
          features[feat_idx++] = NA_REAL;
          continue;
        }

        std::vector<double> y_vals;
        std::vector<double> x_vals;

        for (int k = 0; k < w; k++) {
          if (!NumericVector::is_na(y_extended[n - w + k])) {
            y_vals.push_back(y_extended[n - w + k]);
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

          features[feat_idx++] = var_x > 0 ? cov / var_x : NA_REAL;
        } else {
          features[feat_idx++] = NA_REAL;
        }
      }
    }

    // Compute prediction: dot product of features and coefficients
    double pred = 0.0;
    for (int i = 0; i < features.size(); i++) {
      if (!NumericVector::is_na(features[i]) && i < model_coef.size()) {
        pred += features[i] * model_coef[i];
      }
    }

    forecasts[t] = pred;
    y_extended.push_back(pred);
  }

  return forecasts;
}

//' Build feature matrix for recursive forecasting (model-agnostic)
//'
//' Builds the complete feature matrix for all h forecast steps.
//' Returns predictions as a vector that gets updated recursively.
//' The caller provides predicted values which are used to update history.
//'
//' @param y_hist Numeric vector of historical target values
//' @param h Integer, forecast horizon
//' @param p Integer vector of specific lag indices (e.g., c(1, 4, 6, 12))
//' @param q Integer vector of MA windows
//' @param roll_windows Integer vector of rolling stat windows
//' @param roll_stats Character vector of stats to compute
//' @param trend_windows Integer vector for rolling slopes
//' @param trend_degrees Integer vector for polynomial trends
//' @param y_future Numeric vector of predicted values (updated recursively)
//' @return Numeric matrix (h x n_features) of features for prediction
// [[Rcpp::export]]
NumericMatrix build_features_recursive_cpp(
    NumericVector y_hist,
    int h,
    Nullable<IntegerVector> p = R_NilValue,
    Nullable<IntegerVector> q = R_NilValue,
    Nullable<IntegerVector> roll_windows = R_NilValue,
    Nullable<CharacterVector> roll_stats = R_NilValue,
    Nullable<IntegerVector> trend_windows = R_NilValue,
    Nullable<IntegerVector> trend_degrees = R_NilValue,
    Nullable<NumericVector> y_future = R_NilValue) {

  NumericVector y_extended = clone(y_hist);
  int history_len = y_hist.size();

  // Count features
  int n_features = 0;
  if (p.isNotNull()) n_features += as<IntegerVector>(p).size();
  if (q.isNotNull()) n_features += as<IntegerVector>(q).size();
  if (trend_degrees.isNotNull()) n_features += as<IntegerVector>(trend_degrees).size();
  if (roll_windows.isNotNull() && roll_stats.isNotNull()) {
    n_features += as<IntegerVector>(roll_windows).size() * as<CharacterVector>(roll_stats).size();
  }
  if (trend_windows.isNotNull()) n_features += as<IntegerVector>(trend_windows).size();

  NumericMatrix feature_matrix(h, n_features);

  // If predictions provided, use them for recursive updates
  NumericVector preds_vec(h);
  bool use_predictions = y_future.isNotNull();
  if (use_predictions) {
    preds_vec = as<NumericVector>(y_future);
  }

  // Build features for each time step
  for (int t = 0; t < h; t++) {
    int feat_idx = 0;
    int n = y_extended.size();

    // Lags - iterate over specific lag indices
    if (p.isNotNull()) {
      IntegerVector lag_indices = as<IntegerVector>(p);
      for (int i = 0; i < lag_indices.size(); i++) {
        int lag = lag_indices[i];
        feature_matrix(t, feat_idx++) = (n >= lag) ? y_extended[n - lag] : NA_REAL;
      }
    }

    // MAs
    if (q.isNotNull()) {
      IntegerVector windows = as<IntegerVector>(q);
      for (int i = 0; i < windows.size(); i++) {
        int w = windows[i];
        if (n >= w) {
          double sum = 0.0;
          int count = 0;
          for (int k = 0; k < w; k++) {
            if (!NumericVector::is_na(y_extended[n - 1 - k])) {
              sum += y_extended[n - 1 - k];
              count++;
            }
          }
          feature_matrix(t, feat_idx++) = count > 0 ? sum / count : NA_REAL;
        } else {
          feature_matrix(t, feat_idx++) = NA_REAL;
        }
      }
    }

    // Trend features
    if (trend_degrees.isNotNull()) {
      IntegerVector degrees = as<IntegerVector>(trend_degrees);
      int trend_idx = history_len + t + 1;
      for (int i = 0; i < degrees.size(); i++) {
        int d = degrees[i];
        feature_matrix(t, feat_idx++) = pow(trend_idx, d);
      }
    }

    // Rolling stats
    if (roll_windows.isNotNull() && roll_stats.isNotNull()) {
      IntegerVector windows = as<IntegerVector>(roll_windows);
      CharacterVector stats = as<CharacterVector>(roll_stats);

      for (int i = 0; i < windows.size(); i++) {
        int w = windows[i];

        // Match training behavior: return NA if window is incomplete
        if (n < w) {
          for (int s = 0; s < stats.size(); s++) {
            feature_matrix(t, feat_idx++) = NA_REAL;
          }
          continue;
        }

        std::vector<double> vals;
        for (int k = 0; k < w; k++) {
          if (!NumericVector::is_na(y_extended[n - 1 - k])) {
            vals.push_back(y_extended[n - 1 - k]);
          }
        }

        for (int s = 0; s < stats.size(); s++) {
          String stat = stats[s];

          if (vals.size() > 0) {
            if (stat == "sum") {
              double sum = 0.0;
              for (double v : vals) sum += v;
              feature_matrix(t, feat_idx++) = sum;
            } else if (stat == "sd") {
              if (vals.size() >= 2) {
                double mean = 0.0;
                for (double v : vals) mean += v;
                mean /= vals.size();
                double var = 0.0;
                for (double v : vals) var += (v - mean) * (v - mean);
                feature_matrix(t, feat_idx++) = sqrt(var / (vals.size() - 1));
              } else {
                feature_matrix(t, feat_idx++) = NA_REAL;
              }
            } else if (stat == "min") {
              double min_val = R_PosInf;
              for (double v : vals) min_val = std::min(min_val, v);
              feature_matrix(t, feat_idx++) = min_val;
            } else if (stat == "max") {
              double max_val = R_NegInf;
              for (double v : vals) max_val = std::max(max_val, v);
              feature_matrix(t, feat_idx++) = max_val;
            } else {
              feature_matrix(t, feat_idx++) = NA_REAL;
            }
          } else {
            feature_matrix(t, feat_idx++) = NA_REAL;
          }
        }
      }
    }

    // Trend slopes
    if (trend_windows.isNotNull()) {
      IntegerVector windows = as<IntegerVector>(trend_windows);

      for (int i = 0; i < windows.size(); i++) {
        int w = windows[i];

        // Match training behavior: return NA if window is incomplete
        if (n < w) {
          feature_matrix(t, feat_idx++) = NA_REAL;
          continue;
        }

        std::vector<double> y_vals;
        std::vector<double> x_vals;

        for (int k = 0; k < w; k++) {
          if (!NumericVector::is_na(y_extended[n - w + k])) {
            y_vals.push_back(y_extended[n - w + k]);
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

          feature_matrix(t, feat_idx++) = var_x > 0 ? cov / var_x : NA_REAL;
        } else {
          feature_matrix(t, feat_idx++) = NA_REAL;
        }
      }
    }

    // Update history with prediction if provided
    if (use_predictions && t < h) {
      y_extended.push_back(preds_vec[t]);
    }
  }

  return feature_matrix;
}

//' Fast recursive forecasting with batched predictions
//'
//' Model-agnostic optimizer - batches predictions to minimize R call overhead.
//' Uses adaptive batching: builds features for multiple steps, predicts in batch,
//' updates history, repeats. Much faster than per-step prediction.
//'
//' @param y_hist Numeric vector of historical target values
//' @param h Integer, forecast horizon
//' @param predict_fn R function that takes a numeric matrix and returns predictions
//' @param p Integer vector of specific lag indices (e.g., c(1, 4, 6, 12))
//' @param q Integer vector of MA windows
//' @param roll_windows Integer vector of rolling stat windows
//' @param roll_stats Character vector of stats to compute
//' @param trend_windows Integer vector for rolling slopes
//' @param trend_degrees Integer vector for polynomial trends
//' @param batch_size Integer, number of steps to predict at once (default: h for full batch)
//' @return Numeric vector of forecasts
// [[Rcpp::export]]
NumericVector forecast_iterative_cpp(
    NumericVector y_hist,
    int h,
    Function predict_fn,
    Nullable<IntegerVector> p = R_NilValue,
    Nullable<IntegerVector> q = R_NilValue,
    Nullable<IntegerVector> roll_windows = R_NilValue,
    Nullable<CharacterVector> roll_stats = R_NilValue,
    Nullable<IntegerVector> trend_windows = R_NilValue,
    Nullable<IntegerVector> trend_degrees = R_NilValue,
    int batch_size = -1) {

  NumericVector forecasts(h);
  NumericVector y_extended = clone(y_hist);
  int history_len = y_hist.size();

  // Precompute specification vectors
  // p is now a vector of specific lag indices (e.g., c(1, 4, 6, 12))
  IntegerVector p_vec = p.isNotNull() ? as<IntegerVector>(p) : IntegerVector();
  IntegerVector q_vec = q.isNotNull() ? as<IntegerVector>(q) : IntegerVector();
  IntegerVector roll_win_vec = roll_windows.isNotNull() ? as<IntegerVector>(roll_windows) : IntegerVector();
  CharacterVector roll_stat_vec = roll_stats.isNotNull() ? as<CharacterVector>(roll_stats) : CharacterVector();
  IntegerVector trend_win_vec = trend_windows.isNotNull() ? as<IntegerVector>(trend_windows) : IntegerVector();
  IntegerVector trend_deg_vec = trend_degrees.isNotNull() ? as<IntegerVector>(trend_degrees) : IntegerVector();

  // Count features
  int n_features = 0;
  if (p_vec.size() > 0) n_features += p_vec.size();
  if (q_vec.size() > 0) n_features += q_vec.size();
  if (trend_deg_vec.size() > 0) n_features += trend_deg_vec.size();
  if (roll_win_vec.size() > 0 && roll_stat_vec.size() > 0) {
    n_features += roll_win_vec.size() * roll_stat_vec.size();
  }
  if (trend_win_vec.size() > 0) n_features += trend_win_vec.size();

  const bool have_features = n_features > 0;
  NumericMatrix feature_row(1, n_features);
  NumericMatrix empty_row(1, 0);

  // Determine if target history is required (lags, MAs, rolling stats, slopes)
  bool needs_history = false;
  if (p_vec.size() > 0) needs_history = true;
  if (!needs_history && q_vec.size() > 0) needs_history = true;
  if (!needs_history && roll_win_vec.size() > 0) needs_history = true;
  if (!needs_history && trend_win_vec.size() > 0) needs_history = true;

  auto fill_feature_row = [&](NumericMatrix& mat, int row_idx, int step_idx, int n_current) {
    if (!have_features) return;
    int feat_idx = 0;

    // Lags - iterate over specific lag indices
    if (p_vec.size() > 0) {
      for (int i = 0; i < p_vec.size(); ++i) {
        int lag = p_vec[i];
        mat(row_idx, feat_idx++) = (n_current >= lag) ? y_extended[n_current - lag] : NA_REAL;
      }
    }

    if (q_vec.size() > 0) {
      for (int i = 0; i < q_vec.size(); ++i) {
        int w = q_vec[i];
        if (n_current >= w) {
          double sum = 0.0;
          int count = 0;
          for (int k = 0; k < w; ++k) {
            if (!NumericVector::is_na(y_extended[n_current - 1 - k])) {
              sum += y_extended[n_current - 1 - k];
              count++;
            }
          }
          mat(row_idx, feat_idx++) = count > 0 ? sum / count : NA_REAL;
        } else {
          mat(row_idx, feat_idx++) = NA_REAL;
        }
      }
    }

    if (trend_deg_vec.size() > 0) {
      int trend_idx = history_len + step_idx + 1;
      for (int i = 0; i < trend_deg_vec.size(); ++i) {
        int d = trend_deg_vec[i];
        mat(row_idx, feat_idx++) = pow(trend_idx, d);
      }
    }

    if (roll_win_vec.size() > 0 && roll_stat_vec.size() > 0) {
      for (int i = 0; i < roll_win_vec.size(); ++i) {
        int w = roll_win_vec[i];

        // Refuse to compute if insufficient history (align with R-side validation)
        if (n_current < w) {
          for (int s = 0; s < roll_stat_vec.size(); ++s) {
            mat(row_idx, feat_idx++) = NA_REAL;
          }
          continue;
        }

        std::vector<double> vals;
        vals.reserve(w);
        for (int k = 0; k < w; ++k) {
          if (!NumericVector::is_na(y_extended[n_current - 1 - k])) {
            vals.push_back(y_extended[n_current - 1 - k]);
          }
        }

        for (int s = 0; s < roll_stat_vec.size(); ++s) {
          String stat = roll_stat_vec[s];

          if (!vals.empty()) {
            if (stat == "sum") {
              double sum = 0.0;
              for (double v : vals) sum += v;
              mat(row_idx, feat_idx++) = sum;
            } else if (stat == "sd") {
              if (vals.size() >= 2) {
                double mean = 0.0;
                for (double v : vals) mean += v;
                mean /= static_cast<double>(vals.size());
                double var = 0.0;
                for (double v : vals) {
                  double diff = v - mean;
                  var += diff * diff;
                }
                mat(row_idx, feat_idx++) = sqrt(var / (static_cast<double>(vals.size()) - 1.0));
              } else {
                mat(row_idx, feat_idx++) = NA_REAL;
              }
            } else if (stat == "min") {
              double min_val = vals[0];
              for (size_t idx = 1; idx < vals.size(); ++idx) {
                min_val = std::min(min_val, vals[idx]);
              }
              mat(row_idx, feat_idx++) = min_val;
            } else if (stat == "max") {
              double max_val = vals[0];
              for (size_t idx = 1; idx < vals.size(); ++idx) {
                max_val = std::max(max_val, vals[idx]);
              }
              mat(row_idx, feat_idx++) = max_val;
            } else {
              mat(row_idx, feat_idx++) = NA_REAL;
            }
          } else {
            mat(row_idx, feat_idx++) = NA_REAL;
          }
        }
      }
    }

    if (trend_win_vec.size() > 0) {
      for (int i = 0; i < trend_win_vec.size(); ++i) {
        int w = trend_win_vec[i];

        // Refuse to compute if insufficient history (align with R-side validation)
        if (n_current < w) {
          mat(row_idx, feat_idx++) = NA_REAL;
          continue;
        }

        std::vector<double> y_vals;
        std::vector<double> x_vals;
        y_vals.reserve(w);
        x_vals.reserve(w);

        for (int k = 0; k < w; ++k) {
          if (!NumericVector::is_na(y_extended[n_current - w + k])) {
            y_vals.push_back(y_extended[n_current - w + k]);
            x_vals.push_back(static_cast<double>(k + 1));
          }
        }

        if (y_vals.size() >= 2) {
          double mean_x = 0.0, mean_y = 0.0;
          for (size_t k = 0; k < y_vals.size(); ++k) {
            mean_x += x_vals[k];
            mean_y += y_vals[k];
          }
          mean_x /= static_cast<double>(y_vals.size());
          mean_y /= static_cast<double>(y_vals.size());

          double cov = 0.0, var_x = 0.0;
          for (size_t k = 0; k < y_vals.size(); ++k) {
            double dx = x_vals[k] - mean_x;
            double dy = y_vals[k] - mean_y;
            cov += dx * dy;
            var_x += dx * dx;
          }

          mat(row_idx, feat_idx++) = var_x > 0 ? cov / var_x : NA_REAL;
        } else {
          mat(row_idx, feat_idx++) = NA_REAL;
        }
      }
    }
  };

  if (needs_history || batch_size == 1) {
    for (int t = 0; t < h; ++t) {
      int n_current = y_extended.size();
      if (have_features) {
        fill_feature_row(feature_row, 0, t, n_current);
      }

      NumericVector pred_result = have_features ? predict_fn(feature_row) : predict_fn(empty_row);
      if (pred_result.size() == 0) {
        stop("Prediction function returned empty vector at step %d", t + 1);
      }

      double pred = pred_result[0];
      forecasts[t] = pred;
      y_extended.push_back(pred);
    }
    return forecasts;
  }

  // Batched predictions when there is no dependence on target history
  int step_idx = 0;
  while (step_idx < h) {
    int remaining = h - step_idx;
    int current_batch = batch_size <= 0 ? remaining : std::min(batch_size, remaining);

    NumericMatrix batch_features(current_batch, n_features);
    NumericMatrix batch_empty(current_batch, 0);
    if (have_features) {
      int n_current = y_extended.size();
      for (int r = 0; r < current_batch; ++r) {
        fill_feature_row(batch_features, r, step_idx + r, n_current);
      }
    }

    NumericVector preds = have_features ? predict_fn(batch_features) : predict_fn(batch_empty);
    if (preds.size() != current_batch) {
      stop("Prediction function returned %d values but %d were expected for batch.", preds.size(), current_batch);
    }

    for (int r = 0; r < current_batch; ++r) {
      double pred = preds[r];
      forecasts[step_idx + r] = pred;
      y_extended.push_back(pred);
    }

    step_idx += current_batch;
  }

  return forecasts;
}

//' Fast group-wise recursive forecasting
//'
//' Performs recursive forecasting for multiple groups in parallel.
//'
//' @param y_matrix Numeric matrix where each row is a group's history
//' @param model_coef Numeric vector of model coefficients
//' @param has_intercept Logical, if TRUE first coef is intercept
//' @param h Integer, forecast horizon
//' @param p Integer vector of specific lag indices (e.g., c(1, 4, 6, 12))
//' @param q Integer vector of MA windows
//' @param roll_windows Integer vector of rolling stat windows
//' @param roll_stats Character vector of stats to compute
//' @param trend_windows Integer vector for rolling slopes
//' @param trend_degrees Integer vector for polynomial trends
//' @return Numeric matrix where each row is a group's forecast
// [[Rcpp::export]]
NumericMatrix recursive_forecast_groups_cpp(
    NumericMatrix y_matrix,
    NumericVector model_coef,
    bool has_intercept,
    int h,
    Nullable<IntegerVector> p = R_NilValue,
    Nullable<IntegerVector> q = R_NilValue,
    Nullable<IntegerVector> roll_windows = R_NilValue,
    Nullable<CharacterVector> roll_stats = R_NilValue,
    Nullable<IntegerVector> trend_windows = R_NilValue,
    Nullable<IntegerVector> trend_degrees = R_NilValue) {

  int n_groups = y_matrix.nrow();
  NumericMatrix forecasts(n_groups, h);

  // Process each group
  for (int g = 0; g < n_groups; g++) {
    NumericVector y_hist = y_matrix(g, _);

    NumericVector group_forecast = recursive_forecast_simple_cpp(
      y_hist, model_coef, has_intercept, h,
      p, q, roll_windows, roll_stats, trend_windows, trend_degrees
    );

    forecasts(g, _) = group_forecast;
  }

  return forecasts;
}
