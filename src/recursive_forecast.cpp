#include <cpp11.hpp>
#include <vector>
#include <cmath>
#include <algorithm>
#include <string>

using namespace cpp11;

// Helper: check if value is NA
inline bool is_na_rf(double x) {
  return ISNA(x) || ISNAN(x);
}

//' Fast recursive forecasting engine (no calendar/xreg version)
//'
//' This is the core forecasting loop optimized for speed.
//' Handles the case with only target-based features (lags, MAs, rolling stats).
//'
//' @param y_hist Numeric vector of historical target values
//' @param model_coef Numeric vector of model coefficients
//' @param has_intercept Logical, if TRUE first coef is intercept
//' @param h Integer, forecast horizon
//' @param p Integer vector of specific lag indices
//' @param q Integer vector of MA windows
//' @param roll_windows Integer vector of rolling stat windows
//' @param roll_stats Character vector of stats to compute
//' @param trend_windows Integer vector for rolling slopes
//' @param trend_degrees Integer vector for polynomial trends
//' @return Numeric vector of forecasts
[[cpp11::register]]
doubles recursive_forecast_simple_cpp(
    doubles y_hist,
    doubles model_coef,
    bool has_intercept,
    int h,
    SEXP p,
    SEXP q,
    SEXP roll_windows,
    SEXP roll_stats,
    SEXP trend_windows,
    SEXP trend_degrees) {

  writable::doubles forecasts(h);
  std::vector<double> y_extended(y_hist.begin(), y_hist.end());
  int history_len = y_hist.size();

  // Parse nullable parameters
  bool have_p = (p != R_NilValue);
  bool have_q = (q != R_NilValue);
  bool have_roll = (roll_windows != R_NilValue && roll_stats != R_NilValue);
  bool have_trend_win = (trend_windows != R_NilValue);
  bool have_trend_deg = (trend_degrees != R_NilValue);

  std::vector<int> p_vec, q_vec, roll_win_vec, trend_win_vec, trend_deg_vec;
  std::vector<std::string> roll_stat_vec;

  if (have_p) {
    integers p_int = as_cpp<integers>(p);
    for (R_xlen_t i = 0; i < p_int.size(); i++) p_vec.push_back(p_int[i]);
  }
  if (have_q) {
    integers q_int = as_cpp<integers>(q);
    for (R_xlen_t i = 0; i < q_int.size(); i++) q_vec.push_back(q_int[i]);
  }
  if (have_roll) {
    integers rw_int = as_cpp<integers>(roll_windows);
    strings rs_str = as_cpp<strings>(roll_stats);
    for (R_xlen_t i = 0; i < rw_int.size(); i++) roll_win_vec.push_back(rw_int[i]);
    for (R_xlen_t i = 0; i < rs_str.size(); i++) roll_stat_vec.push_back(std::string(rs_str[i]));
  }
  if (have_trend_win) {
    integers tw_int = as_cpp<integers>(trend_windows);
    for (R_xlen_t i = 0; i < tw_int.size(); i++) trend_win_vec.push_back(tw_int[i]);
  }
  if (have_trend_deg) {
    integers td_int = as_cpp<integers>(trend_degrees);
    for (R_xlen_t i = 0; i < td_int.size(); i++) trend_deg_vec.push_back(td_int[i]);
  }

  // Main forecasting loop
  for (int t = 0; t < h; t++) {
    std::vector<double> features;

    // Intercept
    if (has_intercept) {
      features.push_back(1.0);
    }

    int n = y_extended.size();

    // Lags
    for (int lag : p_vec) {
      features.push_back((n >= lag) ? y_extended[n - lag] : NA_REAL);
    }

    // MAs
    for (int w : q_vec) {
      if (n >= w) {
        double sum = 0.0;
        int count = 0;
        for (int k = 0; k < w; k++) {
          if (!is_na_rf(y_extended[n - 1 - k])) {
            sum += y_extended[n - 1 - k];
            count++;
          }
        }
        features.push_back(count > 0 ? sum / count : NA_REAL);
      } else {
        features.push_back(NA_REAL);
      }
    }

    // Trend features
    if (!trend_deg_vec.empty()) {
      int trend_idx = history_len + t + 1;
      for (int d : trend_deg_vec) {
        features.push_back(std::pow(trend_idx, d));
      }
    }

    // Rolling stats
    for (size_t i = 0; i < roll_win_vec.size(); i++) {
      int w = roll_win_vec[i];

      if (n < w) {
        for (size_t s = 0; s < roll_stat_vec.size(); s++) {
          features.push_back(NA_REAL);
        }
        continue;
      }

      std::vector<double> vals;
      for (int k = 0; k < w; k++) {
        if (!is_na_rf(y_extended[n - 1 - k])) {
          vals.push_back(y_extended[n - 1 - k]);
        }
      }

      for (const std::string& stat : roll_stat_vec) {
        if (vals.empty()) {
          features.push_back(NA_REAL);
        } else if (stat == "sum") {
          double sum = 0.0;
          for (double v : vals) sum += v;
          features.push_back(sum);
        } else if (stat == "sd") {
          if (vals.size() >= 2) {
            double mean = 0.0;
            for (double v : vals) mean += v;
            mean /= vals.size();
            double var = 0.0;
            for (double v : vals) var += (v - mean) * (v - mean);
            features.push_back(std::sqrt(var / (vals.size() - 1)));
          } else {
            features.push_back(NA_REAL);
          }
        } else if (stat == "min") {
          double min_val = R_PosInf;
          for (double v : vals) min_val = std::min(min_val, v);
          features.push_back(min_val);
        } else if (stat == "max") {
          double max_val = R_NegInf;
          for (double v : vals) max_val = std::max(max_val, v);
          features.push_back(max_val);
        } else {
          features.push_back(NA_REAL);
        }
      }
    }

    // Trend slopes
    for (int w : trend_win_vec) {
      if (n < w) {
        features.push_back(NA_REAL);
        continue;
      }

      std::vector<double> y_vals;
      std::vector<double> x_vals;

      for (int k = 0; k < w; k++) {
        if (!is_na_rf(y_extended[n - w + k])) {
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

        features.push_back(var_x > 0 ? cov / var_x : NA_REAL);
      } else {
        features.push_back(NA_REAL);
      }
    }

    // Compute prediction: dot product of features and coefficients
    double pred = 0.0;
    for (size_t i = 0; i < features.size() && i < (size_t)model_coef.size(); i++) {
      if (!is_na_rf(features[i])) {
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
//' @param y_hist Numeric vector of historical target values
//' @param h Integer, forecast horizon
//' @param p Integer vector of specific lag indices
//' @param q Integer vector of MA windows
//' @param roll_windows Integer vector of rolling stat windows
//' @param roll_stats Character vector of stats to compute
//' @param trend_windows Integer vector for rolling slopes
//' @param trend_degrees Integer vector for polynomial trends
//' @param y_future Numeric vector of predicted values (updated recursively)
//' @return Numeric matrix (h x n_features) of features for prediction
[[cpp11::register]]
doubles_matrix<> build_features_recursive_cpp(
    doubles y_hist,
    int h,
    SEXP p,
    SEXP q,
    SEXP roll_windows,
    SEXP roll_stats,
    SEXP trend_windows,
    SEXP trend_degrees,
    SEXP y_future) {

  std::vector<double> y_extended(y_hist.begin(), y_hist.end());
  int history_len = y_hist.size();

  // Parse nullable parameters
  std::vector<int> p_vec, q_vec, roll_win_vec, trend_win_vec, trend_deg_vec;
  std::vector<std::string> roll_stat_vec;

  if (p != R_NilValue) {
    integers p_int = as_cpp<integers>(p);
    for (R_xlen_t i = 0; i < p_int.size(); i++) p_vec.push_back(p_int[i]);
  }
  if (q != R_NilValue) {
    integers q_int = as_cpp<integers>(q);
    for (R_xlen_t i = 0; i < q_int.size(); i++) q_vec.push_back(q_int[i]);
  }
  if (roll_windows != R_NilValue && roll_stats != R_NilValue) {
    integers rw_int = as_cpp<integers>(roll_windows);
    strings rs_str = as_cpp<strings>(roll_stats);
    for (R_xlen_t i = 0; i < rw_int.size(); i++) roll_win_vec.push_back(rw_int[i]);
    for (R_xlen_t i = 0; i < rs_str.size(); i++) roll_stat_vec.push_back(std::string(rs_str[i]));
  }
  if (trend_windows != R_NilValue) {
    integers tw_int = as_cpp<integers>(trend_windows);
    for (R_xlen_t i = 0; i < tw_int.size(); i++) trend_win_vec.push_back(tw_int[i]);
  }
  if (trend_degrees != R_NilValue) {
    integers td_int = as_cpp<integers>(trend_degrees);
    for (R_xlen_t i = 0; i < td_int.size(); i++) trend_deg_vec.push_back(td_int[i]);
  }

  // Count features
  int n_features = 0;
  n_features += p_vec.size();
  n_features += q_vec.size();
  n_features += trend_deg_vec.size();
  n_features += roll_win_vec.size() * roll_stat_vec.size();
  n_features += trend_win_vec.size();

  writable::doubles_matrix<> feature_matrix(h, n_features);

  // If predictions provided, use them for recursive updates
  std::vector<double> preds_vec;
  bool use_predictions = (y_future != R_NilValue);
  if (use_predictions) {
    doubles preds = as_cpp<doubles>(y_future);
    for (R_xlen_t i = 0; i < preds.size(); i++) preds_vec.push_back(preds[i]);
  }

  // Build features for each time step
  for (int t = 0; t < h; t++) {
    int feat_idx = 0;
    int n = y_extended.size();

    // Lags
    for (int lag : p_vec) {
      feature_matrix(t, feat_idx++) = (n >= lag) ? y_extended[n - lag] : NA_REAL;
    }

    // MAs
    for (int w : q_vec) {
      if (n >= w) {
        double sum = 0.0;
        int count = 0;
        for (int k = 0; k < w; k++) {
          if (!is_na_rf(y_extended[n - 1 - k])) {
            sum += y_extended[n - 1 - k];
            count++;
          }
        }
        feature_matrix(t, feat_idx++) = count > 0 ? sum / count : NA_REAL;
      } else {
        feature_matrix(t, feat_idx++) = NA_REAL;
      }
    }

    // Trend features
    if (!trend_deg_vec.empty()) {
      int trend_idx = history_len + t + 1;
      for (int d : trend_deg_vec) {
        feature_matrix(t, feat_idx++) = std::pow(trend_idx, d);
      }
    }

    // Rolling stats
    for (size_t i = 0; i < roll_win_vec.size(); i++) {
      int w = roll_win_vec[i];

      if (n < w) {
        for (size_t s = 0; s < roll_stat_vec.size(); s++) {
          feature_matrix(t, feat_idx++) = NA_REAL;
        }
        continue;
      }

      std::vector<double> vals;
      for (int k = 0; k < w; k++) {
        if (!is_na_rf(y_extended[n - 1 - k])) {
          vals.push_back(y_extended[n - 1 - k]);
        }
      }

      for (const std::string& stat : roll_stat_vec) {
        if (vals.empty()) {
          feature_matrix(t, feat_idx++) = NA_REAL;
        } else if (stat == "sum") {
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
            feature_matrix(t, feat_idx++) = std::sqrt(var / (vals.size() - 1));
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
      }
    }

    // Trend slopes
    for (int w : trend_win_vec) {
      if (n < w) {
        feature_matrix(t, feat_idx++) = NA_REAL;
        continue;
      }

      std::vector<double> y_vals;
      std::vector<double> x_vals;

      for (int k = 0; k < w; k++) {
        if (!is_na_rf(y_extended[n - w + k])) {
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
//'
//' @param y_hist Numeric vector of historical target values
//' @param h Integer, forecast horizon
//' @param predict_fn R function that takes a numeric matrix and returns predictions
//' @param p Integer vector of specific lag indices
//' @param q Integer vector of MA windows
//' @param roll_windows Integer vector of rolling stat windows
//' @param roll_stats Character vector of stats to compute
//' @param trend_windows Integer vector for rolling slopes
//' @param trend_degrees Integer vector for polynomial trends
//' @param batch_size Integer, number of steps to predict at once
//' @return Numeric vector of forecasts
[[cpp11::register]]
doubles forecast_iterative_cpp(
    doubles y_hist,
    int h,
    cpp11::function predict_fn,
    SEXP p,
    SEXP q,
    SEXP roll_windows,
    SEXP roll_stats,
    SEXP trend_windows,
    SEXP trend_degrees,
    int batch_size) {

  writable::doubles forecasts(h);
  std::vector<double> y_extended(y_hist.begin(), y_hist.end());
  int history_len = y_hist.size();

  // Parse nullable parameters
  std::vector<int> p_vec, q_vec, roll_win_vec, trend_win_vec, trend_deg_vec;
  std::vector<std::string> roll_stat_vec;

  if (p != R_NilValue) {
    integers p_int = as_cpp<integers>(p);
    for (R_xlen_t i = 0; i < p_int.size(); i++) p_vec.push_back(p_int[i]);
  }
  if (q != R_NilValue) {
    integers q_int = as_cpp<integers>(q);
    for (R_xlen_t i = 0; i < q_int.size(); i++) q_vec.push_back(q_int[i]);
  }
  if (roll_windows != R_NilValue && roll_stats != R_NilValue) {
    integers rw_int = as_cpp<integers>(roll_windows);
    strings rs_str = as_cpp<strings>(roll_stats);
    for (R_xlen_t i = 0; i < rw_int.size(); i++) roll_win_vec.push_back(rw_int[i]);
    for (R_xlen_t i = 0; i < rs_str.size(); i++) roll_stat_vec.push_back(std::string(rs_str[i]));
  }
  if (trend_windows != R_NilValue) {
    integers tw_int = as_cpp<integers>(trend_windows);
    for (R_xlen_t i = 0; i < tw_int.size(); i++) trend_win_vec.push_back(tw_int[i]);
  }
  if (trend_degrees != R_NilValue) {
    integers td_int = as_cpp<integers>(trend_degrees);
    for (R_xlen_t i = 0; i < td_int.size(); i++) trend_deg_vec.push_back(td_int[i]);
  }

  // Count features
  int n_features = 0;
  n_features += p_vec.size();
  n_features += q_vec.size();
  n_features += trend_deg_vec.size();
  n_features += roll_win_vec.size() * roll_stat_vec.size();
  n_features += trend_win_vec.size();

  const bool have_features = n_features > 0;

  // Determine if target history is required
  bool needs_history = !p_vec.empty() || !q_vec.empty() || !roll_win_vec.empty() || !trend_win_vec.empty();

  // Helper lambda to fill feature row
  auto fill_feature_row = [&](writable::doubles_matrix<>& mat, int row_idx, int step_idx, int n_current) {
    if (!have_features) return;
    int feat_idx = 0;

    // Lags
    for (int lag : p_vec) {
      mat(row_idx, feat_idx++) = (n_current >= lag) ? y_extended[n_current - lag] : NA_REAL;
    }

    // MAs
    for (int w : q_vec) {
      if (n_current >= w) {
        double sum = 0.0;
        int count = 0;
        for (int k = 0; k < w; k++) {
          if (!is_na_rf(y_extended[n_current - 1 - k])) {
            sum += y_extended[n_current - 1 - k];
            count++;
          }
        }
        mat(row_idx, feat_idx++) = count > 0 ? sum / count : NA_REAL;
      } else {
        mat(row_idx, feat_idx++) = NA_REAL;
      }
    }

    // Trend features
    if (!trend_deg_vec.empty()) {
      int trend_idx = history_len + step_idx + 1;
      for (int d : trend_deg_vec) {
        mat(row_idx, feat_idx++) = std::pow(trend_idx, d);
      }
    }

    // Rolling stats
    for (size_t i = 0; i < roll_win_vec.size(); i++) {
      int w = roll_win_vec[i];

      if (n_current < w) {
        for (size_t s = 0; s < roll_stat_vec.size(); s++) {
          mat(row_idx, feat_idx++) = NA_REAL;
        }
        continue;
      }

      std::vector<double> vals;
      for (int k = 0; k < w; k++) {
        if (!is_na_rf(y_extended[n_current - 1 - k])) {
          vals.push_back(y_extended[n_current - 1 - k]);
        }
      }

      for (const std::string& stat : roll_stat_vec) {
        if (vals.empty()) {
          mat(row_idx, feat_idx++) = NA_REAL;
        } else if (stat == "sum") {
          double sum = 0.0;
          for (double v : vals) sum += v;
          mat(row_idx, feat_idx++) = sum;
        } else if (stat == "sd") {
          if (vals.size() >= 2) {
            double mean = 0.0;
            for (double v : vals) mean += v;
            mean /= vals.size();
            double var = 0.0;
            for (double v : vals) var += (v - mean) * (v - mean);
            mat(row_idx, feat_idx++) = std::sqrt(var / (vals.size() - 1));
          } else {
            mat(row_idx, feat_idx++) = NA_REAL;
          }
        } else if (stat == "min") {
          double min_val = R_PosInf;
          for (double v : vals) min_val = std::min(min_val, v);
          mat(row_idx, feat_idx++) = min_val;
        } else if (stat == "max") {
          double max_val = R_NegInf;
          for (double v : vals) max_val = std::max(max_val, v);
          mat(row_idx, feat_idx++) = max_val;
        } else {
          mat(row_idx, feat_idx++) = NA_REAL;
        }
      }
    }

    // Trend slopes
    for (int w : trend_win_vec) {
      if (n_current < w) {
        mat(row_idx, feat_idx++) = NA_REAL;
        continue;
      }

      std::vector<double> y_vals;
      std::vector<double> x_vals;

      for (int k = 0; k < w; k++) {
        if (!is_na_rf(y_extended[n_current - w + k])) {
          y_vals.push_back(y_extended[n_current - w + k]);
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

        mat(row_idx, feat_idx++) = var_x > 0 ? cov / var_x : NA_REAL;
      } else {
        mat(row_idx, feat_idx++) = NA_REAL;
      }
    }
  };

  // Iterative forecasting
  if (needs_history || batch_size == 1) {
    writable::doubles_matrix<> feature_row(1, have_features ? n_features : 0);
    writable::doubles_matrix<> empty_row(1, 0);

    for (int t = 0; t < h; t++) {
      int n_current = y_extended.size();
      if (have_features) {
        fill_feature_row(feature_row, 0, t, n_current);
      }

      SEXP pred_result = have_features ? predict_fn(feature_row) : predict_fn(empty_row);
      doubles preds = as_cpp<doubles>(pred_result);
      if (preds.size() == 0) {
        cpp11::stop("Prediction function returned empty vector at step %d", t + 1);
      }

      double pred = preds[0];
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

    writable::doubles_matrix<> batch_features(current_batch, have_features ? n_features : 0);
    if (have_features) {
      int n_current = y_extended.size();
      for (int r = 0; r < current_batch; r++) {
        fill_feature_row(batch_features, r, step_idx + r, n_current);
      }
    }

    SEXP pred_result = predict_fn(batch_features);
    doubles preds = as_cpp<doubles>(pred_result);
    if ((int)preds.size() != current_batch) {
      cpp11::stop("Prediction function returned %d values but %d were expected for batch.",
                  (int)preds.size(), current_batch);
    }

    for (int r = 0; r < current_batch; r++) {
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
//' Performs recursive forecasting for multiple groups.
//'
//' @param y_matrix Numeric matrix where each row is a group's history
//' @param model_coef Numeric vector of model coefficients
//' @param has_intercept Logical, if TRUE first coef is intercept
//' @param h Integer, forecast horizon
//' @param p Integer vector of specific lag indices
//' @param q Integer vector of MA windows
//' @param roll_windows Integer vector of rolling stat windows
//' @param roll_stats Character vector of stats to compute
//' @param trend_windows Integer vector for rolling slopes
//' @param trend_degrees Integer vector for polynomial trends
//' @return Numeric matrix where each row is a group's forecast
[[cpp11::register]]
doubles_matrix<> recursive_forecast_groups_cpp(
    doubles_matrix<> y_matrix,
    doubles model_coef,
    bool has_intercept,
    int h,
    SEXP p,
    SEXP q,
    SEXP roll_windows,
    SEXP roll_stats,
    SEXP trend_windows,
    SEXP trend_degrees) {

  R_xlen_t n_groups = y_matrix.nrow();
  R_xlen_t n_cols = y_matrix.ncol();

  writable::doubles_matrix<> results(n_groups, h);

  // Process each group
  for (R_xlen_t g = 0; g < n_groups; g++) {
    // Extract row as vector
    writable::doubles y_hist(n_cols);
    for (R_xlen_t c = 0; c < n_cols; c++) {
      y_hist[c] = y_matrix(g, c);
    }

    doubles group_forecast = recursive_forecast_simple_cpp(
      y_hist, model_coef, has_intercept, h,
      p, q, roll_windows, roll_stats,
      trend_windows, trend_degrees
    );

    for (int i = 0; i < h; i++) {
      results(g, i) = group_forecast[i];
    }
  }

  return results;
}
