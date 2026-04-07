#include <Rcpp.h>
using namespace Rcpp;

//' Rolling Annualized Realized Volatility (C++)
//'
//' Computes rolling standard deviation of log returns, annualized by sqrt(252).
//' Much faster than pure R for large vectors (e.g., 5000+ daily observations).
//'
//' @param x     Numeric vector of log returns (pre-computed)
//' @param window Integer rolling window length in trading days
//' @return Numeric vector of same length as x; first (window-1) values are NA
//' @export
// [[Rcpp::export]]
NumericVector rolling_vol_cpp(NumericVector x, int window) {
  int n = x.size();
  NumericVector result(n, NA_REAL);

  if (window < 2 || window > n) return result;

  for (int i = window - 1; i < n; i++) {
    // Welford's online algorithm for numerical stability https://www.reddit.com/r/PLC/comments/1m9dig8/welfords_online_algorithm/ probs could have used something simpler? Ai reccomended this one for quick
    double mean_val = 0.0;
    double M2       = 0.0;

    for (int j = i - window + 1; j <= i; j++) {
      if (NumericVector::is_na(x[j])) { mean_val = NA_REAL; break; }
      double delta  = x[j] - mean_val;
      mean_val     += delta / (j - (i - window + 1) + 1);
      double delta2 = x[j] - mean_val;
      M2           += delta * delta2;
    }

    if (!NumericVector::is_na(mean_val) && window > 1) {
      double var    = M2 / (window - 1);   // sample variance
      result[i]     = std::sqrt(var * 252.0); // annualize
    }
  }
  return result;
}

//' Rolling Pairwise Beta (OLS hedge ratio) in C++
//'
//' For each window, computes beta = cov(x, y) / var(y).
//' Used for hedge ratio estimation across the term structure.
//'
//' @param x      Numeric vector (instrument to hedge)
//' @param y      Numeric vector (hedging instrument)
//' @param window Integer rolling window
//' @return Numeric vector of rolling betas
//' @export
// [[Rcpp::export]]
NumericVector rolling_beta_cpp(NumericVector x, NumericVector y, int window) {
  int n = x.size();
  NumericVector result(n, NA_REAL);

  if (window < 4 || window > n) return result;

  for (int i = window - 1; i < n; i++) {
    double sum_x = 0, sum_y = 0, sum_xy = 0, sum_yy = 0;
    bool   has_na = false;
    int    cnt    = 0;

    for (int j = i - window + 1; j <= i; j++) {
      if (NumericVector::is_na(x[j]) || NumericVector::is_na(y[j])) {
        has_na = true; break;
      }
      sum_x  += x[j];
      sum_y  += y[j];
      sum_xy += x[j] * y[j];
      sum_yy += y[j] * y[j];
      cnt++;
    }

    if (!has_na && cnt > 1) {
      double cov_xy = (sum_xy - sum_x * sum_y / cnt) / (cnt - 1);
      double var_y  = (sum_yy - sum_y * sum_y / cnt) / (cnt - 1);
      if (std::abs(var_y) > 1e-12) result[i] = cov_xy / var_y;
    }
  }
  return result;
}
