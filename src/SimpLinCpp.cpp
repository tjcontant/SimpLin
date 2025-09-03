#include "RcppArmadillo.h"
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
List SimpLinCpp(const arma::vec x, const arma::vec y) {
  
  double n = x.n_elem;

  arma::vec x_delta = x - arma::mean(x);
  arma::vec y_delta = y - arma::mean(y);
  
  double beta_1_hat = arma::dot(x_delta, y_delta) / arma::dot(x_delta, x_delta);
  double beta_0_hat = arma::mean(y) - beta_1_hat * arma::mean(x);
  
  arma::vec preds = beta_0_hat + x * beta_1_hat;
  arma::vec resids = preds - y;
  
  double se_beta_1 = sqrt(arma::dot(resids, resids) / arma::dot(x_delta, x_delta) / (n - 2));
  double se_beta_0 = se_beta_1 * sqrt(arma::dot(x, x) / n);
  
  double t_crit = R::qt(0.975, n - 2, 1, 0);
  
  double beta_1_lower = beta_1_hat - t_crit * se_beta_1;
  double beta_1_upper = beta_1_hat + t_crit * se_beta_1;
  
  double beta_0_lower = beta_0_hat - t_crit * se_beta_0;
  double beta_0_upper = beta_0_hat + t_crit * se_beta_0;
  
  return List::create(
    Rcpp::Named("beta_0_hat") = beta_0_hat,
    Rcpp::Named("beta_1_hat") = beta_1_hat,
    Rcpp::Named("preds") = preds,
    Rcpp::Named("resids") = resids,
    Rcpp::Named("se_beta_0") = se_beta_0,
    Rcpp::Named("se_beta_1") = se_beta_1,
    Named("ci_beta_0") = NumericVector::create(beta_0_lower, beta_0_upper),
    Named("ci_beta_1") = NumericVector::create(beta_1_lower, beta_1_upper)
  );
}

  