// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"

using namespace Rcpp;
using namespace arma;

//[[Rcpp::export]]

List SimpLinCpp(arma::colvec Y, arma::colvec X){
  
  int n = X.size();
  arma::colvec intercept(n, fill::ones);
  mat newX = join_horiz(intercept, X);
  mat Coef = inv(newX.t() * newX) * newX.t() * Y;
  arma::colvec Predicted = newX * Coef;
  arma::colvec Residuals = Y - newX * Coef;
  double sigma2 = accu(pow(Residuals, 2)) / (n - 1);
  arma::colvec StdErr = pow(diagvec(sigma2 * inv(newX.t() * newX)), 0.5);
  double critval = R::qt(0.975, n - 2, 1, 0);
  double lower_int = Coef(0, 0) - critval * StdErr(0);
  double upper_int = Coef(0, 0) + critval * StdErr(0);
  double lower_slope = Coef(1, 0) - critval * StdErr(1);
  double upper_slope = Coef(1, 0) + critval * StdErr(1);
  arma::colvec CI_int = {lower_int, upper_int};
  arma::colvec CI_slope = {lower_slope, upper_slope};
  
  return Rcpp::List::create(Rcpp::Named("Coefficients") = Coef,
                            Rcpp::Named("Predicted") = Predicted,
                            Rcpp::Named("Residuals") = Residuals,
                            Rcpp::Named("Standard_Errors") = StdErr,
                            Rcpp::Named("CI_Intercept") = CI_int,
                            Rcpp::Named("CI_Slope") = CI_slope);
  
}

