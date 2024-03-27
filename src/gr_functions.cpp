#include <Rcpp.h>
using namespace Rcpp;

// gr_co
//============================================================================
// Replication of the Colorado method for simulating GR based on the ground
// snow load. The "*20.8855" makes the necessary conversion from kPa to psf.
//============================================================================
// [[Rcpp::export]]
NumericVector gr_co(NumericVector sL, double cap) {
  // Determine length of input vector.
  int n = sL.length();
  
  // 20.8855 includes conversion from kPa to psf.
  NumericVector gr_mean = log(0.5*exp(-0.034*sL*20.8855) + 0.4);
  NumericVector gr_sd =  0.007*sL*20.8855 + 0.1 - 0.33;
  
  // Way to recover minimum value without calling min function. (For speed)
  gr_sd = -((Rcpp::abs(gr_sd) - gr_sd) / 2) + 0.33;
  
  // Simulate from gr curve
  NumericVector gr = exp(rnorm(n)*gr_sd + gr_mean) - cap;
  
  // Cap simulated values.
  gr = -((Rcpp::abs(gr) - gr) / 2) + cap;
  
  return gr;
}


// gr_or
//============================================================================
// Function to compute ground snow load using the Orourke model.
// The mean is specific to a thermal and exposure factor, and the error
// is multiplicative.
//============================================================================
// [[Rcpp::export]]
NumericVector gr_or(NumericVector sL, double intercept, double mse, double cap) {
  
  // Simulate from lognormal distribution.
  NumericVector GR = rlnorm(sL.length(), 0, mse);
  
  GR = GR * intercept;
  
  GR[GR > cap] = cap;
  
  return GR;
}


// gr_lam_log
//============================================================================
// Function to compute ground snow load using generic models with Tukey
// transformations.
// For flat line model, simply use slope = 0.
// Lam is the required power for a back-transformation. Thus a square
// root normal distribution should use a back transformation of lam = 2.
// Assumes sL is given in metric, but coefficients are for english.
//============================================================================
// [[Rcpp::export]]
NumericVector gr_lam_log(NumericVector sL, double slope, double intercept,
                         double mse, double cap, double lam, double flat_line) {
  
  // Simulate from normal distribution according to sd.
  NumericVector tgr = rnorm(sL.length(), 0, mse);
  
  // Limit sL
  sL[sL > flat_line/20.8855] = flat_line/20.8855;
  sL[sL < 0.0001] = 0.0001;
  
  // Convert kPa to psf and then estimate sqrt(GR) + error
  tgr = tgr + (intercept + slope*log(sL*20.8855));
  
  NumericVector GR;
  
  if (lam == 0) {
    GR = exp(tgr);
  } else if (lam < 0) {
    GR = log(log(tgr));
  } else {
    GR = tgr;
    GR[GR < 0] = 0;
    GR = pow(GR, lam);
  }
  
  GR[GR > cap] = cap;
  
  return GR;
}

//
// // GR_CO_50 //
// //============================================================================
// // Replication of the Colorado method for simulating GR based on the ground
// // snow load. The "*20.8855" makes the necessary conversion from kPa to psf.
// //============================================================================
// // [[Rcpp::export]]
// NumericVector gr_co_50(NumericVector sL) {
//   // Determine length of input vector.
//   int n = sL.length();
//
//   // 20.8855 includes conversion from kPa to psf.
//   NumericVector gr_mean = log(0.5*exp(-0.034*sL*20.8855) + 0.4);
//   NumericVector gr_sd =  0.007*sL*20.8855 + 0.1 - 0.33;
//
//   // Way to recover minimum value without calling min function. (For speed)
//   gr_sd = -((Rcpp::abs(gr_sd) - gr_sd) / 2) + 0.33;
//
//   // Simulate from gr curve
//   NumericVector temp = pow(runif(n), .02);
//   NumericVector gr = exp(qnorm(temp)*gr_sd + gr_mean) - 1.25;
//
//   // Cap simulated values at 1.25.
//   gr = -((Rcpp::abs(gr) - gr) / 2) + 1.25;
//
//   return gr;
// }
//
// // GR_Norway //
// //============================================================================
// // Fit GR using a conditional probability distribution using only flat roof
// // data from Norway. Assumes sL is given in kilopascals and makes the required
// // conversions for psf and kg/m^2.
// //============================================================================
// // [[Rcpp::export]]
// NumericVector gr_no(NumericVector sL, double slope = -0.1267,
//                     double intercept = 1.2836, double mse = 0.1504) {
//   // Determine length of input vector.
//   int n = sL.length();
//
//   // Simulate from normal distribution according to MSE.
//   NumericVector tgr = rnorm(n)*mse;
//
//   // Cap sL predictions at 60psf
//   NumericVector sL_new = clone(sL);
//
//   sL_new[sL_new > 60*0.04788] = 60*0.04788;
//
//   // Add arbitrary amount to avoid taking the log of zero.
//   sL_new[sL_new == 0] = 0.01;
//
//   // Adjust the mean based on the linear model prediction.
//   tgr = tgr + (intercept + sL_new*slope*log(sL_new*101.9716));
//
//   // Cap predictions at one.
//   tgr[tgr > 1] = 1;
//
//   return tgr;
// }
//
// // [[Rcpp::export]]
// NumericVector gr_el(NumericVector sL) {
//   // Determine length of input vector.
//   int n = sL.length();
//
//   // Simulate from normal distribution according to SD.
//   NumericVector tgr = rnorm(n, 0.5, .23*0.5);
//
//   return tgr;
// }
//
// // Function used to simulate a truncated normal distribution
// // [[Rcpp::export]]
// NumericVector rnorm_trunc(int n, double mean, double sd,
//                           double trunc_low, double trunc_high){
//
//   // Simulate from truncated uniform
//   NumericVector temp = runif(n, trunc_low, trunc_high);
//
//   // Map to normal distribution
//   return qnorm(temp, mean, sd);
// }
//
// // GR_m1 //
// //============================================================================
// // "Flat Line" model based on the mean of sqrt(GR) from US and Canadian.
// // locations. The "*20.8855" makes the necessary conversion from kPa to psf.
// // This model assumes that sqrt(GR) is approximately normally distributed,
// // and therefore the inputted mean and sd should be given in terms of sqrt(GR)
// //============================================================================
// // [[Rcpp::export]]
// NumericVector gr_m1(NumericVector sL, double mean=0.7382258,
//                     double sd=0.1966102, double alpha=0.05) {
//   // Determine length of input vector.
//   int n = sL.length();
//   double trunc_low = alpha / 2;
//   double trunc_high = 1 - (alpha / 2);
//
//   // Simulate from normal distribution according to sd.
//   NumericVector tgr = rnorm_trunc(n, mean,
//                                   sd, trunc_low,
//                                   trunc_high);
//
//   // NumericVector gr_mean = rep(mean, n);
//
//   // tgr = (pow(tgr + gr_mean, 2));
//   // convert from sqrt scale to regular scale
//   tgr = pow(tgr, 2);
//   return tgr;
// }
//
// // GR_m2 //
// //============================================================================
// // Regression model based on the mean of sqrt(GR) from US and Canadian
// // locations. The "*20.8855" makes the necessary conversion from kPa to psf.
// //============================================================================
// // [[Rcpp::export]]
// NumericVector gr_m2(NumericVector sL, double slope=-0.053,
//                     double intercept=0.903,
//                     double rmse=0.194, double alpha=0.05) {
//   // Determine length of input vector.
//   int n = sL.length();
//   double trunc_low = alpha / 2;
//   double trunc_high = 1 - (alpha / 2);
//
//   // Simulate from normal distribution according to sd.
//   NumericVector tgr = rnorm_trunc(n, 0,
//                                   rmse, trunc_low,
//                                   trunc_high);
//
//   // Clone Snow Load vector
//   NumericVector sL_new = clone(sL);
//
//   sL_new[sL_new > 60/20.8855] = 60/20.8855;
//   sL_new[sL_new < 1/20.8855] = 1/20.8855;
//
//   // Convert kPa to psf and then estimate sqrt(GR) + error
//   tgr = tgr + (intercept + slope*log(sL_new*20.8855));
//
//   // Un-transform results
//   tgr = pow(tgr, 2);
//
//   return tgr;
// }
//
// // GR_bean1 //
// //============================================================================
// // Regression model based on the mean of sqrt(GR) from US and Canadian
// // locations. The "*20.8855" makes the necessary conversion from kPa to psf.
// //============================================================================
// // [[Rcpp::export]]
// NumericVector gr_bean1(NumericVector sL, double slope=-0.005499,
//                        double intercept=0.718612,
//                        double rmse=0.1985, double alpha=0.01) {
//   // Determine length of input vector.
//   int n = sL.length();
//   double trunc_low = alpha / 2;
//   double trunc_high = 1 - (alpha / 2);
//
//   // Simulate from normal distribution according to sd.
//   NumericVector tgr = rnorm_trunc(n, 0,
//                                   rmse, trunc_low,
//                                   trunc_high);
//
//   // Clone Snow Load vector
//   NumericVector sL_new = clone(sL);
//
//   sL_new[sL_new > 50/20.8855] = 50/20.8855;
//
//   // Convert kPa to psf and then estimate sqrt(GR) + error
//   tgr = tgr + (intercept + slope*sL_new*20.8855);
//
//   return tgr;
// }
//
// // GR_bean2 //
// //============================================================================
// // "Flat Line" model based on the mean of sqrt(GR) from US and Canadian.
// // locations. The "*20.8855" makes the necessary conversion from kPa to psf.
// // This model assumes that sqrt(GR) is approximately normally distributed,
// // and therefore the inputted mean and sd should be given in terms of sqrt(GR)
// //============================================================================
// // [[Rcpp::export]]
// NumericVector gr_bean2(NumericVector sL, double mean=0.6141302,
//                        double sd=0.2140051, double alpha=0.01) {
//   // Determine length of input vector.
//   int n = sL.length();
//   double trunc_low = alpha / 2;
//   double trunc_high = 1 - (alpha / 2);
//
//   // Simulate from normal distribution according to sd.
//   NumericVector tgr = rnorm_trunc(n, mean,
//                                   sd, trunc_low,
//                                   trunc_high);
//
//   return tgr;
// }
//
// // [[Rcpp::export]]
// NumericVector gr_pr(NumericVector sL,
//                     double alpha=0.05) {
//   // Determine length of input vector.
//   int n = sL.length();
//   double trunc_low = alpha / 2;
//   double trunc_high = 1 - (alpha / 2);
//
//   // Simulate from normal distribution according to SD.
//   NumericVector tgr = rnorm_trunc(n, 0.8656, 0.265,
//                                   trunc_low, trunc_high);
//
//   tgr = pow(tgr, 2) * 0.7;
//
//   return tgr;
// }
//
//
// // [[Rcpp::export]]
// NumericVector gr_pr1(NumericVector sL,
//                      double alpha=0.05) {
//   // Determine length of input vector.
//   int n = sL.length();
//   double trunc_low = alpha / 2;
//   double trunc_high = 1 - (alpha / 2);
//
//   // Simulate from normal distribution according to SD.
//   NumericVector tgr = rnorm_trunc(n, 0.7133, 0.3578,
//                                   trunc_low, trunc_high);
//
//   tgr = tgr * 0.7;
//
//   return tgr;
// }
//
// /////////////////
//
// // [[Rcpp::export]]
// NumericVector gr_pr2(NumericVector sL, double slope=-0.005499,
//                      double intercept=0.718612,
//                      double rmse=0.1985) {
//   // Determine length of input vector.
//   int n = sL.length();
//
//   // Simulate from normal distribution according to sd.
//   NumericVector tgr = rnorm(n, 0, rmse);
//
//   // Clone Snow Load vector
//   NumericVector sL_new = clone(sL);
//
//   sL_new[sL_new > 50/20.8855] = 50/20.8855;
//
//   // Convert kPa to psf and then estimate sqrt(GR) + error
//   tgr = tgr + (intercept + slope*sL_new*20.8855);
//
//   // Simulate from normal distribution according to SD.
//   NumericVector PF = rnorm(n, 0.7133, 0.3578);
//
//   tgr = tgr * PF;
//
//   return tgr;
// }
//
//
// // [[Rcpp::export]]
// NumericVector gr_exp(NumericVector sL, double mean=1.754094,
//                      double sd=0.4204623, double trunc_low=0.0,
//                      double trunc_high=1.0) {
//   // Determine length of input vector.
//   int n = sL.length();
//
//   // Simulate from normal distribution according to SD.
//   NumericVector tgr = rnorm(n, mean, sd);
//
//
//   tgr[tgr < 1] = 1;
//   tgr = log(tgr);
//
//   return tgr;
// }
//
//
// // [[Rcpp::export]]
// NumericVector gr_exp1(NumericVector sL, double mean=1.754094,
//                      double sd=0.4204623) {
//   // Determine length of input vector.
//   int n = sL.length();
//
//   // Simulate from normal distribution according to SD.
//   NumericVector tgr = rnorm(n, mean, sd);
//
//   tgr[tgr < 1] = 1;
//   tgr = log(tgr);
//   tgr[tgr > 1] = 1;
//
//   return tgr;
// }
//
// // [[Rcpp::export]]
// NumericVector gr_mod1(NumericVector sL, double mean=1.754094,
//                       double sd=0.4204623) {
//   // Determine length of input vector.
//   int n = sL.length();
//
//   // Simulate from normal distribution according to SD.
//   NumericVector tgr = rnorm(n, mean, sd);
//
//   tgr[tgr < 1] = 1;
//   tgr = log(tgr);
//   tgr[tgr > 1] = 1;
//
//   return tgr;
// }
//
//
// // [[Rcpp::export]]
// NumericVector gr_model1(NumericVector sL, double mean=1.741718,
//                         double sd=0.4280139, bool upper_trunc=false) {
//   // Determine length of input vector.
//   int n = sL.length();
//
//   // Simulate from normal distribution according to sd.
//   NumericVector tgr = rnorm(n, mean, sd);
//
//   // NumericVector gr_mean = rep(mean, n);
//
//   // tgr = (pow(tgr + gr_mean, 2));
//   // convert from sqrt scale to regular scale
//   tgr[tgr < 1] = 1;
//   tgr = log(tgr);
//   if (upper_trunc) {
//     tgr[tgr > 1] = 1;
//   }
//   return tgr;
// }
//
// // [[Rcpp::export]]
// NumericVector gr_model2(NumericVector sL, double slope=-0.05318846,
//                         double intercept=0.8943676,
//                         double rmse=0.2071542, double alpha=0.05) {
//   // Determine length of input vector.
//   int n = sL.length();
//   double trunc_low = alpha / 2;
//   double trunc_high = 1 - (alpha / 2);
//
//   // Simulate from normal distribution according to sd.
//   NumericVector tgr = rnorm_trunc(n, 0,
//                                   rmse, trunc_low,
//                                   trunc_high);
//
//   // Clone Snow Load vector
//   NumericVector sL_new = clone(sL);
//
//   sL_new[sL_new > 60/20.8855] = 60/20.8855;
//   sL_new[sL_new < 1/20.8855] = 1/20.8855;
//
//   // Convert kPa to psf and then estimate sqrt(GR) + error
//   tgr = tgr + (intercept + slope*log(sL_new*20.8855));
//
//   tgr[tgr < 0] = 0;
//   // Un-transform results
//   tgr = pow(tgr, 2);
//
//   return tgr;
// }
//
// // [[Rcpp::export]]
// NumericVector gr_model3(NumericVector sL, double slope=-0.004380777,
//                         double intercept=1.900271,
//                         double rmse=0.4253942) {
//   // Determine length of input vector.
//   int n = sL.length();
//
//   // Simulate from normal distribution according to sd.
//   NumericVector tgr = rnorm(n, 0, rmse);
//
//   // Clone Snow Load vector
//   NumericVector sL_new = clone(sL);
//
//   sL_new[sL_new > 60/20.8855] = 60/20.8855;
//
//   // Convert kPa to psf and then estimate sqrt(GR) + error
//   tgr = tgr + (intercept + slope*sL_new*20.8855);
//
//   tgr[tgr < 1] = 1;
//
//   // Un-transform results
//   tgr = log(tgr);
//
//   return tgr;
// }
//
// // [[Rcpp::export]]
// NumericVector gr_model4(NumericVector sL, double slope=-1.525088,
//                         double intercept=12.50462,
//                         double rmse=2.059595) {
//   // Determine length of input vector.
//   int n = sL.length();
//
//   // Simulate from normal distribution according to sd.
//   NumericVector tgr = rnorm(n, 0, rmse);
//
//   // Clone Snow Load vector
//   NumericVector sL_new = clone(sL);
//
//   sL_new[sL_new > 60/20.8855] = 60/20.8855;
//   sL_new[sL_new < 1/20.8855] = 1/20.8855;
//
//   // Convert kPa to psf and then estimate sqrt(GR) + error
//   tgr = tgr + (intercept + slope*log(sL_new*20.8855));
//
//   tgr[tgr < exp(1)] = exp(1);
//   // Un-transform results
//   tgr = log(log(tgr));
//
//   return tgr;
// }
//
//
// // [[Rcpp::export]]
// NumericVector gr_US1(NumericVector sL, double mean=0.603904,
//                         double sd=0.2311876, bool upper_trunc=false) {
//   // Determine length of input vector.
//   int n = sL.length();
//
//   // Simulate from normal distribution according to sd.
//   NumericVector tgr = rnorm(n, mean, sd);
//
//   // NumericVector gr_mean = rep(mean, n);
//
//   // tgr = (pow(tgr + gr_mean, 2));
//   // convert from sqrt scale to regular scale
//   tgr[tgr < 0] = 0;
//   tgr = pow(tgr, 2);
//   if (upper_trunc) {
//     tgr[tgr > 1] = 1;
//   }
//   return tgr;
// }
//
// // [[Rcpp::export]]
// NumericVector gr_US2(NumericVector sL, double slope=-0.08289,
//                         double intercept=0.92778,
//                         double rmse=0.2346, double alpha=0.05) {
//   // Determine length of input vector.
//   int n = sL.length();
//   double trunc_low = alpha / 2;
//   double trunc_high = 1 - (alpha / 2);
//
//   // Simulate from normal distribution according to sd.
//   NumericVector tgr = rnorm_trunc(n, 0,
//                                   rmse, trunc_low,
//                                   trunc_high);
//
//   // Clone Snow Load vector
//   NumericVector sL_new = clone(sL);
//
//   sL_new[sL_new > 60/20.8855] = 60/20.8855;
//   sL_new[sL_new < 1/20.8855] = 1/20.8855;
//
//   // Convert kPa to psf and then estimate sqrt(GR) + error
//   tgr = tgr + (intercept + slope*log(sL_new*20.8855));
//
//   tgr[tgr < 0] = 0;
//   // Un-transform results
//   tgr = pow(tgr, 2);
//
//   return tgr;
// }
//
//
// // [[Rcpp::export]]
// NumericVector gr_US3(NumericVector sL, double slope=-0.0016618,
//                         double intercept=0.7237756,
//                         double rmse=0.2389) {
//   // Determine length of input vector.
//   int n = sL.length();
//
//   // Simulate from normal distribution according to sd.
//   NumericVector tgr = rnorm(n, 0, rmse);
//
//   // Clone Snow Load vector
//   NumericVector sL_new = clone(sL);
//
//   sL_new[sL_new > 60/20.8855] = 60/20.8855;
//
//   // Convert kPa to psf and then estimate sqrt(GR) + error
//   tgr = tgr + (intercept + slope*sL_new*20.8855);
//
//   tgr[tgr < 0] = 0;
//
//   // Un-transform results
//   tgr = pow(tgr, 2);
//
//   return tgr;
// }
//
//
// // [[Rcpp::export]]
// NumericVector gr_US_pw(NumericVector sL, double slope=-0.008043,
//                        double intercept=0.656515,
//                        double rmse=0.2673) {
//   // Determine length of input vector.
//   int n = sL.length();
//
//   // Simulate from normal distribution according to sd.
//   NumericVector tgr = rnorm(n, 0, rmse);
//
//   // Clone Snow Load vector
//   NumericVector sL_new = clone(sL);
//
//   sL_new[sL_new > 30/20.8855] = 30/20.8855;
//
//   // Convert kPa to psf and then estimate sqrt(GR) + error
//   tgr = tgr + (intercept + slope*sL_new*20.8855);
//
//   tgr[tgr < 0] = 0;
//
//   return tgr;
// }
//
// // [[Rcpp::export]]
// NumericVector gr_log(NumericVector sL, double mean=0,
//                      double sd=0.42) {
//   // Determine length of input vector.
//   int n = sL.length();
//
//   // Simulate from lognormal distribution.
//   NumericVector tgr = rlnorm(n, 0, sd);
//
//   tgr = tgr * 0.47;
//
//   return tgr;
// }
//
// // [[Rcpp::export]]
// NumericVector gr_FlatAll(NumericVector sL, double mean=0.5752898,
//                          double sd=0.2499876,
//                          double upper_trunc=1.158) {
//   // Determine length of input vector.
//   int n = sL.length();
//
//   // Simulate from normal distribution according to sd.
//   NumericVector tgr = rnorm(n, mean, sd);
//
//   // NumericVector gr_mean = rep(mean, n);
//
//   // tgr = (pow(tgr + gr_mean, 2));
//   // convert from sqrt scale to regular scale
//   tgr[tgr < 0] = 0;
//   tgr[tgr > upper_trunc] = upper_trunc;
//   return tgr;
// }
//
// // [[Rcpp::export]]
// NumericVector gr_SlopeAll(NumericVector sL, double slope=-0.09111,
//                           double intercept=0.85916,
//                           double rmse=0.2432,
//                           double upper_trunc=1.158) {
//   // Determine length of input vector.
//   int n = sL.length();
//
//   // Simulate from normal distribution according to sd.
//   NumericVector tgr = rnorm(n, 0, rmse);
//
//   // Clone Snow Load vector
//   NumericVector sL_new = clone(sL);
//
//   sL_new[sL_new > 60/20.8855] = 60/20.8855;
//   sL_new[sL_new < 1/20.8855] = 1/20.8855;
//
//   // Convert kPa to psf and then estimate sqrt(GR) + error
//   tgr = tgr + (intercept + slope*log(sL_new*20.8855));
//
//   tgr[tgr < 0] = 0;
//   tgr[tgr > upper_trunc] = upper_trunc;
//
//   return tgr;
// }
//
// // [[Rcpp::export]]
// NumericVector gr_FlatUSCA(NumericVector sL, double mean=0.6912256,
//                          double sd=0.2035603,
//                          double upper_trunc=1.158) {
//   // Determine length of input vector.
//   int n = sL.length();
//
//   // Simulate from normal distribution according to sd.
//   NumericVector tgr = rnorm(n, mean, sd);
//
//   // NumericVector gr_mean = rep(mean, n);
//
//   // tgr = (pow(tgr + gr_mean, 2));
//   // convert from sqrt scale to regular scale
//   tgr[tgr < 0] = 0;
//   tgr = pow(tgr, 2);
//   tgr[tgr > upper_trunc] = upper_trunc;
//   return tgr;
// }
//
// // [[Rcpp::export]]
// NumericVector gr_SlopeUSCA(NumericVector sL, double slope=-0.09661,
//                           double intercept=0.97951,
//                           double rmse=0.1919,
//                           double upper_trunc=1.158) {
//   // Determine length of input vector.
//   int n = sL.length();
//
//   // Simulate from normal distribution according to sd.
//   NumericVector tgr = rnorm(n, 0, rmse);
//
//   // Clone Snow Load vector
//   NumericVector sL_new = clone(sL);
//
//   sL_new[sL_new > 60/20.8855] = 60/20.8855;
//   sL_new[sL_new < 1/20.8855] = 1/20.8855;
//
//   // Convert kPa to psf and then estimate sqrt(GR) + error
//   tgr = tgr + (intercept + slope*log(sL_new*20.8855));
//
//   tgr[tgr < 0] = 0;
//   tgr = pow(tgr, 2);
//   tgr[tgr > upper_trunc] = upper_trunc;
//
//   return tgr;
// }
