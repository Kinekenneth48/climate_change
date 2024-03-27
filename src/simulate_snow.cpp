#include <Rcpp.h>
using namespace Rcpp;

// Simulate GEV samples and calculate the maximum for each set
// [[Rcpp::export]]
NumericVector simulate_snow(double loc,double loctrend,double scl, double shp,
                            double pr_zero,  int n, int t) {
  
  // Extract GEV parameters from input vector using 0-based indexing
  // double loc = x[0];       // Location parameter
  // double loctrend = x[1];  // Location trend
  // double scl = x[2];       // Scale parameter
  // double shp = x[3];       // Shape parameter
  // double pr_zero = x[4];       // Shape parameter
  
  // Vector to store the max values for each set
  NumericVector ground_snow(n); 
  
  // Check if 'loc' is NA (represented as NaN in C++)
  if (NumericVector::is_na(loc)) {
    return rep(NumericVector::get_na(), n);
  } 
 
  double sL = 0;
  double current_loc = 0;
  double gev_sample = 0;
   
  for (int j = 0; j < n; ++j) {
    NumericVector gev_samples(t); // Vector to store GEV samples for the current set
    
    for (int i = 0; i < t; ++i) {
      // Generate a random number for each sample
       sL = unif_rand();
      
      // Adjust the uniform distribution to account for the proportion of zeros
      if (pr_zero > 0) {
        sL = (sL - pr_zero) / (1 - pr_zero);
      }
      
      // Compute the GEV sample
       current_loc = loc + (loctrend * (i+1)); // time varying location parameter

      if (shp != 0) { 
        // GEV distribution formula for non-zero shape parameter
        gev_sample = current_loc + scl * (pow(-1 * log(sL), -1 * shp) - 1) / shp;
      } else { 
        // Gumbel distribution (limiting case of GEV) for shape parameter close to 0
        gev_sample = current_loc - scl * log(-log(sL));
      }
      
      // Ensure all negative values are set to zero
      gev_sample = std::max(gev_sample, 0.0);
      
      // Store the sample
      gev_samples[i] = gev_sample;
      
    }
    
    // Calculate and store the maximum GEV sample for this set
    ground_snow[j] = max(gev_samples);
  }
  
  return ground_snow;
}