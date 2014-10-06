#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

// Function to perform convolution. See "?convolution" in R

// [[Rcpp::export]]
DataFrame convolution_table(NumericVector capacity, NumericVector efor, double threshold) {
  // Initialize variables
  int n = 0;
  NumericVector cap(0), prob(0);
  
  for (int i = 0; i < capacity.length(); ++i) {
    NumericVector old_cap(cap), old_prob(prob);
    
    if (i == 0) {
      // Create table for first generator
      cap = NumericVector(2, 0.0);
      prob = NumericVector(2, 0.0);
      n = 2;
      
      cap[0] = capacity[0];
      cap[1] = 0.0;
      prob[0] = 1.0 - efor[0];
      prob[1] = efor[0];
    } else {
      // Convolute i-th generator
      int x1 = 0, x2 = 0, old_n = n, maxN = 2 * n;
      double pushCap, pushProb;
      
      cap = NumericVector(maxN, 0.0);
      prob = NumericVector(maxN, 0.0);
      n = 0;

      while (x2 < old_n) {
        if (x1 == old_n) {
          pushCap = old_cap[x2];
          pushProb = old_prob[x2] * efor[i];
          ++x2;
        } else {
          double cap1 = old_cap[x1] + capacity[i];
          double cap2 = old_cap[x2];
          
          if (cap1 > cap2) {
            // Cap1 is larger
            pushCap = cap1;
            pushProb = old_prob[x1] * (1.0 - efor[i]);
            ++x1;
          } else if (cap1 < cap2) {
            // Cap2 is larger
            pushCap = cap2;
            pushProb = old_prob[x2] * efor[i];
            ++x2;
          } else {
            // Tie in cap1 and cap2
            pushCap = cap1;
            pushProb = old_prob[x1] * (1.0 - efor[i]) + old_prob[x2] * efor[i];
            ++x1;
            ++x2;
          }
        }
        
        cap[n] = pushCap;
        prob[n] = pushProb;
        ++n;
      }
    }
  }
  
  // Create LOLP vector
  NumericVector lolp(cumsum(prob));
  lolp = 1.0 - lolp  + prob;
  
  // Create vectors for data output
  NumericVector outCap = cap[(lolp >= threshold) & (prob > 0.0)],
                outProb = prob[(lolp >= threshold) & (prob > 0.0)],
                outLolp = lolp[(lolp >= threshold) & (prob > 0.0)],
                outBaseEue(outCap.size(), 0.0);
  
  // Calculate fixed term in EUE
  for (int i = 0; i < outCap.size(); ++i) {
    double eue = 0.0;
    for (int j = i + 1; j < outCap.size(); ++j)
      eue += (outCap[i] - outCap[j]) * outProb[j];
    outBaseEue[i] = eue;
  }
  
  // Create output as data frame
  return DataFrame::create(Named("Capacity")  = outCap,
                           Named("Capacity2") = outCap,
                           Named("Prob")      = outProb,
                           Named("LOLP")      = outLolp,
                           Named("BaseEUE")   = outBaseEue);
}
