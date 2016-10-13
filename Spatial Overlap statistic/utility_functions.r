## This is a script full of utilty functions used in the model

## @param mu: A vector of means (in log space) of each expected biomass
## @param sigma: A vector of std deviations (in log space) of each observation
## @param n: 
Rlnorm = function(n = 1, mu, sigma) {
  x = rlnorm(n = length(mu)*n, meanlog = log(mu), sdlog = sigma);
  x
}