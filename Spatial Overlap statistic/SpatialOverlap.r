## SpatialOverlap.r
## @author C. Marsh
## @date 18/08/2016
## @description
##
##  A script for running and estimating the spatial Tag Overlap Statistic (TSO) 
##  reference (). Basically this method estimates the biomass of population over 
##  a spatial grid at a single point in time. The biomass in each cell is unknown so we estimate all parameters 
##  By fitting to data in a Bayesian framework. Observations used to inform our unknown 
##  biomass is relative abundance (Commercial catch rate data) and mark recapture observations.
##  The overlap statistics are then summaries of how this biomass is distributed spatially at a 
##  point in time, and which aspect is informed my mark recapture observations for diagnosing 
##  research plans.


## Things that need resolving/Achieved
# - Data weighting/conflicts
# - incorporate a Time Series
# - Playing with RStan =) ~ q as a random effect (Make sure you have a good reason why we are trying this)

####################
## Utility Functions
####################
## @Rlnorm: Simulate data from an expected state, with a given biomass
## 
## @param mu: A vector of means (in log space) of each expected biomass
## @param sigma: A vector of std deviations (in log space) of each observation
## @param n: 

Rlnorm = function(n = 1, mu, sigma) {
  x = rlnorm(n = length(mu)*n, meanlog = log(mu), sdlog = sigma);
  x
}

####################
## Global Variables
####################
set.seed(23);
Cells_ = 11; ## Number of spatial cells

####################
## Observations
####################
## CPUE parameters
sigma = 0.2; # std deviation of CPUE data
## Catchability
q = 1.7;
# Our Observations from CPUE fishery
mu = c(20.085537, 400,    64.56,   32.3526,  148.413159, 403.428793,   24.3452,   59.3425 ,  20.085537,   54.598150, 403.428793) * 100 / q;

obs = Rlnorm(mu = mu, sigma = sigma); ## 100 is a scaler for abundance


## Mark-Recapture likelihood
## Each recapture and scanned individual in size bin follow a binomial random variable
## Peterson estimate = (released * scanned) / recaptures
## A vector of scanned individuals 
scanned = c(587, 3000, 633, 120,60, 324, 146,456,364,548, 413);
## Imaginary recaptures
recaptures = c(4, 8, 6, 12, 0, 2,8,2,0,5, 6);

prob = recaptures/scanned;
rel = ((obs * q) * recaptures) / scanned;

## Generate a random sample from a binomial where n = scanned, p = recap / scanned
(rel * scanned) / recaptures;
Recaptures = rbinom(length(scanned), size = scanned, prob = prob);
(rel * scanned) / Recaptures;
  
ndx = rel == 0;
rel[ndx] = 3112;

#|-------------------------
#|B_1 |B_2 |B_3 |B_4 |B_5 |
#|----|----|----|----|----|------|
#|B_6 |B_7 |B_8 |B_9 |B_10| B_11 |
#|-------------------------------|


library(rstan);
#rstan_options(auto_write = TRUE);
#if (parallel::detectCores() > 2)
#  cores = 2
#options(mc.cores = cores);
## Set wd
if (Sys.info()[["nodename"]]=="NIWA-1007004") {
  ## Work computer
  setwd("C:/Craig/Personal/Projects/Spatial Overlap statistic\\");
} else if (Sys.info()[["user"]]=="Cyrill") {
  ## Personal computer To be filled out
  setwd("C:\\Craig\\projects\\2016\\ANT1601-01 (SpatialOverlap)\\");
}


####################
## load model
####################
#source("SpatialOverlap.rstan")
#model = stanc(file = "SpatialOverlap.rstan", model_name = "Single_time_step");
  
####################
## Load data 
####################
Data = list("S" = Cells_, "Scanned" = scanned, "Recaptured" = Recaptures, "Tagged" = rel, "Catch_rate" = obs, "Sigma" = sigma);


mdl <- stan_model(file = 'SpatialOverlap.rstan');

## Initialise values
fit_init <- function() { list(q = q, Absolute_Abundance_hat = mu) }
# MPD Fit
fit <- optimizing(mdl, data = Data, init = fit_init)






