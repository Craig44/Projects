## This is a debug script to get the overlap statistic to work
if (Sys.info()[["nodename"]]=="NIWA-1007004") {
  ## Work computer
  setwd("C:/Craig/Personal/Projects/Spatial Overlap statistic\\");
} else if (Sys.info()[["user"]]=="Cyrill") {
  ## Personal computer To be filled out
	setwd("D:/Projects/Spatial Overlap statistic")
}

## source utilty functions and other dependencies packages
source("utility_functions.r")
library(rstan);

####################
## Global Variables
####################
set.seed(23);
Cells_ = 11; ## Number of spatial cells


## CPUE parameters
sigma = 0.2; # std deviation of CPUE data
## Catchability
q = 1.7;
# Our Observations from CPUE fishery
mu = c(20.085537, 400,    64.56,   32.3526,  148.413159, 403.428793,   24.3452,   59.3425 ,  20.085537,   54.598150, 403.428793) * 100 / q;

obs = Rlnorm(mu = mu, sigma = sigma) / 1.7; ## 100 is a scaler for abundance


Data = list("S" = Cells_, "Catch_rate" = obs, "Sigma" = sigma);


## load model
mdl <- stan_model(file = 'LogNormal.rstan');

## Initialise values
fit_init <- function() { list(q = q, Absolute_Abundance_hat = mu) }
# MPD Fit
fit <- optimizing(mdl, data = Data, init = fit_init)

