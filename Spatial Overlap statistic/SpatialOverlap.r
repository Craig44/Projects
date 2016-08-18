## SpatialOverlap.r
## @author C. Marsh
## @date 18/08/2016
## @description
##
##	A script for running and estimating the spatial Tag Overlap Statistic (TSO) 
##	reference (). Basically this method estimates the biomass of population over 
##	a spatial grid at a single point in time. The biomass in each cell is unknown so we estimate all parameters 
##	By fitting to data in a Bayesian framework. Observations used to inform our unknown 
##	biomass is relative abundance (Commercial catch rate data) and mark recapture observations.
## 	The overlap statistics are then summaries of how this biomass is distributed spatially at a 
##	point in time, and which aspect is informed my mark recapture observations for diagnosing 
## 	research plans.


## Things that need resolving/Achieved
#	- Data weighting/conflicts
#	- incorporate a Time Series
# 	- Playing with RStan =)


## Utility Functions

## @Rlnorm: Simulate data from an expected state, with a given biomass
## 
## @param mu: A vector of means (in log space) of each expected biomass
## @param sigma: A vector of std deviations (in log space) of each observation
## @param n: 

Rlnorm = function(n = 1, mu, sigma) {
  x = rlnorm(n = length(mu)*n, meanlog = log(mu), sdlog = sigma);
  x
}


## Global Variables
Cells_ = 11; ## Number of spatial cells
set.seed(23);

#|-------------------------
#|B_1 |B_2 |B_3 |B_4 |B_5 |
#|----|----|----|----|----|
#|B_6 |B_7 |B_8 |B_9 |B_10|
#|-------------------------


## Partition
labels = paste("B_" , 1:Cells_, sep = "");
Partition_ = matrix(0,nrow = 1, ncol = Cells_);
colnames(Partition_) = labels

## Parameters


## Partition


#################
## Observations
#################
## CPUE parameters
sigma = 0.2; # std deviation of CPUE data
# Our Observations from CPUE fishery
obs = Rlnorm(mu = c(20.085537, 400,    64.56,   32.3526,  148.413159, 403.428793,   24.3452,   59.3425 ,  20.085537,   54.598150, 403.428793), sigma = sigma);
## Catchability
q = 1.2
## Mark recapture parameters
## Lets generate data that wont conflict wiht the CPUE to begin


## Commercial Catch Rates (CPUE) Well observed fishery;

## Simulate data from an expected state, with a given error
## @param mu: A vector of means (in log space) of each expected biomass
## @param sigma: A vector of std deviations (in log space) of each observation
## @param n: 

Rlnorm = function(n = 1, mu, sigma) {
  x = rlnorm(n = length(mu)*n, meanlog = log(mu), sdlog = sigma);
  x;
}
scanned = c(1000, 2340, 633, 1200,1350, 324, 146,456,1364,1248, 213);
recaptures = c(20, 15, 6, 12, 0, 2,8,2,0,3, 4);

prob = recaptures/scanned
rel = (obs * q) / prob; 

## Mark-Recapture likelihood
## Each recapture and scanned individual in size bin follow a binomial random variable



## A vector of scanned individuals 
Scanned = c()
recaptured = 

## Age-Length Relationship

## Length-Weight Relationship