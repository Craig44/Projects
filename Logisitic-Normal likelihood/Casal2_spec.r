## Casal2_spec.r
## @Author C Marsh
## @Date 8/10/2016
## @Description
## This script is setting up the logistic normal likelihood for composition data

## Add Libraries
library(casal)

## Add other dependency funcitons
source("C:/Craig/projects/2016/DEE2015-02(TrawlSurveySimulation)/R/Initialisation.r");
source(make.filename(file = "auxiliary.functions.R", path = DIR$'General functions'));
source(make.filename(file = "Logistic_normal\\Chris's_r_code.r", path = DIR$'Base'))

## Import example Casal file as this is 
casal_fits  = extract.fits(path = DIR$'HAK1_csl', file = "out.log");


## Run through Chris code manually
compdat = casal_fits$subaTANageDEC
## restructure for Chris's Code
compdat$obs =  as.matrix(casal_fits$subaTANageDEC$obs)
compdat$exp =  round(as.matrix(casal_fits$subaTANageDEC$fits),5)
compdat$N = casal_fits$subaTANageDEC$error.value[,1]


sigma = 0.4
phi=0.2;
covmat=NULL;
sepbysex=F;
sexlag=F;
robust=F;
ARMA=F

NLLlogistnorm(compdat,sigma = 0.4,phi,covmat=NULL,sepbysex=F, sexlag=F, robust=F, ARMA=F)


## For converting R code to C++
library(Rcpp11)


## check inverse function written in C++
x = c(2,1,5,3,5,4,5,7,1,3,8,3,7,7,3,9);
X = matrix(x, byrow = T, nrow = 4)

GetRho = function(Phi1, nBin) {
  rho = rep(1.0,nBin-1);
  for( i in 1:(nBin - 1))
    rho[i]= Phi1^i;
    
  rho;
}

GetRho(0.2, 15)


GetRho2 = function(Phi1,Phi2, nBin) {
  acvect = rep(1.0,nBin);
  acvect[1] = Phi1 / (1 - Phi2);
  for(i in 2:nBin) {
    acvect[i] = Phi1 * acvect[i - 1] + Phi2 * acvect[i - 2];
  }
  acvect;
}

GetRho2(0.2,0.3, 15)
