# test_C_code.r
#@author C.Marsh
#@date 15/10/16
#@description
## This script will be the basis for a unit test in Casal2, it will check all the negative likelihood scores for a range of inputs
## it will also test the Random number generator.

## Add Libraries
library(casal)

## Add other dependency funcitons
## Will be using the HAK1 example for unisex test case.
source("C:/Craig/projects/2016/DEE2015-02(TrawlSurveySimulation)/R/Initialisation.r");
source(make.filename(file = "auxiliary.functions.R", path = DIR$'General functions'));
source(make.filename(file = "Logistic_normal\\Chris's_r_code.r", path = DIR$'Base'))

## Import example Casal file as this is 
casal_fits  = extract.fits(path = DIR$'HAK1_csl', file = "out.log");

###################################################
####  Single sex comp data
###################################################
compdat = casal_fits$subaTANageDEC
## restructure for Chris's Code
compdat$obs =  as.matrix(casal_fits$subaTANageDEC$obs)
compdat$exp =  round(as.matrix(casal_fits$subaTANageDEC$fits),5)
compdat$N = casal_fits$subaTANageDEC$error.value[,1]

sigma = 0.243
phi=0.0;
covmat=NULL;
sepbysex=F;
sexlag=F;
robust=F;
ARMA=F

#########
## Test 1
#########
## unisex data, sigma and no phi
NLLlogistnorm(compdat,sigma ,phi,covmat=NULL,sepbysex=F, sexlag=F, robust=F, ARMA=F)
## C++ answer
## 936.652
#########
## Test 2
#########
## unisex data, sigma and single phi
phi= 0.435;
NLLlogistnorm(compdat,sigma ,phi,covmat=NULL,sepbysex=F, sexlag=F, robust=F, ARMA=F)
## C++ answer
## 1509.43
#########
## Test 3
#########
## unisex data, sigma and two phi and ARMA = F
phi= c(0.235,-0.284);
NLLlogistnorm(compdat,sigma ,phi,covmat=NULL,sepbysex=F, sexlag=F, robust=F, ARMA=F)
## C++ answer
## 1280.26
#########
## Test 4
#########
## unisex data, sigma and two phi and ARMA = T
NLLlogistnorm(compdat,sigma ,phi,covmat=NULL,sepbysex=F, sexlag=F, robust=F, ARMA=T)
## C++ answer
## 982.383

#########
## Test 5
#########
## unisex data, sigma and two phi and ARMA = T,robust=T
NLLlogistnorm(compdat,sigma ,phi,covmat=NULL,sepbysex=F, sexlag=F, robust=T, ARMA=T)
## C++ answer
## -2925.01


###################################################
####  Sexed comp data
###################################################
## Use the ling example now.
casal_fits  = extract.fits(path = DIR$'LIN3_csl', file = "out.log");
compdat = casal_fits$Tangaroa_propn_at_age_Jan
## restructure for Chris's Code
compdat$obs =  as.matrix(casal_fits$Tangaroa_propn_at_age_Jan$obs)
compdat$exp =  round(as.matrix(casal_fits$Tangaroa_propn_at_age_Jan$fits),5)
compdat$N = casal_fits$Tangaroa_propn_at_age_Jan$error.value[,1]

#########
## Test 6
#########
## Sexed data, sepsex = T
sigma = 0.243
phi= c(0.235,-0.284);
sepbysex=T
NLLlogistnorm(compdat,sigma ,phi,covmat=NULL,sepbysex=T, sexlag=F, robust=F, ARMA=F)
## C++ answer
## 936.652



