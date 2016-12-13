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


## now lets comup with a sexed example
binnam = colnames(compdat$fits)
Covar = covmat.logistnorm(sigma = 0.4,phi = 0.2,binnam,sepbysex=F,sexlag=F,ARMA=F)
round( covmat.logistnorm(sigma = 0.4,phi = 0.2,binnam,sepbysex=F,sexlag=F,ARMA=F),3)

round( covmat.logistnorm(sigma = 0.2,phi = c(0.4,0.2),binnam,sepbysex=F,sexlag=F,ARMA=F),3)

getrho(phi = c(0.4,0.2), kk = length(binnam), ARMA = F)

round( covmat.logistnorm(sigma = 0.2,phi = c(0.4,0.2),binnam,sepbysex=F,sexlag=F,ARMA=T),3)


## Randomly generate form this distribution
library(BaM)

expprop = compdat$fits
kk <- length(expprop)
sigma = 0.4
phi = 0.2
rhovec = ARMAacf(ar=phi,lag.max=kk)
round(rhovec,2)

rlogistnorm(1,expprop,sigma,phi=0,covmat=NULL,ARMA=F)


set.seed(324)
library(logitnorm)
rlogitnorm(n = 1,mu = expprop, sigma = sigma)

mu1 = 0.0301401 
mu2 = 0.0466622

rlogitnorm(n = 1, mu = mu1, sigma)
rlogistnorm(1,mu1, sigma,phi=0,covmat=NULL,ARMA=F)


x = seq(0,1,0.001)
compdat$obs = rep(0.03,length(x))
compdat$exp = as.matrix(x)
compdat$N = 1

prob = dlogitnorm(q = x,mu = 0.03, sigma = 0.2)

prob2 = NLLlogistnorm(x,0.2 ,phi =0.0,covmat=NULL,sepbysex, sexlag=F, robust, ARMA)

plot(x,prob)


## test the multivariate normal calculation using the cholkeskry decompostition
## in the bivariate case
covar = matrix(c(1.4,0.6,0.6,1.54), nrow = 2, byrow = TRUE)
mu = c(3.1,1.35)
cho_covar = chol(covar)
nrow(Covar)
rmultinorm = function(n = 100, covar) {
  store_mat = matrix(0.0,nrow = nrow(covar), ncol = n)
  cho_covar = (chol(covar))
  for( k in 1:n) {
  rng_norm = rnorm(n = nrow(covar))
    for (i in 1:nrow(covar)) {
      for(j in 1:nrow(covar)) {
        store_mat[i,k] = store_mat[i,k] +  cho_covar[j,i] * rng_norm[j]
      }
    }
  }
  t(store_mat)
}

rngs = rmultinorm(n = 100000,covar)
mean(rngs)
cov(rngs)
plot(rngs)
library(mvtnorm)
actual = rmvnorm(n = 10000, sigma = covar)
points(actual, col = "red")



sigma <- matrix(c(4,-3,-3,9),2,2)
sigma
## We now seek to find a matrix M such that M times its transpose equals sigma. There are many matrices that do this; one of them is the transpose of the Cholesky square root:
M <- t(chol(sigma))
M %*% t(M)
#We now recall that if Z is a random vector and M is a matrix, then the covariance matrix of MZ equals M cov(Z) Mt
Z <- matrix(rnorm(400),2,200) # 2 rows, 200 columns
X <- t(M %*% Z)

plot(X)
Xbar <- apply(X,2,mean)
S <- cov(X)

rngs = rmultinorm(n = 200,sigma)
cov(rngs)