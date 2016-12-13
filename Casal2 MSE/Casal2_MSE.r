## Trying to do an MSE based algorithm using Casal2

## set directory
setwd("C:/Program Files/CASAL2");

## Call Casal2 on the simple example
command = "casal2 -r -c Examples/Simple/casal2.txt --single-step";

## Do a system call within R to call casal2

system(command, intern = T, show.output.on.console = T);

shell(command);

## MSE algorithm

## Step 1 do a -e (from original data)

## Step 2 evaluate HCR

## Step 3 Project the population forward one year Implementing the HCR rule
  ## Sub commands Time invariant parameters of time variant?
  	# This means that we draw one set of parameters for the projected time or a different parameter for each year
  ## 

## Step 4 Simulate observations

## Step 5 Re estimate (Step 1)

## Repeat Step 1-5 Ny times

## then repeat to sample over any estimable distribution (sample from MCMC sample?)

## Project the population forward one year

## Simulate observations

## Re es








