###############################################################################################################
### Simulations to examine the efficacy of the individual functional response method -- Bayesian framework
###############################################################################################################

### load packages 

library(rstan);library(ggplot2);library(cowplot);library(dplyr);

### set working directory

# setwd('')

### set options for rstan

options(mc.cores = parallel::detectCores())

rstan_options(auto_write = TRUE)

Sys.setenv(LOCAL_CPPFLAGS = '-march=native')


### source R script to define functions for simulation
# make sure script and stan model file are in the same working directory!

source('BayesianFunctionsforSim.R')

### sourcing the R script will load two functions: individualsim and experimentsim

# individualsim takes arguments a, h, and n.prey.start and will return a simulated 
# experiment of an individual with space clearance rate a and handling time h feeding 
# down its functional response with a starting prey density of n.prey.start

# experimentsim takes arguments a, h, n.prey.start, n.retrial, and n.sim
# it calls individualsim to simulate data for individuals and analyzes that data
# the number of individual simulations to do is given by n.sim
# if n.retrial > 0 the code runs that number of retrials for each individual simulation at a prey density of one (to better the estimate of a)
# experimentsim returns the estimates of a and h, credible intervals for each, whether the true a and h are within the credible intervals, and how long the experiment would take to run

### an example simulation

example <- experimentsim(a = 0.5, h = 0.01, n.prey.start = 25, n.retrial = 1, n.sim = 100)

### tablevalues (defined below) is a convenience function for extracting summary data on the model performance

tablevalues <- function(data, truea, trueh){
  CrICoveragea <- sum(data$in.CrI.a)/length(data$in.CrI.a) 
  AbsDiffa <- mean(abs(data$mediana - truea))
  PercGreata <- sum(data$mediana > truea)/length(data$mediana)
  CrICoverageh <- sum(data$in.CrI.h)/length(data$in.CrI.h)
  AbsDiffh <- mean(abs(data$medianh - trueh))
  PercGreath <- sum(data$medianh > trueh)/length(data$medianh)
  
  out <- c(CrICoveragea, AbsDiffa, PercGreata, CrICoverageh, AbsDiffh, PercGreath)
  
  names(out) <- c('CrICoveragea', 'AbsDiffa', 'PercGreata', 'CrIcoverageh', 'AbsDiffh', 'PercGreath')
  
  print(out)
  
}

### example of use

tablevalues(example, truea = 0.5, trueh = 0.01)






