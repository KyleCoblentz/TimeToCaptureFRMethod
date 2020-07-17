################################################################
### Simulations to examine the efficacy of the maximum 
### likelihood approach to estimating parameters using 
### time-to-capture data
################################################################

### load packages

library(bbmle); library(ggplot2); library(cowplot);

### set working directory

#setwd('')

### source functions for simulations -- The sourced R script must be in the working directory 

source('MaxLFunctionsforSim.R')

### sourcing the R script will load two functions: individualsim and experimentsim

# individualsim takes arguments a, h, and n.prey.start and will return a simulated 
# experiment of an individual with space clearance rate a and handling time h feeding 
# down its functional response with a starting prey density of n.prey.start

# experimentsim takes arguments a, h, n.prey.start, n.retrial, and n.sim
# it calls individualsim to simulate data for individuals and analyzes that data
# the number of individual simulations to do is given by n.sim
# if n.retrial > 0 the code runs that number of retrials for each individual simulation at a prey density of one (to better the estimate of a)
# experimentsim returns the estimates of a and h, credible intervals for each, and whether the true a and h are within the credible intervals
# this uses the maximum likelihood method outlined in the supplemental material of the manuscript

### an example simulation

example <- experimentsim(a = 0.5, h = 0.1, n.prey.start = 20, n.retrial = 0, n.sim = 30)

###########################################################################
### write a convenience function to extract summaries from simulations
###########################################################################

tablevalues <- function(data, truea, trueh){
  CrICoveragea <- sum(data$in.CrI.a)/length(data$in.CrI.a) 
  AbsDiffa <- mean(abs(data$esta - truea))
  PercGreata <- sum(data$esta > truea)/length(data$esta)
  CrICoverageh <- sum(data$in.CrI.h)/length(data$in.CrI.h)
  AbsDiffh <- mean(abs(data$hest - trueh))
  PercGreath <- sum(data$hest > trueh)/length(data$hest)
  
  out <- c(CrICoveragea, AbsDiffa, PercGreata, CrICoverageh, AbsDiffh, PercGreath)
  
  names(out) <- c('CrICoveragea', 'AbsDiffa', 'PercGreata', 'CrIcoverageh', 'AbsDiffh', 'PercGreath')
  
  print(out)
  
}

### example of use 

tablevalues(example, truea = 0.5, trueh = 0.1)
