#######################################################
### simulations to examine confidence interval coverage
### for handling times using the Epstein and Sobel 
### formula
#######################################################

### function to perform simulations

individualsim <- function(a, h, n.prey.start) {
  
  Time.to.feed <- vector(length = n.prey.start)
  
  R <- vector(length = n.prey.start)
  
  for (i in 1:n.prey.start) {
    
    R[i] <- ifelse(i == 1, n.prey.start, R[i-1]-1) 
    # number of prey available
    
    Time.to.feed[i] <-rexp(n  = 1, rate = a*R[i]) + h
    # rate is the functional response at that prey density
    
  }
  
  data <- data.frame(Time.to.feed = Time.to.feed, R = R)
  
  return(data)
  
}

### function to run simulations, analyze the data, and give estimates and CI's 

experimentsim <- function(a, h, n.prey.start, n.retrial, n.sim) {
  
  ### set up vectors to store data
  
  aest <- vector(length = n.sim)
  
  uppera <- vector(length = n.sim)
  
  lowera <- vector(length = n.sim)
  
  hest <- vector(length = n.sim)
  
  upperh <- vector(length = n.sim)
  
  lowerh <- vector(length = n.sim)
  
  in.CrI.a <- vector(length = n.sim)
  
  in.CrI.h <- vector(length = n.sim)
  
  Exp.Time <- vector(length = n.sim)
  
  simulation <- 1:n.sim
  
  ### define negative log likelihood 
  
  LL <- function(a, h, data) {
    -sum(log(a*data$R) - a*data$R*(data$Time.to.feed - h))
  }
  
  ### set up a loop to simulate data, perform analysis, and store the results
  
  for(i in 1:n.sim) {
    
    ### simulate data for an individual
    
    if (n.retrial == 0) {
      
      ind <- individualsim(a = a, h = h, n.prey.start = n.prey.start)
      
    } else {
      
      ind <- individualsim(a = a, h = h, n.prey.start = n.prey.start)
      
      indplus <- data.frame(Time.to.feed = rexp(n=n.retrial, a/(1+a*h)), R = rep(1, times = n.retrial))
      
      ind <- rbind(ind, indplus)
      
    }
    
    ### max L estimate for h
    
    maxLh <- min(ind$Time.to.feed)
    
    ### get maximum likelihood estimate and confidence interval for a
    
    fit <- mle2(minuslogl = LL, start = list(a = a), data = list(data = ind), fixed = list(h = maxLh), lower = list(a = 1e-9))
    
    ### get lower interval estimate for h 
    
    Fval <- qf(0.025, 2, 2*(nrow(ind)-1), lower.tail = FALSE)
    
    BoundMean <- Fval/(coef(fit)['a']*mean(ind$R)*nrow(ind))
    
    hlow <- maxLh - BoundMean 
    
    ### fill in all of the vectors with the appropriate estimates
    
    aest[i] <- coef(fit)['a']
    
    uppera[i] <- confint(fit)[2]
    
    lowera[i] <- confint(fit)[1]
    
    hest[i] <- maxLh 
    
    upperh[i] <- maxLh 
    
    lowerh[i] <- hlow
      
    in.CrI.a[i] <- ifelse(a <= uppera[i] & a >= lowera[i], TRUE, FALSE)
    
    in.CrI.h[i] <- ifelse(h <= upperh[i] & h >= lowerh[i], TRUE, FALSE)
    
  }
  
  ### pull all of the vectors together into a data frame
  
  data.out <- data.frame(simulation = simulation, esta = aest, uppera = uppera, 
                         lowera = lowera, hest = hest, upperh = upperh, 
                         lowerh = lowerh,
                         in.CrI.a = in.CrI.a, in.CrI.h = in.CrI.h,
                         in.CrI.hMax = in.CrI.hMax)
  
  return(data.out)
  
}









