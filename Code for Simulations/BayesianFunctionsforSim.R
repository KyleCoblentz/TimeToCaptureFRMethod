##################################################################################################
### Function for simulations to examine the efficacy of the individual functional response method
##################################################################################################

##################################################################################################
### define function for simulating individual's times to feed
##################################################################################################

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

#####################################################################################################################
### define function to run simulations across different scenarios of parameters and experimental designs
#####################################################################################################################

experimentsim <- function(a, h, n.prey.start, n.retrial, n.sim) {
  
  ### set up vectors to store data
  
  mediana <- vector(length = n.sim)
  
  uppera <- vector(length = n.sim)
  
  lowera <- vector(length = n.sim)
  
  medianh <- vector(length = n.sim)
  
  upperh <- vector(length = n.sim)
  
  lowerh <- vector(length = n.sim)
  
  in.CrI.a <- vector(length = n.sim)
  
  in.CrI.h <- vector(length = n.sim)
  
  Exp.Time <- vector(length = n.sim)
  
  simulation <- 1:n.sim
  
  ### define stan model
  
  ExponentialModel <- stan_model(file = 'ExponentialRegression2.stan')
  
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
    
    ### create data list for stan
    
    N <- n.prey.start + n.retrial
    
    R <- ind$R
    
    time <- ind$Time.to.feed
    
    datalist <- list(N = N,
                     R = R,
                     time = time)
    
    ### fit model using stan
    
    fit <- sampling(ExponentialModel,
                data = datalist,
                chains = 4,
                iter = 1000,
                warmup = 500,
                refresh = 0)
    
    ### extract samples and fill in vectors 
    
    samples <- extract(fit)
    
    mediana[i] <- median(samples$a) 
    
    uppera[i] <- quantile(samples$a, probs = c(0.975))
    
    lowera[i] <- quantile(samples$a, probs = c(0.025))
    
    medianh[i] <- median(samples$h)
    
    upperh[i] <- quantile(samples$h, probs = c(0.975))
    
    lowerh[i] <- quantile(samples$h, probs = c(0.025))
    
    in.CrI.a[i] <- ifelse(a <= uppera[i] & a >= lowera[i], TRUE, FALSE)
    
    in.CrI.h[i] <- ifelse(h <= upperh[i] & h >= lowerh[i], TRUE, FALSE)
    
    Exp.Time[i] <- sum(time)
    
  }
  
  ### pull all of the vectors together into a data frame
  
  data.out <- data.frame(simulation = simulation, mediana = mediana, uppera = uppera, 
                         lowera = lowera, medianh = medianh, upperh = upperh, 
                         lowerh = lowerh, in.CrI.a = in.CrI.a, in.CrI.h = in.CrI.h,
                         Exp.Time = Exp.Time)
  
  return(data.out)
  
}






















