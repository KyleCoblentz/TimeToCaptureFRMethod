################################################################################
### Bold Jumping Spider Functional Response Estimation -- Time to Feed Data
################################################################################

### clear workspace

rm(list = ls())

### set working directory

#setwd('/')

### load packages

library(dplyr); library(ggplot2); library(cowplot); library(rstan);

### set options for rstan

options(mc.cores = parallel::detectCores())

rstan_options(auto_write = TRUE)

Sys.setenv(LOCAL_CPPFLAGS = '-march=native')

##################################################################################
### Get everything set up
##################################################################################

### load jumping spider data

data <- read.csv('BoldJumper_TimeToFeed.csv')

colnames(data)[1] <- 'Spider'

### split data by spider

Bold1 <- data %>% filter(Spider == 'Bold1')

Bold2 <- data %>% filter(Spider == 'Bold2')

### drop observations in which spider did not finish feeding before attacking
### the next prey item: observation less than a minute

Bold1 <- Bold1 %>% filter(TimeToFeed > 1)

Bold2 <- Bold2 %>% filter(TimeToFeed > 1)

data <- data %>% filter(TimeToFeed > 1)

### look at some plots of the data 

### All Data
  
ggplot(data = data, aes(x = PreyAvailable, y = TimeToFeed)) + geom_point() + theme_cowplot()
  
### spider 1

ggplot(data = Bold1, aes(x = PreyAvailable, y = TimeToFeed)) + geom_point() + theme_cowplot()

### Spider 2

ggplot(data = Bold2, aes(x = PreyAvailable, y = TimeToFeed)) + geom_point() + theme_cowplot()

### define Stan model: must have ExponentialRegression2.stan in the working directory

ExpModel <- stan_model(file = 'ExponentialRegression2.stan')

######################################################################################
### Fit models for each spider 
######################################################################################

######################################################################################
### Bold Jumper 1
######################################################################################

### set up data list for Stan

N1 <- nrow(Bold1)

R1 <- Bold1$PreyAvailable

time1 <- Bold1$TimeToFeedDays

Bold1DataList <- list(N = N1,
                      R = R1,
                      time = time1)

### fit model using Stan

Bold1fit <- sampling(ExpModel,
                     data = Bold1DataList,
                     chains = 4,
                     iter = 5000,
                     warmup = 1000, 
                     refresh = 0)

### Extract samples 

Bold1Samples <- extract(Bold1fit)

### Get estimates and credible intervals

### space clearance rates -- a

Bold1_a <- median(Bold1Samples$a)

Bold1_a_CrI <- quantile(Bold1Samples$a, probs = c(0.025, 0.975))

### convert space clearance estimates to units of m^2
### arena was 0.196 m^2

Bold1_a_m2 <- Bold1_a * 0.196

Bold1_a_CrI_m2 <- Bold1_a_CrI * 0.196

### handling time -- h

Bold1_h <- median(Bold1Samples$h)

Bold1_h_CrI <- quantile(Bold1Samples$h, probs = c(0.025, 0.975))

### limits for predicted times to feed

Bold1_MedianTime <- apply(Bold1Samples$ypred, 2, function(x) quantile(x, probs = 0.5))

Bold1_UpperTime <- apply(Bold1Samples$ypred, 2, function(x) quantile(x, probs = 0.975))

Bold1_LowerTime <- apply(Bold1Samples$ypred, 2, function(x) quantile(x, probs = 0.025))

### Median for estimated times to kill 

Bold1_EstTimeToKill <- data.frame(PreyAvailable = 10:1,
                                  EstTimeToKill = sort(Bold1_MedianTime),
                                  UpperTime = sort(Bold1_UpperTime),
                                  LowerTime = sort(Bold1_LowerTime))

### plot to evaluate fit of the model

Bold1Plot <- ggplot(data = Bold1, aes(x = PreyAvailable, y = TimeToFeedDays)) + geom_point(size = 2) + 
  geom_line(data = Bold1_EstTimeToKill, aes(x = PreyAvailable, y = EstTimeToKill), size = 1) + 
  geom_line(data = Bold1_EstTimeToKill, aes(x = PreyAvailable, y = UpperTime), linetype = 'dashed') + 
  geom_line(data = Bold1_EstTimeToKill, aes(x = PreyAvailable, y = LowerTime), linetype = 'dashed') + 
  theme_cowplot() + xlab('Prey Available') + ylab('Time to Feed \n(Days)') + ggtitle('Spider 1', 
                                                                            subtitle = 'est. a = 6.5; 95% CrI (3.6, 11.0) \nest. h = 0.00556; CrI (0.004, 0.0058)') + 
  ylim(c(0, 0.15))

### what would a traditional functional response look like

Bold1_FR <- data.frame(PreyAvailable = 0:10,
                       FeedingRate = Bold1_a_m2*0:10/(1 + Bold1_a_m2*Bold1_h*0:10))

Bold1_FR_Lower <- data.frame(PreyAvailable = 0:10,
                       FeedingRate = Bold1_a_CrI_m2[1]*0:10/(1 + Bold1_a_CrI_m2[1]*Bold1_h_CrI[1]*0:10))

Bold1_FR_Upper <- data.frame(PreyAvailable = 0:10,
                             FeedingRate = Bold1_a_CrI_m2[2]*0:10/(1 + Bold1_a_CrI_m2[2]*Bold1_h_CrI[2]*0:10))


ggplot(data = Bold1_FR, aes(x = PreyAvailable, y = FeedingRate)) + geom_line(size = 1) + 
  geom_ribbon(aes(x = 0:10, ymin = Bold1_FR_Lower$FeedingRate, ymax = Bold1_FR_Upper$FeedingRate), alpha = 0.5) +
  theme_cowplot() + xlab('Prey Density') + ylab('Feeding Rate')


######################################################################################
### Bold Jumper 2
######################################################################################

### set up data list for Stan

N2 <- nrow(Bold2)

R2 <- Bold2$PreyAvailable

time2 <- Bold2$TimeToFeedDays

Bold2DataList <- list(N = N2,
                      R = R2,
                      time = time2)

### fit model using Stan

Bold2fit <- sampling(ExpModel,
                     data = Bold2DataList,
                     chains = 4,
                     iter = 1000,
                     warmup = 500, 
                     refresh = 0)

### Extract samples 

Bold2Samples <- extract(Bold2fit)

### Get estimates and credible intervals

### space clearance rates -- a
### and coversions to m^2

Bold2_a <- median(Bold2Samples$a)

Bold2_a_m2 <- Bold2_a * 0.196

Bold2_a_CrI <- quantile(Bold2Samples$a, probs = c(0.025, 0.975))

Bold2_a_CrI_m2 <- Bold2_a_CrI * 0.196

### handling time -- h

Bold2_h <- median(Bold2Samples$h)

Bold2_h_CrI <- quantile(Bold2Samples$h, probs = c(0.025, 0.975))

### limits for predicted times to feed

Bold2_MedianTime <- apply(Bold2Samples$ypred, 2, function(x) quantile(x, probs = 0.5))

Bold2_UpperTime <- apply(Bold2Samples$ypred, 2, function(x) quantile(x, probs = 0.975))

Bold2_LowerTime <- apply(Bold2Samples$ypred, 2, function(x) quantile(x, probs = 0.025))

### Median for estimated times to kill 

Bold2_EstTimeToKill <- data.frame(PreyAvailable = 1:9,
                                  EstTimeToKill = Bold2_MedianTime,
                                  UpperTime = Bold2_UpperTime,
                                  LowerTime = Bold2_LowerTime)

### plot to evaluate fit of the model

Bold2Plot <- ggplot(data = Bold2, aes(x = PreyAvailable, y = TimeToFeedDays)) + geom_point(size = 2) + 
  geom_line(data = Bold2_EstTimeToKill, aes(x = PreyAvailable, y = EstTimeToKill), size = 1) + 
  geom_line(data = Bold2_EstTimeToKill, aes(x = PreyAvailable, y = UpperTime), linetype = 'dashed') + 
  geom_line(data = Bold2_EstTimeToKill, aes(x = PreyAvailable, y = LowerTime), linetype = 'dashed') + 
  theme_cowplot() + xlab('Prey Available') + ylab('Time to Feed \n(Days)') + ggtitle('Spider 2', 
                                                                            subtitle = 'est. a = 14.0; 95% CrI (6.25, 25.83) \nest. h = 0.0035; CrI (0.0024, 0.0037)') +
  ylim(c(0, 0.15))

### results for Bold Jumper 2

### what would a traditional functional response look like

Bold2_FR <- data.frame(PreyAvailable = 0:10,
                       FeedingRate = Bold2_a_m2*0:10/(1 + Bold2_a_m2*Bold2_h*0:10))

Bold2_FR_Lower <- data.frame(PreyAvailable = 0:10,
                             FeedingRate = Bold2_a_CrI_m2[1]*0:10/(1 + Bold2_a_CrI_m2[1]*Bold2_h_CrI[1]*0:10))

Bold2_FR_Upper <- data.frame(PreyAvailable = 0:10,
                             FeedingRate = Bold2_a_CrI_m2[2]*0:10/(1 + Bold2_a_CrI_m2[2]*Bold2_h_CrI[2]*0:10))

ggplot(data = Bold2_FR, aes(x = PreyAvailable, y = FeedingRate)) + geom_line(size = 1) + 
  geom_ribbon(aes(x = 0:10, ymin = Bold2_FR_Lower$FeedingRate, ymax = Bold2_FR_Upper$FeedingRate), alpha = 0.5) +
  theme_cowplot() + xlab('Prey Density') + ylab('Feeding Rate')


##################################################################################################
### All the spider data
##################################################################################################

### set up data list for Stan

Nall <- nrow(data)

Rall <- data$PreyAvailable

timeall <- data$TimeToFeedDays

BoldallDataList <- list(N = Nall,
                      R = Rall,
                      time = timeall)

### fit model using Stan

Boldallfit <- sampling(ExpModel,
                     data = BoldallDataList,
                     chains = 4,
                     iter = 1000,
                     warmup = 500, 
                     refresh = 0)

### Extract samples 

BoldallSamples <- extract(Boldallfit)

### Get estimates and credible intervals

### space clearance rates -- a
### and conversion to m^2

Boldall_a <- median(BoldallSamples$a)

Boldall_a_m2 <- Boldall_a * .196

Boldall_a_CrI <- quantile(BoldallSamples$a, probs = c(0.025, 0.975))

Boldall_a_CrI_m2 <- Boldall_a_CrI * .196

### handling time -- h

Boldall_h <- median(BoldallSamples$h)

Boldall_h_CrI <- quantile(BoldallSamples$h, probs = c(0.025, 0.975))

### limits for predicted times to feed

Boldall_MedianTime <- apply(BoldallSamples$ypred, 2, function(x) quantile(x, probs = 0.5))

Boldall_UpperTime <- apply(BoldallSamples$ypred, 2, function(x) quantile(x, probs = 0.975))

Boldall_LowerTime <- apply(BoldallSamples$ypred, 2, function(x) quantile(x, probs = 0.025))

### Median for estimated times to kill 

Boldall_EstTimeToKill <- data.frame(PreyAvailable = 1:10,
                                  EstTimeToKill = Boldall_MedianTime,
                                  UpperTime = Boldall_UpperTime,
                                  LowerTime = Boldall_LowerTime)

### plot to evaluate fit of the model

BoldAllPlot <- ggplot(data = data, aes(x = PreyAvailable, y = TimeToFeedDays)) + geom_point(size = 2) + 
  geom_line(data = Boldall_EstTimeToKill, aes(x = PreyAvailable, y = EstTimeToKill), size = 1) + 
  geom_line(data = Boldall_EstTimeToKill, aes(x = PreyAvailable, y = UpperTime), linetype = 'dashed') + 
  geom_line(data = Boldall_EstTimeToKill, aes(x = PreyAvailable, y = LowerTime), linetype = 'dashed') + 
  theme_cowplot() + xlab('Prey Available') + ylab('Time to Feed \n(Days)') + ggtitle('Both Spiders', 
                                                                          subtitle = 'est. a = 7.2; 95% CrI (4.6, 10.4) \nest. h = 0.0035; CrI (0.00279, 0.00367)') +
  ylim(c(0,0.15))

### what would a traditional functional response look like

Boldall_FR <- data.frame(PreyAvailable = 0:10,
                       FeedingRate = Boldall_a_m2*0:10/(1 + Boldall_a_m2*Boldall_h*0:10))

Boldall_FR_Lower <- data.frame(PreyAvailable = 0:10,
                             FeedingRate = Boldall_a_CrI_m2[1]*0:10/(1 + Boldall_a_CrI_m2[1]*Boldall_h_CrI[1]*0:10))

Boldall_FR_Upper <- data.frame(PreyAvailable = 0:10,
                             FeedingRate = Boldall_a_CrI_m2[2]*0:10/(1 + Boldall_a_CrI_m2[2]*Boldall_h_CrI[2]*0:10))

ggplot(data = Boldall_FR, aes(x = PreyAvailable, y = FeedingRate)) + geom_line(size = 1) + 
  geom_ribbon(aes(x = 0:10, ymin = Boldall_FR_Lower$FeedingRate, ymax = Boldall_FR_Upper$FeedingRate), alpha = 0.5) +
  theme_cowplot() + xlab('Prey Density') + ylab('Feeding Rate')
