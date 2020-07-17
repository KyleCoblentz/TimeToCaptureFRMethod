############################################################################
### maximum likelihood implementation of time to capture method
############################################################################

### clear workspace

rm(list = ls())

### set working directory

#setwd('')

### load packages

library(dplyr); library(ggplot2); library(cowplot); library(bbmle);

### get bold jumping spider data

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

##################################################################################################
### set up for maximum likelihood estimation using bbmle
##################################################################################################

### function for the negative log likelihood of the truncated exponential distribution

LL <- function(a, h, data) {
  -sum(log(a*data$R) - a*data$R*(data$Time - h))
}

### Spider One

LikDataBold1 <- Bold1 %>% rename(R = PreyAvailable, Time = TimeToFeedDays)

### get space clearance rate with handling time fixed to its maximum likelihood estimate (see supplementary material)

fit <- mle2(minuslogl = LL, start = list(a = 1), data = list(data = LikDataBold1), fixed = list(h = min(LikDataBold1$Time)))

summary(fit)

fit.profile <- profile(fit)

plot(fit.profile)

confint(fit.profile)

### convert estimates to m^2 -- arena was 0.196m^2

coef(fit)['a'] * 0.196

confint(fit.profile) * 0.196

### get lower bound for handling time (see supplementary material)

### use F distribution with 2 and 2(n-1) degrees of freedom

qf(0.025, 2, 2*(nrow(LikDataBold1)-1), lower.tail = FALSE)

4.22/(coef(fit)[1]*mean(LikDataBold1$R)*nrow(LikDataBold1))

min(LikDataBold1$Time) - 0.0015

### 

### Spider Two

LikDataBold2 <- Bold2 %>% rename(R = PreyAvailable, Time = TimeToFeedDays)

### space clearance rate estimate

fit2 <- mle2(minuslogl = LL, start = list(a = 1), data = list(data = LikDataBold2), fixed = list(h = min(LikDataBold2$Time)))

summary(fit2)

fit.profile2 <- profile(fit2)

plot(fit.profile2)

confint(fit.profile2)

### convert to m^2

coef(fit2)['a'] * 0.196

confint(fit.profile2) * 0.196

### get lower bound for handling time

### use F distribution with 2 and 2(n-1) degrees of freedom

qf(0.05, 2, 2*(nrow(LikDataBold2)-1), lower.tail = FALSE)

3.49/(coef(fit2)[1]*mean(LikDataBold2$R)*nrow(LikDataBold2))

min(LikDataBold2$Time) - 0.0007564

### All Data

LikDataBoldAll <- data %>% rename(R = PreyAvailable, Time = TimeToFeedDays)


fit3 <- mle2(minuslogl = LL, start = list(a = 1), data = list(data = LikDataBoldAll), fixed = list(h = min(LikDataBoldAll$Time)))

summary(fit3)

fit.profile3 <- profile(fit3)

plot(fit.profile3)

confint(fit.profile3)

### convert to m^2

coef(fit3)['a'] * 0.196

confint(fit.profile3) * 0.196

### get lower bound for handling time

### use F distribution with 2 and 2(n-1) degrees of freedom

qf(0.05, 2, 2*(nrow(LikDataBoldAll)-1), lower.tail = FALSE)

3.49/(coef(fit2)[1]*1*nrow(LikDataBoldAll))

min(LikDataBoldAll$Time) - 0.0014
