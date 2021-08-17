# Sensitivity Models
# These models are run to produce Table 4

# 1. Model without Facebook data
# 2. Model with Facebook bias at 0%
# 3. Model with Facebook bias at 11% 
# 4. Model with LFS bias at 4%
# 5. Model with LFS bias at 30%
# 6. Model with Gamma(1,1)

# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("rjags")
# install.packages("coda")
# install.packages("MCMCvis")

library(tidyverse)
library(readxl)
library(rjags)
library(coda)
library(MCMCvis)

###############################################################################
# Data Preparation
###############################################################################

# Read data for 2018
data_m1_18 <-read_csv("./data/data_m1_2018.csv")
data_m1_18 <- data_m1_18  %>% arrange(lfs)

# Re-arrange the data with Greece as the last row
data_g_18 <- data_m1_18 %>% filter(country=="Greece")
data_m1_18 <- data_m1_18 %>% filter(country!="Greece")
data_m1_18 <-rbind(data_m1_18, data_g_18)

# Read data for 2019
data_m1_19 <-read_csv("./data/data_m1_2019.csv")
data_m1_19 <- data_m1_19  %>% arrange(lfs)

# Re-arrange the data with Greece as the last row
data_g_19 <- data_m1_19 %>% filter(country=="Greece")
data_m1_19 <- data_m1_19 %>% filter(country!="Greece")

data_m1_19 <-rbind(data_m1_19, data_g_19)

# Merge the two datasets for the two years
data_m1_18 <- data_m1_18 %>% select(country, lfs, fb, confidence, pop_r, rate, inflow_r, outflow_r, gdp_r,
                                    unemp_r, alg) %>% mutate(year="2018")
data_m1_19 <- data_m1_19 %>% select(country,lfs, fb, confidence, pop_r, rate, inflow_r, outflow_r, gdp_r,
                                    unemp_r) %>% mutate(alg=NA, year="2019")

# Re-order the country
data_m1_18$country<- as.factor(data_m1_18$country)
data_m1_19$country<- as.factor(data_m1_19$country)
x=c("Austria", "Belgium", "Czech Republic", "Latvia", "Sweden",
    "France", "Germany", "Hungary", "Lithuania",
    "Denmark", "Finland", "Ireland", "Italy", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Spain",
    "Greece")

data_m1_18<-data_m1_18 %>%
  mutate(country =  factor(country, levels = x)) %>%
  arrange(country)
data_m1_19<-data_m1_19 %>%
  mutate(country =  factor(country, levels = x)) %>%
  arrange(country)

data_m1<-rbind(data_m1_18, data_m1_19)

# Natural Log
data_m1$rate<-log(1-data_m1$rate)
data_m1$alg<-log(1-data_m1$alg)


lfs<-t(as.vector(data_m1['lfs']))
fb<-t(as.vector(data_m1['fb']))
pop<-t(as.vector(data_m1['pop_r']))
rate<-t(as.vector(data_m1['rate']))
inflow<-t(as.vector(data_m1['inflow_r']))
outflow<-t(as.vector(data_m1['outflow_r']))
gdp<-t(as.vector(data_m1['gdp_r']))
unemp_17<-t(as.vector(data_m1['unemp_r']))
alg<-t(as.vector(data_m1['alg']))

###############################################################################
# Model 1. Model without Facebook data
###############################################################################

cat("model {
  ##### Likelihood of data ----------------------------------------------------------------------
  
  for (i in 1:40){
    
    #data
    lfs[i]~dpois(mu.lfs[i])
    #fb[i]~dpois(mu.fb[i])
    
    mu.lfs[i]~dlnorm(m.lfs[i],tau.lfs)
    #mu.fb[i]~dlnorm(m.fb[i],tau.fb)
    
  }
  
  ##### Measurement error model ------------------------------------------------------------------
  
  for(i in 1:19) {
    m.lfs[i] <- bias.lfs[i]+log(true.stock[i])
    #m.fb[i] <- def.fb + bias.fb + alg.fb[i] +cov.fb[i] +log(true.stock[i])
  }
  
  for(i in 20:20) {
    m.lfs[i] <- bias.lfs[i]+log(true.stock[i])
    #m.fb[i] <- def.fb + bias.fb + greece_f[i] + alg.fb[i] +cov.fb[i] +log(true.stock[i])
  }
  
  
  for(i in 21:39) {
    m.lfs[i] <- bias.lfs[i]+log(true.stock[i])
    #m.fb[i] <- def.fb + bias.fb +cov.fb[i] +log(true.stock[i])
  }
  
  for(i in 40:40) {
    m.lfs[i] <- bias.lfs[i]+log(true.stock[i])
    #m.fb[i] <- def.fb + bias.fb + greece_f[i] +cov.fb[i] +log(true.stock[i])
  }
  
  
  # LFS BIAS divided into small and big countries
  for (i in 1:5) {
    bias.lfs[i]~dnorm(-0.04,100)}
  
  for (i in 6:9) {
    bias.lfs[i]~dnorm(-0.13,100)}
  
  for (i in 10:19) {
    bias.lfs[i]~dnorm(-0.35,100)}
  
  for (i in 20:20) {
    bias.lfs[i]~dnorm(-0.13,100)}
  
  for (i in 21:25) {
    bias.lfs[i]~dnorm(-0.04,100)}
  
  for (i in 26:29) {
    bias.lfs[i]~dnorm(-0.13,100)}
  
  for (i in 30:39) {
    bias.lfs[i]~dnorm(-0.35,100)}
  
  for (i in 40:40) {
    bias.lfs[i]~dnorm(-0.13,100)}
  
  #for (i in 1:10) {
  #bias.lfs[i]~dnorm(-0.35,100)}
  
  #for (i in 11:19) {
  #bias.lfs[i]~dnorm(-0.16,100)}
  
  #for (i in 20:20) {
  #bias.lfs[i]~dnorm(-0.35,100)}
  
  #for (i in 21:30) {
  #bias.lfs[i]~dnorm(-0.35,100)}
  
  #for (i in 31:39) {
  #bias.lfs[i]~dnorm(-0.16,100)}
  
  #for (i in 40:40) {
  #bias.lfs[i]~dnorm(-0.22,100)}
  # FB values
  #def.fb~dnorm(0,100) # to relax small variance
  #bias.fb~dnorm(0.04,100)
  
  #for (i in 1:20) {
  #alg.fb[i]~dnorm((alg[i]),100)}
  
  #for (i in 20:20) {
  #greece_f[i]~dnorm(-2.76,10)}
  
  #for (i in 40:40) {
  #greece_f[i]~dnorm(-3.70,10)}
  
  
  #for (i in 1:40) {
  #cov.fb[i]~dnorm(-rate[i],100)}
  
  
  ##### Migration model----------------------------------------------------------------------
  
  for (i in 1:40) {
    migmean[i]<-a0+
      a1*pop[i]+
      a2*(inflow[i])+
      a3*(outflow[i])+
      a4*gdp[i]+
      a5*unemp_17[i]
    true.stock[i]~dlnorm(migmean[i],tau.mig)
  }
  
  ##### Prior distributions -----------------------------------------------------------
  # Precision terms Changed to dgamma(100,1)
  # Gamma (100, 1) has mean 100, and variance 100
  tau.lfs~dgamma(100,1)
  #tau.fb~dgamma(100,1)
  tau.mig~dgamma(100,1)
  
  # Regression model priors
  a0~dnorm(0,0.01)
  a1~dnorm(0,1)
  a2~dnorm(0,1)
  a3~dnorm(0,1)
  a4~dnorm(0,1)
  a5~dnorm(0,1)
  
  ##### Undercount---------------------------------------------------------------------------
  #undercount<-(sum(true.stock[])-sum(lfs[]))/sum(lfs[])
  
  
  undercount18<-(sum(true.stock[1:20])-sum(lfs[1:20]))/sum(lfs[1:20])
  undercount19<-(sum(true.stock[21:40])-sum(lfs[21:40]))/sum(lfs[21:40])
  
}", file="./models/s.model#1.txt")

iterations <- 100000
burnin <- 1001
chains <- 3

model.fit.1 <- jags.model(file="./models/s.model#1.txt",
                        
                        data=list(lfs=as.vector(lfs),
                                  pop=as.vector(pop),
                                  inflow=as.vector(inflow),
                                  outflow=as.vector(outflow),
                                  gdp=as.vector(gdp),
                                  unemp_17=as.vector(unemp_17)
                        ),
                        
                        n.chains = chains)

model.samples.1 <- coda.samples(model.fit.1, c("undercount18", "undercount19","tau.mig", "tau.lfs"), n.iter=iterations, thin=10)
summary(model.samples.1)
summary1.m1<-summary(model.samples.1)$statistics
summary2.m1<-summary(model.samples.1)$quantiles
summary.m1<-cbind(summary1.m1,summary2.m1)
summary.m1<-data.frame(summary.m1)

MCMCsummary(model.samples.1)
MCMCtrace(model.samples.1,
          ISB = FALSE,
          ind=TRUE,
          Rhat=TRUE,
          pdf = FALSE,
          open_pdf = FALSE)

# For the table we are interested in the undercount
undercount.m1<-summary.m1[3:4,-c(2:4)]
undercount.m1*100

###############################################################################
# Model 2. Model with Facebook bias at 0%
###############################################################################

cat("model{
  ##### Likelihood of data ----------------------------------------------------------------------
  
  for (i in 1:40){
    
    #data
    lfs[i]~dpois(mu.lfs[i])
    fb[i]~dpois(mu.fb[i])
    
    mu.lfs[i]~dlnorm(m.lfs[i],tau.lfs)
    mu.fb[i]~dlnorm(m.fb[i],tau.fb)
    
  }
  
  ##### Measurement error model ------------------------------------------------------------------
  
  for(i in 1:19) {
    m.lfs[i] <- bias.lfs[i]+log(true.stock[i])
    m.fb[i] <- def.fb  + alg.fb[i] +cov.fb[i] +log(true.stock[i])
  }
  
  for(i in 20:20) {
    m.lfs[i] <- bias.lfs[i]+log(true.stock[i])
    m.fb[i] <- def.fb + greece_f[i] + alg.fb[i] +cov.fb[i] +log(true.stock[i])
  }
  
  
  for(i in 21:39) {
    m.lfs[i] <- bias.lfs[i]+log(true.stock[i])
    m.fb[i] <- def.fb + cov.fb[i] +log(true.stock[i])
  }
  
  for(i in 40:40) {
    m.lfs[i] <- bias.lfs[i]+log(true.stock[i])
    m.fb[i] <- def.fb + greece_f[i] +cov.fb[i] +log(true.stock[i])
  }
  
  
  # LFS BIAS divided into small and big countries
  for (i in 1:5) {
    bias.lfs[i]~dnorm(-0.04,100)}
  
  for (i in 6:9) {
    bias.lfs[i]~dnorm(-0.13,100)}
  
  for (i in 10:19) {
    bias.lfs[i]~dnorm(-0.35,100)}
  
  for (i in 20:20) {
    bias.lfs[i]~dnorm(-0.13,100)}
  
  for (i in 21:25) {
    bias.lfs[i]~dnorm(-0.04,100)}
  
  for (i in 26:29) {
    bias.lfs[i]~dnorm(-0.13,100)}
  
  for (i in 30:39) {
    bias.lfs[i]~dnorm(-0.35,100)}
  
  for (i in 40:40) {
    bias.lfs[i]~dnorm(-0.13,100)}
  
  #for (i in 1:10) {
  #bias.lfs[i]~dnorm(-0.35,100)}
  
  #for (i in 11:19) {
  #bias.lfs[i]~dnorm(-0.16,100)}
  
  #for (i in 20:20) {
  #bias.lfs[i]~dnorm(-0.35,100)}
  
  #for (i in 21:30) {
  #bias.lfs[i]~dnorm(-0.35,100)}
  
  #for (i in 31:39) {
  #bias.lfs[i]~dnorm(-0.16,100)}
  
  #for (i in 40:40) {
  #bias.lfs[i]~dnorm(-0.22,100)}
  # FB values
  def.fb~dnorm(0,100) # to relax small variance
  # Removed bias bias.fb
  
  for (i in 1:20) {
    alg.fb[i]~dnorm((alg[i]),100)}
  
  for (i in 20:20) {
    greece_f[i]~dnorm(-2.76,10)}
  
  for (i in 40:40) {
    greece_f[i]~dnorm(-3.70,10)}
  
  
  for (i in 1:40) {
    cov.fb[i]~dnorm(-rate[i],100)}
  
  
  ##### Migration model----------------------------------------------------------------------
  
  for (i in 1:40) {
    migmean[i]<-a0+
      a1*pop[i]+
      a2*(inflow[i])+
      a3*(outflow[i])+
      a4*gdp[i]+
      a5*unemp_17[i]
    true.stock[i]~dlnorm(migmean[i],tau.mig)
  }
  
  ##### Prior distributions -----------------------------------------------------------
  # Precision terms Changed to dgamma(100,1)
  # Gamma (100, 1) has mean 100, and variance 100
  tau.lfs~dgamma(100,1)
  tau.fb~dgamma(100,1)
  tau.mig~dgamma(100,1)
  
  # Regression model priors
  a0~dnorm(0,0.01)
  a1~dnorm(0,1)
  a2~dnorm(0,1)
  a3~dnorm(0,1)
  a4~dnorm(0,1)
  a5~dnorm(0,1)
  
  ##### Undercount---------------------------------------------------------------------------
  #undercount<-(sum(true.stock[])-sum(lfs[]))/sum(lfs[])
  
  
  undercount18<-(sum(true.stock[1:20])-sum(lfs[1:20]))/sum(lfs[1:20])
  undercount19<-(sum(true.stock[21:40])-sum(lfs[21:40]))/sum(lfs[21:40])
  
}", file="./models/s.model#2.txt")

iterations <- 100000
burnin <- 1001
chains <- 3

model.fit.2 <- jags.model(file="./models/s.model#2.txt",
                        data=list(lfs=as.vector(lfs),
                                  fb=as.vector(fb),
                                  pop=as.vector(pop),
                                  rate=as.vector(rate),
                                  inflow=as.vector(inflow),
                                  outflow=as.vector(outflow),
                                  gdp=as.vector(gdp),
                                  unemp_17=as.vector(unemp_17),
                                  alg=as.vector(alg)
                        ),
                        
                        n.chains = chains)

model.samples.2 <- coda.samples(model.fit.2, c("undercount18", "undercount19","tau.mig", "tau.lfs", "tau.fb"), n.iter=iterations, thin=10)
summary(model.samples.2)
summary1.m2<-summary(model.samples.2)$statistics
summary2.m2<-summary(model.samples.2)$quantiles
summary.m2<-cbind(summary1.m2,summary2.m2)
summary.m2<-data.frame(summary.m2)

MCMCsummary(model.samples.2)
MCMCtrace(model.samples.2,
          ISB = FALSE,
          ind=TRUE,
          Rhat=TRUE,
          pdf = FALSE,
          open_pdf = FALSE)

# For the table we are interested in the undercount
undercount.m2<-summary.m2[4:5,-c(2:4)]
undercount.m2*100

###############################################################################
# Model 3. Model with Facebook bias at 11% 
###############################################################################

cat("model{
  ##### Likelihood of data ----------------------------------------------------------------------
  
  for (i in 1:40){
    
    #data
    lfs[i]~dpois(mu.lfs[i])
    fb[i]~dpois(mu.fb[i])
    
    mu.lfs[i]~dlnorm(m.lfs[i],tau.lfs)
    mu.fb[i]~dlnorm(m.fb[i],tau.fb)
    
  }
  
  ##### Measurement error model ------------------------------------------------------------------
  
  for(i in 1:19) {
    m.lfs[i] <- bias.lfs[i]+log(true.stock[i])
    m.fb[i] <- def.fb + bias.fb + alg.fb[i] +cov.fb[i] +log(true.stock[i])
  }
  
  for(i in 20:20) {
    m.lfs[i] <- bias.lfs[i]+log(true.stock[i])
    m.fb[i] <- def.fb + bias.fb + greece_f[i] + alg.fb[i] +cov.fb[i] +log(true.stock[i])
  }
  
  
  for(i in 21:39) {
    m.lfs[i] <- bias.lfs[i]+log(true.stock[i])
    m.fb[i] <- def.fb + bias.fb +cov.fb[i] +log(true.stock[i])
  }
  
  for(i in 40:40) {
    m.lfs[i] <- bias.lfs[i]+log(true.stock[i])
    m.fb[i] <- def.fb + bias.fb + greece_f[i] +cov.fb[i] +log(true.stock[i])
  }
  
  
  # LFS BIAS divided into small and big countries
  for (i in 1:5) {
    bias.lfs[i]~dnorm(-0.04,100)}
  
  for (i in 6:9) {
    bias.lfs[i]~dnorm(-0.13,100)}
  
  for (i in 10:19) {
    bias.lfs[i]~dnorm(-0.35,100)}
  
  for (i in 20:20) {
    bias.lfs[i]~dnorm(-0.13,100)}
  
  for (i in 21:25) {
    bias.lfs[i]~dnorm(-0.04,100)}
  
  for (i in 26:29) {
    bias.lfs[i]~dnorm(-0.13,100)}
  
  for (i in 30:39) {
    bias.lfs[i]~dnorm(-0.35,100)}
  
  for (i in 40:40) {
    bias.lfs[i]~dnorm(-0.13,100)}
  
  #for (i in 1:10) {
  #bias.lfs[i]~dnorm(-0.35,100)}
  
  #for (i in 11:19) {
  #bias.lfs[i]~dnorm(-0.16,100)}
  
  #for (i in 20:20) {
  #bias.lfs[i]~dnorm(-0.35,100)}
  
  #for (i in 21:30) {
  #bias.lfs[i]~dnorm(-0.35,100)}
  
  #for (i in 31:39) {
  #bias.lfs[i]~dnorm(-0.16,100)}
  
  #for (i in 40:40) {
  #bias.lfs[i]~dnorm(-0.22,100)}
  # FB values
  def.fb~dnorm(0,100) # to relax small variance
  bias.fb~dnorm(0.11,100)
  
  for (i in 1:20) {
    alg.fb[i]~dnorm((alg[i]),100)}
  
  for (i in 20:20) {
    greece_f[i]~dnorm(-2.76,10)}
  
  for (i in 40:40) {
    greece_f[i]~dnorm(-3.70,10)}
  
  
  for (i in 1:40) {
    cov.fb[i]~dnorm(-rate[i],100)}
  
  
  ##### Migration model----------------------------------------------------------------------
  
  for (i in 1:40) {
    migmean[i]<-a0+
      a1*pop[i]+
      a2*(inflow[i])+
      a3*(outflow[i])+
      a4*gdp[i]+
      a5*unemp_17[i]
    true.stock[i]~dlnorm(migmean[i],tau.mig)
  }
  
  ##### Prior distributions -----------------------------------------------------------
  # Precision terms Changed to dgamma(100,1)
  # Gamma (100, 1) has mean 100, and variance 100
  tau.lfs~dgamma(100,1)
  tau.fb~dgamma(100,1)
  tau.mig~dgamma(100,1)
  
  # Regression model priors
  a0~dnorm(0,0.01)
  a1~dnorm(0,1)
  a2~dnorm(0,1)
  a3~dnorm(0,1)
  a4~dnorm(0,1)
  a5~dnorm(0,1)
  
  ##### Undercount---------------------------------------------------------------------------
  #undercount<-(sum(true.stock[])-sum(lfs[]))/sum(lfs[])
  
  
  undercount18<-(sum(true.stock[1:20])-sum(lfs[1:20]))/sum(lfs[1:20])
  undercount19<-(sum(true.stock[21:40])-sum(lfs[21:40]))/sum(lfs[21:40])
  
}", file="./models/s.model#3.txt")

iterations <- 100000
burnin <- 1001
chains <- 3

model.fit.3 <- jags.model(file="./models/s.model#3.txt",
                        
                        data=list(lfs=as.vector(lfs),
                                  fb=as.vector(fb),
                                  pop=as.vector(pop),
                                  rate=as.vector(rate),
                                  inflow=as.vector(inflow),
                                  outflow=as.vector(outflow),
                                  gdp=as.vector(gdp),
                                  unemp_17=as.vector(unemp_17),
                                  alg=as.vector(alg)
                        ),
                        
                        n.chains = chains)

model.samples.3 <- coda.samples(model.fit.3, c("undercount18", "undercount19","tau.mig", "tau.lfs", "tau.fb"), n.iter=iterations, thin=10)
summary(model.samples.3)
summary1.m3<-summary(model.samples.3)$statistics
summary2.m3<-summary(model.samples.3)$quantiles
summary.m3<-cbind(summary1.m3,summary2.m3)
summary.m3<-data.frame(summary.m3)

MCMCsummary(model.samples.3)
MCMCtrace(model.samples.3,
          ISB = FALSE,
          ind=TRUE,
          Rhat=TRUE,
          pdf = FALSE,
          open_pdf = FALSE)

# For the table we are interested in the undercount
undercount.m3<-summary.m3[4:5,-c(2:4)]
undercount.m3*100
###############################################################################
# Model 4. Model with LFS bias at 4%
###############################################################################

cat("model {
##### Likelihood of data ----------------------------------------------------------------------

for (i in 1:40){

#data
      lfs[i]~dpois(mu.lfs[i])
      fb[i]~dpois(mu.fb[i])

      mu.lfs[i]~dlnorm(m.lfs[i],tau.lfs)
      mu.fb[i]~dlnorm(m.fb[i],tau.fb)

}

##### Measurement error model ------------------------------------------------------------------

for(i in 1:19) {
      m.lfs[i] <- bias.lfs[i]+log(true.stock[i])
      m.fb[i] <- def.fb + bias.fb + alg.fb[i] +cov.fb[i] +log(true.stock[i])
}

for(i in 20:20) {
      m.lfs[i] <- bias.lfs[i]+log(true.stock[i])
      m.fb[i] <- def.fb + bias.fb + greece_f[i] + alg.fb[i] +cov.fb[i] +log(true.stock[i])
}


for(i in 21:39) {
      m.lfs[i] <- bias.lfs[i]+log(true.stock[i])
      m.fb[i] <- def.fb + bias.fb +cov.fb[i] +log(true.stock[i])
}

for(i in 40:40) {
      m.lfs[i] <- bias.lfs[i]+log(true.stock[i])
      m.fb[i] <- def.fb + bias.fb + greece_f[i] +cov.fb[i] +log(true.stock[i])
}


# LFS BIAS divided into small and big countries
    for (i in 1:40) {
    bias.lfs[i]~dnorm(-0.04,100)}



    #for (i in 1:10) {
    #bias.lfs[i]~dnorm(-0.35,100)}

    #for (i in 11:19) {
    #bias.lfs[i]~dnorm(-0.16,100)}

    #for (i in 20:20) {
    #bias.lfs[i]~dnorm(-0.35,100)}

    #for (i in 21:30) {
    #bias.lfs[i]~dnorm(-0.35,100)}

    #for (i in 31:39) {
    #bias.lfs[i]~dnorm(-0.16,100)}

    #for (i in 40:40) {
    #bias.lfs[i]~dnorm(-0.22,100)}
# FB values
  def.fb~dnorm(0,100) # to relax small variance
  bias.fb~dnorm(0.04,100)

for (i in 1:20) {
alg.fb[i]~dnorm((alg[i]),100)}

for (i in 20:20) {
greece_f[i]~dnorm(-2.76,10)}

for (i in 40:40) {
greece_f[i]~dnorm(-3.70,10)}


for (i in 1:40) {
cov.fb[i]~dnorm(-rate[i],100)}


##### Migration model----------------------------------------------------------------------

    for (i in 1:40) {
      migmean[i]<-a0+
        a1*pop[i]+
        a2*(inflow[i])+
        a3*(outflow[i])+
        a4*gdp[i]+
        a5*unemp_17[i]
      true.stock[i]~dlnorm(migmean[i],tau.mig)
    }

##### Prior distributions -----------------------------------------------------------
# Precision terms Changed to dgamma(100,1)
# Gamma (100, 1) has mean 100, and variance 100
  tau.lfs~dgamma(100,1)
  tau.fb~dgamma(100,1)
  tau.mig~dgamma(100,1)

# Regression model priors
  a0~dnorm(0,0.01)
  a1~dnorm(0,1)
  a2~dnorm(0,1)
  a3~dnorm(0,1)
  a4~dnorm(0,1)
  a5~dnorm(0,1)

##### Undercount---------------------------------------------------------------------------
#undercount<-(sum(true.stock[])-sum(lfs[]))/sum(lfs[])


undercount18<-(sum(true.stock[1:20])-sum(lfs[1:20]))/sum(lfs[1:20])
undercount19<-(sum(true.stock[21:40])-sum(lfs[21:40]))/sum(lfs[21:40])

}", file="./models/s.model#4.txt")

iterations <- 100000
burnin <- 1001
chains <- 3

model.fit.4 <- jags.model(file="./models/s.model#4.txt",
                        
                        data=list(lfs=as.vector(lfs),
                                  fb=as.vector(fb),
                                  pop=as.vector(pop),
                                  rate=as.vector(rate),
                                  inflow=as.vector(inflow),
                                  outflow=as.vector(outflow),
                                  gdp=as.vector(gdp),
                                  unemp_17=as.vector(unemp_17),
                                  alg=as.vector(alg)
                        ),
                        
                        n.chains = chains)

model.samples.4 <- coda.samples(model.fit.4, c("undercount18", "undercount19","tau.mig", "tau.lfs", "tau.fb"), n.iter=iterations, thin=10)
summary(model.samples.4)
summary1.m4<-summary(model.samples.4)$statistics
summary2.m4<-summary(model.samples.4)$quantiles
summary.m4<-cbind(summary1.m4,summary2.m4)
summary.m4<-data.frame(summary.m4)

MCMCsummary(model.samples.4)
MCMCtrace(model.samples.4,
          ISB = FALSE,
          ind=TRUE,
          Rhat=TRUE,
          pdf = FALSE,
          open_pdf = FALSE)

# For the table we are interested in the undercount
undercount.m4<-summary.m4[4:5,-c(2:4)]
undercount.m4*100
###############################################################################
# Model 5. Model with LFS bias at 30%
###############################################################################

cat("model {
##### Likelihood of data ----------------------------------------------------------------------

for (i in 1:40){

#data
      lfs[i]~dpois(mu.lfs[i])
      fb[i]~dpois(mu.fb[i])

      mu.lfs[i]~dlnorm(m.lfs[i],tau.lfs)
      mu.fb[i]~dlnorm(m.fb[i],tau.fb)

}

##### Measurement error model ------------------------------------------------------------------

for(i in 1:19) {
      m.lfs[i] <- bias.lfs[i]+log(true.stock[i])
      m.fb[i] <- def.fb + bias.fb + alg.fb[i] +cov.fb[i] +log(true.stock[i])
}

for(i in 20:20) {
      m.lfs[i] <- bias.lfs[i]+log(true.stock[i])
      m.fb[i] <- def.fb + bias.fb + greece_f[i] + alg.fb[i] +cov.fb[i] +log(true.stock[i])
}


for(i in 21:39) {
      m.lfs[i] <- bias.lfs[i]+log(true.stock[i])
      m.fb[i] <- def.fb + bias.fb +cov.fb[i] +log(true.stock[i])
}

for(i in 40:40) {
      m.lfs[i] <- bias.lfs[i]+log(true.stock[i])
      m.fb[i] <- def.fb + bias.fb + greece_f[i] +cov.fb[i] +log(true.stock[i])
}


# LFS BIAS divided into small and big countries
    for (i in 1:40) {
    bias.lfs[i]~dnorm(-0.30,100)}

    #for (i in 1:10) {
    #bias.lfs[i]~dnorm(-0.35,100)}

    #for (i in 11:19) {
    #bias.lfs[i]~dnorm(-0.16,100)}

    #for (i in 20:20) {
    #bias.lfs[i]~dnorm(-0.35,100)}

    #for (i in 21:30) {
    #bias.lfs[i]~dnorm(-0.35,100)}

    #for (i in 31:39) {
    #bias.lfs[i]~dnorm(-0.16,100)}

    #for (i in 40:40) {
    #bias.lfs[i]~dnorm(-0.22,100)}
# FB values
  def.fb~dnorm(0,100) # to relax small variance
  bias.fb~dnorm(0.04,100)

for (i in 1:20) {
alg.fb[i]~dnorm((alg[i]),100)}

for (i in 20:20) {
greece_f[i]~dnorm(-2.76,10)}

for (i in 40:40) {
greece_f[i]~dnorm(-3.70,10)}


for (i in 1:40) {
cov.fb[i]~dnorm(-rate[i],100)}


##### Migration model----------------------------------------------------------------------

    for (i in 1:40) {
      migmean[i]<-a0+
        a1*pop[i]+
        a2*(inflow[i])+
        a3*(outflow[i])+
        a4*gdp[i]+
        a5*unemp_17[i]
      true.stock[i]~dlnorm(migmean[i],tau.mig)
    }

##### Prior distributions -----------------------------------------------------------
# Precision terms Changed to dgamma(100,1)
# Gamma (100, 1) has mean 100, and variance 100
  tau.lfs~dgamma(100,1)
  tau.fb~dgamma(100,1)
  tau.mig~dgamma(100,1)

# Regression model priors
  a0~dnorm(0,0.01)
  a1~dnorm(0,1)
  a2~dnorm(0,1)
  a3~dnorm(0,1)
  a4~dnorm(0,1)
  a5~dnorm(0,1)

##### Undercount---------------------------------------------------------------------------
#undercount<-(sum(true.stock[])-sum(lfs[]))/sum(lfs[])


undercount18<-(sum(true.stock[1:20])-sum(lfs[1:20]))/sum(lfs[1:20])
undercount19<-(sum(true.stock[21:40])-sum(lfs[21:40]))/sum(lfs[21:40])

}", file="./models/s.model#5.txt")

iterations <- 100000
burnin <- 1001
chains <- 3

model.fit.5 <- jags.model(file="./models/s.model#5.txt",
                        
                        data=list(lfs=as.vector(lfs),
                                  fb=as.vector(fb),
                                  pop=as.vector(pop),
                                  rate=as.vector(rate),
                                  inflow=as.vector(inflow),
                                  outflow=as.vector(outflow),
                                  gdp=as.vector(gdp),
                                  unemp_17=as.vector(unemp_17),
                                  alg=as.vector(alg)
                        ),
                        
                        n.chains = chains)

model.samples.5 <- coda.samples(model.fit.5, c("undercount18", "undercount19","tau.mig", "tau.lfs", "tau.fb"), n.iter=iterations, thin=10)
summary(model.samples.5)
summary1.m5<-summary(model.samples.5)$statistics
summary2.m5<-summary(model.samples.5)$quantiles
summary.m5<-cbind(summary1.m5,summary2.m5)
summary.m5<-data.frame(summary.m5)

MCMCsummary(model.samples.5)
MCMCtrace(model.samples.5,
          ISB = FALSE,
          ind=TRUE,
          Rhat=TRUE,
          pdf = FALSE,
          open_pdf = FALSE)

# For the table we are interested in the undercount
undercount.m5<-summary.m5[4:5,-c(2:4)]
undercount.m5*100
###############################################################################
# Model 6. Model with Gamma(1,1)
###############################################################################

cat("model {
##### Likelihood of data ----------------------------------------------------------------------

for (i in 1:40){

#data
      lfs[i]~dpois(mu.lfs[i])
      fb[i]~dpois(mu.fb[i])

      mu.lfs[i]~dlnorm(m.lfs[i],tau.lfs)
      mu.fb[i]~dlnorm(m.fb[i],tau.fb)

}

##### Measurement error model ------------------------------------------------------------------

for(i in 1:19) {
      m.lfs[i] <- bias.lfs[i]+log(true.stock[i])
      m.fb[i] <- def.fb + bias.fb + alg.fb[i] +cov.fb[i] +log(true.stock[i])
}

for(i in 20:20) {
      m.lfs[i] <- bias.lfs[i]+log(true.stock[i])
      m.fb[i] <- def.fb + bias.fb + greece_f[i] + alg.fb[i] +cov.fb[i] +log(true.stock[i])
}


for(i in 21:39) {
      m.lfs[i] <- bias.lfs[i]+log(true.stock[i])
      m.fb[i] <- def.fb + bias.fb +cov.fb[i] +log(true.stock[i])
}

for(i in 40:40) {
      m.lfs[i] <- bias.lfs[i]+log(true.stock[i])
      m.fb[i] <- def.fb + bias.fb + greece_f[i] +cov.fb[i] +log(true.stock[i])
}


# LFS BIAS divided into small and big countries
    for (i in 1:5) {
    bias.lfs[i]~dnorm(-0.04,100)}

    for (i in 6:9) {
    bias.lfs[i]~dnorm(-0.13,100)}

    for (i in 10:19) {
    bias.lfs[i]~dnorm(-0.35,100)}

    for (i in 20:20) {
    bias.lfs[i]~dnorm(-0.13,100)}

    for (i in 21:25) {
    bias.lfs[i]~dnorm(-0.04,100)}

    for (i in 26:29) {
    bias.lfs[i]~dnorm(-0.13,100)}

    for (i in 30:39) {
    bias.lfs[i]~dnorm(-0.35,100)}

    for (i in 40:40) {
    bias.lfs[i]~dnorm(-0.13,100)}

    #for (i in 1:10) {
    #bias.lfs[i]~dnorm(-0.35,100)}

    #for (i in 11:19) {
    #bias.lfs[i]~dnorm(-0.16,100)}

    #for (i in 20:20) {
    #bias.lfs[i]~dnorm(-0.35,100)}

    #for (i in 21:30) {
    #bias.lfs[i]~dnorm(-0.35,100)}

    #for (i in 31:39) {
    #bias.lfs[i]~dnorm(-0.16,100)}

    #for (i in 40:40) {
    #bias.lfs[i]~dnorm(-0.22,100)}
# FB values
  def.fb~dnorm(0,100) # to relax small variance
  bias.fb~dnorm(0.04,100)

for (i in 1:20) {
alg.fb[i]~dnorm((alg[i]),100)}

for (i in 20:20) {
greece_f[i]~dnorm(-2.76,10)}

for (i in 40:40) {
greece_f[i]~dnorm(-3.70,10)}


for (i in 1:40) {
cov.fb[i]~dnorm(-rate[i],100)}


##### Migration model----------------------------------------------------------------------

    for (i in 1:40) {
      migmean[i]<-a0+
        a1*pop[i]+
        a2*(inflow[i])+
        a3*(outflow[i])+
        a4*gdp[i]+
        a5*unemp_17[i]
      true.stock[i]~dlnorm(migmean[i],tau.mig)
    }

##### Prior distributions -----------------------------------------------------------
# Gamma (1,1) 
  tau.lfs~dgamma(1,1)
  tau.fb~dgamma(1,1)
  tau.mig~dgamma(1,1)

# Regression model priors
  a0~dnorm(0,0.01)
  a1~dnorm(0,1)
  a2~dnorm(0,1)
  a3~dnorm(0,1)
  a4~dnorm(0,1)
  a5~dnorm(0,1)

##### Undercount---------------------------------------------------------------------------
#undercount<-(sum(true.stock[])-sum(lfs[]))/sum(lfs[])


undercount18<-(sum(true.stock[1:20])-sum(lfs[1:20]))/sum(lfs[1:20])
undercount19<-(sum(true.stock[21:40])-sum(lfs[21:40]))/sum(lfs[21:40])

}", file="./models/s.model#6.txt")

iterations <- 100000
burnin <- 1001
chains <- 3

model.fit.6 <- jags.model(file="./models/s.model#6.txt",
                        
                        data=list(lfs=as.vector(lfs),
                                  fb=as.vector(fb),
                                  pop=as.vector(pop),
                                  rate=as.vector(rate),
                                  inflow=as.vector(inflow),
                                  outflow=as.vector(outflow),
                                  gdp=as.vector(gdp),
                                  unemp_17=as.vector(unemp_17),
                                  alg=as.vector(alg)
                        ),
                        
                        n.chains = chains)

model.samples.6 <- coda.samples(model.fit.6, c("undercount18", "undercount19","tau.mig", "tau.lfs", "tau.fb"), n.iter=iterations, thin=10)
summary(model.samples.6)
summary1.m6<-summary(model.samples.6)$statistics
summary2.m6<-summary(model.samples.6)$quantiles
summary.m6<-cbind(summary1.m6,summary2.m6)
summary.m6<-data.frame(summary.m6)

MCMCsummary(model.samples.6)
MCMCtrace(model.samples.6,
          ISB = FALSE,
          ind=TRUE,
          Rhat=TRUE,
          pdf = FALSE,
          open_pdf = FALSE)

# For the table we are interested in the undercount
undercount.m6<-summary.m6[4:5,-c(2:4)]
undercount.m6*100

#################################################################################
#################################################################################
#################################################################################
# To produce the table with all the undercounts for the different models
undercount <- rbind(undercount.m1, undercount.m2, undercount.m3, undercount.m4, undercount.m5, undercount.m6)
undercount
