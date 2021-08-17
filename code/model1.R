# Model 1 is the model on the totals (no sex disaggregation)

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
# Model
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

}
", file="./models/model1.txt")

iterations <- 100000
burnin <- 1001
chains <- 3

model.fit <- jags.model(file="./models/model1.txt",
                        data=list(lfs=as.vector(lfs),
                                  fb=as.vector(fb),
                                  pop=as.vector(pop),
                                  rate=as.vector(rate),
                                  inflow=as.vector(inflow),
                                  outflow=as.vector(outflow),
                                  gdp=as.vector(gdp),
                                  unemp_17=as.vector(unemp_17),
                                  alg=as.vector(alg)
                        ),n.chains = chains)

# This code will run the model (it should take around 5 minutes)
model.samples <- coda.samples(model.fit, c("true.stock", "def.fb", "bias.lfs", "bias.fb", "cov.fb", "a0","a1", "a2","a3", "a4", "a5", "greece_f","undercount18", "undercount19"), n.iter=iterations, thin=10)
# Check the summary of the model
summary(model.samples)
# Select the statistics of interests
summary1<-summary(model.samples)$statistics
summary2<-summary(model.samples)$quantiles
summary<-cbind(summary1,summary2)
summary<-data.frame(summary)

# Check for the traceplots 
MCMCsummary(model.samples)
MCMCtrace(model.samples,
          ISB = FALSE,
          ind=TRUE,
          Rhat=TRUE,
          pdf = TRUE,
          open_pdf = FALSE,
          filename = 'model1_traceplots',
          wd = './outcome/')

# Save summaries for statistics in the Appendix
mcmcsum<-MCMCsummary(model.samples)
mcmcsum <- mcmcsum %>% select(Rhat, n.eff)
summary_save<-cbind(summary,mcmcsum)
write.csv(summary_save, "./outcome/summary_model1.csv")

truestock_save<-summary_save[91:130,-c(1:4)]
data_save<-cbind(data_m1, truestock_save)
data_save<- data_save%>% arrange(-X50.) %>%
  mutate(Country=country, '2.5%'=round(X2.5.),'25%'=round(X25.),'50%'=round(X50.),'75%'=round(X75.),'97.5%'=round(X97.5.))%>%
  select(Country,year, '2.5%', '25%', '50%','75%','97.5%', Rhat, n.eff)


write.csv(data_save, "./outcome/summary_truestock_model1.csv")

###############################################################################
# Undercount of the LFS estimates
###############################################################################
undercount<-summary[131:132,-c(2:4)]
undercount*100
# The median should be around 25% dor 2018, and 20% for 2019

###############################################################################
# Plot of some of the results
###############################################################################
truestock<-summary[91:130,-c(2:4)]
data<-cbind(data_m1, truestock)
mod1<- data %>% select(country, Mean, X2.5., X25., X50.,X75.,X97.5.)
mod1 <- mod1 %>% mutate(source="mod1", Gender="All", Age="All")

# Change confidence interval
data$st_err<-(data$confidence/1.96)
data$confidence_50<-(data$st_err*0.674)
data1<-select(data, country, lfs, confidence_50) %>% mutate(y=lfs, ymin=lfs-confidence_50, ymax=lfs+confidence_50, source="LFS", year="2018") %>% select( country, y, ymax, ymin, source, year)
data2<-select(data, country, lfs) %>% mutate(y=fb, ymin=fb, ymax=fb, source="Facebook", year="2018") %>% select( country, y, ymax, ymin, source, year)
data3<-select(data, country, X50., X25., X75.) %>% mutate(y=X50., ymin=X25., ymax=X75., source="Model Estimates", year="2018") %>% select( country, y, ymax, ymin, source, year)
data<-rbind(data1, data2, data3)
# Include interquintile
data<-cbind(data_m1, truestock)
# change confidence interval
data$st_err<-(data$confidence/1.96)
data$confidence_50<-(data$st_err*0.674)
data1_bis<- data %>% mutate(y=lfs, `2.5%`=lfs-confidence, `25%`=lfs-confidence_50, `75%`=lfs+confidence_50, `97.5%`=lfs+confidence, source="LFS") %>% select( country, y,`2.5%`, `25%`, `75%`, `97.5%`, source, year)
data2_bis<- data %>% mutate(y=fb, `2.5%`=fb,`25%`=fb, `75%`=fb, `97.5%`=fb, source="Facebook") %>% select( country, y, `2.5%`, `25%`, `75%`, `97.5%`, source, year)
data3_bis<- data %>% mutate(y=X50., `2.5%`=X2.5.,`25%`=X25., `75%`=X75., `97.5%`=X97.5., source="Model Estimates") %>% select( country, y, `2.5%`, `25%`, `75%`, `97.5%`, source, year)
data_bis<-rbind(data1_bis, data2_bis, data3_bis)
# Save the data
write.csv(data_bis, "./Outcome/model1.csv")

#########################################################################
# Read data from EU settlement scheme
#########################################################################
eu_settle<-read_excel("./data/eu-settlement-scheme-statistics-december-2019.xls",
                      sheet = 4,
                      range = "A29:B61")
# Change Column names
eu_settle <- eu_settle %>% mutate(country=EU27, source="settled", year="2019",  y=`2597300`, `2.5%`=`2597300`, `25%`=`2597300`, `75%`=`2597300`, `97.5%`=`2597300`) %>% select(country, source, year, y, `2.5%`, `25%`, `75%`, `97.5%`)

eu_settle <- eu_settle %>% filter(#country=="Luxembourg"|
  #country=="Estonia"|
  country=="Austria"|
    country=="Finland"|
    country=="Denmark"|
    country=="Belgium"|
    country=="Czech Republic"|
    country=="Sweden"|
    country=="Slovakia"|
    country=="Greece"|
    country=="Netherlands"|
    country=="Hungary"|
    country=="Latvia"|
    country=="Spain"|
    country=="Portugal"|
    country=="France"|
    country=="Lithuania"|
    country=="Italy"|
    country=="Germany"|
    country=="Ireland"|
    country=="Romania"|
    country=="Poland")


#########################################################################
# Combine data from the model and EU settlement status 
data2<-rbind(data_bis, eu_settle)

data2$y<-as.numeric(data2$y)
data2$`2.5%`<-as.numeric(data2$`2.5%`)
data2$`25%`<-as.numeric(data2$`25%`)
data2$`75%`<-as.numeric(data2$`75%`)
data2$`97.5%`<-as.numeric(data2$`97.5%`)

data2 <-data2 %>% filter(country!="Slovenia")
data2 <-data2 %>% filter(country!="Estonia")

# Divide the data 
data3 <- data2 %>% filter(country=="Latvia"|country=="Spain"|country=="Portugal"| country=="France"|
                            country=="Lithuania"|country=="Italy"|country=="Germany"|country=="Ireland"|country=="Romania"|
                            country=="Poland") %>% mutate(group="Group 1")

data3<- data3 %>% mutate(stratum = interaction(year, source))
data3$country<-as.factor(data3$country)
data4 <- data2 %>% filter(country=="Luxembourg"|
                            country=="Estonia"|
                            country=="Austria"|
                            country=="Finland"|
                            country=="Denmark"|
                            country=="Belgium"|
                            country=="Czech Republic"|
                            country=="Sweden"|
                            country=="Slovakia"|
                            country=="Greece"|
                            country=="Netherlands"|
                            country=="Hungary") %>% mutate(group="Group 2")

data4<- data4 %>% mutate(stratum = interaction(year, source))
data_group <- rbind(data3, data4)
d=data.frame(x=c(1.5, 2.5,3.5, 4.5,5.5, 6.5,7.5, 8.5, 9.5), y=c(1.5, 2.5,3.5, 4.5,5.5, 6.5, 7.5, 8.5, 9.5))


pdf(file = "./outcome/model.pdf",   # The directory you want to save the file in
    width = 18, # The width of the plot in inches
    height = 12)
options(scipen = 999)

data_group %>%
  mutate(country = fct_reorder(country, y),
         stratum = interaction(year, source)) %>%
  mutate(stratum = factor(stratum, levels = c("2018.Facebook", "2018.LFS", "2018.Model Estimates","2018.settled",
                                              "2019.Facebook", "2019.LFS", "2019.Model Estimates","2019.settled")))%>%
  ggplot(aes(country, y)) +
  geom_pointrange(aes(y = y, ymin = `2.5%`, ymax = `97.5%`, x=country,group=stratum,shape=year,
                      colour=source),alpha = 0.7,size=0.5,
                  position = position_dodge(0.7))+
  geom_pointrange(aes(ymin = `25%`, ymax = `75%`, x=country, group=stratum,shape=year,
                      colour=source),
                  size=1,
                  position = position_dodge(0.7))+
  coord_flip()+
  geom_vline(data=d, mapping=aes(xintercept=x), color="grey70", linetype="dashed") +
  scale_color_manual(values = c("#3b5998", "#CC9933", "#006600", "red4"),
                     labels=c("Facebook", "Labour Force Survey", "Model Estimates","EU Settled Applications Received"),
                     name="Data Sources",
                     guide=guide_legend(title.position = "top", title.hjust = 0.5 ))+
  scale_shape_manual(values=c(16, 15),
                     labels=c("2018","2019"),
                     name  ="Year",
                     guide=guide_legend(title.position = "top", title.hjust = 0.5 ))+
  guides(
    color = guide_legend(title.position = "top", title.hjust = 0.5,
                         override.aes = list(shape=18, size=2, alpha = 0.8)),
    shape = guide_legend(title.position = "top", title.hjust = 0.5,
                         override.aes = list(size=1.5, alpha = 0.8)))+
  theme_minimal()+
  xlab("") + ylab("Absolute Numbers")+
  theme(axis.text.x = element_text(size=20),
        axis.title.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        legend.text=element_text(size=20),
        legend.title=element_text(size=20),
        plot.title = element_text(size = 20, hjust =0.5),
        legend.position="top",
        legend.box="horizontal",
        plot.caption = element_text(size=20),
        strip.background = element_blank(),
        strip.text.x = element_blank())+
  labs(caption = "LFS 95% C.I., Model Estimations with IQR.")+
  facet_wrap(~group, scales="free")
dev.off()
