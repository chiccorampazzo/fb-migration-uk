# Model 2 is disaggregated by sex

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
library(scales)

###############################################################################
# Data Preparation
###############################################################################

# Data 2018 and 2019
data_m2_18 <- read_csv("./data/data_m2_2018.csv")
data_m2_18 <- data_m2_18 %>% mutate(year="2018")
data_m2_19 <- read_csv("./data/data_m2_2019.csv")
data_m2_19 <- data_m2_19 %>% mutate(year="2019")
# order
data_m2_18$country <- as.factor(data_m2_18$country)
data_m2_18$country <- ordered(data_m2_18$country, levels=c("Austria", "Belgium", "Czech Republic", "Latvia", "Sweden",
                                                           "France", "Germany", "Hungary", "Lithuania",
                                                           "Denmark", "Finland", "Ireland", "Italy", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Spain",
                                                           "Greece"))
data_m2_18<-with(data_m2_18, data_m2_18[order(country),])
data_m2_19$country <- as.factor(data_m2_19$country)
data_m2_19$country <- ordered(data_m2_19$country, levels=c("Austria", "Belgium", "Czech Republic", "Latvia", "Sweden",
                                                           "France", "Germany", "Hungary", "Lithuania",
                                                           "Denmark", "Finland", "Ireland", "Italy", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Spain",
                                                           "Greece"))
data_m2_19<-with(data_m2_19, data_m2_19[order(country),])


# Natural Log
data_m2_18$rate<-log(1-data_m2_18$rate)
data_m2_18$alg<-log(1-data_m2_18$alg)
data_m2_19$rate<-log(1-data_m2_19$rate)

data_m2_19 <- data_m2_19 %>% mutate(alg=NA)
# Change names
data_m2_18 <- data_m2_18 %>% mutate(fbpop=fbpop_19, unemp=unemp_17)
data_m2_19 <- data_m2_19 %>% mutate(fbpop=fbpop_20, unemp=unemp_18)

data_m2_18 <- data_m2_18  %>% select(country, lfs, gender, confidence, fb, pop_r, rate, alg, inflow_r,
                                     outflow_r, gdp_r, unemp_r, year)
data_m2_19 <- data_m2_19  %>% select(country, lfs, gender, confidence, fb, pop_r, rate, alg, inflow_r,
                                     outflow_r, gdp_r, unemp_r, year)
# Merge the two datasets for the two years
data_m2<- rbind(data_m2_18, data_m2_19)


# Female and Male Separation
f <- data_m2 %>% filter(gender=="Female")
m <- data_m2 %>% filter(gender=="Male")
# LFS
lfs_f<- f %>% select(lfs)
lfs_m<- m %>% select(lfs)
lfs<-cbind(lfs_f, lfs_m)
lfs<-as.matrix(t(lfs))
str(lfs)
# FB
fb_f<- f %>% select(fb)
fb_m<- m %>% select(fb)
fb<-cbind(fb_f, fb_m)
fb<-as.matrix(t(fb))
str(fb)
# Pop
pop_f<- f %>% select(pop_r)
pop_m<- m %>% select(pop_r)
pop<-cbind(pop_f, pop_m)
pop<-as.matrix(t(pop))
str(pop)
# Rate
rate_f<- f %>% select(rate)
rate_m<- m %>% select(rate)
rate<-cbind(rate_f, rate_m)
rate<-as.matrix(t(rate))
str(rate)
# Inflow
inflow_f<- f %>% select(inflow_r)
inflow_m<- m %>% select(inflow_r)
inflow<-cbind(inflow_f, inflow_m)
inflow<-as.matrix(t(inflow))
str(inflow)
# Outflow
outflow_f<- f %>% select(outflow_r)
outflow_m<- m %>% select(outflow_r)
outflow<-cbind(outflow_f, outflow_m)
outflow<-as.matrix(t(outflow))
str(outflow)
# GDP
gdp_f<- f %>% select(gdp_r)
gdp_m<- m %>% select(gdp_r)
gdp<-cbind(gdp_f, gdp_m)
gdp<-as.matrix(t(gdp))
str(gdp)
# Unemployment
unemp_f<- f %>% select(unemp_r)
unemp_m<- m %>% select(unemp_r)
unemp<-cbind(unemp_f, unemp_m)
unemp<-as.matrix(t(unemp))
str(unemp)
# Algorithm
alg_f<- f %>% select(alg)
alg_m<- m %>% select(alg)
alg<-cbind(alg_f, alg_m)
alg<-as.matrix(t(alg))
str(alg)

###############################################################################
# Model
###############################################################################

cat("model {

##### Likelihood of data ----------------------------------------------------------------------

    for (t in 1:N.sex) {
    for (i in 1:N.country){

      mu.lfs[t,i]~dlnorm(m.lfs[t,i],tau.lfs)
      mu.fb[t,i]~dlnorm(m.fb[t,i],tau.fb)

      lfs[t,i]~dpois(mu.lfs[t,i])
      fb[t,i]~dpois(mu.fb[t,i])
    }}


##### Measurement error model ------------------------------------------------------------------

    for (t in 1:N.sex) {
    for(i in 1:19) {
      m.lfs[t,i] <- bias.lfs[t,i]+log(true.stock[t,i])
      m.fb[t,i] <- def.fb + bias.fb + alg.fb[t,i] +cov.fb[t,i]+log(true.stock[t,i])
    }}

    for (t in 1:N.sex) {
    for(i in 20:20) {
      m.lfs[t,i] <- bias.lfs[t,i]+log(true.stock[t,i])
      m.fb[t,i] <- def.fb + bias.fb +  greece_f[t,i] +cov.fb[t,i]+log(true.stock[t,i])
    }}

        for (t in 1:N.sex) {
    for(i in 21:39) {
      m.lfs[t,i] <- bias.lfs[t,i]+log(true.stock[t,i])
      m.fb[t,i] <- def.fb + bias.fb  +cov.fb[t,i]+log(true.stock[t,i])
    }}

    for (t in 1:N.sex) {
    for(i in 40:40) {
      m.lfs[t,i] <- bias.lfs[t,i]+log(true.stock[t,i])
      m.fb[t,i] <- def.fb + bias.fb + greece_f[t,i] +cov.fb[t,i]+log(true.stock[t,i])
    }}
    

# LFS BIAS divided into small and big countries
    for (t in 1:N.sex) {
    for (i in 1:5) {
    bias.lfs[t,i]~dnorm(-0.04,100)}}
    for (t in 1:N.sex) {
    for (i in 6:9) {
    bias.lfs[t,i]~dnorm(-0.13,100)}}
    for (t in 1:N.sex) {
    for (i in 10:19) {
    bias.lfs[t,i]~dnorm(-0.35,100)}}
    for (t in 1:N.sex) {
    for (i in 20:20) {
    bias.lfs[t,i]~dnorm(-0.13,100)}}

    for (t in 1:N.sex) {
    for (i in 21:25) {
    bias.lfs[t,i]~dnorm(-0.04,100)}}
    for (t in 1:N.sex) {
    for (i in 26:29) {
    bias.lfs[t,i]~dnorm(-0.13,100)}}
    for (t in 1:N.sex) {
    for (i in 30:39) {
    bias.lfs[t,i]~dnorm(-0.35,100)}}
    for (t in 1:N.sex) {
    for (i in 40:40) {
    bias.lfs[t,i]~dnorm(-0.13,100)}}

# FB values
  def.fb~dnorm(0,100) # to relax small variance
  bias.fb~dnorm(0.04,100)

  for (t in 1:N.sex) {
  for (i in 1:20) {

  alg.fb[t,i]~dnorm((alg[t,i]),100)
  }}


  for (t in 1:N.sex) {
  for (i in 1:N.country) {

  cov.fb[t,i]~dnorm(-((rate[t,i])),100)
  }}

  # unemployment
  for (t in 1:N.sex){
  for (i in 1:N.country) {
  unemp_17[t,i]~dlnorm(0,100)
  }}

  # greece
  for (t in 1:N.sex) {
  for (i in 20:20) {
  greece_f[t,i]~dnorm(-2.76,10)}
  }

  for (t in 1:N.sex) {
  for (i in 40:40) {
  greece_f[t,i]~dnorm(-2.76,10)}
  }


##### Migration model----------------------------------------------------------------------

    for (t in 1:N.sex) {
    for (i in 1:N.country) {
      migmean[t,i]<-a0+
        a1*pop[t,i]+
        a2*inflow[t,i]+
        a3*outflow[t,i]+
        a4*gdp[t,i]+
        a5*unemp[t,i]
      true.stock[t,i]~dlnorm(migmean[t,i],tau.mig)
    }}

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

##### Sum of the stock by Sex --------------------------------------------------------
  for (i in 1:N.country){
  sum.stock[i]<-(true.stock[1,i]+true.stock[2,i])}
}", file="./models/model2.txt")


iterations <- 1000000
burnin <- 1001
chains <- 3

model.fit <- jags.model(file="./models/model2.txt",
                        data=list(lfs=(lfs),
                                  fb=(fb),
                                  pop=(pop),
                                  rate=(rate),
                                  inflow=(inflow),
                                  outflow=(outflow),
                                  gdp=(gdp),
                                  unemp=(unemp),
                                  alg=(alg),
                                  N.country=ncol(lfs),
                                  N.sex=nrow(lfs)),
                        n.chains = chains)

# This code will run the model
# It should take around 30 minutes
model.samples <- coda.samples(model.fit, c("true.stock","def.fb", "bias.lfs", "bias.fb", "cov.fb", "a0","a1", "a2","a3", "a4", "a5", "greece_f", "sum.stock"),
                              n.iter=iterations,
                              thin=10)
# Check the summary of the model
summary(model.samples)
# Select the statistics of interests
summary1<-summary(model.samples)$statistics
summary2<-summary(model.samples)$quantiles
summary<-cbind(summary1,summary2)
summary<-data.frame(summary)
summary<-summary[-c(2:4)]

MCMCsummary(model.samples)
MCMCtrace(model.samples,
          ISB = FALSE,
          ind=TRUE,
          Rhat=TRUE,
          pdf = TRUE,
          open_pdf = FALSE,
          filename = 'model2_traceplots',
          wd = './outcome/')

# Save summaries for statistics in the Appendix
mcmcsum<-MCMCsummary(model.samples)
mcmcsum <- mcmcsum %>% select(Rhat, n.eff)
summary_save<-cbind(summary,mcmcsum )
write.csv(summary_save, "./outcome/summary_model2.csv")

###############################################################################
# Plot of some of the results
###############################################################################

truestock_save<-summary_save[213:292,c(2:8)]
data_save<-cbind(data_m2, truestock_save)

data_save<- data_save%>% arrange(-X50.) %>%
  mutate(Country=country, Gender=gender, '2.5%'=round(X2.5.),'25%'=round(X25.),'50%'=round(X50.),'75%'=round(X75.),'97.5%'=round(X97.5.))%>%
  select(Country, Gender, '2.5%', '25%', '50%','75%','97.5%', Rhat, n.eff, year)

data_save_m <- data_save %>% filter(Gender=="Male")
data_save_f <- data_save %>% filter(Gender=="Female")

# To save the data
#write.csv(data_save_m, "~/Desktop/RC1/Outcome/summary_truestock2_yearsMALE.csv")
#write.csv(data_save_f, "~/Desktop/RC1/Outcome/summary_truestock2_yearsFEMALE.csv")

sum_stock_save<-summary_save[173:212,c(2:8)]
country<-c("Austria", "Belgium", "Czech Republic", "Latvia", "Sweden",
           "France", "Germany", "Hungary", "Lithuania",
           "Denmark", "Finland", "Ireland", "Italy", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Spain",
           "Greece")
sum_stock_save<-cbind(country, sum_stock_save)

sum_stock_save$year <- rep(c("2018","2019"),c(20,20))

sum_stock_save<- sum_stock_save%>% arrange(-X50.) %>%
  mutate(Country=country, '2.5%'=round(X2.5.),'25%'=round(X25.),'50%'=round(X50.),'75%'=round(X75.),'97.5%'=round(X97.5.),
         source="Model 2")%>%
  select(Country, '2.5%', '25%', '50%','75%','97.5%', year, source)
# Saving also this results
write.csv(sum_stock_save, "./outcome/model2_st.csv")


truestock<-truestock_save[,c(1:5)]
data<-cbind(data_m2, truestock)

data$year <- rep(c("2018","2019"),c(20,20))

# change confidence interval
data$st_err<-(data$confidence/1.96)
data$confidence_50<-(data$st_err*0.674)


data1<-select(data, country, gender, lfs, confidence_50, year) %>% mutate(y=lfs, ymin=(lfs-confidence_50), ymax=(lfs+confidence_50), source="LFS") %>% select(country, gender, y, ymax, ymin, source, year)
data2<-select(data, country, gender, fb, year) %>% mutate(y=fb, ymin=fb, ymax=fb, source="FB") %>% select(country, gender, y, ymax, ymin, source, year)
data3<-select(data, country, gender, X50., X25., X75., year) %>% mutate(y=X50., ymin=X25., ymax=X75., source="Estimate") %>% select( country, gender, y, ymax, ymin, source, year)
data<-rbind(data1, data2, data3)
options("scipen" = 10)

write.csv(data, "./outcome/model2.csv")


# with all interquintile
data<-cbind(data_m2, truestock)
data$year <- rep(c("2018","2019"),c(40,40))
# change confidence interval
data$st_err<-(data$confidence/1.96)
data$confidence_50<-(data$st_err*0.674)
data1_bis<- data %>% mutate(y=lfs, `2.5%`=lfs-confidence, `25%`=lfs-confidence_50, `75%`=lfs+confidence_50, `97.5%`=lfs+confidence, source="LFS") %>% select(country, gender, y, `2.5%`, `25%`, `75%`, `97.5%`, source, year)
data2_bis<- data %>% mutate(y=fb, `2.5%`=fb,`25%`=fb, `75%`=fb, `97.5%`=fb, source="FB") %>% select( country, gender, y, `2.5%`, `25%`, `75%`, `97.5%`, source, year)
data3_bis<- data %>% mutate(y=X50., `2.5%`=X2.5.,`25%`=X25., `75%`=X75., `97.5%`=X97.5., source="Estimate") %>% select( country, gender, y, `2.5%`, `25%`, `75%`, `97.5%`, source, year)
data_bis<-rbind(data1_bis, data2_bis, data3_bis)

#write.csv(data_bis, "~/Desktop/RC1/2018/Outcome/model2_18_bisyears.csv")

data2 <- data_bis %>% filter(country=="Latvia"|country=="Spain"|country=="Portugal"| country=="France"|
                               country=="Lithuania"|country=="Italy"|country=="Germany"|country=="Ireland"|country=="Romania"|
                               country=="Poland") %>% mutate(group="Group 1")

data2 %>%
  mutate(country = fct_reorder(country, y),
         stratum = interaction(year, source)) %>%
  mutate(stratum = factor(stratum, levels = c("2018.FB", "2018.LFS", "2018.Estimate",
                                              "2019.FB", "2019.LFS", "2019.Estimate")))%>%
  ggplot(aes(country, y)) +
  geom_pointrange(aes(y = y, ymin = `2.5%`, ymax = `97.5%`, x=country,group=stratum,shape=year,
                      colour=source),alpha = 0.7,size=0.5,
                  position = position_dodge(0.7))+
  geom_pointrange(aes(ymin = `25%`, ymax = `75%`, x=country, group=stratum,shape=year,
                      colour=source),
                  size=1,
                  position = position_dodge(0.7))+
  coord_flip()+
  scale_color_manual(values = c( "#006600","#3b5998", "#CC9933"),
                     labels=c("Model Estimates", "Facebook", "Labour Force Survey"),
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
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12),
        legend.title=element_text(size=14),
        plot.title = element_text(size = 14, hjust =0.5),
        legend.position="top",
        legend.box="horizontal",
        #strip.background = element_blank(),
        strip.text.x = element_text(size = 14))+
  labs(caption = "LFS 95% C.I., Model Estimations with IQR.")+
  facet_wrap(~gender, ncol=2)




data3 <- data_bis %>% filter(country=="Luxembourg"|
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

data3 %>%
  mutate(country = fct_reorder(country, y),
         stratum = interaction(year, source)) %>%
  mutate(stratum = factor(stratum, levels = c("2018.FB", "2018.LFS", "2018.Estimate",
                                              "2019.FB", "2019.LFS", "2019.Estimate")))%>%
  ggplot(aes(country, y)) +
  geom_pointrange(aes(y = y, ymin = `2.5%`, ymax = `97.5%`, x=country,group=stratum,shape=year,
                      colour=source),alpha = 0.7,size=0.5,
                  position = position_dodge(0.7))+
  geom_pointrange(aes(ymin = `25%`, ymax = `75%`, x=country, group=stratum,shape=year,
                      colour=source),
                  size=1,
                  position = position_dodge(0.7))+
  coord_flip()+
  scale_color_manual(values = c( "#006600","#3b5998", "#CC9933"),
                     labels=c("Model Estimates", "Facebook", "Labour Force Survey"),
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
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12),
        legend.title=element_text(size=14),
        plot.title = element_text(size = 14, hjust =0.5),
        legend.position="top",
        legend.box="horizontal",
        #strip.background = element_blank(),
        strip.text.x = element_text(size = 14))+
  labs(caption = "LFS 95% C.I., Model Estimations with IQR.")+
  facet_wrap(~gender, ncol=2)

data_group <- rbind(data2, data3)
dline=data.frame(x=c(1.5, 2.5,3.5, 4.5,5.5, 6.5,7.5, 8.5, 9.5), y=c(1.5, 2.5,3.5, 4.5,5.5, 6.5, 7.5, 8.5, 9.5))

pdf(file = "./outcome/model2.pdf",   # The directory you want to save the file in
    width = 24, # The width of the plot in inches
    height = 18)

options(scipen = 999)

sex <- c("Female","Female", "Male", "Male")
names(sex) <- c("Female.Group 1", "Female.Group 2", "Male.Group 1","Male.Group 2")

d<-data_group %>%
  mutate(country = fct_reorder(country, y),
         stratum = interaction(year, source),
         square= interaction(gender, group),
         square2= interaction(gender, group)) %>%
  mutate(stratum = factor(stratum, levels = c("2018.FB", "2018.LFS", "2018.Estimate",
                                              "2019.FB", "2019.LFS", "2019.Estimate")))%>%
  mutate(square2=recode(square2, "Female.Group 1"="Female", "Female.Group 2"="Female",
                        "Male.Group 1"="Male", "Male.Group 2"="Male"))


d %>% ggplot(aes(country, y)) +
  geom_pointrange(aes(y = y, ymin = `2.5%`, ymax = `97.5%`, x=country,group=stratum,shape=year,
                      colour=source),alpha = 0.7,size=0.5,
                  position = position_dodge(0.7))+
  geom_pointrange(aes(ymin = `25%`, ymax = `75%`, x=country, group=stratum,shape=year,
                      colour=source),
                  size=1,
                  position = position_dodge(0.7))+
  coord_flip()+
  geom_vline(data=dline, mapping=aes(xintercept=x), color="grey70", linetype="dashed") +
  scale_color_manual(values = c( "#006600","#3b5998", "#CC9933"),
                     labels=c("Model Estimates", "Facebook", "Labour Force Survey"),
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
        strip.text.x = element_text(
          size = 20, color = "black"
        ))+
  labs(caption = "LFS 95% C.I., Model Estimations with IQR.")+
  facet_wrap(~square, scales="free", labeller = labeller(square=sex))
dev.off()


#################################################################################
#################################################################################
# Plot Sensitivity Checks - Figure 6
#################################################################################
#################################################################################

# Data from Model 1
summary_m1<- read_csv("./outcome/model1.csv")
summary_m1<- summary_m1 %>% mutate(`50%`=y, Country=country) %>% select(Country, '2.5%','25%', '50%','75%', '97.5%', source, year) %>% 
  filter(source=="Model Estimates") %>% mutate(source="Model 1")

# Data from model 2
summary_m2<- read_csv("./outcome/model2_st.csv")
summary_m2<- summary_m2 %>% select(Country, '2.5%','25%', '50%','75%', '97.5%', source, year) 


check<-rbind(summary_m1, summary_m2)


check1 <- check %>% filter(Country=="Latvia"|Country=="Spain"| Country=="Portugal"| Country=="France"|
                             Country=="Lithuania"|Country=="Italy"|Country=="Germany"|Country=="Ireland"|Country=="Romania"|
                             Country=="Poland") %>% mutate(group="Group 1")

check2 <- check %>% filter(Country=="Luxembourg"|Country=="Estonia"|Country=="Austria"|
                             Country=="Finland"| Country=="Denmark"| Country=="Belgium"|
                             Country=="Czech Republic"| Country=="Sweden"|Country=="Slovakia"|
                             Country=="Greece"|Country=="Netherlands"| Country=="Hungary") %>% mutate(group="Group 2")

check3<- rbind(check1, check2)

neworder <- c("Poland", "Romania", "Ireland",  "Germany",
              "Italy","Lithuania", "France", "Portugal",
              "Spain", "Latvia", "Hungary", "Netherlands",
              "Greece", "Slovakia", "Sweden", "Czech Republic",
              "Belgium", "Denmark", "Finland", "Austria")

check3 <-  arrange(mutate(check3,Country=factor(Country,levels=neworder)),Country)


pdf(file = "./outcome/comparison_m1_m2.pdf",   # The directory you want to save the file in
    width = 18, # The width of the plot in inches
    height = 12)

dline=data.frame(x=c(1.5, 2.5,3.5, 4.5,5.5, 6.5,7.5, 8.5, 9.5), y=c(1.5, 2.5,3.5, 4.5,5.5, 6.5, 7.5, 8.5, 9.5))

check3$year<-as.factor(check3$year)


check3 %>%
  mutate(Country = fct_reorder(Country, `50%`),
         stratum = interaction(year, source)) %>%
  mutate(stratum = factor(stratum, levels = c("2018.Model 1", "2018.Model 2",
                                              "2019.Model 1", "2019.Model 2"))) %>%
  ggplot(aes(Country, `50%`)) +
  geom_pointrange(aes(y = `50%`, ymin = `2.5%`, ymax = `97.5%`, x=Country, group=stratum, shape=year,
                      colour=source),alpha = 0.7,size=0.5,
                  position = position_dodge(0.7))+
  geom_pointrange(aes(ymin = `25%`, ymax = `75%`, x=Country, group=stratum,shape=year,
                      colour=source),
                  size=1,
                  position = position_dodge(0.7))+
  geom_vline(data=dline, mapping=aes(xintercept=x), color="grey70", linetype="dashed") +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3), breaks= pretty_breaks())+ 
  coord_flip()+
  scale_color_manual(values = c("#009900", "#006600", "#bbc404"),
                     labels=c("Estimates Model 1", "Sum of the Estimates from Model 2", "Estimates Model 1 only LFS"),
                     name="Data Sources",
                     guide=guide_legend(title.position = "top", title.hjust = 0.5 ))+
  scale_shape_manual(values=c(16, 15),
                     labels=c("2018","2019"),
                     name  ="Year",
                     guide=guide_legend(title.position = "top", title.hjust = 0.5 ))+
  guides(
    color = guide_legend(title.position = "top", title.hjust = 0.5,
                         override.aes = list(shape=18, size=2, alpha = 0.8), order = 1),
    shape = guide_legend(title.position = "top", title.hjust = 0.5,
                         override.aes = list(size=1.5, alpha = 0.8)), order = 0)+
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



