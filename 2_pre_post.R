## repeated measures pre-post analysis
rm(list=ls())
library(dplyr)
library(tidyr)
library(MASS)
library(lmtest)
library(lme4)
library(glmmTMB)
library(DHARMa)
library(sandwich)
library(splines)

select = dplyr::select
load(file = "../ambit.rds")

fit_admit <- glmmTMB(admitted ~ bs(age) + sex + time
                  + (1|study_id),  
                  family=binomial,
                  data=ambit)

summary(fit_admit)

fit_ed <- glmmTMB(ed ~ bs(age) + sex + time
                     + (1|study_id),  
                     family=binomial,
                     data=ambit)


summary(fit_ed)

##zero inflated negative binomial
fit_hosp <- glmmTMB(days_in_hospital ~ bs(age) + sex  +  time
                   + (1|study_id),  
                    # ziformula = ~1,          # Constant zero-inflation probability
                   family=nbinom2,
                   data=ambit) #Poisson overdispersed
summary(fit_hosp)

#Poisson
fit_num_ed <- glmmTMB(ed_presentations ~ bs(age) + sex + time + (1|study_id), 
                      data = ambit, 
                      family = poisson) #overdispersion p = 0.064

summary(fit_num_ed)


fit_ncmht <- glmmTMB(ncmht_contacts ~ bs(age)+ sex + time + (1|study_id), 
                      data = ambit, 
                      family = poisson) #Poisson overdispersed

summary(fit_ncmht) #significant outlier

#remove outliers

fit_ncmht2 <- glmmTMB(ncmht_contacts ~ bs(age) + sex + time + (1|study_id), 
                     data = ambit 
                      %>% filter( sex != "Other" ) #ncmht_contacts < 200,
                     ,family = nbinom2)

summary(fit_ncmht2)

##testing Poisson overdispersion and zero inflation

#https://cran.r-project.org/web/packages/DHARMa/refman/DHARMa.html#testZeroInflation

simulationOutput <- simulateResiduals(fittedModel = fit_ncmht2)
plot(simulationOutput, quantreg = TRUE)
testDispersion(simulationOutput)
testZeroInflation(simulationOutput)
testOutliers(simulationOutput, type= "bootstrap")
