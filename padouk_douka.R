# install everything and read in csv

getwd()
library(ggplot2)
# install.packages("GGally")
library(GGally)
library(tidyverse)

pd <- read.csv("Padouk-Douka.csv", 
               stringsAsFactors = T)
               # colClasses="factor")

# make sure data is in correct format

pd$Age[pd$Age == " "] = NA

pd$Gender <- as.factor(pd$Gender)
levels(pd$Gender)

pd$Axis <- as.factor(pd$Axis)
levels(pd$Axis)

pd$Ethnicity <- as.factor(pd$Ethnicity)
levels(pd$Ethnicity)

pd$Age <- as.factor(pd$Age)
levels(pd$Age)

pd$Hunter <- as.factor(pd$Hunter)
levels(pd$Hunter)
class(pd$Hunter)

pd$Farmer <- as.factor(pd$Farmer)
levels(pd$Farmer)
class(pd$Farmer)

pd$Distance.Ivindo.km <- as.numeric(pd$Distance.Ivindo.km)
class(pd$Distance.Ivindo.km)
pd$Distance.Ivindo.km

pd$Distance.Makokou.km <- as.numeric(pd$Distance.Makokou.km)
class(pd$Distance.Makokou.km)
pd$Distance.Makokou.km

table(pd$Hunter)
table(pd$Farmer)
table(pd$Age)

pd = na.omit(pd)

# pd = pd %>% 
#   filter(Ethnicity != "n/a") %>% 
#   filter(Age != "n/a") %>% 
#   filter(Farmer != "n/a") %>% 
#   filter(Hunter != "n/a")

# make separate data sets for douka and padouk

padouk = pd %>% select(Village:Padouk)
douka = pd %>% select(Village:Hunter, Douka)

######## padouk ########

ggpairs(padouk)

padouk.glm1 <- glm(Padouk ~ 
                       Distance.Makokou.km +
                       # factor(Axis) + 
                       Ethnicity + 
                       Gender + 
                       Age + 
                       Farmer + 
                       Hunter, 
               family = binomial, 
               data = padouk)
summary(padouk.glm1)

padouk.glm2 = update(padouk.glm1, .~. -factor(Hunter))
summary(padouk.glm2)

padouk.glm3 = update(padouk.glm2, .~. -factor(Age))
summary(padouk.glm3)

padouk.glm4 = update(padouk.glm3, .~. -Farmer)
summary(padouk.glm4)

padouk.glm5 = update(padouk.glm4, .~. -Distance.Makokou.km)
summary(padouk.glm5)

padouk.glm6 = update(padouk.glm5, .~. -factor(Ethnicity))
summary(padouk.glm6)

AIC(padouk.glm1, 
     padouk.glm2, 
     padouk.glm3, 
     padouk.glm4, 
     padouk.glm5,
     padouk.glm6)

padouk.glma <- glm(Padouk ~ 
                    Distance.Makokou.km +
                    Distance.Ivindo.km +
                    Ethnicity + 
                    Gender + 
                    Age + 
                    Farmer + 
                    Hunter, 
                   family = binomial, 
                   data = padouk)
summary(padouk.glma)

padouk.glmb = update(padouk.glma, .~. -Age)
summary(padouk.glmb)

padouk.glmc = update(padouk.glmb, .~. -Hunter)
summary(padouk.glmc)

padouk.glmd = update(padouk.glmc, .~. -Farmer)
summary(padouk.glmd)

padouk.glme = update(padouk.glmd, .~. -Ethnicity)
summary(padouk.glme)

padouk.glmf = update(padouk.glme, .~. -Gender)
summary(padouk.glmf)

padouk.glmg = update(padouk.glmf, .~. -Distance.Ivindo.km)
summary(padouk.glmg)

AIC(padouk.glma, 
    padouk.glmb, 
    padouk.glmc, 
    padouk.glmd, 
    padouk.glme,
    padouk.glmf,
    padouk.glmg)

padouk.glmh <- glm(Padouk ~ 
                       (1|Distance.Makokou.km) +
                       (1|Distance.Ivindo.km) +
                       Ethnicity + 
                       Gender + 
                       Age + 
                       Farmer + 
                       Hunter, 
                   family = binomial, 
                   data = padouk)
summary(padouk.glmh)

padouk.glmi = update(padouk.glmh, .~. -Age)
summary(padouk.glmi)

padouk.glmj = update(padouk.glmi, .~. -Hunter)
summary(padouk.glmj)

padouk.glmk = update(padouk.glmj, .~. -Farmer)
summary(padouk.glmk)

padouk.glml = update(padouk.glmk, .~. -Ethnicity)
summary(padouk.glml)

AICc(padouk.glma, 
    padouk.glmb, 
    padouk.glmc, 
    padouk.glmd, 
    padouk.glme,
    padouk.glmf,
    padouk.glmg,
    padouk.glmh, 
    padouk.glmi, 
    padouk.glmj, 
    padouk.glmk, 
    padouk.glml)

padouk.glm11 <- glm(Padouk ~ 
                       (1|Distance.Makokou.km) +
                       Distance.Ivindo.km +
                       Ethnicity + 
                       Gender + 
                       Age + 
                       Farmer + 
                       Hunter, 
                   family = binomial, 
                   data = padouk)
summary(padouk.glm11)

######## douka ########

ggpairs(douka)

douka.glm1 <- glm(Douka ~ 
                     Distance.Makokou.km + 
                     # factor(Axis) + 
                     factor(Ethnicity) + 
                     factor(Gender) + 
                     factor(Age) + 
                     Farmer + 
                     Hunter, 
                   family = binomial, 
                   data = douka)
summary(douka.glm1)

douka.glm2 = update(douka.glm1, .~. -factor(Age))
summary(douka.glm2)

douka.glm3 = update(douka.glm2, .~. -factor(Ethnicity))
summary(douka.glm3)

douka.glm4 = update(douka.glm3, .~. -Distance.Makokou.km)
summary(douka.glm4)

douka.glm5 = update(douka.glm4, .~. -Hunter)
summary(douka.glm5)

douka.glm6 = update(douka.glm5, .~. -Farmer)
summary(douka.glm6)

AIC(douka.glm1, 
    douka.glm2, 
    douka.glm3,
    douka.glm4,
    douka.glm5, 
    douka.glm6)

douka.glma <- glm(Douka ~ 
                       Distance.Makokou.km +
                       Distance.Ivindo.km +
                       Ethnicity + 
                       Gender + 
                       Age + 
                       Farmer + 
                       Hunter, 
                   family = binomial, 
                   data = douka)
summary(padouk.glma)

douka.glmb = update(douka.glma, .~. -Age)
summary(douka.glmb)

douka.glmc = update(douka.glmb, .~. -Ethnicity)
summary(douka.glmc)

douka.glmd = update(douka.glmc, .~. -Farmer)
summary(douka.glmd)

douka.glme = update(douka.glmd, .~. -Hunter)
summary(douka.glme)

douka.glmf = update(douka.glme, .~. -Distance.Makokou.km)
summary(douka.glmf)

douka.glmg = update(douka.glmf, .~. -Distance.Ivindo.km)
summary(douka.glmg)

AICc(douka.glma, 
    douka.glmb, 
    douka.glmc,
    douka.glmd,
    douka.glme, 
    douka.glmf,
    douka.glmg)

douka.glmh <- glm(Douka ~ 
                      (1|Distance.Makokou.km) +
                      (1|Distance.Ivindo.km) +
                      Ethnicity + 
                      Gender + 
                      Age + 
                      Farmer + 
                      Hunter, 
                  family = binomial, 
                  data = douka)
summary(padouk.glmh)

douka.glmi = update(douka.glmh, .~. -Age)
summary(douka.glmi)

douka.glmj = update(douka.glmi, .~. -Ethnicity)
summary(douka.glmj)

douka.glmk = update(douka.glmj, .~. -Hunter)
summary(douka.glmk)

douka.glml = update(douka.glmk, .~. -Farmer)
summary(douka.glml)

AIC(douka.glma, 
    douka.glmb, 
    douka.glmc,
    douka.glmd,
    douka.glme, 
    douka.glmf,
    douka.glmg,
    douka.glmh, 
    douka.glmi, 
    douka.glmj,
    douka.glmk,
    douka.glml)
