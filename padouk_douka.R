# install everything and read in csv

getwd()
library(ggplot2)
# install.packages("GGally")
library(GGally)
library(tidyverse)

pd <- read.csv("Padouk-Douka.csv", 
               stringsAsFactors = T,
               colClasses="factor")

# make sure data is in correct format

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

pd$Farmer <- as.factor(pd$Farmer)
levels(pd$Farmer)

pd$Distance.km <- as.numeric(pd$Distance)
class(pd$Distance.km)

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
                 Distance.km + 
                 factor(Axis) + 
                 factor(Ethnicity) + 
                 factor(Gender) + 
                 factor(Age) + 
                 Farmer + 
                 Hunter, 
               family = binomial, 
               data = padouk)
summary(padouk.glm1)

padouk.glm2 <- glm(Padouk ~ 
                     Distance.km + 
                     factor(Axis) + 
                     factor(Ethnicity) +
                     factor(Gender) + 
                     # factor(Age) + 
                     Farmer + 
                     Hunter, 
                   family = binomial, 
                   data = padouk)
summary(padouk.glm2)

padouk.glm3 <- glm(Padouk ~ 
                     Distance.km + 
                     factor(Axis) +
                     factor(Ethnicity) +
                     factor(Gender) + 
                     # factor(Age) + 
                     # Farmer + 
                     Hunter, 
                   family = binomial, 
                   data = padouk)
summary(padouk.glm3)

padouk.glm4.1 <- glm(Padouk ~ 
                     Distance.km + 
                     # factor(Axis) +
                     factor(Ethnicity) +
                     factor(Gender) +
                     # factor(Age) + 
                     # Farmer + 
                     Hunter,
                   family = binomial, 
                   data = padouk)
summary(padouk.glm4.1)

padouk.glm4.2 <- glm(Padouk ~ 
                     Distance.km + 
                     factor(Axis) +
                     factor(Ethnicity) +
                     factor(Gender),
                     # factor(Age) + 
                     # Farmer + 
                     # Hunter,
                   family = binomial, 
                   data = padouk)
summary(padouk.glm4.2)

padouk.glm4.3 <- glm(Padouk ~ 
                       Distance.km + 
                     # factor(Axis) +
                       factor(Ethnicity) +
                       factor(Gender),
                     # factor(Age) + 
                     # Farmer + 
                     # Hunter,
                     family = binomial, 
                     data = padouk)
summary(padouk.glm4.3)

padouk.glm5 <- glm(Padouk ~ 
                     Distance.km + 
                     # factor(Axis) +
                     # factor(Ethnicity) +
                     factor(Gender),
                     # factor(Age) + 
                     # Farmer + 
                     # Hunter, 
                   family = binomial, 
                   data = padouk)
summary(padouk.glm5)

padouk.glm6 <- glm(Padouk ~ 
                     Distance.km,
                     # factor(Axis) + 
                     # factor(Ethnicity) + 
                     # factor(Gender), 
                     # factor(Age) + 
                     # Farmer +
                     # Hunter, 
                   family = binomial, 
                   data = padouk)
summary(padouk.glm6)

padouk.glm7 <- glm(Padouk ~ 
                     # Distance.km,
                     # factor(Axis) + 
                     # factor(Ethnicity) + 
                     factor(Gender),
                     # factor(Age) + 
                     # Farmer +
                     # Hunter, 
                   family = binomial, 
                   data = padouk)
summary(padouk.glm7)

# padouk.glm2 <- glm(Padouk ~ 
#                      Distance.km * 
#                      factor(Axis) * 
#                      factor(Ethnicity) * 
#                      factor(Gender) * 
#                      factor(Age) * 
#                      Farmer * 
#                      Hunter, 
#                    family = binomial, 
#                    data = padouk)
# summary(padouk.glm2)

AIC(padouk.glm1, 
     padouk.glm2, 
     padouk.glm3, 
     padouk.glm4.1, 
     padouk.glm4.2, 
     padouk.glm4.3, 
     padouk.glm5,
     padouk.glm6, 
     padouk.glm7)

######## padouk ########

ggpairs(douka)

douka.glm1 <- glm(Douka ~ 
                     Distance.km + 
                     factor(Axis) + 
                     factor(Ethnicity) + 
                     factor(Gender) + 
                     factor(Age) + 
                     Farmer + 
                     Hunter, 
                   family = binomial, 
                   data = douka)
summary(douka.glm1)

douka.glm2 <- glm(Douka ~ 
                    Distance.km + 
                    factor(Axis) + 
                    factor(Ethnicity) +
                    factor(Gender) + 
                    factor(Age) + 
                    # Farmer + 
                    Hunter, 
                  family = binomial, 
                  data = douka)
summary(douka.glm2)

douka.glm3 <- glm(Douka ~ 
                    Distance.km + 
                    factor(Axis) + 
                    factor(Ethnicity) +
                    factor(Gender) + 
                    factor(Age),
                    # Farmer + 
                    # Hunter, 
                  family = binomial, 
                  data = douka)
summary(douka.glm3)

douka.glm4.1 <- glm(Douka ~ 
                    Distance.km + 
                    factor(Axis) + 
                    # factor(Ethnicity) + 
                    factor(Gender) +
                    factor(Age),
                    # Farmer + 
                    # Hunter, 
                  family = binomial, 
                  data = douka)
summary(douka.glm4.1)

douka.glm4.2 <- glm(Douka ~ 
                      Distance.km + 
                      factor(Axis) + 
                      factor(Ethnicity) +
                      factor(Gender),
                    # factor(Age) + 
                    # Farmer + 
                    # Hunter, 
                    family = binomial, 
                    data = douka)
summary(douka.glm4.2)

douka.glm4.3 <- glm(Douka ~ 
                      # Distance.km + 
                      factor(Axis) + 
                      factor(Ethnicity) +
                      factor(Gender) +
                      factor(Age),
                    # Farmer + 
                    # Hunter, 
                    family = binomial, 
                    data = douka)
summary(douka.glm4.3)

douka.glm3.1 <- glm(Douka ~ 
                    Distance.km + 
                    factor(Axis) + 
                    # factor(Ethnicity) +
                    factor(Gender) + 
                    factor(Age),
                  # Farmer + 
                  # Hunter, 
                  family = binomial, 
                  data = douka)
summary(douka.glm3.1)

douka.glm3.2 <- glm(Douka ~ 
                      # Distance.km + 
                      factor(Axis) + 
                      # factor(Ethnicity) +
                      factor(Gender) + 
                      factor(Age),
                    # Farmer + 
                    # Hunter, 
                    family = binomial, 
                    data = douka)
summary(douka.glm3.2)

douka.glm5 <- glm(Douka ~ 
                      # Distance.km + 
                      factor(Axis) + 
                      # factor(Ethnicity) +
                      factor(Gender),
                    # factor(Age),
                    # Farmer + 
                    # Hunter, 
                    family = binomial, 
                    data = douka)
summary(douka.glm5)

douka.glm6 <- glm(Douka ~ 
                    # Distance.km + 
                    # factor(Axis) + 
                    # factor(Ethnicity) +
                    factor(Gender),
                  # factor(Age),
                  # Farmer + 
                  # Hunter, 
                  family = binomial, 
                  data = douka)
summary(douka.glm6)

douka.glm7 <- glm(Douka ~ 
                    # Distance.km + 
                    factor(Axis),
                    # factor(Ethnicity) +
                    # factor(Gender),
                  # factor(Age),
                  # Farmer + 
                  # Hunter, 
                  family = binomial, 
                  data = douka)
summary(douka.glm7)

# douka.glm3 <- glm(Douka ~ 
#                      Distance.km * 
#                      factor(Axis) * 
#                      factor(Ethnicity) * 
#                      factor(Gender) * 
#                      factor(Age) * 
#                      Farmer * 
#                      Hunter, 
#                    family = binomial, 
#                    data = douka)
# summary(douka.glm3)

AIC(douka.glm1, 
    douka.glm2, 
    douka.glm3, 
    douka.glm3.1,
    douka.glm3.2,
    douka.glm4.1, 
    douka.glm4.2,
    douka.glm4.3,
    douka.glm5, 
    douka.glm6, 
    douka.glm7)
