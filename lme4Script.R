install.packages("merTools")
library(dplyr)
library(ggplot2)
library(lme4)
library(merTools)
#search for missing values
which(is.na(pisaListWdeletion)==T)

n <- nrow(pisaListWdeletion)
#the student house-weights
#Student house weight, also called normalized weight, is used 
#when analyses are sensitive to sample size. 
#Student house weight is essentially a linear transformation of 
#total student weight so that the sum of the weights is equal to the sample size.
pisaListWdeletion$W_HOUSEWHT <- n * pisaListWdeletion$W_FSTUWT / sum(pisaListWdeletion$W_FSTUWT)

pisaListWdeletion %>% group_by(STRATUM) %>% 
                      summarise(avg1 = weighted.mean(PV1SCIE, w = W_HOUSEWHT),
                        avg2 = weighted.mean(PV2SCIE, w = W_HOUSEWHT),
                        avg3 = weighted.mean(PV3SCIE, w = W_HOUSEWHT),
                        avg4 = weighted.mean(PV4SCIE, w = W_HOUSEWHT))


##########################
### ListWiseDeletion ###
##########################
#Inquiry-based science teaching and learning practices (IBTEACH)
#fixed effect = Gender and ESCS
#samplePisa <- sample_n(pisaListWdeletion, 100)
#n <- nrow(samplePisa)
#samplePisa$W_HOUSEWHT <- n * samplePisa$W_FSTUWT / sum(samplePisa$W_FSTUWT)
#which(is.na(samplePisa)==T)



#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# LINEAR MODEL #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
options(scipen = 9999)
xmdl1 = lm(PV1SCIE ~ ESCS, pisaListWdeletion)
summary(xmdl1)
xmdl2 = lm(PV2SCIE ~ ESCS, pisaListWdeletion)
summary(xmdl2)
xmdl3 = lm(PV3SCIE ~ ESCS, pisaListWdeletion)
summary(xmdl3)

mean(pisaListWdeletion[pisaListWdeletion$ST004D01T=="1",]$PV1SCIE)
mean(pisaListWdeletion[pisaListWdeletion$ST004D01T=="2",]$PV1SCIE)

#---------------##---------------##---------------#
#---------------##---------------##---------------#
#                 Assumptions                     #
#---------------##---------------##---------------#
#---------------##---------------##---------------#
# 1: Linearity 
#---------------#
# If there were a nonlinear or curvy pattern, then this would indicate a violation of the linearity assumption. 
# If it doesn’t, the residual plot will indicate some kind of curve
plot(pisaListWdeletion$ESCS,pisaListWdeletion$PV1SCIE)
plot(fitted(xmdl1),residuals(xmdl1))


#---------------#---------------#
# 2: Abscence of Collinearity 
#---------------#---------------#
# these correlated predictors, within the same model, are likely going to run into a collinearity problem.
# If there’s collinearity, the interpretation of the model becomes unstable
# the fixed effects become significant or cease to be significant. 
# If multiple predictors are very similar to each other, it becomes very difficult to decide who is playing a big role.
# How to get rid of collinearity?
#---------------#-----------#---------------#---------------#
# 3: Homoskedasticity or abscence of heteroskedasticity
#---------------#-----------#---------------#---------------#
# heteroskedasticity = a problem with unequal variances
# homoskedasticity = equal variances
# A good residual plot essentially looks blob-like
# heteroskedasticity problems: higher fitted values have larger residuals, indicating that the model is more “off” 
# with larger predicted means. So, the variability is not homoscedastic: it’s smaller in the lower range and larger 
# in the higher range.
#---------------#---------------#
# 4: Normality of residuals
#---------------#---------------#
# Is least important
# To test the assumption, Either you make a histogram of the residuals of your model or a Q-Q plot 
hist(residuals(xmdl1))
qqnorm(residuals(xmdl1))
# IF The histogram is relatively bell-shaped and the Q-Q plot indicates that the data falls on a straight line 
# (which means that it’s similar to a normal distribution). 
# Here, we would conclude that there are no obvious violations of the normality assumption.
#---------------#---------------------#
# 5: Absence of influential data points
#---------------#---------------------#
# Any value that changes the sign of the slope is an influential point and special attention
options(scipen = 9999)
dfbeta(xmdl1)
#--------#---------#
# 6: Independence
#--------#---------#
# IS the most important assumption of all statistical tests. 



#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#         RANDOM INTERCEPT MODEL              #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
IbNull <- lmer(PV1SCIE ~   (1 | CNTSCHID), data = pisaListWdeletion,weights = W_HOUSEWHT,REML = FALSE)
summary(IbNull)
#confint(IbNull)

IbFull <- lmer(PV1SCIE ~ ESCS + (1| CNTSCHID), data = pisaListWdeletion,weights = W_HOUSEWHT,REML = FALSE)
summary(IbFull)
#confint(IbFull)

options(scipen = 9999)
#model will throw an errors if there are missing values in the data
anova(IbNull,IbFull)
#(coef((IbFull)))

boxplot(pisaListWdeletion$ESCS,pisaListWdeletion$PV1SCIE,horizontal = TRUE)


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#             RANDOM SLOPE MODEL              #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
IbNullS <- lmer(PV1SCIE ~ ESCS  + (1 | CNTSCHID), data = pisaListWdeletion,weights = W_HOUSEWHT)
summary(IbNullS)
#confint(IbNullS)

IbFullS <- lmer(PV1SCIE ~ ESCS + (1+ ESCS| CNTSCHID), data = pisaListWdeletion,weights = W_HOUSEWHT)
summary(IbFullS)
#confint(IbNullS)

options(scipen = 9999)
#model will throw an errors if there are missing values in the data
anova(IbNullS,IbFullS)
#(coef((IbFull)))

##################
#Teacher support in a science classes (TEACHSUP)
TeachNull <- lmer(PV1SCIE ~ ST004D01T+ ESCS + (1 | CNTSCHID), data = pisaListWdeletion,weights = W_HOUSEWHT,REML=FALSE)
summary(TeachNull)

TeachFull <- lmer(PV1SCIE ~ ST004D01T + ESCS + TEACHSUP + (1| CNTSCHID), data = pisaListWdeletion,weights = W_HOUSEWHT,REML=FALSE)
summary(TeachFull)

options(scipen = 9999)
#model will throw an errors if there are missing values in the data
anova(TeachNull,TeachFull)

##################
#Teacher-directed science instruction (TDTEACH)
TdNull <- lmer(PV1SCIE ~ ST004D01T+ ESCS + (1 | CNTSCHID), data = pisaListWdeletion,weights = W_HOUSEWHT,REML=FALSE)
summary(TeachNull)

TdFull <- lmer(PV1SCIE ~ ST004D01T + ESCS + TDTEACH + (1| CNTSCHID), data = pisaListWdeletion,weights = W_HOUSEWHT,REML=FALSE)
summary(TeachFull)

options(scipen = 9999)
#model will throw an errors if there are missing values in the data
anova(TdNull,TdFull)


##########################
### Imputation ###
##########################
#search for missing values
which(is.na(impPISALong)==T)

n <- nrow(impPISALong)
impPISALong$W_HOUSEWHT <- n * impPISALong$W_FSTUWT / sum(impPISALong$W_FSTUWT)

impPISALong %>% group_by(STRATUM) %>% 
  summarise(avg1 = weighted.mean(PV1SCIE, w = W_HOUSEWHT),
            avg2 = weighted.mean(PV2SCIE, w = W_HOUSEWHT),
            avg3 = weighted.mean(PV3SCIE, w = W_HOUSEWHT),
            avg4 = weighted.mean(PV4SCIE, w = W_HOUSEWHT))

HLMMas00 <- lmer(PV1SCIE ~ (1 | CNTSCHID), data = impPISALong,weights = W_HOUSEWHT)
summary(HLMMas0)

HLMMas11 <- lmer(PV1SCIE ~ ESCS + (1 + ESCS| CNTSCHID), data = impPISALong,weights = W_HOUSEWHT)
summary(HLMMas1)

options(scipen = 9999)
#model will throw an errors if there are missing values in the data
anova(HLMMas00,HLMMas11)



##################
#which(is.na(PisaCol)==T)
#PisaCol <- na.omit(PisaCol)
#nC <- nrow(PisaCol)
#PisaCol$W_HOUSEWHT <- nC * PisaCol$W_FSTUWT / sum(PisaCol$W_FSTUWT)

#PisaCol %>%
#  group_by(STRATUM) %>%
#  summarise(avg1 = weighted.mean(PV1MATH, w = W_HOUSEWHT),
#            avg2 = weighted.mean(PV2MATH, w = W_HOUSEWHT))

##################
### Null model ###
##################

#HLM0 <- lmer(PV1MATH ~ (1 | SCHOOLID), data = PisaCol,weights = W_HOUSEWHT)
#coef(HLM0)
#summary(HLM0)
#100 * 3569 / (3569 + 2113)

#HLM1 <- lmer(PV1MATH ~ ESCS + (1 + ESCS | SCHOOLID), data = PisaCol,
#             weights = W_HOUSEWHT)

#anova(HLM0,HLM1)
#anova(HLM0,HLM2)

#coef(HLM1)
#summary(HLM1)
#100 * (96.12 + 1697.36) / (3392.58 + 96.12 + 1697.36)

#ggplot(data = PisaCol, aes(x = ESCS, y = PV1MATH, size = W_HOUSEWHT)) +
#  theme_minimal() + geom_point() + theme(legend.position="none")

#ggplot(data = PisaCol, aes(x = ESCS, y = PV1MATH, size = W_HOUSEWHT)) +
#  geom_point(aes(colour = SCHOOLID)) + theme(legend.position="none")

######################################################
### BELAJAR LME4 ### Politeness database
######################################################
politeness <- read.csv("politeness_data.csv")
which(is.na(politeness)==T)
boxplot(frequency ~ attitude*gender,
        col=c("green","lightgray"),politeness)


lmer(frequency ~ attitude, data=politeness)
politeness.model = lmer(frequency ~ attitude + (1|subject) + (1|scenario), data=politeness)
coef(politeness.model)
