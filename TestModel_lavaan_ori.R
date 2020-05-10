# few groups that can be focused
# SEX (1 = female, 2 = male)
# CNTSCHID - 225 schools
# STRATUM - Various Types (1-9)
# SCHLTYPE - Types of School (1-Private Independent, 2-Private Govt-Dependent, 3-Public )
impPISALong$ST004D01T <- factor(impPISALong$ST004D01T,labels = c("Female","Male"))
female <- subset(impPISALong, c(ST004D01T=="Female"))
male <- subset(impPISALong, c(ST004D01T=="Male"))
NationalSecondary <- subset(impPISALong,STRATUM == "MYS0101")
SBP <- subset(impPISALong,STRATUM == "MYS0204")

# ----------------------------------------- Testing ANALYSIS ----------------------------------------- #
# one level model with all variables from Science learning in school
# nice model - with no warning after executing fit.surv2 - good sign that this model fit nicely
# ----------------------------------------- ANALYSIS NO 2 ----------------------------------------- #


efficacy <- '
          science =~ PV1SCIE + PV2SCIE + PV3SCIE + PV4SCIE + PV5SCIE + PV6SCIE + PV7SCIE + PV8SCIE + PV9SCIE + PV10SCIE
          SCIEEFF =~ ST129Q01TA + ST129Q02TA + ST129Q03TA + ST129Q04TA + ST129Q05TA + ST129Q06TA + ST129Q07TA + ST129Q08TA

'
modelDesign <- svrepdesign( weights=~W_FSTUWT, data=impPISALong,repweights="W_FSTURWT[0-9]+", 
                         type="Fay", rho=0.5,combined.weights = TRUE)
fitAll <- lavaan(efficacy, data=impPISALong,meanstructure=TRUE,auto.var=TRUE, std.lv=TRUE,int.ov.free=TRUE)
fitAllSurv <-lavaan.survey(lavaan.fit=fitAll, survey.design=modelDesign)
fitmeasures(fitAllSurv,c("cfi","tli","rmsea","srmr"))
summary(fitAllSurv, fit.measures = TRUE, standardized = TRUE,rsquare=TRUE)

# female only data
fitFemale <- lavaan(efficacy, data=female,auto.var=TRUE, std.lv=TRUE, meanstructure=TRUE, 
                 int.ov.free=TRUE,estimator="MLM")
fitFemSurv <-lavaan.survey(lavaan.fit=fitFemale, survey.design=modelDesign)
fitmeasures(fitFemSurv,c("cfi","tli","rmsea","srmr"))

# male only data
fitMale <- lavaan(efficacy, data=male,auto.var=TRUE, std.lv=TRUE, meanstructure=TRUE, 
                    int.ov.free=TRUE,estimator="MLM")
fitMaleSurv <-lavaan.survey(lavaan.fit=fitMale, survey.design=modelDesign)
fitmeasures(fitMaleSurv,c("cfi","tli","rmsea","srmr"))

# Secondary School only data
fitSecondarySch <- lavaan(efficacy, data=NationalSecondary, auto.var=TRUE, std.lv=TRUE, meanstructure=TRUE, 
                  int.ov.free=TRUE,estimator="MLM")
fitSecSchSurv <-lavaan.survey(lavaan.fit=fitSecondarySch, survey.design=modelDesign)
fitmeasures(fitSecSchSurv,c("cfi","tli","rmsea","srmr"))

# SBP School only data
fitSBPSch <- lavaan(efficacy, data=SBP, auto.var=TRUE, std.lv=TRUE, meanstructure=TRUE, 
                          int.ov.free=TRUE,estimator="MLM")
fitSBPSchSurv <-lavaan.survey(lavaan.fit=fitSBPSch, survey.design=modelDesign)
fitmeasures(fitSBPSchSurv,c("cfi","tli","rmsea","srmr"))

# measurement invariance
options(scipen = 999)
multisteps <- measurementInvariance(model = efficacy,data = impPISALong,group="STRATUM",strict = TRUE)

# adding more restrictive in the model (Metric|group.equal = "loadings",Scalar|"intercepts",Strict|"residuals")
fitAllStrict <- lavaan(efficacy, data=impPISALong,meanstructure=TRUE,auto.var=TRUE, std.lv=TRUE,int.ov.free=TRUE,group = "STRATUM",
                       group.equal=c("loadings","intercepts","residuals"))
fitAllSurvStrict <-lavaan.survey(lavaan.fit=fitAllStrict, survey.design=modelDesign)
fitmeasures(fitAllSurvStrict,c("cfi","tli","rmsea","srmr"))
summary(fitAllSurvStrict, fit.measures = TRUE, standardized = TRUE,rsquare=TRUE)

# ----------------------------------------- Testing  COMPOSITE ----------------------------------------- #

efficacy1 <- '
          science =~ PV1SCIE + PV2SCIE + PV3SCIE + PV4SCIE + PV5SCIE + PV6SCIE + PV7SCIE + PV8SCIE + PV9SCIE + PV10SCIE
          SCIEEFF =~ ST129Q01TA + ST129Q02TA + ST129Q03TA + ST129Q04TA + ST129Q05TA + ST129Q06TA + ST129Q07TA + ST129Q08TA
          EPIST   =~ ST131Q01NA+ST131Q03NA+ST131Q04NA+ST131Q06NA+ST131Q08NA+ST131Q11NA
          SCIEACT =~ ST146Q01TA+ST146Q02TA+ST146Q03TA+ST146Q04TA+ST146Q05TA+ST146Q06NA+ST146Q07NA+ST146Q08NA+ST146Q09NA
    
          science <~ SCIEEFF + EPIST + SCIEACT + ESCS 


'
modelDesign <- svrepdesign( weights=~W_FSTUWT, data=impPISALong,repweights="W_FSTURWT[0-9]+", 
                            type="Fay", rho=0.5,combined.weights = TRUE)
fitAll1 <- lavaan(efficacy1, data=impPISALong,meanstructure=TRUE,auto.var=TRUE, std.lv=TRUE,int.ov.free=TRUE)
#APPLY THE RUBIN TECHNIQUE
fitAllSurv1 <-lavaan.survey(lavaan.fit=fitAll1, survey.design=modelDesign)
fitmeasures(fitAllSurv1,c("cfi","tli","rmsea","srmr"))
summary(fitAllSurv1, fit.measures = TRUE, standardized = TRUE,rsquare=TRUE)
semPaths(fitAllSurv1)
# ----------------------------------------- Testing  TWO.LEVEL ----------------------------------------- #
SchToDrop <- c("45800150")
impPISALongT <- subset(impPISALong, CNTSCHID %in% SchToDrop, select = CNT:PROSTAT)
impPISALongN <- subset(impPISALong, !(CNTSCHID %in% SchToDrop), select = (CNT:PROSTAT))
str(impPISALongN, list.len=ncol(impPISALongN))

model.pisa9 <- '
    level: 1     
          science =~ PV1SCIE + PV2SCIE + PV3SCIE + PV4SCIE + PV5SCIE + PV6SCIE + PV7SCIE + PV8SCIE + PV9SCIE + PV10SCIE
          science ~ SCIEEFF + EPIST + SCIEACT


    level: 2      
          scienceQ =~ PV1SCIE + PV2SCIE + PV3SCIE + PV4SCIE + PV5SCIE + PV6SCIE + PV7SCIE + PV8SCIE + PV9SCIE + PV10SCIE
          scienceQ ~  SCIEEFF + EPIST + SCIEACT

'

modelDesign1 <- svrepdesign( weights=~W_FSTUWT, data=impPISALong,repweights="W_FSTURWT[0-9]+", 
                            type="Fay", rho=0.5,combined.weights = TRUE)
fitAll9 <- sem(model.pisa9, data=impPISALong,cluster = "CNTSCHID",meanstructure=TRUE,auto.var=TRUE, std.lv=TRUE,int.ov.free=TRUE)
#APPLIED THE RUBIN TECHNIQUE
fitAllSurv9 <-lavaan.survey(lavaan.fit=fitAll9, survey.design=modelDesign1)
fitmeasures(fitAll9,c("cfi","tli","rmsea","srmr"))
summary(fitAllSurv9, fit.measures = TRUE, standardized = TRUE,rsquare=TRUE)


# ----------------------------------------- Testing DEMO TWO.LEVEL ----------------------------------------- #
model <- '
    level: 1
          fw =~ y1 + y2 + y3
          fw ~ x1 + x2 + x3
    level: 2
          fb =~ y1 + y2 + y3
          fb ~ w1 + w2
'
fit <- lavaan(model, data = Demo.twolevel, cluster = "cluster",meanstructure=TRUE,auto.var=TRUE, std.lv=TRUE,int.ov.free=TRUE)
summary(fit,fit.measures = TRUE, standardized = TRUE,rsquare=TRUE)
fitmeasures(fit,c("cfi","tli","rmsea","srmr"))




