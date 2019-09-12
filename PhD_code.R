install.packages('intsvy')
install.packages('foreign')
install.packages("lavaan")
install.packages("lavaan.survey", dependencies=TRUE)
install.packages("agrmt")
install.packages("semPlot")
install.packages("semTools")


library("foreign") #penting 1
library("intsvy") #penting 2
library("lavaan") #penting
library("lavaan.survey")
library("mice")
library("semTools")
library("semPlot")


# ---------------------------------- #
# read the data
# ---------------------------------- #


#Print the data labels to decide which variables and country to analyze
#pisa.var.label(student.file = c("CY6_MS_CM2_STU_QQQ.sav"),
#               school.file = "CY6_MS_CM2_SCH_QQQ.sav")
#pisa.var.label(student.file = c("CY6_MS_CM2_STU_QQQ.sav"),
#               school.file = "CY6_MS_CM2_TCH_QQQ.sav",name = "Student-Teach Label")

#using intsvy package to merge two datafiles
pisaMas2015 <- pisa.select.merge(student.file = "CY6_MS_CM2_STU_QQQ.sav",
                                 school.file = "CY6_MS_CM2_SCH_QQQ.sav",
                                 #parent.file = "CY6_MS_CM2_TCH_QQQ.sav",
                                 # variables are numerical
                                 student = c(#Sex "ST004D01T",
                                   
                                   
                                   #Science learning in school
                                   #Inquiry-based science teaching and learning practices (IBTEACH)
                                   "ST098Q01TA","ST098Q02TA","ST098Q03NA","ST098Q05TA","ST098Q06TA","ST098Q07TA",
                                   "ST098Q08NA","ST098Q09TA","ST098Q10NA","IBTEACH",
                                   #Teacher support in a science classes (TEACHSUP)
                                   "ST100Q01TA","ST100Q02TA","ST100Q03TA","ST100Q04TA","ST100Q05TA","TEACHSUP",
                                   #Teacher-directed science instruction (TDTEACH)
                                   "ST103Q01NA","ST103Q03NA","ST103Q08NA","ST103Q11NA","TDTEACH", 
                                   #Perceived Feedback (PERFEED)
                                   "ST104Q01NA","ST104Q02NA","ST104Q03NA","ST104Q04NA","ST104Q05NA","PERFEED",
                                   #Adaption of instruction (ADINST)
                                   "ST107Q01NA","ST107Q02NA","ST107Q03NA","ADINST",
                                   #Instrumental motivation (INSTSCIE)
                                   "ST113Q01TA","ST113Q02TA","ST113Q03TA","ST113Q04TA","INSTSCIE",
                                   #Disciplinary climate in science classes (DISCLISCI)
                                   "ST097Q01TA","ST097Q02TA","ST097Q03TA","ST097Q04TA","ST097Q05TA","DISCLISCI",
                                   
                                   
                                   #Science-related dispositions / Other science related
                                   #Science self-efficacy (SCIEEFF) - effectiveness 
                                   "ST129Q01TA","ST129Q02TA","ST129Q03TA","ST129Q04TA","ST129Q05TA","ST129Q06TA",
                                   "ST129Q07TA","ST129Q08TA","SCIEEFF",
                                   #Epistemological beliefs (EPIST) - Knowledge
                                   "ST131Q01NA","ST131Q03NA","ST131Q04NA","ST131Q06NA","ST131Q08NA","ST131Q11NA","EPIST",
                                   #Science activities (SCIEACT)
                                   "ST146Q01TA","ST146Q02TA","ST146Q03TA","ST146Q04TA","ST146Q05TA","ST146Q06NA","ST146Q07NA",
                                   "ST146Q08NA","ST146Q09NA","SCIEACT",
                                   
                                   
                                   #Students’ dispositions for collaborative problem solving
                                   #Enjoy co-operation (COOPERATE)
                                   "ST082Q02NA","ST082Q03NA","ST082Q08NA","ST082Q12NA","COOPERATE",
                                   #Value co-operation (CPSVALUE)
                                   "ST082Q01NA","ST082Q09NA","ST082Q13NA","ST082Q14NA","CPSVALUE",
                                   
                                   
                                   #Sense of Belonging in School
                                   #Sense of Belonging to School (BELONG)
                                   "ST034Q01TA","ST034Q02TA","ST034Q03TA","ST034Q04TA","ST034Q05TA","ST034Q06TA","BELONG",
                                   
                                   #Interest in science
                                   #Enjoyment of science (JOYSCIE)
                                   "ST094Q01NA","ST094Q02NA","ST094Q03NA","ST094Q04NA","ST094Q05NA","JOYSCIE",
                                   #Interest in broad science topics (INTBRSCI)
                                   "ST095Q04NA","ST095Q07NA","ST095Q08NA","ST095Q13NA","ST095Q15NA","INTBRSCI",
                                   
                                   #Students ́ motivation
                                   #Test Anxiety (ANXTEST)
                                   "ST118Q01NA","ST118Q02NA","ST118Q03NA","ST118Q04NA","ST118Q05NA","ANXTEST",
                                   #Achievement motivation (MOTIVAT)
                                   "ST119Q01NA","ST119Q02NA","ST119Q03NA","ST119Q04NA","ST119Q05NA","MOTIVAT",
                                   
                                   
                                   #Parental Supports
                                   #Parents emotional support (EMOSUPS)
                                   "ST123Q01NA","ST123Q02NA","ST123Q03NA","ST123Q04NA","EMOSUPS",
                                   
                                   
                                   #Environmental awareness and optimism
                                   #Environmental Awareness (ENVAWARE)
                                   "ST092Q01TA","ST092Q02TA","ST092Q04TA","ST092Q05TA","ST092Q06NA","ST092Q08NA",
                                   "ST092Q09NA","ENVAWARE",
                                   #Item parameters for Environmental optimism (ENVOPT)
                                   "ST093Q01TA","ST093Q03TA","ST093Q04TA","ST093Q05TA","ST093Q06TA",
                                   "ST093Q07NA","ST093Q08NA","ENVOPT",
                                   
                                   # Derived indeces from the questionnaire
                                   "CULTPOSS","HEDRES","WEALTH","ICTRES","HOMEPOS",
                                   
                                   
                                   "STRATUM","ESCS"),
                                 # variables are numerical
                                 school = c(#"SCHSIZE","CLSIZE","SCHLTYPE",
                                            
                                            #School Leadership
                                            #Educational leadership (LEAD)
                                            "LEAD",
                                            #Curricular development (LEADCOM)
                                            "LEADCOM",
                                            #Instructional leadership (LEADINST)
                                            "LEADINST",
                                            #Professional development (LEADPD)
                                            "LEADPD",
                                            #Teachers participation (LEADTCH)
                                            "LEADTCH",
                                            
                                            #School Resources
                                            #Shortage of educational material (EDUSHORT)
                                            "EDUSHORT",
                                            #Shortage of educational staff (STAFFSHORT)
                                            "STAFFSHORT",
                                            
                                            #School Climate
                                            #Student-related factors affecting school climate (STUBEHA)
                                            "STUBEHA",
                                            #Teacher-related factors affecting school climate (TEACHBEHA)
                                            "TEACHBEHA",
                                            
                                            #Extra-curricular activities at school
                                            "CREACTIV",
                                            
                                            #Quantity of teaching staff at school
                                            #total number of teachers at their school (TOTAT)
                                            "TOTAT",
                                            #proportion of fully certified teachers (PROATCE)
                                            "PROATCE",
                                            #proportion of teachers with an ISCED 5A bachelor qualification (PROAT5AB)
                                            "PROAT5AB",
                                            #proportion of teachers with an ISCED 5A master qualification (PROAT5AM)
                                            "PROAT5AM",
                                            #proportion of teachers with an ISCED level 6 qualification (PROAT6)
                                            "PROAT6",
                                            #student-teacher ratio (STRATIO)
                                            "STRATIO",
                                            #proportion of science teachers (PROSTAT)
                                            "PROSTAT"
                                            ),
                                countries = "MYS"
                                )    


write.csv(pisaMas2015,'pisaMalaysia.csv',row.names = FALSE)
pisaMalaysia <- read.csv("pisaMalaysia.csv")



# ---------------------------------- #
# Exploring the data
# ---------------------------------- #

#Check data structure of the merged files 
str(pisaMas2015, list.len=ncol(pisaMas2015))


#This line of code will check for every columns if there's any missing values (NAs)
sapply(pisaMas2015, function(x) sum(is.na(x))) # <---- dari sini boleh cek brapa byk column yg NAs

#how does the data look like?
dim(pisaMas2015)  #shows me the number of rows and columns in the data set
summary(pisaMas2015)

#how many clusters?
length(table(impPISALong$CNTSCHID))
pisaMalaysia[pisaMalaysia==""]  <- NA # This data has blanks.  Let's convert the blanks to "NA"

#what are the different cluster sizes?
table(table(impPISALong$CNTSCHID))

# if you want to see how may unique values there are for each column:
sapply(pisaMalaysia, function(x) length(unique(x)))





# ---------------------------------- #
# e.	Model Evaluations LAVAAN
# ---------------------------------- #

# Analyze the data after imputation using mice
imputedPisa <- mice(pisaMas2015,maxit = 0,m=5)
summary(imputedPisa)

#impPISA <- complete(imputedPisa,"repeated",include = TRUE) #imputed data append in new collumn
impPISALong <- complete(imputedPisa,"long",include = FALSE) #imputed data stacked in the same collumn, exclude original data

#check the structure of the new imputed data
str(imputedPisa, list.len=ncol(imputedPisa))

#This line of code will check for every columns if there's any missing values (NAs)
sapply(impPISALong, function(x) sum(is.na(x))) # <---- dari sini boleh cek brapa byk column yg NAs

#dropped
impPISALong$.id <- NULL
impPISALong$.imp<-NULL

# few groups that can be focused
# SEX (1 = female, 2 = male)
# CNTSCHID - 225 schools
# STRATUM - Various Types (1-9)
# SCHLTYPE - Types of School (1-Private Independent, 2-Private Govt-Dependent, 3-Public )
impPISALong$ST004D01T <- factor(impPISALong$ST004D01T,labels = c("Female","Male"))
female <- subset(impPISALong, c(ST004D01T=="Female"))
NationalSecondary <- subset(impPISALong,STRATUM == "MYS0101")

# ----------------------------------------- ANALYSIS NO 1 ----------------------------------------- #
# one level model with all variables from Science learning in school
# took quite some time to process - need to investigate more
# lots of warning after executing fit.surv1
# ----------------------------------------- ANALYSIS NO 1 ----------------------------------------- #

model.pisa1 <- '
        science =~ PV1SCIE + PV2SCIE + PV3SCIE + PV4SCIE + PV5SCIE + PV6SCIE + PV7SCIE + PV8SCIE + PV9SCIE + PV10SCIE

        #Science learning in school
        #Inquiry-based science teaching and learning practices (IBTEACH)
        IBTEACH   =~ ST098Q01TA+ST098Q02TA+ST098Q03NA+ST098Q05TA+ST098Q06TA+ST098Q07TA+ST098Q08NA+ST098Q09TA+ST098Q10NA 
        #Teacher support in a science classes (TEACHSUP)          
        TEACHSUP  =~ ST100Q01TA+ST100Q02TA+ST100Q03TA+ST100Q04TA+ST100Q05TA
        #Teacher-directed science instruction (TDTEACH)
        TDTEACH   =~ ST103Q01NA+ST103Q03NA+ST103Q08NA+ST103Q11NA
        #Perceived Feedback (PERFEED)
        PERFEED   =~ ST104Q01NA+ST104Q02NA+ST104Q03NA+ST104Q04NA+ST104Q05NA
        #Adaption of instruction (ADINST)
        ADINST    =~ ST107Q01NA+ST107Q02NA+ST107Q03NA
        #Instrumental motivation (INSTSCIE)
        INSTSCIE  =~ ST113Q01TA+ST113Q02TA+ST113Q03TA+ST113Q04TA
        #Disciplinary climate in science classes (DISCLISCI)        
        DISCLISCI =~ ST097Q01TA+ST097Q02TA+ST097Q03TA+ST097Q04TA+ST097Q05TA



        science ~ ST004D01T + SCHLTYPE + ESCS

'

des.rep1 <- svrepdesign( weights=~W_FSTUWT, data=impPISALong,repweights="W_FSTURWT[0-9]+", 
                         type="Fay", rho=0.5,combined.weights = TRUE)
fit1 <- lavaan(model.pisa1, data=impPISALong, auto.var=TRUE, std.lv=TRUE, meanstructure=TRUE, 
               int.ov.free=TRUE,estimator="MLM")
fit.surv1 <-lavaan.survey(lavaan.fit=fit1, survey.design=des.rep1)
fitmeasures(fit.surv1,c("cfi","rmsea","srmr"))


#analysis 1
sink("analysis1.txt")
summary(fit.surv1, fit.measures = TRUE, standardized = TRUE)

sink()

# ----------------------------------------- ANALYSIS NO 2 ----------------------------------------- #
# one level model with all variables from Science learning in school
# nice model - with no warning after executing fit.surv2 - good sign that this model fit nicely
# ----------------------------------------- ANALYSIS NO 2 ----------------------------------------- #


model.pisa2 <- '
            #science =~ PV1SCIE + PV2SCIE + PV3SCIE + PV4SCIE + PV5SCIE + PV6SCIE + PV7SCIE + PV8SCIE + PV9SCIE + PV10SCIE
            
            #Science-related dispositions / Other science related
            #Science self-efficacy (SCIEEFF)  
            SCIEEFF =~ ST129Q01TA+ST129Q02TA+ST129Q03TA+ST129Q04TA+ST129Q05TA+ST129Q06TA+ST129Q07TA+ST129Q08TA
            #Epistemological beliefs (EPIST)
            #EPIST   =~ ST131Q01NA+ST131Q03NA+ST131Q04NA+ST131Q06NA+ST131Q08NA+ST131Q11NA
            #Science activities (SCIEACT)
            #SCIEACT =~ ST146Q01TA+ST146Q02TA+ST146Q03TA+ST146Q04TA+ST146Q05TA+ST146Q06NA+ST146Q07NA+ST146Q08NA+ST146Q09NA
            
            
            #science ~ SCIEEFF + EPIST + SCIEACT  + SCHLTYPE + ESCS 

'
des.rep2 <- svrepdesign( weights=~W_FSTUWT, data=impPISALong,repweights="W_FSTURWT[0-9]+", 
                         type="Fay", rho=0.5,combined.weights = TRUE)
fit2 <- lavaan(model.pisa2, data=impPISALong,auto.var=TRUE, std.lv=TRUE, meanstructure=TRUE, 
               int.ov.free=TRUE,estimator="MLM")
fit.surv2 <-lavaan.survey(lavaan.fit=fit2, survey.design=des.rep2)

fitmeasures(fit.surv2,c("cfi","rmsea","srmr"))

#analysis 2
sink("analysis2.txt")
summary(fit.surv2, fit.measures = TRUE, standardized = TRUE)
 fitmeasures(fit.surv2,"chisq")
sink()

measurementInvariance(model = fit2,data = impPISALong,group="STRATUM",strict = TRUE)

# ----------------------------------------- ANALYSIS NO 3 ----------------------------------------- #
# one level model - SCIELEARNSCHOOL
# use the raw questionnaire data as in tutorials from intsvy package
# USING impPISALong data
# measurement model -> (=~ is measured by) | (~ is regressed on) | (~~ is correlated with)
# ----------------------------------------- ANALYSIS NO 3 ----------------------------------------- #

model.pisa3 <- '
            science =~ PV1SCIE + PV2SCIE + PV3SCIE + PV4SCIE + PV5SCIE + PV6SCIE + PV7SCIE + PV8SCIE + PV9SCIE + PV10SCIE
            
            #Students’ dispositions for collaborative problem solving
            #Enjoy co-operation (COOPERATE)
            COOPERATE =~ ST082Q02NA+ST082Q03NA+ST082Q08NA+ST082Q12NA
            #Value co-operation (CPSVALUE)
            CPSVALUE =~ ST082Q01NA+ST082Q09NA+ST082Q13NA+ST082Q14NA
            
            science ~ COOPERATE + CPSVALUE + ST004D01T + SCHLTYPE + ESCS

'

des.rep3 <- svrepdesign( weights=~W_FSTUWT, data=impPISALong,repweights="W_FSTURWT[0-9]+", 
                         type="Fay", rho=0.5,combined.weights = TRUE)
fit3 <- lavaan(model.pisa3, data=impPISALong,auto.var=TRUE, std.lv=TRUE, meanstructure=TRUE, 
               int.ov.free=TRUE,estimator="MLM")
fit.surv3 <-lavaan.survey(lavaan.fit=fit3, survey.design=des.rep3)
# 10 mins with cluster = "CNTSCHID" included 
# 8 mins with no cluster = "CNTSCHID" included + with correlation 
# 8 mins NEW DESIGN - FAILED

#analysis 3 - no correlation + with cluster
sink("analysis3.txt")
summary(fit.surv3, fit.measures = TRUE, standardized = TRUE)
fitmeasures(fit.surv3,"chisq")
sink()



# ----------------------------------------- ANALYSIS NO 4 ----------------------------------------- #
# one level model - OTHERSCIERELATED
# use the raw questionnaire data as in tutorials from intsvy package
# USING impPISALong data
# measurement model -> (=~ is measured by) | (~ is regressed on) | (~~ is correlated with)
# ----------------------------------------- ANALYSIS NO 4 ----------------------------------------- #


model.pisa4 <- '
            science =~ PV1SCIE + PV2SCIE + PV3SCIE + PV4SCIE + PV5SCIE + PV6SCIE + PV7SCIE + PV8SCIE + PV9SCIE + PV10SCIE
            
            #Sense of Belonging in School
            #Sense of Belonging to School (BELONG)
            BELONG =~ ST034Q01TA+ST034Q02TA+ST034Q03TA+ST034Q04TA+ST034Q05TA+ST034Q06TA
            
            science ~ BELONG + ST004D01T + SCHLTYPE + ESCS

'

des.rep4 <- svrepdesign( weights=~W_FSTUWT, data=impPISALong,repweights="W_FSTURWT[0-9]+", 
                         type="Fay", rho=0.5,combined.weights = TRUE)

fit4 <- lavaan(model.pisa4, data=impPISALong,auto.var=TRUE, std.lv=TRUE, meanstructure=TRUE, 
               int.ov.free=TRUE,estimator="MLM")
fit.surv4 <-lavaan.survey(lavaan.fit=fit4, survey.design=des.rep4)
# 7 mins to finish execute!
# 8 mins to finish execute! NEW DESIGN  

#analysis 4
sink("analysis4.txt")
summary(fit.surv4, fit.measures = TRUE, standardized = TRUE) 
fitmeasures(fit.surv4,"chisq")
sink()


# ----------------------------------------- ANALYSIS NO 5 ----------------------------------------- #
# one level 
# use the raw questionnaire data as in tutorials from intsvy package
# USING impPISALong data
# measurement model -> (=~ is measured by) | (~ is regressed on) | (~~ is correlated with)
# ----------------------------------------- ANALYSIS NO 5 ----------------------------------------- #


model.pisa5 <- '
            science =~ PV1SCIE + PV2SCIE + PV3SCIE + PV4SCIE + PV5SCIE + PV6SCIE + PV7SCIE + PV8SCIE + PV9SCIE + PV10SCIE
            
            #Interest in science
            #Enjoyment of science (JOYSCIE)
            JOYSCIE =~ ST094Q01NA+ST094Q02NA+ST094Q03NA+ST094Q04NA+ST094Q05NA
            #Interest in broad science topics (INTBRSCI)
            INTBRSCI =~ ST095Q04NA+ST095Q07NA+ST095Q08NA+ST095Q13NA+ST095Q15NA
            
            science ~ JOYSCIE + INTBRSCI + ST004D01T + SCHLTYPE + ESCS
'

des.rep5 <- svrepdesign( weights=~W_FSTUWT, data=impPISALong,repweights="W_FSTURWT[0-9]+", 
                         type="Fay", rho=0.5,combined.weights = TRUE)

fit5 <- lavaan(model.pisa5, data=impPISALong,auto.var=TRUE, std.lv=TRUE, meanstructure=TRUE, 
               int.ov.free=TRUE,estimator="MLM")
fit.surv5 <-lavaan.survey(lavaan.fit=fit5, survey.design=des.rep5)

#analysis 5
sink("analysis5COMBINE.txt")
summary(fit.surv5, fit.measures = TRUE, standardized = TRUE)
fitmeasures(fit.surv5,"chisq")
sink()


# ----------------------------------------- ANALYSIS NO 6 ----------------------------------------- #
# one level 
# use the raw questionnaire data as in tutorials from intsvy package
# USING impPISALong data
# measurement model -> (=~ is measured by) | (~ is regressed on) | (~~ is correlated with)
# ----------------------------------------- ANALYSIS NO 6 ----------------------------------------- #

model.pisa6 <- '
            science =~ PV1SCIE + PV2SCIE + PV3SCIE + PV4SCIE + PV5SCIE + PV6SCIE + PV7SCIE + PV8SCIE + PV9SCIE + PV10SCIE
            
            #Students ́ motivation
            #Test Anxiety (ANXTEST)
            ANXTEST =~ ST118Q01NA+ST118Q02NA+ST118Q03NA+ST118Q04NA+ST118Q05NA
            #Achievement motivation (MOTIVAT)
            MOTIVAT =~ ST119Q01NA+ST119Q02NA+ST119Q03NA+ST119Q04NA+ST119Q05NA
            
            science ~ ANXTEST + MOTIVAT + ST004D01T + SCHLTYPE + ESCS

'

des.rep6 <- svrepdesign( weights=~W_FSTUWT, data=impPISALong,repweights="W_FSTURWT[0-9]+", 
                         type="Fay", rho=0.5,combined.weights = TRUE)

fit6 <- lavaan(model.pisa6, data=impPISALong,auto.var=TRUE, std.lv=TRUE, meanstructure=TRUE, 
               int.ov.free=TRUE,estimator="MLM")
fit.surv6 <-lavaan.survey(lavaan.fit=fit6, survey.design=des.rep6)


#analysis 6
sink("analysis6COMBINE.txt")
summary(fit.surv6, fit.measures = TRUE, standardized = TRUE)
fitmeasures(fit.surv6,"chisq")
sink()


# ----------------------------------------- ANALYSIS NO 7 ----------------------------------------- #
# one level model
# Students’ dispositions for collaborative problem solving
# use the raw questionnaire data as in tutorials from intsvy package
# USING impPISALong data
# measurement model -> (=~ is measured by) | (~ is regressed on) | (~~ is correlated with)
# ----------------------------------------- ANALYSIS NO 7 ----------------------------------------- #


model.pisa7 <- '
            science =~ PV1SCIE + PV2SCIE + PV3SCIE + PV4SCIE + PV5SCIE + PV6SCIE + PV7SCIE + PV8SCIE + PV9SCIE + PV10SCIE
            
            #Parental Supports
            #Parents emotional support (EMOSUPS)
            EMOSUPS =~ ST123Q01NA+ST123Q02NA+ST123Q03NA+ST123Q04NA            
            
            science ~  EMOSUPS + ST004D01T + SCHLTYPE + ESCS

'

fit7 <- lavaan(model.pisa7, data=impPISALong,auto.var=TRUE, std.lv=TRUE, meanstructure=TRUE, 
               int.ov.free=TRUE,estimator="MLM")
des.rep7 <- svrepdesign( weights=~W_FSTUWT, data=impPISALong,repweights="W_FSTURWT[0-9]+", 
                         type="Fay", rho=0.5,combined.weights = TRUE)
fit.surv7 <-lavaan.survey(lavaan.fit=fit7, survey.design=des.rep7)
# 1 mins to finish execute!


#analysis 7
sink("analysis7.txt")
summary(fit.surv7, fit.measures = TRUE, standardized = TRUE)
fitmeasures(fit.surv7,"chisq")
sink()

measurementInvariance(model = fit7,data = impPISALong,group="STRATUM",strict = TRUE)

# ----------------------------------------- ANALYSIS NO 8 ----------------------------------------- #
# one level model
# Students’ dispositions for collaborative problem solving
# use the raw questionnaire data as in tutorials from intsvy package
# USING impPISALong data
# measurement model -> (=~ is measured by) | (~ is regressed on) | (~~ is correlated with)
# ----------------------------------------- ANALYSIS NO 7 ----------------------------------------- #


model.pisa8 <- '
            science =~ PV1SCIE + PV2SCIE + PV3SCIE + PV4SCIE + PV5SCIE + PV6SCIE + PV7SCIE + PV8SCIE + PV9SCIE + PV10SCIE
            
            #Environmental awareness and optimism
            #Environmental Awareness (ENVAWARE)
            ENVAWARE =~ ST092Q01TA+ST092Q02TA+ST092Q04TA+ST092Q05TA+ST092Q06NA+ST092Q08NA+ST092Q09NA
            #Item parameters for Environmental optimism (ENVOPT)
            ENVOPT =~ ST093Q01TA+ST093Q03TA+ST093Q04TA+ST093Q05TA+ST093Q06TA+ST093Q07NA+ST093Q08NA
            
            science ~  ENVAWARE + ENVOPT + ST004D01T + SCHLTYPE + ESCS

'

des.rep8 <- svrepdesign( weights=~W_FSTUWT, data=impPISALong,repweights="W_FSTURWT[0-9]+", 
                         type="Fay", rho=0.5,combined.weights = TRUE)

fit8 <- lavaan(model.pisa8, data=impPISALong,auto.var=TRUE, std.lv=TRUE, meanstructure=TRUE, 
               int.ov.free=TRUE,estimator="MLM")
fit.surv8 <-lavaan.survey(lavaan.fit=fit8, survey.design=des.rep8)



#analysis 8
sink("analysis8.txt")
summary(fit.surv8, fit.measures = TRUE, standardized = TRUE)
fitmeasures(fit.surv8,"chisq")
sink()


# ----------------------------------------- ANALYSIS NO 9 ----------------------------------------- #
# two level model
# Students’ dispositions for collaborative problem solving
# use the raw questionnaire data as in tutorials from intsvy package
# USING impPISALong data
# measurement model -> (=~ is measured by) | (~ is regressed on) | (~~ is correlated with)
# ----------------------------------------- ANALYSIS NO 7 ----------------------------------------- #


model.pisa9 <- '
    level: within     
          science =~ PV1SCIE + PV2SCIE + PV3SCIE + PV4SCIE + PV5SCIE + PV6SCIE + PV7SCIE + PV8SCIE + PV9SCIE + PV10SCIE

          #Science-related dispositions / Other science related
          #Science self-efficacy (SCIEEFF)  
          selfEfficay =~ ST129Q01TA+ST129Q02TA+ST129Q03TA+ST129Q04TA+ST129Q05TA+ST129Q06TA+ST129Q07TA+ST129Q08TA
          #Epistemological beliefs (EPIST)
          epistBelief   =~ ST131Q01NA+ST131Q03NA+ST131Q04NA+ST131Q06NA+ST131Q08NA+ST131Q11NA
          #Science activities (SCIEACT)
          scieActivities =~ ST146Q01TA+ST146Q02TA+ST146Q03TA+ST146Q04TA+ST146Q05TA+ST146Q06NA+ST146Q07NA+ST146Q08NA+ST146Q09NA
          
          
          



level: between      
          
          
          #School Leadership
          SchLeadShip =~ LEADINST
          
          

'

des.rep9 <- svrepdesign( weights=~W_FSTUWT, data=impPISALong,repweights="W_FSTURWT[0-9]+", 
                         type="Fay", rho=0.5,combined.weights = TRUE)

fit9 <- lavaan(model.pisa9, data=impPISALong,auto.var=TRUE, std.lv=TRUE, meanstructure=TRUE, 
               int.ov.free=TRUE,estimator="MLM",cluster = "CNTSCHID")
fit9 <- sem(model.pisa9, data=impPISALong, std.lv=TRUE, meanstructure=TRUE, 
            int.ov.free=TRUE,estimator="MLM",cluster = "CNTSCHID")

fit.surv9 <-lavaan.survey(lavaan.fit=fit9, survey.design=des.rep9)



#analysis 9
sink("analysis9.txt")
summary(fit.surv9, fit.measures = TRUE, standardized = TRUE)
fitmeasures(fit.surv9,c("cfi","rmsea","srmr"))
sink()

inspect(fit9, what="list")
