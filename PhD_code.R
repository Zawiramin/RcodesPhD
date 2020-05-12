#install.packages('intsvy')
#install.packages('foreign')
#install.packages("lavaan")
#install.packages("lavaan.survey", dependencies=TRUE)
#install.packages("agrmt")
#install.packages("semPlot")
#install.packages("semTools")
#install.packages("carData")


library("foreign") #penting 1 - read spss/sav
library("intsvy") #penting 2 - use select.merge
library("lavaan") #penting
library("lavaan.survey")
library("mice")
library("semTools")
library("semPlot")
library("car")
library("tidyverse") #- pipe %>% 

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
                                 student = c(
                                   #Sex 
                                   "ST004D01T",
                                   #Science learning in school
                                   #Inquiry-based science teaching and learning practices (IBTEACH) 4 Points Scale
                                   #Items reverse-coded
                                   "ST098Q01TA","ST098Q02TA","ST098Q03NA","ST098Q05TA","ST098Q06TA","ST098Q07TA",
                                   "ST098Q08NA","ST098Q09TA","IBTEACH",
                                   #Teacher support in a science classes (TEACHSUP)
                                   "ST100Q01TA","ST100Q02TA","ST100Q03TA","ST100Q04TA","ST100Q05TA","TEACHSUP",
                                   #Teacher-directed science instruction (TDTEACH)
                                   "ST103Q01NA","ST103Q03NA","ST103Q08NA","ST103Q11NA","TDTEACH", 
                                   #Perceived Feedback (PERFEED)
                                   "ST104Q01NA","ST104Q02NA","ST104Q03NA","ST104Q04NA","ST104Q05NA","PERFEED",
                                   #Adaption of instruction (ADINST)
                                   "ST107Q01NA","ST107Q02NA","ST107Q03NA","ADINST",
                                   #Instrumental motivation (INSTSCIE)  4 Points Scale
                                   #Items reverse-coded
                                   "ST113Q01TA","ST113Q02TA","ST113Q03TA","ST113Q04TA","INSTSCIE",
                                   #Disciplinary climate in science classes (DISCLISCI)
                                   "ST097Q01TA","ST097Q02TA","ST097Q03TA","ST097Q04TA","ST097Q05TA","DISCLISCI",
                                   
                                   
                                   #Science-related dispositions / Other science related
                                   #Science self-efficacy (SCIEEFF) - effectiveness 4 Points Scale
                                   #Items reverse-coded
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
                                   #Sense of Belonging to School (BELONG) 4 Points Scale
                                   "ST034Q01TA","ST034Q04TA","ST034Q06TA",
                                   #Items reverse-coded 
                                   "ST034Q02TA","ST034Q03TA","ST034Q05TA","BELONG",
                                   
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
                                   #Environmental Awareness (ENVAWARE) 4 Points Scale
                                   "ST092Q01TA","ST092Q02TA","ST092Q04TA","ST092Q05TA","ST092Q06NA","ST092Q08NA",
                                   "ST092Q09NA","ENVAWARE",
                                   #Item parameters for Environmental optimism (ENVOPT) 3 Points Scale
                                   #Items reverse-coded 
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
#pisaMalaysia <- read.csv("pisaMalaysia.csv")



# ---------------------------------- #
# Exploring the data
# ---------------------------------- #

#Check data structure of the merged files 
str(pisaMas2015, list.len=ncol(pisaMas2015))

#This line of code will check for every columns if there's any missing values (NAs)
sapply(pisaMas2015, function(x) sum(is.na(x))) # <---- dari sini boleh cek brapa byk column yg NAs

#how does the data look like?
dim(pisaMas2015)  #shows me the number of rows and columns in the data set
#summary(pisaMas2015)

findNA2015 <-data.frame(sapply(pisaListWdeletion, function(x) sum(is.na(x))))
#listwise deletion of missing values
pisaListWdeletion <- na.omit(pisaMas2015)


# ---------------------------------- #
# e.	Model Evaluations LAVAAN
# ---------------------------------- #

# Analyze the data after imputation using mice maxit = 0 give a default 5 iterations
imputedPisa <- mice(pisaMas2015,m=5,maxit = 0)
#summary(imputedPisa)

#imputed data append in new column
#impPISA <- complete(imputedPisa,"repeated",include = TRUE)

#imputed data stacked in the same column, exclude original data
impPISALong <- complete(imputedPisa,"long",include = FALSE) 

#This line of code will check for every columns if there's any missing values (NAs)
sapply(impPISALong, function(x) sum(is.na(x))) # <---- dari sini boleh cek brapa byk column yg NAs

#check the structure of the new imputed data
#str(impPISALong, list.len=ncol(impPISALong))

#how many clusters?
#length(table(impPISALong$CNTSCHID))
#pisaMalaysia[pisaMalaysia==""]  <- NA # This data has blanks.  Let's convert the blanks to "NA"


#what are the different cluster sizes?
table(table(impPISALong$CNTSCHID))

# if you want to see how may unique values there are for each column:
#sapply(pisaMalaysia, function(x) length(unique(x)))


# ----------------------------------------- Testing Items reverse-coded ----------------------------------------- #

# 4 Points Scale
#"IBTEACH",
impPISALong$RevST098Q01TA <- recode(impPISALong$ST098Q01TA,"1=4;2=3;3=2;4=1",as.factor = FALSE)
impPISALong$RevST098Q02TA <- recode(impPISALong$ST098Q02TA,"1=4;2=3;3=2;4=1",as.factor = FALSE)
impPISALong$RevST098Q03NA <- recode(impPISALong$ST098Q03NA,"1=4;2=3;3=2;4=1",as.factor = FALSE)
impPISALong$RevST098Q05TA <- recode(impPISALong$ST098Q05TA,"1=4;2=3;3=2;4=1",as.factor = FALSE)
impPISALong$RevST098Q06TA <- recode(impPISALong$ST098Q06TA,"1=4;2=3;3=2;4=1",as.factor = FALSE)
impPISALong$RevST098Q07TA <- recode(impPISALong$ST098Q07TA,"1=4;2=3;3=2;4=1",as.factor = FALSE)
impPISALong$RevST098Q08NA <- recode(impPISALong$ST098Q08NA,"1=4;2=3;3=2;4=1",as.factor = FALSE)
impPISALong$RevST098Q09TA <- recode(impPISALong$ST098Q09TA,"1=4;2=3;3=2;4=1",as.factor = FALSE)
#pisaMas2015$NewIBTEACH <- with(pisaMas2015,(RevST098Q01TA+RevST098Q02TA+RevST098Q03NA+RevST098Q05TA+
#                                             RevST098Q06TA+RevST098Q07TA+RevST098Q08NA+RevST098Q09TA)/8)

#"INSTSCIE",
impPISALong$RevST113Q01TA <- recode(impPISALong$ST113Q01TA,"1=4;2=3;3=2;4=1",as.factor = FALSE)
impPISALong$RevST113Q02TA <- recode(impPISALong$ST113Q02TA,"1=4;2=3;3=2;4=1",as.factor = FALSE)
impPISALong$RevST113Q03TA <- recode(impPISALong$ST113Q03TA,"1=4;2=3;3=2;4=1",as.factor = FALSE)
impPISALong$RevST113Q04TA <- recode(impPISALong$ST113Q04TA,"1=4;2=3;3=2;4=1",as.factor = FALSE)
#pisaMas2015$NewINSTSCIE <- with(pisaMas2015,(RevST113Q01TA+RevST113Q02TA+RevST113Q03TA+RevST113Q04TA)/4)

#"SCIEEFF",
impPISALong$RevST129Q01TA <- recode(impPISALong$ST129Q01TA,"1=4;2=3;3=2;4=1",as.factor = FALSE)
impPISALong$RevST129Q02TA <- recode(impPISALong$ST129Q02TA,"1=4;2=3;3=2;4=1",as.factor = FALSE)
impPISALong$RevST129Q03TA <- recode(impPISALong$ST129Q03TA,"1=4;2=3;3=2;4=1",as.factor = FALSE)
impPISALong$RevST129Q04TA <- recode(impPISALong$ST129Q04TA,"1=4;2=3;3=2;4=1",as.factor = FALSE)
impPISALong$RevST129Q05TA <- recode(impPISALong$ST129Q05TA,"1=4;2=3;3=2;4=1",as.factor = FALSE)
impPISALong$RevST129Q06TA <- recode(impPISALong$ST129Q06TA,"1=4;2=3;3=2;4=1",as.factor = FALSE)
impPISALong$RevST129Q07TA <- recode(impPISALong$ST129Q07TA,"1=4;2=3;3=2;4=1",as.factor = FALSE)
impPISALong$RevST129Q08TA <- recode(impPISALong$ST129Q08TA,"1=4;2=3;3=2;4=1",as.factor = FALSE)
#pisaMas2015$NewSCIEEFF <- with(pisaMas2015,(RevST129Q01TA+RevST129Q02TA+RevST129Q03TA+RevST129Q04TA+
#                                              RevST129Q05TA+RevST129Q06TA+RevST129Q07TA+RevST129Q08TA)/8)

#"BELONG",
impPISALong$RevST034Q02TA <- recode(impPISALong$ST034Q02TA,"1=4;2=3;3=2;4=1",as.factor = FALSE)
impPISALong$RevST034Q03TA <- recode(impPISALong$ST034Q03TA,"1=4;2=3;3=2;4=1",as.factor = FALSE)
impPISALong$RevST034Q05TA <- recode(impPISALong$ST034Q05TA,"1=4;2=3;3=2;4=1",as.factor = FALSE)
#pisaMas2015$NewBELONG <- with(pisaMas2015,(ST034Q01TA+RevST034Q02TA+RevST034Q03TA+ST034Q04TA+RevST034Q05TA+ST034Q06TA)/6)

# 3 Points Scale
#"ENVOPT",
impPISALong$RevST093Q01TA <- recode(impPISALong$ST093Q01TA,"1=3;2=2;3=1",as.factor = FALSE)
impPISALong$RevST093Q03TA <- recode(impPISALong$ST093Q03TA,"1=3;2=2;3=1",as.factor = FALSE)
impPISALong$RevST093Q04TA <- recode(impPISALong$ST093Q04TA,"1=3;2=2;3=1",as.factor = FALSE)
impPISALong$RevST093Q05TA <- recode(impPISALong$ST093Q05TA,"1=3;2=2;3=1",as.factor = FALSE)
impPISALong$RevST093Q06TA <- recode(impPISALong$ST093Q06TA,"1=3;2=2;3=1",as.factor = FALSE)
impPISALong$RevST093Q07NA <- recode(impPISALong$ST093Q07NA,"1=3;2=2;3=1",as.factor = FALSE)
impPISALong$RevST093Q08NA <- recode(impPISALong$ST093Q08NA,"1=3;2=2;3=1",as.factor = FALSE)
#pisaMas2015$NewENVOPT <- with(pisaMas2015,(RevST093Q01TA+RevST093Q03TA+RevST093Q04TA+RevST093Q05TA+RevST093Q06TA+
#                                             RevST093Q07NA+RevST093Q08NA)/7)





#This line of code will check for every columns if there's any missing values (NAs)
#sapply(impPISALong, function(x) sum(is.na(x))) # <---- dari sini boleh cek brapa byk column yg NAs
#dropImpPisa <- na.omit(impPISALong) # <-- excluding NAs
#sapply(dropImpPisa, function(x) sum(is.na(x))) # <---- dari sini boleh cek brapa byk column yg NAs
#check the structure of the new imputed data
#str(dropImpPisa, list.len=ncol(impPISALong))

#dropped
#impPISALong$.id <- NULL
#impPISALong$.imp<-NULL

#write.csv(impPISALong,'impPISALong.csv',row.names = FALSE)

# few groups that can be focused
# SEX (1 = female, 2 = male)
# CNTSCHID - 225 schools
# STRATUM - Various Types (1-9)
# SCHLTYPE - Types of School (1-Private Independent, 2-Private Govt-Dependent, 3-Public )
#impPISALong$ST004D01T <- factor(impPISALong$ST004D01T,labels = c("Female","Male"))
#female <- subset(impPISALong, c(ST004D01T=="Female"))
#NationalSecondary <- subset(impPISALong,STRATUM == "MYS0101")

# ----------------------------------------- ANALYSIS NO 1 ----------------------------------------- #
# one level model with all variables from Science learning in school
# took quite some time to process - need to investigate more
# lots of warning after executing fit.surv1
# ----------------------------------------- ANALYSIS NO 1 ----------------------------------------- #
des.rep <- svrepdesign( weights=~W_FSTUWT, data=pisaMas2015,repweights="W_FSTURWT[0-9]+", 
                         type="Fay", rho=0.5,combined.weights = TRUE)

#after drop ST098Q07TA ST098Q03NA ST098Q08NA from IBTEACH
#model.pisa1 <- '
#        science =~ PV1SCIE + PV2SCIE + PV3SCIE + PV4SCIE + PV5SCIE + PV6SCIE + PV7SCIE + PV8SCIE + PV9SCIE + PV10SCIE

        
#        IBTEACH   =~ ST098Q01TA+ST098Q02TA+ST098Q05TA+ST098Q06TA+ST098Q09TA+ST098Q07TA+ST098Q03NA+ST098Q08NA 
#        TEACHSUP  =~ ST100Q01TA+ST100Q02TA+ST100Q03TA+ST100Q04TA+ST100Q05TA
#        TDTEACH   =~ ST103Q01NA+ST103Q03NA+ST103Q08NA+ST103Q11NA
#        PERFEED   =~ ST104Q01NA+ST104Q02NA+ST104Q03NA+ST104Q04NA+ST104Q05NA
#        ADINST    =~ ST107Q01NA+ST107Q02NA+ST107Q03NA
#        INSTSCIE  =~ ST113Q01TA+ST113Q02TA+ST113Q03TA+ST113Q04TA 
#        DISCLISCI =~ ST097Q01TA+ST097Q02TA+ST097Q03TA+ST097Q04TA+ST097Q05TA

#        science ~ IBTEACH + ESCS + TEACHSUP + TDTEACH + PERFEED + ADINST + INSTSCIE + DISCLISCI

#'

#fit1 <- lavaan(model.pisa1, data=pisaMas2015, auto.var=TRUE, std.lv=TRUE, meanstructure=TRUE, 
#               int.ov.free=TRUE,estimator="MLM")
#APPLY THE RUBIN TECHNIQUE
#fit.surv1 <-lavaan.survey(lavaan.fit=fit1, survey.design=des.rep)
#fitmeasures(fit.surv1,c("cfi","tli","rmsea","srmr"))
#summary(fit.surv1, fit.measures = TRUE, standardized = TRUE,modindices=TRUE)

recode.pisa1 <-'
        science =~ PV1SCIE + PV2SCIE + PV3SCIE + PV4SCIE + PV5SCIE + PV6SCIE + PV7SCIE + PV8SCIE + PV9SCIE + PV10SCIE

        #IBTEACH   =~ RevST098Q01TA+RevST098Q02TA+RevST098Q03NA+RevST098Q05TA+RevST098Q06TA+RevST098Q07TA+RevST098Q08NA+RevST098Q09TA
        INSTSCIE  =~ RevST113Q01TA+RevST113Q02TA+RevST113Q03TA+RevST113Q04TA
        
        #science ~ IBTEACH + INSTSCIE + ESCS


'
recode.fit1 <- lavaan(recode.pisa1, data=pisaMas2015, auto.var=TRUE, std.lv=TRUE, meanstructure=TRUE, 
               int.ov.free=TRUE,estimator="MLM")
#APPLY THE RUBIN TECHNIQUE
recode.fit.surv1 <-lavaan.survey(lavaan.fit=recode.fit1, survey.design=des.rep)
fitmeasures(recode.fit.surv1,c("cfi","tli","rmsea","srmr"))


#analysis 1
sink("analysis1.txt")
summary(recode.fit.surv1, fit.measures = TRUE, standardized = TRUE)

sink()

# ----------------------------------------- ANALYSIS NO 2 ----------------------------------------- #
# one level model with all variables from Science learning in school
# nice model - with no warning after executing fit.surv2 - good sign that this model fit nicely
# ----------------------------------------- ANALYSIS NO 2 ----------------------------------------- #

#"SCIEEFF",
pisaMas2015$RevST129Q01TA <- recode(pisaMas2015$ST129Q01TA,"1=4;2=3;3=2;4=1",as.factor = FALSE)
pisaMas2015$RevST129Q02TA <- recode(pisaMas2015$ST129Q02TA,"1=4;2=3;3=2;4=1",as.factor = FALSE)
pisaMas2015$RevST129Q03TA <- recode(pisaMas2015$ST129Q03TA,"1=4;2=3;3=2;4=1",as.factor = FALSE)
pisaMas2015$RevST129Q04TA <- recode(pisaMas2015$ST129Q04TA,"1=4;2=3;3=2;4=1",as.factor = FALSE)
pisaMas2015$RevST129Q05TA <- recode(pisaMas2015$ST129Q05TA,"1=4;2=3;3=2;4=1",as.factor = FALSE)
pisaMas2015$RevST129Q06TA <- recode(pisaMas2015$ST129Q06TA,"1=4;2=3;3=2;4=1",as.factor = FALSE)
pisaMas2015$RevST129Q07TA <- recode(pisaMas2015$ST129Q07TA,"1=4;2=3;3=2;4=1",as.factor = FALSE)
pisaMas2015$RevST129Q08TA <- recode(pisaMas2015$ST129Q08TA,"1=4;2=3;3=2;4=1",as.factor = FALSE)


model.pisa2 <- '
            science =~ PV1SCIE + PV2SCIE + PV3SCIE + PV4SCIE + PV5SCIE + PV6SCIE + PV7SCIE + PV8SCIE + PV9SCIE + PV10SCIE
              
            SCIEEFF =~ RevST129Q01TA+RevST129Q02TA+RevST129Q03TA+RevST129Q04TA+RevST129Q05TA+RevST129Q06TA+RevST129Q07TA+RevST129Q08TA
            EPIST   =~ ST131Q01NA+ST131Q03NA+ST131Q04NA+ST131Q06NA+ST131Q08NA+ST131Q11NA
            SCIEACT =~ ST146Q01TA+ST146Q02TA+ST146Q03TA+ST146Q04TA+ST146Q05TA+ST146Q06NA+ST146Q07NA+ST146Q08NA+ST146Q09NA
            
            
            science ~ SCIEEFF + EPIST + SCIEACT  + ESCS 

'

fit2 <- lavaan(model.pisa2, data=pisaMas2015,auto.var=TRUE, std.lv=TRUE, meanstructure=TRUE,int.ov.free=TRUE)
#APPLY THE RUBIN TECHNIQUE
fit.surv2 <-lavaan.survey(lavaan.fit=fit2, survey.design=des.rep)
fitmeasures(fit.surv2,c("cfi","tli","rmsea","srmr"))
semPaths(fit.surv2)

#analysis 2
sink("analysis2.txt")
summary(fit.surv2, fit.measures = TRUE, standardized = TRUE)
sink()

measurementInvariance(model = fit2,data = pisaMas2015,group="STRATUM",strict = TRUE)

# ----------------------------------------- ANALYSIS NO 3 ----------------------------------------- #
# one level model - SCIELEARNSCHOOL
# use the raw questionnaire data as in tutorials from intsvy package
# USING impPISALong data
# measurement model -> (=~ is measured by) | (~ is regressed on) | (~~ is correlated with)
# ----------------------------------------- ANALYSIS NO 3 ----------------------------------------- #

# ST082Q02NA ST082Q03NA
model.pisa3 <- '
            science =~ PV1SCIE + PV2SCIE + PV3SCIE + PV4SCIE + PV5SCIE + PV6SCIE + PV7SCIE + PV8SCIE + PV9SCIE + PV10SCIE
            
            COOPERATE =~ ST082Q08NA+ST082Q12NA
            CPSVALUE =~ ST082Q01NA+ST082Q09NA+ST082Q13NA+ST082Q14NA
            
            science ~ COOPERATE + CPSVALUE  + ESCS

'

fit3 <- lavaan(model.pisa3, data=pisaMas2015,auto.var=TRUE, std.lv=TRUE, meanstructure=TRUE, int.ov.free=TRUE)
fit.surv3 <-lavaan.survey(lavaan.fit=fit3, survey.design=des.rep)
fitmeasures(fit.surv3,c("cfi","tli","rmsea","srmr"))

#analysis 3 - no correlation + with cluster
sink("analysis3.txt")
summary(fit.surv3, fit.measures = TRUE, standardized = TRUE)
sink()



# ----------------------------------------- ANALYSIS NO 4 ----------------------------------------- #
# one level model - OTHERSCIERELATED
# use the raw questionnaire data as in tutorials from intsvy package
# USING impPISALong data
# measurement model -> (=~ is measured by) | (~ is regressed on) | (~~ is correlated with)
# ----------------------------------------- ANALYSIS NO 4 ----------------------------------------- #

# ST034Q02TA ST034Q03TA ST034Q05TA
model.pisa4 <- '
            science =~ PV1SCIE + PV2SCIE + PV3SCIE + PV4SCIE + PV5SCIE + PV6SCIE + PV7SCIE + PV8SCIE + PV9SCIE + PV10SCIE
            
            BELONG =~ ST034Q01TA+ST034Q04TA+ST034Q06TA
            
            science ~ BELONG + ESCS

'

fit4 <- lavaan(model.pisa4, data=pisaMas2015,auto.var=TRUE, std.lv=TRUE, meanstructure=TRUE, int.ov.free=TRUE)
fit.surv4 <-lavaan.survey(lavaan.fit=fit4, survey.design=des.rep)
fitmeasures(fit.surv4,c("cfi","tli","rmsea","srmr"))

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
            JOYSCIE =~ ST094Q01NA+ST094Q02NA+ST094Q03NA+ST094Q04NA+ST094Q05NA
            INTBRSCI =~ ST095Q04NA+ST095Q07NA+ST095Q08NA+ST095Q13NA+ST095Q15NA
            
            science ~ JOYSCIE + INTBRSCI + ESCS
'

fit5 <- lavaan(model.pisa5, data=pisaMas2015,auto.var=TRUE, std.lv=TRUE, meanstructure=TRUE, int.ov.free=TRUE)
fit.surv5 <-lavaan.survey(lavaan.fit=fit5, survey.design=des.rep)
fitmeasures(fit.surv5,c("cfi","tli","rmsea","srmr"))

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

fit6 <- lavaan(model.pisa6, data=pisaMas2015,auto.var=TRUE, std.lv=TRUE, meanstructure=TRUE, int.ov.free=TRUE)
fit.surv6 <-lavaan.survey(lavaan.fit=fit6, survey.design=des.rep)


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

fit7 <- lavaan(model.pisa7, data=pisaMas2015,auto.var=TRUE, std.lv=TRUE, meanstructure=TRUE, int.ov.free=TRUE)
fit.surv7 <-lavaan.survey(lavaan.fit=fit7, survey.design=des.rep)
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
# 3 Points Scale
#"ENVOPT",
pisaMas2015$RevST093Q01TA <- recode(pisaMas2015$ST093Q01TA,"1=3;2=2;3=1",as.factor = FALSE)
pisaMas2015$RevST093Q03TA <- recode(pisaMas2015$ST093Q03TA,"1=3;2=2;3=1",as.factor = FALSE)
pisaMas2015$RevST093Q04TA <- recode(pisaMas2015$ST093Q04TA,"1=3;2=2;3=1",as.factor = FALSE)
pisaMas2015$RevST093Q05TA <- recode(pisaMas2015$ST093Q05TA,"1=3;2=2;3=1",as.factor = FALSE)
pisaMas2015$RevST093Q06TA <- recode(pisaMas2015$ST093Q06TA,"1=3;2=2;3=1",as.factor = FALSE)
pisaMas2015$RevST093Q07NA <- recode(pisaMas2015$ST093Q07NA,"1=3;2=2;3=1",as.factor = FALSE)
pisaMas2015$RevST093Q08NA <- recode(pisaMas2015$ST093Q08NA,"1=3;2=2;3=1",as.factor = FALSE)

model.pisa8 <- '
            science =~ PV1SCIE + PV2SCIE + PV3SCIE + PV4SCIE + PV5SCIE + PV6SCIE + PV7SCIE + PV8SCIE + PV9SCIE + PV10SCIE
            

            ENVAWARE =~ ST092Q01TA+ST092Q02TA+ST092Q04TA+ST092Q05TA+ST092Q06NA+ST092Q08NA+ST092Q09NA
            ENVOPT   =~ RevST093Q01TA+RevST093Q03TA+RevST093Q04TA+RevST093Q05TA+RevST093Q06TA+RevST093Q07NA+RevST093Q08NA
            
            science ~  ENVAWARE + ENVOPT  + ESCS

'

fit8 <- lavaan(model.pisa8, data=pisaMas2015,auto.var=TRUE, std.lv=TRUE, meanstructure=TRUE, int.ov.free=TRUE)
fit.surv8 <-lavaan.survey(lavaan.fit=fit8, survey.design=des.rep)
fitmeasures(fit.surv8,c("cfi","tli","rmsea","srmr"))


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

            ENVAWARE =~ ST092Q01TA+ST092Q02TA+ST092Q04TA+ST092Q05TA+ST092Q06NA+ST092Q08NA+ST092Q09NA
            ENVOPT   =~ RevST093Q01TA+RevST093Q03TA+RevST093Q04TA+RevST093Q05TA+RevST093Q06TA+RevST093Q07NA+RevST093Q08NA

    level: between      

          

'

fit9 <- lavaan(model.pisa9, data=pisaMas2015,auto.var=TRUE, std.lv=TRUE, meanstructure=TRUE, 
               int.ov.free=TRUE,cluster = "CNTSCHID")

fit.surv9 <-lavaan.survey(lavaan.fit=fit9, survey.design=des.rep)



#analysis 9
sink("analysis9.txt")
summary(fit.surv9, fit.measures = TRUE, standardized = TRUE)
fitmeasures(fit.surv9,c("cfi","rmsea","srmr"))
sink()

inspect(fit9, what="list")


install.packages("lmerTest")
install.packages("merTools")


