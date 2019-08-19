install.packages('intsvy')
install.packages('foreign')
install.packages("lavaan")
install.packages("lavaan.survey", dependencies=TRUE)
install.packages("agrmt")
install.packages("semPlot")


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
                                 student = c(#Sex
                                   "ST004D01T",
                                   
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
                                   
                                   
                                   "EMOSUPS","SCIEEFF", "EPIST","SCIEACT","STRATUM","ESCS"),
                                 # variables are numerical
                                 school = c("SCHSIZE","CLSIZE","SCHLTYPE",
                                            
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
                                            "TOTAT" # not done yet
                                            
                                            ),
                                countries = "MYS"
                                )    
