# ----------------------------------------- Testing Read data direct  ----------------------------------------- #
# cek data NAs read raw or converted into NAs


StudPisa <- read.spss("CY6_MS_CM2_STU_QQQ.sav")
StudPisa <- data.frame(StudPisa)
CountryToKeep <- c("Malaysia")
StudPisa <- subset(StudPisa, CNT %in% CountryToKeep)

str(StudPisa1, list.len=ncol(StudPisa1))

#dropped - Data Cleaning
StudPisa1 <- subset(StudPisa, select = -c(IC001Q01TA:PA042Q01TA, PV1SCEP:PV10SSES))
StudPisa1$CNTRYID <- NULL
StudPisa1$CNT<-NULL
StudPisa1$CYC<-NULL
StudPisa1$NatCen<-NULL
StudPisa1$Region<-NULL
StudPisa1$SUBNATIO<-NULL
StudPisa1$PROGN<-NULL
StudPisa1$Option_CPS<-NULL
StudPisa1$Option_ECQ<-NULL
StudPisa1$Option_UH<-NULL
StudPisa1$Option_FL<-NULL
StudPisa1$Option_ICTQ<-NULL
StudPisa1$Option_PQ<-NULL
StudPisa1$Option_TQ<-NULL
StudPisa1$Option_Read<-NULL
StudPisa1$Option_Math<-NULL
StudPisa1$ADMINMODE<-NULL
StudPisa1$OECD<-NULL
StudPisa1$LANGTEST_QQQ<-NULL
StudPisa1$LANGTEST_COG<-NULL
StudPisa1$LANGTEST_PAQ<-NULL

write.csv(StudPisa1,'StudPisa1.csv',row.names = FALSE)

pisaMalaysia[pisaMalaysia$ST093Q01TA==""]
table(StudPisa1$ST093Q01TA=="No Response")
table(StudPisa1$ST093Q01TA)

# this shows the NAs (missing by design)
sapply(StudPisa1, function(x) sum(is.na(x)))

table(pisaMas2015$ST098Q01TA)
summary(pisaMas2015$ST098Q01TA)

table(StudPisa1$ST098Q01TA)
summary(StudPisa1$ST098Q01TA)


# 4 Points Scale
#"IBTEACH",
StudPisa1$RevST098Q01TA <- recode(StudPisa1$ST098Q01TA,'"In all lessons"=4;
                                                            "In most lessons"=3;
                                  "In some lessons"=2;
                                  "Never or hardly ever"=1;
                                  "Valid Skip"=5;
                                  "Not Reached"=6;
                                  "Not Applicable"=7;
                                  "Invalid"=8;
                                  "No Response"=9',as.factor = FALSE)
StudPisa1$RevST098Q02TA <- recode(StudPisa1$ST098Q02TA,'"In all lessons"=4;
                                                            "In most lessons"=3;
                                    "In some lessons"=2;
                                    "Never or hardly ever"=1;
                                    "Valid Skip"=5;
                                    "Not Reached"=6;
                                    "Not Applicable"=7;
                                    "Invalid"=8;
                                    "No Response"=9',as.factor = FALSE)
StudPisa1$RevST098Q03NA <- recode(StudPisa1$ST098Q03NA,'"In all lessons"=4;
                                                            "In most lessons"=3;
                                    "In some lessons"=2;
                                    "Never or hardly ever"=1;
                                    "Valid Skip"=5;
                                    "Not Reached"=6;
                                    "Not Applicable"=7;
                                    "Invalid"=8;
                                    "No Response"=9',as.factor = FALSE)
StudPisa1$RevST098Q05TA <- recode(StudPisa1$ST098Q05TA,'"In all lessons"=4;
                                                            "In most lessons"=3;
                                    "In some lessons"=2;
                                    "Never or hardly ever"=1;
                                    "Valid Skip"=5;
                                    "Not Reached"=6;
                                    "Not Applicable"=7;
                                    "Invalid"=8;
                                    "No Response"=9',as.factor = FALSE)
StudPisa1$RevST098Q06TA <- recode(StudPisa1$ST098Q06TA,'"In all lessons"=4;
                                                            "In most lessons"=3;
                                    "In some lessons"=2;
                                    "Never or hardly ever"=1;
                                    "Valid Skip"=5;
                                    "Not Reached"=6;
                                    "Not Applicable"=7;
                                    "Invalid"=8;
                                    "No Response"=9',as.factor = FALSE)
StudPisa1$RevST098Q07TA <- recode(StudPisa1$ST098Q07TA,'"In all lessons"=4;
                                                            "In most lessons"=3;
                                    "In some lessons"=2;
                                    "Never or hardly ever"=1;
                                    "Valid Skip"=5;
                                    "Not Reached"=6;
                                    "Not Applicable"=7;
                                    "Invalid"=8;
                                    "No Response"=9',as.factor = FALSE)
StudPisa1$RevST098Q08NA <- recode(StudPisa1$ST098Q08NA,'"In all lessons"=4;
                                                            "In most lessons"=3;
                                    "In some lessons"=2;
                                    "Never or hardly ever"=1;
                                    "Valid Skip"=5;
                                    "Not Reached"=6;
                                    "Not Applicable"=7;
                                    "Invalid"=8;
                                    "No Response"=9',as.factor = FALSE)
StudPisa1$RevST098Q09TA <- recode(StudPisa1$ST098Q09TA,'"In all lessons"=4;
                                                            "In most lessons"=3;
                                    "In some lessons"=2;
                                    "Never or hardly ever"=1;
                                    "Valid Skip"=5;
                                    "Not Reached"=6;
                                    "Not Applicable"=7;
                                    "Invalid"=8;
                                    "No Response"=9',as.factor = FALSE)


summary(StudPisa$ST098Q01TA)
table(StudPisa$ST098Q01TA)
#summary(StudPisa$RevST098Q01TA)
#StudPisa1<- na.omit(StudPisa1)


#"INSTSCIE",
impPISALong$RevST113Q01TA <- recode(impPISALong$ST113Q01TA,"1=4;2=3;3=2;4=1",as.factor = FALSE)
impPISALong$RevST113Q02TA <- recode(impPISALong$ST113Q02TA,"1=4;2=3;3=2;4=1",as.factor = FALSE)
impPISALong$RevST113Q03TA <- recode(impPISALong$ST113Q03TA,"1=4;2=3;3=2;4=1",as.factor = FALSE)
impPISALong$RevST113Q04TA <- recode(impPISALong$ST113Q04TA,"1=4;2=3;3=2;4=1",as.factor = FALSE)

