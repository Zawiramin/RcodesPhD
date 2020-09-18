#' WHAT ARE THE STEP 2
#' Basically step 2 SUPPOSEDLY are focusing on the data management section
#' 1st we extract only Malaysia from the original database
#' 2nd i rename the school stratum into much simpler one
#' 3rd i subset only the variables needed for the statistic and analysis
#' 4th i performed IMPUTATION using MICE
#' 5th i split or partitioned the data into desired % (70-30 or 80-20)
#' #--------------------------------------------------------------------------------------------------#
#'                          LINK AND NOTE USING DATA.TABLE
#' https://okanbulut.github.io/bigdata/wrangling-big-data.html#readingwriting-data-with-data.table
#' #--------------------------------------------------------------------------------------------------#
#' #-------------------------------------------------#
pisa2015M <- pisa2015[CNTRYID == "Malaysia"]
pisa2015M[,table(CNTRYID)]
#pisaSch2015 <-pisaSch2015[CNTRYID=="Malaysia"]
#pisaSch2015[,table(CNTRYID)]

pisa15CPS[,table(CNTRYID)]
names(pisa15CPS)
#-------------------------------------------------#
#' rename variables in STRATUM to a more intuitive name
#-------------------------------------------------#
#' a user commented that my method is not efficient because it is not vectorized
#' one user suggested to use fcase however the package is in other data.table source
#rname.SchType <- function(x){
#  if (is.na(x)) NA
#  else if (x == "MYS - stratum 01: MOE National Secondary School\\Other States")  "Public"
#  else if(x == "MYS - stratum 02: MOE Religious School\\Other States") "Religious" 
#  else if(x == "MYS - stratum 03: MOE Technical School\\Other States") "Technical"
#  else if(x == "MYS - stratum 04: MOE Fully Residential School")"SBP"
#  else if(x == "MYS - stratum 05: non-MOE MARA Junior Science College\\Other States")"MARA"
#  else if(x == "MYS - stratum 06: non-MOE Other Schools\\Other States")"Private"
#  else if(x == "MYS - stratum 07: Perlis non-“MOE Fully Residential Schools”")"Perlis Fully Residential"
#  else if(x == "MYS - stratum 08: Wilayah Persekutuan Putrajaya non-“MOE Fully Residential Schools”")"Putrajaya Fully Residential"
#  else if(x == "MYS - stratum 09: Wilayah Persekutuan Labuan non-“MOE Fully Residential Schools”")"Labuan Fully Residential"
#}
#' RETIRED CODE

lookUpStratum <- data.table(STRATUM=c("MYS - stratum 01: MOE National Secondary School\\Other States",
                                      "MYS - stratum 02: MOE Religious School\\Other States",
                                      "MYS - stratum 03: MOE Technical School\\Other States",
                                      "MYS - stratum 04: MOE Fully Residential School",
                                      "MYS - stratum 05: non-MOE MARA Junior Science College\\Other States",
                                      "MYS - stratum 06: non-MOE Other Schools\\Other States",
                                      "MYS - stratum 07: Perlis non-“MOE Fully Residential Schools”",
                                      "MYS - stratum 08: Wilayah Persekutuan Putrajaya non-“MOE Fully Residential Schools”",
                                      "MYS - stratum 09: Wilayah Persekutuan Labuan non-“MOE Fully Residential Schools”"),
                            SCH.TYPE=c("Public",
                                       "Religious",
                                       "Technical",
                                       "SBP",
                                       "MARA",
                                       "Private",
                                       "Perlis Fully Residential",
                                       "Putrajaya Fully Residential",
                                       "Labuan Fully Residential"))




#' rename variable names in columns using setDT referencing to the old name with the new
#' SCH.Strat
#setDT(pisaSch2015)[,SCH.Strat := lookUpStratum$SCH.Strat[match(pisaSch2015$STRATUM,lookUpStratum$STRATUM)]]
setDT(pisa2015)[lookUpStratum,SCH.TYPE := i.SCH.TYPE, on = c(STRATUM = "STRATUM")]
pisa2015[,length(unique(CNTSCHID))]
#unique(pisa2015$SCH.TYPE)

pisa2015[,table(STRATUM)]
pisa2015[,table(SCH.TYPE)]
#pisaSch2015[,table(SCH.Strat)]
#'----------------------------#
#' Variables with ICT + 25 Latent Variables 
#' The only variables used in the analysis
#'----------------------------#
Var.All.LatVar <- subset(pisa2015,
                         select = c(SCH.TYPE,
                                    ST004D01T, #sex
                                    ESCS, #' Other derived indices from the questionnaire 
                                    
                                    DISCLISCI:SCIEACT,# 12 
                                    BELONG:ADINST,    # 8
                                    CULTPOSS:WEALTH,  # 5
                                    PV1SCIE:PV10SCIE
                         ))







#'----------------------------#
#' 25 Latent Variables 
#' The only variables used in the analysis
#'----------------------------#
LATENTVAR <- subset(pisa2015,
                   select = c(SCH.TYPE,
                              ST004D01T,
                              ESCS, #' Other derived indices from the questionnaire 
                              
                              DISCLISCI:SCIEACT,# 12 
                              BELONG:ADINST,    # 8
                              CULTPOSS:WEALTH,  # 5
                              PV1SCIE:PV10SCIE
                   ))

#-----------------------------------------------------------------------------------------------#
#' Next, we will split our dataset into a training dataset and a test dataset. We will train 
#' the decision tree on the training data and check its accuracy using the test data. In order 
#' to replicate the results later on, we need to set the seed – which will allow us to fix the 
#' randomization. Next, we remove the missing cases, save it as a new dataset, and then use 
#' createDataPartition() from the caret package to create an index to split the dataset as 70% to 30% using p = 0.7.
#' https://okanbulut.github.io/bigdata/supervised-machine-learning-part-i.html#decision-trees-in-r
#' MICE for IMPUTATION
#' #--------------------------------------------------------------------------------------------#

#' Set the seed before splitting the data
set.seed(12345)
library("mice") #' handling missing values

#' Check for missing values and its pattern
md.pattern(LATENTVAR)
summary(LATENTVAR)

#' Since PV doesn't have any missing values, proceed to get the mean first
#' and create new variables based on the cut off value from malaysia's results from OECD

#'----------------------------#
#' Get the MEAN for the PV values
#'----------------------------#
LATENTVAR[,':='
         (science = rowMeans(LATENTVAR[, c(paste0("PV", 1:10, "SCIE"))], na.rm = TRUE))]
LATENTVAR[,mean(science)]


#--------------------------------------------------------------#
#' DATA SUMMARY - PLAYING WITH DATA - Exploratory Data Analysis
#' prior to Training and Testing - Univariate Descriptive analysis
#' eg:  central tendency (mean, mode and median) and 
#' dispersion: range, variance, maximum, minimum, 
#' quartiles (including the interquartile range), and standard deviation.
#' #-----------------------------------------------------------#
#' What if we want to compare just the students' science performance 
#' mean on the enjoyment of science 

#' PISA 2015 science Focus: mean science w/ Enjoy Science
LATENTVAR[,.(science = mean(science, na.rm = TRUE),
             enjoySc = mean(JOYSCIE, na.rm = TRUE)),
          by = .(SCH.TYPE)
          ][order(science,decreasing = TRUE)
            ]
LATENTVAR[,.(science = mean(science, na.rm = TRUE),
             enjoy = mean(JOYSCIE, na.rm = TRUE)),
          by = .(ST004D01T,SCH.TYPE)]




#--------------#
#' Performed Imputation using MICE
summary(LATENTVAR)
#latVar.imp <- mice(LATENTVAR,m=5,maxit = 50,method = 'pmm', seed = 1000)
#latVarNew <- complete(latVar.imp) #commenting this as to note that i don't need to run this code agein
summary(latVarNew)

#' convert data.frame to data.table 
#' read here for more details on the disadvantages of data.frame (R base)
#' https://www.quora.com/What-is-the-difference-between-data-frame-and-data-table-in-R-programming-language

#latVarNew <- data.table(latVarNew)


#' Since calculating the mean PV doesn't affected by the imputation, due to the PVs have no NAs,
#' I chose to performed it after imputation was done. 
#' And partitioning the data also was done after imputation.
#' That's the correct way of managing the data

#--------------#
#' TARGET CLASS
#' Using a set of predictors in the pisa dataset, we will predict whether students are above 
#' or below the mean scale score for science. The average science score in PISA 2015 was 493 
#' across all participating countries (see PISA 2015 Results in Focus for more details). 
#' Using this score as a cut-off value, we will first create a binary variable called 
#' science_perf where science_perf= High if a student’s science score is equal or larger than
#'  493; otherwise science_perf= Low
#'  USING Malaysia' OECD AVERAGE AS CUT OFF

#'  USING MALAYSIA AVERAGE AS CUT OFF
latVarNew[, science_perf := 
            as.factor(ifelse(science >= mean(science), "High", "Low"))]

latVarNew[,table(science_perf)]

#' now i need to exclude non usable variables from the dataset
#' the variables that i'm not going to use now is, (science,PV1SCIE:PV10SCIE) as it will affect the model
#' using subset
x<-subset(latVarNew,select = -c(PV1SCIE:science))
x<-subset(x,select =-(SCH.TYPE)) #' terpaksa exclude sbb effect model
x[,table(BELONG)]
#-------------------------
#' Perform the PARTITIONIONG
set.seed(123456)


#' data with all 25 latent variabls
indexlatVar.55 <- createDataPartition(x$science_perf, p = 0.5, list = FALSE)
train.latVarImp.55 <- x[indexlatVar.55,]
test.latVarImp.55 <- x[-indexlatVar.55,]

indexlatVar.73 <- createDataPartition(x$science_perf, p = 0.7, list = FALSE)
train.latVarImp.73 <- x[indexlatVar.73,]
test.latVarImp.73 <- x[-indexlatVar.73,]

indexlatVar.82 <- createDataPartition(x$science_perf, p = 0.8, list = FALSE)
train.latVarImp.82 <- x[indexlatVar.82,]
test.latVarImp.82 <- x[-indexlatVar.82,]

indexlatVar.91 <- createDataPartition(x$science_perf, p = 0.9, list = FALSE)
train.latVarImp.91 <- x[indexlatVar.91,]
summtest.latVarImp.91 <- x[-indexlatVar.91,]

#'----------------------------#
#' 15 Latent Variables 
#' The variables used in the analysis
#' with Reliability > 0.8
#'----------------------------#
#y <- subset(x,select = -c(ST004D01T,ESCS,BELONG,
#                          ANXTEST,COOPERATE,CPSVALUE,
#                          EMOSUPS,ADINST,CULTPOSS,HEDRES,ICTRES,WEALTH) )
#' data with only 15 latent variabls
#indexNew.55 <- createDataPartition(x$science_perf, p = 0.5, list = FALSE)
#train.New.55 <- x[indexNew.55,]
#test.New.55 <- x[-indexNew.55,]

#indexNew.73 <- createDataPartition(x$science_perf, p = 0.7, list = FALSE)
#train.New.73 <- x[indexNew.73,]
#test.New.73 <- x[-indexNew.73,]

#indexNew.82 <- createDataPartition(x$science_perf, p = 0.8, list = FALSE)
#train.New.82 <- x[indexNew.82,]
#test.New.82 <- x[-indexNew.82,]

#indexNew.91 <- createDataPartition(x$science_perf, p = 0.9, list = FALSE)
#train.New.91 <- x[indexNew.91,]
#test.New.91 <- x[-indexNew.91,]

#'----------------------------#
#' Science Related (Module 4 and Module 10) Latent Variables 
#' The variables used in the analysis
#' SCIEEFF,INTBRSCI, JOYSCIE, INSTSCIE, EPIST, ENVAWARE, ENVOPT
#' ANXTEST,BELONG, MOTIVAT, COOPERATE, CPSVALUE
#'----------------------------#
ScRltd <- subset(x,select = c(ST004D01T,ESCS,
                              SCIEEFF,INTBRSCI, JOYSCIE, INSTSCIE, EPIST, ENVAWARE, ENVOPT,
                              ANXTEST,BELONG, MOTIVAT, COOPERATE, CPSVALUE,science_perf))
#' data with only 15 latent variabls
indexNew.55 <- createDataPartition(x$science_perf, p = 0.5, list = FALSE)
train.New.55 <- x[indexNew.55,]
test.New.55 <- x[-indexNew.55,]

indexNew.73 <- createDataPartition(x$science_perf, p = 0.7, list = FALSE)
train.New.73 <- x[indexNew.73,]
test.New.73 <- x[-indexNew.73,]

indexNew.82 <- createDataPartition(x$science_perf, p = 0.8, list = FALSE)
train.New.82 <- x[indexNew.82,]
test.New.82 <- x[-indexNew.82,]

indexNew.91 <- createDataPartition(x$science_perf, p = 0.9, list = FALSE)
train.New.91 <- x[indexNew.91,]
test.New.91 <- x[-indexNew.91,]

