#' #--------------------------------------------------------------------------------------------------#
#'                          LINK AND NOTE USING DATA.TABLE
#' https://okanbulut.github.io/bigdata/wrangling-big-data.html#readingwriting-data-with-data.table
#' #--------------------------------------------------------------------------------------------------#
#' #-------------------------------------------------#
pisa2015 <- pisa2015[CNTRYID == "Malaysia"]
pisa2015[,table(CNTRYID)]
pisaSch2015[,table(CNTRYID)]
pisaSch2015 <-pisaSch2015[CNTRYID=="Malaysia"]

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

#lookUpStratum <- data.table(STRATUM=c("MYS - stratum 01: MOE National Secondary School\\Other States",
#                                      "MYS - stratum 02: MOE Religious School\\Other States",
#                                      "MYS - stratum 03: MOE Technical School\\Other States",
#                                      "MYS - stratum 04: MOE Fully Residential School",
#                                      "MYS - stratum 05: non-MOE MARA Junior Science College\\Other States",
#                                      "MYS - stratum 06: non-MOE Other Schools\\Other States",
#                                      "MYS - stratum 07: Perlis non-“MOE Fully Residential Schools”",
#                                      "MYS - stratum 08: Wilayah Persekutuan Putrajaya non-“MOE Fully Residential Schools”",
#                                      "MYS - stratum 09: Wilayah Persekutuan Labuan non-“MOE Fully Residential Schools”"),
#                            SCH.Strat=c("Public",
#                                       "Religious",
#                                       "Technical",
#                                       "SBP",
#                                       "MARA",
#                                       "Private",
#                                       "Perlis Fully Residential",
#                                       "Putrajaya Fully Residential",
#                                       "Labuan Fully Residential"))




#' rename variable names in columns using setDT referencing to the old name with the new
#' SCH.Strat
#setDT(pisaSch2015)[,SCH.Strat := lookUpStratum$SCH.Strat[match(pisaSch2015$STRATUM,lookUpStratum$STRATUM)]]
#setDT(pisa15Mas)[lookUpStratum,SCH.TYPE1 := i.SCH.TYPE, on = c(STRATUM = "STRATUM")]
pisa2015[,length(unique(CNTSCHID))]
unique(pisa2015$SCH.TYPE)

pisa2015[,table(STRATUM)]
pisa2015[,table(SCH.TYPE)]
pisaSch2015[,table(SCH.Strat)]

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
LATENTVAR[,summary(science)]

#-------------------------------------------------------------------#
#' TARGET CLASS
#' Using a set of predictors in the pisa dataset, we will predict whether students are above 
#' or below the mean scale score for science. The average science score in PISA 2015 was 493 
#' across all participating countries (see PISA 2015 Results in Focus for more details). 
#' Using this score as a cut-off value, we will first create a binary variable called 
#' science_perf where science_perf= High if a student’s science score is equal or larger than
#'  493; otherwise science_perf= Low
#'  USING Malaysia' OECD AVERAGE AS CUT OFF

#'  USING MALAYSIA AVERAGE AS CUT OFF
pisa2015[, science_perf := 
           as.factor(ifelse(science >= (mean(science)), "High", "Low"))]

pisa2015[,table(science_perf)]
#fwrite(pisa15Mas,file = "pisa15Mas.csv")

#' Performed Imputation using MICE
latVar.imp <- mice(LATENTVAR,m=5,maxit = 50,method = 'pmm', seed = 1000)
latVar.imp <- complete(latVar.imp)


#--------------------------------------------------------------#
#' DATA SUMMARY - PLAYING WITH DATA - Exploratory Data Analysis
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


