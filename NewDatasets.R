#-------------------------------------------------#
#' need to clean more data
#' need to drop unused columns 
#-------------------------------------------------#

#' #-------------------------------------------------------------------------------------#
#' Let's call the new dataset pisa_model with only SCIENCE LEARNING IN SCHOOL VARIABLES
#' 
scLearn <- subset(pisa15Mas,
                  select = c(science_perf,#reading, math,
                             ESCS, #' Other derived indices from the questionnaire 
                             
                             #' Disciplinary climate in science classes (DISCLISCI)
                             notListen,noise,quiteDown,cannotWorkWell,delayWorking,
                             
                             #' Inquiry-based science teaching and learning practices (IBTEACH)
                             giveIdeas,doingPractExp,scieQuest,conclExp,techExpl,designOwnExp,
                             classDebate,techExplCon,
                             
                             #' Teacher support in a science classes (TEACHSUP)
                             showInterest,giveHelp,helpLearn,contTeach,giveOpp,
                             
                             #' Teacher-directed science instruction (TDTEACH)
                             explScIdeas,discussion,discussQ,demonstrateIdea,
                             
                             #' Perceived Feedback (PERFEED)
                             tellsMyPerf,gvFdbckOnMyStrgth,tellsAreaToImprv,tellsHowToImprove,
                             advceHowToRchGols,
                             
                             #' Adaption of instruction (ADINST)
                             adpt2ClassNeeds,provIndivHelp,changeStructure,
                             
                             #' Instrumental motivation (INSTSCIE)
                             effortWorth,learnImportant,worthLearning,thingLearnHelp,
                             
                             #' Summary Indices
                             DISCLISCI,IBTEACH,TEACHSUP,TDTEACH,PERFEED,ADINST,INSTSCIE
                             ))

#' #-------------------------------------------------------------------------------------#
#' Let's call the new dataset pisa_model with only Environmental awareness and optimism
#' 
envAwareOpt <- subset(pisa15Mas,
                  select = c(science_perf,#reading, math,
                             ESCS, #' Other derived indices from the questionnaire 
                             
                             
                             #'----------------------------#
                             #' 6. Environmental awareness and optimism
                             #' Environmental awareness (ENVAWARE)
                             greenhouseA,
                             geneticA,
                             nuclearA,
                             deforestA,
                             airPollutA,
                             extinctionA,
                             waterShortA,
                             
                             #' Environmental optimism (ENVOPT)
                             airPollutO,
                             extinctionO,
                             deforestO,
                             waterShortO,
                             nuclearO,
                             greenhouseO,
                             geneticO,
                             
                             #' Summary Indices
                             ENVAWARE,ENVOPT
                  ))
envAware <- subset(pisa15Mas,
                  select = c(science_perf,#reading, math,
                             ESCS, #' Other derived indices from the questionnaire 
                                 
                                 
                            #'----------------------------#
                            #' 6. Environmental awareness and optimism
                            #' Environmental awareness (ENVAWARE)
                            greenhouseA,
                            geneticA,
                            nuclearA,
                            deforestA,
                            airPollutA,
                            extinctionA,
                            waterShortA,
                                 
                            #' Summary Indices
                            ENVAWARE
                            ))

envOpt <- subset(pisa15Mas,
                 select = c(science_perfM,#reading, math,
                            ESCS, #' Other derived indices from the questionnaire 
                            #' Environmental optimism (ENVOPT)
                            airPollutO,
                            extinctionO,
                            deforestO,
                            waterShortO,
                            nuclearO,
                            greenhouseO,
                            geneticO,
                            #' Summary Indices
                            ENVOPT
                            ))

#' #-------------------------------------------------------------------------------------#
#'----------------------------#
#' Trend Scale Indices (TSI)
#'----------------------------#
TSIndces <- subset(pisa15Mas,
                   select = c(science_perfM,#reading, math,
                              science_perfM11,
                              ESCS, #' Other derived indices from the questionnaire 
                              
                              #' 1. Interest in Science 
                              JOYSCIE,
                              INTBRSCI,
                              #'----------------------------#
                              #' 2. Sense of belonging
                              #BELONG,
                              #'----------------------------#
                              #' 3. Science Learning In School
                              DISCLISCI,
                              (IBTEACH),
                              (TEACHSUP),
                              (TDTEACH),
                              (PERFEED),
                              #(ADINST),
                              (INSTSCIE),
                              #'----------------------------#
                              #' 4. Science self-efficacy 
                              (SCIEEFF),EPIST,SCIEACT,
                              #'----------------------------#
                              #' 5. Household possessions
                              #CULTPOSS,HEDRES,WEALTH,ICTRES,
                              HOMEPOS,
                              #'----------------------------#
                              #' 6. Environmental awareness and optimism
                              #' Environmental awareness 
                              (ENVAWARE),
                              #' Environmental optimism 
                              (ENVOPT)
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
md.pattern(envOpt)
summary(envOpt)

envOptImputed <- mice(envOpt,m=5,maxit = 50,method = 'pmm', seed = 1000)
summary(envOptImputed)
summary(envOptImputed$data)
envOptNew <- complete(envOptImputed)
summary(envOptNew)

#' We need to remove missing cases
#' listwise deletion of missing values
#scLearnNew <- na.omit(scLearn)
#envAwareOptNew <- na.omit(envAwareOpt)
#envAwareNew <- na.omit(envAware)
#envOptNew <- na.omit(envOpt)

summary(TSIndces)
tsi.Imputed <- mice(TSIndces,m=5,maxit = 50,method = 'pmm', seed = 1000)
TSIndcesNew <- complete(tsi.Imputed)
summary(TSIndcesNew)
#TSIndcesNew <- na.omit(TSIndces)

#pisaM_nm[,(unique(SCH.TYPE))]

#' Split the data into training and test 70-30
#' Option: using split data xval kene = 0
#' Splitting Based on the Outcome
set.seed(123456)
#indexScL <- createDataPartition(scLearnNew$science_perf, p = 0.7, list = FALSE)
#train.scL <- scLearnNew[indexScL, ]
#test.scL  <- scLearnNew[-indexScL, ]

indexEnvOpt <- createDataPartition(envOptNew$science_perfM, p = 0.7, list = FALSE)
train.envOpt73 <- envOptNew[indexEnvOpt, ]
test.envOpt73  <- envOptNew[-indexEnvOpt, ]

indexTSI.73 <- createDataPartition(TSIndcesNew$science_perfM, p = 0.7, list = FALSE)
train.TSI.73 <- TSIndcesNew[indexTSI.73,]
test.TSI.73 <- TSIndcesNew[-indexTSI.73,]

indexTSI.82 <- createDataPartition(TSIndcesNew$science_perfM, p = 0.8, list = FALSE)
train.TSI.82 <- TSIndcesNew[indexTSI.82,]
test.TSI.82 <- TSIndcesNew[-indexTSI.82,]

#' The function createDataPartition can be used to create balanced splits of the data. If the y argument to this 
#' function is a factor, the random sampling occurs within each class and should preserve the overall class distribution of the data. 
#' The list = FALSE avoids returns the data as a list. This function also has an argument, times, that can create multiple splits at once; 
#' the data indices are returned in a list of integer vectors. 
#' Similarly, createResample can be used to make simple bootstrap samples and 
#' createFolds can be used to generate balanced cross–validation groupings from a set of data.

