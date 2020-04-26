#' #--------------------------------------------------------------------------------------------------#
#'                               USING DATA.TABLE
#' https://okanbulut.github.io/bigdata/wrangling-big-data.html#readingwriting-data-with-data.table
#'
#' #--------------------------------------------------------------------------------------------------#
#' #-------------------------------------------------#
pisa15Mas <- pisa15[CNTRYID == "Malaysia"]
table(pisa15$CNTRYID)
#pisaSchM <-pisaSch[CNTRYID=="Malaysia"]
#pisa18Mas <- pisa18[CNTRYID=="Malaysia"]
#' #-------------------------------------------------#
#' rename variables in STRATUM to a more intuitive name
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
#' PISA2015
setDT(pisa15Mas)[,SCH.TYPE := lookUpStratum$SCH.TYPE[match(pisa15Mas$STRATUM,lookUpStratum$STRATUM)]]
#setDT(pisa15Mas)[lookUpStratum,SCH.TYPE1 := i.SCH.TYPE, on = c(STRATUM = "STRATUM")]
pisa15Mas[,length(unique(CNTSCHID))]
unique(pisa15Mas$SCH.TYPE)

pisa15Mas[,table(STRATUM)]
pisa15Mas[,table(SCH.TYPE)]
#pisa18Mas[,table(STRATUM)]


#' --------------------------------------------
#' we need to recode some variables
#' as there are lots of dichotomous items in the pisa data set.
#' the := syntax adds this variable directly to the DT. 
#' We also specified the L to ensure the variable was treated as an integer and not a double
#' which uses less memory.

#' Convert a dichtomous item (yes/no) to numeric scoring
#' @param x a character vector containing "Yes" and "No" responses.
#' creating new function to help converting later
bin.to.num <- function(x){
  if (is.na(x)) NA
  else if (x == "Yes") 1L
  else if (x == "No") 0L
}

#' Convert other 4-5 Likert Scale to numeric scoring
#' @param x a character vector containing "Agree","Disagree","Strongly agree" ,and "Strongly disagree" responses.
#' need to identify which variables that in need of reverse code as well
#' so that higher WLEs correspond to higher level of positivity according to the questionnaire
#' creating new function to help converting later - item reverse coded can be used here as well

#----------------------------------#
#' Trend Scale Indices
#' #-------------------------------#
#' In addition, we will subset the students from Malaysia and choose our desired variables 
#' ([WLE], rather than the entire set of variables) for our model. 
#' We will use the following variables in the model:
#----------------------------------------------------#
#' 1. Enjoyment of science
#' #-------------------------------------------------#
#' Enjoyment of science [(JOYSCIE)]
#' 4-point Likert
#' How much do you disagree or agree with the statements about yourself below?
#' “strongly agree”, “agree”, “disagree”, and “strongly disagree”
fourScale.to.num1 <- function(x){
  if (is.na(x)) NA
  else if (x == "Strongly disagree") 1L
  else if (x == "Disagree") 2L
  else if (x == "Agree") 3L
  else if (x == "Strongly agree") 4L
}

#' Interest in broad science topics [(INTBRSCI)] 
#' 5-point Likert
#' To what extent are you interested in the following <broad science> topics?
#' "I don't know what this is","Not interested","Hardly interested","Interested","Highly interested"  
fiveScale.to.num1 <- function(x){
  if (is.na(x)) NA
  else if (x == "I don't know what this is") 0L
  else if (x == "Not interested") 1L
  else if (x == "Hardly interested") 2L
  else if (x == "Interested") 3L
  else if (x == "Highly interested") 4L
}

#----------------------------------------------------#
#' 2. Sense of Belonging
#' #-------------------------------------------------#
#' [BELONG] 
#' ONLY Items [ST034Q02TA], [ST034Q03TA] and [ST034Q05TA] were [Items reverse-coded ] so that higher WLEs and higher 
#' difficulty correspond to higher level of sense of belonging on all items.
#'
#' (so if I were to recode this three items, how should I make sense of this?)
#' (ok, so i need to understand the meaning of sense of belonging according to the question asked)
#' (we want more 4 = [Pos] answer to the questionnaire)
#' Questions: 
#' [Neg]feel like outsider at school:     1=,2=,3=,4=[Strongly Agree - Strongly Disagree] => more [Strongly Disagree] = [Pos] => fourScale.to.num2
#' [Pos]make friends easily at school[r]: 1=,2=,3=,4=[Strongly Agree - Strongly Disagree] => more [Strongly Agree]    = [Pos] => fourScale.to.num1
#' [Pos]feel belong at school[r]:         1=,2=,3=,4=[Strongly Agree - Strongly Disagree] => more [Strongly Agree]    = [Pos] => fourScale.to.num1
#' [Neg]feel awkward in school:           1=,2=,3=,4=[Strongly Agree - Strongly Disagree] => more [Strongly Disagree] = [Pos] => fourScale.to.num2
#' [Pos]other students likes me[r]:       1=,2=,3=,4=[Strongly Agree - Strongly Disagree] => more [Strongly Agree]    = [Pos] => fourScale.to.num1
#' [Neg]feel lonely at school:            1=,2=,3=,4=[Strongly Agree - Strongly Disagree] => more [Strongly Disagree] = [Pos] => fourScale.to.num2
#'  
fourScale.to.num2 <- function(x){
  if (is.na(x)) NA
  else if (x == "Strongly agree") 1L
  else if (x == "Agree") 2L
  else if (x == "Disagree") 3L
  else if (x == "Strongly disagree") 4L
}

#----------------------------------------------------#
#' 3. Science learning in school
#' #-------------------------------------------------#
#' Disciplinary climate in science classes [DISCLISCI] 
#' Questions [Neg]:
#' Scales [1,4] = [Every lesson - Never or hardly ever] => more [Never or hardly ever] = [Pos] => fourScale.to.num3
fourScale.to.num3 <- function(x){
  if (is.na(x)) NA
  else if (x == "Every lesson") 1L
  else if (x == "Most lessons") 2L
  else if (x == "Some lessons") 3L
  else if (x == "Never or hardly ever") 4L
}

#' Inquiry-based science teaching and learning practices [IBTEACH] [Items reverse-coded ]
#' Questions [Pos]:
#' Scales [1,4] = [Never or hardly ever - In all lessons] => more [In all lessons] = [Pos] => fourScale.to.num4
fourScale.to.num4 <- function(x){
  if (is.na(x)) NA
  else if (x == "Never or hardly ever") 1L
  else if (x == "In some lessons") 2L
  else if (x == "In most lessons") 3L
  else if (x == "In all lessons") 4L
}

#' 3 Teacher support in a science classes [TEACHSUP] [Items reverse-coded]
#' Questions [Pos]:
#' Scales [1,4] = [Never or hardly ever - In all lessons] => more [In all lessons] = [Pos] => fourScale.to.num5
fourScale.to.num5 <- function(x){
  if (is.na(x)) NA
  else if (x == "Never or hardly ever") 1L
  else if (x == "Some lessons") 2L
  else if (x == "Most lessons") 3L
  else if (x == "Every lesson") 4L
}

#' 4 Teacher-directed science instruction [TDTEACH] 
#' Questions [Pos]:
#' Scales [1,4] = [Never or almost never - Every lesson or almost every lesson] 
#' => more [Every lesson or almost every lesson] = [Pos] => fourScale.to.num6
fourScale.to.num6 <- function(x){
  if (is.na(x)) NA
  else if (x == "Never or almost never") 1L
  else if (x == "Some lessons") 2L
  else if (x == "Many lessons") 3L
  else if (x == "Every lesson or almost every lesson") 4L
}

#' 5 Perceived Feedback [PERFEED] 
#' How often do these things happen in your lessons for this <school science> course? (teacher in class)
#' Questions [Pos]:
#fourScale.to.num6

#' 6 Adaption of instruction [ADINST] 
#' How often do these things happen in your lessons for this <school science> course? (teacher in class)
#' Questions [Pos]:
#fourScale.to.num6

#' Instrumental motivation [INSTSCIE] [Items reverse-coded]
#' How much do you agree with the statements below?(me in Science class and for my future endeavor)
#' Questions [Pos]:
#fourScale.to.num1
fourScale.to.num1a <- function(x){
  if (is.na(x)) NA
  else if (x == "Strongly Disagree") 1L
  else if (x == "Disagree") 2L
  else if (x == "Agree") 3L
  else if (x == "Strongly Agree") 4L
}

#------------------------------------------------------------#
#' 4. Science self-efficacy [(SCIEEFF)] [Items reverse-coded]
#' #---------------------------------------------------------#
#' I could do this easily”, “I could do this with a bit of effort”, “I would struggle to do this on my own”, and “I couldn’t do this”
#' How easy do you think it would be for you to (recognize/explain/describe/identify/predict/interpret/discuss) on your own?
#' Questions [Pos]:
#' Scales [1,4] = [I couldn’t do this - I could do this easily] 
#' => more [I could do this easily] = [Pos] => fourScale.to.num7
fourScale.to.num7 <- function(x){
  if (is.na(x)) NA
  else if (x == "I couldn't do this") 1L
  else if (x == "I would struggle to do this on my own") 2L
  else if (x == "I could do this with a bit of effort") 3L
  else if (x == "I could do this easily") 4L
}

#----------------------------------------------------#
#' 5. Household Possessions [EXCLUDED]
#----------------------------------------------------#

#----------------------------------------------------#
#' 6. Environmental awareness and optimism
#----------------------------------------------------#
#' Environmental Awareness [(ENVAWARE)]
#' “I have never heard of this”, “I have heard about this but I would not be able to explain what it is really about”, 
#' “I know something about this and could explain the general issue”, “I am familiar with this and I would be able to explain this well”
#' How informed are you about the following environmental issues?
#' Questions [Issues]:
#' Scales [1,4] = [I have never heard of this - I am familiar with this and I would be able to explain this well] 
#' => more [I am familiar with this and I would be able to explain this well] = [Pos] => fourScale.to.num8
fourScale.to.num8 <- function(x){
  if (is.na(x)) NA
  else if (x == "I have never heard of this") 1L
  else if (x == "I have heard about this but I would not be able to explain what it is really about") 2L
  else if (x == "I know something about this and could explain the general issue") 3L
  else if (x == "I am familiar with this and I would be able to explain this well") 4L
}

#' Environmental optimism [(ENVOPT)] [Items reverse-coded]
#' “Improve”, “Stay about the same”, and “Get worse”
#' Do you think problems associated with the environmental issues below will improve or get worse over the next 20 years?
#' Questions [Issues]:
#' Scales [1,3] = [Get worse - Improve] 
#' => more [Improve] = [Pos] => threeScale.to.num1
threeScale.to.num1 <- function(x){
  if (is.na(x)) NA
  else if (x == "Get worse") 1L
  else if (x == "Stay about the same") 2L
  else if (x == "Improve") 3L
} 

#----------------------------------------------------#
#' (New Scale Indices)
#' #-------------------------------------------------#
#' applicable to Student Motivation 
#' Anxtest 
#' To what extent do you disagree or agree with the following statements about yourself?
#' Questions [Neg]:
#' Scales [1,4] = [Strongly Agree - Strongly Disagree] 
#' => more [Strongly Disagree] = [Pos] => fourScale.to.num2
#fourScale.to.num2

#' Motivat
#' To what extent do you disagree or agree with the following statements about yourself?
#' Questions [Pos]:
#' Scales [1,4] = [Strongly Disagree - Strongly Agree] 
#' => more [Strongly Agree] = [Pos] => fourScale.to.num1
#fourScale.to.num1

#'----------------------------#
#' Student level scale indices
#'----------------------------#
pisa15Mas[, `:=` 
          (Gender = ifelse(ST004D01T == "Female", 1, 0),
            Sex = ST004D01T,
            
            #' Total learning times in hours
            learning = round(TMINS/60,0),
            
            
            #' create new variables MEAN using PV values
            math = rowMeans(pisa15Mas[, c(paste0("PV", 1:10, "MATH"))], na.rm = TRUE),
            reading = rowMeans(pisa15Mas[, c(paste0("PV", 1:10, "READ"))], na.rm = TRUE),
            science = rowMeans(pisa15Mas[, c(paste0("PV", 1:10, "SCIE"))], na.rm = TRUE)
            )]

pisa15[,':='
       (science = rowMeans(pisa15Mas[, c(paste0("PV", 1:10, "SCIE"))], na.rm = TRUE))]

#'----------------------------#
#' New Scale Indices
#'----------------------------#
pisa15Mas[, `:=` 
          (
            #' Students ́ motivation
            #' ANXTEST
            worryTest = sapply(ST118Q01NA, fourScale.to.num2),
            worryGred = sapply(ST118Q02NA, fourScale.to.num2),
            feelAnxious = sapply(ST118Q03NA, fourScale.to.num2),
            tenseStudying = sapply(ST118Q04NA, fourScale.to.num2),
            getNervous = sapply(ST118Q05NA, fourScale.to.num2),
            
            #' MOTIVAT
            wantTopGrade = sapply(ST119Q01NA, fourScale.to.num1),
            haveGoodOptions = sapply(ST119Q02NA, fourScale.to.num1),
            bestInAnything = sapply(ST119Q03NA, fourScale.to.num1),
            ambitious = sapply(ST119Q04NA, fourScale.to.num1),
            bestInClass = sapply(ST119Q05NA, fourScale.to.num1)
          )]

#'----------------------------#
#' Trend Scale Indices (TSI)
#'----------------------------#
pisa15Mas[, `:=` 
          (
            #' 1. Interest in Science 
            #' JOYSCIE
            #' Enjoyment of science (ST094) is a trend question from PISA 2006
            funLearningSc = sapply(ST094Q01NA,fourScale.to.num1),
            likeReadingSc = sapply(ST094Q02NA,fourScale.to.num1),
            happyWorkOnSc = sapply(ST094Q03NA,fourScale.to.num1),
            enjoyScKnwlge = sapply(ST094Q04NA,fourScale.to.num1),
            intLearningSc = sapply(ST094Q05NA,fourScale.to.num1),
            
            #' Interest in broad science topics (INTBRSCI) 
            #' INTBRSCI 
            biosphere = sapply(ST095Q04NA,fiveScale.to.num1),
            motion = sapply(ST095Q07NA,fiveScale.to.num1),
            energy = sapply(ST095Q08NA,fiveScale.to.num1),
            universe = sapply(ST095Q13NA,fiveScale.to.num1),
            disease = sapply(ST095Q15NA,fiveScale.to.num1),
            
            #'----------------------------#
            #' 2. Sense of belonging
            #' BELONG
            feltOutsider = sapply(ST034Q01TA, fourScale.to.num2),
            friendEasily = sapply(ST034Q01TA, fourScale.to.num1),
            feltBelong = sapply(ST034Q01TA, fourScale.to.num1),
            feltAwkward = sapply(ST034Q01TA, fourScale.to.num2),
            studentLikesMe = sapply(ST034Q01TA, fourScale.to.num1),
            feltLonely = sapply(ST034Q01TA, fourScale.to.num2),
            
            #'----------------------------#
            #' 3. Science Learning In School
            #' Disciplinary climate in science classes (DISCLISCI)
            notListen = sapply(ST097Q01TA, fourScale.to.num3),
            noise = sapply(ST097Q02TA, fourScale.to.num3),
            quiteDown = sapply(ST097Q03TA, fourScale.to.num3),
            cannotWorkWell = sapply(ST097Q04TA, fourScale.to.num3),
            delayWorking = sapply(ST097Q05TA, fourScale.to.num3),
            
            #' Inquiry-based science teaching and learning practices (IBTEACH)
            giveIdeas = sapply(ST098Q01TA, fourScale.to.num4),
            doingPractExp = sapply(ST098Q02TA, fourScale.to.num4),
            scieQuest = sapply(ST098Q03NA, fourScale.to.num4),
            conclExp = sapply(ST098Q05TA, fourScale.to.num4),
            techExpl = sapply(ST098Q06TA, fourScale.to.num4),
            designOwnExp = sapply(ST098Q07TA, fourScale.to.num4),
            classDebate = sapply(ST098Q08NA, fourScale.to.num4),
            techExplCon = sapply(ST098Q09TA, fourScale.to.num4),
            
            #' Teacher support in a science classes (TEACHSUP)
            showInterest = sapply(ST100Q01TA, fourScale.to.num5),
            giveHelp = sapply(ST100Q02TA, fourScale.to.num5),
            helpLearn = sapply(ST100Q03TA, fourScale.to.num5),
            contTeach = sapply(ST100Q04TA, fourScale.to.num5),
            giveOpp = sapply(ST100Q05TA, fourScale.to.num5),
            
            #' Teacher-directed science instruction (TDTEACH)
            explScIdeas = sapply(ST103Q01NA, fourScale.to.num6),
            discussion = sapply(ST103Q03NA, fourScale.to.num6),
            discussQ = sapply(ST103Q08NA, fourScale.to.num6),
            demonstrateIdea = sapply(ST103Q11NA, fourScale.to.num6),
            
            #' Perceived Feedback (PERFEED)
            tellsMyPerf = sapply(ST104Q01NA, fourScale.to.num6),
            gvFdbckOnMyStrgth = sapply(ST104Q02NA, fourScale.to.num6),
            tellsAreaToImprv = sapply(ST104Q03NA, fourScale.to.num6),
            tellsHowToImprove = sapply(ST104Q04NA, fourScale.to.num6),
            advceHowToRchGols = sapply(ST104Q05NA, fourScale.to.num6),
            
            #' Adaption of instruction (ADINST)
            adpt2ClassNeeds = sapply(ST107Q01NA, fourScale.to.num6),
            provIndivHelp = sapply(ST107Q02NA, fourScale.to.num6),
            changeStructure = sapply(ST107Q03NA, fourScale.to.num6),
            
            #' Instrumental motivation (INSTSCIE)
            effortWorth = sapply(ST113Q01TA, fourScale.to.num1a),
            learnImportant = sapply(ST113Q02TA, fourScale.to.num1a),
            worthLearning = sapply(ST113Q03TA, fourScale.to.num1a),
            thingLearnHelp = sapply(ST113Q04TA, fourScale.to.num1a),
            
            #'----------------------------#
            #' 4. Science self-efficacy (SCIEEFF)
            healthIssue = sapply(ST129Q01TA, fourScale.to.num7),
            earthquakes = sapply(ST129Q02TA, fourScale.to.num7),
            antibiotics = sapply(ST129Q03TA, fourScale.to.num7),
            garbageDisposal = sapply(ST129Q04TA, fourScale.to.num7),
            speciesSurvival = sapply(ST129Q05TA, fourScale.to.num7),
            foodLabels = sapply(ST129Q06TA, fourScale.to.num7),
            lifeOnMars = sapply(ST129Q07TA, fourScale.to.num7),
            acidRain = sapply(ST129Q08TA, fourScale.to.num7),
            
            #' EPIST,SCIEACT,
            
            #'----------------------------#
            #' 5. Household possessions
            #' CULTPOSS,HEDRES,WEALTH,ICTRES,HOMEPOS,
            #' Items is [Excluded]
            
            #'----------------------------#
            #' 6. Environmental awareness and optimism
            #' Environmental awareness (ENVAWARE)
            greenhouseA = sapply(ST092Q01TA, fourScale.to.num8),
            geneticA = sapply(ST092Q02TA, fourScale.to.num8),
            nuclearA = sapply(ST092Q04TA, fourScale.to.num8),
            deforestA = sapply(ST092Q05TA, fourScale.to.num8),
            airPollutA = sapply(ST092Q06NA, fourScale.to.num8),
            extinctionA = sapply(ST092Q08NA, fourScale.to.num8),
            waterShortA = sapply(ST092Q09NA, fourScale.to.num8),
            
            #' Environmental optimism (ENVOPT)
            airPollutO = sapply(ST093Q01TA, threeScale.to.num1),
            extinctionO = sapply(ST093Q03TA, threeScale.to.num1),
            deforestO = sapply(ST093Q04TA, threeScale.to.num1),
            waterShortO = sapply(ST093Q05TA, threeScale.to.num1),
            nuclearO = sapply(ST093Q06TA, threeScale.to.num1),
            greenhouseO = sapply(ST093Q07NA, threeScale.to.num1),
            geneticO = sapply(ST093Q08NA, threeScale.to.num1)
          )]




#--------------------------------------------------------------#
#' DATA SUMMARY - PLAYING WITH DATA - Exploratory Data Analysis
#' #-----------------------------------------------------------#
#' What if we want to compare just the students' science performance 
#' mean on the enjoyment of science 

#' PISA 2015 science Focus: mean science w/ Enjoy Science
pisa15Mas[,.(science = mean(science, na.rm = TRUE),
             enjoySc = mean(JOYSCIE, na.rm = TRUE),
             airPollutA = mean(airPollutA, na.rm = TRUE)),
          by = .(SCH.TYPE,CNTSCHID)
          ][order(science,decreasing = TRUE)
            ][1:10]
pisa15Mas[,.(science = mean(science, na.rm = TRUE),
             enjoy = mean(JOYSCIE, na.rm = TRUE)),
          by = .( SchoolType, Sex)]

pisa15Mas[,.(math = mean(math, na.rm = TRUE),
             enjoy = mean(JOYSCIE, na.rm = TRUE)),
          by = .( SchoolType, Sex)]

pisa15[,.(science = mean(science,na.rm = TRUE)),
       by = .(CNTRYID == 'MALAYSIA')
]


#-------------------------------------------------------------------#
#' TARGET CLASS
#' Using a set of predictors in the pisa dataset, we will predict whether students are above 
#' or below the mean scale score for science. The average science score in PISA 2015 was 493 
#' across all participating countries (see PISA 2015 Results in Focus for more details). 
#' Using this score as a cut-off value, we will first create a binary variable called 
#' science_perf where science_perf= High if a student’s science score is equal or larger than
#'  493; otherwise science_perf= Low
#'  USING OECD AVERAGE AS CUT OFF
pisa15Mas[, science_perf := 
                         as.factor(ifelse(science >= 493, "High", "Low"))]
#'  USING MALAYSIA AVERAGE AS CUT OFF
pisa15Mas[, science_perfM := 
            as.factor(ifelse(science >= (mean(science)), "High", "Low"))]

pisa15Mas[,table(science_perf)]
pisa15Mas[,table(science_perfM)]
#fwrite(pisa15Mas,file = "pisa15Mas.csv")
