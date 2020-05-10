#' #-------------------------------------------------------------------------------------# 
#' THE PLAN:
#' Building [Model 1] Tree Using Gini Index
#' [Model 1]- SCIENCE LEARNING with Gini Index with xval = 10 
#' [Model 1a]- SCIENCE LEARNING with Gini Index with 70-30 & XVAL 10
#' 
#' Building [Model 1E] Tree Using Entropy Index (Information Gain)
#' [Model 1E]- SCIENCE LEARNING with Information Gain with xval = 10 
#' [Model 1Ea]- SCIENCE LEARNING with Information Gain with 70-30 & XVAL 10

#' #-------------------------------------------------------------------------------------# 
#' [Model 1]- SCIENCE LEARNING with Gini Index with xval = 10 
#' 
scLSumIndx <- science_perf ~ DISCLISCI+IBTEACH+TEACHSUP+TDTEACH+PERFEED+ADINST+INSTSCIE+ESCS
scLM1 <- rpart(formula = scLSumIndx,
                    data = scLearnNew,
                    method = "class", 
                    parms = list(split = "gini"),
                    control = rpart.control(minsplit = 20, cp = 0,xval = 10))

printcp(scLM1)

cp.prune <- scLM1$cptable[which.min(scLM1$cptable[,"xerror"]),"CP"]
cp.prune
plotcp(scLM1)
scLM1new <- rpart(formula = scLSumIndx,
                       data = scLearnNew,
                       method = "class", 
                       parms = list(split = "gini"),
                       control = rpart.control(minsplit = 20, cp = cp.prune, xval = 0))
rpart.rules(scLM1new, cover = TRUE)
summary(scLM1new) #' this is signed of overfitted
varImp(scLM1new)
scL.pred <- predict(scLM1new, scLearnNew) %>%
  as.data.frame()
head(scL.pred)
scL.pred <- mutate(scL.pred,
                     science_perf = as.factor(ifelse(High >= 0.5, "High", "Low"))) %>% 
  select(science_perf)

#' Confusion Matrix Model 1
confusionMatrix(scL.pred$science_perf, scLearnNew$science_perf,mode = "everything")
#' 

#' #-------------------------------------------------------------------------------------# 
#' [Model 1a]- SCIENCE LEARNING with Gini Index with 70-30 and xval
#' 

scLM1a <- rpart(formula = scLSumIndx,
               data = scL_Train,
               method = "class", 
               parms = list(split = "gini"),
               control = rpart.control(minsplit = 20, cp = 0,xval = 10))

printcp(scLM1a)

cp.prune <- scLM1a$cptable[which.min(scLM1a$cptable[,"xerror"]),"CP"]
cp.prune
plotcp(scLM1a)
scLM1aNew <- rpart(formula = scLSumIndx,
                  data = scL_Train,
                  method = "class", 
                  parms = list(split = "gini"),
                  control = rpart.control(minsplit = 20, cp = cp.prune, xval = 0))
rpart.rules(scLM1aNew, cover = TRUE)
summary(scLM1aNew) #' overfitted
varImp(scLM1aNew)
scL.predA <- predict(scLM1aNew, scL_Test) %>%
  as.data.frame()
head(scL.predA)
scL.predA <- mutate(scL.predA,
                   science_perf = as.factor(ifelse(High >= 0.5, "High", "Low"))) %>% 
  select(science_perf)

#' Confusion Matrix Model 1a
confusionMatrix(scL.predA$science_perf, scL_Test$science_perf,mode = "everything")
#' 

#' #-------------------------------------------------------------------------------------# 
#' [Model 1E]- SCIENCE LEARNING with Information Gain with xval 10
#' 

scLM1E <- rpart(formula = scLSumIndx,
                data = scLearnNew,
                method = "class", 
                parms = list(split = "information"),
                control = rpart.control(minsplit = 20, cp = 0,xval = 10))

printcp(scLM1E)

cp.prune <- scLM1E$cptable[which.min(scLM1E$cptable[,"xerror"]),"CP"]
cp.prune
plotcp(scLM1E)
scLM1ENew <- rpart(formula = scLSumIndx,
                   data = scL_Train,
                   method = "class", 
                   parms = list(split = "information"),
                   control = rpart.control(minsplit = 20, cp = cp.prune, xval = 0))
rpart.rules(scLM1ENew, cover = TRUE)
summary(scLM1ENew) #' overfitted
varImp(scLM1ENew)
scL.predB <- predict(scLM1ENew, scLearnNew) %>%
  as.data.frame()
head(scL.predB)
scL.predB <- mutate(scL.predB,
                    science_perf = as.factor(ifelse(High >= 0.5, "High", "Low"))) %>% 
  select(science_perf)

#' Confusion Matrix Model 1E
confusionMatrix(scL.predB$science_perf, scLearnNew$science_perf,mode = "everything")
#' 
