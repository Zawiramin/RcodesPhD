#' #-------------------------------------------------------------------------------------# 
#' THE PLAN:
#' Building [Model 2] Tree Using Gini Index
#' [Model 2]- ENVIRONMENTAL AWARENESS AND OPTIMISMS with Gini Index with xval = 10 
#' [Model 1a]- ENVIRONMENTAL AWARENESS AND OPTIMISMS with Gini Index with 70-30 & XVAL 10
#' 
#' Building [Model 2E] Tree Using Entropy Index (Information Gain)
#' [Model 1E]- SCIENCE LEARNING with Information Gain with xval = 10 
#' [Model 1Ea]- SCIENCE LEARNING with Information Gain with 70-30 & XVAL 10

##' #-------------------------------------------------------------------------------------# 
#' [Model 2]- ENVIRONMENTAL AWARENESS AND OPTIMISMS with Gini Index with xval = 10 
#' 
envoptSumIndx <- science_perf ~ ENVAWARE+ENVOPT+ESCS

envoptM1 <- rpart(formula = envoptSumIndx,
               data = envAwareOptNew,
               method = "class", 
               parms = list(split = "gini"),
               control = rpart.control(minsplit = 20, cp = 0,xval = 10))

printcp(envoptM1)

cp.prune <- envoptM1$cptable[which.min(envoptM1$cptable[,"xerror"]),"CP"]
cp.prune
plotcp(envoptM1)
envoptM1new <- rpart(formula =  envoptSumIndx,
                  data = envAwareOptNew,
                  method = "class", 
                  parms = list(split = "gini"),
                  control = rpart.control(minsplit = 20, cp = cp.prune, xval = 0))
rpart.rules(envoptM1new, cover = TRUE)
summary(envoptM1new) #' this is signed of overfitted
varImp(envoptM1new)
envopt.pred <- predict(envoptM1new, envAwareOptNew) %>%
  as.data.frame()
head(envopt.pred)
envopt.pred <- mutate(envopt.pred,
                   science_perf = as.factor(ifelse(High >= 0.5, "High", "Low"))) %>% 
  select(science_perf)

#' Confusion Matrix Model 2
confusionMatrix(envopt.pred$science_perf, envAwareOptNew$science_perf,mode = "everything")
#' 



