#' #-------------------------------------------------------------------------------------# 
#' THE PLAN:
#' Building [Model 3] All TSI summaries combined
#' tsiM1a - gini (train70-30 & xval 10)
#' tsiM1b - gini (train80-20 & xval 10)
#' tsiM2a - information (train70-30 & xval 10)
#' tsiM2b - information (train80-20 & xval 10)

##' #-------------------------------------------------------------------------------------# 
#' 1 [tsiM1a] All TSI summaries combined - gini (train70-30 & xval 10)
#' 
set.seed(1234)
tsiM1a <- rpart(formula = science_perfM ~.,
                  data = train.TSI.73,
                  method = "class", 
                  parms = list(split = "gini"),
                  control = rpart.control(cp = 0))
plot(tsiM1a)
printcp(tsiM1a)

cp.prune <- tsiM1a$cptable[which.min(tsiM1a$cptable[,"xerror"]),"CP"]
cp.prune
plotcp(tsiM1a)
tsiM1aNew <- rpart(formula =  science_perfM ~.,
                     data = train.TSI.73,
                     method = "class", 
                     parms = list(split = "gini"),
                     control = rpart.control(cp = cp.prune))
plot(tsiM1aNew)
rpart.rules(tsiM1aNew, cover = TRUE)
summary(tsiM1aNew) #' this is signed of overfitted

impVar.tsi1a<-(varImp(tsiM1aNew))
impVar.tsi1a <- mutate(impVar.tsi1a, Variable = rownames(impVar.tsi1a))
impVar.gini73 <- impVar.tsi1a[order(impVar.tsi1a$Overall,decreasing = TRUE),]
impVar.gini73

tsi1a.pred <- predict(tsiM1aNew, test.TSI.73) %>%
  as.data.frame()
head(tsi1a.pred)
tsi1a.pred <- mutate(tsi1a.pred,
                      science_perfM = as.factor(ifelse(High >= 0.5, "High", "Low"))) %>% 
  select(science_perfM)

#' Confusion Matrix tsi1 - Gini + datasetsAll
cm1a <- confusionMatrix(tsi1a.pred$science_perfM, test.TSI.73$science_perfM,mode = "everything")
cm1a 

##' #-------------------------------------------------------------------------------------# 
#' 2 [tsiM1b] All TSI summaries combined - gini (train80-20 & xval 10)
#' 
set.seed(1234)
tsiM1b <- rpart(formula = science_perfM ~.,
                data = train.TSI.82,
                method = "class", 
                parms = list(split = "gini"),
                control = rpart.control(cp = 0))
plot(tsiM1b)
printcp(tsiM1b)

cp.prune <- tsiM1b$cptable[which.min(tsiM1b$cptable[,"xerror"]),"CP"]
cp.prune
plotcp(tsiM1b)
tsiM1bNew <- rpart(formula =  science_perfM ~.,
                    data = train.TSI.82,
                    method = "class", 
                    parms = list(split = "gini"),
                    control = rpart.control(cp = cp.prune))
plot(tsiM1bNew)
rpart.rules(tsiM1bNew, cover = TRUE)
summary(tsiM1bNew) #' this is signed of overfitted

impVar.tsi1b <- varImp(tsiM1bNew)
impVar.tsi1b <- mutate(impVar.tsi1b, Variable = rownames(impVar.tsi1b))
impVar.gini82 <- impVar.tsi1b[order(impVar.tsi1b$Overall,decreasing = TRUE),]
impVar.gini82

tsi1b.pred <- predict(tsiM1bNew, test.TSI.82) %>%
  as.data.frame()
head(tsi1b.pred)
tsi1b.pred <- mutate(tsi1b.pred,
                     science_perfM = as.factor(ifelse(High >= 0.5, "High", "Low"))) %>% 
  select(science_perfM)

#' Confusion Matrix tsi1 - Gini + datasetsAll
cm1b <- confusionMatrix(tsi1b.pred$science_perfM, test.TSI.82$science_perfM,mode = "everything")
cm1b
##' #-------------------------------------------------------------------------------------# 
#' 3 [tsiM2a]- information (train70-30 & xval 10)
set.seed(1234)
tsiM2a <- rpart(formula = science_perfM ~.,
               data = train.TSI.73,
               method = "class", 
               parms = list(split = "information"),
               control = rpart.control(cp = 0))
plot(tsiM2a)
printcp(tsiM2a)

cp.prune <- tsiM2a$cptable[which.min(tsiM2a$cptable[,"xerror"]),"CP"]
cp.prune
plotcp(tsiM2a)
tsiM2aNew <- rpart(formula =  science_perfM ~.,
                  data = train.TSI.73,
                  method = "class", 
                  parms = list(split = "information"),
                  control = rpart.control(cp = cp.prune))
plot(tsiM2aNew)
rpart.rules(tsiM2aNew, cover = TRUE)
summary(tsiM2aNew) #' this is signed of overfitted

impVar.tsi2a <- varImp(tsiM2aNew)
impVar.tsi2a <- mutate(impVar.tsi2a, Variable = rownames(impVar.tsi2a))
impVar.info73 <- impVar.tsi2a[order(impVar.tsi2a$Overall,decreasing = TRUE),]
impVar.info73

tsi2a.pred <- predict(tsiM2aNew, test.TSI.73) %>%
  as.data.frame()
head(tsi2a.pred)
tsi2a.pred <- mutate(tsi2a.pred,
                   science_perfM = as.factor(ifelse(High >= 0.5, "High", "Low"))) %>% 
  select(science_perfM)

#' Confusion Matrix tsi2 - Entropy + datasetsAll
cm2a <- confusionMatrix(tsi2a.pred$science_perfM, test.TSI.73$science_perfM,mode = "everything")
cm2a

##' #-------------------------------------------------------------------------------------# 
#' 4 [tsiM2b] - information (train80-20 & xval 10)
#' 
set.seed(1234)
tsiM2b <- rpart(formula = science_perfM ~.,
               data = train.TSI.82,
               method = "class", 
               parms = list(split = "information"),
               control = rpart.control(cp = 0))
plot(tsiM2b)
printcp(tsiM2b)

cp.prune <- tsiM2b$cptable[which.min(tsiM2b$cptable[,"xerror"]),"CP"]
cp.prune
plotcp(tsiM2b)
tsiM2bNew <- rpart(formula =  science_perfM ~.,
                  data = train.TSI.82,
                  method = "class", 
                  parms = list(split = "information"),
                  control = rpart.control(cp = cp.prune))
plot(tsiM2bNew)
rpart.rules(tsiM2bNew, cover = TRUE)
summary(tsiM2bNew) #' this is signed of overfitted

impVar.tsi2b <- varImp(tsiM2bNew)
impVar.tsi2b <- mutate(impVar.tsi2b, Variable = rownames(impVar.tsi2b))
impVar.info82 <- impVar.tsi2b[order(impVar.tsi2b$Overall, decreasing = TRUE),]
impVar.info82

tsi2b.pred <- predict(tsiM2bNew, test.TSI.82) %>%
  as.data.frame()
head(tsi2b.pred)


tsi2b.pred <- mutate(tsi2b.pred,
                    science_perf = as.factor(ifelse(High >= 0.5, "High", "Low"))) %>% 
  select(science_perf)


#' Confusion Matrix tsi3 - Entropy + train.test
cm2b <- confusionMatrix(tsi2b.pred$science_perf, test.TSI.82$science_perf,mode = "everything")
cm2b

#' sensitivity, recall, hit rate, or true positive rate (TPR)
#' specificity, selectivity or true negative rate (TNR)
#' precision or positive predictive value (PPV)
#' https://en.wikipedia.org/wiki/Precision_and_recall#Definition_(classification_context)
cm1a$overall[c("Accuracy")]
cm1a$byClass[c("Sensitivity","Specificity","Precision","F1")]
impVar.gini73
cm1b$overall["Accuracy"]
cm1b$byClass[c("Sensitivity","Specificity","Precision","F1")]
impVar.gini82

cm2a$overall["Accuracy"]
cm2a$byClass[c("Sensitivity","Specificity","Precision","F1")]
impVar.info73
cm2b$overall["Accuracy"]
cm2b$byClass[c("Sensitivity","Specificity","Precision","F1")]
impVar.info82
