#' This is where all the analysis starts for Decision Trees
#' #-------------------------------------------------------------------------------------# 
#' THE PLAN:
#' Building [Model 1] All latent var
#' latVar73a - gini (train70-30 & xval 10)
#' latVar73b - information (train70-30 & xval 10)
#' latVar82a - gini (train80-20 & xval 10)
#' latVar82b - information (train80-20 & xval 10)

##' #-------------------------------------------------------------------------------------# 
##' #-------------------------------------------------------------------------------------# 
#'  [Model1a] #' latVar73a - gini (train70-30 & xval 10)
##' #-------------------------------------------------------------------------------------#  
##' #-------------------------------------------------------------------------------------# 
set.seed(1234)
#' train the model with no validation first to see how it goes
latVar73a <- rpart(formula = science_perf ~.,
                   data = train.latVarImp.73,
                   method = "class", 
                   parms = list(split = "gini"),
                   control = rpart.control(cp = 0))
#plot(tsiM1a)
printcp(latVar73a)

#' using test sample to predict without pruning (validate using xval)
#latentVarwith7030usingGini.predict.notprune
lv73a.pred.np <- predict(latVar73a, test.latVarImp.73) %>%
  as.data.frame()
head(lv73a.pred.np)
lv73a.pred.np <- mutate(lv73a.pred.np,
                     science_perf = as.factor(ifelse(High >= 0.5, "High", "Low"))) %>% 
  select(science_perf)

#' Confusion Matrix - Gini + datasetsAll for test data of unprune data
cmM1a.np <- confusionMatrix(lv73a.pred.np$science_perf, test.latVarImp.73$science_perf,mode = "everything")
cmM1a.np
#' compare with the error rate for missclass (1-missclass = Accuracy)
#' misclassification error rate at .3111 = .69 accuracy of training sample 
#library("tree")
#latVar.tree73 <-tree(science_perf ~.,data = train.latVarImp.73)
#summary(latVar.tree73)
#plot(latVar.tree73)
#text(latVar.tree73,pretty = 0)

#' ------------------
#' validation test set
#' ------------------
cp.prune <- latVar73a$cptable[which.min(latVar73a$cptable[,"xerror"]),"CP"]
cp.prune
#plotcp(tsiM1a)
latVar73aNew <- rpart(formula =  science_perf~.,
                     data = train.latVarImp.73,
                     method = "class", 
                     parms = list(split = "gini"),
                     control = rpart.control(cp = cp.prune))
plot(latVar73aNew)
text(latVar73aNew,pretty = 0)
#rpart.rules(latVar73aNew, cover = TRUE)
#summary(latVar73aNew) 

#' ImportantVar.latVar7030GiniNew(pruned)
IV.latVar73aNew<-varImp(latVar73aNew)
IV.latVar73aNew <- mutate(IV.latVar73aNew, Variable = rownames(IV.latVar73aNew))
IV.latVar73aNew <- IV.latVar73aNew[order(IV.latVar73aNew$Overall,decreasing = TRUE),]
IV.latVar73aNew

#latentVarwith7030usingGini.predict.prune
lv73a.pred.p <- predict(latVar73aNew, test.latVarImp.73) %>%
  as.data.frame()
head(lv73a.pred.p)
lv73a.pred.p <- mutate(lv73a.pred.p,
                       science_perf = as.factor(ifelse(High >= 0.5, "High", "Low"))) %>% 
  select(science_perf)

#' Confusion Matrix Model 1  - Gini + pruned
cmM1a.p <- confusionMatrix(lv73a.pred.p$science_perf, test.latVarImp.73$science_perf,mode = "everything")
cmM1a.p 

#' not prune VS pruned
cmM1a.np
cmM1a.p 


##' #-------------------------------------------------------------------------------------# 
##' #-------------------------------------------------------------------------------------# 
#' 3 [Model1b]- #' latVar73b - information (train70-30 & xval 10)
##' #-------------------------------------------------------------------------------------# 
##' #-------------------------------------------------------------------------------------# 
set.seed(1234)
latVar73b <- rpart(formula = science_perf ~.,
                   data = train.latVarImp.73,
                   method = "class", 
                   parms = list(split = "information"),
                   control = rpart.control(cp = 0))

#plot(tsiM2a)

#' using test sample to predict without pruning (validate using xval)
#latentVarwith7030usingGini.predict.notprune
lv73b.pred.np <- predict(latVar73b, test.latVarImp.73) %>%
  as.data.frame()
head(lv73b.pred.np)
lv73b.pred.np <- mutate(lv73b.pred.np,
                        science_perf = as.factor(ifelse(High >= 0.5, "High", "Low"))) %>% 
  select(science_perf)

#' Confusion Matrix - Gini + datasetsAll for test data of unprune data
cmM1b.np <- confusionMatrix(lv73b.pred.np$science_perf, test.latVarImp.73$science_perf,mode = "everything")
cmM1b.np
#' compare with the error rate for missclass (1-missclass = Accuracy)
#' misclassification error rate at .3111 = .69 accuracy of training sample 
#library("tree")
#latVar.tree73 <-tree(science_perf ~.,data = train.latVarImp.73)
#summary(latVar.tree73)
#plot(latVar.tree73)
#text(latVar.tree73,pretty = 0)

#' ------------------
#' validation test set
#' ------------------
cp.prune <- latVar73b$cptable[which.min(latVar73b$cptable[,"xerror"]),"CP"]
cp.prune
#plotcp(latVar73b)
latVar73bNew <- rpart(formula =  science_perf ~.,
                   data = train.latVarImp.73,
                   method = "class", 
                   parms = list(split = "information"),
                   control = rpart.control(cp = cp.prune))

#plot(latVar73bNew)
#rpart.rules(tsiM2aNew, cover = TRUE)
#summary(tsiM2aNew) #' this is signed of overfitted

#' ImportantVar.latVar7030InformationNew(pruned)
IV.latVar73bNew <- varImp(latVar73bNew)
IV.latVar73bNew <- mutate(IV.latVar73bNew, Variable = rownames(IV.latVar73bNew))
IV.latVar73bNew <- IV.latVar73bNew[order(IV.latVar73bNew$Overall,decreasing = TRUE),]
IV.latVar73bNew

#' latentVarwith7030usingInformation.predict.prune
lv73b.pred.p <- predict(latVar73bNew, test.latVarImp.73) %>%
  as.data.frame()
head(lv73b.pred.p)
lv73b.pred.p <- mutate(lv73b.pred.p,
                     science_perf = as.factor(ifelse(High >= 0.5, "High", "Low"))) %>% 
  select(science_perf)

#' Confusion Matrix tsi2 - Entropy + datasetsAll
cmM1b.p <- confusionMatrix(lv73b.pred.p$science_perf, test.latVarImp.73$science_perf,mode = "everything")
cmM1b.p

#' not prune VS pruned
cmM1b.np
cmM1b.p 

##' #-------------------------------------------------------------------------------------# 
##' #-------------------------------------------------------------------------------------# 
#' 2 [tsiM1b] All TSI summaries combined - gini (train80-20 & xval 10)
##' #-------------------------------------------------------------------------------------#  
##' #-------------------------------------------------------------------------------------# 
set.seed(1234)
tsiM1b <- rpart(formula = science_perfM ~.,
                data = train.TSI.82,
                method = "class", 
                parms = list(split = "gini"),
                control = rpart.control(cp = 0))
#plot(tsiM1b)
#printcp(tsiM1b)
summary(tree(science_perfM ~., data = train.TSI.82))

#' using test sample to predict without pruning (validate using xval)
tsiM1b.pred <- predict(tsiM1b, test.TSI.82) %>%
  as.data.frame()
head(tsiM1b.pred)
tsiM1b.pred <- mutate(tsiM1b.pred,
                      science_perfM = as.factor(ifelse(High >= 0.5, "High", "Low"))) %>% 
  select(science_perfM)

#' Confusion Matrix tsi1 - Gini + datasetsAll for test data of unprune data
cmM1b <- confusionMatrix(tsiM1b.pred$science_perfM, test.TSI.82$science_perfM,mode = "everything")
cmM1b 

#' ------------------
#' validation test set
#' ------------------
cp.prune <- tsiM1b$cptable[which.min(tsiM1b$cptable[,"xerror"]),"CP"]
cp.prune
plotcp(tsiM1b)
tsiM1bNew <- rpart(formula =  science_perfM ~.,
                   data = train.TSI.82,
                   method = "class", 
                   parms = list(split = "gini"),
                   control = rpart.control(cp = cp.prune))
#plot(tsiM1bNew)
#rpart.rules(tsiM1bNew, cover = TRUE)
#summary(tsiM1bNew) #' this is signed of overfitted

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
##' #-------------------------------------------------------------------------------------# 
#' 4 [tsiM2b] - information (train80-20 & xval 10)
##' #-------------------------------------------------------------------------------------# 
##' #-------------------------------------------------------------------------------------# 
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
