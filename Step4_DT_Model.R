#' THIS IS THE OLD CODE
#' THE FINAL CODE HAS BEEN MOVED TO NEW SCRIPT: DT_Function.R
#' THE NEW SCRIPT HAS BEEN SIMPLIFIED USING FUNCTIONS INSTEAD OF HARD CODED EVERY MODEL
#' AND THIS IS WAY MORE CODE FRIENDLY AND EASY TO MANAGE AND REPRODUCIBLE
#' 
#' This is where all the analysis starts for Decision Trees
#' #-------------------------------------------------------------------------------------# 
#' THE PLAN:
#' Building [Model 1] All latent var
#' scRltd73a - gini (train70-30 & xval 10)
#' scRltd73b - information (train70-30 & xval 10)
#' scRltd82a - gini (train80-20 & xval 10)
#' scRltd82b - information (train80-20 & xval 10) - x buat

##' #-------------------------------------------------------------------------------------# 
##' #-------------------------------------------------------------------------------------# 
#'  [Model1a] 
#'  #' Science Related for non-cognitive:
#'  scRltd73a - gini (train70-30 & xval 10)
##' #-------------------------------------------------------------------------------------#  
##' #-------------------------------------------------------------------------------------# 
set.seed(1234)
#' train the model with no validation first to see how it goes
scRltd73a <- rpart(formula = science_perf ~.,
                   data = train.New.73,
                   method = "class", 
                   parms = list(split = "gini"),
                   control = rpart.control(cp = 0,xval = 0))


#' using test sample to predict without pruning (validate using xval)
#' ScienceRelatedwith7030usingGini.predict.notprune
sR73a.pred.np <- predict(scRltd73a, test.New.73) %>%
  as.data.frame()
head(sR73a.pred.np)
sR73a.pred.np <- mutate(sR73a.pred.np,
                     science_perf = as.factor(ifelse(High >= 0.5, "High", "Low"))) %>% 
  select(science_perf)

#' Confusion Matrix - Gini + datasetsAll for test data of unprune data
cmM1a.np <- confusionMatrix(sR73a.pred.np$science_perf, test.New.73$science_perf,mode = "everything")
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
cp.prune <- scRltd73a$cptable[which.min(scRltd73a$cptable[,"xerror"]),"CP"]
cp.prune
#' if xval = 0, we can't use above code since xerror is not available
printcp(scRltd73a)

#plotcp(tsiM1a)
scRltd73aNew <- rpart(formula =  science_perf~.,
                     data = train.New.73,
                     method = "class", 
                     parms = list(split = "gini"),
                     control = rpart.control(cp = cp.prune,xval = 0))

#plot(scRltd73aNew)
#text(scRltd73aNew,pretty = 0)
#rpart.rules(scRltd73aNew, cover = TRUE)
#summary(scRltd73aNew) 

#' ImportantVar.latVar7030Gini(pruned)
IV.scRltd73aNew <- varImp(scRltd73aNew)
IV.scRltd73aNew <- mutate(IV.scRltd73aNew, Variable = rownames(IV.scRltd73aNew))
IV.scRltd73aNew <- IV.scRltd73aNew[order(IV.scRltd73aNew$Overall,decreasing = TRUE),]
IV.scRltd73aNew

#' ScienceRelatedwith7030usingGini.predict.prune
sR73a.pred.p <- predict(scRltd73aNew, test.New.73) %>%
  as.data.frame()
head(sR73a.pred.p)
sR73a.pred.p <- mutate(sR73a.pred.p,
                       science_perf = as.factor(ifelse(High >= 0.5, "High", "Low"))) %>% 
  select(science_perf)

performance(sR73a.pred.p,"tpr","fpr")

#' Confusion Matrix Model 1  - Gini + pruned
cmM1a.p <- confusionMatrix(sR73a.pred.p$science_perf, test.New.73$science_perf,mode = "everything")
cmM1a.p 

#' not prune VS pruned
cmM1a.np
cmM1a.p 


##' #-------------------------------------------------------------------------------------# 
##' #-------------------------------------------------------------------------------------# 
#' [Model1b]- 
#' Science Related for non-cognitive
#' #' scRltd73b - information (train70-30 & xval 10)
##' #-------------------------------------------------------------------------------------# 
##' #-------------------------------------------------------------------------------------# 
set.seed(1234)
scRltd73b <- rpart(formula = science_perf ~.,
                   data = train.New.73,
                   method = "class", 
                   parms = list(split = "information"),
                   control = rpart.control(cp = 0))

#plot(tsiM2a)

#' using test sample to predict without pruning (validate using xval)
#' ScienceRelatedwith7030usingInformation.predict.notprune
sR73b.pred.np <- predict(scRltd73b, test.New.73) %>%
  as.data.frame()
head(sR73b.pred.np)
sR73b.pred.np <- mutate(sR73b.pred.np,
                        science_perf = as.factor(ifelse(High >= 0.5, "High", "Low"))) %>% 
  select(science_perf)

#' Confusion Matrix - Gini + datasetsAll for test data of unprune data
cmM1b.np <- confusionMatrix(sR73b.pred.np$science_perf, test.New.73$science_perf,mode = "everything")
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
cp.prune <- scRltd73b$cptable[which.min(scRltd73b$cptable[,"xerror"]),"CP"]
cp.prune
#plotcp(scRltd73b)
scRltd73bNew <- rpart(formula =  science_perf ~.,
                   data = train.New.73,
                   method = "class", 
                   parms = list(split = "information"),
                   control = rpart.control(cp = cp.prune))

#plot(scRltd73bNew)
#rpart.rules(tsiM2aNew, cover = TRUE)
#summary(tsiM2aNew) #' this is signed of overfitted

#' ImportantVar.latVar7030Information(pruned)
IV.scRltd73bNew <- varImp(scRltd73bNew)
IV.scRltd73bNew <- mutate(IV.scRltd73bNew, Variable = rownames(IV.scRltd73bNew))
IV.scRltd73bNew <- IV.scRltd73bNew[order(IV.scRltd73bNew$Overall,decreasing = TRUE),]
IV.scRltd73bNew

#' ScienceRelatedwith7030usingInformation.predict.prune
sR73b.pred.p <- predict(scRltd73bNew, test.New.73) %>%
  as.data.frame()
head(sR73b.pred.p)
sR73b.pred.p <- mutate(sR73b.pred.p,
                     science_perf = as.factor(ifelse(High >= 0.5, "High", "Low"))) %>% 
  select(science_perf)

#' Confusion Matrix tsi2 - Entropy + datasetsAll
cmM1b.p <- confusionMatrix(sR73b.pred.p$science_perf, test.New.73$science_perf,mode = "everything")
cmM1b.p

#' not prune VS pruned
cmM1b.np
cmM1b.p
#gini vs info
cmM1a.p
cmM1b.p

##' #-------------------------------------------------------------------------------------# 
##' #-------------------------------------------------------------------------------------# 
#' [Model2a]- 
#' Science Related for non-cognitive
#' #' scRltd82a - gini (train80-20 & xval 10)
##' #-------------------------------------------------------------------------------------#  
##' #-------------------------------------------------------------------------------------# 
set.seed(1234)
scRltd82a <- rpart(formula = science_perf ~.,
                data = train.New.82,
                method = "class", 
                parms = list(split = "gini"),
                control = rpart.control(cp = 0))
#plot(tsiM1b)
#printcp(tsiM1b)
summary(tree(science_perf ~., data = train.New.82))

#' using test sample to predict without pruning (validate using xval)
scRltd82a.pred <- predict(scRltd82a, test.New.82) %>%
  as.data.frame()
head(scRltd82a.pred)
scRltd82a.pred <- mutate(scRltd82a.pred,
                      science_perf = as.factor(ifelse(High >= 0.5, "High", "Low"))) %>% 
  select(science_perf)

#' Confusion Matrix Model2a - Gini + datasetsAll for test data of unprune data
cmM2a.np <- confusionMatrix(scRltd82a.pred$science_perf, test.New.82$science_perf,mode = "everything")
cmM2a.np 

#' ------------------
#' validation test set
#' ------------------
cp.prune <- scRltd82a$cptable[which.min(scRltd82a$cptable[,"xerror"]),"CP"]
cp.prune
plotcp(scRltd82a)
scRltd82aNew <- rpart(formula =  science_perf ~.,
                   data = train.New.82,
                   method = "class", 
                   parms = list(split = "gini"),
                   control = rpart.control(cp = cp.prune))
#plot(tsiM1bNew)
#rpart.rules(tsiM1bNew, cover = TRUE)
#summary(tsiM1bNew) #' this is signed of overfitted

#' ImportantVar.latVar8020Gini(pruned)
IV.scRltd82aNew <- varImp(scRltd82aNew,scale=FALSE)
IV.scRltd82aNew <- mutate(IV.scRltd82aNew, Variable = rownames(IV.scRltd82aNew))
IV.scRltd82aNew <- IV.scRltd82aNew[order(IV.scRltd82aNew$Overall,decreasing = TRUE),]
IV.scRltd82aNew

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
