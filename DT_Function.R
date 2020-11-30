#' my general function 
#' goal ialah nk print Confusion Matrix
#' I would like to have two CM for each models
#' cm.np and cm.p(final model)

#' decision tree function using rpart
#' takes the value df,gini/information,cp value
#' use training set
#' 
dtGini <- function(dfTrain,cp) {
  rpart(formula =  science_perf~.,
        data = dfTrain,
        method = "class", 
        parms = list(split = "gini"),
        control = rpart.control(cp = cp))
}

dtInfo <- function(dfTrain,cp) {
  rpart(formula =  science_perf~.,
        data = dfTrain,
        method = "class", 
        parms = list(split = "information"),
        control = rpart.control(cp = cp))
} 
#' function predict
#' predictDataTable.confusionMatrix
predDT.cm<-function(modelDT,dfTest){
  x<-predict(modelDT,dfTest) %>% as.data.frame()
  y<-mutate(x,science_perf = as.factor(ifelse(High >= 0.5, "High", "Low"))) %>% select(science_perf)
  confusionMatrix(y$science_perf,dfTest$science_perf,mode = "everything")  
}

#' prediction function untuk Gini
modelGini<-function(dfTrain,dfTest){
  set.seed(1234)
  model.NP<- dtGini(dfTrain,0)
  cp.prune <- model.NP$cptable[which.min(model.NP$cptable[,"xerror"]),"CP"]
  #new prune model
  model.P<- dtGini(dfTrain,cp.prune)
  predDT.cm(model.P,dfTest)
}

#' prediction function untuk Information Gain
modelInfo<-function(dfTrain,dfTest){
  set.seed(1234)
  model.NP<- dtInfo(dfTrain,0)
  cp.prune <- model.NP$cptable[which.min(model.NP$cptable[,"xerror"]),"CP"]
  #new prune model
  model.P<- dtInfo(dfTrain,cp.prune)
  predDT.cm(model.P,dfTest)
}

#' nak create fx utk Important Variable
#' varImp feed from data rpart
#' rpart perlukan data
#' rpart yg pruned perlukan cp
impVar <- function(modelDT){
  varImp(modelDT)
}

#' Important Variable Function for Gini
IV.gini <- function(dfTrain){
  set.seed(1234)
  model.NP <- dtGini(dfTrain,0)
  cp.prune <- model.NP$cptable[which.min(model.NP$cptable[,"xerror"]),"CP"]
  #new prune model
  model.P <- dtGini(dfTrain,cp.prune)
  toMutate <- impVar(model.P)
  toOrder <- mutate(toMutate, Variable=rownames(toMutate))
  toOrder[order(toOrder$Overall,decreasing = TRUE),]
}

#' Important Variable Function for Information Gain
IV.info <- function(dfTrain){
  set.seed(1234)
  model.NP <- dtInfo(dfTrain,0)
  cp.prune <- model.NP$cptable[which.min(model.NP$cptable[,"xerror"]),"CP"]
  #new prune model
  model.P <- dtInfo(dfTrain,cp.prune)
  toMutate <- impVar(model.P)
  toOrder <- mutate(toMutate, Variable=rownames(toMutate))
  toOrder[order(toOrder$Overall,decreasing = TRUE),]
}

##' #--------------------------------------------#
##' #--------------------------------------------#
##' ROC & AUC 
##' #--------------------------------------------#
##' #--------------------------------------------#
#' nak create function untuk plot ROC Curve
#' plot        function -> performance functions
#' performance function -> prediction  function
#' prediction  function -> predict     function
#' predict     function -> rpart       function 
ROCAUC_Info <- function(dfTrain,dfTest){
  set.seed(1234)
  model.NP<- dtInfo(dfTrain,0)
  cp.prune <- model.NP$cptable[which.min(model.NP$cptable[,"xerror"]),"CP"]
  #new prune model
  model.P<- dtInfo(dfTrain,cp.prune)
  y <- predict(model.P, type="prob",dfTest)[,2]
  pred <- prediction(y,dfTest$science_perf)
  plot(performance(pred,"tpr","fpr"))
  abline(0, 1, lty = 2)
  perfAUC <- performance(pred,"auc")
  perfAUC@y.values
}

#'
ROCAUC_Gini <- function(dfTrain,dfTest){
  set.seed(1234)
  model.NP<- dtGini(dfTrain,0)
  cp.prune <- model.NP$cptable[which.min(model.NP$cptable[,"xerror"]),"CP"]
  #new prune model
  model.P<- dtGini(dfTrain,cp.prune)
  y <- predict(model.P, type="prob",dfTest)[,2]
  pred <- prediction(y,dfTest$science_perf)
  plot(performance(pred,"tpr","fpr"))
  abline(0, 1, lty = 2)
  perfAUC <- performance(pred,"auc")
  perfAUC@y.values
}


##' #--------------------------------------------#
##' #--------------------------------------------#
#'  [Model1a] mdl1a - gini (train70-30 & xval 10)
##' #--------------------------------------------#
##' #--------------------------------------------#
Rprof(tf <- "Rprof.out")
model1a<-modelGini(train.New.73,test.New.73)
model1a
IV.gini73 <- IV.gini(train.New.73)
IV.gini73
Rprof (NULL) 
#print(summaryRprof(tf))
model1a$overall
model1a$table
ROCAUC_Gini(train.New.73,test.New.73)
##' #--------------------------------------------#
##' #--------------------------------------------#
#' [Model1b] mdl1b - information (train70-30 & xval 10)
##' #--------------------------------------------#
##' #--------------------------------------------#
set.seed(1234)
Rprof(tf <- "Rprof.out")
model1b<-modelInfo(train.New.73,test.New.73)
model1b
IV.info73 <- IV.info(train.New.73)
IV.info73
Rprof (NULL)
#print(summaryRprof(tf))
model1b$overall
model1b$table
ROCAUC_Info(train.New.73,test.New.73)

##' #--------------------------------------------#
##' #--------------------------------------------#
##' [Model2a] mdl2a - gini (train80-20 & xval 10)
##' #--------------------------------------------#
##' #--------------------------------------------#
Rprof(tf <- "Rprof.out")
model2a<-modelGini(train.New.82,test.New.82)
model2a
IV.gini82 <- IV.gini(train.New.82)
IV.gini82
Rprof (NULL) 
#print(summaryRprof(tf))
model2a$overall
model2a$table
ROCAUC_Gini(train.New.82,test.New.82)

##' #--------------------------------------------#
##' #--------------------------------------------#
##' [Model2b] mdl2b - information (train80-20 & xval 10)
##' #--------------------------------------------#
##' #--------------------------------------------#
Rprof(tf <- "Rprof.out")
model2b<-modelInfo(train.New.82,test.New.82)
model2b
IV.info82 <- IV.info(train.New.82)
IV.info82
Rprof (NULL)  
#print(summaryRprof(tf))
model2b$overall
model2b$table
ROCAUC_Info(train.New.82,test.New.82)

##' #--------------------------------------------#
##' #--------------------------------------------#
##' [Model3a] mdl4a - gini (train90-10 & xval 10)
##' #--------------------------------------------#
##' #--------------------------------------------#
Rprof(tf <- "Rprof.out")
model3a<-modelGini(train.New.91,test.New.91)
model3a
IV.gini91 <- IV.gini(train.New.91)
IV.gini91
Rprof (NULL)  
#print(summaryRprof(tf))
model3a$overall
model3a$table
ROCAUC_Gini(train.New.91,test.New.91)

##' #--------------------------------------------#
##' #--------------------------------------------#
##' [Model3b] mdl4b - information (train90-10 & xval 10)
##' #--------------------------------------------#
##' #--------------------------------------------#
Rprof(tf <- "Rprof.out")
model3b<-modelInfo(train.New.91,test.New.91)
model3b
IV.info91 <- IV.info(train.New.91)
IV.info91
Rprof (NULL)  
#print(summaryRprof(tf))
model3b$overall
model3b$table
ROCAUC_Info(train.New.91,test.New.91)

##' #--------------------------------------------#
##' #--------------------------------------------#
#'
#'GINI INDEX
model1a$overall[c("Accuracy")]*100
model1a$byClass[c("Sensitivity","Specificity","Precision","F1")]
IV.gini73
model1a$table
ROCAUC_Gini(train.New.73,test.New.73)
model1a$overall["Kappa"]
#
model2a$overall[c("Accuracy")]*100
model2a$byClass[c("Sensitivity","Specificity","Precision","F1")]
IV.gini82
model2a$table
ROCAUC_Gini(train.New.82,test.New.82)
model2a$overall["Kappa"]
#'
model3a$overall["Accuracy"]
model3a$byClass[c("Sensitivity","Specificity","Precision","F1")]
IV.gini91
model3a$table
ROCAUC_Gini(train.New.91,test.New.91)
#'

#'INFORMATION GAIN
model1b$overall[c("Accuracy")]*100
model1b$byClass[c("Sensitivity","Specificity","Precision","F1")]*100
IV.info73
model1b$table
ROCAUC_Info(train.New.73,test.New.73)
model1b$overall["Kappa"]
#
model2b$overall[c("Accuracy")]*100
model2b$byClass[c("Sensitivity","Specificity","Precision","F1")]
IV.info82
model2b$table
ROCAUC_Info(train.New.82,test.New.82)
#
model3b$overall[c("Accuracy")]
model3b$byClass[c("Sensitivity","Specificity","Precision","F1")]
IV.info91
model3b$table
ROCAUC_Info(train.New.91,test.New.91)
#
#
#
##' #--------------------------------------------#
##' #--------------------------------------------#
##' TREE PLOTTING
##' #--------------------------------------------#
##' #--------------------------------------------#

treeGini73 <- rpart(formula =  science_perf~.,
                    data = train.New.73,
                    method = "class", 
                    parms = list(split = "gini"))
fancyRpartPlot(treeGini73)
rpart.plot(treeGini73)
rpart.rules(treeGini73)
print(treeGini73)

treeInfo73 <- rpart(formula =  science_perf~.,
                    data = train.New.73,
                    method = "class", 
                    parms = list(split = "information"))
fancyRpartPlot(treeInfo73)

treeInfo82 <- rpart(formula =  science_perf~.,
                    data = train.New.82,
                    method = "class", 
                    parms = list(split = "information"))
fancyRpartPlot(treeInfo82)


##' #--------------------------------------------#
##' #--------------------------------------------#
set.seed(1234)
j48tree73 <- J48(formula =  science_perf~.,
                    data = test.New.73)
#print(j48tree73)
summary(j48tree73) #calls function same as evaluate_Weka_classifier

j48eval <- evaluate_Weka_classifier(j48tree73,numFolds = 10,complexity = TRUE)
table(test.New.73$science_perf, predict(j48tree73))

#' using CARET
train()



##' #--------------------------------------------#
##' #--------------------------------------------#

chaid1<-chaid(formula =  science_perf~.,
              data = train.New.73)



