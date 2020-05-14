#' my general function 
#' goal ialah nk print Confusion Matrix
#' I would like to have two CM for each models
#' cm.np and cm.p(final model)

#' decision tree function using rpart
#' takes the value df,gini/information,cp value
#' use training set
dtGini <- function(dfTrain,cp){
  x<-rpart(formula =  science_perf~.,
           data = dfTrain,
           method = "class", 
           parms = list(split = "gini"),
           control = rpart.control(cp = cp))
  return(x)
}
dtInfo <- function(dfTrain,cp){
  x<-rpart(formula =  science_perf~.,
           data = dfTrain,
           method = "class", 
           parms = list(split = "information"),
           control = rpart.control(cp = cp))
  return(x)
}
#' function predict
predDT.cm<-function(modelDT,dfTest){
  x<-predict(modelDT,dfTest) %>% as.data.frame()
  y<-mutate(x,science_perf = as.factor(ifelse(High >= 0.5, "High", "Low"))) %>% select(science_perf)
  z<-confusionMatrix(y$science_perf,dfTest$science_perf,mode = "everything")  
  return(z)
}

modelGini<-function(dfTrain,dfTest){
  model.NP<- dtGini(dfTrain,0)
  cp.prune <- model.NP$cptable[which.min(model.NP$cptable[,"xerror"]),"CP"]
  cp.prune
  #new prune model
  model.P<- dtGini(dfTrain,cp.prune)
  cm<-predDT.cm(model.P,dfTest)
  return(cm)
}
modelInfo<-function(dfTrain,dfTest){
  model.NP<- dtInfo(dfTrain,0)
  cp.prune <- model.NP$cptable[which.min(model.NP$cptable[,"xerror"]),"CP"]
  cp.prune
  #new prune model
  model.P<- dtInfo(dfTrain,cp.prune)
  cm<-predDT.cm(model.P,dfTest)
  return(cm)
}

##' #--------------------------------------------#
##' #--------------------------------------------#
#'  [Model1a] mdl1a - gini (train70-30 & xval 10)
##' #--------------------------------------------#
##' #--------------------------------------------#
set.seed(1234)
model1a<-modelGini(train.latVarImp.73,test.latVarImp.73)
##' #--------------------------------------------#
##' #--------------------------------------------#
#' [Model1b] mdl1b - information (train70-30 & xval 10)
##' #--------------------------------------------#
##' #--------------------------------------------#
set.seed(1234)
model1b<-modelInfo(train.latVarImp.73,test.latVarImp.73)
##' #--------------------------------------------#
##' #--------------------------------------------#
##' [Model2a] mdl2a - gini (train80-20 & xval 10)
##' #--------------------------------------------#
##' #--------------------------------------------#
set.seed(1234)
model2a<-modelGini(train.latVarImp.82,test.latVarImp.82)
##' #--------------------------------------------#
##' #--------------------------------------------#
##' [Model2b] mdl2b - information (train80-20 & xval 10)
##' #--------------------------------------------#
##' #--------------------------------------------#
set.seed(1234)
model2b<-modelInfo(train.latVarImp.82,test.latVarImp.82)

model1a$overall[c("Accuracy")]
model1a$byClass[c("Sensitivity","Specificity","Precision","F1")]

model1b$overall[c("Accuracy")]
model1b$byClass[c("Sensitivity","Specificity","Precision","F1")]

model2a$overall[c("Accuracy")]
model2a$byClass[c("Sensitivity","Specificity","Precision","F1")]

model2b$overall[c("Accuracy")]
model2b$byClass[c("Sensitivity","Specificity","Precision","F1")]
