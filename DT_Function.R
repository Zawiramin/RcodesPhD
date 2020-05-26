#' my general function 
#' goal ialah nk print Confusion Matrix
#' I would like to have two CM for each models
#' cm.np and cm.p(final model)

#' decision tree function using rpart
#' takes the value df,gini/information,cp value
#' use training set
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
predDT.cm<-function(modelDT,dfTest){
  x<-predict(modelDT,dfTest) %>% as.data.frame()
  y<-mutate(x,science_perf = as.factor(ifelse(High >= 0.5, "High", "Low"))) %>% select(science_perf)
  confusionMatrix(y$science_perf,dfTest$science_perf,mode = "everything")  
}

modelGini<-function(dfTrain,dfTest){
  model.NP<- dtGini(dfTrain,0)
  cp.prune <- model.NP$cptable[which.min(model.NP$cptable[,"xerror"]),"CP"]
  #new prune model
  model.P<- dtGini(dfTrain,cp.prune)
  predDT.cm(model.P,dfTest)
}
modelInfo<-function(dfTrain,dfTest){
  model.NP<- dtInfo(dfTrain,0)
  cp.prune <- model.NP$cptable[which.min(model.NP$cptable[,"xerror"]),"CP"]
  #new prune model
  model.P<- dtInfo(dfTrain,cp.prune)
  predDT.cm(model.P,dfTest)
}

##' #--------------------------------------------#
##' #--------------------------------------------#
#'  [Model1a] mdl1a - gini (train70-30 & xval 10)
##' #--------------------------------------------#
##' #--------------------------------------------#
set.seed(1234)
Rprof(tf <- "Rprof.out")
model1a<-modelGini(train.latVarImp.73,test.latVarImp.73)
model1a
Rprof (NULL) 
print(summaryRprof(tf))

##' #--------------------------------------------#
##' #--------------------------------------------#
#' [Model1b] mdl1b - information (train70-30 & xval 10)
##' #--------------------------------------------#
##' #--------------------------------------------#
set.seed(1234)
Rprof(tf <- "Rprof.out")
model1b<-modelInfo(train.latVarImp.73,test.latVarImp.73)
model1b
Rprof (NULL)
print(summaryRprof(tf))

##' #--------------------------------------------#
##' #--------------------------------------------#
##' [Model2a] mdl2a - gini (train80-20 & xval 10)
##' #--------------------------------------------#
##' #--------------------------------------------#
set.seed(1234)
Rprof(tf <- "Rprof.out")
model2a<-modelGini(train.latVarImp.82,test.latVarImp.82)
model2a
Rprof (NULL) 
print(summaryRprof(tf))
##' #--------------------------------------------#
##' #--------------------------------------------#
##' [Model2b] mdl2b - information (train80-20 & xval 10)
##' #--------------------------------------------#
##' #--------------------------------------------#
set.seed(1234)
Rprof(tf <- "Rprof.out")
model2b<-modelInfo(train.latVarImp.82,test.latVarImp.82)
model2b
Rprof (NULL)  
print(summaryRprof(tf))
##' #--------------------------------------------#
##' #--------------------------------------------#

#'----------------------------#
#' 15 Latent Variables 
#' The variables used in the analysis
#' with Reliability > 0.8
#'----------------------------#
##' #--------------------------------------------#
##' #--------------------------------------------#
#'  [Model1a] mdl1a - gini (train70-30 & xval 10)
##' #--------------------------------------------#
##' #--------------------------------------------#
set.seed(1234)
Rprof(tf <- "Rprof.out")
New.model1a<-modelGini(train.New.73,test.New.73)
New.model1a
Rprof (NULL) 
print(summaryRprof(tf))

##' #--------------------------------------------#
##' #--------------------------------------------#
#' [Model1b] mdl1b - information (train70-30 & xval 10)
##' #--------------------------------------------#
##' #--------------------------------------------#
set.seed(1234)
Rprof(tf <- "Rprof.out")
New.model1b<-modelInfo(train.New.73,test.New.73)
New.model1b
Rprof (NULL)
print(summaryRprof(tf))

##' #--------------------------------------------#
##' #--------------------------------------------#
##' [Model2a] mdl2a - gini (train80-20 & xval 10)
##' #--------------------------------------------#
##' #--------------------------------------------#
set.seed(1234)
Rprof(tf <- "Rprof.out")
New.model2a<-modelGini(train.New.82,test.New.82)
New.model2a
Rprof (NULL) 
print(summaryRprof(tf))
##' #--------------------------------------------#
##' #--------------------------------------------#
##' [Model2b] mdl2b - information (train80-20 & xval 10)
##' #--------------------------------------------#
##' #--------------------------------------------#
set.seed(1234)
Rprof(tf <- "Rprof.out")
New.model2b<-modelInfo(train.New.82,test.New.82)
New.model2b
Rprof (NULL)  
print(summaryRprof(tf))
##' #--------------------------------------------#
##' #--------------------------------------------#



model1a$overall[c("Accuracy")]
model1a$byClass[c("Sensitivity","Specificity","Precision","F1")]

model1b$overall[c("Accuracy")]
model1b$byClass[c("Sensitivity","Specificity","Precision","F1")]

model2a$overall[c("Accuracy")]
model2a$byClass[c("Sensitivity","Specificity","Precision","F1")]

model2b$overall[c("Accuracy")]
model2b$byClass[c("Sensitivity","Specificity","Precision","F1")]
