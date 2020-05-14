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
predDT<-function(modelDT,dfTest){
  x<-predict(modelDT,dfTest) %>% as.data.frame()
  return(x)
}
#' function mutate
mutateDT <- function(predDT){
  x<-mutate(predDT,science_perf = as.factor(ifelse(High >= 0.5, "High", "Low"))) %>% select(science_perf)
  return(x)
}
#' funtion CM
cmDT <- function(mutateDT,dfTest){
  x<-confusionMatrix(mutateDT$science_perf,dfTest$science_perf,mode = "everything")  
  return(x)
}

##' #-------------------------------------------------------------------------------------# 
##' #-------------------------------------------------------------------------------------# 
#'  [Model1a] mdl1a - gini (train70-30 & xval 10)
##' #-------------------------------------------------------------------------------------# 
##' #-------------------------------------------------------------------------------------# 
set.seed(1234)
mdl1a.np<- dtGini(train.latVarImp.73,0)
#pred.mdl1a.np <- predDT(mdl1a.np,test.latVarImp.73)
#mutate.mdl1a.np <- mutateDT(pred.mdl1a.np)
#' CM not prune
#cmDT(mutate.mdl1a.np,test.latVarImp.73)

cp.prune <- mdl1a.np$cptable[which.min(mdl1a.np$cptable[,"xerror"]),"CP"]
cp.prune
#new prune
mdl1a.p<- dtGini(train.latVarImp.73,cp.prune)
pred.mdl1a.p <- predDT(mdl1a.p,test.latVarImp.73)
mutate.mdl1a.p <- mutateDT(pred.mdl1a.p)
#' CM final
cmDT(mutate.mdl1a.p,test.latVarImp.73)

##' #-------------------------------------------------------------------------------------# 
##' #-------------------------------------------------------------------------------------# 
#' [Model1b] mdl1b - information (train70-30 & xval 10)
##' #-------------------------------------------------------------------------------------# 
##' #-------------------------------------------------------------------------------------# 
set.seed(1234)
mdl1b.np<- dtInfo(train.latVarImp.73,0)
#pred.mdl1b.np <- predDT(mdl1b.np,test.latVarImp.73)
#mutate.mdl1b.np <- mutateDT(pred.mdl1b.np)
#' CM not prune
#cmDT(mutate.mdl1b.np,test.latVarImp.73)

cp.prune <- mdl1b.np$cptable[which.min(mdl1b.np$cptable[,"xerror"]),"CP"]
cp.prune
#new prune
mdl1b.p<- dtInfo(train.latVarImp.73,cp.prune)
pred.mdl1b.p <- predDT(mdl1b.p,test.latVarImp.73)
mutate.mdl1b.p <- mutateDT(pred.mdl1b.p)
#' CM final
cmDT(mutate.mdl1b.p,test.latVarImp.73)

##' #-------------------------------------------------------------------------------------# 
##' #-------------------------------------------------------------------------------------# 
##' [Model2a] mdl2a - gini (train80-20 & xval 10)
##' #-------------------------------------------------------------------------------------# 
##' #-------------------------------------------------------------------------------------# 


##' #-------------------------------------------------------------------------------------# 
##' #-------------------------------------------------------------------------------------# 
##' [Model2b] mdl2b - information (train80-20 & xval 10)
##' #-------------------------------------------------------------------------------------# 
##' #-------------------------------------------------------------------------------------# 
