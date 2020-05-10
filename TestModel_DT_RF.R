##' #-------------------------------------------------------------------------------------# 
#' 1 [envOpt] - gini (train70-30 & xval 10)
#' 
set.seed(1234)
envoptM1 <- rpart(formula = science_perfM ~.,
                data = train.envOpt73,
                method = "class", 
                parms = list(split = "gini"),
                control = rpart.control(cp = 0))
plot(envoptM1)
printcp(envoptM1)

cp.prune <- envoptM1$cptable[which.min(envoptM1$cptable[,"xerror"]),"CP"]
cp.prune
plotcp(envoptM1)
envoptM1new <- rpart(formula =  science_perfM ~.,
                   data = train.envOpt73,
                   method = "class", 
                   parms = list(split = "gini"),
                   control = rpart.control(cp = cp.prune))
plot(envoptM1new)
rpart.rules(envoptM1new, cover = TRUE)
summary(envoptM1new) #' this is signed of overfitted

impVar.envOpt73 <- varImp(envoptM1new)
impVar.envOpt73 <- mutate(impVar.envOpt73, Variable = rownames(impVar.envOpt73))
impVar.envOpt73 <- impVar.envOpt73[order(impVar.envOpt73$Overall, decreasing = TRUE),]
impVar.envOpt73

envOpt73.pred <- predict(envoptM1new, test.envOpt73) %>%
  as.data.frame()
head(envOpt73.pred)
envOpt73.pred <- mutate(envOpt73.pred,
                     science_perfM = as.factor(ifelse(High >= 0.5, "High", "Low"))) %>% 
  select(science_perfM)

#' Confusion Matrix tsi3 - Entropy + train.test
cm.envOpt73 <- confusionMatrix(envOpt73.pred$science_perfM, test.envOpt73$science_perfM,mode = "everything")
cm.envOpt73


##' #-------------------------------------------------------------------------------------# 
#' 2 [envOpt] - information (train70-30 & xval 10)
#' 
set.seed(1234)
envoptM2 <- rpart(formula = science_perfM ~.,
                  data = train.envOpt73,
                  method = "class", 
                  parms = list(split = "information"),
                  control = rpart.control(cp = 0))
plot(envoptM2)
printcp(envoptM2)

cp.prune <- envoptM2$cptable[which.min(envoptM2$cptable[,"xerror"]),"CP"]
cp.prune
plotcp(envoptM2)
envoptM2new <- rpart(formula =  science_perfM ~.,
                     data = train.envOpt73,
                     method = "class", 
                     parms = list(split = "information"),
                     control = rpart.control(cp = cp.prune))
plot(envoptM2new)
rpart.rules(envoptM2new, cover = TRUE)
summary(envoptM2new) #' this is signed of overfitted

impVar.envOpt73n <- varImp(envoptM2new)
impVar.envOpt73n <- mutate(impVar.envOpt73n, Variable = rownames(impVar.envOpt73n))
impVar.envOpt73n <- impVar.envOpt73n[order(impVar.envOpt73n$Overall, decreasing = TRUE),]
impVar.envOpt73n

envOpt73.pred <- predict(envoptM2new, test.envOpt73) %>%
  as.data.frame()
head(envOpt73.pred)
envOpt73.pred <- mutate(envOpt73.pred,
                        science_perf = as.factor(ifelse(High >= 0.5, "High", "Low"))) %>% 
  select(science_perf)

#' Confusion Matrix tsi3 - Entropy + train.test
cm.envOpt73n <- confusionMatrix(envOpt73.pred$science_perf, test.envOpt73$science_perf,mode = "everything")
cm.envOpt73n

##' #-------------------------------------------------------------------------------------# 
##' #-------------------------------------------------------------------------------------# 
##' guna GLM

#'  USING MALAYSIA AVERAGE AS CUT OFF
pisa15Mas[, science_perfM11 := 
            as.factor(ifelse(science >= (mean(science)), 1, 0))]

pisa15Mas[,table(science_perfM11)]

set.seed(123456)
indexTSI.7311 <- createDataPartition(TSIndcesNew$science_perfM11, p = 0.7, list = FALSE)
train.TSI.7311 <- TSIndcesNew[indexTSI.7311,]
test.TSI.7311 <- TSIndcesNew[-indexTSI.7311,]

indexTSI.8211 <- createDataPartition(TSIndcesNew$science_perfM11, p = 0.8, list = FALSE)
train.TSI.8211 <- TSIndcesNew[indexTSI.8211,]
test.TSI.8211 <- TSIndcesNew[-indexTSI.8211,]


set.seed(1234)
tsi.glm <- glm(formula = science_perfM ~.,
                data = train.TSI.82,
               family = "binomial")
tsi.glm
coef(tsi.glm)
pred <- predict(tsi.glm,type = "response")
cm.train <- table(train.TSI.82$science_perfM,pred > 0.5)
cm.train

#' sensitivity, recall, hit rate, or true positive rate (TPR)
#' specificity, selectivity or true negative rate (TNR)
#' precision or positive predictive value (PPV)
  tp <- matrix[1,2]
  tn <- matrix[2,1]
  fp <- matrix[2,2]
  fn <- matrix[1,1]
  

accuracy_Test <- function(matrix) {
  tp <- matrix[1,2]
  tn <- matrix[2,1]
  fp <- matrix[2,2]
  fn <- matrix[1,1]
  return((tp+tn)/sum(tp,tn,fp,fn))
}
accuracy_Test(cm.train)

#' sensitivity
recall <- function(matrix) {
  # true positive
  tp <- matrix[2, 2]# false positive
  fn <- matrix[2, 1]
  return (tp / (tp + fn))
}

#' specificity
specificity <- function(matrix) {
  # true negative
  tn <- matrix[]
  fp <- matrix[] 
}

precision <- function(matrix) {
  # True positive
  tp <- matrix[2, 2]
  # false positive
  fp <- matrix[1, 2]
  return (tp / (tp + fp))
}


prec <- precision(cm.train)
prec
sens <- recall(cm.train)
sens


