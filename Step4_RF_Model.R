#' #-------------------------------------------------------------------------------------# 
#' THE PLAN:
#' Building [Model 4] All TSI summaries combined
#' Random Forest 
#' tsi_rf1 - random forest with 70-30
#' tsi_rf2 - random forest with 70-30 and crossVal

##' #-------------------------------------------------------------------------------------# 
#' [Model 4] All TSI summaries combined 70-30
#' 
tsi_rf1 <- randomForest(formula = science_perf ~ .,
                          data = TSIndcesNew,
                          method = "class", 
                          importance=TRUE, ntree = 1000)
print(tsi_rf1)
#' #' In the output, we see the confusion matrix along with classification error and out-of-bag (OOB) error. 
#' OBB is a method of measuring the prediction error of random forests, finding the mean prediction error 
#' on each training sample, using only the trees that did not have in their bootstrap sample. The results 
#' show that the overall OBB error is around  8%, while the classification error is  10%
#' for the high category and around  6% for the low category.
#' 
#' Next, by checking the level error across the number of trees, we can determine the ideal number of trees 
#' for our model.plot to see the error if it's go down as number of trees added
plot(tsi_rf1)
#' 
#' 
#' 
tsi_rf2 <- randomForest(formula = science_perf ~ .,
                          data = train.TSI,
                          method = "class", 
                          importance=TRUE, ntree = 300)
print(tsi_rf2)
plot(tsi_rf2)
#' The plot shows that the error level does not go down any further after roughly 50 trees. So, we can 
#' run our model again by using ntree = 50 this time.
#' 
#' see the overall accuracy of mode
sum(diag(tsi_rf2$confusion)) / nrow(train.TSI)
#' 
#' As we did for the decision trees, we can check the importance of the predictors in the model, 
#' using importance() and varImpPlot(). 
#' With importance(), we will first import the importance measures, 
#' turn it into a data.frame, save the row names as predictor names, and finally sort the data by 
#' MeanDecreaseGini (or, you can also see the basic output using only importance(rf_fit2))
#' AKA FEATURE SELECTION USING mean decrease in accuracy and mean decrease gini
#' 
importance(tsi_rf2) %>%
  as.data.frame() %>%
  mutate(Predictors = row.names(.)) %>%
  arrange(desc(MeanDecreaseGini))

importance(tsi_rf2) %>%
  as.data.frame() %>%
  mutate(Predictors = row.names(.)) %>%
  arrange(desc(MeanDecreaseAccuracy))

#' 
varImpPlot(tsi_rf2, 
           main = "Importance of Variables for Science Performance")
#' The output shows different importance measures for the predictors that we used in the model. 
#' MeanDecreaseAccuracy and MeanDecreaseGini represent the overall classification error rate 
#' (or, mean squared error for regression) and the total decrease in node impurities from splitting on the variable, 
#' averaged over all trees. 
#' In the output, math and reading are the two predictors that seem to influence the model performance substantially, 
#' whereas EPIST and HEDRES are the least important variables. varImpPlot() presents the same information visually.
#' 
#' Next, we check the confusion matrix to see the accuracy, sensitivity, and specificity of our model.
tsi1.rfpred <- predict(tsi_rf2, test.TSI) %>%
  as.data.frame() %>%
  mutate(science_perf = as.factor(`.`)) %>%
  select(science_perf)

confusionMatrix(tsi1.rfpred$science_perf, test.TSI$science_perf, mode = "everything")

#' The results show that the accuracy is quite high (92%). Similarly, sensitivity and specificity are also very high. 
#' This is not necessarily surprising because we already knew that the math and reading scores are highly correlated 
#' with the science performance. Also, our decision tree model yielded very similar results.


#' #-------------------------------------------------------------------------------------# 
#' performing cross validation in random forest
#' using package [caret] & [randomForest]
#' #-------------------------------------------------------------------------------------# 
#' 

tsi.control <- trainControl(method = "cv",number = 10, search = "grid")
tsi.tuneGrid <- expand.grid(.mtry = c(1:10))
set.seed(1234)

Rprof(tf <- "log.log", memory.profiling = TRUE)
#' train the rf model using Caret with the accuracy measure
#' using Grid Search may take awhile 
#' [70-30 DATA SPLIT]
#dataTrain <- train.TSI.73
#' [80-20 DATA SPLIT]
dataTrain <- train.TSI.82


#' method: [rf]
#' using package randomForest on top of caret package
#' 
#Rprof(tf <- "log.log", memory.profiling = TRUE)
#' train the rf model using Caret with the accuracy measure
#' using Grid Search may take awhile since it
tsi.rf.train <- train(science_perfM ~.,
                       data = dataTrain,
                       method = "rf",
                       metric = "Accuracy",
                       trControl= tsi.control,
                       tuneGrid = tsi.tuneGrid,
                       importance = TRUE)

#' the code you want to profile must be in between
#Rprof (NULL) ; print(summaryRprof(tf))

#' print the results
print(tsi.rf.train)

#' The best value of mtry is stored in:
tsi.rf.train$bestTune$mtry
#' You can store it and use it when you need to tune the other parameters.
max(tsi.rf.train$results$Accuracy)
best.mtry <- tsi.rf.train$bestTune$mtry

#' #-------------------------------------------------------------------------------------# 
#' THE PLAN:
#' Search the best max-nodes
#' You need to create a loop to evaluate the different values of maxnodes. In the following code, you will:
#' - Create a list
#' - Create a variable with the best value of the parameter mtry; Compulsory
#' - Create the loop
#' - Store the current value of maxnode
#' - Summarize the results
#' https://www.guru99.com/r-random-forest-tutorial.html#3
##' #-------------------------------------------------------------------------------------# 

#Rprof(tf <- "log.log", memory.profiling = TRUE)
store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = best.mtry)
for (maxnodes in c(20:30)) {
  set.seed(1234)
  rf_maxnode <- train(science_perfM~.,
                      data = dataTrain,
                      method = "rf",
                      metric = "Accuracy",
                      tuneGrid = tuneGrid,
                      trControl = tsi.control,
                      importance = TRUE,
                      nodesize = 14,
                      maxnodes = maxnodes,
                      ntree = 300)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)
summary(results_mtry)

#Rprof (NULL) ; print(summaryRprof(tf))

#' #-------------------------------------------------------------------------------------# 
#' search the best ntress
#' #-------------------------------------------------------------------------------------# 
store_maxtrees <- list()
for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
  set.seed(5678)
  rf_maxtrees <- train(science_perfM~.,
                       data = dataTrain,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tuneGrid,
                       trControl = tsi.control,
                       importance = TRUE,
                       nodesize = 14,
                       maxnodes = 28,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)


#' #-------------------------------------------------------------------------------------# 
#' You have your final model. You can train the random forest with the following parameters:
#' - ntree = 350: 350 trees will be trained
#' - mtry=2: 2 features is chosen for each iteration
#' - maxnodes = 20: Maximum 20 nodes in the terminal nodes (leaves)
tsi.control <- trainControl(method = "cv",number = 10, search = "grid",summaryFunction = twoClassSummary,classProbs = TRUE)
tsi.Final.rf <- train(science_perfM~.,
                   data = dataTrain,
                   method = "rf",
                   metric = "ROC",
                   tuneGrid = tuneGrid,
                   trControl = tsi.control,
                   importance = TRUE,
                   nodesize = 14,
                   maxnodes = 29,
                   ntree = 250)
#' #-------------------------------------------------------------------------------------# 
#' Evaluate the model
#dataTest <- test.TSI.73
dataTest <- test.TSI.82

tsi.Final.pred82 <-predict(tsi.Final.rf, dataTest) %>%
  as.data.frame() %>%
  mutate(science_perfM = as.factor(`.`)) %>%
  select(science_perfM)


#tsi.Final.pred73
#cm.rf73 <- confusionMatrix(tsi.Final.pred73$science_perfM,dataTest$science_perfM,mode = "everything")
cm.rf73$overall["Accuracy"]
cm.rf73$byClass[c("Sensitivity","Specificity","Precision","F1")]

#' [80-20 DATA SPLIT]
#tsi.Final.pred82
cm.rf82 <- confusionMatrix(tsi.Final.pred82$science_perfM,dataTest$science_perfM,mode = "everything")
cm.rf82$overall["Accuracy"]
cm.rf82$byClass[c("Sensitivity","Specificity","Precision","F1")]

#' [80-20 DATA SPLIT]
varImp(tsi.Final.rf)
#' [70-30 DATA SPLIT]
varImp(tsi.cv.rf)
