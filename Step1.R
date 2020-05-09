#-----------------------------------------------#
#'             Required 
#' #--------------------------------------------#
#install.packages("devtools")
#install.packages('tidyverse', dependencies=TRUE, repos='http://cran.rstudio.com/')
#library("survey") # the core of everything we will do today
#library("mitools") # needed for working with plausible values
#' optional but makes life easier --- 
library("tidyverse") # a suite of tools to make coding easier
library("data.table") # FASTER THAN tidyverse and RECOMMENDED
#data.table::update.dev.pkg()
#library("readit") # Figures out what sort of file you are giving it and read in the data
#library("srvyr") # tidyverse for survey data
#library("svyPVpack") # convience functions for survey data and plausible values
#' F or analysis in parallel ---
#library("furrr") # needed to easily set up and run models in parallel
#plan(multiprocess) # prepare to run things in parallel

#' Pretty graphs --- 
#library("ggthemes") # emulate the style of professional data vis
#library("wesanderson") # colour pallets that work well together

library("foreign") #penting 1 - read spss/sav
library("intsvy") #penting 2 - using select.merge
library("lavaan.survey")
library("semPlot") #plotting the SEM
library("mice")
library("rpart") #for calculate decision tree
library("rpart.plot") #plotting DT
library("Rcpp")
library("modelr")
library("ggplot2")
library("caret") #confusionMatrix
library("randomForest")
library("e1071")
#library("rfUtilities")
library(dplyr)
library("ROCR") #ROC curve 

#dataviz package
library("cowplot")
library("GGally")
library(ggExtra)
library(ggalluvial)
library("plotly")

#citation("rpart")
#citation("ggplot2")
#citation("caret")
#citation("randomForest")

#' #-------------------------------------------------#
#pisa.var.label(student.file = c("CY6_MS_CM2_STU_QQQ.sav"),name = "StudentQ Label")
#pisa.var.label(student.file = c("CY6_MS_CM2_SCH_QQQ.sav"),name = "SchoolQ Label")
#pisa.var.label(student.file = c("CY07_MSU_STU_QQQ.sav"),name = "StudentQ18 Label")
#pisa.var.label(student.file = c("CY6_MS_CM2_TCH_QQQ.sav"),name = "Tch Mas Label")
#PisaCOG2015 <- read.spss("CY6_MS_CMB_STU_COG.sav",to.data.frame = TRUE)
pisa2015 <- read.spss("CY6_MS_CM2_STU_QQQ.sav", to.data.frame = TRUE,stringsAsFactors=F, na.strings=c(NA,"NA"," NA"))
pisaSch2015 <- read.spss("CY6_MS_CM2_SCH_QQQ.sav", to.data.frame = TRUE,stringsAsFactors=F, na.strings=c(NA,"NA"," NA"))
#pisa2018 <- read.spss("CY07_MSU_STU_QQQ.sav", to.data.frame = TRUE,stringsAsFactors=F, na.strings=c(NA,"NA"," NA"))
#pisaTch2015 <- read.spss("CY6_MS_CM2_TCH_QQQ.sav", to.data.frame = TRUE,stringsAsFactors=F, na.strings=c(NA,"NA"," NA"))
#' #-------------------------------------------------#
#' use data.table to create more lighter and faster execution fast
fwrite(pisa2015, file = "pisa2015.csv")
fwrite(pisaSch2015,file = "pisaSch2015.csv")
#fwrite(pisa2018,file = "pisa2018.csv")
#fwrite(pisaTch2015,file = "pisaTch2015.csv")
#fwrite(PisaCOG2015,file = "PisaCOG2015.csv")
#pisa15Tch <- fread("pisaTch2015.csv",na.strings = "")
#pisa15Cog <- fread("pisaTch2015.csv",na.strings = "")
pisa2015 <- fread("pisa2015.csv", na.strings = "")
pisaSch2015 <- fread("pisaSch2015.csv",na.strings = "")
#pisa18 <- fread("pisa2018.csv", na.strings = "")
#str(pisa, list.len=ncol(pisa2015))

