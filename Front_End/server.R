
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(readr)
library(class)
library("nnet", lib.loc="/usr/lib/R/library")
library(dplyr)
library("e1071")
library("e1071", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")
library("FSelector", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")
library("FNN", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")
library("rpart", lib.loc="/usr/lib/R/library")
#library(lubridate)
library("randomForest", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")

KDDTrain_20Percent <- read_csv("~/Desktop/FYP/KDDTrain+_20Percent.csv",col_names = FALSE)
test <- read_csv("~/Desktop/FYP/test.csv",col_names = FALSE)
#Preprocessing.

unique(KDDTrain_20Percent$X42)
uni<-unique(KDDTrain_20Percent$X42)
count<-table(KDDTrain_20Percent$X42)

attacktype = as.data.frame(count)

numer =  function(x) {
  x <- as.factor(x)
  levels(x) <- 1:length(levels(x))
  x <- as.numeric(x)
  return(x)
}

KDDTrain_20Percent$X2 = numer(KDDTrain_20Percent$X2)
KDDTrain_20Percent$X3 = numer(KDDTrain_20Percent$X3)
KDDTrain_20Percent$X4 = numer(KDDTrain_20Percent$X4)
KDDTrain_20Percent$X42 = numer(KDDTrain_20Percent$X42)

for(i in 1:nrow(KDDTrain_20Percent))
  if((KDDTrain_20Percent$X42[i]== 1 ||KDDTrain_20Percent$X42[i]== 7||KDDTrain_20Percent$X42[i]== 10|| KDDTrain_20Percent$X42[i]== 14||KDDTrain_20Percent$X42[i]== 18||KDDTrain_20Percent$X42[i]== 20))
    KDDTrain_20Percent$X42[i]=100


for(i in 1:nrow(KDDTrain_20Percent))
  if((KDDTrain_20Percent$X42[i]== 17 ||KDDTrain_20Percent$X42[i]== 6||KDDTrain_20Percent$X42[i]== 11|| KDDTrain_20Percent$X42[i]== 15))
    KDDTrain_20Percent$X42[i]=200

for(i in 1:nrow(KDDTrain_20Percent))
  if((KDDTrain_20Percent$X42[i]== 4 ||KDDTrain_20Percent$X42[i]== 3||KDDTrain_20Percent$X42[i]== 5|| KDDTrain_20Percent$X42[i]== 13||  KDDTrain_20Percent$X42[i]== 9 ||  KDDTrain_20Percent$X42[i]== 22 ||  KDDTrain_20Percent$X42[i]== 21 ||  KDDTrain_20Percent$X42[i]== 19))
    KDDTrain_20Percent$X42[i]=300



for(i in 1:nrow(KDDTrain_20Percent))
  if((KDDTrain_20Percent$X42[i]== 2||KDDTrain_20Percent$X42[i]== 8||KDDTrain_20Percent$X42[i]== 16))
    KDDTrain_20Percent$X42[i]=400

for(i in 1:nrow(KDDTrain_20Percent))
  if((KDDTrain_20Percent$X42[i]== 12))
    KDDTrain_20Percent$X42[i]=500

KDDTrain_20Percent=KDDTrain_20Percent[-43]




shinyServer(function(input, output) {
  
  test<-reactive({
    
    
    inFile <- input$file
    
    if (is.null(inFile))
      return(NULL)
    
    return(read_csv(inFile$datapath,col_names = FALSE))
  })

  output$text1 <- renderText({ 
  data()
  })
  
  output$text2 <- renderText({
    data1()
  })
  
  output$text3 <- renderText({
    data2()
  })

  
  data <- eventReactive(input$go, {paste("You have selected", input$cl)})
  data1 <- eventReactive(input$go, 
    
    {t<-test()
      if(input$cl=="Random Forest Classifier"){
      
      my_modelRF <- readRDS("RFmodel.rds")
      start.time <- Sys.time()
      rfpred=predict(my_modelRF,t)
      end.time <- Sys.time()
      time.taken <<- end.time - start.time
      rfAcc=sum(rfpred==t$X42)/nrow(t)
      paste("Accuracy is",rfAcc*100,"%")
    }
      else if(input$cl=="SVM Classifier"){
        my_modelSVM <- readRDS("SVMmodel.rds")
        start.time <- Sys.time()
        svmpred=predict(my_modelSVM,t)
        end.time <- Sys.time()
        time.taken <<- end.time - start.time
        svmAcc=sum(svmpred==t$X42)/nrow(t)
        paste("Accuracy is",svmAcc*100,"%")
      }
    else if(input$cl=="Decision Tree Classifier"){
      my_modeltree <- readRDS("tree.rds")
      start.time <- Sys.time()
      treePred=predict(my_modeltree,newdata=t,type = c("class"))
      end.time <- Sys.time()
      time.taken <<- end.time - start.time
      treeTest=sum(treePred==t$X42)/nrow(t)
      paste("Accuracy is",treeTest*100,"%")
      
    }
    else if(input$cl=="Naive Bayes Classifier")
    {
      my_modelnaive <- readRDS("naive.rds")
      start.time <- Sys.time()
      naivepred=predict(my_modelnaive,t,threshold = 0.1,type="class")
      end.time <- Sys.time()
      time.taken <<- end.time - start.time
      #paste("Time taken : ",time.taken)
      naiveAcc=sum(naivepred==t$X42)/nrow(t)
      paste("Accuracy is",naiveAcc*100,"%")
     # paste("Time taken : ",time.taken)
      
      #paste("hehehehuhuhuhahaha!!!!!! :D")
    }
    #else if(input$cl=="KNN Classifier")
    #{
      
    #}
      else
      {
        paste("No classifier chosen yet!!!")
      }
    
    }
      )
  
  data2<-eventReactive(input$go,{paste("Time take :",time.taken)})
  #output$acc<-({})
  
  
 # source("scripts/classifiers/knn.R")
  
  

})
