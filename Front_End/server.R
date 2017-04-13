

# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(compare)
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



shinyServer(function(input, output, session) {
  
  test<-reactive({
    
    
    inFile <- input$file
    
    if (is.null(inFile))
      return(NULL)
    
    return(read_csv(inFile$datapath,col_names = FALSE))
  })
  
  click<-reactive({
    clic<<-as.numeric(input$random)
    print(clic)
    return(clic)
    
  }
  )
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
                         
                         {
                           t<-test()
                           cli<-click()
                           print("haha")
                           print(cli)
                           if(cli ==1 )
                           {
                             rndm <- sample(1:nrow(t), 1)
                             print(rndm)
                             #rndm <- sample(1:nrow(t), 1)
                             t<-t[rndm,]
                             print(t)
                             #assign("t", rndm, .GlobalEnv)
                             
                           }
                           else
                           {
                             t<-test()
                             
                           }
                           print(t)
                           if(input$cl=="Random Forest Classifier"){
                             
                             my_modelRF <- readRDS("RFmodel.rds")
                             start.time <- Sys.time()
                             
                             rfpred=predict(my_modelRF,t)
                            
                             end.time <- Sys.time()
                             
                             output$plot1 <- renderPlot({
                               
                               barplot(table(t$X42), names.arg = c("DoS", "Probing","R2L","U2R","Normal"), main = "Attack types in given set of packets", xlab = "Attack types", ylab ="No of instances" )
                               
                             })
                             time.taken <<- end.time - start.time
                             rfAcc=sum(rfpred==t$X42)/nrow(t)
                             #print(cli)
                             #print(cli!=0)
                             if(cli!=0)
                               
                             {
                               if(rfpred==t$X42)
                                 
                               {
                                 print(t$X42)
                                 print(rfpred)
                                 print(rfpred!=500)
                                 if(rfpred!=500)
                                   
                                 {
                                   #my_slider_check_test <- "The packet is Malicious!!"
                                   #js_string <- 'confirm("SOMETHING");'
                                   #js_string <- sub("SOMETHING",my_slider_check_test,js_string)
                                   #session$sendCustomMessage(type='jsCode', list(value = js_string))
                                   if(rfpred==100)
                                   {
                                     paste("The packet is Malicious !!\n Attack type - DOS!\nSeverity level 2")
                                   }
                                   else if(rfpred==200)
                                   {
                                     paste("The packet is Malicious !!\n Attack type - Probe!\nSeverity level 5")
                                   }
                                   else if(rfpred==300)
                                   {
                                     paste("The packet is Malicious !!\n Attack type - R2L!\nSeverity level 7")
                                   }
                                   else
                                   {
                                     paste("The packet is Malicious !!\n Attack type - U2R!\nSeverity level 8")
                                   }
                                 }
                                 else
                                 {
                                   paste("The packet is Normal!!!")
                                 }
                                 
                               }
                               else
                               {
                                 paste("The packet is wrongly classified!!!")
                               }
                               
                             }
                             else
                             {
                               
                               paste("Accuracy is",rfAcc*100,"%")
                               
                             }
                             
                             
                           }
                           else if(input$cl=="SVM Classifier"){
                             my_modelSVM <- readRDS("SVMmodel.rds")
                             start.time <- Sys.time()
                             svmpred=predict(my_modelSVM,t)
                             end.time <- Sys.time()
                             output$plot1 <- renderPlot({
                               
                               barplot(table(t$X42), names.arg = c("DoS", "Probing","R2L","U2R","Normal"), main = "Attack types in given set of packets", xlab = "Attack types", ylab ="No of instances" )
                               
                             })
                             time.taken <<- end.time - start.time
                             svmAcc=sum(svmpred==t$X42)/nrow(t)
                             
                             if(cli!=0)
                               
                             {
                               if(svmpred==t$X42)
                                 
                               {
                                 print(t$X42)
                                 print(svmpred)
                                 print(svmpred!=500)
                                 if(svmpred!=500)
                                   
                                 {
                                   
                                   if(svmpred==100)
                                   {
                                      paste("The packet is Malicious !!\n Attack type - DOS!\nSeverity level 2")
                                   }
                                   else if(svmpred==200)
                                   {
                                      paste("The packet is Malicious !!\n Attack type - R2L!\nSeverity level 7")
                                   }
                                   else if(svmpred==300)
                                   {
                                     paste("The packet is Malicious !!\n Attack type - R2L!\nSeverity level 7")
                                   }
                                   else
                                   {
                                     paste("The packet is Malicious !!\n Attack type - U2R!\nSeverity level 8")
                                   }
                                  # my_slider_check_test <- "The packet is Malicious!!"
                                  # js_string <- 'alert("SOMETHING");'
                                   #session$sendCustomMessage(type='jsCode', list(value = js_string))
                                 }
                                 else
                                 {
                                   paste("The packet is Normal!!!")
                                 }
                                 
                               }
                               else
                               {
                                 paste("The packet is wrongly classified!!!")
                               }
                               
                             }
                             else
                             {
                               paste("Accuracy is",svmAcc*100,"%")
                               
                             }
                             
                             
                           }
                           else if(input$cl=="Decision Tree Classifier"){
                             my_modeltree <- readRDS("tree.rds")
                             start.time <- Sys.time()
                             treepred=predict(my_modeltree,newdata=t,type = c("class"))
                             end.time <- Sys.time()
                             output$plot1 <- renderPlot({
                               
                               barplot(table(t$X42), names.arg = c("DoS", "Probing","R2L","U2R","Normal"), main = "Attack types in given set of packets", xlab = "Attack types", ylab ="No of instances" )
                               
                             })
                             time.taken <<- end.time - start.time
                             treeTest=sum(treepred==t$X42)/nrow(t)
                             if(cli!=0)
                               
                             {
                               if(treepred==t$X42 )
                                 
                               {
                                 print(t$X42)
                                 print(treepred)
                                 
                                 print(treepred!=500)
                                 if(treepred!=500)
                                   
                                 {
                                   if(treepred==100)
                                   {
                                      paste("The packet is Malicious !!\n Attack type - DOS!\nSeverity level 2")
                                   }
                                   else if(treepred==200)
                                   {
                                      paste("The packet is Malicious !!\n Attack type - R2L!\nSeverity level 7")
                                   }
                                   else if(treepred==300)
                                   {
                                     paste("The packet is Malicious !!\n Attack type - R2L!\nSeverity level 7")
                                   }
                                   else
                                   {
                                     paste("The packet is Malicious !!\n Attack type - U2R!\nSeverity level 8")
                                   }
                                 #  my_slider_check_test <- "The packet is Malicious!!"
                                  # js_string <- 'alert("SOMETHING");'
                                  # js_string <- sub("SOMETHING",my_slider_check_test,js_string)
                                   #session$sendCustomMessage(type='jsCode', list(value = js_string))
                                 }
                                 else
                                 {
                                   paste("The packet is Normal!!!")
                                 }
                                 
                               }
                               else
                               {
                                 paste("The packet is wrongly classified!!!")
                               }
                               
                             }
                             else
                             {
                               paste("Accuracy is",treeTest*100,"%")
                             }
                             
                             
                           }
                           else if(input$cl=="Naive Bayes Classifier")
                           {
                             my_modelnaive <- readRDS("naive.rds")
                             start.time <- Sys.time()
                             naivepred=predict(my_modelnaive,t,threshold = 0.1,type="class")
                             end.time <- Sys.time()
                             output$plot1 <- renderPlot({
                               
                               barplot(table(t$X42), names.arg = c("DoS", "Probing","R2L","U2R","Normal"), main = "Attack types in given set of packets", xlab = "Attack types", ylab ="No of instances" )
                               
                             })
                             time.taken <<- end.time - start.time
                             #paste("Time taken : ",time.taken)
                             naiveAcc=sum(naivepred==t$X42)/nrow(t)
                             if(cli!=0)
                               
                             {
                               if(naivepred==t$X42 )
                                 
                               {
                                 print(t$X42)
                                 print(naivepred)
                                 print(naivepred!=500)
                                 if(naivepred!=500)
                                   
                                 {
                                   if(naivepred==100)
                                   {
                                      paste("The packet is Malicious !!\n Attack type - DOS!\nSeverity level 2")
                                   }
                                   else if(naivepred==200)
                                   {
                                      paste("The packet is Malicious !!\n Attack type - R2L!\nSeverity level 7")
                                   }
                                   else if(naivepred==300)
                                   {
                                     paste("The packet is Malicious !!\n Attack type - R2L!\nSeverity level 7")
                                   }
                                   else
                                   {
                                     paste("The packet is Malicious !!\n Attack type - U2R!\nSeverity level 8")
                                   }
                                  # my_slider_check_test <- "The packet is Malicious!!"
                                   #js_string <- 'alert("SOMETHING");'
                                   #js_string <- sub("SOMETHING",my_slider_check_test,js_string)
                                   #session$sendCustomMessage(type='jsCode', list(value = js_string))
                                 }
                                 else
                                 {
                                   paste("The packet is Normal!!!")
                                 }
                                 
                               }
                               else
                               {
                                 paste("The packet is wrongly classified!!!")
                               }
                               
                             }
                             else
                             {
                               paste("Accuracy is",naiveAcc*100,"%")
                             }
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

