# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("jsCode",function(message) {eval(message.value);});')),tags$link(rel = "stylesheet", type = "text/css", href = "1.css")),
  # Application title
 titlePanel(div(HTML("<em style='color:blue'>Cyberthreat Detection</em> "))),
tags$br(),
tags$br(),
tags$br(),
  fluidRow(
    # Sidebar with a slider input for number of bins
    
    sidebarLayout(
      sidebarPanel(
        helpText("Choose the packets to be scanned"),
        
        
        fileInput(inputId="file", label = h3("File input")),
        
        #helpText("Choose random packets from selected file input"),
        
        helpText("Click below to choose a random packet from selected file input"),
        
        actionButton(inputId="random", "Pick packet")
        
      ),
      #sidebarPanel(
      # helpText("Choose single packet")
      
      
      # fileInput(inputId="file", label = h3("File input"))
      # ),
      # Show a plot of the generated distribution
      mainPanel(
        
       tags$h1(class="radioSelect",radioButtons(inputId="cl", label = h3("Choose Classifier",style="color:green"),
                            choices = list("Naive Bayes"="Naive Bayes Classifier","SVM" = "SVM Classifier",
                                           "Decision Tree" = "Decision Tree Classifier", "Random Forest" = "Random Forest Classifier"),selected = 1)) ,
        #plotOutput("distPlot")
        actionButton(inputId = "go",
                     label = "Classify")
        
        
        ,tags$hr(),tags$h5(textOutput("text1"),style="color:#ffffcc"),tags$hr(),
        tags$h3(textOutput("text2"),style="color:green",class='right'),tags$br(),
        tags$h4(textOutput("text3"),style="color:green",class='right'),
       plotOutput('plot1')
        #textOutput("acc")
      )
    )
  )
 
   
   
   
 )
)
