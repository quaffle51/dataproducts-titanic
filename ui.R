# load required packages
require(shiny)
require(ggplot2)
require(caret)
require(pROC)      

# read the data
load('titanic-train.Rdata')
load('titanic-test.Rdata')
load('titanic-varimp.RData')

# join data
dataset <- rbind(train, test)

# shinyUI(pageWithSidebar(
shinyUI(pageWithSidebar(
  
  # show main title
  headerPanel("Titanic: machine learning from disaster ... a Shiny App"),
  
  # define content of sidebar panel
  sidebarPanel(
  
    # let's add some text
    # I need to remember to end each code line with a comma, except the last line
    
    # add introduction as header level 2
    h2("Introduction"),
  
    # describe the Titanic data 
    h3("The Titanic"),
    
    tags$p(
      "The Titanic was a British passenger liner that sank in the North Atlantic Ocean in the early morning of 15 April 1912 
      after colliding with an iceberg during her maiden voyage from Southampton, UK to New York City, US. 
      The sinking of Titanic caused the deaths of more than 1,500 people in one of the deadliest peacetime maritime disasters 
      in modern history. Source:",
      tags$a(href="http://en.wikipedia.org/wiki/RMS_Titanic", "Wikipedia"),
      # add an image of the titanic taken from wikipedia.com
      # the image must be in a sub-directory called "www" which must be in the same directory as ui.R
      img(src="600px-RMS_Titanic_3.jpg")
    ), 
    
    # add a paragraph to talk about the Kaggle competion
    tags$p(
      "Titanic is also the theme for a machine learning competition on",
      tags$a(href="https://www.kaggle.com", "kaggle."),
      "The Titanic competition is a so called knowledge competition designed to learn as well 
      as test your knowledge by competing against the community."
    ),
    
    tags$p(
      "I have used the Titanic competion as theme for my",
      tags$a(href="http://shiny.rstudio.com", "Shiny App"),
      "that I created for the",
      tags$a(href="https://www.coursera.org", "Coursera"),
      "course on Developing Data Products by the",
      tags$a(href="https://www.coursera.org/jhu", "John Hopkins University.")
    ),
    
    h3("About me"),
    tags$p(
      "My name is Thiemo. I am an engineer working in the Energy industry. 
      One of my personal interests is Machine Learning. Ever since I discovered",
      tags$a(href="https://www.coursera.org", "Coursera"),
      "I have been a great fan of it. Just wish I had more time to devote to it!"
    ),
    
    # now add the dataset selector
    h3("Select the dataset"),
    selectInput("dataset", "Dataset:",
                choices = c("train", "test", "combined"),
                selected = "train"),
    
    tags$p(
      "For more information on the data and the meaning of the variables have a look at the",
      tags$a(href="https://www.kaggle.com/c/titanic-gettingStarted", "Titanic competion on Kaggle")
    ),
    
    tags$p(
      "More details about the modeling can be found on my",
      tags$a(href="", "Titanic github"),
      "page."  
    )
    
  , width = 4), # this end the sidebar panel

  # define content of the main panel
  mainPanel(
    
    # create tabs in main panel
    tabsetPanel(
      # first tab will show a data table 
      tabPanel("Data",
               # first create a fluid row for input selectors
               # the number after column controls the width of the selector
               fluidRow(
                 column(3, 
                        selectInput("sex", 
                                    "Sex:", 
                                    c("All", 
                                      unique(as.character(dataset$Sex))))
                 ),
                 column(3, 
                        selectInput("title", 
                                    "Honorific:", 
                                    c("All", 
                                      unique(as.character(dataset$Title))))
                 ),
                 column(3, 
                        selectInput("embarked", 
                                    "Embarked:", 
                                    c("All", 
                                      unique(as.character(dataset$Embarked))))
                 ),
                 column(3, 
                        selectInput("fate", 
                                    "Fate:", 
                                    c("All", 
                                      unique(as.character(dataset$Fate))))
                 ) 
               ),
               # now create a new row for the table
               fluidRow(
                 dataTableOutput(outputId="table")
               )    
      ),
      
      tabPanel("Explore",
               
               # first create a fluid row for variable selectors
               fluidRow(
                 column(4,  
                        selectInput("x",
                                    "X variable:",
                                    unique(as.character(names(dataset))),
                                    'Age')
                 ),
                 column(4,  
                        selectInput("y",
                                    "Y variable:",
                                    unique(as.character(names(dataset))),
                                    'Sex')
                 ),
                 column(4,  
                        selectInput("color",
                                    "Color by:",
                                    unique(as.character(names(dataset))),
                                    'Fate')
                 )
               ),
              
               # add a row for plot options
               fluidRow(
                 column(4,  
                        checkboxInput('smooth', 'Add smoother', value = FALSE)
                 ),
                 column(4,  
                        checkboxInput('jitter', 'Add jitter', value = TRUE)
                 )
               ),
               
               # and finally add a row for the plot
               fluidRow(
                 plotOutput("scatter")
               )
                
      ), # ends tab plot
      tabPanel("Model",
               
               # create row for variable importance selection and go button
               fluidRow(
                 column(8,
                        sliderInput("sliderminvimp", label = "Minimum Variable Importance Score",
                                    min = 0, max = max(svImp), value = mean(svImp))
                 ),
                 column(4,
                        actionButton("goButton", "Update model")
                 )
               ),
               
               # add row for the variable importance plot
               fluidRow(
                h5("Relative Variable Importance"),
                plotOutput("variableimportance")
               ),
               
               # finally add row for the ROC plot
               fluidRow(
                 h5("Receiver Operating Characteristic of the Logistic Regression Model"),
                 # add the plot
                 plotOutput("roc"),
                 # utput area under the curve (auc)
                 textOutput("auc")
               )

      ) # ends tab model
    )
  )

))