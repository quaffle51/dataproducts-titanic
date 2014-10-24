# load required packages
require(shiny)
require(ggplot2)
require(rpart)

# read the data
load('titanictrain.Rdata')
load('titanictest.Rdata')

# join data
dataset <- rbind(train, test)

shinyUI(pageWithSidebar(

  headerPanel("Titanic: machine learning from disaster"),

  sidebarPanel(

    selectInput("dataset", "Choose a dataset:",
                choices = c("train", "test", "combined")),

    selectInput('x', 'X', names(dataset), 'Age'),

    selectInput('y', 'Y', names(dataset), 'Sex'),

    selectInput('color', 'Color',
                c('None', names(dataset)), 'Survived'),

    checkboxInput('smooth', 'Smooth')

  ),

  # show a tabset with summary and plot views
  mainPanel(
    tabsetPanel(
      tabPanel("Summary",
               verbatimTextOutput("summary_of_x"),
               verbatimTextOutput("summary_of_y"),
               verbatimTextOutput("summary_of_color")),

      tabPanel("Box plot",
               h2("Trees"),
               sliderInput("decimal", "Complexity:",
                           min=0.1, max=1.0,
                           value=0.5, step = 0.1),
               plotOutput("tree")),
      tabPanel("Scatter plot", plotOutput("scatter"))
    )
  )

))