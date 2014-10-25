# load required packages
require(shiny)
require(ggplot2)
require(caret)
require(pROC)      

# read the data
load('titanic-train.RData')
load('titanic-test.RData')
load('titanic-varimp.RData')

# join data
combi <- rbind(train, test)

# server logic required to run ui.R
shinyServer(function(input, output) {

  # dataset selection
  # train, test are loaded from data and combi is created by binding train and test
  # depending on the user selection one of the three sets is assigned to the variable dataset
  # this is reactive so it will update all the output of the Shiny App
  dataset <- reactive({
    switch(input$dataset,
           "train" = train,
           "test" = test,
           "combined" = combi)
  })

  # filter data based on selection in the data summary tab
  output$table <- renderDataTable({
    
    d <- dataset() # init data 
    
    if (input$sex != "All"){
      d <- d[d$Sex == input$sex,]
    }
    if (input$title != "All"){
      d <- d[d$Title == input$title,]
    }
    if (input$embarked != "All"){
      d <- d[d$Embarked == input$embarked,]
    }
    if (input$fate != "All"){
      d <- d[d$Fate == input$fate,]
    }
    d
  })
  
  # scatter plot for data exploration
  output$scatter <- renderPlot({
    
    d <- dataset() # init data
    
    # create scatter plot (plot x versus y)
    p <- ggplot(d,
                aes_string(x=input$x,
                           y=input$y)) + geom_point()

    # color the data points
    if (input$color != 'None') {
      p <- p + aes_string(color=input$color)
    }
    # this adds the default smoother to the plot
    # the method of smoothing depends on the number of data points
    if (input$smooth) {
      p <- p + geom_smooth()
    }
    # this adds jitter to the plot
    # jitter introduces random scatter to avoid overplotting
    if (input$jitter) {
      p <- p + geom_jitter()
    }
    print(p)

  })
  
  # bar plot for variable importance
  output$variableimportance <- renderPlot({
    
    # the variable importance pre-computed and not reactive here to speed up things for this demo project 
    # it would be easy to change later by changing the init here
    d <-  data.frame(svImp) # init data to a dataframe 
    
    # get reactive slider value for minimum relative variable importance score
    minimp <- input$sliderminvimp
    
    # add feature names
    d$features <- names(svImp)
    
    # create new feature to track if a variable is selected or not
    # this will be reactive and linked to the slider input
    d$selected <- 0 # init to 0 --> not selected
    d$selected[d$svImp >= minimp] = 1 # set features with a relative importance score larger or equal to slider to 1
    d$selected <- factor(d$selected) # make factor for plotting

    # safety check if anything is selected at all 
    # because I use the min/max values from the dataset in ui.R this cannot happen
    # however, I'd like this code independent of the exact ui.R implementation
    if (sum(d$selected != 0)) {
      # create a bar chart which is by default vertical
      # identity means not to use bins but the value for the bar height
      # pretty important since each value is probably unique and the bins would be all equal
      # and finally flip it to make it horizontal
      p <- ggplot(data=d, aes(x=features, y=svImp, fill=selected)) + geom_bar(stat="identity") + coord_flip()
    }
    
    print(p)
    
  })
  
  # build logistic regression model with the selected variables
  # then use this model to show the ROC (Receiver Operating Characteristic) plot 
  output$roc <- renderPlot({
  
    # accessing input$goButton here makes this reactive code depend on it
    # when the input$goButton changes, the code will re-execute
    # it is better to do this here because it can take quite some time to calculate this block
    # note that the code is executed on initialization 
    input$goButton
    
    isolate({
      
      # get reactive slider value for minimum relative variable importance score
      minimp <- input$sliderminvimp
      
      # select features for variable selection
      Keep <- c("Fate", names(svImp[svImp>minimp]))
      
      # init training data
      d <- train[Keep]
    
      # use fixed random seed for repeatability
      set.seed(2014)
      
      # split data into train batch and test batch using caret functionality
      train.rows <- createDataPartition(d$Fate, p=0.8, list = FALSE)
      train.batch <- d[train.rows, ] # training
      test.batch <- d[-train.rows, ] # test
      
      # define control parameters to handle optional arguments for train function
      cv.ctrl <- trainControl(method = "repeatedcv", repeats = 3, summaryFunction = twoClassSummary, classProbs = TRUE)
      
      # train the logistic regression model
      glm <- train(Fate ~ ., data = train.batch,
                   method = "glm", metric = "ROC", trControl = cv.ctrl)
      
      # use model to generate predictions using the test data which was not used to train the model
      glm.pred <- predict(glm, test.batch)
      confusionMatrix(glm.pred, test.batch$Fate)
      glm.probs <- predict(glm, test.batch, type = "prob")
      
      # calculate ROC
      glm.ROC <- roc(response = test.batch$Fate,
                     predictor = glm.probs$Survived,
                     levels = levels(test.batch$Fate))
      
      # create reactive text output with the compute area under the curve
      output$auc <- renderText({ 
        paste("Area under the curve:", auc(glm.ROC))
      })
      
      # plot ROC using the stair steps (S) plot option
      p <- plot(glm.ROC, type="S", col='blue', lwd=5)
      
    }) # end isolate
    
    print(p)
    
  })
  


})