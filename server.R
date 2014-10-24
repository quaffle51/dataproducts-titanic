# load required packages
require(shiny)
require(ggplot2)
require(rpart)
require(rattle)
require(rpart.plot)
# require(RColorBrewer)

# read the data
load('titanictrain.Rdata')
load('titanictest.Rdata')

# join data
combi <- rbind(train, test)

# server logic required to run ui.R
shinyServer(function(input, output) {

  dataset <- reactive({
    switch(input$dataset,
           "train" = train,
           "test" = test,
           "combined" = combi)
  })

  output$summary_of_x <- renderPrint({
    v <- input$x
    data <- dataset()
    summary(data[v])
  })

  output$summary_of_y <- renderPrint({
    v <- input$y
    data <- dataset()
    summary(data[v])
  })

  output$summary_of_color <- renderPrint({
    if (input$color != 'None') {
      v <- input$color
      data <- dataset()
      summary(data[v])
    }
  })

  output$scatter <- renderPlot({
    data <- dataset()
    p <- ggplot(data,
                aes_string(x=input$x,
                           y=input$y)) + geom_point()

    if (input$color != 'None') {
      p <- p + aes_string(color=input$color)
    }

    if (input$smooth) {
      p <- p + geom_smooth()
    }

    print(p)

  })

  output$box <- renderPlot({

    cat(input$x, input$y, input$color, '\n')

    p <- ggplot(data = dataset(),
                mapping = aes(factor(input$x),
                              input$y))

    if (input$color != 'None') {
      p <- p + geom_boxplot(mapping = aes_string(
        fill = factor(input$color)))
    }
    else {
      p <- p + geom_boxplot()
    }

    print(p)

  })

  output$tree <- renderPlot({

    # reactive update of dataset
    data <- dataset()

    # build a tree of the features
    fit <- rpart(Survived ~ Pclass + Sex + Age +
                   Fare + Embarked + Title + FamilySize +
                   Child, data=data, method="class")

    complexity <- input$complexity

    fit <- prune(fit, cp = complexity)

    cat(data$names)
    cat(fit$variable.importance)

    p <- fancyRpartPlot(fit)

    print(p)

  })




})