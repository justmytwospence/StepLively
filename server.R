#### Load data, functions, and packages ####
library(shiny)
library(ggplot2)
source('stepwise.R')
bodyfat <- dget('bodyfat.dput')

#### Define server logic ####
shinyServer(function(input, output) {
  
  # Set the alpha threshold specified in the UI
  alpha <- reactive({
    switch(input$alpha,
           ".05" = .05,
           ".01" = .01,
           ".001" = .001)
  })
  
  # Display a dropdown menu for choosing the dependent variable,
  # also according to variables present in the dataset
  output$dependent <- renderUI({
    colnames <- names(bodyfat)
    selectInput("dependent", 
                "Dependent variable:", 
                choices = colnames,
                selected = "BODYFAT")
  })
  
  # Display the potential variable checkboxes according to the
  # variables that are present in the dataset
  output$independents <- renderUI({
    colnames <- names(bodyfat)
    checkboxGroupInput("independents", 
                       "Prospective independent variables:", 
                       choices  = setdiff(colnames, input$dependent),
                       selected = setdiff(colnames, input$dependent))
  })
  
  # Perform the step-wise regression
  traced <- reactive({
    stepforward(y = input$dependent, 
                x = setdiff(input$independents, input$dependent),
                data = bodyfat,
                alpha = alpha())
  })
  
  # Set the slider bar range according to the number of iterations
  # the step-wise regression required
  output$sliderUI <- renderUI({ 
    sliderInput("index", "Iteration:", 
                min = 1, 
                max = length(traced()),
                step = 1,
                value = 1)
  })
  
  # Print the next-most correlated variable that will be added next
  output$next.up <- renderPrint({
    cat(traced()[[input$index]]$next.up)
    #traced()[[input$index]][[1]]
  })
  
  # Create a bar plot of the beta values and update every time
  # the model is updated
  output$betaplot <- renderPlot({
    scaling.max <- max(sapply(traced(), function(x) {max(x$steps$Beta)}))
    scaling.min <- min(sapply(traced(), function(x) {min(x$steps$Beta)}))
    df <- traced()[[input$index]][[1]]
    b <- ggplot(data = df, 
                aes(x = Coefficient,
                    y = Beta)) + 
      geom_bar(stat = 'identity') + 
      ylim(scaling.min, scaling.max) + 
      ggtitle('Betas for the current iteration') +
      geom_hline(yintercept = 0) +
      coord_flip()
    print(b)
  })
  
  output$pplot <- renderPlot({
    all.p <- sapply(traced(), function(x) {x$p.value})
    p <- qplot(p)
  })
})