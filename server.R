#### Load data, functions, and packages ####
library(shiny)
library(ggplot2)
source('stepwise.R')

#### Define server logic ####
shinyServer(function(input, output) {
  
  # Use the provided .csv
  data <- reactive({
    inFile <- input$file
    if (is.null(inFile))
      return(dget('bodyfat.dput'))
    read.csv(inFile$datapath, header = input$header, sep = input$sep)
  })
  
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
    colnames <- names(data())
    selectInput("dependent", 
                "Dependent variable:", 
                choices = colnames,
                selected = colnames[[1]])
  })
  
  # Display the potential variable checkboxes according to the
  # variables that are present in the dataset
  output$independents <- renderUI({
    colnames <- names(data())
    checkboxGroupInput("independents", 
                       "Prospective independent variables:", 
                       choices  = setdiff(colnames, input$dependent),
                       selected = setdiff(colnames, input$dependent))
  })
  
  # Perform the step-wise regression
  traced <- reactive({
    stepforward(y = input$dependent, 
                x = setdiff(input$independents, input$dependent),
                data = data(),
                alpha = alpha(),
                intercept = input$intercept)
  })
  
  # Set the slider bar range according to the number of iterations
  output$sliderUI <- renderUI({ 
    try({sliderInput("index", label = "Iteration:", 
                     min = 1, max = length(traced()), step = 1, value = 1)},
        silent = TRUE)
  })
  
  # Create a bar plot of the beta values 
  output$betaplot <- renderPlot({
    try({scaling.max <- max(sapply(traced(), function(x) {max(x$steps$Beta)}))
         scaling.min <- min(sapply(traced(), function(x) {min(x$steps$Beta)}))
         df.b <- traced()[[input$index]][[1]]
         if (input$index != length(traced())) {
           pad <- data.frame('Beta' = 0,
                             'Coefficient' = setdiff(tail(traced(), 1)[[1]][[1]]$Coefficient, 
                                                     df.b$Coefficient))
           df.b <- rbind(df.b, pad)
         }
         df.b$Coefficient <- factor(df.b$Coefficient, 
                                    levels = as.character(df.b$Coefficient))
         b <- ggplot(data = df.b,
                     aes(x = Coefficient,
                         y = Beta)) + 
           geom_bar(stat = 'identity',
                    fill = '#2075c7',
                    colour = '#2075c7') + 
           ylim(scaling.min, scaling.max) + 
           geom_hline(yintercept = 0) +
           xlab(NULL) +
           theme(axis.text.y = element_text(size = 14,
                                            colour = 'black'),
                 axis.ticks = element_line(size = 0)) +
           coord_flip() 
         print(b)
    },
        silent = TRUE)
  })
  
  output$pplot <- renderPlot({
    try({
      df.anova <- data.frame(P.value = sapply(traced()[1:input$index], function(x) {x$anova.p}),
                             Type = 'ANOVA',
                             Count = c(1:input$index))
      df.model <- data.frame(P.value = sapply(traced()[1:input$index], function(x) {x$model.p}),
                             Type = 'Model',
                             Count = c(1:input$index))
      df.p <- rbind(df.anova, df.model)
      p <- ggplot(data = df.p,
                  aes(x = Count,
                      y = P.value,
                      colour = Type,
                      linetype = Type)) +
        geom_line(size = 2,
                  alpha = .5) +
        geom_hline(yintercept = alpha(),
                   linetype = 'dashed') +
        geom_point(size = 6) +
        scale_color_manual(values = c('#2075c7', '#CC0000')) +
        xlab('Iteration') +
        ylab('P-value') +
        theme(axis.title.y = element_text(angle = 0),
              legend.position = 'bottom',
              legend.direction = 'horizontal',
              legend.key = theme_blank()) +
        scale_x_discrete()
      print(p)},
        silent = TRUE)
  })
  
  # Print the next-most correlated variable that will be added next
  output$next.up <- renderPrint({
    try({cat(traced()[[input$index]]$next.up)},
        silent = TRUE)
  })
  
  # Print the current model
  output$model <- renderTable({
    traced()[[input$index]]$model
  })
})