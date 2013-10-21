library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("Step-wise Regression"),
  sidebarPanel(
    fileInput('file', 'Upload .csv: (Not yet implemented)', multiple = FALSE, accept = NULL),
    checkboxInput('intercept', 'Show intercept? (Not yet implemented)', TRUE),
    selectInput("alpha", "Alpha:", choices = c(".05", ".01", ".001")),
    uiOutput('sliderUI'),
    uiOutput('dependent'),
    uiOutput('independents')),
  mainPanel(
    h4('Betas'),
    plotOutput('betaplot'),
    h4('F-tests'),
    plotOutput('pplot'),
    withTags(div(class='row-fluid',
                 div(class='span3',
                     h4('Next up'),
                     verbatimTextOutput('next.up')),
                 div(class='span5', 
                     h4('Current model'),
                     tableOutput('model'))
    ))
)))