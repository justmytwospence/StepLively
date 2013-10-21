library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("Step-wise Regression"),
  sidebarPanel(
    fileInput('file', 'Upload .csv: (Not yet implemented)', multiple = FALSE, accept = NULL),
    selectInput("alpha", "Alpha:", choices = c(".05", ".01", ".001")),
    uiOutput('sliderUI'),
    uiOutput('dependent'),
    uiOutput('independents'),
    checkboxInput('intercept', 'Show intercept? (Not yet implemented)', TRUE)),
  mainPanel(
    h4('Betas for the current iteration'),
    plotOutput('betaplot'),
    h4('P value progression'),
    plotOutput('pplot'),
    h4('Model'),
    tableOutput('model'),
    h4('Next up:'),
    verbatimTextOutput('next.up'))
))