library(shiny)
customSidebar <- function(...) {
  div(class = 'span2', tags$form(class = 'well', ...))
}

shinyUI(pageWithSidebar(
  headerPanel("Step-wise Regression"),
  customSidebar(
    fileInput('file', 'Upload .csv: (Not yet implemented)', multiple = FALSE, accept = NULL),
    checkboxInput('intercept', 'Show intercept? (Not yet implemented)', TRUE),
    selectInput("alpha", "Alpha:", choices = c(".05", ".01", ".001")),
    uiOutput('sliderUI'),
    uiOutput('dependent'),
    uiOutput('independents')),
  mainPanel(
    withTags(div(class='row-fluid',
                 div(class='span9',
                     h4('Betas'),
                     plotOutput('betaplot')),
                 div(class='span3', 
                     h4('Current model'),
                     tableOutput('model'))
    )),
    withTags(div(class='row-fluid',
                 div(class='span9',
                     h4('F-tests'),
                     plotOutput('pplot')),
                 div(class='span2',
                     h4('Next up'),
                     verbatimTextOutput('next.up'))
    ))
  )))