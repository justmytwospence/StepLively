library(shiny)
customSidebar <- function(...) {
  div(class = 'span3', tags$form(class = 'well', ...))
}

shinyUI(pageWithSidebar(
  headerPanel("Step-wise Regression"),
  customSidebar(
    fileInput('file', 'Upload .csv', multiple = FALSE, accept = NULL),
    checkboxInput('header', 'Header', TRUE),
    radioButtons('sep', 'Separator',
                 c(Comma=',',
                   Semicolon=';',
                   Tab='\t'),
                 'Comma'),
    tags$hr(),
    uiOutput('sliderUI'),
    selectInput("alpha", label = "Alpha:", choices = c(".05", ".01", ".001")),
    uiOutput('dependent'),
    checkboxInput('intercept', label = 'Show intercept?', value = FALSE),
    uiOutput('independents')),
  mainPanel(
    withTags(div(class='row-fluid',
                 div(class='span8',
                     h4('Coefficients'),
                     plotOutput('betaplot')),
                 div(class='span3', 
                     h4('Current model'),
                     tableOutput('model'))
    )),
    withTags(div(class='row-fluid',
                 div(class='span8',
                     h4('F-tests'),
                     plotOutput('pplot')),
                 div(class='span3',
                     h4('Next up'),
                     verbatimTextOutput('next.up'))
    )))
))