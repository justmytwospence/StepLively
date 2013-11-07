library(shiny)
library(shinyIncubator)

customSidebar <- function(...) {
  div(class = 'span3', style = 'min-width: 275px', tags$form(class = 'well', ...))
}
uploadBar <- function(...) {
  div(class = 'span12', tags$form(class = 'well', ...))
}
actionButton <- function(inputId, label, btn.style = "" , css.class = "") {
  if ( btn.style %in% c("primary","info","success","warning","danger","inverse","link")) {
    btn.css.class <- paste("btn", btn.style, sep="-")
  } else btn.css.class = ""
  tags$button(id=inputId, type="button", class=paste("btn action-button",btn.css.class,css.class,collapse=" "), label)
}

shinyUI(pageWithSidebar(
  headerPanel("StepLively"),
  customSidebar(
    actionButton("goButton", "Execute", css.class = 'btn-primary btn-block'),
    conditionalPanel(condition = 'input.goButton != 0', uiOutput('sliderUI')),
    div(br()),
    selectInput("alpha", label = "Alpha:", choices = c(".05", ".01", ".001")),
    uiOutput('dependent'),
    checkboxInput('intercept', label = 'Show intercept (requires re-execution)', value = FALSE),
    uiOutput('independents')),
  mainPanel(
    progressInit(),
    tabsetPanel(tabPanel("Variable Selection", 
                         conditionalPanel(condition = 'input.goButton == 0', div('Click the "Execute" button to StepLively!', 
                                                                                 class = 'alert alert-block')),
                         conditionalPanel(condition = 'input.goButton != 0',
                                          div(class = 'row-fluid',
                                              div(class = 'span6',
                                                  h4('Coefficients'),
                                                  plotOutput('bplot', height = '550px')),
                                              div(class = 'span6',
                                                  h4('Partial Correlations'),
                                                  plotOutput('nplot', height = '550px'))))),
                tabPanel("Model Evaluation",
                         conditionalPanel(condition = 'input.goButton == 0', div('Click the "Execute" button to StepLively!', 
                                                                                 class = 'alert alert-block')),
                         conditionalPanel(condition = 'input.goButton != 0',
                                          div(class = 'row-fluid',
                                              div(class = 'span6',
                                                  h4('F-tests'),
                                                  plotOutput('pplot', 
                                                             height = '550px')),
                                              div(class = 'span6', 
                                                  h4('R-squared'),
                                                  plotOutput('rplot', 
                                                             height = '550px'))))),
                tabPanel("Current Model",
                         conditionalPanel(condition = 'input.goButton == 0', div('Click the "Execute" button to StepLively!', 
                                                                                 class = 'alert alert-block')),
                         conditionalPanel(condition = 'input.goButton != 0', 
                                          tableOutput('model'))),
                tabPanel("Data",
                         uploadBar(
                           div(class = 'row-fluid',
                               div(class = 'span4', 
                                   fileInput('file', 'Upload your own data', 
                                             multiple = FALSE, 
                                             accept = NULL)),
                               div(class = 'span2', 
                                   radioButtons('sep', 'Separator',
                                                c(Comma=',',
                                                  Semicolon=';',
                                                  Tab='\t'),
                                                'Comma')),
                               div(class = 'span2', 
                                   radioButtons('quote', 'Quote',
                                                c(None='',
                                                  'Double Quote'='"',
                                                  'Single Quote'="'"),
                                                'Double Quote')),
                               div(class = 'span2', 
                                   checkboxInput('header', 'Header', TRUE)))),
                         tableOutput('data')),
                tabPanel("About",
                         div(tags$p('Created by Spencer Boucher, MS candidate in data analytics.')),
                         div(tags$a(href = 'http://spencerboucher.com', 'spencerboucher.com')),
                         div(tags$a(href = 'http://github.com/justmytwospence/StepLively', 'Find the source code here.')))))
))