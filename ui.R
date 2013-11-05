library(shiny)

customSidebar <- function(...) {
  div(class = 'span3', style = 'min-width: 275px', tags$form(class = 'well', ...))
}
uploadBar <- function(...) {
  div(class = 'span12', style = 'min-width: 400px', tags$form(class = 'well', ...))
}
actionButton <- function(inputId, label, btn.style = "" , css.class = "") {
  if ( btn.style %in% c("primary","info","success","warning","danger","inverse","link")) {
    btn.css.class <- paste("btn",btn.style,sep="-")
  } else btn.css.class = ""
  tags$button(id=inputId, type="button", class=paste("btn action-button",btn.css.class,css.class,collapse=" "), label)
}

shinyUI(pageWithSidebar(
  headerPanel("Step-wise Regression"),
  customSidebar(
    actionButton("goButton", "Step Lively", css.class = 'btn-primary btn-block'),
    conditionalPanel(condition = 'input.goButton != 0', uiOutput('sliderUI')),
    br(),
    selectInput("alpha", label = "Alpha:", choices = c(".05", ".01", ".001")),
    uiOutput('dependent'),
    checkboxInput('intercept', label = 'Show intercept', value = FALSE),
    uiOutput('independents')),
  mainPanel(
    tabsetPanel(tabPanel("Variable Selection", 
                         h4('Coefficients'),
                         plotOutput('bplot'),
                         h4('Next Up...'),
                         plotOutput('nplot')),
                tabPanel("Model Evaluation",
                         h4('F-tests'),
                         plotOutput('pplot'),
                         h4('R-squared'),
                         plotOutput('rplot')),
                tabPanel("Current Model",
                         tableOutput('model')),
                tabPanel("Data",
                         uploadBar(
                           withTags(div(class='row-fluid',
                                        div(class='span4', 
                                            fileInput('file', 'Upload your own CSV', 
                                                      multiple = FALSE, 
                                                      accept = NULL)),
                                        div(class='span2', 
                                            radioButtons('sep', 'Separator',
                                                         c(Comma=',',
                                                           Semicolon=';',
                                                           Tab='\t'),
                                                         'Comma')),
                                        div(class='span2',
                                            checkboxInput('header', 'Header', TRUE))))),
                         tableOutput('data'))))
))