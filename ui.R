library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("Step-wise Regression"),
  sidebarPanel(
    selectInput("alpha", "Alpha:", 
                choices = c(".05", ".01", ".001")),
    uiOutput('sliderUI'),
    uiOutput('dependent'),
    uiOutput("independents")),
  mainPanel(
    plotOutput("betaplot"),
    verbatimTextOutput("next.up"))
))