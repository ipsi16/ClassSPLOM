library("shiny")
library("shinyjs")
library("colourpicker")
library(shiny)
library(plotly)

source("getdata.R")

fluidPage(
  
  sidebarPanel( position = "right",
                fileInput(
                  "chosenfile",
                  label = h4("File input"),
                  accept = ".csv"
                ),
                
                selectizeInput("variable", "Variable:",
                               unique(getdata()["label"]), multiple = TRUE,  options= list(maxItems = 2)),
                actionButton("merge","Merge"),
                actionButton("undo", "Undo"),
                actionButton("split", "Split"),
                h1(""),
                tabsetPanel(
                  tabPanel("Selected", verbatimTextOutput("selected"))
                )
                
                #uiOutput("plot")
                #column( 10, verbatimTextOutput("selected"))
                
  ),
  
  # Shinyjs used for show/hide html elements(Loading screen, Submenus on buttons) 
  #useShinyjs(),
  #use to change background of any element
  #inlineCSS(list(.red = "background: blue")) 
  mainPanel(   
    
    #uiOutput('plots')
    # ,
    # div(
    # id = "main_content",
    # fluidRow(column( 1, plotlyOutput('diag1', height = "10%" , width = "10%"))), # Calls Diagonal plot 1
    
    tabsetPanel(
      tabPanel("Plot", plotlyOutput("bigplot")),
      tabPanel("ScatterPlot Matrix", uiOutput('plots'))
      
    ),
    # Loading page div
      div(
       id= "loading_page", 
        h1("Loading...")  
     ) 
  )
  
  #)#, # Calls entire scatterplot matrix
  #  uiOutput("hover_info") # Shows hovered point
  
  #   )
  #  ),
)
# hidden(
#  colourInput("col", "Select colour of new class"),
# actionButton("okay","Okay")
#  )

#)
























