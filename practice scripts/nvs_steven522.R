library(shiny)
library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(DT)
library(tools)
master1 <- read.csv("c:/Users/romri/Documents/Working/Master.txt")

master2 <- select(master1, State, School_Name, Degree_Name, CIP_Name)

ui <- fluidPage(
  
  titlePanel("Test Table", windowTitle = "Test"),
      
  sidebarLayout(
    
    sidebarPanel(
      
      h3("State"),
      
      selectInput(inputId = "x",
                  label= "State:",
                  choices = names(table(master1$State)),
                  selected = "ID"),
      
      hr(),
      h3("School Name"),
      selectInput(inputId = "y",
                  label = "School Name:",
                  choices = NULL),
                 
      
      hr(),
      h3("Degree Type"),
      
      selectInput(inputId = "z",
                  label = "Degree Name:",
                  choices = NULL)
                  

  ),
      #output
      mainPanel(
        
        DT::dataTableOutput(outputId = "degreetable")
      )
      
      
  )                    
  
)
# Define server function required to create the scatterplot
server <- function(input, output, session) {
  observe({
    y_school <- master1 %>% filter(State == input$x) %>% select(School_Name)
    updateSelectInput(session, "y", "School Name:", choices = unique(y_school))
  })
  observe({
    z_degree <- master1$Degree_Name[master1$School_Name == input$y]
    updateSelectInput(session, "z", "Degree Name:", choices = unique(z_degree))
  })
  # Print data table if 
  output$degreetable <- renderDataTable({
      DT::datatable(data = filter(master1, State == input$x, School_Name == input$y, Degree_Name == input$z),
                    options = list(pageLength = 20)                )
    }
  )
  
}

# Create Shiny app object
shinyApp(ui = ui, server = server)