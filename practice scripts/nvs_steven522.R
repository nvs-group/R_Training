library(shiny)
library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(DT)
library(tools)
master1 <- read.csv("c:/Users/romri/Documents/Working/Master.txt")
# Create alphabetical, unique, sorted list of state abbreviations
tablestate <- names(table(master1$State))
# Create alphabetical, unique, sorted list of school names
tableschoolname <- names(table(master1$School_Name))

# Create alphabetical, unique, sorted list of degree types
tabledegreename <- names(table(master1$Degree_Name))


master2 <- select(master1, State, School_Name, Degree_Name, CIP_Name)

ui <- fluidPage(
  
  titlePanel("Test Table", windowTitle = "Test"),
      
  sidebarLayout(
    
    sidebarPanel(
      
      h3("State"),
      
      selectInput(inputId = "x",
                  label= "State:",
                  choices = names(table(master2$State)),
                  selected = "ID"),
      
      hr(),
      h3("School Name"),
      selectInput(inputId = "y",
                  label = "School Name:",
                  choices = names(table(master2$School_Name)),
                  selected = "Idaho State University"),
      
      hr(),
      h3("Degree Type"),
      
      selectInput(inputId = "z",
                  label = "Degree Name:",
                  choices = names(table(master2$Degree_Name)),
                  selected = "Associates Degree"),
      hr(),
      h3("Course Name"),
      
      selectInput(inputId = "w",
                  label = "Course Name:",
                  choices = names(table(master2$CIP_Name)),
                  selected = "Business")
      

  ),
      #output
      mainPanel(
        
        DT::dataTableOutput(outputId = "degreetable")
      )
      
      
  )                    
  
)
# Define server function required to create the scatterplot
server <- function(input, output) {
  #degree_selected <- reactive ({filter(master2, State %in% input$x & School_Name %in% input$y)
  #})

  # Print data table if checked
  output$degreetable <- renderDataTable({
      DT::datatable(data = filter(master1, State == input$x, School_Name == input$y, Degree_Name == input$z),
                    options = list(pageLength = 20)                )
    }
  )
  
}

# Create Shiny app object
shinyApp(ui = ui, server = server)