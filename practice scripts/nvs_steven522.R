library(shiny)
library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(DT)
library(tools)
master1 <- read.csv("c:/Users/romri/Documents/Working/master1.txt")

master2 <- select(master1, State, school.Name, degree.name, cip.name)

ui <- fluidPage(
  
  titlePanel("Test Table", windowTitle = "Test"),
      
  sidebarLayout(
    
    sidebarPanel(
      
      h3("State"),
      
      selectInput(inputId = "nvs.state",
                  label= "State:",
                  choices = names(table(master1$State)),
                  selected = "VA"),
      
      hr(),
      h3("School Name"),
      selectInput(inputId = "nvs.school.name",
                  label = "School Name:",
                  choices = NULL),
                 
      
      hr(),
      h3("Degree Type"),
      
      selectInput(inputId = "nvs.degree.name",
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
    y_school <- master1 %>% filter(State == input$nvs.state) %>% select(school.name)
    updateSelectInput(session, "nvs.school.name", "School Name:", choices = unique(y_school))
  })
  observe({
    z_degree <- master1$degree.name[master1$school.name == input$nvs.school.name]
    updateSelectInput(session, "nvs.degree.name", "Degree Name:", choices = unique(z_degree))
  })
  # Print data table if 
  output$degreetable <- renderDataTable({
      DT::datatable(data = filter(master1, State == input$nvs.state, school.name == input$nvs.school.name,
                                  degree.name == input$nvs.degree.name),
                    options = list(pageLength = 20)                )
    }
  )
  
}

# Create Shiny app object

shinyApp(ui = ui, server = server)