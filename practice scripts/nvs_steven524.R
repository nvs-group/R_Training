library(shiny)
library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(DT)
library(tools)

master1 <- read.csv("c:/Users/romri/Documents/Working/master1.txt")

#navbarPage sets up tab pages
ui <- navbarPage("NVS College Plannning App",
#Beginning of First Tab Panel                 
                 tabPanel("Survey Page",
                       h4("College plan Survey"),
                       sidebarLayout(
                         
                         sidebarPanel(
                           
                          
                           selectInput(inputId = "nvs.school.name2",
                                       label= "School Name:",
                                       choices =  unique(master1$school.name),
                                       multiple = TRUE,
                                       selectize = TRUE),
                           
                           
                           selectInput(inputId = "nvs.degree.name2",
                                       label = "Degree Name:",
                                       choices = NULL)
                           

                         ),
                       
                       mainPanel()
                       )
                     ),
#End of First Tab Panel
#Beginning of Second Tab Panel
                 tabPanel("College Selection",
      
                          sidebarLayout(
    
                            sidebarPanel(
      
                             verbatimTextOutput("out.nvs.school.name"),
                             selectInput(inputId = "nvs.school.name",
                                        label= "School Name:",
                                        choices =  unique(master1$school.name),
                                        multiple = TRUE),
      
                             
                             selectInput(inputId = "nvs.degree.name",
                                        label = "Degree Name:",
                                        choices = NULL),
                            
                             
                             selectInput(inputId = "nvs.cip.name",
                                         label = "Curriculum name:",
                                         choices = unique(master1$cip.name),
                                         multiple = TRUE),
                             
                            
                             selectInput(inputId = "nvs.occ.name",
                                         label = "Occupation:",
                                         choices = unique(master1$occ.name),
                                         multiple = TRUE)

                                         ),
#End of Second Tab Panel                            
#output
                             mainPanel(
        
                               DT::dataTableOutput(outputId = "nvs.choice.table")
                )
          )   
      )                    
)
# Define server function 
server <- function(input, output, session) {

  observeEvent (input$nvs.school.name2, {
    updateSelectInput(session, "nvs.school.name", label= "School Name:", selected = input$nvs.school.name2)
    z_degree <- master1$degree.name[master1$school.name == input$nvs.school.name2]
    updateSelectInput(session, "nvs.degree.name2", "Degree Name:", choices = unique(z_degree))
    updateSelectInput(session, "nvs.degree.name", "Degree Name:", choices = unique(z_degree))

  })

  observeEvent (input$nvs.school.name, {
    updateSelectInput(session, "nvs.school.name2", label= "School Name:", selected = input$nvs.school.name)
    z_degree <- master1$degree.name[master1$school.name == input$nvs.school.name]
    updateSelectInput(session, "nvs.degree.name2", "Degree Name:", choices = unique(z_degree))
    updateSelectInput(session, "nvs.degree.name", "Degree Name:", choices = unique(z_degree))
    
  })  
  
  observeEvent (input$nvs.degree.name, {
    updateSelectInput(session, "nvs.degree.name2", label= "Degree Name:",selected = input$nvs.degree.name)
    
  })  

    observeEvent (input$nvs.degree.name2, {
    updateSelectInput(session, "nvs.degree.name", label= "Degree Name:",selected = input$nvs.degree.name2)
   
  })
  
    # Print data table
  observe ({  
 
    output$nvs.choice.table <- renderDataTable({
      DT::datatable(data = filter(master1, school.name == input$nvs.school.name,
                                 degree.name == input$nvs.degree.name), options = list(pageLength = 20)                )

  }) 
    
})
  

}

# Create Shiny app object

shinyApp(ui = ui, server = server)