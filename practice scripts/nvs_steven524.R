library(shiny)
library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(DT)
library(tools)

master1 <- read.csv("master1.txt")
cip2 <- read_tsv("cip_code.txt")
cip1 <- cip2[order(cip2$CIP_Category),]

soc1 <- read_tsv("soc_code.txt")
cip_group1 <- (cip1$CIP_Category[1:10])
cip_group2 <- (cip1$CIP_Category[11:19])
cip_group3 <- (cip1$CIP_Category[20:28])
cip_group4 <- (cip1$CIP_Category[29:37])
#soc_group1 <- (soc1$SOC_Cat_Name)
#navbarPage sets up tab pages
ui <- shinyUI( navbarPage("College Plannning App",
                 tabPanel("Survey Page",
                        h4("Please check all that you are interested in"),
                        fluidRow(
                              
                          column(width = 3,
                              checkboxGroupInput(inputId = "nvs.Cip_Category1",
                                                label = "Categories",
                                                choices = cip_group1)
                                ),
                          column(width = 3,
                              checkboxGroupInput(inputId = "nvs.Cip_Category2",
                                                 label = "",
                                                 choices = cip_group2)
                                 ),
                          column(width = 3,
                              checkboxGroupInput(inputId = "nvs.Cip_Category3",
                                                 label = "",
                                                 choices = cip_group3)
                                 ),
                          column(width = 3,
                                 checkboxGroupInput(inputId = "nvs.Cip_Category4",
                                                    label = "",
                                                    choices = cip_group4)
                                 )
                              )
                          ),

                 
                 #Beginning of First Tab Panel                 
                 tabPanel("College and Degree select page",
                       
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

#Beginning of Second Tab Panel
                 tabPanel("College Selection",
      
                          sidebarLayout(
    
                            sidebarPanel(
      
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
                                         choices = NULL,
                                         multiple = TRUE)

                                         ),
#End of Second Tab Panel                            
#output
                             mainPanel(
        
                               DT::dataTableOutput(outputId = "nvs.choice.table")
                )
          )   
      )                    
))
# Define server function 
server <- function(input, output, session) {

  observeEvent(input$nvs.Cip_Category1, {
  cip_var <- cip1$CIP_Code[cip1$CIP_Category %in% input$nvs.Cip_Category1 |cip1$CIP_Category %in% input$nvs.Cip_Category2 |
                             cip1$CIP_Category %in% input$nvs.Cip_Category3 | cip1$CIP_Category %in% input$nvs.Cip_Category4]
  print (cip_var)
  })

  observeEvent(input$nvs.Cip_Category2, {
    cip_var <- cip1$CIP_Code[cip1$CIP_Category %in% input$nvs.Cip_Category1 |cip1$CIP_Category %in% input$nvs.Cip_Category2 |
                                        cip1$CIP_Category %in% input$nvs.Cip_Category3 | cip1$CIP_Category %in% input$nvs.Cip_Category4]
    print (cip_var)
  })
  
  observeEvent(input$nvs.Cip_Category3, {
    cip_var <- cip1$CIP_Code[cip1$CIP_Category %in% input$nvs.Cip_Category1 |cip1$CIP_Category %in% input$nvs.Cip_Category2 |
                                        cip1$CIP_Category %in% input$nvs.Cip_Category3 | cip1$CIP_Category %in% input$nvs.Cip_Category4]
    print (cip_var)
  })
  
  observeEvent(input$nvs.Cip_Category4, {
    cip_var <- cip1$CIP_Code[cip1$CIP_Category %in% input$nvs.Cip_Category1 |cip1$CIP_Category %in% input$nvs.Cip_Category2 |
                                        cip1$CIP_Category %in% input$nvs.Cip_Category3 | cip1$CIP_Category %in% input$nvs.Cip_Category4]
    print (cip_var)
  })
  
  
  observeEvent (input$nvs.school.name2, {
    updateSelectInput(session, "nvs.school.name", label= "School Name:", selected = input$nvs.school.name2)
    z_degree <- master1$degree.name[master1$school.name %in% input$nvs.school.name2]
    z_degree <- sort(z_degree)
    updateSelectInput(session, "nvs.degree.name2", "Degree Name:", choices = unique(z_degree))
    updateSelectInput(session, "nvs.degree.name", "Degree Name:", choices = unique(z_degree))
    
    cip_var <- cip1$CIP_Code[cip1$CIP_Category %in% input$nvs.Cip_Category1 |cip1$CIP_Category %in% input$nvs.Cip_Category2 |
                               cip1$CIP_Category %in% input$nvs.Cip_Category3 | cip1$CIP_Category %in% input$nvs.Cip_Category4]
    
   
    occ_var1 <- master1 %>% filter(school.name %in% input$nvs.school.name2, degree.name %in% input$nvs.degree.name,
                                 cip.cat %in% cip_var) %>% select(occ.name)

#    print (occ_var1)
    updateSelectInput(session, "nvs.occ.name", "Occupation:", choices = unique(occ_var1))

  })

  observeEvent (input$nvs.school.name, {
    updateSelectInput(session, "nvs.school.name2", label= "School Name:", selected = input$nvs.school.name)
    z_degree <- master1$degree.name[master1$school.name %in% input$nvs.school.name]
    z_degree <- sort(z_degree)
    updateSelectInput(session, "nvs.degree.name2", "Degree Name:", choices = unique(z_degree))
    updateSelectInput(session, "nvs.degree.name", "Degree Name:", choices = unique(z_degree))
    cip_var <- cip1$CIP_Code[cip1$CIP_Category %in% input$nvs.Cip_Category1 |cip1$CIP_Category %in% input$nvs.Cip_Category2 |
                               cip1$CIP_Category %in% input$nvs.Cip_Category3 | cip1$CIP_Category %in% input$nvs.Cip_Category4]
    
    occ_var1 <- master1 %>% filter(school.name %in% input$nvs.school.name2, degree.name %in% input$nvs.degree.name,
                                   cip.cat %in% cip_var) %>% select(occ.name)

    updateSelectInput(session, "nvs.occ.name", "Occupation:", choices = unique(occ_var1))
    
  })  
  
  observeEvent (input$nvs.degree.name, {
    updateSelectInput(session, "nvs.degree.name2", label= "Degree Name:",selected = input$nvs.degree.name)
    cip_var <- cip1$CIP_Code[cip1$CIP_Category %in% input$nvs.Cip_Category1 |cip1$CIP_Category %in% input$nvs.Cip_Category2 |
                               cip1$CIP_Category %in% input$nvs.Cip_Category3 | cip1$CIP_Category %in% input$nvs.Cip_Category4]
    
    occ_var1 <- master1 %>% filter(school.name %in% input$nvs.school.name2, degree.name %in% input$nvs.degree.name,
                                   cip.cat %in% cip_var) %>% select(occ.name)

    updateSelectInput(session, "nvs.occ.name", "Occupation:", choices = unique(occ_var1))
  })  

    observeEvent (input$nvs.degree.name2, {
    updateSelectInput(session, "nvs.degree.name", label= "Degree Name:",selected = input$nvs.degree.name2)
    cip_var <- cip1$CIP_Code[cip1$CIP_Category %in% input$nvs.Cip_Category1 |cip1$CIP_Category %in% input$nvs.Cip_Category2 |
                                 cip1$CIP_Category %in% input$nvs.Cip_Category3 | cip1$CIP_Category %in% input$nvs.Cip_Category4]
      
      occ_var1 <- master1 %>% filter(school.name %in% input$nvs.school.name2, degree.name %in% input$nvs.degree.name,
                                     cip.cat %in% cip_var) %>% select(occ.name)

      updateSelectInput(session, "nvs.occ.name", "Occupation:", choices = unique(occ_var1))
  })
  
    # Print data table
  observe ( {  
    req(input$nvs.school.name)
    output$nvs.choice.table <- renderDataTable({
        DT::datatable(data = filter(master1, school.name %in% input$nvs.school.name,
                                    degree.name == input$nvs.degree.name, occ.name %in% input$nvs.occ.name), 
                      options = list(pageLength = 10)) 
    })
})

}

# Create Shiny app object

shinyApp(ui = ui, server = server)