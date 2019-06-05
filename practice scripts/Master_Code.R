library(markdown)
library(shiny)
library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(DT)
library(tools)
selectedrowindex = 0

#Read in main data table
master1 <- read.csv("master1.txt")
#Read cip data table and order alphabetically
cip2 <- read_tsv("cip_code.txt")
cip1 <- cip2[order(cip2$CIP_Category),]
#Read soc data table and order alphabetically
soc2 <- read_tsv("soc_code.txt")
soc1 <- soc2[order(soc2$SOC_Cat_Name),]
#split soc into two groups
soc_group1 <- (soc1$SOC_Cat_Name[1:12])
soc_group2 <- (soc1$SOC_Cat_Name[13:24])
#spit cip into 4 groups
cip_group1 <- (cip1$CIP_Category[1:10])
cip_group2 <- (cip1$CIP_Category[11:19])
cip_group3 <- (cip1$CIP_Category[20:28])
cip_group4 <- (cip1$CIP_Category[29:37])

ui <- fluidPage(h1("College Planning"),
                
                navbarPage("                   ",
                           
                           tabPanel("My Profile",
                                    
                                    navlistPanel(widths = c(3, 9),
                                                 tabPanel("Instructions",
                                                          h2("Go through each tab and select the items you currently know about")),
                                                 
                                                 tabPanel("School select",
                                                          h3("I know exactly where I want to go:"),
                                                          
                                                          selectInput(inputId = "pre.school.name",
                                                                      label = "",
                                                                      choices = levels(master1$school.name),
                                                                      multiple = TRUE)
                                                 ),
                                                 tabPanel("Degree select",
                                                          h3("What is the highest degree you are planning to get?"),
                                                          
                                                          selectInput(inputId = "pre.degree.name",
                                                                      label = "",
                                                                      choices = levels(master1$degree.name),
                                                                      multiple = TRUE)
                                                 ),
                                                 tabPanel("Occupation Category Select",
                                                          fluidRow(
                                                            h4("Which of these occupations would you consider?"),
                                                            
                                                            hr(),
                                                            column(width = 6,
                                                                   checkboxGroupInput(inputId = "pre.occupation1",
                                                                                      label = "",
                                                                                      choices = soc_group1)),
                                                            column(width = 6,
                                                                   checkboxGroupInput(inputId = "pre.occupation2",
                                                                                      label = "",
                                                                                      choices = soc_group2))
                                                          )
                                                 ),
                                                 tabPanel("Curriculum Category Select",
                                                          fluidPage(
                                                            h4("Please check all curriculum that you are interested in"),
                                                            
                                                            fluidRow(
                                                              
                                                              column(width = 3,
                                                                     checkboxGroupInput(inputId = "survey.Cip_Category1",
                                                                                        label = "Categories",
                                                                                        choices = cip_group1)
                                                              ),
                                                              column(width = 3,
                                                                     checkboxGroupInput(inputId = "survey.Cip_Category2",
                                                                                        label = "",
                                                                                        choices = cip_group2)
                                                              ),
                                                              column(width = 3,
                                                                     checkboxGroupInput(inputId = "survey.Cip_Category3",
                                                                                        label = "",
                                                                                        choices = cip_group3)
                                                              ),
                                                              column(width = 3,
                                                                     checkboxGroupInput(inputId = "survey.Cip_Category4",
                                                                                        label = "",
                                                                                        choices = cip_group4)
                                                              )
                                                            )
                                                          )),
                                                 tabPanel("Occupation Name select",
                                                          h3("What occupation are you planning to have?"),
                                                          
                                                          selectInput(inputId = "pre.occ.name",
                                                                      label = "",
                                                                      choices = levels(master1$occ.name),
                                                                      multiple = TRUE)
                                                 ),
                                                 
                                                 tabPanel("Curriculum Name select",
                                                          
                                                          h3("What curriculum are you planning to study?"),
                                                          
                                                          selectInput(inputId = "pre.cip.name",
                                                                      label = "",
                                                                      choices = levels(master1$cip.name),
                                                                      multiple = TRUE)
                                                 ),
                                                 
                                                 tabPanel("Salary Select",
                                                          h3("How much income would you like to make in 10 to 15 years?"), 
                                                          
                                                          sliderInput(inputId = "pre.income",
                                                                      label = "",
                                                                      value = min(sort(unique(master1$X10p))),
                                                                      min = min(sort(unique(master1$X10p))),
                                                                      max = max(sort(unique(master1$X10p))))
                                                          
                                                 ),
                                                 tabPanel("Max Tuition Select",
                                                          h3("How much tuition would you like to pay per years?"),
                                                          
                                                          sliderInput(inputId = "pre.tuition",
                                                                      label = "",
                                                                      value = max(sort(unique(master1$InStOff))),
                                                                      min = min(sort(unique(master1$InStOff))),
                                                                      max = max(sort(unique(master1$InStOff))))
                                                 )
                                    )
                           ),
                           tabPanel("Build Scenerios",
                                    sidebarLayout(
                                      
                                      sidebarPanel(width = 3,
                                                   
                                                   selectInput(inputId = "nvs.school.name",
                                                               label= "School Name:",
                                                               choices =  levels(master1$school.name),
                                                               multiple = TRUE),
                                                   
                                                   selectInput(inputId = "nvs.degree.name",
                                                               label = "Degree Name:",
                                                               choices =  levels(master1$degree.name),
                                                               multiple = TRUE),
                                                   
                                                   selectInput(inputId = "nvs.cip.cat",
                                                               label = "Curriculum Category:",
                                                               choices = cip1$CIP_Category,
                                                               multiple = TRUE),
                                                   
                                                   selectInput(inputId = "nvs.cip.name",
                                                               label = "Curriculum Name:",
                                                               choices = levels(master1$cip.name),
                                                               multiple = TRUE),
                                                   
                                                   selectInput(inputId = "nvs.occ.cat",
                                                               label = "Occupation Category:",
                                                               choices = soc1$SOC_Cat_Name,
                                                               multiple = TRUE),
                                                   
                                                   selectInput(inputId = "nvs.occ.name",
                                                               label = "Occupation Name:",
                                                               choices = levels(master1$occ.name),
                                                               multiple = TRUE),
                                                   
                                                   sliderInput(inputId = "nvs.income",
                                                               label = "Desired Income Level:",
                                                               value = min(sort(unique(master1$X10p))),
                                                               min = min(sort(unique(master1$X10p))),
                                                               max = max(sort(unique(master1$X10p)))),
                                                   
                                                   sliderInput(inputId = "nvs.tuition",
                                                               label = "Desired Tuition Level",
                                                               value = max(sort(unique(master1$InStOff))),
                                                               min = min(sort(unique(master1$InStOff))),
                                                               max = max(sort(unique(master1$InStOff))))
                                                   
                                      ),
                                      
                                      #output
                                      mainPanel(width = 9,
                                                
                                                DT::dataTableOutput(outputId = "nvs.choice.table")
                                      )
                                    )    
                           ),
                           tabPanel("Compare Scenerios",
                                    sidebarLayout(
                                      sidebarPanel(width = 3
                                      ),
                                      mainPanel(width = 9,
                                                
                                                DT::dataTableOutput(outputId = "row.choice.table")
                                      )  
                                    )
                           ),
                           tabPanel("Tools"),
                           
                           
                           tabPanel("About",
                                    h1("Welcome"),
                                    hr(),
                                    "Congratulations.",
                                    "You have found the best",
                                    "college planning app!"
                           )
                )
)   

server <- function(input, output, session) {
  #Reactive variable that uses selected choices or full column if empty
  
  school.name_var <- reactive({
    if(is.null(input$nvs.school.name )) {
      sort(unique(master1$school.name))} else {
        input$nvs.school.name
      }
  })
  
  #Reactive variable that uses selected choices or full column if empty 
  
  degree.name_var <- reactive({
    if(is.null(input$nvs.degree.name )) {
      sort(unique(master1$degree.name))} else {
        input$nvs.degree.name
      }
  })
  #Reactive variable that uses selected choices or full column if empty
  
  occ.name_var <- reactive({
    if(is.null(input$nvs.occ.name)) {
      sort(unique(master1$occ.name))} else {
        input$nvs.occ.name
      }
  })  
  #Reactive variable that uses selected choices or full column if empty
  
  cip.name_var <- reactive({
    if(is.null(input$nvs.cip.name)) {
      sort(unique(master1$cip.name))} else {
        input$nvs.cip.name
      }
  })
  #Filter for First Table
  table_var <- reactive({
    filter(master1, school.name %in% school.name_var(), degree.name %in% degree.name_var(),
           occ.name %in% occ.name_var(), cip.name %in% cip.name_var(),
           X10p >= input$nvs.income, InStOff <= input$nvs.tuition)
  })
  
  
  #First Table
  observe ( {  
    #   req(input$nvs.school.name)
    
    output$nvs.choice.table <- renderDataTable({
      DT::datatable(data = table_var(), 
                    options = list(pageLength = 10),selection = list(mode = "multiple"))
    })
  })
  #Using input to school prefernece to update other fields choices
  observeEvent(input$nvs.school.name, {
    if(is.null(input$nvs.degree.name)) {
      updateSelectInput(session, "nvs.degree.name", "Degree Name:", choices = unique(table_var()$degree.name))
    }
    if(is.null(input$nvs.occ.name)) {
      updateSelectInput(session, "nvs.occ.name", "Occupation Name:", choices = unique(table_var()$occ.name))
    }
    if(is.null(input$nvs.cip.name)) {
      updateSelectInput(session, "nvs.cip.name", "Curriculum Name:", choices = unique(table_var()$cip.name))
    }
  })
  
  observeEvent(input$nvs.degree.name, {
    if(is.null(input$nvs.school.name)) {
      updateSelectInput(session, "nvs.school.name", "School Name:", choices = unique(table_var()$school.name))
    }
    if(is.null(input$nvs.occ.name)) {
      updateSelectInput(session, "nvs.occ.name", "Occupation Name:", choices = unique(table_var()$occ.name))
    }
    if(is.null(input$nvs.cip.name)) {
      updateSelectInput(session, "nvs.cip.name", "Curriculum Name:", choices = unique(table_var()$cip.name))
    }
  })
  observeEvent(input$nvs.occ.name, {
    if(is.null(input$nvs.school.name)) {
      updateSelectInput(session, "nvs.school.name", "School Name:", choices = unique(table_var()$school.name))
    }
    if(is.null(input$nvs.degree.name)) {
      updateSelectInput(session, "nvs.degree.name", "Degree Name:", choices = unique(table_var()$degree.name))
    }
    if(is.null(input$nvs.cip.name)) {
      updateSelectInput(session, "nvs.cip.name", "Curriculum Name:", choices = unique(table_var()$cip.name))
    }
  })  
  observeEvent(input$nvs.cip.name, {  
    if(is.null(input$nvs.school.name)) {
      updateSelectInput(session, "nvs.school.name", "School Name:", choices = unique(table_var()$school.name))
    }
    if(is.null(input$nvs.degree.name)) {
      updateSelectInput(session, "nvs.degree.name", "Degree Name:", choices = unique(table_var()$degree.name))
    }
    if(is.null(input$nvs.occ.name)) {
      updateSelectInput(session, "nvs.occ.name", "Occupation Name:", choices = unique(table_var()$occ.name))
    }
    
  })    
  
  
  
  
  # from school choice on preference page  
  observeEvent(input$pre.school.name, {
    updateSelectInput(session, "nvs.school.name", "School Name:", selected = input$pre.school.name)
  })
  #Import degree choice to scenerio from degree choice on preference page  
  observeEvent(input$pre.degree.name, {
    updateSelectInput(session, "nvs.degree.name", "Degree Name:", selected = input$pre.degree.name)
  })
  # from occupation name choice on preference page    
  observeEvent(input$pre.occ.name, {
    updateSelectInput(session, "nvs.occ.name", "Occupation Name:", selected = input$pre.occ.name)
  })
  # from curriculum name choice on preference page  
  observeEvent(input$pre.cip.name, {
    updateSelectInput(session, "nvs.cip.name", "Curriculum Name:", selected = input$pre.cip.name)
  })
  # from income level choice on preference page  
  observeEvent(input$pre.income, {
    updateSliderInput(session, "nvs.income", "Desired Income Level:", value = input$pre.income)
  })
  # from tuition cost level choice on preference page  
  observeEvent(input$pre.tuition, {
    updateSliderInput(session, "nvs.tuition", "Desired Tuition Level:", value = input$pre.tuition)
  })
  
  #Table prep with filters and Column choices for second table
  new_var <- reactive({
    master1 %>% filter(school.name %in% school.name_var(), degree.name %in% degree.name_var(),
                       occ.name %in% occ.name_var() , cip.name %in% cip.name_var()) %>% 
      select(school.name, degree.name, cip.name, occ.name,InStOff, X25p)
  })
  #Second Table after choosing rows    
  output$row.choice.table <- renderDataTable({ 
    DT::datatable(data = new_var()[input$nvs.choice.table_rows_selected,],
                  options = list(pageLength = 10), selection = list(mode = "none"))
  })
}
shinyApp(ui = ui, server = server)