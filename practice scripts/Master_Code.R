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

soc_group1 <- (soc1$SOC_Cat_Name[1:12])
soc_group2 <- (soc1$SOC_Cat_Name[13:24])

cip_group1 <- (cip1$CIP_Category[1:10])
cip_group2 <- (cip1$CIP_Category[11:19])
cip_group3 <- (cip1$CIP_Category[20:28])
cip_group4 <- (cip1$CIP_Category[29:37])

ui <- fluidPage(h1("NVS, LLC"),
                
                navbarPage("College Planning",
                           tabPanel("About",
                                    h1("Welcome"),
                                    hr(),
                                    "Congratulations.",
                                    "You have found the best",
                                    "college planning app!"
                           ),
                           tabPanel("Preferences",
                                    
                                    navlistPanel(widths = c(3, 9),
                                                 tabPanel("Instructions",
                                                          h2("Go through each tab and select the items you currently know about")),
                                                 tabPanel("School select",
                                                          h3("I know exactly where I want to go:"),
                                                          actionButton(inputId = "pre.school.button",
                                                                       label = "Click to Add"),
                                                          
                                                          selectInput(inputId = "pre.school.name",
                                                                      label = "",
                                                                      choices = c("All" = "",levels(master1$school.name)),
                                                                      multiple = TRUE,
                                                                      selected = "All")
                                                 ),
                                                 
                                                 tabPanel("Degree select",
                                                          h3("What is the highest degree you are planning to get?"),
                                                          actionButton(inputId = "pre.degree.button",
                                                                       label = "Click to Add"),
                                                          
                                                          selectInput(inputId = "pre.degree.name",
                                                                      label = "",
                                                                      choices = c("All" = "", levels(master1$degree.name)),
                                                                      selected = "All", selectize = TRUE )
                                                 
                                                 ),
                                                 tabPanel("Occupation Category Select",
                                                          fluidRow(
                                                            h4("Which of these occupations would you consider?"),
                                                            actionButton(inputId = "pre.occupation.button",
                                                                         label = "Click to Add"),
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
                                                            
                                                            actionButton(inputId = "pre.curriculum.button",
                                                                         label = "Click to Add"),
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
                                                          actionButton(inputId = "pre.occ.button",
                                                                       label = "Click to Add"),
                                                          
                                                          selectInput(inputId = "pre.occ.name",
                                                                      label = "",
                                                                      choices = c("All" = "", levels(master1$occ.name)),
                                                                      multiple = TRUE,
                                                                      selected = "All" )
                                                  
                                                 ),
                                                 
                                                 tabPanel("Curriculum Name select",
                                                          
                                                          h3("What curriculum are you planning to study?"),
                                                          
                                                          actionButton(inputId = "pre.cip.button",
                                                                       label = "Click to Add"),
                                                          
                                                          selectInput(inputId = "pre.cip.name",
                                                                      label = "",
                                                                      choices = c("All" = "", levels(master1$cip.name)),
                                                                      multiple = TRUE,
                                                                      selected = "All" )
                                                 ),
                                                 
                                                 tabPanel("Salary Select",
                                                          fluidRow(
                                                            column(width = 12,
                                                                   numericInput(inputId = "pre.income",
                                                                                label = "How much income would you like to make in 10 to 15 years",
                                                                                value = 25000,
                                                                                min = 20000,
                                                                                max = 200000,
                                                                                step = 1000)),
                                                            hr(),
                                                            actionButton(inputId = "pre.income.button",
                                                                         label = "Click to Add")  
                                                          )
                                                 ),
                                                 tabPanel("Max Tuition Select",
                                                          fluidRow(
                                                            column(width = 12,
                                                                   numericInput(inputId = "pre.tuition",
                                                                                label = "How much tuition would you like to pay per years",
                                                                                value = 15000,
                                                                                min = 5000,
                                                                                max = 200000,
                                                                                step = 1000)),
                                                            hr(),
                                                            actionButton(inputId = "pre.tuition.button",
                                                                         label = "Click to Add")  
                                                          )
                                                 )
                                    )
                          ),
                           tabPanel("Scenerios",
                                    sidebarLayout(
                                      
                                      sidebarPanel(width = 3,
                                        
                                        selectInput(inputId = "nvs.school.name",
                                                    label= "School Name:",
                                                    choices = c("All" = "", levels(master1$school.name)),
                                                    multiple = TRUE,
                                                    selected = "All"),
                                      
                                        selectInput(inputId = "nvs.degree.name",
                                                    label = "Degree Name:",
                                                    choices = c("All" = "", levels(master1$degree.name)),
                                                    selected = "All",
                                                    selectize = TRUE),
                                      
                                        selectInput(inputId = "nvs.cip.cat",
                                                    label = "Curriculum Category:",
                                                    choices = c("All" = "", cip1$CIP_Category),
                                                    multiple = TRUE,
                                                    selected = "All"),
                                       
                                        selectInput(inputId = "nvs.occ.cat",
                                                    label = "Occupation Category:",
                                                    choices = c("All" = "", soc1$SOC_Cat_Name),
                                                    multiple = TRUE,
                                                    selected = "All"),
                                        
                                        selectInput(inputId = "nvs.cip.name",
                                                    label = "Curriculum Name:",
                                                    choices = c("All" = "", levels(master1$cip.name)),
                                                    multiple = TRUE,
                                                    selected = "All"),
                                        
                                        selectInput(inputId = "nvs.occ.name",
                                                    label = "Occupation Name:",
                                                    choices = c("All" = "", levels(master1$occ.name)),
                                                    multiple = TRUE,
                                                    selected = "All")
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
                                    )
                           )
                )

server <- function(input, output, session) {

  school.name_var <- reactive({
    if(is.null(input$nvs.school.name )) {
      levels(master1$school.name)} else {
        input$nvs.school.name
    }
  })
 
  degree.name_var <- reactive({
    if(is.null(input$nvs.degree.name )) {
      levels(master1$degree.name)} else {
        input$nvs.degree.name
      }
  })   

  occ.name_var <- reactive({
    if(is.null(input$nvs.occ.name )) {
      levels(master1$occ.name)} else {
        input$nvs.occ.name
      }
  })   

  cip.name_var <- reactive({
    if(is.null(input$nvs.cip.name )) {
      levels(master1$cip.name)} else {
        input$nvs.cip.name
      }
  })   

 table_var <- reactive({
   master1 %>% filter(school.name %in% school.name_var(), degree.name %in% degree.name_var(),
                      occ.name %in% occ.name_var(), cip.name %in% cip.name_var())
 })

  observe ( {  
 #   req(input$nvs.school.name)
    output$nvs.choice.table <- renderDataTable({
      DT::datatable(data = table_var(), 
                    options = list(pageLength = 10),selection = list(mode = "multiple"))
      })
  })
  new_var <- reactive({
    master1 %>% filter(school.name %in% school.name_var(), degree.name %in% degree.name_var(),
                       occ.name %in% occ.name_var(), cip.name %in% cip.name_var()) %>% select(school.name, degree.name, cip.name,
                                                                        occ.name,InStOff, X25p)
    })
  
  observeEvent(input$pre.school.button, {
    if(is.null(input$pre.school.name)) return()
      updateSelectInput(session, "nvs.school.name", "School Name:", selected = input$pre.school.name)
  })

  observeEvent(input$pre.degree.button, {
    if(is.null(input$pre.degree.name)) return()
    updateSelectInput(session, "nvs.degree.name", "Degree Name:", selected = input$pre.degree.name)
  })
  
  observeEvent(input$pre.occ.button, {
    if(is.null(input$pre.occ.name)) return()
    updateSelectInput(session, "nvs.occ.name", "Occupation Name:", selected = input$pre.occ.name)
  })

  observeEvent(input$pre.cip.button, {
    if(is.null(input$pre.cip.name)) return()
    updateSelectInput(session, "nvs.cip.name", "Curriculum Name:", selected = input$pre.cip.name)
  })  
    
   output$row.choice.table <- renderDataTable({ 
     DT::datatable(data = new_var()[input$nvs.choice.table_rows_selected,],
                   options = list(pageLength = 10), selection = list(mode = "none"))
  
   }) 

}
shinyApp(ui = ui, server = server)