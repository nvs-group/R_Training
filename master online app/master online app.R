## app.R ##
library(shiny)
library(shinydashboard)
library(markdown)
library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(DT)
library(tools)
selectedrowindex = 0
#Read in main data table from your local directory
master1 <- read.csv("C:/Users/romri/Documents/NVS/shinyapp/master1.txt")
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


ui <- dashboardPage(
  dashboardHeader(title = "College Planning"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("My Profile", tabName = "profile", icon = icon("user"),
               menuSubItem("Instructions", tabName = "instructions"),
               menuSubItem("School Select", tabName = "school"),
               menuSubItem("Degree Select", tabName = "degree"),
               menuSubItem("Occupation Select", tabName = "occupation"),
               menuSubItem("Curriculum Select", tabName = "curriculum"),
               menuSubItem("Salary Select", tabName = "salary"),
               menuSubItem("Tuition Select", tabName = "tuition")),
      menuItem("Scenerios", tabName = "Scenerios", icon = icon("tasks"),
               menuSubItem("Build Scenerios", tabName = "build"),
               menuSubItem("Compare Scenerios", tabName = "compare")),
      menuItem("Tools", tabName = "tools", icon = icon("toolbox")),
      menuItem("About", tabName = "about", icon = icon("info"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "profile",
              h2("Thanks")
      ),
      tabItem(tabName = "about",
              h2("Welcome and Hello")
      ),
      tabItem(tabName = "tools",
              h2("This is where tools go.")
      ),
      
      tabItem(tabName = "instructions",
              h2("Go through each tab and select the items you currently know about")
      ),
      
      tabItem(tabName = "school",
              h3("I know exactly where I want to go:"),
              box(
                width = 5,
                selectInput(inputId = "pre.school.name",
                            label = "",
                            choices = levels(master1$school.name),
                            multiple = TRUE)
              )
      ),
      tabItem(tabName = "degree",
              h3("What is the highest degree you are planning to get?"),
              box(
                width = 5,
                selectInput(inputId = "pre.degree.name",
                            label = "",
                            choices = levels(master1$degree.name),
                            multiple = TRUE)
              )
      ),
      
      tabItem(tabName = "occupation",
              h3("Which of these occupations would you consider?"),
              
              fluidRow(
                box(
                  width = 4,
                  checkboxGroupInput(inputId = "pre.occupation1",
                                     label = "",
                                     choices = soc_group1)),
                box(width = 4,
                    checkboxGroupInput(inputId = "pre.occupation2",
                                       label = "",
                                       choices = soc_group2))
              )
              
      ),
      tabItem(tabName = "curriculum",
              h3("Please check all curriculum that you are interested in"),
              fluidRow(
                box(width = 3,
                    checkboxGroupInput(inputId = "survey.Cip_Category1",
                                       label = "Categories",
                                       choices = cip_group1)
                ),
                box(width = 3,
                    checkboxGroupInput(inputId = "survey.Cip_Category2",
                                       label = "",
                                       choices = cip_group2)
                ),
                box(width = 3,
                    checkboxGroupInput(inputId = "survey.Cip_Category3",
                                       label = "",
                                       choices = cip_group3)
                ),
                box(width = 3,
                    checkboxGroupInput(inputId = "survey.Cip_Category4",
                                       label = "",
                                       choices = cip_group4)
                )
              )
      ),
      
      tabItem(tabName = "salary",
              h3("How much income would you like to make in 10 to 15 years?"),
              box(
                sliderInput(inputId = "pre.income",
                            label = "",
                            value = min(sort(unique(master1$X10p))),
                            min = min(sort(unique(master1$X10p))),
                            max = max(sort(unique(master1$X10p))))
              )
      ),
      tabItem(tabName = "tuition",
              h3("How much tuition would you like to pay per years?"),
              box(
                sliderInput(inputId = "pre.tuition",
                            label = "",
                            value = max(sort(unique(master1$InStOff))),
                            min = min(sort(unique(master1$InStOff))),
                            max = max(sort(unique(master1$InStOff))))
              )
      ),
      tabItem(tabName = "build",
              fluidRow(
                box(width = 3,
                    
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
                box(
                  width = 9,
                  DT::dataTableOutput(outputId = "nvs.choice.table")
                )
              )
              
      ),
      tabItem(tabName = "compare",
              fluidRow(
                box(
                  width = 3
                ),
                box(
                  width = 9,
                  DT::dataTableOutput(outputId = "row.choice.table")
                ))
      )
      
    )
  )
)

server <- function(input, output, session) {
  #Reactive variable that uses selected choices or full column if empty
  
  school.name_var <- reactive({
    if(is.null(input$nvs.school.name )) {
      unique(master1$school.name)} else {
        input$nvs.school.name
      }
  })
  #Reactive variable that uses selected choices or full column if empty 
  
  degree.name_var <- reactive({
    if(is.null(input$nvs.degree.name )) {
      unique(master1$degree.name)} else {
        input$nvs.degree.name
      }
  })
  #Reactive variable that uses selected choices or full column if empty
  
  occ.name_var <- reactive({
    if(is.null(input$nvs.occ.name)) {
      unique(master1$occ.name)} else {
        input$nvs.occ.name
      }
  })  
  #Reactive variable that uses selected choices or full column if empty
  
  cip.name_var <- reactive({
    if(is.null(input$nvs.cip.name)) {
      unique(master1$cip.name)} else {
        input$nvs.cip.name
      }
  })
  cip.cat_var <- reactive ({
    if(is.null(input$nvs.cip.cat)){
      unique(master1$cip.cat)} else {
        cip1$CIP_Code[cip1$CIP_Category %in% input$nvs.cip.cat]
      }
  })
  occ.cat_var <- reactive ({
    if(is.null(input$nvs.occ.cat)){
      unique(master1$soc.cat)} else {
        soc1$SOC_Code[soc1$SOC_Cat_Name %in% input$nvs.occ.cat]
      }
  })
  
  occ_var <- reactive ({
    soc1$SOC_Cat_Name[soc1$SOC_Cat_Name %in% input$pre.occupation1 | soc1$SOC_Cat_Name %in% input$pre.occupation2]
  })
  
  cip_var <- reactive ({
    cip1$CIP_Category[cip1$CIP_Category %in% input$survey.Cip_Category1 |cip1$CIP_Category %in% input$survey.Cip_Category2 |
                        cip1$CIP_Category %in% input$survey.Cip_Category3 | cip1$CIP_Category %in% input$survey.Cip_Category4]
    
  })
  #Filter for First Table
  table_var <- reactive({
    filter(master1, school.name %in% school.name_var(), degree.name %in% degree.name_var(),
           occ.name %in% occ.name_var(), cip.name %in% cip.name_var(),
           cip.cat %in% cip.cat_var(), 
           soc.cat %in% occ.cat_var())
  })
  #X10p >= input$nvs.income, InStOff <= input$nvs.tuition,
  observe ({
    req(cip_var())
    updateSelectInput(session, "nvs.cip.cat", "Curriculum Category:", selected = cip_var())
  })
  observeEvent (occ_var(),{
    updateSelectInput(session, "nvs.occ.cat", "Occupation Category:", selected = occ_var())
  }) 
  observe({
    if(is.null(input$nvs.school.name)) {
      updateSelectInput(session, "nvs.school.name", "School Name:", choices = unique(table_var()$school.name))  
    }
    if(is.null(input$nvs.degree.name)) {
      updateSelectInput(session, "nvs.degree.name", "Degree Name:", choices = unique(table_var()$degree.name))
    }
    if(is.null(input$nvs.occ.name)) {
      updateSelectInput(session, "nvs.occ.name", "Occupation Name:", choices = unique(table_var()$occ.name))
    }
    if(is.null(input$nvs.cip.name)) {
      updateSelectInput(session, "nvs.cip.name", "Curriculum Name:", choices = unique(table_var()$cip.name))
    }
    if(is.null(input$nvs.cip.cat)) {
      updateSelectInput(session, "nvs.cip.cat", "Curriculum Category:",
                        choices = cip1$CIP_Category[cip1$CIP_Code %in% table_var()$cip.cat])
    }
    if(is.null(input$nvs.occ.cat)){
      updateSelectInput(session, "nvs.occ.cat", "Occupation Category:", 
                        choices = soc1$SOC_Cat_Name[soc1$SOC_Code %in% table_var()$soc.cat])
    }
  })
  #First Table
  observe ( {  
    #   req(input$nvs.school.name)
    
    output$nvs.choice.table <- renderDataTable({
      DT::datatable(data = table_var(), 
                    options = list(pageLength = 10),selection = list(mode = "multiple"))
    })
  })
  #ObserveEvents go back here  
  
  
  # from school choice on preference page  
  observeEvent(input$pre.school.name, {
    updateSelectInput(session, "nvs.school.name", "School Name:", selected = input$pre.school.name)
  })
  #Import degree choice to scenerio from degree choice on preference page  
  observeEvent(input$pre.degree.name, {
    updateSelectInput(session, "nvs.degree.name", "Degree Name:", selected = input$pre.degree.name)
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
    table_var() %>% select(occ.name, school.name, degree.name, Experience, entry.degree, X50p)
  })
  #Second Table after choosing rows    
  output$row.choice.table <- renderDataTable({ 
    DT::datatable(data = new_var()[input$nvs.choice.table_rows_selected,],
                  options = list(pageLength = 10), selection = list(mode = "none"))
  })
}
shinyApp(ui = ui, server = server)