# code from https://community.rstudio.com/t/applying-multiple-filters-for-a-reactive-table/4932
# on 20190530
# uploaded to shinyapps.io - https://chapmjs.shinyapps.io/MasterThreeFilters/


library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(DT)
library(tools)


# import data
# master1 <- read.csv("https://www.dropbox.com/s/fgty42qwpkzudwz/master1.txt?dl=1")
master1 <- read.csv("master1.txt")


# mtcarsDf <- mtcars %>%
#   mutate(car_name = row.names(mtcars)) %>%
#   select(car_name, cyl, mpg, gear)

# variable names to lower case
names(master1) <- tolower(names(master1))

# Variables to keep: occ.name, entry.degree, experience, school.name, degree.name
# select on the following columns(/variables) from master1
master2 <- master1 %>%
  select(c("occ.name","entry.degree","experience","school.name","degree.name","x50p"))

# rename X50p column to salary
colnames(master2)[colnames(master2)=="x50p"] <- "salary"



ui <- fluidPage(
  titlePanel("College and Education Filter"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("school.name",
                "School Name(s):",
                choices = master2$school.name %>% unique() %>% sort(),
                selected = ""
                ),
      selectInput("entry.degree",
                  "Your highest degree:",
                  choices = master2$entry.degree %>% unique() %>% sort(),
                  selected = ""
                  ),
      selectInput("experience",
                  "Number of years of work experience you will have to begin career:",
                  choices = master2$experience %>% unique() %>% sort(),
                  selected = ""
                  )
      # selectInput("occ.name",
      #             "Possible Occupation(s):",
      #             choices = master2$occ.name %>% unique() %>% sort(),
      #             selected = ""
      #             )
    ),
    mainPanel(
      DT::dataTableOutput("masterDf")
      #plotOutput("plot")
    )
  )
)

server <- function(input, output, session) {
  
  reactiveDf <- reactive({
    if (input$school.name == "" &
        input$entry.degree == "" &
        input$experience == "" # & input$occ.name == ""
        ) {
      return(master2)
    }
    
    if (input$school.name != "") {
      master2 <- master2 %>%
        filter(
          #grepl(input$school.name, school.name, ignore.case = TRUE)
          school.name == input$school.name
        )
      
    }
    
    if (input$entry.degree != "") {
      master2 <- master2 %>%
        filter(
          entry.degree == input$entry.degree
        )
      
    }
    
    if (input$experience != "") {
      master2 <- master2 %>%
        filter(
          experience == input$experience
        )
      
    }
    
    # if (input$occ.name != "") {
    #   master2 <- master2 %>%
    #     filter(
    #       occ.name == input$occ.name
    #     )
    #   
    # }
    
    return(master2)
  })
  
  output$masterDf <- DT::renderDataTable({
    reactiveDf()
  })
  
}

shinyApp(ui, server)