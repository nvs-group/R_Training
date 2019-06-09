#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)
library(shiny)


# import oe.area
# oe.area <- read.delim("https://download.bls.gov/pub/time.series/oe/oe.area")

# import oe.series
# oe.series <- read.delim("https://download.bls.gov/pub/time.series/oe/oe.series")

# import oe.areatype
# oe.areatype <- read.delim("https://download.bls.gov/pub/time.series/oe/oe.areatype")

# import oe.datatype
# oe.datatype <- read.delim("https://download.bls.gov/pub/time.series/oe/oe.datatype")


series_title_choices <- unique(oe.datatype$datatype_name)


# area_name_choices <- unique(filter(oe.areatype, areatype_code == "M"))
area_name_choices <- oe.area %>% filter(areatype_code == "M") %>% select(area_name,area_code)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Download BLS Data"),

    # Sidebar 
    sidebarLayout(
        sidebarPanel(
            # selectInput("area",
            #             "Select Area Type",
            #             choices = levels(oe.areatype$areatype_name)
            #             ),
            selectizeInput("area",
                        "Select Area",
                        choices = area_name_choices,
                        multiple = TRUE
                        ),
            selectInput("series_title",
                        "Select Series",
                        choices = series_title_choices,
                        multiple = TRUE
                        ),
            sliderInput("years",
                        "Which years:",
                        min = 1990,
                        max = 2018,
                        step = 1,
                        value = c("2012","2018"),
                        sep = ""
                        )
                    ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
