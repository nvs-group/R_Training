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


series_title_choices <- levels(oe.series$series_title)
area_name_choices <- levels(oe.area$area_name)

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
            selectInput("area",
                        "Select Area",
                        choices = area_name_choices
                        ),
            selectInput("series_title",
                        "Select Series",
                        choices = series_title_choices
                        ),
            sliderInput("years",
                        "Which years:",
                        min = min(oe.data$year),
                        max = max(oe.data$year),
                        step = 1
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
