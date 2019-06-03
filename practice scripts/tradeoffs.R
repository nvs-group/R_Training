#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Trade Offs - how choosing a phone, a vehicle, and travel relate to a future job"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("phone",
                        "How nice of a phone do you want to have?",
                        choices = c(
                            "Latest and greatest iPhone" = "latest",
                            "Not quite the latest and greatest iPhone" = "notquite",
                            "Average phone" = "average",
                            "Below average phone" = "belowaverage"
                        ),
                        selected = "latest"
            ),
            selectInput("vehicle",
                        "How nice of a vehicle would you like to drive?",
                        choices = c(
                            "Latest and greatest vehicle" = "latest", 
                            "Not quite the latest and greatest vehicle" = "notquite",
                            "Average vehicle" = "average",
                            "Below average vehicle" = "belowaverage"
                        ),
                        selected = "latest"
            ),
            selectInput("travel",
                        "How much travel would you like to do?",
                        choices = c(
                            "International travel at least yearly" = "alot", 
                            "One international trip every 2-10 years would be nice" = "quiteabit",
                            "One international trip every 10-20 years" = "rarely",
                            "Mostly domestic travel, not too frequently" = "domestic",
                            "No, I don't need to travel" = "nada"),
                        selected = "alot"
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
