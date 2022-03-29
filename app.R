library(shiny)
library(MITREShiny)

# Define UI for application that draws a histogram
ui <- MITREnavbarPage(
    "Gentrification, Redlining, and Health",
    tabPanel("Outline",
             includeHTML("notebooks/Outline.nb.html")),
    tabPanel("Methods",
             includeHTML("notebooks/Methods.nb.html"))
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {


}

# Run the application 
shinyApp(ui = ui, server = server)
