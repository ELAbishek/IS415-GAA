# REEEEEEEEEÈEEEEEEEEÉEEEEEEEEEËEEEEEEEEEEEEEEEEEEEEEEEÊËEEEEEEEEEEĘEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEĘĖÊÈEEEE

# Imports
pacman::p_load(shiny, sf, tidyverse, tmap)

# Load Data
sgpools <- read_csv("data/aspatial/SGPools_svy21.csv")
sgpools_sf <- st_as_sf(sgpools, 
                       coords = c("XCOORD", "YCOORD"),
                       crs = 3414)

ui <- fluidPage(
  titlePanel("Static Proportional Symbol Map"),
  
  sidebarLayout(
    sidebarPanel("Side bar panel"),
    
    mainPanel(
      "View display",
      plotOutput("mapPlot") # Careful with the naming, must be unique
    )
  )
)

server <- function(input, output) {
  output$mapPlot <- renderPlot({
    tm_shape(sgpools_sf) +
      tm_bubbles(col = "OUTLET TYPE",
                 size = "Gp1Gp2 Winnings",
                 border.col="black",
                 border.lwd = 0.5)
  })
}

shinyApp(ui = ui, server = server)