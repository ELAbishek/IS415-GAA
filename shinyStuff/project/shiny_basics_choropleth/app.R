# Imports
pacman::p_load(shiny, sf, tidyverse, tmap)

# Load Data
popdata <- read_csv("data/aspatial/respopagesextod2011to2020.csv")
mpsz <- st_read(dsn = "data/geospatial", 
                layer = "MP14_SUBZONE_WEB_PL")

#Data wrangling
popdata2020 <- popdata %>%
  filter(Time == 2019) %>%
  group_by(PA, SZ, AG) %>%
  summarise(`POP` = sum(`Pop`)) %>%
  ungroup()%>%
  pivot_wider(names_from=AG, 
              values_from=POP) %>%
  mutate(YOUNG = rowSums(.[3:6])
         +rowSums(.[12])) %>%
  mutate(`ECONOMY ACTIVE` = rowSums(.[7:11])+
           rowSums(.[13:15]))%>%
  mutate(`AGED`=rowSums(.[16:21])) %>%
  mutate(`TOTAL`=rowSums(.[3:21])) %>%  
  mutate(`DEPENDENCY` = (`YOUNG` + `AGED`)
         /`ECONOMY ACTIVE`) %>%
  select(`PA`, `SZ`, `YOUNG`, 
         `ECONOMY ACTIVE`, `AGED`, 
         `TOTAL`, `DEPENDENCY`)

#joining data
popdata2020 <- popdata2020 %>%
  mutate_at(.vars = vars(PA, SZ), 
            .funs = funs(toupper)) %>%
  filter(`ECONOMY ACTIVE` > 0)

mpsz_pop2020 <- left_join(mpsz, popdata2020,
                          by = c("SUBZONE_N" = "SZ"))

#Chloropleth

#UI
ui <- fluidPage(
  titlePanel("Interactive Proportional Symbol Map"),
  
  sidebarLayout(
    sidebarPanel("Side bar panel",
                 selectInput(inputId = "type",
                             label = "Mapping_variable",
                             choices = c( "YOUNG", 
                                         "ECONOMY ACTIVE", "AGED", 
                                         "TOTAL", "DEPENDENCY"),
                             "YOUNG",
                             multiple = FALSE),
                 selectInput(inputId = "method",
                             label = "method",
                             choices = c("cat",
                                         "fixed",
                                         "sd",
                                         "equal",
                                         "pretty",
                                         "quantile",
                                         "kmeans",
                                         "hclust",
                                         "bclust",
                                         "fisher",
                                         "jenks",
                                         "dpih",
                                         "headtails",
                                         "log10_pretty"),
                             "jenks",
                             multiple = FALSE),
                 selectInput(inputId = "clr",
                             label = "Map colour",
                             choices = c("YlOrRd",
                                         "YlOrBr",
                                         "YlGnBu",
                                         "YlGn",
                                         "Reds",
                                         "RdPu",
                                         "Purples",
                                         "PuBuGn",
                                         "PuBu",
                                         "OrRd",
                                         "Oranges",
                                         "Greys",
                                         "Greens",
                                         "GnBu",
                                         "BuPu",
                                         "BuGn",
                                         "Blues"),
                             "Branch",
                             multiple = FALSE),
                 sliderInput(inputId = "slider",
                             label = "Number of classes",
                             min = 6,
                             max = 12,
                             value = 8),
                 checkboxInput(inputId = "showData",
                               label = "Show data table",
                               value = TRUE)
    ),
    
    mainPanel(
      "View display",
      tmapOutput("mapPlot"), # Careful with the naming, must be unique
      DT::dataTableOutput(outputId = "aTable")
    )
  )
)

server <- function(input, output) {
#  dataset <- reactive({
#    popdata2020 %>%
#      filter(`OUTLET TYPE` %in% input$type) %>%
#      filter(`Gp1Gp2 Winnings` >= input$winning)
#  })
#Plotting here
  output$mapPlot <- renderTmap({
    tmap_options(check.and.fix = TRUE)
    tmap_mode("view")
    tm_shape(mpsz_pop2020)+
      tm_fill(input$type,
              n = input$slider,
              palette = input$clr,
              style = input$method) +
      tm_borders(lwd = 0.1,  alpha = 1) +
      tm_view(set.zoom.limits = c(10,14))
    
  })
  output$aTable <- DT::renderDataTable({
    if(input$showData) {
 #     DT::datatable(data = dataset() %>%
 #                     select(1:4),
 #                   options = list(pageLength = 10),
  #                  rownames = FALSE)
    }
  })
}

shinyApp(ui = ui, server = server)