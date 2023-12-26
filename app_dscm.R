
library(shiny)
library(leaflet)
library(readxl)
library(ggplot2)

ui <- fluidPage(

  
    titlePanel("Warehouse and Customer locations Dashboard"),

    sidebarLayout(
        sidebarPanel(
            fileInput('file1', 'Choose xlsx file with Warehouse data',
                      accept = c(".xlsx")),
            fileInput('file2', 'Choose xlsx file with Customer data',
                      accept = c(".xlsx")),
            fileInput('file3', 'Choose xlsx file with Supplier data',
                      accept = c(".xlsx")),
            plotOutput("plot2"),
            plotOutput("plot1")
        ),

        mainPanel(
            leafletOutput("mymap"),
            fluidRow(
              splitLayout(cellWidths = c("33%", "33%", "33%"), tableOutput('file1'), tableOutput('file2'), tableOutput('file3'))
            )
        )
    )
)


server <- function(input, output, session) {

  output$file2 <- renderTable({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    read_excel(inFile$datapath)
  })
  
  output$file1 <- renderTable({
    inFile2 <- input$file2
   if (is.null(inFile2))
      return(NULL)
    read_excel(inFile2$datapath)
  })
  
  output$file3 <- renderTable({
    inFile3 <- input$file3
    if (is.null(inFile3))
      return(NULL)
    read_excel(inFile3$datapath)
  })
  
  m <- leaflet() %>%
    setView(lng = 60, lat = 55, zoom = 4) %>%
    addProviderTiles(providers$CartoDB.Positron)
  output$mymap <- renderLeaflet(m) 
  
  observe({
    req(c(input$file1, input$file2, input$file3))
    inFile <- input$file1
    inFile2 <- input$file2
    inFile3 <- input$file3
    if (is.null(inFile))
      return(NULL)
    if (is.null(inFile2))
      return(NULL)
    if (is.null(inFile3))
      return(NULL)
    
    exceldata <- read_excel(inFile$datapath)
    df = data.frame(exceldata)
    exceldata2 <- read_excel(inFile2$datapath)
    df2 = data.frame(exceldata2)
    exceldata3 <- read_excel(inFile3$datapath)
    df3 = data.frame(exceldata3)
    
    proxy <- leafletProxy("mymap", data = df, session)
    proxy %>% addProviderTiles(providers$Esri.NatGeoWorldMap, group = "NatGeo")
    proxy %>% addProviderTiles(providers$OpenTopoMap, group = "TopoMap")
    proxy %>% addProviderTiles(providers$Esri.WorldImagery, group = "Satellite")
    proxy %>% addProviderTiles(providers$OpenRailwayMap, group = "Rail")
    proxy %>% addProviderTiles("NASAGIBS.ViirsEarthAtNight2012", group = "NASA")
    
    proxy %>% addCircles(df2$long, df2$lat, radius = (df2$demand)*2000, popup = df2$ID, color = "darkgreen")
    proxy %>% addCircleMarkers(df2$long, df2$lat, popup = df2$ID, color = "green", fillColor = "green", radius = 5, fillOpacity = 1)
    proxy %>% addMarkers(df$long, df$lat, popup = df$ID)
    proxy %>% addCircleMarkers(df3$long, df3$lat, popup = df3$ID, color = "red", fillColor = "red", radius = 5, fillOpacity = 1)
    proxy %>%addLayersControl(baseGroups = c("Drawing", "Sattelite", "NatGeo", "TopoMap", "NASA", "Rail"),
                              #overlayGroups = c("1","2","3"),
                              options = layersControlOptions(collapsed = FALSE))
    
    output$plot2 <- renderPlot(
      ggplot(df2) + 
        aes(x = reorder(ID,demand), y = demand) +
        geom_col(fill = "green") +
        labs(
          x = "ID",
          y = "Demand",
          title = "Demand distribution",
          subtitle = "6 stations",
          caption = "data from"
        ) +
        coord_flip() +
        theme_minimal()
    )
    
    output$plot1 <- renderPlot(
      ggplot(df2) +
        aes(x = cost, y = ID) +
        geom_col(fill = "pink") +
        labs(
          x = "Cost",
          y = "ID",
          title = "Transportation cost",
          caption = "data from"
        ) +
        theme_minimal()
    )
    
    
      
  })
  
}

shinyApp(ui = ui, server = server)
