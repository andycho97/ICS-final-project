require(shiny)
require(rworldmap)
require(plotly)

ui <- fluidPage(

  fluidRow(
    column(9,
      plotlyOutput("plot")
    ),
    column(2,
      tableOutput("legend")
    )
  ),
  
  wellPanel(
    htmlOutput("city"),
    textOutput("click")
  ), 
  
  wellPanel(
    HTML("<h4>Event</h4>"),
    textOutput("click2")
  ), 
  
  wellPanel(
    HTML("<h4>Impact</h4>"),
    textOutput("click3")
  ), 
  
  wellPanel(
    uiOutput("pic")
  )
)

server <- function(input, output, session) {

  #require(readxl)
  #ames_cities <- read_excel("ames_cities.xlsx")
  
  df <- reactive(ames_cities)
  leg <- matrix(c("Purple", "Yellow", "Green", "North Korean Nuclear Program", "North Korean Defectors and Refugees", "International Human Rights"),3,2)
  colnames(leg) <- c("Color", "Topic")
  
  output$city <- renderUI({
    d2 <- event_data("plotly_click")
    if (is.null(d2)) {
      HTML("<font size = 4><b>", "Choose a City from the Map", "</b></font>")
    }
    else if ((d2[2]$pointNumber + 1) == 1) {
      HTML(paste("<font size = 4><b>", "Seoul, South Korea","</b></font>"))
    }
    else if ((d2[2]$pointNumber + 1) == 2) {
      HTML(paste("<font size = 4><b>", "Pyongyang, North Korea","</b></font>"))
    }
    else if ((d2[2]$pointNumber + 1) == 3) {
      HTML(paste("<font size = 4><b>", "Osaka, Japan","</b></font>"))
    }
    else if ((d2[2]$pointNumber + 1) == 4) {
      HTML(paste("<font size = 4><b>", "New York, USA","</b></font>"))
    }
    else if ((d2[2]$pointNumber + 1) == 5) {
      HTML(paste("<font size = 4><b>", "Washington, DC, USA","</b></font>"))
    }
    else if ((d2[2]$pointNumber + 1) == 6) {
      HTML(paste("<font size = 4><b>", "Bangkok, Thailand","</b></font>"))
    }
    else if ((d2[2]$pointNumber + 1) == 7) {
      HTML(paste("<font size = 4><b>", "Pyeongchang, South Korea","</b></font>"))
    }
    else if ((d2[2]$pointNumber + 1) == 8) {
      HTML(paste("<font size = 4><b>", "Beijing, China","</b></font>"))
    }
    else if ((d2[2]$pointNumber + 1) == 9) {
      HTML(paste("<font size = 4><b>", "London, UK","</b></font>"))
    }
    else if ((d2[2]$pointNumber + 1) == 10) {
      HTML(paste("<font size = 4><b>", "Toronto, Canada","</b></font>"))
    }
    else if ((d2[2]$pointNumber + 1) == 11) {
      HTML(paste("<font size = 4><b>", "DMZ, Korea","</b></font>"))
    }
    else if ((d2[2]$pointNumber + 1) == 12) {
      HTML(paste("<font size = 4><b>", "Manila, Philippines","</b></font>"))
    }
    else if ((d2[2]$pointNumber + 1) == 13) {
      HTML(paste("<font size = 4><b>", "Ulaanbaatar, Mongolia","</b></font>"))
    }
    else if ((d2[2]$pointNumber + 1) == 14) {
      HTML(paste("<font size = 4><b>", "Dandong, China","</b></font>"))
    }
    else if ((d2[2]$pointNumber + 1) == 15) {
      HTML(paste("<font size = 4><b>", "Uijongbu, South Korea","</b></font>"))
    }
    else if ((d2[2]$pointNumber + 1) == 16) {
      HTML(paste("<font size = 4><b>", "Camp 14, North Korea","</b></font>"))
    }
    else if ((d2[2]$pointNumber + 1) == 17) {
      HTML(paste("<font size = 4><b>", "Vientiane, Laos","</b></font>"))
    }
  })
  
  output$legend <- renderTable(
    leg
  )
  
  output$plot <- renderPlotly({
    
    plot_ly(ames_cities) %>%
      
      add_trace(z = ames_cities$hover, lat = ames_cities$lat, lon = ames_cities$long, text = ames_cities$name,
            color = ames_cities$capital, type = 'scattergeo', locationmode = 'ISO-3', mode = 'markers',
            marker = list(size = 8, opacity = 0.8, symbol = 'circle')) %>%

      hide_colorbar() %>%
      
      layout(
           title = "ICS Final Project",
           legend = list(orientation = "v",
                         xanchor = "right"),
           geo = list(
               scope = 'world',
               showframe = FALSE,
               showcoastlines = TRUE,
               projection = list(type = 'natural earth'),
               showland = TRUE,
               landcolor = toRGB("beige"),
               showocean = TRUE,
               showlakes = TRUE,
               showrivers = FALSE,
               showcountries = TRUE
            )
       )
  })
  

  output$click <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) {
      cat("")
    }
    else {
      cat(strsplit(d[3]$z,"; ")[[1]][1])
    }
  })
  
  output$click2 <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) {
      cat("")
    }
    else {
      cat(strsplit(d[3]$z,"; ")[[1]][2])
    }
  })
  
  output$click3 <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) {
      cat("")
    }
    else {
      cat(strsplit(d[3]$z,"; ")[[1]][3])
    }
  })
  
  output$click4 <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) {
      cat("")
    }
    else {
      cat(strsplit(d[3]$z,"; ")[[1]][4])
    }
  })
  
  output$pic <- renderUI({
    d2 <- event_data("plotly_click")
    if (is.null(d2)) {
      NULL
    }
    else if ((d2[2]$pointNumber + 1) == 1) {
      img(src = df()[1,8], width="800", height="600")
    }
    else if ((d2[2]$pointNumber + 1) == 2) {
      img(src = df()[2,8], width="800", height="600")
    }
    else if ((d2[2]$pointNumber + 1) == 3) {
      img(src = df()[3,8], width="800", height="600")
    }
    else if ((d2[2]$pointNumber + 1) == 4) {
      img(src = df()[4,8], width="800", height="600")
    }
    else if ((d2[2]$pointNumber + 1) == 5) {
      img(src = df()[5,8], width="800", height="600")
    }
    else if ((d2[2]$pointNumber + 1) == 6) {
      img(src = df()[6,8], width="800", height="600")
    }
    else if ((d2[2]$pointNumber + 1) == 7) {
      img(src = df()[7,8], width="800", height="600")
    }
    else if ((d2[2]$pointNumber + 1) == 8) {
      img(src = df()[8,8], width="800", height="600")
    }
    else if ((d2[2]$pointNumber + 1) == 9) {
      img(src = df()[9,8], width="800", height="600")
    }
    else if ((d2[2]$pointNumber + 1) == 10) {
      img(src = df()[10,8], width="800", height="600")
    }
    else if ((d2[2]$pointNumber + 1) == 11) {
      img(src = df()[11,8], width="800", height="600")
    }
    else if ((d2[2]$pointNumber + 1) == 12) {
      img(src = df()[12,8], width="800", height="600")
    }
    else if ((d2[2]$pointNumber + 1) == 13) {
      img(src = df()[13,8], width="800", height="600")
    }
    else if ((d2[2]$pointNumber + 1) == 14) {
      img(src = df()[14,8], width="800", height="600")
    }
    else if ((d2[2]$pointNumber + 1) == 15) {
      img(src = df()[15,8], width="800", height="600")
    }
    else if ((d2[2]$pointNumber + 1) == 16) {
      img(src = df()[16,8], width="800", height="600")
    }
    else if ((d2[2]$pointNumber + 1) == 17) {
      img(src = df()[17,8], width="800", height="600")
    }
  })

}

shinyApp(ui = ui, server = server)

