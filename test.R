library(shiny)
library(rhino)

# Create a dataset
df <- data.frame(
  Year = c(2020),
  Month = c("Compare to Previous Year"),
  SALES_REVENUJE = c(66060),
  PRODUCTION_COSTS = c(5255),
  ACTIVE_USERS = c(14472),
  OPENED_COMPLAINTS = c(915),
  Country = c("United States", "China"),
  stringsAsFactors = FALSE
)

# Create a Rhino app
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Country:", choices = df$Country, selected = "United States")
    ),
    mainPanel(
      tabItems(
        tabItem("Overview",
                sidebarLayout(
                  sidebarPanel(
                    sliderInput("year", "Year:", 2019, 2020, value = 2020)
                  ),
                  mainPanel(
                    box(
                      title = "Sales Revenue by Country",
                      width = 12,
                      plotOutput("sales_revenue_by_country", height = 300)
                    ),
                    box(
                      title = "Trend in Sales Revenue over Time",
                      width = 12,
                      plotOutput("trend_in_sales_revenue")
                    )
                  )
                )
        ),
        tabItem("Complaints",
                leafletOutput("map_of_opened_complaints", height = 400)
        )
      )
    )
  )
)

server <- function(input, output) {
  # Calculate the sales revenue by country
  output$sales_revenue_by_country <- renderPlot({
    ggplot(df, aes(x = Country, y = SALES_REVENUJE)) +
      geom_bar(stat = "sum", fill = "steelblue") +
      labs(title = "Sales Revenue by Country", x = "Country", y = "Sales Revenue ($)")
  })
  
  # Calculate the trend in sales revenue over time
  output$trend_in_sales_revenue <- renderPlot({
    ggplot(df, aes(x = Year, y = SALES_REVENUJE)) +
      geom_line(color = "steelblue") +
      labs(title = "Trend in Sales Revenue over Time", x = "Year", y = "Sales Revenue ($)")
  })
  
  # Filter the data by country
  filtered_df <- reactive({
    subset(df, Country == input$country)
  })
  
  # Create the map of opened complaints
  output$map_of_opened_complaints <- renderLeaflet({
    leaflet(filtered_df()) %>%
      addTiles() %>%
      addCircles(
        lng = filtered_df()$Longitude,
        lat = filtered_df()$Latitude,
        radius = filtered_df()$OPENED_COMPLAINTS / 100,
        color = "steelblue",
        fillOpacity = 0.5
      ) %>%
      addLegend(
        position = "bottomleft",
        title = "Opened Complaints",
        values = c(100, 200, 300, 400, 500),
        colors = c("yellow", "orange", "red", "purple", "black")
      )
  })
}

# Run the app
shinyApp(ui, server)
