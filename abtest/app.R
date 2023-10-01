library(shiny)
library(bs4Dash)
library(tidyverse)

# Load the data
data <- read.csv("website_ab_test.csv")

# Define the UI function for the Shiny app
ui <- bs4DashPage(
  header = bs4DashNavbar(
    title = "Website A/B Test",
    rightSidebarIcon = "bars"
  ),
  sidebar = bs4DashSidebar(
    bs4DashSidebarItem(
      tabName = "home",
      text = "Home",
      icon = "home"
    ),
    bs4DashSidebarItem(
      tabName = "data",
      text = "Data",
      icon = "table"
    ),
    bs4DashSidebarItem(
      tabName = "analysis",
      text = "Analysis",
      icon = "chart-bar"
    )
  ),
  body = bs4DashBody(
    bs4DashTabItems(
      bs4DashTabItem(
        tabName = "home",
        bs4DashInfoBox(
          title = "Welcome to the Website A/B Test",
          text = "This is a simple Shiny app to demonstrate the use of the bs4Dash package.",
          icon = "chart-line"
        )
      ),
      bs4DashTabItem(
        tabName = "data",
        bs4DashDataTable(
          data = data,
          columns = list(
            Theme = "Theme",
            `Click Through Rate` = "Click Through Rate",
            `Conversion Rate` = "Conversion Rate",
            `Bounce Rate` = "Bounce Rate",
            `Scroll_Depth` = "Scroll Depth",
            Age = "Age",
            Location = "Location",
            `Session_Duration` = "Session Duration",
            Purchases = "Purchases",
            `Added_to_Cart` = "Added to Cart"
          )
        )
      ),
      bs4DashTabItem(
        tabName = "analysis",
        bs4DashBox(
          title = "Analysis",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 12,
          plotlyOutput("plot")
        )
      )
    )
  )
)

# Define the server function for the Shiny app
server <- function(input, output) {
  # Data visualization
  output$plot <- renderPlotly({
    data %>%
      ggplot(aes(x = Theme, y = `Click Through Rate`, fill = Theme)) +
      geom_boxplot() +
      labs(title = "Click Through Rate by Theme")
  })
  
  # Statistical analysis
  output$summary <- renderPrint({
    t.test(`Click Through Rate` ~ Theme, data = data)
  })
}

# Run the Shiny app
shinyApp(ui, server)
