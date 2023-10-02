library(shiny)
library(bs4Dash)
library(tidyverse)
library(DT)
library(plotly)

# Load the data
data <- read.csv("website_ab_test.csv")

# Define the UI function for the Shiny app
ui <- bs4DashPage(
  header = bs4DashNavbar(
    title = "Website A/B Test",
    rightSidebarIcon = "bars"
  ),
  sidebar = bs4DashSidebar(
    bs4SidebarMenuItem(
      tabName = "home",
      text = "Home",
      icon = shiny::icon("home")
    ),
    bs4SidebarMenuItem(
      tabName = "data",
      text = "Data",
      icon = shiny::icon("table")
    ),
    bs4SidebarMenuItem(
      tabName = "analysis",
      text = "Analysis",
      icon = shiny::icon("chart-bar")
    )
  ),
  body = bs4DashBody(
    bs4TabItems(
      bs4TabItem(
        tabName = "home",
        bs4InfoBox(
          title = "Welcome to the Website A/B Test",
          subtitle = "This is a simple Shiny app to demonstrate the use of the bs4Dash package.",
          icon = shiny::icon("chart-line")
        )
      ),
      bs4TabItem(
        tabName = "data",
        dataTableOutput("data_table")
        
      ),
      bs4TabItem(
        tabName = "analysis",
        bs4InfoBox(
          title = "Analysis",
          color = "primary",
          #solidHeader = TRUE,
          #collapsible = TRUE,
          width = 12,
          plotlyOutput("plot"),
          verbatimTextOutput("summary"),
          plotOutput("ctr_plot"),  # Display Click Through Rate plot
          plotOutput("br_plot"),   # Display Bounce Rate plot
          plotOutput("sd_plot"),   # Display Session Duration plot
          verbatimTextOutput("ttest_ctrr"),  # Display Click Through Rate t-test results
          verbatimTextOutput("ttest_crr"),   # Display Conversion Rate t-test results
          verbatimTextOutput("ttest_brr"),   # Display Bounce Rate t-test results
          plotOutput("hist_brr")
        )
      )
    )
  )
)

# Define the server function for the Shiny app
server <- function(input, output) {
  #data table
  output$data_table <- DT::renderDataTable({
    datatable(
      data,
      options = list(
        columnDefs = list(list(className='dt-center', targets ='_all'))
      ),
      rownames = FALSE
    )
  })
  # Calculate the CTR for each theme
  light_theme_ctr <- mean(data[data$Theme == 'Light Theme', 'Click Through Rate'])
  dark_theme_ctr <- mean(data[data$Theme == 'Dark Theme', 'Click Through Rate'])
  
  # Plot the CTR for each theme
  output$ctr_plot <- renderPlot({
    bar_colors <- c('red', 'green')
    bar_heights <- c(light_theme_ctr, dark_theme_ctr)
    bar_labels <- c('Light Theme', 'Dark Theme')
    
    barplot(bar_heights, names.arg = bar_labels, col = bar_colors,
            main = 'Click Through Rate by Theme', xlab = 'Theme', ylab = 'Click Through Rate')
  })
  
  # Calculate the Bounce Rate for each theme
  light_theme_br <- mean(data[data$Theme == 'Light Theme', 'Bounce Rate'])
  dark_theme_br <- mean(data[data$Theme == 'Dark Theme', 'Bounce Rate'])
  
  # Plot the Bounce Rate for each theme
  output$br_plot <- renderPlot({
    bar_colors <- c('blue', 'purple')
    bar_heights <- c(light_theme_br, dark_theme_br)
    bar_labels <- c('Light Theme', 'Dark Theme')
    
    barplot(bar_heights, names.arg = bar_labels, col = bar_colors,
            main = 'Average Bounce Rate by Theme', xlab = 'Theme', ylab = 'Bounce Rate')
  })
  
  # Calculate the Session Duration for each theme
  light_theme_sd <- mean(data[data$Theme == 'Light Theme', 'Session Duration'])
  dark_theme_sd <- mean(data[data$Theme == 'Dark Theme', 'Session Duration'])
  
  # Plot the Session Duration for each theme
  output$sd_plot <- renderPlot({
    bar_colors <- c('orange', 'green')
    bar_heights <- c(light_theme_sd, dark_theme_sd)
    bar_labels <- c('Light Theme', 'Dark Theme')
    
    barplot(bar_heights, names.arg = bar_labels, col = bar_colors,
            main = 'Session Duration by Theme', xlab = 'Theme', ylab = 'Session Duration')
  })
  
  # Perform a t-test to compare two themes for Click Through Rate
  light_theme_ctrr <- data[data$Theme == 'Light Theme', 'Click Through Rate']
  dark_theme_ctrr <- data[data$Theme == 'Dark Theme', 'Click Through Rate']
  
  t_test_result_ctrr <- t.test(light_theme_ctrr, dark_theme_ctrr)
  
  output$ttest_ctrr <- renderPrint({
    cat('t-statistic:', t_test_result_ctrr$statistic, '\n')
    cat('p-value:', t_test_result_ctrr$p.value, '\n')
  })
  
  # Perform a t-test to compare two themes for Conversion Rate
  light_theme_crr <- data[data$Theme == 'Light Theme', 'Conversion Rate']
  dark_theme_crr <- data[data$Theme == 'Dark Theme', 'Conversion Rate']
  
  t_test_result_crr <- t.test(light_theme_crr, dark_theme_crr)
  
  output$ttest_crr <- renderPrint({
    cat('t-statistic:', t_test_result_crr$statistic, '\n')
    cat('p-value:', t_test_result_crr$p.value, '\n')
  })
  
  # Perform a t-test to compare two themes for Bounce Rate
  light_theme_brr <- data[data$Theme == 'Light Theme', 'Bounce Rate']
  dark_theme_brr <- data[data$Theme == 'Dark Theme', 'Bounce Rate']
  
  t_test_result_brr <- t.test(light_theme_brr, dark_theme_brr)
  
  output$ttest_brr <- renderPrint({
    cat('t-statistic:', t_test_result_brr$statistic, '\n')
    cat('p-value:', t_test_result_brr$p.value, '\n')
  })
  
  # Plot histograms for Bounce Rate
  output$hist_brr <- renderPlot({
    par(mfrow = c(1, 2))
    hist(light_theme_brr, col = 'orange', alpha = 0.5,
         main = 'Bounce Rate Histogram by Theme (Light Theme)')
    hist(dark_theme_brr, col = 'green', alpha = 0.5,
         main = 'Bounce Rate Histogram by Theme (Dark Theme)')
  })
}

# Run the Shiny app
shinyApp(ui, server)
