library(shiny)
library(bs4Dash)
library(tidyverse)
library(DT)
library(waiter)
library(highcharter)
library(reactable)

# Define the UI function for the Shiny app
ui <- dashboardPage(
  dashboardHeader(title = dashboardBrand(
    title = "Theme Analysis",
    color = "gray-dark",
    image = "theme_logo.png")),
  dashboardSidebar(
    skin ="light",
    status = "gray-dark",
    sidebarMenu(
      menuItem("Click Through Rate", tabName ="home", icon = icon("home")),
      menuItem("data", tabName = "data", icon=icon("database")),
      menuItem("Conversion Rate", tabName = "Conversion_Rate", icon = icon("percent")),
      menuItem("analysis", tabName = "analysis", icon = icon("chart-bar")))
  ),#end of sidebar
  
  dashboardBody(
    tabItems(
      tabItem("home",
              fluidRow(
                column(4, valueBoxOutput("average_site_click_through_rate", width=12)),
                column(4, valueBoxOutput("average_light_theme_click_through_rate", width=12)),
                column(4, valueBoxOutput("average_dark_theme_click_through_rate", width = 12))
              ),
              fluidRow(
                column(6, box(title="Light Theme Click Through Rate", solidHeader = TRUE,
                              height=380, icon = icon("chart-area"), width=12,
                              highchartOutput("light_theme_click_through_rate_distribution_chart", height=360))),
                column(6, box(title = "Dark Theme Click Through Rate", solidHeader = TRUE,
                              height = 380, icon = icon("chart-area"), width = 12,
                              highchartOutput("dark_theme_click_through_rate_distribution_chart", height=360)))
              ),
              fluidRow(
                box(title = "Site Duration", solidHeader = TRUE,
                    height = 380, icon = icon("chart-bar"), width=12,
                    highchartOutput("site_duration_by_location_chart", height = 360))
              ),
      ),#end of the tab item- home
      tabItem("data",
              dataTableOutput("data_table")
        
      ),#end of tab -data
      tabItem("Conversion_Rate",
              fluidRow(
                column(6, valueBoxOutput("average_light_theme_conv_rate", width=12)),
                column(6, valueBoxOutput("average_dark_theme_conv_rate", width=12)),
              ),
              fluidRow(
                column(6, box(title = "Light Theme Conversion Rate", solidHeader = TRUE,
                              height = 380, width = 12,
                              highchartOutput("light_theme_conv_rate_dist_chart", height = 360))),
                column(6, box(title = "Dark Theme Conversion Rate", solidHeader = TRUE,
                              height = 380, width =12,
                              highchartOutput("dark_theme_conv_rate_dist_chart", height = 360))),
              )
              ),#end of conv rate
      tabItem("analysis",
              box(
                title = "hypothesis testing",
                solidHeader = TRUE,
                status = "primary",
                width = 12,
                textOutput("ttest_ctrr"),
                textOutput("ttest_crr"),
                textOutput("ttest_brr"),
                dataTableOutput("ttest_table")
              ))
    )
    
  )#end of the body
)#end of page

server <- function(input, output, session) {
  
  # Reactive function to process the data
  website_interaction_df <- reactive({
    read_csv("website_ab_test.csv") %>%
      mutate(Session_Duration = round(Session_Duration/60, 0),
             Theme = factor(Theme, levels = c("Light Theme", "Dark Theme"), ordered = TRUE),
             `Click Through Rate` = `Click Through Rate` * 100)
  })
#   
  # Hypothesis Testing
  output$ttest_ctrr <- renderText({
    light_theme_ctrr <- website_interaction_df()[website_interaction_df()$Theme == 'Light Theme', 'Click Through Rate']
    dark_theme_ctrr <- website_interaction_df()[website_interaction_df()$Theme == 'Dark Theme', 'Click Through Rate']

    t_test_result_ctrr <- t.test(light_theme_ctrr, dark_theme_ctrr)

    paste("Click Through Rate Hypothesis Testing:",
          "t-statistic:", round(t_test_result_ctrr$statistic, 2),
          "p-value:", round(t_test_result_ctrr$p.value, 4))
  })
#   
  # Perform a t-test to compare two themes for Conversion Rate
  output$ttest_crr <- renderText({
    light_theme_crr <- website_interaction_df()[website_interaction_df()$Theme == 'Light Theme', 'Conversion Rate']
    dark_theme_crr <- website_interaction_df()[website_interaction_df()$Theme == 'Dark Theme', 'Conversion Rate']

    t_test_result_crr <- t.test(light_theme_crr, dark_theme_crr)

    paste("Conversion Rate Hypothesis Testing:",
          "t-statistic:", round(t_test_result_crr$statistic, 2),
          "p-value:", round(t_test_result_crr$p.value, 4))
  })
#   
  # Perform a t-test to compare two themes for Bounce Rate
  output$ttest_brr <- renderText({
    light_theme_brr <- website_interaction_df()[website_interaction_df()$Theme == 'Light Theme', 'Bounce Rate']
    dark_theme_brr <- website_interaction_df()[website_interaction_df()$Theme == 'Dark Theme', 'Bounce Rate']

    t_test_result_brr <- t.test(light_theme_brr, dark_theme_brr)

    paste("Bounce Rate Hypothesis Testing:",
          "t-statistic:", round(t_test_result_brr$statistic, 2),
          "p-value:", round(t_test_result_brr$p.value, 4))
  })
# tabulate the hypothesis testing results
  output$ttest_table <- renderDataTable({
    t_test_results <- data.frame(
      metric = c("Click Through Rate", "Conversion Rate", "Bounce Rate"),
      t_statistic = c(round(t_test_result_ctrr$statistic, 2),
                      round(t_test_result_crr$statistic, 2),
                      round(t_test_result_brr$statistic, 2)),
      p_value = c(round(t_test_result_ctrr$p.value, 4),
                  round(t_test_result_crr$p.value, 4),
                  round(t_test_result_brr$p.value, 4))
    )
    datatable(t_test_results, rownames = FALSE)
  })
  # 
  # Data table
  output$data_table <- DT::renderDataTable({
    datatable(
      website_interaction_df(),
      options = list(
        columnDefs = list(list(className='dt-center', targets='_all'))
      ),
      rownames = FALSE
    )
  })
#   
  # Average Click Through Rate
  output$average_site_click_through_rate <- renderValueBox({
    valueBox(
      value = paste0(round(mean(website_interaction_df()$`Click Through Rate`),2), " %"),
      subtitle = "Average Site Click Through Rate",
      color = "olive",
      icon = icon("arrow-pointer")
    )
  })

  # Average Light Theme Click Through Rate
  output$average_light_theme_click_through_rate <- renderValueBox({
    valueBox(
      value = paste0(round(mean(website_interaction_df()[website_interaction_df()$Theme == 'Light Theme', 'Click Through Rate']$`Click Through Rate`),2), " %"),
      subtitle = "Average Light Theme Click Through Rate",
      color = "olive",
      icon = icon("arrow-pointer")
    )
  })

  # Average Dark Theme Click Through Rate
  output$average_dark_theme_click_through_rate <- renderValueBox({
    valueBox(
      value = paste0(round(mean(website_interaction_df()[website_interaction_df()$Theme == 'Dark Theme', 'Click Through Rate']$`Click Through Rate`),2), " %"),
      subtitle = "Average Dark Theme Click Through Rate",
      color = "olive",
      icon = icon("arrow-pointer")
    )
  })
  #Average Light Theme Conversion Rate
  output$average_light_theme_conv_rate <- renderValueBox({
    valueBox(
      value = paste0(round(mean(website_interaction_df()[website_interaction_df()$Theme == 'Light Theme', 'Conversion Rate']$`Conversion Rate`),2), "%"),
      subtitle = "Average Light Theme Conversion Rate",
      color = "lime",
      icon = icon("percent")
    )
  })
  #Average Dark Theme Conversion Rate
  output$average_dark_theme_conv_rate <- renderValueBox({
    valueBox(
      value = paste0(round(mean(website_interaction_df()[website_interaction_df()$Theme == 'Dark Theme', 'Conversion Rate']$`Conversion Rate`),2), "%"),
      subtitle = "Average Dark Theme Conversion Rate",
      color = "lime",
      icon = icon("percent")
    )
  })
   
  # Create Light Theme Click Through Rate distribution chart
  output$light_theme_click_through_rate_distribution_chart <- renderHighchart({
    website_interaction_df() %>% 
      filter(Theme %in% c("Light Theme")) %>% 
      select(`Click Through Rate`) %>% 
      pull(`Click Through Rate`) %>% 
      hchart(name = "Light Theme") %>% 
      hc_title(text = "Light Theme Click Through Rate Distribution") %>%
      hc_xAxis(title = list(text = "Click Through Rate (%)"),
               labels = list(format =  "{value}%")) %>% 
      hc_yAxis(title = list(text = "Count"),
               labels = list(format = "{value}")) %>% 
      hc_add_theme(
        hc_theme(chart = list(
          backgroundColor = "white")))
  })
  col = "teal"
  # Create Dark Theme Click Through Rate distribution chart
  output$dark_theme_click_through_rate_distribution_chart <- renderHighchart({
    website_interaction_df() %>% 
      filter(Theme %in% c("Dark Theme")) %>% 
      select(`Click Through Rate`) %>% 
      pull(`Click Through Rate`) %>% 
      hchart(name = "Dark Theme", color=col) %>% 
      hc_title(text = "Dark Theme Click Through Rate Distribution") %>%
      hc_xAxis(title = list(text = "Click Through Rate (%)"),
               labels = list(format =  "{value}%")) %>% 
      hc_yAxis(title = list(text = "Count"),
               labels = list(format = "{value}")) %>% 
      hc_add_theme(
        hc_theme(
          chart = list(
          backgroundColor = "white")))
  })
  # create light theme conversion rate distribution chart
  output$light_theme_conv_rate_dist_chart <- renderHighchart({
    website_interaction_df() %>%
      filter(Theme %in% c("Light Theme")) %>%
      select(`Conversion Rate`) %>%
      pull(`Conversion Rate`) %>%
      hchart(name="Light Theme", color = col) %>%
      hc_title(text = "Light Theme Conversion Rate Distribution") %>%
      hc_xAxis(title = list(text = "Conversion Rate (%)"),
               labels = list(format = "{value}%")) %>%
      hc_yAxis(title = list(text = "Count"),
               labels = list(format = "{value}")) %>%
      hc_add_theme(
        hc_theme(
          chart = list(
            backgroundColor = "white"
          )
        )
      )
  })
#create dark theme conversion rate distribution chart
  
  output$dark_theme_conv_rate_dist_chart <- renderHighchart({
    website_interaction_df() %>%
      filter(Theme %in% c("Dark Theme")) %>%
      select(`Conversion Rate`) %>%
      pull(`Conversion Rate`) %>%
      hchart(name="Dark Theme", color = col) %>%
      hc_title(text = "Dark Theme Conversion Rate Distribution") %>%
      hc_xAxis(title = list(text = "Conversion Rate (%)"),
               labels = list(format = "{value}%")) %>%
      hc_yAxis(title = list(text = "Count"),
               labels = list(format = "{value}")) %>%
      hc_add_theme(
        hc_theme(
          chart = list(
            backgroundColor = "white"
          )
        )
      )
  })
  # Create Site Duration by Location chart
  output$site_duration_by_location_chart <- renderHighchart({
    website_interaction_df() %>% 
      group_by(Location, Theme) %>%
      summarise(average_session_duration = round(mean(Session_Duration),2)) %>% 
      arrange(desc(average_session_duration)) %>% 
      hchart("column",hcaes(x = Location, y = average_session_duration, group = Theme), 
             dataLabels = list(enabled = TRUE, format = "{y}")) %>% 
      hc_title(
        text = paste0("Average Site Duration by location (in Minutes)")
      ) %>% 
      hc_xAxis(title = list(text = "Location")) %>% 
      hc_yAxis(title = list(text = "Average Session Duration (Minutes)"),
               labels = list(format = "{value}")) %>% 
      hc_legend(title = list(text = "Theme")) %>%
      hc_add_theme(
        hc_theme(
        chart = list(
          backgroundColor = "white")))
  })
  # hypothesis testing results tabulated called here in
  output$ttest_table <- renderDataTable({
    t_test_results <- data.frame(
      metric = c("Click Through Rate", "Conversion Rate", "Bounce Rate"),
      t_statistic = c(round(t_test_result_ctrr$statistic, 2),
                      round(t_test_result_crr$statistic, 2),
                      round(t_test_result_brr$statistic, 2)),
      p_value = c(round(t_test_result_ctrr$p.value, 4),
                  round(t_test_result_crr$p.value, 4),
                  round(t_test_result_brr$p.value, 4))
    )
    datatable(t_test_results, rownames = FALSE)
  })
  
  }                                
shinyApp(ui, server)