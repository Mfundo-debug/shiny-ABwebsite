server <- function(input, output, session) {
  
  website_interaction_df <- reactive({
    read_csv("website_ab_test.csv") %>% 
      mutate(Session_Duration = round(Session_Duration/60, 0),
             Theme = factor(Theme, levels = c("Light Theme", "Dark Theme"), 
                            ordered = TRUE),
             `Click Through Rate` = `Click Through Rate` * 100)
  })
  
  
  output$average_site_click_through_rate <- renderValueBox({
    valueBox(
      value = paste0(round(mean(website_interaction_df()$`Click Through Rate`),2), " %"),
      subtitle = "Average Site Click Through Rate",
      color = "gray-dark",
      icon = icon("arrow-pointer")
    )
  })
  
  output$average_light_theme_click_through_rate <- renderValueBox({
    valueBox(
      value = paste0(round(mean(website_interaction_df()[website_interaction_df()$Theme == 'Light Theme', 'Click Through Rate']$`Click Through Rate`),2), " %"),
      subtitle = "Average Light Theme Click Through Rate",
      color = "gray-dark",
      icon = icon("arrow-pointer")
    )
  })
  
  output$average_dark_theme_click_through_rate <- renderValueBox({
    valueBox(
      value = paste0(round(mean(website_interaction_df()[website_interaction_df()$Theme == 'Dark Theme', 'Click Through Rate']$`Click Through Rate`),2), " %"),
      subtitle = "Average Dark Theme Click Through Rate",
      color = "gray-dark",
      icon = icon("arrow-pointer")
    )
  })
  
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
  
  
  output$dark_theme_click_through_rate_distribution_chart <- renderHighchart({
    website_interaction_df() %>% 
      filter(Theme %in% c("Dark Theme")) %>% 
      select(`Click Through Rate`) %>% 
      pull(`Click Through Rate`) %>% 
      hchart(name = "Dark Theme") %>% 
      hc_title(text = "Light Theme Click Through Rate Distribution") %>%
      hc_xAxis(title = list(text = "Click Through Rate (%)"),
               labels = list(format =  "{value}%")) %>% 
      hc_yAxis(title = list(text = "Count"),
               labels = list(format = "{value}")) %>% 
      hc_add_theme(
        hc_theme(chart = list(
          backgroundColor = "white")))
  })
  
  output$site_duration_by_location_chart <- renderHighchart({
    website_interaction_df() %>% 
      group_by(Location, Theme) %>%
      summarise(average_session_duration = round(mean(Session_Duration),2)) %>% 
      arrange(desc(average_session_duration)) %>% 
      hchart("column", hcaes(x = Location, y = average_session_duration, group = Theme), 
             dataLabels = list(enabled = TRUE, format = "{y}")) %>% 
      hc_title(
        text = paste0("Average Site Duration by location (in Minutes)")
      ) %>% 
      hc_xAxis(title = list(text = "Location")) %>% 
      hc_yAxis(title = list(text = "Average Session Duration (Minutes)"),
               labels = list(format = "{value}")) %>% 
      hc_legend(title = list(text = "Theme")) %>%
      hc_add_theme(
        hc_theme(chart = list(
          backgroundColor = "white")))
  })
  
  
}
