library(tidyverse)
library(reactable)
library(DT)
library(highcharter)


website_interaction_df <- read_csv("website_ab_test.csv") %>% 
  mutate(Session_Duration = round(Session_Duration/60, 0),
         Theme = factor(Theme, levels = c("Light Theme", "Dark Theme"), 
                        ordered = TRUE))

# Dashboard cards or KPIs 
site_click_through_rate <- round(mean(website_interaction_df$`Click Through Rate`),3)


light_theme_ctr_avg <- round(mean(website_interaction_df[website_interaction_df$Theme == 'Light Theme', 'Click Through Rate']$`Click Through Rate`),3)
dark_theme_ctr_avg <- round(mean(website_interaction_df[website_interaction_df$Theme == 'Dark Theme', 'Click Through Rate']$`Click Through Rate`),3)
  
# light_theme_ctr <- website_interaction_df[website_interaction_df$Theme == 'Light Theme', 'Click Through Rate']
# dark_theme_ctr <- website_interaction_df[website_interaction_df$Theme == 'Dark Theme', 'Click Through Rate']


# Click through Rate distribution of each theme 

website_interaction_df %>% 
  filter(Theme %in% c("Light Theme")) %>% 
  select(`Click Through Rate`) %>% 
  pull(`Click Through Rate`) %>% 
  hchart(name = "Light Theme", color = "#17b8b6") %>% 
  hc_title(text = "Light Theme Click Through Rate Distribution")

website_interaction_df %>% 
  filter(Theme %in% c("Dark Theme")) %>% 
  select(`Click Through Rate`) %>% 
  pull(`Click Through Rate`) %>% 
  hchart(name = "Dark Theme", color = "#17b8b6") %>% 
  hc_title(text = "Light Theme Click Through Rate Distribution")

# Session duration of each theme 

# Dark Theme 
website_interaction_df %>% 
  filter(Theme %in% c("Dark Theme")) %>% 
  select(Session_Duration) %>% 
  pull(Session_Duration) %>% 
  hchart(name = "Dark Theme", color = "#17b8b6") %>% 
  hc_title(text = "Dark Theme Session Duration Distribution (in Minutes)") %>% 
  hc_yAxis(title = list(text = "Number of users")) %>% 
  hc_xAxis(title = list(text = "Minutes"))

# Light Theme 
website_interaction_df %>% 
  filter(Theme %in% c("Light Theme")) %>% 
  select(Session_Duration) %>% 
  pull(Session_Duration) %>% 
  hchart(name = "Light Theme", color = "#17b8b6") %>% 
  hc_title(text = "Light Theme Session Duration Distribution (in Minutes)") %>% 
  hc_yAxis(title = list(text = "Number of users")) %>% 
  hc_xAxis(title = list(text = "Minutes"))

# Session duration by location of user 

website_interaction_df %>% 
  group_by(Location, Theme) %>%
  summarise(average_session_duration = round(mean(Session_Duration),2)) %>% 
  arrange(desc(average_session_duration)) %>% 
  hchart("column", hcaes(x = Location, y = average_session_duration, group = Theme), 
         dataLabels = list(enabled = TRUE, format = "{y}")) %>% 
  hc_title(
    text = paste0("Average Site Duration by location")
  ) %>% 
  hc_xAxis(title = list(text = "Location")) %>% 
  hc_yAxis(title = list(text = "Average Session Duration"),
           labels = list(format = "{value}")) %>% 
  hc_legend(title = list(text = "Theme"))
  
    

# Convert Click Through Rate, Conversion Rate and Bounce Rate to Percentages
# Create a DT table of Theme, Click Through Rate, Conversion Rate and Bounce Rate






















