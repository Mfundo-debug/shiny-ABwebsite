library(tidyverse)
library(highcharter)
library(DT)
library(shiny)
library(bs4Dash)
library(waiter)

dashboardPage(
  dashboardHeader(title = dashboardBrand(
    title = "Website Theme Analysis",
    color = "gray-dark",
    image = "theme_logo.png")), # end of header
  dashboardSidebar(disable = TRUE), # end of sidebar
  dashboardBody(
    fluidRow(
      column(4, valueBoxOutput("average_site_click_through_rate", width = 12)),
      column(4, valueBoxOutput("average_light_theme_click_through_rate", width = 12)),
      column(4, valueBoxOutput("average_dark_theme_click_through_rate", width = 12))
    ),
    fluidRow(
      column(6, box(title = "Light Theme Click Through Rate", solidHeader = TRUE, 
                    height = 380, icon = icon("chart-area"), width = 12,
                    highchartOutput("light_theme_click_through_rate_distribution_chart", height = 360))),
      column(6, box(title = "Dark Theme Click Through Rate", solidHeader = TRUE, 
                    height = 380, icon = icon("chart-area"), width = 12,
                    highchartOutput("dark_theme_click_through_rate_distribution_chart", height = 360)))
    ),
    fluidRow(
      box(title = "Site Duration", solidHeader = TRUE, 
          height = 380, icon = icon("chart-bar"), width = 12,
          highchartOutput("site_duration_by_location_chart", height = 360))
    )
  ) # end of body 
) # end of page