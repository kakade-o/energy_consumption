library(shiny)
library(shinydashboard)
library(leaflet)
library(ggplot2)
library(plotly)


header <- dashboardHeader(
  title="Energy Consumption Analysis")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Consumption", 
             tabName = "consumption", 
             icon = icon("dashboard")),
    menuItem("Emissions",
             tabName = "emissions",
             icon = icon("cloud")),
    menuItem("Correlation",
             tabName = "correlation",
             icon = icon("chart-line")),
    menuItem("Predictive Analysis",
             tabName = "prediction",
             icon = icon("chart-area"))))

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "consumption",
            
            fluidRow(
              column(width = 9,
                     box(width = NULL, solidHeader = TRUE,
                         plotlyOutput("choropleth", height = 500))),
              column(width = 3,
                     box(width = NULL, status = "primary",
                         radioButtons("energy", 
                                      "Type", 
                                      choices = c("Solar",
                                                  "Wind",
                                                  "Coal"),
                                      selected = "Solar"),
                         radioButtons("year",
                                      "Year",
                                      choices = c("2013",
                                                  "2014",
                                                  "2015"),
                                      selected = "2015"),
                         p(class = "text-muted",
                           paste("")))))),
    tabItem(tabName = "emissions",
            fluidRow(
              column(width = 9,
                     box(width = NULL, solidHeader = TRUE,
                         plotlyOutput("emit", height = 500))),
              column(width = 3,
                     box(width = NULL, status = "primary",
                         radioButtons("emit_year",
                                      "Year",
                                      choices = c("2013" = "emit_2013",
                                                  "2014" = "emit_2014",
                                                  "2015" = "emit_2015")))))),
    tabItem(tabName = "correlation",
            fluidRow(
              column(width = 9,
                     box(width = NULL, solidHeader = TRUE,
                         plotlyOutput("corr", height = 500))),
              column(width = 3,
                     box(width = NULL, status = "primary",
                         radioButtons("var_corr",
                                      "Variable Correlation",
                                      choices = c("gdp", "population", 
                                                  "large cities" = "large_2015",
                                                  "medium cities" = "medium_2015",
                                                  "small cities" = "small_2015",
                                                  "consumption")))))),
    tabItem(tabName = "prediction",
            fluidRow(
              column(width = 6,
                     box(width = NULL, solidHeader = TRUE,
                         plotlyOutput("em", height = 500))),
              column(width = 6,
                     box(width = NULL, solidHeader = TRUE,
                         plotlyOutput("pred", height = 500)))
              ))))

dashboardPage(
  skin = "green",
  header,
  sidebar = sidebar,
  body)
