library(shiny)
library(shinydashboard)
library(shinyjs)

dashboardPage(
  dashboardHeader(
    title = "Crypto Intelligence Dashboard", titleWidth = 650,
    tags$li(class = "dropdown", tags$a(href = "https://dev-genkolokosa.pantheonsite.io/", icon("globe"), "Website", target = "_blank")),
    tags$li(class = "dropdown", tags$a(href = "https://www.linkedin.com/in/godfreyn321/", icon("linkedin"), "My Profile", target = "_blank")),
    tags$li(class = "dropdown", tags$a(href = "https://github.com/GodfreyElia", icon("github"), "GitHub", target = "_blank"))
  ),
  
  dashboardSidebar(
    sidebarMenu(id = "sidebar",
                menuItem("Overview", tabName = "overview", icon = icon("info-circle")),
                menuItem("Visualization", tabName = "visualization", icon = icon("chart-line")),
                menuItem("Prediction", tabName = "prediction", icon = icon("robot")),
                hr(),
                selectInput("primary_crypto", "Primary Crypto:", choices = NULL),
                selectInput("comparative_crypto", "Comparative Crypto:", choices = NULL),
                selectInput("chart1_var", "Chart 1 Variable:", choices = NULL),
                selectInput("chart2_var", "Chart 2 Variable:", choices = NULL)
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(tabName = "overview",
              tabBox(id = "overview_tabs", width = 12, side = "left",
                     tabPanel("About", icon = icon("address-card"),
                              h4("About the Dashboard"),
                              p("This dashboard provides comparative analysis and predictive insights into selected cryptocurrencies using data from Binance."),
                              uiOutput("about_plots")
                     ),
                     tabPanel("Dataset", icon = icon("table"),
                              box(
                                title = "Full Dataset View",
                                width = 12,
                                status = "primary",
                                solidHeader = TRUE,
                                DT::dataTableOutput("data_table")
                              )
                     ),
                     tabPanel("Structure", icon = icon("cogs"),
                              verbatimTextOutput("data_structure")
                     ),
                     tabPanel("Summary Stats", icon = icon("chart-pie"),
                              box(
                                title = "Summary Statistics (All Cryptocurrencies)",
                                status = "primary",
                                solidHeader = TRUE,
                                width = 12,
                                DT::dataTableOutput("summary_table")
                              )
                     )
              )
      ),
      
      tabItem(tabName = "visualization",
              tabBox(id = "viz_tabs", width = 12, side = "left",
                     tabPanel("Trend Comparison", icon = icon("chart-line"),
                              plotlyOutput("trend_plot", height = "350px")
                     ),
                     tabPanel("Distribution Comparison", icon = icon("chart-bar"),
                              plotlyOutput("dist_plot")
                     ),
                     tabPanel("Latest Stats", icon = icon("magnifying-glass"),
                              uiOutput("latest_stats")
                     )
              )
      ),
      
      tabItem(tabName = "prediction",
              tabBox(id = "pred_tabs", width = 12, side = "left",
                     tabPanel("Predicted Return", icon = icon("brain"),
                              uiOutput("predicted_return")
                     ),
                     tabPanel("Model Diagnostics", icon = icon("flask"),
                              uiOutput("model_diagnostics")
                     )
              )
      )
    )
  )
)
