library(ggplot2)
library(ggpubr)
library('treemapify')
library(shiny)
library(dplyr)
library(plotly)
library(zoo)
library(DT)


server <- function(input, output, session) {
  observe({
    cryptos <- unique(model_data$Crypto)
    updateSelectInput(session, "primary_crypto", choices = cryptos, selected = cryptos[1])
    updateSelectInput(session, "comparative_crypto", choices = cryptos, selected = cryptos[2])
    
    var_choices <- c("Open", "High", "Low", "Close", "Volume", "NumTrades", "Return", "DailyRange", "DailyVolatility")
    updateSelectInput(session, "chart1_var", choices = var_choices, selected = "Close")
    updateSelectInput(session, "chart2_var", choices = var_choices, selected = "Return")
  })
  
  # Use shinyjs to hide/show elements based on the tab
  observe({
    current_tab <- input$sidebar
    if (current_tab == "overview") {
      hide("primary_crypto")
      hide("comparative_crypto")
      hide("chart1_var")
      hide("chart2_var")
    } else if (current_tab == "visualization") {
      # Show based on sub-tab
      if (input$viz_tabs == "Trend Comparison") {
        show("primary_crypto")
        show("comparative_crypto")
        show("chart1_var")
        hide("chart2_var")
      } else if (input$viz_tabs == "Distribution Comparison") {
        show("primary_crypto")
        show("comparative_crypto")
        show("chart2_var")
        hide("chart1_var")
      } else {
        show("primary_crypto")
        show("comparative_crypto")
        hide("chart1_var")
        hide("chart2_var")
      }
    } else if (current_tab == "prediction") {
      show("primary_crypto")
      show("comparative_crypto")
      hide("chart1_var")
      hide("chart2_var")
    }
  })
  
  
  # --- NEW: About Tab with two interactive plots side by side
  output$about_plots <- renderUI({
    fluidRow(
      # Left column for the first plot
      column(6,
             plotlyOutput("price_plot", height = "250px"),
             plotOutput("bar_chart", height = "400px")
      ),
      # Right column for the second plot
      column(6,
             plotOutput("heatmap_plot", height = "400px"),
             plotlyOutput("return_plot", height = "250px")
      )
    )
  })
  
  
  output$price_plot <- renderPlotly({
    plot_ly(model_data, x = ~OpenTime, y = ~Close, color = ~Crypto, type = 'scatter', mode = 'lines') %>%
      layout(title = "Crypto Price Trend",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Price (USD)"))
  })
  
  output$return_plot <- renderPlotly({
    plot_ly(model_data, x = ~OpenTime, y = ~Return, color = ~Crypto, type = 'scatter', mode = 'lines') %>%
      layout(title = "Crypto Return Trend",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Return (%)"))
  })
  

  output$heatmap_plot <- renderPlot({

    Cryp <- model_data %>%
      group_by(Crypto) %>%
      summarise(Average = mean(Return, na.rm = TRUE))
    
      ggplot(Cryp, aes(area = Average, fill = Average)) +
      geom_treemap() +
      geom_treemap_text(aes(label = Crypto), color = "white") +
      labs(title = "Heatmap of Average Daily Returns of Cryptos") +
      scale_fill_viridis_c(name = "Avg Return", option = "turbo") +
      theme(
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 12, vjust = 2, face = "bold"),
        plot.subtitle = element_text(size = 10, vjust = 2),
        legend.position = "right",
        legend.text = element_text(size = 8)
      )
    
    
  })
  

  output$bar_chart <- renderPlot({
    Outliers <- model_data %>%
      group_by(Crypto) %>%
      summarise(Day0 = mean(Return, na.rm = TRUE)) %>%
      arrange(desc(Day0)) %>%
      slice(c(1:10, (n()-9):n()))
    
      ggbarplot(Outliers, x = "Crypto", y = "Day0",
              fill = "Crypto", color = "white",
              palette = "viridis", sort.val = "desc", 
              sort.by.groups = FALSE, legend = "right", 
              ggtheme = theme_pubclean()) +
      font("x.text", size = 8, vjust = 0) +
      scale_x_discrete(breaks = seq(0, 20, 1)) +
      labs(x = "Crypto", y = "Return (%)") +
      theme(axis.text = element_text(size = 8), axis.title = element_text(size = 10)) +
      geom_text(aes(label = Crypto, angle = 90), size = 2.5) +
      labs(title = "Top 10 Daily Losers and Gainers YTD",
           subtitle = '* The Returns exclude outliers that have been plotted separately') +
      theme(plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(size = 9, hjust = 0.5))

  })
  
  output$data_table <- DT::renderDataTable({
    rounded_data <- model_data %>%
      mutate_if(is.numeric, ~round(., 2))%>%
      select(-c("Ignore","Symbol"))
    
    DT::datatable(rounded_data, 
                  options = list(
                    scrollX = TRUE,          
                    scrollY = "500px",       
                    paging = TRUE,           
                    pageLength = 10,         
                    lengthMenu = c(10, 25, 50, 100), 
                    autoWidth = TRUE,       
                    dom = 'lfrtip'           
                  ))
  })

  output$data_structure <- renderPrint({
    str(model_data)
  })
  
  output$trend_plot <- renderPlotly({
    req(input$primary_crypto, input$comparative_crypto, input$chart1_var)
    plot_data <- model_data %>%
      filter(Crypto %in% c(input$primary_crypto, input$comparative_crypto))
    
    plot_ly(plot_data, x = ~OpenTime, y = as.formula(paste0("~", input$chart1_var)),
            color = ~Crypto, type = 'scatter', mode = 'lines') %>%
      layout(title = paste("Comparative", input$chart1_var, "Trend"),
             xaxis = list(title = "Date"),
             yaxis = list(title = input$chart1_var))
  })
  
  output$dist_plot <- renderPlotly({
    req(input$chart2_var)
    plot_data <- model_data %>%
      filter(Crypto %in% c(input$primary_crypto, input$comparative_crypto))
    
    plot_ly(plot_data, x = as.formula(paste0("~", input$chart2_var)), color = ~Crypto,
            type = 'histogram', nbinsx = 50, opacity = 0.6) %>%
      layout(barmode = "overlay",
             title = paste(input$chart2_var, "Distribution"),
             xaxis = list(title = input$chart2_var),
             yaxis = list(title = "Frequency"))
  })
  
  output$latest_stats <- renderUI({
    req(input$primary_crypto, input$comparative_crypto)
    primary <- model_data %>% filter(Crypto == input$primary_crypto) %>% slice_max(OpenTime, n = 1)
    comparative <- model_data %>% filter(Crypto == input$comparative_crypto) %>% slice_max(OpenTime, n = 1)
    
    tags$table(class = "table",
               tags$thead(
                 tags$tr(
                   tags$th("Metric"), tags$th(input$primary_crypto), tags$th(input$comparative_crypto)
                 )
               ),
               tags$tbody(
                 tags$tr(tags$td("Closing Price"), tags$td(round(primary$Close, 2)), tags$td(round(comparative$Close, 2))),
                 tags$tr(tags$td("Return (%)"), tags$td(round(primary$Return, 2)), tags$td(round(comparative$Return, 2))),
                 tags$tr(tags$td("Volatility"), tags$td(round(primary$DailyVolatility, 4)), tags$td(round(comparative$DailyVolatility, 4)))
               )
    )
  })
  
  output$summary_table <- DT::renderDataTable({
    req(model_data)
    
    df <- model_data
    
    summary_stats <- df %>%
      group_by(Crypto) %>%
      summarise(
        `Mean Return (%)` = round(mean(Return, na.rm = TRUE), 2),
        `Median Return (%)` = round(median(Return, na.rm = TRUE), 2),
        `Std Dev (%)` = round(sd(Return, na.rm = TRUE), 2),
        `Min Return (%)` = round(min(Return, na.rm = TRUE), 2),
        `Max Return (%)` = round(max(Return, na.rm = TRUE), 2),
        `Positive Days (%)` = round(mean(Return > 0, na.rm = TRUE), 2),
        `Negative Days (%)` = round(mean(Return < 0, na.rm = TRUE), 2),
        `Total Days` = n()
      ) %>%
      arrange(desc(`Mean Return (%)`))
    
    DT::datatable(summary_stats, options = list(
                  scrollX = TRUE,          
                  scrollY = "500px",       
                  paging = TRUE,           
                  pageLength = 10,         
                  lengthMenu = c(10, 25, 50, 100), 
                  autoWidth = TRUE,       
                  dom = 'lfrtip' 
                    ))
  })
  
  
  predict_next_return <- function(df) {
    df <- df %>% filter(!is.na(Lag1_Return), !is.na(MA5_Return), !is.na(Volatility5), !is.na(Lag1_Volume)) %>%
      arrange(OpenTime) %>%
      mutate(Target = lead(Return)) %>%
      drop_na(Target)
    if (nrow(df) < 30) return(NA)
    model <- lm(Target ~ Lag1_Return + MA5_Return + Volatility5 + Lag1_Volume, data = df)
    latest <- df %>% slice_max(OpenTime, n = 1)
    predict(model, newdata = latest)
  }
  
  evaluate_model <- function(df) {
    df <- df %>% filter(!is.na(Lag1_Return), !is.na(MA5_Return), !is.na(Volatility5), !is.na(Lag1_Volume)) %>%
      arrange(OpenTime) %>%
      mutate(Target = lead(Return)) %>%
      drop_na(Target)
    if (nrow(df) < 50) return(NULL)
    split <- floor(0.8 * nrow(df))
    train <- df[1:split, ]; test <- df[(split+1):nrow(df), ]
    model <- lm(Target ~ Lag1_Return + MA5_Return + Volatility5 + Lag1_Volume, data = train)
    test$Pred <- predict(model, newdata = test)
    list(
      RMSE = round(sqrt(mean((test$Target - test$Pred)^2)), 4),
      MAE = round(mean(abs(test$Target - test$Pred)), 4),
      R2 = round(summary(model)$r.squared, 4)
    )
  }
  
  output$predicted_return <- renderUI({
    p <- round(predict_next_return(filter(model_data, Crypto == input$primary_crypto)), 3)
    c <- round(predict_next_return(filter(model_data, Crypto == input$comparative_crypto)), 3)
    tags$table(class = "table table-bordered",
               tags$thead(tags$tr(tags$th("Crypto"), tags$th("Predicted Return (%)"))),
               tags$tbody(
                 tags$tr(tags$td(input$primary_crypto), tags$td(p)),
                 tags$tr(tags$td(input$comparative_crypto), tags$td(c))
               )
    )
  })
  
  output$model_diagnostics <- renderUI({
    p <- evaluate_model(filter(model_data, Crypto == input$primary_crypto))
    c <- evaluate_model(filter(model_data, Crypto == input$comparative_crypto))
    if (is.null(p) || is.null(c)) return(tags$p("Not enough data"))
    tags$table(class = "table table-striped",
               tags$thead(tags$tr(tags$th("Metric"), tags$th(input$primary_crypto), tags$th(input$comparative_crypto))),
               tags$tbody(
                 tags$tr(tags$td("RMSE"), tags$td(p$RMSE), tags$td(c$RMSE)),
                 tags$tr(tags$td("MAE"), tags$td(p$MAE), tags$td(c$MAE)),
                 tags$tr(tags$td("R-squared"), tags$td(p$R2), tags$td(c$R2))
               )
    )
  })
}
