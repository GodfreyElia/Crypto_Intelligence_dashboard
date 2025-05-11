library(randomForest)
library(ipred)
library(gbm)
library(e1071)
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
      hide("model_choice_wrapper")
      
    } else if (current_tab == "visualization") {
      # Show/hide elements based on sub-tab
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
      hide("model_choice_wrapper")
      
    } else if (current_tab == "prediction") {
      show("primary_crypto")
      show("comparative_crypto")
      hide("chart1_var")
      hide("chart2_var")
      show("model_choice_wrapper")
    }
  })
  
  
  
  # --- NEW: About Tab with two interactive plots side by side
  output$about_plots <- renderUI({
    fluidRow(
      column(
        width = 6,
        box(
          title = "Price Trend", solidHeader = FALSE,
          plotlyOutput("price_plot", height = "300px"),
          width = NULL,
          style = "border: 1px solid #ddd; margin-bottom: 10px; padding: 5px;"
        ),
        box(
          title = "Bar Chart", solidHeader = FALSE,
          plotlyOutput("bar_chart", height = "250px"),
          width = NULL,
          style = "border: 1px solid #ddd; padding: 5px;"
        )
      ),
      column(
        width = 6,
        box(
          title = "Heatmap", solidHeader = FALSE,
          plotlyOutput("heatmap_plot", height = "250px"),
          width = NULL,
          style = "border: 1px solid #ddd; margin-bottom: 10px; padding: 5px;"
        ),
        box(
          title = "Return Trend", solidHeader = FALSE,
          plotlyOutput("return_plot", height = "300px"),
          width = NULL,
          style = "border: 1px solid #ddd; padding: 5px;"
        )
      )
    )
  })
  
  
  
  output$price_plot <- renderPlotly({
    plot_ly(model_data, 
            x = ~OpenTime, 
            y = ~Close, 
            color = ~Crypto,
            colors = "Set1",
            type = 'scatter', 
            mode = 'lines') %>%
      layout(title = "Crypto Price Trend",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Price (USD)"),
             legend = list(
               font = list(size = 8),
               orientation = "v",   # Vertical to avoid taking up width
               x = 1.02,            # Just outside the plot
               y = 1,
               bgcolor = 'rgba(255,255,255,0.5)',  # semi-transparent background
               bordercolor = 'gray',
               borderwidth = 0.5
             ),
             margin = list(r = 120)  # Extra space on the right for the legend
      )
  })
  
  output$return_plot <- renderPlotly({
    plot_ly(model_data, 
            x = ~OpenTime, 
            y = ~Return, 
            color = ~Crypto, 
            colors = "Set1",
            type = 'scatter', 
            mode = 'lines') %>%
      layout(
        title = "Crypto Return Trend",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Return (%)"),
        legend = list(
          font = list(size = 8),
          orientation = "v",   # Vertical to avoid taking up width
          x = 1.02,            # Just outside the plot
          y = 1,
          bgcolor = 'rgba(255,255,255,0.5)',  # semi-transparent background
          bordercolor = 'gray',
          borderwidth = 0.5
        ),
        margin = list(r = 120)  # Extra space on the right for the legend
      )
  })
  

  output$heatmap_plot <- renderPlotly({
    
    Cryp <- model_data %>%
      group_by(Crypto) %>%
      summarise(Average = mean(Return, na.rm = TRUE)) 
    
    plot_ly(
      data = Cryp, 
      type = "treemap",
      labels = ~Crypto, 
      parents = "",
      values = ~Average,
      textinfo = "label+value",
      hoverinfo = "label+value",
      marker = list(colors = ~Average,
                    colorscale = 'viridis', showscale = TRUE) 
    ) %>%
      layout(
        title = "Interactive Heatmap of Average Daily Returns of Cryptos (To Date)",
        font = list(size = 10),
        margin = list(t = 30)
      )
  })
  

output$bar_chart <- renderPlotly({
  Outliers <- model_data %>%
    group_by(Crypto) %>%
    summarise(Return = mean(Return, na.rm = TRUE)) %>%
    arrange(desc(Return)) %>%
    slice(c(1:10, (n() - 9):n()))
  
  p <- ggplot(Outliers, aes(x = reorder(Crypto, Return), y = Return, fill = Crypto)) +
    geom_col(color = "white") +
    coord_flip() +
    scale_fill_viridis_d(option = "F") +
    labs(x = "Crypto", y = "Return (%)",
         title = "Top 10 Daily Losers and Gainers YTD") +
    theme_minimal(base_size = 10) +
    theme(legend.position = "none",
          plot.title = element_text(size = 12, hjust = 0.5),
          plot.subtitle = element_text(size = 9, hjust = 0.5))
  
  ggplotly(p)
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
    
    plot_ly(plot_data, 
            x = ~OpenTime, 
            y = as.formula(paste0("~", input$chart1_var)),
            color = ~Crypto,
            colors = "Set1",
            type = 'scatter', 
            mode = 'lines') %>%
      layout(title = paste("Comparative", input$chart1_var, "Trend"),
             xaxis = list(title = "Date"),
             yaxis = list(title = input$chart1_var),
             legend = list(
               font = list(size = 8),        # Smaller legend text
               orientation = "h",            # Horizontal legend
               x = 0, y = -0.2               # Position below the plot
             ))
  })
  
  output$dist_plot <- renderPlotly({
    req(input$chart2_var)
    plot_data <- model_data %>%
      filter(Crypto %in% c(input$primary_crypto, input$comparative_crypto))
    
    plot_ly(plot_data, 
            x = as.formula(paste0("~", input$chart2_var)), 
            color = ~Crypto,
            colors = "Set1",
            type = 'histogram', nbinsx = 50, opacity = 0.6) %>%
      layout(barmode = "overlay",
             title = paste(input$chart2_var, "Distribution"),
             xaxis = list(title = input$chart2_var),
             yaxis = list(title = "Frequency"),
             legend = list(
               font = list(size = 8),        # Smaller legend text
               orientation = "h",            # Horizontal legend
               x = 0, y = -0.2               # Position below the plot
             ))
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
  
  predict_next_return <- function(df, model_name) {
    df <- df %>% arrange(OpenTime) %>% mutate(
      Lag1_Return = lag(Return, 1),
      MA5_Return = zoo::rollmean(Return, 5, fill = NA, align = "right"),
      Volatility5 = zoo::rollapply(Return, 5, sd, fill = NA, align = "right"),
      Lag1_Volume = lag(Volume, 1),
      Target = lead(Return)
    ) %>% drop_na()
    
    if (nrow(df) < 30) return(NA)
    train <- df[1:(nrow(df)-1), ]
    latest <- df[nrow(df), ]
    
    model <- switch(model_name,
                    "Linear Regression" = lm(Target ~ Lag1_Return + MA5_Return + Volatility5 + Lag1_Volume, data = train),
                    "Random Forest" = randomForest(Target ~ Lag1_Return + MA5_Return + Volatility5 + Lag1_Volume, data = train),
                    "Bagging" = bagging(Target ~ Lag1_Return + MA5_Return + Volatility5 + Lag1_Volume, data = train),
                    "Boosting" = gbm(Target ~ Lag1_Return + MA5_Return + Volatility5 + Lag1_Volume, data = train, distribution = "gaussian", n.trees = 100),
                    "SVM" = svm(Target ~ Lag1_Return + MA5_Return + Volatility5 + Lag1_Volume, data = train)
    )
    
    if (model_name == "Boosting") {
      predict(model, newdata = latest, n.trees = 100)
    } else {
      predict(model, newdata = latest)
    }
  }
  
  output$predicted_return <- renderUI({
    req(input$primary_crypto, input$model_choice)
    df <- model_data %>% filter(Crypto == input$primary_crypto)
    pred <- predict_next_return(df, input$model_choice)
    if (is.na(pred)) {
      h4("Not enough data to generate prediction.")
    } else {
      valueBox(subtitle = paste("Predicted Return for", input$primary_crypto, "(Tomorrow)"),
               value = paste0(round(pred, 2), "%"),
               icon = icon("chart-line"), color = "green")
    }
  })
  
  output$comparative_predicted_return <- renderUI({
    req(input$comparative_crypto, input$model_choice)
    df <- model_data %>% filter(Crypto == input$comparative_crypto)
    pred <- predict_next_return(df, input$model_choice)
    if (is.na(pred)) {
      h4("Not enough data to generate comparative prediction.")
    } else {
      valueBox(subtitle = paste("Predicted Return for", input$comparative_crypto, "(Tomorrow)"),
               value = paste0(round(pred, 2), "%"),
               icon = icon("chart-line"), color = "blue")
    }
  })
  
  output$model_diagnostics <- renderUI({
    req(input$primary_crypto, input$model_choice)
    df <- model_data %>% filter(Crypto == input$primary_crypto) %>% arrange(OpenTime) %>% mutate(
      Lag1_Return = lag(Return, 1),
      MA5_Return = zoo::rollmean(Return, 5, fill = NA, align = "right"),
      Volatility5 = zoo::rollapply(Return, 5, sd, fill = NA, align = "right"),
      Lag1_Volume = lag(Volume, 1),
      Target = lead(Return)
    ) %>% drop_na()
    if (nrow(df) < 50) return(h4("Insufficient data for diagnostics."))
    split <- floor(0.8 * nrow(df))
    train <- df[1:split, ]; test <- df[(split+1):nrow(df), ]
    model <- switch(input$model_choice,
                    "Linear Regression" = lm(Target ~ Lag1_Return + MA5_Return + Volatility5 + Lag1_Volume, data = train),
                    "Random Forest" = randomForest(Target ~ Lag1_Return + MA5_Return + Volatility5 + Lag1_Volume, data = train),
                    "Bagging" = bagging(Target ~ Lag1_Return + MA5_Return + Volatility5 + Lag1_Volume, data = train),
                    "Boosting" = gbm(Target ~ Lag1_Return + MA5_Return + Volatility5 + Lag1_Volume, data = train, distribution = "gaussian", n.trees = 100),
                    "SVM" = svm(Target ~ Lag1_Return + MA5_Return + Volatility5 + Lag1_Volume, data = train)
    )
    pred <- if (input$model_choice == "Boosting") predict(model, newdata = test, n.trees = 100) else predict(model, newdata = test)
    RMSE <- sqrt(mean((test$Target - pred)^2))
    MAE <- mean(abs(test$Target - pred))
    R2 <- cor(test$Target, pred)^2
    tagList(h4(paste("Model Diagnostics for", input$primary_crypto)), tags$ul(
      tags$li(paste("RMSE:", round(RMSE, 4))),
      tags$li(paste("MAE:", round(MAE, 4))),
      tags$li(paste("R-squared:", round(R2, 4)))
    ))
  })
  
  
  output$comparative_model_diagnostics <- renderUI({
    req(input$comparative_crypto, input$model_choice)
    df <- model_data %>% filter(Crypto == input$comparative_crypto) %>% arrange(OpenTime) %>% mutate(
      Lag1_Return = lag(Return, 1),
      MA5_Return = zoo::rollmean(Return, 5, fill = NA, align = "right"),
      Volatility5 = zoo::rollapply(Return, 5, sd, fill = NA, align = "right"),
      Lag1_Volume = lag(Volume, 1),
      Target = lead(Return)
    ) %>% drop_na()
    if (nrow(df) < 50) return(h4("Insufficient data for comparative diagnostics."))
    split <- floor(0.8 * nrow(df))
    train <- df[1:split, ]; test <- df[(split+1):nrow(df), ]
    model <- switch(input$model_choice,
                    "Linear Regression" = lm(Target ~ Lag1_Return + MA5_Return + Volatility5 + Lag1_Volume, data = train),
                    "Random Forest" = randomForest(Target ~ Lag1_Return + MA5_Return + Volatility5 + Lag1_Volume, data = train),
                    "Bagging" = bagging(Target ~ Lag1_Return + MA5_Return + Volatility5 + Lag1_Volume, data = train),
                    "Boosting" = gbm(Target ~ Lag1_Return + MA5_Return + Volatility5 + Lag1_Volume, data = train, distribution = "gaussian", n.trees = 100),
                    "SVM" = svm(Target ~ Lag1_Return + MA5_Return + Volatility5 + Lag1_Volume, data = train)
    )
    pred <- if (input$model_choice == "Boosting") predict(model, newdata = test, n.trees = 100) else predict(model, newdata = test)
    RMSE <- sqrt(mean((test$Target - pred)^2))
    MAE <- mean(abs(test$Target - pred))
    R2 <- cor(test$Target, pred)^2
    tagList(h4(paste("Model Diagnostics for", input$comparative_crypto)), tags$ul(
      tags$li(paste("RMSE:", round(RMSE, 4))),
      tags$li(paste("MAE:", round(MAE, 4))),
      tags$li(paste("R-squared:", round(R2, 4)))
    ))
  })
  
}
