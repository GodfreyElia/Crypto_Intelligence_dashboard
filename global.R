library(dplyr)
library(httr)
library(jsonlite)
library(zoo)
library(plotly)
library(ggplot2)
library(tidyr)

fetch_and_cache_binance_data <- function(symbol, interval = "1d", limit = 500, cache_dir = "data_cache") {
  dir.create(cache_dir, showWarnings = FALSE)
  cache_file <- file.path(cache_dir, paste0(symbol, ".rds"))
  
  url <- paste0("https://api.binance.com/api/v3/klines?symbol=", symbol,
                "&interval=", interval, "&limit=", limit)
  
  result <- tryCatch({
    response <- GET(url)
    if (status_code(response) != 200) stop("API error")
    raw_data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    
    col_names <- c("OpenTime", "Open", "High", "Low", "Close", "Volume",
                   "CloseTime", "QuoteAssetVolume", "NumTrades",
                   "TakerBuyBaseVolume", "TakerBuyQuoteVolume", "Ignore")
    
    df <- as.data.frame(raw_data, stringsAsFactors = FALSE)
    names(df) <- col_names
    df <- df %>%
      mutate(OpenTime = as.numeric(OpenTime),
             CloseTime = as.numeric(CloseTime),
             across(c(Open, High, Low, Close, Volume, QuoteAssetVolume,
                      TakerBuyBaseVolume, TakerBuyQuoteVolume), as.numeric),
             NumTrades = as.integer(NumTrades),
             Symbol = symbol)
    
    saveRDS(df, cache_file)
    return(df)
  }, error = function(e) {
    if (file.exists(cache_file)) {
      message(paste("⚠️ Using cached data for", symbol))
      return(readRDS(cache_file))
    } else {
      message(paste("❌ Failed to fetch or find cache for", symbol))
      return(NULL)
    }
  })
  
  return(result)
}

# Get available trading symbols safely
get_trading_symbols <- function(cache_file = "data_cache/symbols.rds") {
  dir.create("data_cache", showWarnings = FALSE)
  
  if (file.exists(cache_file)) {
    message("✅ Loaded trading symbols from cache.")
    return(readRDS(cache_file))
  }
  
  tryCatch({
    url <- "https://api.binance.com/api/v3/exchangeInfo"
    response <- GET(url)
    if (status_code(response) != 200) stop("Exchange info API failed")
    data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    symbols_data <- data$symbols
    trading_symbols <- symbols_data %>%
      filter(status == "TRADING") %>%
      select(symbol, baseAsset, quoteAsset)
    unique_base_assets <- trading_symbols %>% distinct(baseAsset)
    symbols <- paste0(unique_base_assets$baseAsset, "USDT")
    saveRDS(symbols, cache_file)
    return(symbols)
  }, error = function(e) {
    message("❌ Could not load exchange info. Using fallback symbols.")
    return(c("BTCUSDT", "ETHUSDT", "BNBUSDT", "XRPUSDT", "SOLUSDT"))
  })
}

symbols <- get_trading_symbols()
all_data <- purrr::map_dfr(symbols, fetch_and_cache_binance_data)

model_data <- all_data %>%
  mutate(OpenTime = as.POSIXct(OpenTime / 1000, origin = "1970-01-01", tz = "UTC"),
         CloseTime = as.POSIXct(CloseTime / 1000, origin = "1970-01-01", tz = "UTC")) %>%
  mutate(Crypto = Symbol) %>%
  arrange(Crypto, OpenTime) %>%
  group_by(Crypto) %>%
  mutate(Return = (Close - Open) / Open * 100,
         DailyRange = High - Low,
         DailyVolatility = DailyRange / 4) %>%
  ungroup() %>%
  group_by(Crypto) %>%
  mutate(
    Lag1_Return = lag(Return, 1),
    MA5_Return = rollmean(Return, 5, fill = NA, align = "right"),
    Volatility5 = rollapply(Return, 5, sd, fill = NA, align = "right"),
    Lag1_Volume = lag(Volume, 1)
  ) %>%
  ungroup()