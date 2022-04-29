get_candle <- function(symbol, timeframe) {
 
  url <- paste0('https://api.sandbox.gemini.com/v2/candles/',symbol,"/",timeframe)
  
  response <- GET(url)
  response <- fromJSON(rawToChar(response$content))
  
  colnames(response) <- c('time','open','high','low','close','volume')
  response1 <- as_tibble(response)
  
  return(response)
}