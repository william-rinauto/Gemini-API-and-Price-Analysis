get_ticker <- function(symbol) {
  
 url <- paste0('https://api.sandbox.gemini.com/v1/pubticker/',symbol)
 
 response <- GET(url)
 response <- fromJSON(rawToChar(response$content))
 
 return(response)
   
}