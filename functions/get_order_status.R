get_order_status <- function(order_id) {
  endpoint <- '/v1/order/status' 
  x_gemini_payload <- toJSON(list(request = endpoint, 
                                  nonce = as.character(as.integer(now())*1000),
                                  order_id = order_id)
  
  #Added auto-unbox to un-bracket single element vectors. This matches python example code
  #https://www.base64decode.org/
  ,auto_unbox = T
  )
  x_gemini_payload <- base64enc::base64encode(charToRaw(x_gemini_payload))
  
  signature <- sha384(x_gemini_payload, secret)
  
  url <- paste0('https://api.sandbox.gemini.com',endpoint)
  
  
  request_header <- add_headers(
    `Content-Type` = 'text/plain',
    `Content-Length` = '0',
    `X-GEMINI-APIKEY` = api_key,
    `X-GEMINI-PAYLOAD` = x_gemini_payload,
    `X-GEMINI-SIGNATURE` = signature,
    `Cache-Control` = 'no-cache'
  )
  
  
  response <- httr::POST(url, 
                         request_header)
  response <- fromJSON(rawToChar(response$content))
  return(response)
  
  
}