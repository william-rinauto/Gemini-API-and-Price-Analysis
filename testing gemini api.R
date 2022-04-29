library(httr)
library(jsonlite)
library(lubridate)
library(openssl)
library(dplyr)

content_length <- 0
content_type <- 'text/plain'

order <- place_order(symbol = 'btcusd', 
                     amount = 0.001, 
                     price = as.numeric(get_ticker('btcusd')$bid)-10,
                     buy_or_sell = 'buy')

order_status <- get_order_status(order$order_id)

active_orders <- get_active_orders()




