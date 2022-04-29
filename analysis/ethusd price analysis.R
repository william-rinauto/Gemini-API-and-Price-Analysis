library(tidyverse)
library(lubridate)
library(zoo)
library(h2o)
library(future.apply)

moving_mean <- function(var, n) rollapply({{ var }}, n, mean, fill = NA, align = 'right')

eth_files <- list.files('cryptodatadownload pricing')


plan('multisession', workers = 16)
dat <- lapply(eth_files, function(file) {

  
  year <- str_extract(file, "\\d{4}")
  pair <- str_match(file, "^[a-zA-Z]+_{1}([a-zA-Z]+)")[,2]
  pair
  
  m <- read.csv(file.path('cryptodatadownload pricing',file),header = F)[-1,]
  names(m) <- unlist(m[1,])
  m <- m[-1,]
  
  m <- m %>%
    mutate(year = year, 
           pair = pair)
  
}) %>%
  bind_rows() %>%
  mutate(Close = as.numeric(Close))



dat1 <- mutate(dat, datetime = ymd_hms(Date)) %>%
  mutate(data_date = ymd(substr(datetime,1,10))) 


max_date <- dat1 %>%
  arrange(datetime) %>%
  group_by(Symbol) %>%
  mutate(moving_mean60 = moving_mean(Close, 60)) %>%
  mutate(moving_mean5 = moving_mean(Close, 5)) %>%
  mutate(moving_mean120 = moving_mean(Close, 120)) %>%
  arrange(Symbol,desc(datetime)) %>%
  ungroup()


eth_only <- filter(max_date, Symbol == 'ETHUSD') 

no_eth <- filter(max_date, Symbol != 'ETHUSD') %>%
  select(datetime, Close, Symbol, contains('moving')) %>%
  ungroup() %>%
  pivot_wider(id_cols = datetime,
              names_from = Symbol,
              values_from = 3:ncol(.))

max_date1 <- left_join(eth_only, no_eth)

# day_close <- max_date1 %>%
#   group_by(data_date) %>%
#   filter(lubridate::hour(datetime) == max(lubridate::hour(datetime))) %>%
#   filter(lubridate::minute(datetime) == max(lubridate::minute(datetime))) %>%
#   ungroup()

#Trying with below line to keep data at the minute level
day_close <- max_date1


close_diff <- function(x) x - lead(x,1)
lead1 <- function(x) lead(x,1)

day_close1 <- day_close %>%
  mutate_at(vars(contains('Close')), list(yesterday = lead1)) %>%
  mutate_at(vars(contains('Close')), list(closed_diff = close_diff))

leads <- c(1:100, 200, 500, 1000)

for(i in leads) {
  
  day_close1[[paste0('lead_',i)]] <- lead(day_close1$Close,i)
}


model <- day_close1 %>%
  ungroup() %>%
  mutate(gain = Close - lead_1) %>%
  select(gain, data_date, starts_with("lead_"), starts_with('moving_'), contains("_yesterday"),)




####
#Start h2o testing
####

h2o.init(
  nthreads = -1,
  max_mem_size = "50g"
)
h2o.removeAll()

leave_out <- filter(model, data_date >= max(model$data_date)-150)
leave_out_h2o <- as.h2o(leave_out)

model1 <- anti_join(model, leave_out)
h2o_model <- as.h2o(model1)


split <- h2o.splitFrame(
  h2o_model,
  c(.6,.2),
  seed = 123
)


train <- h2o.assign(split[[1]], 'train')
valid <- h2o.assign(split[[2]], 'valid')
test <- h2o.assign(split[[3]], 'test')


gbm_model <- h2o.gbm(
  training_frame = train,
  validation_frame = valid,
  x = 3:ncol(train), 
  model_id = "gbm_covType1",     ## name the model in H2O
  y = 1,
  seed = 123)

summary(gbm_model)


test1 <- h2o.predict(
  gbm_model,
  test
)

test2 <- test
test2$prediction <- test1

test2 <- as_tibble(test2) %>%
  mutate(sign_match = case_when(gain < 0 & prediction < 0 ~ 1,
                                gain > 0 & prediction > 0 ~ 1,
                                T ~ 0))
direction_match_rate <- mean(test2$sign_match)
direction_match_rate

####
# model 2
####


gbm_model <- h2o.gbm(
  training_frame = train,
  validation_frame = valid,
  x = 3:ncol(train), 
  model_id = "gbm_covType1",     ## name the model in H2O
  y = 1,
  learn_rate = .2,
  max_depth = 10,
  seed = 123)

summary(gbm_model)


test1 <- h2o.predict(
  gbm_model,
  test
)

test2 <- test
test2$prediction <- test1

test2 <- as_tibble(test2) %>%
  mutate(sign_match = case_when(gain < 0 & prediction < 0 ~ 1,
                                gain > 0 & prediction > 0 ~ 1,
                                T ~ 0))
direction_match_rate <- mean(test2$sign_match)
direction_match_rate

out_test <- h2o.predict(
  gbm_model,
  leave_out_h2o
)

leave_out_h2o$prediction <- out_test
leave_out_pred <- as_tibble(leave_out_h2o) %>%  
  mutate(sign_match = case_when(gain < 0 & prediction < 0 ~ 1,
                                gain > 0 & prediction > 0 ~ 1,
                                T ~ 0))
mean(leave_out_pred$sign_match)



####
# model 3
####


gbm_model <- h2o.gbm(
  training_frame = train,
  validation_frame = valid,
  x = 3:ncol(train), 
  y = 1,
  ntrees = 75,
  learn_rate = .3,
  sample_rate = 0.9,          
  col_sample_rate = 0.9,       
  stopping_rounds = 2,        
  stopping_tolerance = 0.01,  
  score_each_iteration = T,   
  max_depth = 10,
  seed = 123)

summary(gbm_model)


test1 <- h2o.predict(
  gbm_model,
  test
)

test2 <- test
test2$prediction <- test1

test2 <- as_tibble(test2) %>%
  mutate(sign_match = case_when(gain < 0 & prediction < 0 ~ 1,
                                gain > 0 & prediction > 0 ~ 1,
                                T ~ 0))
direction_match_rate <- mean(test2$sign_match)
direction_match_rate



