unique_client_id <- function() {
 
  if(!dir.exists('client id')) dir.create('client id') 
  
  if(!file.exists('client id/client_id.rds')) {
    id <- 1
  } else {
    id <- readRDS('client id/client_id.rds')
  }
  
  id_up <- id+1
  saveRDS(id_up, 'client id/client_id.rds')
  id <- as.character(id)
  return(id)
  
}