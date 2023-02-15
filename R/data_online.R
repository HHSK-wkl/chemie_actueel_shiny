data_online <- function(file, url = "https://www.schielandendekrimpenerwaard.nl/kaart/waterkwaliteit/alle_wkl_metingen/data"){
  url <- paste0(url, "/", file)
  
  temp <- tempfile() 
  download.file(url, temp)
  res <- readRDS(temp)
  unlink(temp)
  res
}


# fys_chem <- data_online("fys_chem.rds")
# meetpunten <- data_online("meetpunten.rds")
# parameters <- data_online("parameters.rds")
