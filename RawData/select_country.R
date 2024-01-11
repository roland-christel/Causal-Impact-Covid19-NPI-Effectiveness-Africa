
subset_country <- function(dataBase) {
  dat <- data.frame()
  for (i in 1:25){
    Country <- c("NGA","SEN","TGO","GHA","BEN",
                 "EGY", "DZA", "MAR", "LBY", 
                 "TUN","GAB", "COD", 
                 "CMR", "CAF","TCD",
                 "SWZ","ZAF", "MRT", "ZMB",
                 "NAM", "KEN", "SDN", "ETH","TZA", 
                 "RWA")
    
    data <- dataBase  |>  
      filter(
        CountryCode == Country[i]
      )
    
    dat <- rbind(data, dat)  
    
  }
  return(dat)
}
