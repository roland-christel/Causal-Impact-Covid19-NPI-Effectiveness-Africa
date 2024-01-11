
backfilling <- function(a) 
  
{ 
  # This function helps correct the discrepancy observes in the cumulative number
  # of confirmed cases. 
  # It takes as argument a which is the data set containing Region, CountryCode, 
  # Date, Time, A1, A1_Flag, ... ,H1, Cum_ConfirmedCases, Cum_ConfirmedDeaths, 
  # Daily_ConfirmedCases, Daily_ConfirmedDeaths
  
  list_result <- list()               # a list to keep the final results 
  
  country <- unique(a$CountryCode)    # Vector of the 25 countries 
  
  result_cases   <- vector()         # vector to save the countries with discrepancy in cum_cases

  result_deaths  <- vector()         # vector to save the countries with discrepancy in cum_deaths
 
 
    
  for (i in 1 : length(country)) 
  {
    
    d <- a[a$CountryCode==country[i],]                    # select each country 
    
    for(j in 1 : (nrow(d)-1)) 
    {
      if( d$Cum_ConfirmedCases[j+1] < d$Cum_ConfirmedCases[j]) # check if the cumulative has decreased 
        
      {
        result_cases[j] <- unique(d$CountryCode)  # in case of decrease, the code of the country is saved                   
        
      }
    }
  }
  
result_countries_cases <- result_cases[which(!is.na(result_cases))]

list_result[[1]] <- result_countries_cases
  
a_new_cases  <- filter(a, !CountryCode %in% result_countries_cases)  # filtering the data set by keeping only 
                                                                       # the countries that do not show any discrepancy
  
  for(k in 1:length(result_countries_cases))                   # this loop, first select the data related to each 
                                                               # country having a discrepancy next it applies a backfilling.
  {
    dt       <- a[a$CountryCode==result_countries_cases[k], ]
    
    for(m in nrow(dt) : 2 ) 
    {
      if (dt[m,"Cum_ConfirmedCases"] < dt[m-1,"Cum_ConfirmedCases"])
        
      {
        
        dt[m-1,"Cum_ConfirmedCases"] <- dt[m,"Cum_ConfirmedCases"]
       
      }
    }
    
 a_new_cases <-  rbind(dt, a_new_cases)    # after applying the back filling, the data for this particular country 
                                           # is added to main data set 
    
    
  }
  a_new_cases  <- a_new_cases[order(a_new_cases$CountryCode), ]

  list_result[[2]] <- a_new_cases
 
  for (i in 1 : length(country)) 
  {
    
    d <- a[a$CountryCode==country[i],] # select each country 
    
    for(j in 1 : (nrow(d)-1)) 
    {
      if( d$Cum_ConfirmedDeaths[j+1] < d$Cum_ConfirmedDeaths[j]) # check if the cumulative has decreased 
        
      {
        result_deaths[j] <- unique(d$CountryCode)  # in case of decrease, the code of the country is saved                   
        
      }
    }
  }
  
  result_countries_deaths <- result_deaths[which(!is.na(result_deaths))]
  
  list_result[[3]] <- result_countries_deaths
  
  a_new_deaths  <- filter(a, !CountryCode %in% result_countries_deaths)  # filtering the data set by keeping only 
                                                           # the countries that do not show any discrepancy
  
  for(k in 1:length(result_countries_deaths))                   # this loop, first select the data related to each 
    # country having a discrepancy next it applies a backfilling.
  {
    dt       <- a[a$CountryCode==result_countries_deaths[k], ]
    
    for(m in nrow(dt) : 2 ) 
    {
      if (dt[m,"Cum_ConfirmedDeaths"] < dt[m-1,"Cum_ConfirmedDeaths"])
        
      {
        
        dt[m-1,"Cum_ConfirmedDeaths"] <- dt[m,"Cum_ConfirmedDeaths"]
        
      }
      
    }
    
    a_new_deaths <-  rbind(dt, a_new_deaths)    # after applying the back filling, the data for this particular country 
                                                # is added to main data set 
    
    
  }
  
  a_new_deaths <- a_new_deaths[order(a_new_deaths$CountryCode), ]
  
  list_result[[4]]          <- a_new_deaths
  
  a_new                     <- a_new_cases
  
  a_new$Cum_ConfirmedDeaths <- a_new_deaths$Cum_ConfirmedDeaths

  list_result[[5]] <- a_new
  
  return(list_result)               # It returns a list containing a vector of the countries 
                                    # that showed a discrepancy and the corrected data set. 

}
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  



