

filtering <- function( a)
{
 country <- unique(a$CountryCode)
 data <- data.frame()
 
 for( p in  1: length(country))
 {
   c <- country[p]
   
   if (c == "RWA")
     {
   dat <- a %>% 
              filter(CountryCode == "RWA" & Time <= 338)
   data <- rbind(dat,data)
     }
   else if ( c == "BEN" )
     {
     dat <- a %>% 
       filter(CountryCode == "BEN" & Time <= 381)
     data <- rbind(dat,data) 
     }
   else if ( c == "CAF" )
   {
     dat <- a %>% 
       filter(CountryCode == "CAF" & Time <= 431)
     data <- rbind(dat,data) 
   }
   else if ( c == "CMR" )
   {
     dat <- a %>% 
       filter(CountryCode == "CMR" & Time <= 402)
     data <- rbind(dat,data) 
   }
   else if ( c == "COD" )
   {
     dat <- a %>% 
       filter(CountryCode == "COD" & Time <= 404)
     data <- rbind(dat,data) 
   }
   else if ( c == "DZA" )
   {
     dat <- a %>% 
       filter(CountryCode == "DZA" & Time <= 341)
     data <- rbind(dat,data) 
   }
   else if ( c == "EGY" )
   {
     dat <- a %>% 
       filter(CountryCode == "EGY" & Time <= 345)
     data <- rbind(dat,data) 
   }
   else if ( c == "ETH" )
   {
     dat <- a %>% 
       filter(CountryCode == "ETH" & Time <= 365)
     data <- rbind(dat,data) 
   }
   else if ( c == "GAB" )
   {
     dat <- a %>% 
       filter(CountryCode == "GAB" & Time <= 374)
     data <- rbind(dat,data) 
   }
   else if ( c == "GHA" )
   {
     dat <- a %>% 
       filter(CountryCode == "GHA" & Time <= 353)
     data <- rbind(dat,data) 
   }
  else if ( c == "KEN" )
   {
     dat <- a %>% 
       filter(CountryCode == "KEN" & Time <= 355)
     data <- rbind(dat,data) 
   }
   else if ( c == "LBY" )
   {
     dat <- a %>% 
       filter(CountryCode == "LBY" & Time <= 382)
     data <- rbind(dat,data) 
   }
   else if ( c == "MAR" )
   {
     dat <- a %>% 
       filter(CountryCode == "MAR" & Time <= 332)
     data <- rbind(dat,data) 
   }
   else if ( c == "MRT" )
   {
     dat <- a %>% 
       filter(CountryCode == "MRT" & Time <= 377)
     data <- rbind(dat,data) 
   }
   else if ( c == "NAM" )
   {
     dat <- a %>% 
       filter(CountryCode == "NAM" & Time <= 370)
     data <- rbind(dat,data) 
   }
   else if ( c == "NGA" )
   {
     dat <- a %>% 
       filter(CountryCode == "NGA" & Time <= 371)
     data <- rbind(dat,data) 
   }
   else if ( c == "SDN" )
   {
     dat <- a %>% 
       filter(CountryCode == "SDN" & Time <= 360)
     data <- rbind(dat,data) 
   }
   else if ( c == "SEN" )
   {
     dat <- a %>% 
       filter(CountryCode == "SEN" & Time <= 358)
     data <- rbind(dat,data) 
   }
   else if ( c == "SWZ" )
   {
     dat <- a %>% 
       filter(CountryCode == "SWZ" & Time <= 375)
     data <- rbind(dat,data) 
   }
   else if ( c == "TCD" )
   {
     dat <- a %>% 
       filter(CountryCode == "TCD" & Time <= 442)
     data <- rbind(dat,data) 
   }
   else if ( c == "TGO" )
   {
     dat <- a %>% 
       filter(CountryCode == "TGO" & Time <= 369)
     data <- rbind(dat,data) 
   }
   else if ( c == "TUN" )
   {
     dat <- a %>% 
       filter(CountryCode == "TUN" & Time <= 374)
     data <- rbind(dat,data) 
   }
   else if ( c == "TZA" )
   {
     dat <- a %>% 
       filter(CountryCode == "TZA" & Time <= 499)
     data <- rbind(dat,data) 
   }
   else if ( c == "ZAM" )
   {
     dat <- a %>% 
       filter(CountryCode == "ZAM" & Time <= 394)
     data <- rbind(dat,data) 
   }
}
 return(data)
}
   
   

