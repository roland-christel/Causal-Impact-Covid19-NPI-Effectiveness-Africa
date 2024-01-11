## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++###
##                            CODE R: Cleaning                               ###
##                                                                           ###
##        Assessing The Effectiveness of NPI on COVID-19                     ###
##                                                                           ###
##                                                                           ###
## Updated: July, 2022                                                       ###
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++###

## Clear R memory

rm(list=ls())                                                  

## Package to load 

if(!require("tidyverse")){install.packages("tidyverse")}
if(!require("readxl")){install.packages("readxl")}
if(!require("rio")){install.packages("rio")}

## Download NPI Data from OxCGRT API 

#URL = "https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/0cd554946cd8b5c12d3f69a78feddff1960436c0/data/OxCGRT_latest.csv"
#data <- import(URL) 
#save(data,file = "data.RData")

## Load the data downloaded
load("RawData/data.RData"); attach(data)  

### 1.Select the set of 25 countries considerd in this study  

##This function helps select the countries of interest

source("RawData/select_country.R")

## Let's select the data of the 25 African countries 
data_a <- subset_country(data)
remove(subset_country)

## detach("package:MASS", unload = TRUE) in case of conflict with the command select()

#load("data_ab.RData")

data_ab <-  data_a |>
  rename(
    ## we renamed the variables related to the 12 NPIs of interest
    
    "A1"                   =  `C1_School closing`, 
    "A1_Flag"              =   C1_Flag,
    "A2"                   =  `C2_Workplace closing`,
    "A2_Flag"              =   C2_Flag, 
    "A3"                   =  `C3_Cancel public events`,
    "A3_Flag"              =   C3_Flag,
    "A4"                   =  `C4_Restrictions on gatherings`, 
    "A4_Flag"              =   C4_Flag,
    "M1"                   =  `C5_Close public transport`,
    "M1_Flag"              =   C5_Flag,
    "M2"                   =  `C6_Stay at home requirements`, 
    "M2_Flag"              =   C6_Flag,
    "M3"                   =  `C7_Restrictions on internal movement`,
    "M3_Flag"              =   C7_Flag, 
    "M4"                   =  `C8_International travel controls`,
    "P1"                   =  `H1_Public information campaigns`,
    "P1_Flag"              =   H1_Flag,
    "P2"                   =  `H2_Testing policy`,
    "P3"                   =  `H3_Contact tracing`,
    "H1"                   =  `H4_Emergency investment in healthcare`,
    "Cum_ConfirmedCases"    =   ConfirmedCases,
    "Cum_ConfirmedDeaths"   =   ConfirmedDeaths
    
  )|> 
  
  select(
    
    ## We select all the variable of interest 
    
    CountryCode,Date,
    A1, A1_Flag, A2, A2_Flag, A3, A3_Flag, A4, A4_Flag, 
    M1, M1_Flag, M2, M2_Flag, M3, M3_Flag, M4,
    P1, P1_Flag, P2, P3, H1, Cum_ConfirmedCases, Cum_ConfirmedDeaths 
    
  ) |>  
  group_by(CountryCode) |> 
  
  filter(
    ## We removed the rows with Confirmed-cases equal to zero or NA
    
    Cum_ConfirmedCases>0
    
  ) |> 
  
  mutate(
    
    ## We created the column Region and Time (=1,2,3,...)
    
    Region =  if_else(CountryCode == "BEN" | CountryCode == "NGA" | CountryCode == "SEN" | CountryCode == "TGO" | CountryCode == "GHA"  , "Western", 
                      ifelse(CountryCode  == "EGY" | CountryCode == "DZA" | CountryCode == "MAR" | CountryCode == "LBY" | CountryCode == "TUN", "Northern", 
                             ifelse(CountryCode  == "CMR" | CountryCode == "CAF" | CountryCode == "TCD" | CountryCode == "GAB" | CountryCode == "COD", "Central", 
                                    ifelse(CountryCode  == "SWZ" | CountryCode == "ZAF" | CountryCode == "MRT" | CountryCode == "ZMB" | CountryCode == "NAM", "Southern", "Eastern")))),
    Time = 1:n()
  )|> 
  
  relocate(
    
    ## we relocated the variable Region before the codes of the countries 
    
    Region, .before = CountryCode
    
  ) |> 
  relocate(
    
    ## we relocated the variable Time just after the variable Date 
    
    Time, .after = Date
    
  ) |> 
  mutate(
    ## We replaced the missing values in each columns representing the generality by 1
    ## 1 means "general"                   
    across(ends_with("flag"), ~replace(.,is.na(.), 1))
    
  ) |> 
  mutate(
    
    ## The missing values in each columns of NPIs are replaced by 0 
    
    across(everything(), ~replace(.,is.na(.), 0))
    
  ) |> 
  
  mutate(
    
    ## we computed the daily Confirmed Cases and Deaths per country 
    
    Daily_ConfirmedCases  = c(first(Cum_ConfirmedCases),diff(Cum_ConfirmedCases)),
    Daily_ConfirmedDeaths = c(first(Cum_ConfirmedDeaths),diff(Cum_ConfirmedDeaths))
    
  ) |> 
  
  ## this is to deal with some dubious values (confirmed cases and deaths)
  ## some cumulative data were less than the one of the dat before 
  ## this gives negative daily cases which is meaningless
  mutate_at(
    vars(c(Daily_ConfirmedCases,Daily_ConfirmedDeaths)),
    ~ifelse(.<0,0,.)
  ) |> 
  mutate(
    Date = as.Date(as.character(Date), format="%Y%m%d")
  ) |> 
  ungroup()



### 2. NPI Intensities Computations 

## Let's import a function to compute the NPI Intensity for NPI with Flag
#load("data_npi_intensity_with_flag.RData")
#load("data_npi_intensity_without_flag.RData")

source("RawData/npi_intensity.R")

## Let's create the dataset 

data_with_flag    <- data_ab %>% 
  select(
    
    ## We selected NPIs that do have flag
    
    A1, A1_Flag, A2, A2_Flag, A3, A3_Flag, 
    A4, A4_Flag, M1, M1_Flag, M2, M2_Flag,
    M3, M3_Flag, P1, P1_Flag 
    
  )

data_without_flag <- data_ab %>% 
  select(
    
    ## We selected NPIs that do not have flag
    
    M4,P2, P3, H1
    
  )                

# NPI intensity computation 

## It'll take some time
data_npi_intensity_with_flag    <- NPI.INTENSITY(data_with_flag)

data_npi_intensity_without_flag <- data_without_flag %>% 
  mutate(
    M4   =  (M4/max(M4)),
    P2   =  (P2/max(P2)),
    P3   =  (P3/max(P3))
  )
remove(data_with_flag, data_without_flag, NPI.INTENSITY)

## We need to correct the discrepancy in the cumulative data 

source("RawData/backfilling_real.R")

result_backfilling <- backfilling(data_ab)

data_ab <- result_backfilling[[5]]


### 3. Country Specific Data  

## Let's import the data set downloaded from the World Bank website 

data_CSC  <- read_xlsx("RawData/country_specific_data.xlsx",sheet = "data_owid" )


## It contains 23 variables (specific information for each country)

unique(data_CSC$indicator)
colnames(data_CSC)

## Let's create the data set 
data_CSC <- data_CSC  |>  
  select(
    
    ## we selected the variables for the year 2018 because we do have information
    ## for each country 
    
    CountryCode,indicator, value
    
  ) |>  
  mutate(
    
    CountryCode = as.factor(CountryCode)
  )  |>  
  pivot_wider(
    
    ## we transformed the rows to columns 
    names_from = indicator, values_from = value
    
  ) 

### 4. The Main Data Set 

#load("Dat_Set.RData")

Dat_Set <-  cbind(
  
  Daily_ConfirmedCases  = data_ab$Daily_ConfirmedCases,
  
  Daily_ConfirmedDeaths = data_ab$Daily_ConfirmedDeaths,
  
  Cum_ConfirmedCases = data_ab$Cum_ConfirmedCases,
  
  Cum_ConfirmedDeaths = data_ab$Cum_ConfirmedDeaths,
  
  Region=data_ab$Region,CountryCode=data_ab$CountryCode,Date=data_ab$Date,
  
  Time=data_ab$Time,
  
  data_npi_intensity_with_flag,
  
  data_npi_intensity_without_flag
  
  
)

## Let's add the column of country specific data to the main data set 

Dat_Set <- Dat_Set %>% 
  
  left_join(
    
    data_CSC
    
  )
attach(Dat_Set)

remove( data_npi_intensity_with_flag, data_npi_intensity_without_flag, data_CSC)


### 6. Let's generate the data set from the first case to the date the vaccine became available
###    in a given country 

source("RawData/filtering.R")

Dat_Set <- filtering(Dat_Set)

