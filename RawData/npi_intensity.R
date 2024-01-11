NPI.INTENSITY <-  function(data)
  
{
  
  npi_intensity <- data.frame ("A1"=NA,"A2"=NA, "A3"=NA, "A4"=NA, "M1"=NA,
                               "M2"=NA, "M3"=NA, "P1"=NA)  
  g = 1
  for ( i in 1 : (dim(data)[2]/2) )
  {
    for ( j in 1 : dim(data)[1] )
    {
      npi_intensity[j,i] <- (data[j,g] - 0.5*( 1 - data[j,g+1] ))/max(data[,g])
    }
    g = g + 2
    if ( g + 1 == i*2 ) break
  }  
  return(npi_intensity)
}

