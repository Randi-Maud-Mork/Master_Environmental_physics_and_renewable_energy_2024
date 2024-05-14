## Convective triggering potential

# integral between the moist adiabatic lapse rate and the actual temperature 
# between 100hPa and 300hPa above ground level


# function for calculation of integrals 
calculate_integral_trapezian_rule <- function(vec, air_pressure){
  areas <- c()
  for (n in c(2:length(vec))){
    area <- ((air_pressure[n-1] + air_pressure[n])/2)*(vec[n]-vec[n-1])
    areas <- append(areas, area)
    
  }
  int <- sum(areas)
  
  return(int)
}

# Defining a function to calculate CTP
  calculate_CTP<- function(air_temperature, altitude, MALR, air_pressure){
    MALR_temp = air_temperature[1] + (MALR/1000) * (altitude - altitude[1])
    MALR_int <- calculate_integral_trapezian_rule(MALR_temp, air_pressure)
    air_temperature_int <- calculate_integral_trapezian_rule(air_temperature, air_pressure)
    CTP <- MALR_int-air_temperature_int
  
  return(CTP)
}


  