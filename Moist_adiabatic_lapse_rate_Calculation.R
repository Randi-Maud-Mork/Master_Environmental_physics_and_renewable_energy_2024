library(data.table)
######################
# Script to calculate moist adiabatic lapse rate (MALR)


# Pre-calculations needed to calculated MALR
# 1. Calculate saturation vapor pressure
# 2. Calculate saturation mixing ratio
# 3. Calculate dry adiabatic lapse rate
# 4. Calculate moist adiabatic lapse rate


# Function to calculate Saturation Vapor Pressure (e_s)
calculate_saturation_vapor_pressure <- function(temperature) {
  # Constants
  L_v <- 2.5e6  # Latent heat of vaporization (J/kg)
  R_v <- 461    # Gas constant for water vapor (J/(kg*K))
  
  # Calculate saturation vapor pressure
  e_s <- 611 * exp((L_v / (R_v * temperature)) * (1 / 273.15 - 1 / temperature))
  
  return(e_s)
}

##################
# Function to calculate Saturation Mixing Ratio (w_s)
calculate_saturation_mixing_ratio <- function(temperature, pressure) {
  # Constants
  R_d <- 287    # Gas constant for dry air (J/(kg*K))
  
  # Calculate saturation vapor pressure
  e_s <- calculate_saturation_vapor_pressure(temperature)
  
  # Calculate saturation mixing ratio
  w_s <- 0.622 * e_s / (pressure - e_s)
  
  return(w_s)
}


# Function to calculate dry adiabatic lapse rate
calculate_dry_adiabatic_lapse_rate <- function(temperature, pressure) {
  # Constants
  R_d <- 287  # Gas constant for dry air (J/(kg*K))
  
  # Calculate dry adiabatic lapse rate
  gamma_d <- -R_d / (temperature )
  
  return(gamma_d)
}

######
# Function to calculate the moist adiabatic lapse rate
calculate_moist_adiabatic_lapse_rate <- function(temperature, pressure, w_s) {
  # Constants
  L_v <- 2.5e6  # Latent heat of vaporization (J/kg)
  c_p <- 1005   # Specific heat of dry air (J/(kg*K))
  R_d <- 287    # Gas constant for dry air (J/(kg*K))
  R_v <- 461    # Gas constant for water vapor (J/(kg*K))
  
  # Calculate moist adiabatic lapse rate
  gamma_d <- calculate_dry_adiabatic_lapse_rate(temperature, pressure)
  gamma_s <- gamma_d * ((1 + w_s * L_v / (R_d * temperature)) / (1 + w_s * L_v^2 / (c_p * R_v * temperature^2)))
  
  return(gamma_s)
}

split_dataset_by_date <- function() {
  ans <- DT[, list(list(.SD)), by=date(dateTime)]$V1
  setattr(ans,'names', unique(date(DT$dateTime))) # sets names by reference, no copy here.
}


split_dataset_by_dateTime <- function() {
  ans <- DT[, list(list(.SD)), by=ymd_hms(dateTime)]$V1
  setattr(ans,'names', unique(date(DT$dateTime))) # sets names by reference, no copy here.
}

### 2021 ### 

sounding_21_data <- read.csv('sounding_21')
DT <- as.data.table(sounding_21_data)
split_data_21 <- split_dataset_by_dateTime()

data_with_MALR <- list()
for (n in split_data_21) {
  #new day
  temperature <- n$air_temperature
  pressure <- n$air_pressure
  
  temperature_profile <- n$air_temperature
  pressure_levels <- n$air_pressure
  
  # Calculate saturation mixing ratio using the function
  w_s <- saturation_mixing_ratio <- calculate_saturation_mixing_ratio(temperature_profile, pressure_levels)
  # Calculate MALR
  MALR <- calculate_moist_adiabatic_lapse_rate(temperature,pressure,w_s)
  
  new_rows<- cbind(n,MALR)
  data_with_MALR <- rbind(data_with_MALR,new_rows)
  
}

Data_21_with_MALR <- cbind(DT$dateTime, data_with_MALR)
DT <- as.data.table(Data_21_with_MALR)
names(DT)[names(DT) == 'V1'] <- 'dateTime'
Data_21_with_MALR <- split_dataset_by_dateTime()

### 2022 ### 

sounding_22_data <- read.csv('sounding_22')
DT <- as.data.table(sounding_22_data)
split_data_22 <- split_dataset_by_dateTime()

data_22_with_MALR <- list()
for (n in split_data_22) {
  #new day
  temperature <- n$air_temperature
  pressure <- n$air_pressure
  
  temperature_profile <- n$air_temperature
  pressure_levels <- n$air_pressure
  
  # Calculate saturation mixing ratio using the function
  w_s <- saturation_mixing_ratio <- calculate_saturation_mixing_ratio(temperature_profile, pressure_levels)
  # Calculate MALR
  MALR <- calculate_moist_adiabatic_lapse_rate(temperature,pressure,w_s)
  
  new_rows<- cbind(n,MALR)
  data_22_with_MALR <- rbind(data_22_with_MALR,new_rows)
  
}

Data_22_with_MALR <- cbind(DT$dateTime, data_22_with_MALR)
DT <- as.data.table(Data_22_with_MALR)
names(DT)[names(DT) == 'V1'] <- 'dateTime'
Data_22_with_MALR <- split_dataset_by_dateTime()

### 2023 ### 

sounding_23_data <- read.csv('sounding_23')
DT <- as.data.table(sounding_23_data)
split_data_23 <- split_dataset_by_dateTime()

data_with_MALR <- list()
for (n in split_data_23) {
  #new day
  temperature <- n$air_temperature
  pressure <- n$air_pressure
  
  temperature_profile <- n$air_temperature
  pressure_levels <- n$air_pressure
  
  # Calculate saturation mixing ratio using the function
  w_s <- saturation_mixing_ratio <- calculate_saturation_mixing_ratio(temperature_profile, pressure_levels)
  # Calculate MALR
  MALR <- calculate_moist_adiabatic_lapse_rate(temperature,pressure,w_s)

  new_rows<- cbind(n,MALR)
  data_with_MALR <- rbind(data_with_MALR,new_rows)
  
}

Data_23_with_MALR <- cbind(DT$dateTime, data_with_MALR)
DT <- as.data.table(Data_23_with_MALR)
names(DT)[names(DT) == 'V1'] <- 'dateTime'
Data_23_with_MALR <- split_dataset_by_dateTime()










