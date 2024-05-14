# Comparison of Reanalysis and real sounding data
# Extracting only comparable pressure levels for each sounding
pressure_vec <- c(1000, 975, 950, 925, 900, 875, 850, 825, 800, 775, 750, 700)

sounding_filtered_21 <- c()
for (list in split_data_21){
  new_list <- c()
  for (n in pressure_vec){
    row <- list[which.min(abs(n-list$air_pressure))]
    new_list <- rbind(new_list, row)
  }
  sounding_filtered_21 <- append(sounding_filtered_21, list(new_list))
}
names(sounding_filtered_21) <- names(split_data_21)
sounding_filtered_21 <- sounding_filtered_21[-2]

sounding_filtered_22 <- c()
for (list in split_data_22){
  new_list <- c()
  for (n in pressure_vec){
    row <- list[which.min(abs(n-list$air_pressure))]
    new_list <- rbind(new_list, row)
  }
  sounding_filtered_22 <- append(sounding_filtered_22, list(new_list))
}
names(sounding_filtered_22) <- names(split_data_22)
sounding_filtered_22 <- sounding_filtered_22[-c(7,13,14,17,18)]

sounding_filtered_23 <- c()
for (list in split_data_23){
  new_list <- c()
  for (n in pressure_vec){
    row <- list[which.min(abs(n-list$air_pressure))]
    new_list <- rbind(new_list, row)
  }
  sounding_filtered_23 <- append(sounding_filtered_23, list(new_list))
}
names(sounding_filtered_23) <- names(split_data_23)
sounding_filtered_23 <- sounding_filtered_23[-c(10,11,13,14,16,18,20)]

# Combining every sounding into one df for 3 years

Sounding_filtered_21_df <- do.call(rbind, sounding_filtered_21)
Sounding_filtered_22_df <- do.call(rbind, sounding_filtered_22)
Sounding_filtered_23_df <- do.call(rbind, sounding_filtered_23)

DateTime <- rep(names(sounding_filtered_21), each = length(pressure_vec))
Sounding_filtered_21_df <- cbind(DateTime, Sounding_filtered_21_df)

DateTime <- rep(names(sounding_filtered_22), each = length(pressure_vec))
Sounding_filtered_22_df <- cbind(DateTime, Sounding_filtered_22_df)

DateTime <- rep(names(sounding_filtered_23), each = length(pressure_vec))
Sounding_filtered_23_df <- cbind(DateTime, Sounding_filtered_23_df)

Sounding_filtered_df <- rbind(Sounding_filtered_21_df, Sounding_filtered_22_df, Sounding_filtered_23_df)



# Filter out only sounding dates in reanalysis data


Reanalysis_21 <- do.call(rbind, Reanalysis_data_21_with_MALR)
Reanalysis_22 <- do.call(rbind, Reanalysis_data_22_with_MALR)
Reanalysis_23 <- do.call(rbind, Reanalysis_data_23_with_MALR)

Reanalysis_21 <- filter(Reanalysis_21, air_pressure %in% pressure_vec)
Reanalysis_22 <- filter(Reanalysis_22, air_pressure %in% pressure_vec)
Reanalysis_23 <- filter(Reanalysis_23, air_pressure %in% pressure_vec)


DateTime <- rep(names(Reanalysis_data_21_with_MALR), each = length(pressure_vec))
Reanalysis_21 <- cbind(DateTime, Reanalysis_21)

DateTime <- rep(names(Reanalysis_data_22_with_MALR), each = length(pressure_vec))
Reanalysis_22 <- cbind(DateTime, Reanalysis_22)

DateTime <- rep(names(Reanalysis_data_23_with_MALR), each = length(pressure_vec))
Reanalysis_23 <- cbind(DateTime, Reanalysis_23)

Reanalysis_21_filtered <- filter(Reanalysis_21, date(DateTime) %in% date(Sounding_filtered_21_df$DateTime))
Reanalysis_22_filtered <- filter(Reanalysis_22, date(DateTime) %in% date(Sounding_filtered_22_df$DateTime))
Reanalysis_23_filtered <- filter(Reanalysis_23, date(DateTime) %in% date(Sounding_filtered_23_df$DateTime))
Reanalysis_23_filtered <- Reanalysis_23_filtered[1:180,]
Reanalysis_22_filtered <- Reanalysis_22_filtered[1:264,]
# Combining reanalysis data into one df for 3 years

Reanalysis_sd_filtered <- rbind(Reanalysis_21_filtered, Reanalysis_22_filtered, Reanalysis_23_filtered)


## Correlation analysis of air temperature ## 

Air_temp_df <- data.frame( DateTime = Sounding_filtered_df$DateTime ,
                           S_T_a = Sounding_filtered_df$air_temperature,
                           R_T_a = Reanalysis_sd_filtered$air_temperature,
                           air_pressure = Reanalysis_sd_filtered$air_pressure)

# Air_temp_df <- Air_temp_df%>%
#   filter(air_pressure>700, air_pressure<900)

Combined_fig_T_a <- ggplot(data = Air_temp_df, aes(x=S_T_a, y = R_T_a))+
  geom_abline(slope=1, intercept=0)+
  geom_point(aes(color = as.factor(month(DateTime))))+
  scale_color_manual(values = c("#E69F00", "#009E73", "#0072B2", "#D55E00", "#CC79A7"),
                     labels = c("5" = "May", 
                                "6" = "June",
                                "7" = "July",
                                "8" = "August",
                                "9" = "September"))+
  xlab("Sounding: air temperature [K]")+
  ylab("Reanalysis: air temperature [K]")+
  labs(color = "Month")+
  theme(legend.text = element_text(size= 14),
        legend.title = element_text(size=16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size =14),
        plot.title = element_text(size = 18))+
  scale_y_continuous(limits = c(260, 300), n.breaks = 6)+
  scale_x_continuous(limits = c(260, 300), n.breaks = 6)
Combined_fig_T_a


## Correlation of dew point temperature ##

dew_temp_df <- data.frame(DateTime = Sounding_filtered_df$DateTime  ,
                          S_T_d = Sounding_filtered_df$dew_point_temperature,
                          R_T_d = Reanalysis_sd_filtered$dew_point_temperature,
                          air_pressure = Reanalysis_sd_filtered$air_pressure)


Combined_fig_T_d <- ggplot(data = dew_temp_df, aes(x=S_T_d, y = R_T_d))+
  geom_abline(slope=1, intercept=0)+
  geom_point(aes(color = as.factor(month(DateTime))))+
  scale_color_manual(values = c("#E69F00", "#009E73", "#0072B2", "#D55E00", "#CC79A7"),
                     labels = c("5" = "May", 
                                "6" = "June",
                                "7" = "July",
                                "8" = "August",
                                "9" = "September"))+
  xlab("Sounding: dew point temperature [K]")+
  ylab("Reanalysis: dew point temperature [K]")+
  labs(color = "Month")+
  theme(legend.text = element_text(size= 14),
        legend.title = element_text(size=16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size =14),
        plot.title = element_text(size = 18))+
  scale_y_continuous(limits= c(230, 300), n.breaks = 6)+
  scale_x_continuous(limits = c(230,300), n.breaks = 6)

Combined_fig_T_d

## Correlation of relative humidity ##
RH_df <- data.frame( DateTime =  Sounding_filtered_df$DateTime,
                     S_RH = Sounding_filtered_df$relative_humidity,
                     R_RH = Reanalysis_sd_filtered$Relative_humidity,
                     air_pressure = Reanalysis_sd_filtered$air_pressure)


Combined_fig_RH <- ggplot(data = RH_df, aes(x=S_RH, y = R_RH))+
  geom_abline(slope=1, intercept=0)+
  geom_point(aes(color = as.factor(month(DateTime))))+
  scale_color_manual(values = c("#E69F00", "#009E73", "#0072B2", "#D55E00", "#CC79A7"),
                     labels = c("5" = "May", 
                                "6" = "June",
                                "7" = "July",
                                "8" = "August",
                                "9" = "September"))+
  labs( color = "Month")+
  xlab("Sounding: Relative humidity [%]")+
  ylab("Reanalysis: Relative humidity [%]")+
  theme(legend.text = element_text(size= 14),
        legend.title = element_text(size=16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size =14),
        plot.title = element_text(size = 18))+
  scale_y_continuous(limits = c(0, 105), n.breaks = 6 )+
  scale_x_continuous(limits = c(0, 105), n.breaks = 6)
Combined_fig_RH

## Correlation of soil moisture
SM_Soeraasjordet_21 <- data.frame(DateTime = Combined_df_21_repl$DateTime,
                                  SM_10_perc_1 = Combined_df_21_repl$Soil_moisture)
SM_Soeraasjordet_22 <- data.frame(DateTime = Combined_df_22_repl$DateTime,
                                  SM_10_perc_1 = Combined_df_22_repl$Soil_moisture)
SM_Soeraasjordet_23 <- data.frame(DateTime = Combined_df_23_repl$DateTime,
                                  SM_10_perc_1 = Combined_df_23_repl$Soil_moisture)
SM_Soeraasjordet <- rbind(SM_Soeraasjordet_21, SM_Soeraasjordet_22, SM_Soeraasjordet_23)

SM_reanalysis_21 <- data.frame(DateTime = Reanalysis_21_EC$DateTime,
                               SM_reanalysis = Reanalysis_21_EC$SM)
SM_reanalysis_22 <- data.frame(DateTime = Reanalysis_22_EC$DateTime,
                               SM_reanalysis = Reanalysis_22_EC$SM)
SM_reanalysis_23 <- data.frame(DateTime = Reanalysis_23_EC$DateTime,
                               SM_reanalysis = Reanalysis_23_EC$SM)
SM_reanalysis <- rbind(SM_reanalysis_21, SM_reanalysis_22, SM_reanalysis_23)


SM_df <- data.frame( DateTime =  SM_Soeraasjordet$DateTime,
                     SM_Soeraasjordet = SM_Soeraasjordet$SM_10_perc_1,
                     SM_reanalysis = SM_reanalysis$SM_reanalysis*100)

SM_df <- drop_na(SM_df)

Combined_fig_SM <- ggplot(data = SM_df, aes(x=SM_Soeraasjordet, y = SM_reanalysis))+
  geom_abline(slope=1, intercept=0)+
  geom_point(aes(color = as.factor(month(DateTime))))+
  scale_color_manual(values = c("#E69F00", "#009E73", "#0072B2", "#D55E00", "#CC79A7"),
                     labels = c("5" = "May", 
                                "6" = "June",
                                "7" = "July",
                                "8" = "August",
                                "9" = "September"))+
  labs(color = "Month")+
  xlab("Soil moisture at Søråsjordet [%]")+
  ylab("Soil moisture from reanalysis [%]")+
  theme(legend.text = element_text(size= 14),
        legend.title = element_text(size=16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size =14),
        plot.title = element_text(size = 18))+
  scale_y_continuous(limits = c(0, 65), n.breaks = 6 )+
  scale_x_continuous(limits = c(0, 65), n.breaks = 6)
Combined_fig_SM


## Correlations of sensible and latent heat fluxes ##

HF_soeraasjordet_21 <- data.frame(DateTime = Combined_df_21_repl$DateTime,
                               LE = Combined_df_21_repl$Latent_heat,
                               SH = Combined_df_21_repl$Sensible_heat)
HF_soeraasjordet_22 <- data.frame(DateTime = Combined_df_22_repl$DateTime,
                                  LE = Combined_df_22_repl$Latent_heat,
                                  SH = Combined_df_22_repl$Sensible_heat)
HF_soeraasjordet_23 <- data.frame(DateTime = Combined_df_23_repl$DateTime,
                                  LE = Combined_df_23_repl$Latent_heat,
                                  SH = Combined_df_23_repl$Sensible_heat)
HF_soeraasjordet <- rbind(HF_soeraasjordet_21, HF_soeraasjordet_22, HF_soeraasjordet_23)

HF_df <- data.frame(DateTime = HF_soeraasjordet$DateTime,
                    LE_S = HF_soeraasjordet$LE,
                    SH_S = HF_soeraasjordet$SH,
                    LE_R = Reanalysis_EC$LE,
                    SH_R = Reanalysis_EC$SH)
## Sensible heat flux ## 

Combined_fig_SH <- ggplot(data = HF_df, aes(x=SH_S, y = SH_R))+
  geom_abline(slope=1, intercept=0)+
  geom_point(aes(color = as.factor(month(DateTime))))+
  scale_color_manual(values = cbPalette <- c("#E69F00", "#009E73", "#0072B2", "#D55E00", "#CC79A7"),
                     labels = c("5" = "May", 
                                "6" = "June",
                                "7" = "July",
                                "8" = "August",
                                "9" = "September"))+
  labs(color = "Month")+
  xlab(expression(paste("Sensible heat flux at Søråsjordet [ ", W/m^2," ]")))+
  ylab(expression(paste("Sensible heat flux from reanalysis [ ", W/m^2, " ]")))+
  theme(legend.text = element_text(size= 14),
        legend.title = element_text(size=16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size =14),
        plot.title = element_text(size = 18))
  #scale_y_continuous(limits = c(0, 65), n.breaks = 6 )+
  #scale_x_continuous(limits = c(0, 65), n.breaks = 6)
Combined_fig_SH

## Latent heat flux

Combined_fig_LE <- ggplot(data = HF_df, aes(x=LE_S, y = LE_R))+
  geom_abline(slope=1, intercept=0)+
  geom_point(aes(color = as.factor(month(DateTime))))+
  scale_color_manual(values = cbPalette <- c("#E69F00", "#009E73", "#0072B2", "#D55E00", "#CC79A7"),
                     labels = c("5" = "May", 
                                "6" = "June",
                                "7" = "July",
                                "8" = "August",
                                "9" = "September"))+ 
  labs(color = "Month")+
  xlab(expression(paste("Latent heat flux at Søråsjordet [ ", W/m^2," ]")))+
  ylab(expression(paste("Latent heat flux from reanalysis [ ", W/m^2, " ]")))+
  scale_x_continuous(limits = c(-1500, 1500), n.breaks = 6)+
  theme(legend.text = element_text(size= 14),
        legend.title = element_text(size=16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size =14),
        plot.title = element_text(size = 18))
Combined_fig_LE
