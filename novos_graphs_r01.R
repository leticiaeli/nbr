## macapa ----

setwd("D:/Frentes de Trabalho/simu_NBR/VNxAC_janela/BRA_AP_Macapa-Alcolumbre.Intl.AP.820980_TMYx.2003-2017/")

file_list = list.files(pattern="*_1.csv") # todos os csv menos o table

library("data.table")

for (i in 1:5){ #eu sei que os 5 primeiros sao hvac
  assign(file_list[i], read.csv(file_list[i])[,c("Date.Time",
                                                 "Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.",
                                                 "DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.",
                                                 "DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.",
                                                 "SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.",
                                                 "SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.",
                                                 "DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.",
                                                 "DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.")])
  for(k in 6:10){ # eu sei que os 5 ultimos sao vn
    assign(file_list[k], read.csv(file_list[k])[,c("Date.Time",
                                                   "Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.",
                                                   "DORMITORIO1.People.Occupant.Count....TimeStep.",
                                                   "DORMITORIO2.People.Occupant.Count....TimeStep.",
                                                   "SALA1.People.Occupant.Count....TimeStep.",
                                                   "DORM2.Zone.Operative.Temperature..C..TimeStep.",
                                                   "DORM2.Zone.Mean.Air.Temperature..C..TimeStep.",
                                                   "DORM1.Zone.Operative.Temperature..C..TimeStep.",
                                                   "DORM1.Zone.Mean.Air.Temperature..C..TimeStep.",
                                                   "SALA.Zone.Operative.Temperature..C..TimeStep.",
                                                   "SALA.Zone.Mean.Air.Temperature..C..TimeStep.")])
  }
  
}

ocup_DORM1 = data.frame(ocup_DORM = vn_Caso1_rj_mac_1.csv$DORMITORIO1.People.Occupant.Count....TimeStep.)
ocup_DORM1$ocup = ifelse(ocup_DORM1$ocup_DORM > 0,1,0)
ocup_DORM1 = subset(ocup_DORM1, select = c("ocup"))

ocup_DORM2 = subset(ocup_DORM1, select = c("ocup"))  ## a ocupacao eh igual

ocup_SALA = data.frame(ocup_DORM = vn_Caso1_rj_mac_1.csv$SALA1.People.Occupant.Count....TimeStep.)
ocup_SALA$ocup = ifelse(ocup_SALA$ocup_DORM > 0,1,0)
ocup_SALA = subset(ocup_SALA, select = c("ocup"))

# ocup = rbind(ocup_dorm1,ocup_dorm2,ocup_sala)
# rm(ocup_dorm1,ocup_dorm2,ocup_sala)

# temperatura ----

c = as.numeric(28) ## limite calor
f = as.numeric(18) ## limite frio

conf_mac_DORM1 = data.frame(APP=c('DORM1'),
                      ocup=ocup_DORM1,
                      temp_ext=vn_Caso1_rj_mac_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.,
                      REF_temp=vn_Caso1_rj_mac_1.csv$DORM1.Zone.Operative.Temperature..C..TimeStep.,
                      REF_cooling=ac1_Caso1_rj_mac_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                      REF_heating=ac1_Caso1_rj_mac_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                      Caso2_temp=vn_Caso2_1.csv$DORM1.Zone.Operative.Temperature..C..TimeStep.,
                      Caso2_cooling=ac1_Caso2_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                      Caso2_heating=ac1_Caso2_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                      Caso3_temp=vn_Caso3_1.csv$DORM1.Zone.Operative.Temperature..C..TimeStep.,
                      Caso3_cooling=ac1_Caso3_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                      Caso3_heating=ac1_Caso3_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                      Caso4_temp=vn_Caso4_1.csv$DORM1.Zone.Operative.Temperature..C..TimeStep.,
                      Caso4_cooling=ac1_Caso4_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                      Caso4_heating=ac1_Caso4_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                      Caso5_temp=vn_Caso5_1.csv$DORM1.Zone.Operative.Temperature..C..TimeStep.,
                      Caso5_cooling=ac1_Caso5_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                      Caso5_heating=ac1_Caso5_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.)

conf_mac_DORM1$EXT_calor = ifelse((conf_mac_DORM1$temp_ext >= c & conf_mac_DORM1$ocup > 0),1,0)
conf_mac_DORM1$EXT_frio = ifelse((conf_mac_DORM1$temp_ext <= f) & (conf_mac_DORM1$ocup > 0),1,0)
conf_mac_DORM1$EXT_conf = ifelse(((conf_mac_DORM1$temp_ext < c) & (conf_mac_DORM1$temp_ext > f) & (conf_mac_DORM1$ocup > 0)),1,0)

conf_mac_DORM1$REF_calor = ifelse((conf_mac_DORM1$REF_temp >= c) & (conf_mac_DORM1$ocup > 0),1,0)
conf_mac_DORM1$REF_frio = ifelse((conf_mac_DORM1$REF_temp <= f) & (conf_mac_DORM1$ocup > 0),1,0)
conf_mac_DORM1$REF_conf = ifelse(((conf_mac_DORM1$REF_temp < c) & (conf_mac_DORM1$REF_temp > f) & (conf_mac_DORM1$ocup > 0)),1,0)
conf_mac_DORM1$REF_cooling = ifelse((conf_mac_DORM1$REF_calor > 0),conf_mac_DORM1$REF_cooling,0)
conf_mac_DORM1$REF_heating = ifelse((conf_mac_DORM1$REF_frio > 0),conf_mac_DORM1$REF_heating,0)

conf_mac_DORM1$Caso2_calor = ifelse((conf_mac_DORM1$Caso2_temp >= c) & (conf_mac_DORM1$ocup > 0),1,0)
conf_mac_DORM1$Caso2_frio = ifelse((conf_mac_DORM1$Caso2_temp <= f) & (conf_mac_DORM1$ocup > 0),1,0)
conf_mac_DORM1$Caso2_conf = ifelse(((conf_mac_DORM1$Caso2_temp < c) & (conf_mac_DORM1$Caso2_temp > f) & (conf_mac_DORM1$ocup > 0)),1,0)
conf_mac_DORM1$Caso2_cooling = ifelse((conf_mac_DORM1$Caso2_calor > 0),conf_mac_DORM1$Caso2_cooling,0)
conf_mac_DORM1$Caso2_heating = ifelse((conf_mac_DORM1$Caso2_frio > 0),conf_mac_DORM1$Caso2_heating,0)

conf_mac_DORM1$Caso3_calor = ifelse((conf_mac_DORM1$Caso3_temp >= c) & (conf_mac_DORM1$ocup > 0),1,0)
conf_mac_DORM1$Caso3_frio = ifelse((conf_mac_DORM1$Caso3_temp <= f) & (conf_mac_DORM1$ocup > 0),1,0)
conf_mac_DORM1$Caso3_conf = ifelse(((conf_mac_DORM1$Caso3_temp < c) & (conf_mac_DORM1$Caso3_temp > f) & (conf_mac_DORM1$ocup > 0)),1,0)
conf_mac_DORM1$Caso3_cooling = ifelse((conf_mac_DORM1$Caso3_calor > 0),conf_mac_DORM1$Caso3_cooling,0)
conf_mac_DORM1$Caso3_heating = ifelse((conf_mac_DORM1$Caso3_frio > 0),conf_mac_DORM1$Caso3_heating,0)

conf_mac_DORM1$Caso4_calor = ifelse((conf_mac_DORM1$Caso4_temp >= c) & (conf_mac_DORM1$ocup > 0),1,0)
conf_mac_DORM1$Caso4_frio = ifelse((conf_mac_DORM1$Caso4_temp <= f) & (conf_mac_DORM1$ocup > 0),1,0)
conf_mac_DORM1$Caso4_conf = ifelse(((conf_mac_DORM1$Caso4_temp < c) & (conf_mac_DORM1$Caso4_temp > f) & (conf_mac_DORM1$ocup > 0)),1,0)
conf_mac_DORM1$Caso4_cooling = ifelse((conf_mac_DORM1$Caso4_calor > 0),conf_mac_DORM1$Caso4_cooling,0)
conf_mac_DORM1$Caso4_heating = ifelse((conf_mac_DORM1$Caso4_frio > 0),conf_mac_DORM1$Caso4_heating,0)

conf_mac_DORM1$Caso5_calor = ifelse((conf_mac_DORM1$Caso5_temp >= c) & (conf_mac_DORM1$ocup > 0),1,0)
conf_mac_DORM1$Caso5_frio = ifelse((conf_mac_DORM1$Caso5_temp <= f) & (conf_mac_DORM1$ocup > 0),1,0)
conf_mac_DORM1$Caso5_conf = ifelse(((conf_mac_DORM1$Caso5_temp < c) & (conf_mac_DORM1$Caso5_temp > f) & (conf_mac_DORM1$ocup > 0)),1,0)
conf_mac_DORM1$Caso5_cooling = ifelse((conf_mac_DORM1$Caso5_calor > 0),conf_mac_DORM1$Caso5_cooling,0)
conf_mac_DORM1$Caso5_heating = ifelse((conf_mac_DORM1$Caso5_frio > 0),conf_mac_DORM1$Caso5_heating,0)

conf_mac_DORM2 = data.frame(APP=c('DORM2'),
                            ocup=ocup_DORM2,
                            temp_ext=vn_Caso1_rj_mac_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.,
                            REF_temp=vn_Caso1_rj_mac_1.csv$DORM2.Zone.Operative.Temperature..C..TimeStep.,
                            REF_cooling=ac1_Caso1_rj_mac_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            REF_heating=ac1_Caso1_rj_mac_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                            Caso2_temp=vn_Caso2_1.csv$DORM2.Zone.Operative.Temperature..C..TimeStep.,
                            Caso2_cooling=ac1_Caso2_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            Caso2_heating=ac1_Caso2_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                            Caso3_temp=vn_Caso3_1.csv$DORM2.Zone.Operative.Temperature..C..TimeStep.,
                            Caso3_cooling=ac1_Caso3_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            Caso3_heating=ac1_Caso3_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                            Caso4_temp=vn_Caso4_1.csv$DORM2.Zone.Operative.Temperature..C..TimeStep.,
                            Caso4_cooling=ac1_Caso4_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            Caso4_heating=ac1_Caso4_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                            Caso5_temp=vn_Caso5_1.csv$DORM2.Zone.Operative.Temperature..C..TimeStep.,
                            Caso5_cooling=ac1_Caso5_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            Caso5_heating=ac1_Caso5_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.)

conf_mac_DORM2$EXT_calor = ifelse((conf_mac_DORM2$temp_ext >= c & conf_mac_DORM2$ocup > 0),1,0)
conf_mac_DORM2$EXT_frio = ifelse((conf_mac_DORM2$temp_ext <= f) & (conf_mac_DORM2$ocup > 0),1,0)
conf_mac_DORM2$EXT_conf = ifelse(((conf_mac_DORM2$temp_ext < c) & (conf_mac_DORM2$temp_ext > f) & (conf_mac_DORM2$ocup > 0)),1,0)

conf_mac_DORM2$REF_calor = ifelse((conf_mac_DORM2$REF_temp >= c) & (conf_mac_DORM2$ocup > 0),1,0)
conf_mac_DORM2$REF_frio = ifelse((conf_mac_DORM2$REF_temp <= f) & (conf_mac_DORM2$ocup > 0),1,0)
conf_mac_DORM2$REF_conf = ifelse(((conf_mac_DORM2$REF_temp < c) & (conf_mac_DORM2$REF_temp > f) & (conf_mac_DORM2$ocup > 0)),1,0)
conf_mac_DORM2$REF_cooling = ifelse((conf_mac_DORM2$REF_calor > 0),conf_mac_DORM2$REF_cooling,0)
conf_mac_DORM2$REF_heating = ifelse((conf_mac_DORM2$REF_frio > 0),conf_mac_DORM2$REF_heating,0)

conf_mac_DORM2$Caso2_calor = ifelse((conf_mac_DORM2$Caso2_temp >= c) & (conf_mac_DORM2$ocup > 0),1,0)
conf_mac_DORM2$Caso2_frio = ifelse((conf_mac_DORM2$Caso2_temp <= f) & (conf_mac_DORM2$ocup > 0),1,0)
conf_mac_DORM2$Caso2_conf = ifelse(((conf_mac_DORM2$Caso2_temp < c) & (conf_mac_DORM2$Caso2_temp > f) & (conf_mac_DORM2$ocup > 0)),1,0)
conf_mac_DORM2$Caso2_cooling = ifelse((conf_mac_DORM2$Caso2_calor > 0),conf_mac_DORM2$Caso2_cooling,0)
conf_mac_DORM2$Caso2_heating = ifelse((conf_mac_DORM2$Caso2_frio > 0),conf_mac_DORM2$Caso2_heating,0)

conf_mac_DORM2$Caso3_calor = ifelse((conf_mac_DORM2$Caso3_temp >= c) & (conf_mac_DORM2$ocup > 0),1,0)
conf_mac_DORM2$Caso3_frio = ifelse((conf_mac_DORM2$Caso3_temp <= f) & (conf_mac_DORM2$ocup > 0),1,0)
conf_mac_DORM2$Caso3_conf = ifelse(((conf_mac_DORM2$Caso3_temp < c) & (conf_mac_DORM2$Caso3_temp > f) & (conf_mac_DORM2$ocup > 0)),1,0)
conf_mac_DORM2$Caso3_cooling = ifelse((conf_mac_DORM2$Caso3_calor > 0),conf_mac_DORM2$Caso3_cooling,0)
conf_mac_DORM2$Caso3_heating = ifelse((conf_mac_DORM2$Caso3_frio > 0),conf_mac_DORM2$Caso3_heating,0)

conf_mac_DORM2$Caso4_calor = ifelse((conf_mac_DORM2$Caso4_temp >= c) & (conf_mac_DORM2$ocup > 0),1,0)
conf_mac_DORM2$Caso4_frio = ifelse((conf_mac_DORM2$Caso4_temp <= f) & (conf_mac_DORM2$ocup > 0),1,0)
conf_mac_DORM2$Caso4_conf = ifelse(((conf_mac_DORM2$Caso4_temp < c) & (conf_mac_DORM2$Caso4_temp > f) & (conf_mac_DORM2$ocup > 0)),1,0)
conf_mac_DORM2$Caso4_cooling = ifelse((conf_mac_DORM2$Caso4_calor > 0),conf_mac_DORM2$Caso4_cooling,0)
conf_mac_DORM2$Caso4_heating = ifelse((conf_mac_DORM2$Caso4_frio > 0),conf_mac_DORM2$Caso4_heating,0)

conf_mac_DORM2$Caso5_calor = ifelse((conf_mac_DORM2$Caso5_temp >= c) & (conf_mac_DORM2$ocup > 0),1,0)
conf_mac_DORM2$Caso5_frio = ifelse((conf_mac_DORM2$Caso5_temp <= f) & (conf_mac_DORM2$ocup > 0),1,0)
conf_mac_DORM2$Caso5_conf = ifelse(((conf_mac_DORM2$Caso5_temp < c) & (conf_mac_DORM2$Caso5_temp > f) & (conf_mac_DORM2$ocup > 0)),1,0)
conf_mac_DORM2$Caso5_cooling = ifelse((conf_mac_DORM2$Caso5_calor > 0),conf_mac_DORM2$Caso5_cooling,0)
conf_mac_DORM2$Caso5_heating = ifelse((conf_mac_DORM2$Caso5_frio > 0),conf_mac_DORM2$Caso5_heating,0)

conf_mac_SALA = data.frame(APP=c('SALA'),
                            ocup=ocup_SALA,
                            temp_ext=vn_Caso1_rj_mac_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.,
                            REF_temp=vn_Caso1_rj_mac_1.csv$SALA.Zone.Operative.Temperature..C..TimeStep.,
                            REF_cooling=ac1_Caso1_rj_mac_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            REF_heating=ac1_Caso1_rj_mac_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                            Caso2_temp=vn_Caso2_1.csv$SALA.Zone.Operative.Temperature..C..TimeStep.,
                            Caso2_cooling=ac1_Caso2_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            Caso2_heating=ac1_Caso2_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                            Caso3_temp=vn_Caso3_1.csv$SALA.Zone.Operative.Temperature..C..TimeStep.,
                            Caso3_cooling=ac1_Caso3_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            Caso3_heating=ac1_Caso3_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                            Caso4_temp=vn_Caso4_1.csv$SALA.Zone.Operative.Temperature..C..TimeStep.,
                            Caso4_cooling=ac1_Caso4_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            Caso4_heating=ac1_Caso4_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                            Caso5_temp=vn_Caso5_1.csv$SALA.Zone.Operative.Temperature..C..TimeStep.,
                            Caso5_cooling=ac1_Caso5_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            Caso5_heating=ac1_Caso5_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.)

conf_mac_SALA$EXT_calor = ifelse((conf_mac_SALA$temp_ext >= c & conf_mac_SALA$ocup > 0),1,0)
conf_mac_SALA$EXT_frio = ifelse((conf_mac_SALA$temp_ext <= f) & (conf_mac_SALA$ocup > 0),1,0)
conf_mac_SALA$EXT_conf = ifelse(((conf_mac_SALA$temp_ext < c) & (conf_mac_SALA$temp_ext > f) & (conf_mac_SALA$ocup > 0)),1,0)

conf_mac_SALA$REF_calor = ifelse((conf_mac_SALA$REF_temp >= c) & (conf_mac_SALA$ocup > 0),1,0)
conf_mac_SALA$REF_frio = ifelse((conf_mac_SALA$REF_temp <= f) & (conf_mac_SALA$ocup > 0),1,0)
conf_mac_SALA$REF_conf = ifelse(((conf_mac_SALA$REF_temp < c) & (conf_mac_SALA$REF_temp > f) & (conf_mac_SALA$ocup > 0)),1,0)
conf_mac_SALA$REF_cooling = ifelse((conf_mac_SALA$REF_calor > 0),conf_mac_SALA$REF_cooling,0)
conf_mac_SALA$REF_heating = ifelse((conf_mac_SALA$REF_frio > 0),conf_mac_SALA$REF_heating,0)

conf_mac_SALA$Caso2_calor = ifelse((conf_mac_SALA$Caso2_temp >= c) & (conf_mac_SALA$ocup > 0),1,0)
conf_mac_SALA$Caso2_frio = ifelse((conf_mac_SALA$Caso2_temp <= f) & (conf_mac_SALA$ocup > 0),1,0)
conf_mac_SALA$Caso2_conf = ifelse(((conf_mac_SALA$Caso2_temp < c) & (conf_mac_SALA$Caso2_temp > f) & (conf_mac_SALA$ocup > 0)),1,0)
conf_mac_SALA$Caso2_cooling = ifelse((conf_mac_SALA$Caso2_calor > 0),conf_mac_SALA$Caso2_cooling,0)
conf_mac_SALA$Caso2_heating = ifelse((conf_mac_SALA$Caso2_frio > 0),conf_mac_SALA$Caso2_heating,0)

conf_mac_SALA$Caso3_calor = ifelse((conf_mac_SALA$Caso3_temp >= c) & (conf_mac_SALA$ocup > 0),1,0)
conf_mac_SALA$Caso3_frio = ifelse((conf_mac_SALA$Caso3_temp <= f) & (conf_mac_SALA$ocup > 0),1,0)
conf_mac_SALA$Caso3_conf = ifelse(((conf_mac_SALA$Caso3_temp < c) & (conf_mac_SALA$Caso3_temp > f) & (conf_mac_SALA$ocup > 0)),1,0)
conf_mac_SALA$Caso3_cooling = ifelse((conf_mac_SALA$Caso3_calor > 0),conf_mac_SALA$Caso3_cooling,0)
conf_mac_SALA$Caso3_heating = ifelse((conf_mac_SALA$Caso3_frio > 0),conf_mac_SALA$Caso3_heating,0)

conf_mac_SALA$Caso4_calor = ifelse((conf_mac_SALA$Caso4_temp >= c) & (conf_mac_SALA$ocup > 0),1,0)
conf_mac_SALA$Caso4_frio = ifelse((conf_mac_SALA$Caso4_temp <= f) & (conf_mac_SALA$ocup > 0),1,0)
conf_mac_SALA$Caso4_conf = ifelse(((conf_mac_SALA$Caso4_temp < c) & (conf_mac_SALA$Caso4_temp > f) & (conf_mac_SALA$ocup > 0)),1,0)
conf_mac_SALA$Caso4_cooling = ifelse((conf_mac_SALA$Caso4_calor > 0),conf_mac_SALA$Caso4_cooling,0)
conf_mac_SALA$Caso4_heating = ifelse((conf_mac_SALA$Caso4_frio > 0),conf_mac_SALA$Caso4_heating,0)

conf_mac_SALA$Caso5_calor = ifelse((conf_mac_SALA$Caso5_temp >= c) & (conf_mac_SALA$ocup > 0),1,0)
conf_mac_SALA$Caso5_frio = ifelse((conf_mac_SALA$Caso5_temp <= f) & (conf_mac_SALA$ocup > 0),1,0)
conf_mac_SALA$Caso5_conf = ifelse(((conf_mac_SALA$Caso5_temp < c) & (conf_mac_SALA$Caso5_temp > f) & (conf_mac_SALA$ocup > 0)),1,0)
conf_mac_SALA$Caso5_cooling = ifelse((conf_mac_SALA$Caso5_calor > 0),conf_mac_SALA$Caso5_cooling,0)
conf_mac_SALA$Caso5_heating = ifelse((conf_mac_SALA$Caso5_frio > 0),conf_mac_SALA$Caso5_heating,0)

# criando as listas que estarao nos for

caso = c('EXT','REF','Caso2','Caso3','Caso4','Caso5')
caso1 = c('REF','Caso2','Caso3','Caso4','Caso5')
app = c('DORM1','DORM2','SALA')
tipo = c('frio','calor','conf')
hvac = c('cooling','heating')

conf_mac = data.frame(CASO = rep(c('EXT','REF','Caso2','Caso3','Caso4','Caso5'),each=3), APP = c('DORM1','DORM2','SALA'), PHFT=c(1:1),Tipo=rep(c('frio','calor','conf'),each=18))

df = rbind(conf_mac_DORM1,conf_mac_DORM2,conf_mac_SALA)

for(d in caso){
  for(a in app){
    for(t in tipo){
      conf_mac$PHFT[conf_mac$APP == a & conf_mac$CASO == d & conf_mac$Tipo == t] = round(((sum(df[,grepl(paste0(d,'_',t),colnames(df))][df$APP == a])/sum(df$ocup[df$APP == a]))*100),1)
    }
  }
}

ref_mac = data.frame(CASO = rep(c('REF','Caso2','Caso3','Caso4','Caso5'),each=3), APP = c('DORM1','DORM2','SALA'), Cgt=rep(c('cooling','heating'),each=15), Cgt_kwh=c(1:1))

for(d in caso1){
  for(a in app){
    for(h in hvac){
      ref_mac$Cgt_kwh[ref_mac$APP == a & ref_mac$CASO == d & ref_mac$Cgt == h] = round((sum(df[,grepl(paste0(d,'_',h),colnames(df))][df$APP == a])/3600000),1)
    }
  }
}

conf_mac_uh = data.frame(CASO = c('EXT','REF','Caso2','Caso3','Caso4','Caso5'), PHFT=c(1:1), Tipo=rep(c('frio','calor','conf'),each=6))
for(d in caso){
  for(t in tipo){
    conf_mac_uh$PHFT[conf_mac_uh$CASO == d & conf_mac_uh$Tipo == t] = round((sum(conf_mac$PHFT[conf_mac$CASO == d & conf_mac$Tipo == t])/3),1)
  }
}

ref_mac_uh = data.frame(CASO = c('REF','Caso2','Caso3','Caso4','Caso5'), Cgt=rep(c('cooling','heating'),each=15),Cgt_kwh=c(1:1))

for(d in caso1){
  for(h in hvac){
    ref_mac_uh$Cgt_kwh[ref_mac_uh$CASO == d & ref_mac_uh$Cgt == h] = round((sum(ref_mac$Cgt_kwh[ref_mac$CASO == d & ref_mac$Cgt == h])),1) 
  }
}

setwd('D:/Frentes de Trabalho/simu_NBR/novos_graphs')

write.csv(ref_mac, 'cgt_mac.csv')
write.csv(ref_mac_uh, 'cgt_mac_uh.csv')
write.csv(conf_mac, 'conf_mac.csv')
write.csv(conf_mac_uh, 'conf_mac_uh.csv')

a = read.csv("conf_mac_uh.csv")

rm(list = ls())

## rio ----

setwd("D:/Frentes de Trabalho/simu_NBR/VNxAC_janela/BRA_RJ_Rio.de.Janeiro-Santos.Dumont.AP.837550_TMYx.2003-2017/")

file_list = list.files(pattern="*_1.csv") # todos os csv menos o table

library("data.table")

for (i in 1:5){ #eu sei que os 5 primeiros sao hvac
  assign(file_list[i], read.csv(file_list[i])[,c("Date.Time",
                                                 "Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.",
                                                 "DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.",
                                                 "DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.",
                                                 "SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.",
                                                 "SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.",
                                                 "DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.",
                                                 "DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.")])
  for(k in 6:10){ # eu sei que os 5 ultimos sao vn
    assign(file_list[k], read.csv(file_list[k])[,c("Date.Time",
                                                   "Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.",
                                                   "DORMITORIO1.People.Occupant.Count....TimeStep.",
                                                   "DORMITORIO2.People.Occupant.Count....TimeStep.",
                                                   "SALA1.People.Occupant.Count....TimeStep.",
                                                   "DORM2.Zone.Operative.Temperature..C..TimeStep.",
                                                   "DORM2.Zone.Mean.Air.Temperature..C..TimeStep.",
                                                   "DORM1.Zone.Operative.Temperature..C..TimeStep.",
                                                   "DORM1.Zone.Mean.Air.Temperature..C..TimeStep.",
                                                   "SALA.Zone.Operative.Temperature..C..TimeStep.",
                                                   "SALA.Zone.Mean.Air.Temperature..C..TimeStep.")])
  }
  
}

ocup_DORM1 = data.frame(ocup_DORM = vn_Caso1_rj_mac_1.csv$DORMITORIO1.People.Occupant.Count....TimeStep.)
ocup_DORM1$ocup = ifelse(ocup_DORM1$ocup_DORM > 0,1,0)
ocup_DORM1 = subset(ocup_DORM1, select = c("ocup"))

ocup_DORM2 = subset(ocup_DORM1, select = c("ocup"))  ## a ocupacao eh igual

ocup_SALA = data.frame(ocup_DORM = vn_Caso1_rj_mac_1.csv$SALA1.People.Occupant.Count....TimeStep.)
ocup_SALA$ocup = ifelse(ocup_SALA$ocup_DORM > 0,1,0)
ocup_SALA = subset(ocup_SALA, select = c("ocup"))

# ocup = rbind(ocup_dorm1,ocup_dorm2,ocup_sala)
# rm(ocup_dorm1,ocup_dorm2,ocup_sala)

# temperatura ----

c = as.numeric(26) ## limite calor
f = as.numeric(18) ## limite frio

conf_rj_DORM1 = data.frame(APP=c('DORM1'),
                            ocup=ocup_DORM1,
                            temp_ext=vn_Caso1_rj_mac_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.,
                            REF_temp=vn_Caso1_rj_mac_1.csv$DORM1.Zone.Operative.Temperature..C..TimeStep.,
                            REF_cooling=ac1_Caso1_rj_mac_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            REF_heating=ac1_Caso1_rj_mac_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                            Caso2_temp=vn_Caso2_1.csv$DORM1.Zone.Operative.Temperature..C..TimeStep.,
                            Caso2_cooling=ac1_Caso2_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            Caso2_heating=ac1_Caso2_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                            Caso3_temp=vn_Caso3_1.csv$DORM1.Zone.Operative.Temperature..C..TimeStep.,
                            Caso3_cooling=ac1_Caso3_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            Caso3_heating=ac1_Caso3_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                            Caso4_temp=vn_Caso4_1.csv$DORM1.Zone.Operative.Temperature..C..TimeStep.,
                            Caso4_cooling=ac1_Caso4_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            Caso4_heating=ac1_Caso4_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                            Caso5_temp=vn_Caso5_1.csv$DORM1.Zone.Operative.Temperature..C..TimeStep.,
                            Caso5_cooling=ac1_Caso5_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            Caso5_heating=ac1_Caso5_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.)

conf_rj_DORM1$EXT_calor = ifelse((conf_rj_DORM1$temp_ext >= c & conf_rj_DORM1$ocup > 0),1,0)
conf_rj_DORM1$EXT_frio = ifelse((conf_rj_DORM1$temp_ext <= f) & (conf_rj_DORM1$ocup > 0),1,0)
conf_rj_DORM1$EXT_conf = ifelse(((conf_rj_DORM1$temp_ext < c) & (conf_rj_DORM1$temp_ext > f) & (conf_rj_DORM1$ocup > 0)),1,0)

conf_rj_DORM1$REF_calor = ifelse((conf_rj_DORM1$REF_temp >= c) & (conf_rj_DORM1$ocup > 0),1,0)
conf_rj_DORM1$REF_frio = ifelse((conf_rj_DORM1$REF_temp <= f) & (conf_rj_DORM1$ocup > 0),1,0)
conf_rj_DORM1$REF_conf = ifelse(((conf_rj_DORM1$REF_temp < c) & (conf_rj_DORM1$REF_temp > f) & (conf_rj_DORM1$ocup > 0)),1,0)
conf_rj_DORM1$REF_cooling = ifelse((conf_rj_DORM1$REF_calor > 0),conf_rj_DORM1$REF_cooling,0)
conf_rj_DORM1$REF_heating = ifelse((conf_rj_DORM1$REF_frio > 0),conf_rj_DORM1$REF_heating,0)

conf_rj_DORM1$Caso2_calor = ifelse((conf_rj_DORM1$Caso2_temp >= c) & (conf_rj_DORM1$ocup > 0),1,0)
conf_rj_DORM1$Caso2_frio = ifelse((conf_rj_DORM1$Caso2_temp <= f) & (conf_rj_DORM1$ocup > 0),1,0)
conf_rj_DORM1$Caso2_conf = ifelse(((conf_rj_DORM1$Caso2_temp < c) & (conf_rj_DORM1$Caso2_temp > f) & (conf_rj_DORM1$ocup > 0)),1,0)
conf_rj_DORM1$Caso2_cooling = ifelse((conf_rj_DORM1$Caso2_calor > 0),conf_rj_DORM1$Caso2_cooling,0)
conf_rj_DORM1$Caso2_heating = ifelse((conf_rj_DORM1$Caso2_frio > 0),conf_rj_DORM1$Caso2_heating,0)

conf_rj_DORM1$Caso3_calor = ifelse((conf_rj_DORM1$Caso3_temp >= c) & (conf_rj_DORM1$ocup > 0),1,0)
conf_rj_DORM1$Caso3_frio = ifelse((conf_rj_DORM1$Caso3_temp <= f) & (conf_rj_DORM1$ocup > 0),1,0)
conf_rj_DORM1$Caso3_conf = ifelse(((conf_rj_DORM1$Caso3_temp < c) & (conf_rj_DORM1$Caso3_temp > f) & (conf_rj_DORM1$ocup > 0)),1,0)
conf_rj_DORM1$Caso3_cooling = ifelse((conf_rj_DORM1$Caso3_calor > 0),conf_rj_DORM1$Caso3_cooling,0)
conf_rj_DORM1$Caso3_heating = ifelse((conf_rj_DORM1$Caso3_frio > 0),conf_rj_DORM1$Caso3_heating,0)

conf_rj_DORM1$Caso4_calor = ifelse((conf_rj_DORM1$Caso4_temp >= c) & (conf_rj_DORM1$ocup > 0),1,0)
conf_rj_DORM1$Caso4_frio = ifelse((conf_rj_DORM1$Caso4_temp <= f) & (conf_rj_DORM1$ocup > 0),1,0)
conf_rj_DORM1$Caso4_conf = ifelse(((conf_rj_DORM1$Caso4_temp < c) & (conf_rj_DORM1$Caso4_temp > f) & (conf_rj_DORM1$ocup > 0)),1,0)
conf_rj_DORM1$Caso4_cooling = ifelse((conf_rj_DORM1$Caso4_calor > 0),conf_rj_DORM1$Caso4_cooling,0)
conf_rj_DORM1$Caso4_heating = ifelse((conf_rj_DORM1$Caso4_frio > 0),conf_rj_DORM1$Caso4_heating,0)

conf_rj_DORM1$Caso5_calor = ifelse((conf_rj_DORM1$Caso5_temp >= c) & (conf_rj_DORM1$ocup > 0),1,0)
conf_rj_DORM1$Caso5_frio = ifelse((conf_rj_DORM1$Caso5_temp <= f) & (conf_rj_DORM1$ocup > 0),1,0)
conf_rj_DORM1$Caso5_conf = ifelse(((conf_rj_DORM1$Caso5_temp < c) & (conf_rj_DORM1$Caso5_temp > f) & (conf_rj_DORM1$ocup > 0)),1,0)
conf_rj_DORM1$Caso5_cooling = ifelse((conf_rj_DORM1$Caso5_calor > 0),conf_rj_DORM1$Caso5_cooling,0)
conf_rj_DORM1$Caso5_heating = ifelse((conf_rj_DORM1$Caso5_frio > 0),conf_rj_DORM1$Caso5_heating,0)

conf_rj_DORM2 = data.frame(APP=c('DORM2'),
                            ocup=ocup_DORM2,
                            temp_ext=vn_Caso1_rj_mac_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.,
                            REF_temp=vn_Caso1_rj_mac_1.csv$DORM2.Zone.Operative.Temperature..C..TimeStep.,
                            REF_cooling=ac1_Caso1_rj_mac_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            REF_heating=ac1_Caso1_rj_mac_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                            Caso2_temp=vn_Caso2_1.csv$DORM2.Zone.Operative.Temperature..C..TimeStep.,
                            Caso2_cooling=ac1_Caso2_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            Caso2_heating=ac1_Caso2_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                            Caso3_temp=vn_Caso3_1.csv$DORM2.Zone.Operative.Temperature..C..TimeStep.,
                            Caso3_cooling=ac1_Caso3_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            Caso3_heating=ac1_Caso3_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                            Caso4_temp=vn_Caso4_1.csv$DORM2.Zone.Operative.Temperature..C..TimeStep.,
                            Caso4_cooling=ac1_Caso4_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            Caso4_heating=ac1_Caso4_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                            Caso5_temp=vn_Caso5_1.csv$DORM2.Zone.Operative.Temperature..C..TimeStep.,
                            Caso5_cooling=ac1_Caso5_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            Caso5_heating=ac1_Caso5_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.)

conf_rj_DORM2$EXT_calor = ifelse((conf_rj_DORM2$temp_ext >= c & conf_rj_DORM2$ocup > 0),1,0)
conf_rj_DORM2$EXT_frio = ifelse((conf_rj_DORM2$temp_ext <= f) & (conf_rj_DORM2$ocup > 0),1,0)
conf_rj_DORM2$EXT_conf = ifelse(((conf_rj_DORM2$temp_ext < c) & (conf_rj_DORM2$temp_ext > f) & (conf_rj_DORM2$ocup > 0)),1,0)

conf_rj_DORM2$REF_calor = ifelse((conf_rj_DORM2$REF_temp >= c) & (conf_rj_DORM2$ocup > 0),1,0)
conf_rj_DORM2$REF_frio = ifelse((conf_rj_DORM2$REF_temp <= f) & (conf_rj_DORM2$ocup > 0),1,0)
conf_rj_DORM2$REF_conf = ifelse(((conf_rj_DORM2$REF_temp < c) & (conf_rj_DORM2$REF_temp > f) & (conf_rj_DORM2$ocup > 0)),1,0)
conf_rj_DORM2$REF_cooling = ifelse((conf_rj_DORM2$REF_calor > 0),conf_rj_DORM2$REF_cooling,0)
conf_rj_DORM2$REF_heating = ifelse((conf_rj_DORM2$REF_frio > 0),conf_rj_DORM2$REF_heating,0)

conf_rj_DORM2$Caso2_calor = ifelse((conf_rj_DORM2$Caso2_temp >= c) & (conf_rj_DORM2$ocup > 0),1,0)
conf_rj_DORM2$Caso2_frio = ifelse((conf_rj_DORM2$Caso2_temp <= f) & (conf_rj_DORM2$ocup > 0),1,0)
conf_rj_DORM2$Caso2_conf = ifelse(((conf_rj_DORM2$Caso2_temp < c) & (conf_rj_DORM2$Caso2_temp > f) & (conf_rj_DORM2$ocup > 0)),1,0)
conf_rj_DORM2$Caso2_cooling = ifelse((conf_rj_DORM2$Caso2_calor > 0),conf_rj_DORM2$Caso2_cooling,0)
conf_rj_DORM2$Caso2_heating = ifelse((conf_rj_DORM2$Caso2_frio > 0),conf_rj_DORM2$Caso2_heating,0)

conf_rj_DORM2$Caso3_calor = ifelse((conf_rj_DORM2$Caso3_temp >= c) & (conf_rj_DORM2$ocup > 0),1,0)
conf_rj_DORM2$Caso3_frio = ifelse((conf_rj_DORM2$Caso3_temp <= f) & (conf_rj_DORM2$ocup > 0),1,0)
conf_rj_DORM2$Caso3_conf = ifelse(((conf_rj_DORM2$Caso3_temp < c) & (conf_rj_DORM2$Caso3_temp > f) & (conf_rj_DORM2$ocup > 0)),1,0)
conf_rj_DORM2$Caso3_cooling = ifelse((conf_rj_DORM2$Caso3_calor > 0),conf_rj_DORM2$Caso3_cooling,0)
conf_rj_DORM2$Caso3_heating = ifelse((conf_rj_DORM2$Caso3_frio > 0),conf_rj_DORM2$Caso3_heating,0)

conf_rj_DORM2$Caso4_calor = ifelse((conf_rj_DORM2$Caso4_temp >= c) & (conf_rj_DORM2$ocup > 0),1,0)
conf_rj_DORM2$Caso4_frio = ifelse((conf_rj_DORM2$Caso4_temp <= f) & (conf_rj_DORM2$ocup > 0),1,0)
conf_rj_DORM2$Caso4_conf = ifelse(((conf_rj_DORM2$Caso4_temp < c) & (conf_rj_DORM2$Caso4_temp > f) & (conf_rj_DORM2$ocup > 0)),1,0)
conf_rj_DORM2$Caso4_cooling = ifelse((conf_rj_DORM2$Caso4_calor > 0),conf_rj_DORM2$Caso4_cooling,0)
conf_rj_DORM2$Caso4_heating = ifelse((conf_rj_DORM2$Caso4_frio > 0),conf_rj_DORM2$Caso4_heating,0)

conf_rj_DORM2$Caso5_calor = ifelse((conf_rj_DORM2$Caso5_temp >= c) & (conf_rj_DORM2$ocup > 0),1,0)
conf_rj_DORM2$Caso5_frio = ifelse((conf_rj_DORM2$Caso5_temp <= f) & (conf_rj_DORM2$ocup > 0),1,0)
conf_rj_DORM2$Caso5_conf = ifelse(((conf_rj_DORM2$Caso5_temp < c) & (conf_rj_DORM2$Caso5_temp > f) & (conf_rj_DORM2$ocup > 0)),1,0)
conf_rj_DORM2$Caso5_cooling = ifelse((conf_rj_DORM2$Caso5_calor > 0),conf_rj_DORM2$Caso5_cooling,0)
conf_rj_DORM2$Caso5_heating = ifelse((conf_rj_DORM2$Caso5_frio > 0),conf_rj_DORM2$Caso5_heating,0)

conf_rj_SALA = data.frame(APP=c('SALA'),
                           ocup=ocup_SALA,
                           temp_ext=vn_Caso1_rj_mac_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.,
                           REF_temp=vn_Caso1_rj_mac_1.csv$SALA.Zone.Operative.Temperature..C..TimeStep.,
                           REF_cooling=ac1_Caso1_rj_mac_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                           REF_heating=ac1_Caso1_rj_mac_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                           Caso2_temp=vn_Caso2_1.csv$SALA.Zone.Operative.Temperature..C..TimeStep.,
                           Caso2_cooling=ac1_Caso2_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                           Caso2_heating=ac1_Caso2_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                           Caso3_temp=vn_Caso3_1.csv$SALA.Zone.Operative.Temperature..C..TimeStep.,
                           Caso3_cooling=ac1_Caso3_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                           Caso3_heating=ac1_Caso3_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                           Caso4_temp=vn_Caso4_1.csv$SALA.Zone.Operative.Temperature..C..TimeStep.,
                           Caso4_cooling=ac1_Caso4_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                           Caso4_heating=ac1_Caso4_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                           Caso5_temp=vn_Caso5_1.csv$SALA.Zone.Operative.Temperature..C..TimeStep.,
                           Caso5_cooling=ac1_Caso5_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                           Caso5_heating=ac1_Caso5_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.)

conf_rj_SALA$EXT_calor = ifelse((conf_rj_SALA$temp_ext >= c & conf_rj_SALA$ocup > 0),1,0)
conf_rj_SALA$EXT_frio = ifelse((conf_rj_SALA$temp_ext <= f) & (conf_rj_SALA$ocup > 0),1,0)
conf_rj_SALA$EXT_conf = ifelse(((conf_rj_SALA$temp_ext < c) & (conf_rj_SALA$temp_ext > f) & (conf_rj_SALA$ocup > 0)),1,0)

conf_rj_SALA$REF_calor = ifelse((conf_rj_SALA$REF_temp >= c) & (conf_rj_SALA$ocup > 0),1,0)
conf_rj_SALA$REF_frio = ifelse((conf_rj_SALA$REF_temp <= f) & (conf_rj_SALA$ocup > 0),1,0)
conf_rj_SALA$REF_conf = ifelse(((conf_rj_SALA$REF_temp < c) & (conf_rj_SALA$REF_temp > f) & (conf_rj_SALA$ocup > 0)),1,0)
conf_rj_SALA$REF_cooling = ifelse((conf_rj_SALA$REF_calor > 0),conf_rj_SALA$REF_cooling,0)
conf_rj_SALA$REF_heating = ifelse((conf_rj_SALA$REF_frio > 0),conf_rj_SALA$REF_heating,0)

conf_rj_SALA$Caso2_calor = ifelse((conf_rj_SALA$Caso2_temp >= c) & (conf_rj_SALA$ocup > 0),1,0)
conf_rj_SALA$Caso2_frio = ifelse((conf_rj_SALA$Caso2_temp <= f) & (conf_rj_SALA$ocup > 0),1,0)
conf_rj_SALA$Caso2_conf = ifelse(((conf_rj_SALA$Caso2_temp < c) & (conf_rj_SALA$Caso2_temp > f) & (conf_rj_SALA$ocup > 0)),1,0)
conf_rj_SALA$Caso2_cooling = ifelse((conf_rj_SALA$Caso2_calor > 0),conf_rj_SALA$Caso2_cooling,0)
conf_rj_SALA$Caso2_heating = ifelse((conf_rj_SALA$Caso2_frio > 0),conf_rj_SALA$Caso2_heating,0)

conf_rj_SALA$Caso3_calor = ifelse((conf_rj_SALA$Caso3_temp >= c) & (conf_rj_SALA$ocup > 0),1,0)
conf_rj_SALA$Caso3_frio = ifelse((conf_rj_SALA$Caso3_temp <= f) & (conf_rj_SALA$ocup > 0),1,0)
conf_rj_SALA$Caso3_conf = ifelse(((conf_rj_SALA$Caso3_temp < c) & (conf_rj_SALA$Caso3_temp > f) & (conf_rj_SALA$ocup > 0)),1,0)
conf_rj_SALA$Caso3_cooling = ifelse((conf_rj_SALA$Caso3_calor > 0),conf_rj_SALA$Caso3_cooling,0)
conf_rj_SALA$Caso3_heating = ifelse((conf_rj_SALA$Caso3_frio > 0),conf_rj_SALA$Caso3_heating,0)

conf_rj_SALA$Caso4_calor = ifelse((conf_rj_SALA$Caso4_temp >= c) & (conf_rj_SALA$ocup > 0),1,0)
conf_rj_SALA$Caso4_frio = ifelse((conf_rj_SALA$Caso4_temp <= f) & (conf_rj_SALA$ocup > 0),1,0)
conf_rj_SALA$Caso4_conf = ifelse(((conf_rj_SALA$Caso4_temp < c) & (conf_rj_SALA$Caso4_temp > f) & (conf_rj_SALA$ocup > 0)),1,0)
conf_rj_SALA$Caso4_cooling = ifelse((conf_rj_SALA$Caso4_calor > 0),conf_rj_SALA$Caso4_cooling,0)
conf_rj_SALA$Caso4_heating = ifelse((conf_rj_SALA$Caso4_frio > 0),conf_rj_SALA$Caso4_heating,0)

conf_rj_SALA$Caso5_calor = ifelse((conf_rj_SALA$Caso5_temp >= c) & (conf_rj_SALA$ocup > 0),1,0)
conf_rj_SALA$Caso5_frio = ifelse((conf_rj_SALA$Caso5_temp <= f) & (conf_rj_SALA$ocup > 0),1,0)
conf_rj_SALA$Caso5_conf = ifelse(((conf_rj_SALA$Caso5_temp < c) & (conf_rj_SALA$Caso5_temp > f) & (conf_rj_SALA$ocup > 0)),1,0)
conf_rj_SALA$Caso5_cooling = ifelse((conf_rj_SALA$Caso5_calor > 0),conf_rj_SALA$Caso5_cooling,0)
conf_rj_SALA$Caso5_heating = ifelse((conf_rj_SALA$Caso5_frio > 0),conf_rj_SALA$Caso5_heating,0)

caso = c('EXT','REF','Caso2','Caso3','Caso4','Caso5')
caso1 = c('REF','Caso2','Caso3','Caso4','Caso5')
app = c('DORM1','DORM2','SALA')
tipo = c('frio','calor','conf')
hvac = c('cooling','heating')

conf_rj = data.frame(CASO = rep(c('EXT','REF','Caso2','Caso3','Caso4','Caso5'),each=3), APP = c('DORM1','DORM2','SALA'), PHFT=c(1:1),Tipo=rep(c('frio','calor','conf'),each=18))

df = rbind(conf_rj_DORM1,conf_rj_DORM2,conf_rj_SALA)

for(d in caso){
  for(a in app){
    for(t in tipo){
      conf_rj$PHFT[conf_rj$APP == a & conf_rj$CASO == d & conf_rj$Tipo == t] = round(((sum(df[,grepl(paste0(d,'_',t),colnames(df))][df$APP == a])/sum(df$ocup[df$APP == a]))*100),1)
    }
  }
}

ref_rj = data.frame(CASO = rep(c('REF','Caso2','Caso3','Caso4','Caso5'),each=3), APP = c('DORM1','DORM2','SALA'), Cgt=rep(c('cooling','heating'),each=15), Cgt_kwh=c(1:1))

for(d in caso1){
  for(a in app){
    for(h in hvac){
      ref_rj$Cgt_kwh[ref_rj$APP == a & ref_rj$CASO == d & ref_rj$Cgt == h] = round((sum(df[,grepl(paste0(d,'_',h),colnames(df))][df$APP == a])/3600000),1)
    }
  }
}

conf_rj_uh = data.frame(CASO = c('EXT','REF','Caso2','Caso3','Caso4','Caso5'), PHFT=c(1:1), Tipo=rep(c('frio','calor','conf'),each=6))
for(d in caso){
  for(t in tipo){
    conf_rj_uh$PHFT[conf_rj_uh$CASO == d & conf_rj_uh$Tipo == t] = round((sum(conf_rj$PHFT[conf_rj$CASO == d & conf_rj$Tipo == t])/3),1)
  }
}

ref_rj_uh = data.frame(CASO = c('REF','Caso2','Caso3','Caso4','Caso5'), Cgt=rep(c('cooling','heating'),each=15),Cgt_kwh=c(1:1))

for(d in caso1){
  for(h in hvac){
    ref_rj_uh$Cgt_kwh[ref_rj_uh$CASO == d & ref_rj_uh$Cgt == h] = round((sum(ref_rj$Cgt_kwh[ref_rj$CASO == d & ref_rj$Cgt == h])),1) 
  }
}

setwd('D:/Frentes de Trabalho/simu_NBR/novos_graphs')

write.csv(ref_rj, 'cgt_rj.csv')
write.csv(ref_rj_uh, 'cgt_rj_uh.csv')
write.csv(conf_rj, 'conf_rj.csv')
write.csv(conf_rj_uh, 'conf_rj_uh.csv')

rm(list = ls())

## curitiba ----

setwd("D:/Frentes de Trabalho/simu_NBR/VNxAC_janela/BRA_PR_Curitiba-Bacacheri.AP.838420_TMYx.2003-2017/")

file_list = list.files(pattern="*_1.csv") # todos os csv menos o table

library("data.table")

for (i in 1:5){ #eu sei que os 5 primeiros sao hvac
  assign(file_list[i], read.csv(file_list[i])[,c("Date.Time",
                                                 "Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.",
                                                 "DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.",
                                                 "DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.",
                                                 "SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.",
                                                 "SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.",
                                                 "DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.",
                                                 "DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.")])
  for(k in 6:10){ # eu sei que os 5 ultimos sao vn
    assign(file_list[k], read.csv(file_list[k])[,c("Date.Time",
                                                   "Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.",
                                                   "DORMITORIO1.People.Occupant.Count....TimeStep.",
                                                   "DORMITORIO2.People.Occupant.Count....TimeStep.",
                                                   "SALA1.People.Occupant.Count....TimeStep.",
                                                   "DORM2.Zone.Operative.Temperature..C..TimeStep.",
                                                   "DORM2.Zone.Mean.Air.Temperature..C..TimeStep.",
                                                   "DORM1.Zone.Operative.Temperature..C..TimeStep.",
                                                   "DORM1.Zone.Mean.Air.Temperature..C..TimeStep.",
                                                   "SALA.Zone.Operative.Temperature..C..TimeStep.",
                                                   "SALA.Zone.Mean.Air.Temperature..C..TimeStep.")])
  }
  
}

ocup_DORM1 = data.frame(ocup_DORM = vn_Caso1_sp_sta_co_1.csv$DORMITORIO1.People.Occupant.Count....TimeStep.)
ocup_DORM1$ocup = ifelse(ocup_DORM1$ocup_DORM > 0,1,0)
ocup_DORM1 = subset(ocup_DORM1, select = c("ocup"))

ocup_DORM2 = subset(ocup_DORM1, select = c("ocup"))  ## a ocupacao eh igual

ocup_SALA = data.frame(ocup_DORM = vn_Caso1_sp_sta_co_1.csv$SALA1.People.Occupant.Count....TimeStep.)
ocup_SALA$ocup = ifelse(ocup_SALA$ocup_DORM > 0,1,0)
ocup_SALA = subset(ocup_SALA, select = c("ocup"))

# ocup = rbind(ocup_dorm1,ocup_dorm2,ocup_sala)
# rm(ocup_dorm1,ocup_dorm2,ocup_sala)

# temperatura ----

c = as.numeric(26) ## limite calor
f = as.numeric(18) ## limite frio

conf_co_DORM1 = data.frame(APP=c('DORM1'),
                            ocup=ocup_DORM1,
                            temp_ext=vn_Caso1_sp_sta_co_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.,
                            REF_temp=vn_Caso1_sp_sta_co_1.csv$DORM1.Zone.Operative.Temperature..C..TimeStep.,
                            REF_cooling=ac1_Caso1_sp_sta_co_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            REF_heating=ac1_Caso1_sp_sta_co_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                            Caso2_temp=vn_Caso2_1.csv$DORM1.Zone.Operative.Temperature..C..TimeStep.,
                            Caso2_cooling=ac1_Caso2_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            Caso2_heating=ac1_Caso2_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                            Caso3_temp=vn_Caso3_1.csv$DORM1.Zone.Operative.Temperature..C..TimeStep.,
                            Caso3_cooling=ac1_Caso3_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            Caso3_heating=ac1_Caso3_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                            Caso4_temp=vn_Caso4_1.csv$DORM1.Zone.Operative.Temperature..C..TimeStep.,
                            Caso4_cooling=ac1_Caso4_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            Caso4_heating=ac1_Caso4_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                            Caso5_temp=vn_Caso5_1.csv$DORM1.Zone.Operative.Temperature..C..TimeStep.,
                            Caso5_cooling=ac1_Caso5_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            Caso5_heating=ac1_Caso5_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.)

conf_co_DORM1$EXT_calor = ifelse((conf_co_DORM1$temp_ext >= c & conf_co_DORM1$ocup > 0),1,0)
conf_co_DORM1$EXT_frio = ifelse((conf_co_DORM1$temp_ext <= f) & (conf_co_DORM1$ocup > 0),1,0)
conf_co_DORM1$EXT_conf = ifelse(((conf_co_DORM1$temp_ext < c) & (conf_co_DORM1$temp_ext > f) & (conf_co_DORM1$ocup > 0)),1,0)

conf_co_DORM1$REF_calor = ifelse((conf_co_DORM1$REF_temp >= c) & (conf_co_DORM1$ocup > 0),1,0)
conf_co_DORM1$REF_frio = ifelse((conf_co_DORM1$REF_temp <= f) & (conf_co_DORM1$ocup > 0),1,0)
conf_co_DORM1$REF_conf = ifelse(((conf_co_DORM1$REF_temp < c) & (conf_co_DORM1$REF_temp > f) & (conf_co_DORM1$ocup > 0)),1,0)
conf_co_DORM1$REF_cooling = ifelse((conf_co_DORM1$REF_calor > 0),conf_co_DORM1$REF_cooling,0)
conf_co_DORM1$REF_heating = ifelse((conf_co_DORM1$REF_frio > 0),conf_co_DORM1$REF_heating,0)

conf_co_DORM1$Caso2_calor = ifelse((conf_co_DORM1$Caso2_temp >= c) & (conf_co_DORM1$ocup > 0),1,0)
conf_co_DORM1$Caso2_frio = ifelse((conf_co_DORM1$Caso2_temp <= f) & (conf_co_DORM1$ocup > 0),1,0)
conf_co_DORM1$Caso2_conf = ifelse(((conf_co_DORM1$Caso2_temp < c) & (conf_co_DORM1$Caso2_temp > f) & (conf_co_DORM1$ocup > 0)),1,0)
conf_co_DORM1$Caso2_cooling = ifelse((conf_co_DORM1$Caso2_calor > 0),conf_co_DORM1$Caso2_cooling,0)
conf_co_DORM1$Caso2_heating = ifelse((conf_co_DORM1$Caso2_frio > 0),conf_co_DORM1$Caso2_heating,0)

conf_co_DORM1$Caso3_calor = ifelse((conf_co_DORM1$Caso3_temp >= c) & (conf_co_DORM1$ocup > 0),1,0)
conf_co_DORM1$Caso3_frio = ifelse((conf_co_DORM1$Caso3_temp <= f) & (conf_co_DORM1$ocup > 0),1,0)
conf_co_DORM1$Caso3_conf = ifelse(((conf_co_DORM1$Caso3_temp < c) & (conf_co_DORM1$Caso3_temp > f) & (conf_co_DORM1$ocup > 0)),1,0)
conf_co_DORM1$Caso3_cooling = ifelse((conf_co_DORM1$Caso3_calor > 0),conf_co_DORM1$Caso3_cooling,0)
conf_co_DORM1$Caso3_heating = ifelse((conf_co_DORM1$Caso3_frio > 0),conf_co_DORM1$Caso3_heating,0)

conf_co_DORM1$Caso4_calor = ifelse((conf_co_DORM1$Caso4_temp >= c) & (conf_co_DORM1$ocup > 0),1,0)
conf_co_DORM1$Caso4_frio = ifelse((conf_co_DORM1$Caso4_temp <= f) & (conf_co_DORM1$ocup > 0),1,0)
conf_co_DORM1$Caso4_conf = ifelse(((conf_co_DORM1$Caso4_temp < c) & (conf_co_DORM1$Caso4_temp > f) & (conf_co_DORM1$ocup > 0)),1,0)
conf_co_DORM1$Caso4_cooling = ifelse((conf_co_DORM1$Caso4_calor > 0),conf_co_DORM1$Caso4_cooling,0)
conf_co_DORM1$Caso4_heating = ifelse((conf_co_DORM1$Caso4_frio > 0),conf_co_DORM1$Caso4_heating,0)

conf_co_DORM1$Caso5_calor = ifelse((conf_co_DORM1$Caso5_temp >= c) & (conf_co_DORM1$ocup > 0),1,0)
conf_co_DORM1$Caso5_frio = ifelse((conf_co_DORM1$Caso5_temp <= f) & (conf_co_DORM1$ocup > 0),1,0)
conf_co_DORM1$Caso5_conf = ifelse(((conf_co_DORM1$Caso5_temp < c) & (conf_co_DORM1$Caso5_temp > f) & (conf_co_DORM1$ocup > 0)),1,0)
conf_co_DORM1$Caso5_cooling = ifelse((conf_co_DORM1$Caso5_calor > 0),conf_co_DORM1$Caso5_cooling,0)
conf_co_DORM1$Caso5_heating = ifelse((conf_co_DORM1$Caso5_frio > 0),conf_co_DORM1$Caso5_heating,0)

conf_co_DORM2 = data.frame(APP=c('DORM2'),
                            ocup=ocup_DORM2,
                            temp_ext=vn_Caso1_sp_sta_co_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.,
                            REF_temp=vn_Caso1_sp_sta_co_1.csv$DORM2.Zone.Operative.Temperature..C..TimeStep.,
                            REF_cooling=ac1_Caso1_sp_sta_co_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            REF_heating=ac1_Caso1_sp_sta_co_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                            Caso2_temp=vn_Caso2_1.csv$DORM2.Zone.Operative.Temperature..C..TimeStep.,
                            Caso2_cooling=ac1_Caso2_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            Caso2_heating=ac1_Caso2_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                            Caso3_temp=vn_Caso3_1.csv$DORM2.Zone.Operative.Temperature..C..TimeStep.,
                            Caso3_cooling=ac1_Caso3_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            Caso3_heating=ac1_Caso3_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                            Caso4_temp=vn_Caso4_1.csv$DORM2.Zone.Operative.Temperature..C..TimeStep.,
                            Caso4_cooling=ac1_Caso4_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            Caso4_heating=ac1_Caso4_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                            Caso5_temp=vn_Caso5_1.csv$DORM2.Zone.Operative.Temperature..C..TimeStep.,
                            Caso5_cooling=ac1_Caso5_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            Caso5_heating=ac1_Caso5_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.)

conf_co_DORM2$EXT_calor = ifelse((conf_co_DORM2$temp_ext >= c & conf_co_DORM2$ocup > 0),1,0)
conf_co_DORM2$EXT_frio = ifelse((conf_co_DORM2$temp_ext <= f) & (conf_co_DORM2$ocup > 0),1,0)
conf_co_DORM2$EXT_conf = ifelse(((conf_co_DORM2$temp_ext < c) & (conf_co_DORM2$temp_ext > f) & (conf_co_DORM2$ocup > 0)),1,0)

conf_co_DORM2$REF_calor = ifelse((conf_co_DORM2$REF_temp >= c) & (conf_co_DORM2$ocup > 0),1,0)
conf_co_DORM2$REF_frio = ifelse((conf_co_DORM2$REF_temp <= f) & (conf_co_DORM2$ocup > 0),1,0)
conf_co_DORM2$REF_conf = ifelse(((conf_co_DORM2$REF_temp < c) & (conf_co_DORM2$REF_temp > f) & (conf_co_DORM2$ocup > 0)),1,0)
conf_co_DORM2$REF_cooling = ifelse((conf_co_DORM2$REF_calor > 0),conf_co_DORM2$REF_cooling,0)
conf_co_DORM2$REF_heating = ifelse((conf_co_DORM2$REF_frio > 0),conf_co_DORM2$REF_heating,0)

conf_co_DORM2$Caso2_calor = ifelse((conf_co_DORM2$Caso2_temp >= c) & (conf_co_DORM2$ocup > 0),1,0)
conf_co_DORM2$Caso2_frio = ifelse((conf_co_DORM2$Caso2_temp <= f) & (conf_co_DORM2$ocup > 0),1,0)
conf_co_DORM2$Caso2_conf = ifelse(((conf_co_DORM2$Caso2_temp < c) & (conf_co_DORM2$Caso2_temp > f) & (conf_co_DORM2$ocup > 0)),1,0)
conf_co_DORM2$Caso2_cooling = ifelse((conf_co_DORM2$Caso2_calor > 0),conf_co_DORM2$Caso2_cooling,0)
conf_co_DORM2$Caso2_heating = ifelse((conf_co_DORM2$Caso2_frio > 0),conf_co_DORM2$Caso2_heating,0)

conf_co_DORM2$Caso3_calor = ifelse((conf_co_DORM2$Caso3_temp >= c) & (conf_co_DORM2$ocup > 0),1,0)
conf_co_DORM2$Caso3_frio = ifelse((conf_co_DORM2$Caso3_temp <= f) & (conf_co_DORM2$ocup > 0),1,0)
conf_co_DORM2$Caso3_conf = ifelse(((conf_co_DORM2$Caso3_temp < c) & (conf_co_DORM2$Caso3_temp > f) & (conf_co_DORM2$ocup > 0)),1,0)
conf_co_DORM2$Caso3_cooling = ifelse((conf_co_DORM2$Caso3_calor > 0),conf_co_DORM2$Caso3_cooling,0)
conf_co_DORM2$Caso3_heating = ifelse((conf_co_DORM2$Caso3_frio > 0),conf_co_DORM2$Caso3_heating,0)

conf_co_DORM2$Caso4_calor = ifelse((conf_co_DORM2$Caso4_temp >= c) & (conf_co_DORM2$ocup > 0),1,0)
conf_co_DORM2$Caso4_frio = ifelse((conf_co_DORM2$Caso4_temp <= f) & (conf_co_DORM2$ocup > 0),1,0)
conf_co_DORM2$Caso4_conf = ifelse(((conf_co_DORM2$Caso4_temp < c) & (conf_co_DORM2$Caso4_temp > f) & (conf_co_DORM2$ocup > 0)),1,0)
conf_co_DORM2$Caso4_cooling = ifelse((conf_co_DORM2$Caso4_calor > 0),conf_co_DORM2$Caso4_cooling,0)
conf_co_DORM2$Caso4_heating = ifelse((conf_co_DORM2$Caso4_frio > 0),conf_co_DORM2$Caso4_heating,0)

conf_co_DORM2$Caso5_calor = ifelse((conf_co_DORM2$Caso5_temp >= c) & (conf_co_DORM2$ocup > 0),1,0)
conf_co_DORM2$Caso5_frio = ifelse((conf_co_DORM2$Caso5_temp <= f) & (conf_co_DORM2$ocup > 0),1,0)
conf_co_DORM2$Caso5_conf = ifelse(((conf_co_DORM2$Caso5_temp < c) & (conf_co_DORM2$Caso5_temp > f) & (conf_co_DORM2$ocup > 0)),1,0)
conf_co_DORM2$Caso5_cooling = ifelse((conf_co_DORM2$Caso5_calor > 0),conf_co_DORM2$Caso5_cooling,0)
conf_co_DORM2$Caso5_heating = ifelse((conf_co_DORM2$Caso5_frio > 0),conf_co_DORM2$Caso5_heating,0)

conf_co_SALA = data.frame(APP=c('SALA'),
                           ocup=ocup_SALA,
                           temp_ext=vn_Caso1_sp_sta_co_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.,
                           REF_temp=vn_Caso1_sp_sta_co_1.csv$SALA.Zone.Operative.Temperature..C..TimeStep.,
                           REF_cooling=ac1_Caso1_sp_sta_co_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                           REF_heating=ac1_Caso1_sp_sta_co_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                           Caso2_temp=vn_Caso2_1.csv$SALA.Zone.Operative.Temperature..C..TimeStep.,
                           Caso2_cooling=ac1_Caso2_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                           Caso2_heating=ac1_Caso2_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                           Caso3_temp=vn_Caso3_1.csv$SALA.Zone.Operative.Temperature..C..TimeStep.,
                           Caso3_cooling=ac1_Caso3_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                           Caso3_heating=ac1_Caso3_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                           Caso4_temp=vn_Caso4_1.csv$SALA.Zone.Operative.Temperature..C..TimeStep.,
                           Caso4_cooling=ac1_Caso4_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                           Caso4_heating=ac1_Caso4_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                           Caso5_temp=vn_Caso5_1.csv$SALA.Zone.Operative.Temperature..C..TimeStep.,
                           Caso5_cooling=ac1_Caso5_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                           Caso5_heating=ac1_Caso5_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.)

conf_co_SALA$EXT_calor = ifelse((conf_co_SALA$temp_ext >= c & conf_co_SALA$ocup > 0),1,0)
conf_co_SALA$EXT_frio = ifelse((conf_co_SALA$temp_ext <= f) & (conf_co_SALA$ocup > 0),1,0)
conf_co_SALA$EXT_conf = ifelse(((conf_co_SALA$temp_ext < c) & (conf_co_SALA$temp_ext > f) & (conf_co_SALA$ocup > 0)),1,0)

conf_co_SALA$REF_calor = ifelse((conf_co_SALA$REF_temp >= c) & (conf_co_SALA$ocup > 0),1,0)
conf_co_SALA$REF_frio = ifelse((conf_co_SALA$REF_temp <= f) & (conf_co_SALA$ocup > 0),1,0)
conf_co_SALA$REF_conf = ifelse(((conf_co_SALA$REF_temp < c) & (conf_co_SALA$REF_temp > f) & (conf_co_SALA$ocup > 0)),1,0)
conf_co_SALA$REF_cooling = ifelse((conf_co_SALA$REF_calor > 0),conf_co_SALA$REF_cooling,0)
conf_co_SALA$REF_heating = ifelse((conf_co_SALA$REF_frio > 0),conf_co_SALA$REF_heating,0)

conf_co_SALA$Caso2_calor = ifelse((conf_co_SALA$Caso2_temp >= c) & (conf_co_SALA$ocup > 0),1,0)
conf_co_SALA$Caso2_frio = ifelse((conf_co_SALA$Caso2_temp <= f) & (conf_co_SALA$ocup > 0),1,0)
conf_co_SALA$Caso2_conf = ifelse(((conf_co_SALA$Caso2_temp < c) & (conf_co_SALA$Caso2_temp > f) & (conf_co_SALA$ocup > 0)),1,0)
conf_co_SALA$Caso2_cooling = ifelse((conf_co_SALA$Caso2_calor > 0),conf_co_SALA$Caso2_cooling,0)
conf_co_SALA$Caso2_heating = ifelse((conf_co_SALA$Caso2_frio > 0),conf_co_SALA$Caso2_heating,0)

conf_co_SALA$Caso3_calor = ifelse((conf_co_SALA$Caso3_temp >= c) & (conf_co_SALA$ocup > 0),1,0)
conf_co_SALA$Caso3_frio = ifelse((conf_co_SALA$Caso3_temp <= f) & (conf_co_SALA$ocup > 0),1,0)
conf_co_SALA$Caso3_conf = ifelse(((conf_co_SALA$Caso3_temp < c) & (conf_co_SALA$Caso3_temp > f) & (conf_co_SALA$ocup > 0)),1,0)
conf_co_SALA$Caso3_cooling = ifelse((conf_co_SALA$Caso3_calor > 0),conf_co_SALA$Caso3_cooling,0)
conf_co_SALA$Caso3_heating = ifelse((conf_co_SALA$Caso3_frio > 0),conf_co_SALA$Caso3_heating,0)

conf_co_SALA$Caso4_calor = ifelse((conf_co_SALA$Caso4_temp >= c) & (conf_co_SALA$ocup > 0),1,0)
conf_co_SALA$Caso4_frio = ifelse((conf_co_SALA$Caso4_temp <= f) & (conf_co_SALA$ocup > 0),1,0)
conf_co_SALA$Caso4_conf = ifelse(((conf_co_SALA$Caso4_temp < c) & (conf_co_SALA$Caso4_temp > f) & (conf_co_SALA$ocup > 0)),1,0)
conf_co_SALA$Caso4_cooling = ifelse((conf_co_SALA$Caso4_calor > 0),conf_co_SALA$Caso4_cooling,0)
conf_co_SALA$Caso4_heating = ifelse((conf_co_SALA$Caso4_frio > 0),conf_co_SALA$Caso4_heating,0)

conf_co_SALA$Caso5_calor = ifelse((conf_co_SALA$Caso5_temp >= c) & (conf_co_SALA$ocup > 0),1,0)
conf_co_SALA$Caso5_frio = ifelse((conf_co_SALA$Caso5_temp <= f) & (conf_co_SALA$ocup > 0),1,0)
conf_co_SALA$Caso5_conf = ifelse(((conf_co_SALA$Caso5_temp < c) & (conf_co_SALA$Caso5_temp > f) & (conf_co_SALA$ocup > 0)),1,0)
conf_co_SALA$Caso5_cooling = ifelse((conf_co_SALA$Caso5_calor > 0),conf_co_SALA$Caso5_cooling,0)
conf_co_SALA$Caso5_heating = ifelse((conf_co_SALA$Caso5_frio > 0),conf_co_SALA$Caso5_heating,0)

caso = c('EXT','REF','Caso2','Caso3','Caso4','Caso5')
caso1 = c('REF','Caso2','Caso3','Caso4','Caso5')
app = c('DORM1','DORM2','SALA')
tipo = c('frio','calor','conf')
hvac = c('cooling','heating')

conf_co = data.frame(CASO = rep(c('EXT','REF','Caso2','Caso3','Caso4','Caso5'),each=3), APP = c('DORM1','DORM2','SALA'), PHFT=c(1:1),Tipo=rep(c('frio','calor','conf'),each=18))

df = rbind(conf_co_DORM1,conf_co_DORM2,conf_co_SALA)

for(d in caso){
  for(a in app){
    for(t in tipo){
      conf_co$PHFT[conf_co$APP == a & conf_co$CASO == d & conf_co$Tipo == t] = round(((sum(df[,grepl(paste0(d,'_',t),colnames(df))][df$APP == a])/sum(df$ocup[df$APP == a]))*100),1)
    }
  }
}

ref_co = data.frame(CASO = rep(c('REF','Caso2','Caso3','Caso4','Caso5'),each=3), APP = c('DORM1','DORM2','SALA'), Cgt=rep(c('cooling','heating'),each=15), Cgt_kwh=c(1:1))

for(d in caso1){
  for(a in app){
    for(h in hvac){
      ref_co$Cgt_kwh[ref_co$APP == a & ref_co$CASO == d & ref_co$Cgt == h] = round((sum(df[,grepl(paste0(d,'_',h),colnames(df))][df$APP == a])/3600000),1)
    }
  }
}

conf_co_uh = data.frame(CASO = c('EXT','REF','Caso2','Caso3','Caso4','Caso5'), PHFT=c(1:1), Tipo=rep(c('frio','calor','conf'),each=6))
for(d in caso){
  for(t in tipo){
    conf_co_uh$PHFT[conf_co_uh$CASO == d & conf_co_uh$Tipo == t] = round((sum(conf_co$PHFT[conf_co$CASO == d & conf_co$Tipo == t])/3),1)
  }
}

ref_co_uh = data.frame(CASO = c('REF','Caso2','Caso3','Caso4','Caso5'), Cgt=rep(c('cooling','heating'),each=15),Cgt_kwh=c(1:1))

for(d in caso1){
  for(h in hvac){
    ref_co_uh$Cgt_kwh[ref_co_uh$CASO == d & ref_co_uh$Cgt == h] = round((sum(ref_co$Cgt_kwh[ref_co$CASO == d & ref_co$Cgt == h])),1) 
  }
}

setwd('D:/Frentes de Trabalho/simu_NBR/novos_graphs')

write.csv(ref_co, 'cgt_co.csv')
write.csv(ref_co_uh, 'cgt_co_uh.csv')
write.csv(conf_co, 'conf_co.csv')
write.csv(conf_co_uh, 'conf_co_uh.csv')

rm(list = ls())

## sao paulo ----

setwd("D:/Frentes de Trabalho/simu_NBR/VNxAC_janela/BRA_SP_Sao.Paulo-Congonhas.AP.837800_TMYx.2003-2017/")

file_list = list.files(pattern="*_1.csv") # todos os csv menos o table

library("data.table")

for (i in 1:5){ #eu sei que os 5 primeiros sao hvac
  assign(file_list[i], read.csv(file_list[i])[,c("Date.Time",
                                                 "Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.",
                                                 "DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.",
                                                 "DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.",
                                                 "SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.",
                                                 "SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.",
                                                 "DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.",
                                                 "DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.")])
  for(k in 6:10){ # eu sei que os 5 ultimos sao vn
    assign(file_list[k], read.csv(file_list[k])[,c("Date.Time",
                                                   "Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.",
                                                   "DORMITORIO1.People.Occupant.Count....TimeStep.",
                                                   "DORMITORIO2.People.Occupant.Count....TimeStep.",
                                                   "SALA1.People.Occupant.Count....TimeStep.",
                                                   "DORM2.Zone.Operative.Temperature..C..TimeStep.",
                                                   "DORM2.Zone.Mean.Air.Temperature..C..TimeStep.",
                                                   "DORM1.Zone.Operative.Temperature..C..TimeStep.",
                                                   "DORM1.Zone.Mean.Air.Temperature..C..TimeStep.",
                                                   "SALA.Zone.Operative.Temperature..C..TimeStep.",
                                                   "SALA.Zone.Mean.Air.Temperature..C..TimeStep.")])
  }
  
}

ocup_DORM1 = data.frame(ocup_DORM = vn_Caso1_sp_sta_co_1.csv$DORMITORIO1.People.Occupant.Count....TimeStep.)
ocup_DORM1$ocup = ifelse(ocup_DORM1$ocup_DORM > 0,1,0)
ocup_DORM1 = subset(ocup_DORM1, select = c("ocup"))

ocup_DORM2 = subset(ocup_DORM1, select = c("ocup"))  ## a ocupacao eh igual

ocup_SALA = data.frame(ocup_DORM = vn_Caso1_sp_sta_co_1.csv$SALA1.People.Occupant.Count....TimeStep.)
ocup_SALA$ocup = ifelse(ocup_SALA$ocup_DORM > 0,1,0)
ocup_SALA = subset(ocup_SALA, select = c("ocup"))

# ocup = rbind(ocup_dorm1,ocup_dorm2,ocup_sala)
# rm(ocup_dorm1,ocup_dorm2,ocup_sala)

# temperatura ----

c = as.numeric(26) ## limite calor
f = as.numeric(18) ## limite frio

conf_sp_DORM1 = data.frame(APP=c('DORM1'),
                            ocup=ocup_DORM1,
                            temp_ext=vn_Caso1_sp_sta_co_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.,
                            REF_temp=vn_Caso1_sp_sta_co_1.csv$DORM1.Zone.Operative.Temperature..C..TimeStep.,
                            REF_cooling=ac1_Caso1_sp_sta_co_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            REF_heating=ac1_Caso1_sp_sta_co_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                            Caso2_temp=vn_Caso2_1.csv$DORM1.Zone.Operative.Temperature..C..TimeStep.,
                            Caso2_cooling=ac1_Caso2_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            Caso2_heating=ac1_Caso2_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                            Caso3_temp=vn_Caso3_1.csv$DORM1.Zone.Operative.Temperature..C..TimeStep.,
                            Caso3_cooling=ac1_Caso3_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            Caso3_heating=ac1_Caso3_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                            Caso4_temp=vn_Caso4_1.csv$DORM1.Zone.Operative.Temperature..C..TimeStep.,
                            Caso4_cooling=ac1_Caso4_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            Caso4_heating=ac1_Caso4_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                            Caso5_temp=vn_Caso5_1.csv$DORM1.Zone.Operative.Temperature..C..TimeStep.,
                            Caso5_cooling=ac1_Caso5_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            Caso5_heating=ac1_Caso5_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.)

conf_sp_DORM1$EXT_calor = ifelse((conf_sp_DORM1$temp_ext >= c & conf_sp_DORM1$ocup > 0),1,0)
conf_sp_DORM1$EXT_frio = ifelse((conf_sp_DORM1$temp_ext <= f) & (conf_sp_DORM1$ocup > 0),1,0)
conf_sp_DORM1$EXT_conf = ifelse(((conf_sp_DORM1$temp_ext < c) & (conf_sp_DORM1$temp_ext > f) & (conf_sp_DORM1$ocup > 0)),1,0)

conf_sp_DORM1$REF_calor = ifelse((conf_sp_DORM1$REF_temp >= c) & (conf_sp_DORM1$ocup > 0),1,0)
conf_sp_DORM1$REF_frio = ifelse((conf_sp_DORM1$REF_temp <= f) & (conf_sp_DORM1$ocup > 0),1,0)
conf_sp_DORM1$REF_conf = ifelse(((conf_sp_DORM1$REF_temp < c) & (conf_sp_DORM1$REF_temp > f) & (conf_sp_DORM1$ocup > 0)),1,0)
conf_sp_DORM1$REF_cooling = ifelse((conf_sp_DORM1$REF_calor > 0),conf_sp_DORM1$REF_cooling,0)
conf_sp_DORM1$REF_heating = ifelse((conf_sp_DORM1$REF_frio > 0),conf_sp_DORM1$REF_heating,0)

conf_sp_DORM1$Caso2_calor = ifelse((conf_sp_DORM1$Caso2_temp >= c) & (conf_sp_DORM1$ocup > 0),1,0)
conf_sp_DORM1$Caso2_frio = ifelse((conf_sp_DORM1$Caso2_temp <= f) & (conf_sp_DORM1$ocup > 0),1,0)
conf_sp_DORM1$Caso2_conf = ifelse(((conf_sp_DORM1$Caso2_temp < c) & (conf_sp_DORM1$Caso2_temp > f) & (conf_sp_DORM1$ocup > 0)),1,0)
conf_sp_DORM1$Caso2_cooling = ifelse((conf_sp_DORM1$Caso2_calor > 0),conf_sp_DORM1$Caso2_cooling,0)
conf_sp_DORM1$Caso2_heating = ifelse((conf_sp_DORM1$Caso2_frio > 0),conf_sp_DORM1$Caso2_heating,0)

conf_sp_DORM1$Caso3_calor = ifelse((conf_sp_DORM1$Caso3_temp >= c) & (conf_sp_DORM1$ocup > 0),1,0)
conf_sp_DORM1$Caso3_frio = ifelse((conf_sp_DORM1$Caso3_temp <= f) & (conf_sp_DORM1$ocup > 0),1,0)
conf_sp_DORM1$Caso3_conf = ifelse(((conf_sp_DORM1$Caso3_temp < c) & (conf_sp_DORM1$Caso3_temp > f) & (conf_sp_DORM1$ocup > 0)),1,0)
conf_sp_DORM1$Caso3_cooling = ifelse((conf_sp_DORM1$Caso3_calor > 0),conf_sp_DORM1$Caso3_cooling,0)
conf_sp_DORM1$Caso3_heating = ifelse((conf_sp_DORM1$Caso3_frio > 0),conf_sp_DORM1$Caso3_heating,0)

conf_sp_DORM1$Caso4_calor = ifelse((conf_sp_DORM1$Caso4_temp >= c) & (conf_sp_DORM1$ocup > 0),1,0)
conf_sp_DORM1$Caso4_frio = ifelse((conf_sp_DORM1$Caso4_temp <= f) & (conf_sp_DORM1$ocup > 0),1,0)
conf_sp_DORM1$Caso4_conf = ifelse(((conf_sp_DORM1$Caso4_temp < c) & (conf_sp_DORM1$Caso4_temp > f) & (conf_sp_DORM1$ocup > 0)),1,0)
conf_sp_DORM1$Caso4_cooling = ifelse((conf_sp_DORM1$Caso4_calor > 0),conf_sp_DORM1$Caso4_cooling,0)
conf_sp_DORM1$Caso4_heating = ifelse((conf_sp_DORM1$Caso4_frio > 0),conf_sp_DORM1$Caso4_heating,0)

conf_sp_DORM1$Caso5_calor = ifelse((conf_sp_DORM1$Caso5_temp >= c) & (conf_sp_DORM1$ocup > 0),1,0)
conf_sp_DORM1$Caso5_frio = ifelse((conf_sp_DORM1$Caso5_temp <= f) & (conf_sp_DORM1$ocup > 0),1,0)
conf_sp_DORM1$Caso5_conf = ifelse(((conf_sp_DORM1$Caso5_temp < c) & (conf_sp_DORM1$Caso5_temp > f) & (conf_sp_DORM1$ocup > 0)),1,0)
conf_sp_DORM1$Caso5_cooling = ifelse((conf_sp_DORM1$Caso5_calor > 0),conf_sp_DORM1$Caso5_cooling,0)
conf_sp_DORM1$Caso5_heating = ifelse((conf_sp_DORM1$Caso5_frio > 0),conf_sp_DORM1$Caso5_heating,0)

conf_sp_DORM2 = data.frame(APP=c('DORM2'),
                            ocup=ocup_DORM2,
                            temp_ext=vn_Caso1_sp_sta_co_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.,
                            REF_temp=vn_Caso1_sp_sta_co_1.csv$DORM2.Zone.Operative.Temperature..C..TimeStep.,
                            REF_cooling=ac1_Caso1_sp_sta_co_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            REF_heating=ac1_Caso1_sp_sta_co_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                            Caso2_temp=vn_Caso2_1.csv$DORM2.Zone.Operative.Temperature..C..TimeStep.,
                            Caso2_cooling=ac1_Caso2_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            Caso2_heating=ac1_Caso2_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                            Caso3_temp=vn_Caso3_1.csv$DORM2.Zone.Operative.Temperature..C..TimeStep.,
                            Caso3_cooling=ac1_Caso3_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            Caso3_heating=ac1_Caso3_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                            Caso4_temp=vn_Caso4_1.csv$DORM2.Zone.Operative.Temperature..C..TimeStep.,
                            Caso4_cooling=ac1_Caso4_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            Caso4_heating=ac1_Caso4_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                            Caso5_temp=vn_Caso5_1.csv$DORM2.Zone.Operative.Temperature..C..TimeStep.,
                            Caso5_cooling=ac1_Caso5_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            Caso5_heating=ac1_Caso5_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.)

conf_sp_DORM2$EXT_calor = ifelse((conf_sp_DORM2$temp_ext >= c & conf_sp_DORM2$ocup > 0),1,0)
conf_sp_DORM2$EXT_frio = ifelse((conf_sp_DORM2$temp_ext <= f) & (conf_sp_DORM2$ocup > 0),1,0)
conf_sp_DORM2$EXT_conf = ifelse(((conf_sp_DORM2$temp_ext < c) & (conf_sp_DORM2$temp_ext > f) & (conf_sp_DORM2$ocup > 0)),1,0)

conf_sp_DORM2$REF_calor = ifelse((conf_sp_DORM2$REF_temp >= c) & (conf_sp_DORM2$ocup > 0),1,0)
conf_sp_DORM2$REF_frio = ifelse((conf_sp_DORM2$REF_temp <= f) & (conf_sp_DORM2$ocup > 0),1,0)
conf_sp_DORM2$REF_conf = ifelse(((conf_sp_DORM2$REF_temp < c) & (conf_sp_DORM2$REF_temp > f) & (conf_sp_DORM2$ocup > 0)),1,0)
conf_sp_DORM2$REF_cooling = ifelse((conf_sp_DORM2$REF_calor > 0),conf_sp_DORM2$REF_cooling,0)
conf_sp_DORM2$REF_heating = ifelse((conf_sp_DORM2$REF_frio > 0),conf_sp_DORM2$REF_heating,0)

conf_sp_DORM2$Caso2_calor = ifelse((conf_sp_DORM2$Caso2_temp >= c) & (conf_sp_DORM2$ocup > 0),1,0)
conf_sp_DORM2$Caso2_frio = ifelse((conf_sp_DORM2$Caso2_temp <= f) & (conf_sp_DORM2$ocup > 0),1,0)
conf_sp_DORM2$Caso2_conf = ifelse(((conf_sp_DORM2$Caso2_temp < c) & (conf_sp_DORM2$Caso2_temp > f) & (conf_sp_DORM2$ocup > 0)),1,0)
conf_sp_DORM2$Caso2_cooling = ifelse((conf_sp_DORM2$Caso2_calor > 0),conf_sp_DORM2$Caso2_cooling,0)
conf_sp_DORM2$Caso2_heating = ifelse((conf_sp_DORM2$Caso2_frio > 0),conf_sp_DORM2$Caso2_heating,0)

conf_sp_DORM2$Caso3_calor = ifelse((conf_sp_DORM2$Caso3_temp >= c) & (conf_sp_DORM2$ocup > 0),1,0)
conf_sp_DORM2$Caso3_frio = ifelse((conf_sp_DORM2$Caso3_temp <= f) & (conf_sp_DORM2$ocup > 0),1,0)
conf_sp_DORM2$Caso3_conf = ifelse(((conf_sp_DORM2$Caso3_temp < c) & (conf_sp_DORM2$Caso3_temp > f) & (conf_sp_DORM2$ocup > 0)),1,0)
conf_sp_DORM2$Caso3_cooling = ifelse((conf_sp_DORM2$Caso3_calor > 0),conf_sp_DORM2$Caso3_cooling,0)
conf_sp_DORM2$Caso3_heating = ifelse((conf_sp_DORM2$Caso3_frio > 0),conf_sp_DORM2$Caso3_heating,0)

conf_sp_DORM2$Caso4_calor = ifelse((conf_sp_DORM2$Caso4_temp >= c) & (conf_sp_DORM2$ocup > 0),1,0)
conf_sp_DORM2$Caso4_frio = ifelse((conf_sp_DORM2$Caso4_temp <= f) & (conf_sp_DORM2$ocup > 0),1,0)
conf_sp_DORM2$Caso4_conf = ifelse(((conf_sp_DORM2$Caso4_temp < c) & (conf_sp_DORM2$Caso4_temp > f) & (conf_sp_DORM2$ocup > 0)),1,0)
conf_sp_DORM2$Caso4_cooling = ifelse((conf_sp_DORM2$Caso4_calor > 0),conf_sp_DORM2$Caso4_cooling,0)
conf_sp_DORM2$Caso4_heating = ifelse((conf_sp_DORM2$Caso4_frio > 0),conf_sp_DORM2$Caso4_heating,0)

conf_sp_DORM2$Caso5_calor = ifelse((conf_sp_DORM2$Caso5_temp >= c) & (conf_sp_DORM2$ocup > 0),1,0)
conf_sp_DORM2$Caso5_frio = ifelse((conf_sp_DORM2$Caso5_temp <= f) & (conf_sp_DORM2$ocup > 0),1,0)
conf_sp_DORM2$Caso5_conf = ifelse(((conf_sp_DORM2$Caso5_temp < c) & (conf_sp_DORM2$Caso5_temp > f) & (conf_sp_DORM2$ocup > 0)),1,0)
conf_sp_DORM2$Caso5_cooling = ifelse((conf_sp_DORM2$Caso5_calor > 0),conf_sp_DORM2$Caso5_cooling,0)
conf_sp_DORM2$Caso5_heating = ifelse((conf_sp_DORM2$Caso5_frio > 0),conf_sp_DORM2$Caso5_heating,0)

conf_sp_SALA = data.frame(APP=c('SALA'),
                           ocup=ocup_SALA,
                           temp_ext=vn_Caso1_sp_sta_co_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.,
                           REF_temp=vn_Caso1_sp_sta_co_1.csv$SALA.Zone.Operative.Temperature..C..TimeStep.,
                           REF_cooling=ac1_Caso1_sp_sta_co_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                           REF_heating=ac1_Caso1_sp_sta_co_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                           Caso2_temp=vn_Caso2_1.csv$SALA.Zone.Operative.Temperature..C..TimeStep.,
                           Caso2_cooling=ac1_Caso2_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                           Caso2_heating=ac1_Caso2_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                           Caso3_temp=vn_Caso3_1.csv$SALA.Zone.Operative.Temperature..C..TimeStep.,
                           Caso3_cooling=ac1_Caso3_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                           Caso3_heating=ac1_Caso3_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                           Caso4_temp=vn_Caso4_1.csv$SALA.Zone.Operative.Temperature..C..TimeStep.,
                           Caso4_cooling=ac1_Caso4_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                           Caso4_heating=ac1_Caso4_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                           Caso5_temp=vn_Caso5_1.csv$SALA.Zone.Operative.Temperature..C..TimeStep.,
                           Caso5_cooling=ac1_Caso5_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                           Caso5_heating=ac1_Caso5_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.)

conf_sp_SALA$EXT_calor = ifelse((conf_sp_SALA$temp_ext >= c & conf_sp_SALA$ocup > 0),1,0)
conf_sp_SALA$EXT_frio = ifelse((conf_sp_SALA$temp_ext <= f) & (conf_sp_SALA$ocup > 0),1,0)
conf_sp_SALA$EXT_conf = ifelse(((conf_sp_SALA$temp_ext < c) & (conf_sp_SALA$temp_ext > f) & (conf_sp_SALA$ocup > 0)),1,0)

conf_sp_SALA$REF_calor = ifelse((conf_sp_SALA$REF_temp >= c) & (conf_sp_SALA$ocup > 0),1,0)
conf_sp_SALA$REF_frio = ifelse((conf_sp_SALA$REF_temp <= f) & (conf_sp_SALA$ocup > 0),1,0)
conf_sp_SALA$REF_conf = ifelse(((conf_sp_SALA$REF_temp < c) & (conf_sp_SALA$REF_temp > f) & (conf_sp_SALA$ocup > 0)),1,0)
conf_sp_SALA$REF_cooling = ifelse((conf_sp_SALA$REF_calor > 0),conf_sp_SALA$REF_cooling,0)
conf_sp_SALA$REF_heating = ifelse((conf_sp_SALA$REF_frio > 0),conf_sp_SALA$REF_heating,0)

conf_sp_SALA$Caso2_calor = ifelse((conf_sp_SALA$Caso2_temp >= c) & (conf_sp_SALA$ocup > 0),1,0)
conf_sp_SALA$Caso2_frio = ifelse((conf_sp_SALA$Caso2_temp <= f) & (conf_sp_SALA$ocup > 0),1,0)
conf_sp_SALA$Caso2_conf = ifelse(((conf_sp_SALA$Caso2_temp < c) & (conf_sp_SALA$Caso2_temp > f) & (conf_sp_SALA$ocup > 0)),1,0)
conf_sp_SALA$Caso2_cooling = ifelse((conf_sp_SALA$Caso2_calor > 0),conf_sp_SALA$Caso2_cooling,0)
conf_sp_SALA$Caso2_heating = ifelse((conf_sp_SALA$Caso2_frio > 0),conf_sp_SALA$Caso2_heating,0)

conf_sp_SALA$Caso3_calor = ifelse((conf_sp_SALA$Caso3_temp >= c) & (conf_sp_SALA$ocup > 0),1,0)
conf_sp_SALA$Caso3_frio = ifelse((conf_sp_SALA$Caso3_temp <= f) & (conf_sp_SALA$ocup > 0),1,0)
conf_sp_SALA$Caso3_conf = ifelse(((conf_sp_SALA$Caso3_temp < c) & (conf_sp_SALA$Caso3_temp > f) & (conf_sp_SALA$ocup > 0)),1,0)
conf_sp_SALA$Caso3_cooling = ifelse((conf_sp_SALA$Caso3_calor > 0),conf_sp_SALA$Caso3_cooling,0)
conf_sp_SALA$Caso3_heating = ifelse((conf_sp_SALA$Caso3_frio > 0),conf_sp_SALA$Caso3_heating,0)

conf_sp_SALA$Caso4_calor = ifelse((conf_sp_SALA$Caso4_temp >= c) & (conf_sp_SALA$ocup > 0),1,0)
conf_sp_SALA$Caso4_frio = ifelse((conf_sp_SALA$Caso4_temp <= f) & (conf_sp_SALA$ocup > 0),1,0)
conf_sp_SALA$Caso4_conf = ifelse(((conf_sp_SALA$Caso4_temp < c) & (conf_sp_SALA$Caso4_temp > f) & (conf_sp_SALA$ocup > 0)),1,0)
conf_sp_SALA$Caso4_cooling = ifelse((conf_sp_SALA$Caso4_calor > 0),conf_sp_SALA$Caso4_cooling,0)
conf_sp_SALA$Caso4_heating = ifelse((conf_sp_SALA$Caso4_frio > 0),conf_sp_SALA$Caso4_heating,0)

conf_sp_SALA$Caso5_calor = ifelse((conf_sp_SALA$Caso5_temp >= c) & (conf_sp_SALA$ocup > 0),1,0)
conf_sp_SALA$Caso5_frio = ifelse((conf_sp_SALA$Caso5_temp <= f) & (conf_sp_SALA$ocup > 0),1,0)
conf_sp_SALA$Caso5_conf = ifelse(((conf_sp_SALA$Caso5_temp < c) & (conf_sp_SALA$Caso5_temp > f) & (conf_sp_SALA$ocup > 0)),1,0)
conf_sp_SALA$Caso5_cooling = ifelse((conf_sp_SALA$Caso5_calor > 0),conf_sp_SALA$Caso5_cooling,0)
conf_sp_SALA$Caso5_heating = ifelse((conf_sp_SALA$Caso5_frio > 0),conf_sp_SALA$Caso5_heating,0)

caso = c('EXT','REF','Caso2','Caso3','Caso4','Caso5')
caso1 = c('REF','Caso2','Caso3','Caso4','Caso5')
app = c('DORM1','DORM2','SALA')
tipo = c('frio','calor','conf')
hvac = c('cooling','heating')

conf_sp = data.frame(CASO = rep(c('EXT','REF','Caso2','Caso3','Caso4','Caso5'),each=3), APP = c('DORM1','DORM2','SALA'), PHFT=c(1:1),Tipo=rep(c('frio','calor','conf'),each=18))

df = rbind(conf_sp_DORM1,conf_sp_DORM2,conf_sp_SALA)

for(d in caso){
  for(a in app){
    for(t in tipo){
      conf_sp$PHFT[conf_sp$APP == a & conf_sp$CASO == d & conf_sp$Tipo == t] = round(((sum(df[,grepl(paste0(d,'_',t),colnames(df))][df$APP == a])/sum(df$ocup[df$APP == a]))*100),1)
    }
  }
}

ref_sp = data.frame(CASO = rep(c('REF','Caso2','Caso3','Caso4','Caso5'),each=3), APP = c('DORM1','DORM2','SALA'), Cgt=rep(c('cooling','heating'),each=15), Cgt_kwh=c(1:1))

for(d in caso1){
  for(a in app){
    for(h in hvac){
      ref_sp$Cgt_kwh[ref_sp$APP == a & ref_sp$CASO == d & ref_sp$Cgt == h] = round((sum(df[,grepl(paste0(d,'_',h),colnames(df))][df$APP == a])/3600000),1)
    }
  }
}

conf_sp_uh = data.frame(CASO = c('EXT','REF','Caso2','Caso3','Caso4','Caso5'), PHFT=c(1:1), Tipo=rep(c('frio','calor','conf'),each=6))
for(d in caso){
  for(t in tipo){
    conf_sp_uh$PHFT[conf_sp_uh$CASO == d & conf_sp_uh$Tipo == t] = round((sum(conf_sp$PHFT[conf_sp$CASO == d & conf_sp$Tipo == t])/3),1)
  }
}

ref_sp_uh = data.frame(CASO = c('REF','Caso2','Caso3','Caso4','Caso5'), Cgt=rep(c('cooling','heating'),each=15),Cgt_kwh=c(1:1))

for(d in caso1){
  for(h in hvac){
    ref_sp_uh$Cgt_kwh[ref_sp_uh$CASO == d & ref_sp_uh$Cgt == h] = round((sum(ref_sp$Cgt_kwh[ref_sp$CASO == d & ref_sp$Cgt == h])),1) 
  }
}

setwd('D:/Frentes de Trabalho/simu_NBR/novos_graphs')

write.csv(ref_sp, 'cgt_sp.csv')
write.csv(ref_sp_uh, 'cgt_sp_uh.csv')
write.csv(conf_sp, 'conf_sp.csv')
write.csv(conf_sp_uh, 'conf_sp_uh.csv')

rm(list = ls())

conf_sp_uh = read.csv('conf_sp_uh.csv')

## santa maria ----

setwd("D:/Frentes de Trabalho/simu_NBR/VNxAC_janela/BRA_RS_Santa.Maria.AB.839370_TMYx.2003-2017/")

file_list = list.files(pattern="*_1.csv") # todos os csv menos o table

library("data.table")

for (i in 1:5){ #eu sei que os 5 primeiros sao hvac
  assign(file_list[i], read.csv(file_list[i])[,c("Date.Time",
                                                 "Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.",
                                                 "DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.",
                                                 "DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.",
                                                 "SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.",
                                                 "SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.",
                                                 "DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.",
                                                 "DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.")])
  for(k in 6:10){ # eu sei que os 5 ultimos sao vn
    assign(file_list[k], read.csv(file_list[k])[,c("Date.Time",
                                                   "Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.",
                                                   "DORMITORIO1.People.Occupant.Count....TimeStep.",
                                                   "DORMITORIO2.People.Occupant.Count....TimeStep.",
                                                   "SALA1.People.Occupant.Count....TimeStep.",
                                                   "DORM2.Zone.Operative.Temperature..C..TimeStep.",
                                                   "DORM2.Zone.Mean.Air.Temperature..C..TimeStep.",
                                                   "DORM1.Zone.Operative.Temperature..C..TimeStep.",
                                                   "DORM1.Zone.Mean.Air.Temperature..C..TimeStep.",
                                                   "SALA.Zone.Operative.Temperature..C..TimeStep.",
                                                   "SALA.Zone.Mean.Air.Temperature..C..TimeStep.")])
  }
  
}

ocup_DORM1 = data.frame(ocup_DORM = vn_Caso1_sp_sta_co_1.csv$DORMITORIO1.People.Occupant.Count....TimeStep.)
ocup_DORM1$ocup = ifelse(ocup_DORM1$ocup_DORM > 0,1,0)
ocup_DORM1 = subset(ocup_DORM1, select = c("ocup"))

ocup_DORM2 = subset(ocup_DORM1, select = c("ocup"))  ## a ocupacao eh igual

ocup_SALA = data.frame(ocup_DORM = vn_Caso1_sp_sta_co_1.csv$SALA1.People.Occupant.Count....TimeStep.)
ocup_SALA$ocup = ifelse(ocup_SALA$ocup_DORM > 0,1,0)
ocup_SALA = subset(ocup_SALA, select = c("ocup"))

# ocup = rbind(ocup_dorm1,ocup_dorm2,ocup_sala)
# rm(ocup_dorm1,ocup_dorm2,ocup_sala)

# temperatura ----

c = as.numeric(26) ## limite calor
f = as.numeric(18) ## limite frio

conf_sta_DORM1 = data.frame(APP=c('DORM1'),
                            ocup=ocup_DORM1,
                            temp_ext=vn_Caso1_sp_sta_co_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.,
                            REF_temp=vn_Caso1_sp_sta_co_1.csv$DORM1.Zone.Operative.Temperature..C..TimeStep.,
                            REF_cooling=ac1_Caso1_sp_sta_co_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            REF_heating=ac1_Caso1_sp_sta_co_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                            Caso2_temp=vn_Caso2_1.csv$DORM1.Zone.Operative.Temperature..C..TimeStep.,
                            Caso2_cooling=ac1_Caso2_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            Caso2_heating=ac1_Caso2_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                            Caso3_temp=vn_Caso3_1.csv$DORM1.Zone.Operative.Temperature..C..TimeStep.,
                            Caso3_cooling=ac1_Caso3_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            Caso3_heating=ac1_Caso3_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                            Caso4_temp=vn_Caso4_1.csv$DORM1.Zone.Operative.Temperature..C..TimeStep.,
                            Caso4_cooling=ac1_Caso4_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            Caso4_heating=ac1_Caso4_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                            Caso5_temp=vn_Caso5_1.csv$DORM1.Zone.Operative.Temperature..C..TimeStep.,
                            Caso5_cooling=ac1_Caso5_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            Caso5_heating=ac1_Caso5_1.csv$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.)

conf_sta_DORM1$EXT_calor = ifelse((conf_sta_DORM1$temp_ext >= c & conf_sta_DORM1$ocup > 0),1,0)
conf_sta_DORM1$EXT_frio = ifelse((conf_sta_DORM1$temp_ext <= f) & (conf_sta_DORM1$ocup > 0),1,0)
conf_sta_DORM1$EXT_conf = ifelse(((conf_sta_DORM1$temp_ext < c) & (conf_sta_DORM1$temp_ext > f) & (conf_sta_DORM1$ocup > 0)),1,0)

conf_sta_DORM1$REF_calor = ifelse((conf_sta_DORM1$REF_temp >= c) & (conf_sta_DORM1$ocup > 0),1,0)
conf_sta_DORM1$REF_frio = ifelse((conf_sta_DORM1$REF_temp <= f) & (conf_sta_DORM1$ocup > 0),1,0)
conf_sta_DORM1$REF_conf = ifelse(((conf_sta_DORM1$REF_temp < c) & (conf_sta_DORM1$REF_temp > f) & (conf_sta_DORM1$ocup > 0)),1,0)
conf_sta_DORM1$REF_cooling = ifelse((conf_sta_DORM1$REF_calor > 0),conf_sta_DORM1$REF_cooling,0)
conf_sta_DORM1$REF_heating = ifelse((conf_sta_DORM1$REF_frio > 0),conf_sta_DORM1$REF_heating,0)

conf_sta_DORM1$Caso2_calor = ifelse((conf_sta_DORM1$Caso2_temp >= c) & (conf_sta_DORM1$ocup > 0),1,0)
conf_sta_DORM1$Caso2_frio = ifelse((conf_sta_DORM1$Caso2_temp <= f) & (conf_sta_DORM1$ocup > 0),1,0)
conf_sta_DORM1$Caso2_conf = ifelse(((conf_sta_DORM1$Caso2_temp < c) & (conf_sta_DORM1$Caso2_temp > f) & (conf_sta_DORM1$ocup > 0)),1,0)
conf_sta_DORM1$Caso2_cooling = ifelse((conf_sta_DORM1$Caso2_calor > 0),conf_sta_DORM1$Caso2_cooling,0)
conf_sta_DORM1$Caso2_heating = ifelse((conf_sta_DORM1$Caso2_frio > 0),conf_sta_DORM1$Caso2_heating,0)

conf_sta_DORM1$Caso3_calor = ifelse((conf_sta_DORM1$Caso3_temp >= c) & (conf_sta_DORM1$ocup > 0),1,0)
conf_sta_DORM1$Caso3_frio = ifelse((conf_sta_DORM1$Caso3_temp <= f) & (conf_sta_DORM1$ocup > 0),1,0)
conf_sta_DORM1$Caso3_conf = ifelse(((conf_sta_DORM1$Caso3_temp < c) & (conf_sta_DORM1$Caso3_temp > f) & (conf_sta_DORM1$ocup > 0)),1,0)
conf_sta_DORM1$Caso3_cooling = ifelse((conf_sta_DORM1$Caso3_calor > 0),conf_sta_DORM1$Caso3_cooling,0)
conf_sta_DORM1$Caso3_heating = ifelse((conf_sta_DORM1$Caso3_frio > 0),conf_sta_DORM1$Caso3_heating,0)

conf_sta_DORM1$Caso4_calor = ifelse((conf_sta_DORM1$Caso4_temp >= c) & (conf_sta_DORM1$ocup > 0),1,0)
conf_sta_DORM1$Caso4_frio = ifelse((conf_sta_DORM1$Caso4_temp <= f) & (conf_sta_DORM1$ocup > 0),1,0)
conf_sta_DORM1$Caso4_conf = ifelse(((conf_sta_DORM1$Caso4_temp < c) & (conf_sta_DORM1$Caso4_temp > f) & (conf_sta_DORM1$ocup > 0)),1,0)
conf_sta_DORM1$Caso4_cooling = ifelse((conf_sta_DORM1$Caso4_calor > 0),conf_sta_DORM1$Caso4_cooling,0)
conf_sta_DORM1$Caso4_heating = ifelse((conf_sta_DORM1$Caso4_frio > 0),conf_sta_DORM1$Caso4_heating,0)

conf_sta_DORM1$Caso5_calor = ifelse((conf_sta_DORM1$Caso5_temp >= c) & (conf_sta_DORM1$ocup > 0),1,0)
conf_sta_DORM1$Caso5_frio = ifelse((conf_sta_DORM1$Caso5_temp <= f) & (conf_sta_DORM1$ocup > 0),1,0)
conf_sta_DORM1$Caso5_conf = ifelse(((conf_sta_DORM1$Caso5_temp < c) & (conf_sta_DORM1$Caso5_temp > f) & (conf_sta_DORM1$ocup > 0)),1,0)
conf_sta_DORM1$Caso5_cooling = ifelse((conf_sta_DORM1$Caso5_calor > 0),conf_sta_DORM1$Caso5_cooling,0)
conf_sta_DORM1$Caso5_heating = ifelse((conf_sta_DORM1$Caso5_frio > 0),conf_sta_DORM1$Caso5_heating,0)

conf_sta_DORM2 = data.frame(APP=c('DORM2'),
                            ocup=ocup_DORM2,
                            temp_ext=vn_Caso1_sp_sta_co_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.,
                            REF_temp=vn_Caso1_sp_sta_co_1.csv$DORM2.Zone.Operative.Temperature..C..TimeStep.,
                            REF_cooling=ac1_Caso1_sp_sta_co_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            REF_heating=ac1_Caso1_sp_sta_co_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                            Caso2_temp=vn_Caso2_1.csv$DORM2.Zone.Operative.Temperature..C..TimeStep.,
                            Caso2_cooling=ac1_Caso2_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            Caso2_heating=ac1_Caso2_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                            Caso3_temp=vn_Caso3_1.csv$DORM2.Zone.Operative.Temperature..C..TimeStep.,
                            Caso3_cooling=ac1_Caso3_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            Caso3_heating=ac1_Caso3_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                            Caso4_temp=vn_Caso4_1.csv$DORM2.Zone.Operative.Temperature..C..TimeStep.,
                            Caso4_cooling=ac1_Caso4_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            Caso4_heating=ac1_Caso4_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                            Caso5_temp=vn_Caso5_1.csv$DORM2.Zone.Operative.Temperature..C..TimeStep.,
                            Caso5_cooling=ac1_Caso5_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                            Caso5_heating=ac1_Caso5_1.csv$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.)

conf_sta_DORM2$EXT_calor = ifelse((conf_sta_DORM2$temp_ext >= c & conf_sta_DORM2$ocup > 0),1,0)
conf_sta_DORM2$EXT_frio = ifelse((conf_sta_DORM2$temp_ext <= f) & (conf_sta_DORM2$ocup > 0),1,0)
conf_sta_DORM2$EXT_conf = ifelse(((conf_sta_DORM2$temp_ext < c) & (conf_sta_DORM2$temp_ext > f) & (conf_sta_DORM2$ocup > 0)),1,0)

conf_sta_DORM2$REF_calor = ifelse((conf_sta_DORM2$REF_temp >= c) & (conf_sta_DORM2$ocup > 0),1,0)
conf_sta_DORM2$REF_frio = ifelse((conf_sta_DORM2$REF_temp <= f) & (conf_sta_DORM2$ocup > 0),1,0)
conf_sta_DORM2$REF_conf = ifelse(((conf_sta_DORM2$REF_temp < c) & (conf_sta_DORM2$REF_temp > f) & (conf_sta_DORM2$ocup > 0)),1,0)
conf_sta_DORM2$REF_cooling = ifelse((conf_sta_DORM2$REF_calor > 0),conf_sta_DORM2$REF_cooling,0)
conf_sta_DORM2$REF_heating = ifelse((conf_sta_DORM2$REF_frio > 0),conf_sta_DORM2$REF_heating,0)

conf_sta_DORM2$Caso2_calor = ifelse((conf_sta_DORM2$Caso2_temp >= c) & (conf_sta_DORM2$ocup > 0),1,0)
conf_sta_DORM2$Caso2_frio = ifelse((conf_sta_DORM2$Caso2_temp <= f) & (conf_sta_DORM2$ocup > 0),1,0)
conf_sta_DORM2$Caso2_conf = ifelse(((conf_sta_DORM2$Caso2_temp < c) & (conf_sta_DORM2$Caso2_temp > f) & (conf_sta_DORM2$ocup > 0)),1,0)
conf_sta_DORM2$Caso2_cooling = ifelse((conf_sta_DORM2$Caso2_calor > 0),conf_sta_DORM2$Caso2_cooling,0)
conf_sta_DORM2$Caso2_heating = ifelse((conf_sta_DORM2$Caso2_frio > 0),conf_sta_DORM2$Caso2_heating,0)

conf_sta_DORM2$Caso3_calor = ifelse((conf_sta_DORM2$Caso3_temp >= c) & (conf_sta_DORM2$ocup > 0),1,0)
conf_sta_DORM2$Caso3_frio = ifelse((conf_sta_DORM2$Caso3_temp <= f) & (conf_sta_DORM2$ocup > 0),1,0)
conf_sta_DORM2$Caso3_conf = ifelse(((conf_sta_DORM2$Caso3_temp < c) & (conf_sta_DORM2$Caso3_temp > f) & (conf_sta_DORM2$ocup > 0)),1,0)
conf_sta_DORM2$Caso3_cooling = ifelse((conf_sta_DORM2$Caso3_calor > 0),conf_sta_DORM2$Caso3_cooling,0)
conf_sta_DORM2$Caso3_heating = ifelse((conf_sta_DORM2$Caso3_frio > 0),conf_sta_DORM2$Caso3_heating,0)

conf_sta_DORM2$Caso4_calor = ifelse((conf_sta_DORM2$Caso4_temp >= c) & (conf_sta_DORM2$ocup > 0),1,0)
conf_sta_DORM2$Caso4_frio = ifelse((conf_sta_DORM2$Caso4_temp <= f) & (conf_sta_DORM2$ocup > 0),1,0)
conf_sta_DORM2$Caso4_conf = ifelse(((conf_sta_DORM2$Caso4_temp < c) & (conf_sta_DORM2$Caso4_temp > f) & (conf_sta_DORM2$ocup > 0)),1,0)
conf_sta_DORM2$Caso4_cooling = ifelse((conf_sta_DORM2$Caso4_calor > 0),conf_sta_DORM2$Caso4_cooling,0)
conf_sta_DORM2$Caso4_heating = ifelse((conf_sta_DORM2$Caso4_frio > 0),conf_sta_DORM2$Caso4_heating,0)

conf_sta_DORM2$Caso5_calor = ifelse((conf_sta_DORM2$Caso5_temp >= c) & (conf_sta_DORM2$ocup > 0),1,0)
conf_sta_DORM2$Caso5_frio = ifelse((conf_sta_DORM2$Caso5_temp <= f) & (conf_sta_DORM2$ocup > 0),1,0)
conf_sta_DORM2$Caso5_conf = ifelse(((conf_sta_DORM2$Caso5_temp < c) & (conf_sta_DORM2$Caso5_temp > f) & (conf_sta_DORM2$ocup > 0)),1,0)
conf_sta_DORM2$Caso5_cooling = ifelse((conf_sta_DORM2$Caso5_calor > 0),conf_sta_DORM2$Caso5_cooling,0)
conf_sta_DORM2$Caso5_heating = ifelse((conf_sta_DORM2$Caso5_frio > 0),conf_sta_DORM2$Caso5_heating,0)

conf_sta_SALA = data.frame(APP=c('SALA'),
                           ocup=ocup_SALA,
                           temp_ext=vn_Caso1_sp_sta_co_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.,
                           REF_temp=vn_Caso1_sp_sta_co_1.csv$SALA.Zone.Operative.Temperature..C..TimeStep.,
                           REF_cooling=ac1_Caso1_sp_sta_co_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                           REF_heating=ac1_Caso1_sp_sta_co_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                           Caso2_temp=vn_Caso2_1.csv$SALA.Zone.Operative.Temperature..C..TimeStep.,
                           Caso2_cooling=ac1_Caso2_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                           Caso2_heating=ac1_Caso2_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                           Caso3_temp=vn_Caso3_1.csv$SALA.Zone.Operative.Temperature..C..TimeStep.,
                           Caso3_cooling=ac1_Caso3_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                           Caso3_heating=ac1_Caso3_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                           Caso4_temp=vn_Caso4_1.csv$SALA.Zone.Operative.Temperature..C..TimeStep.,
                           Caso4_cooling=ac1_Caso4_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                           Caso4_heating=ac1_Caso4_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.,
                           Caso5_temp=vn_Caso5_1.csv$SALA.Zone.Operative.Temperature..C..TimeStep.,
                           Caso5_cooling=ac1_Caso5_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..TimeStep.,
                           Caso5_heating=ac1_Caso5_1.csv$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..TimeStep.)

conf_sta_SALA$EXT_calor = ifelse((conf_sta_SALA$temp_ext >= c & conf_sta_SALA$ocup > 0),1,0)
conf_sta_SALA$EXT_frio = ifelse((conf_sta_SALA$temp_ext <= f) & (conf_sta_SALA$ocup > 0),1,0)
conf_sta_SALA$EXT_conf = ifelse(((conf_sta_SALA$temp_ext < c) & (conf_sta_SALA$temp_ext > f) & (conf_sta_SALA$ocup > 0)),1,0)

conf_sta_SALA$REF_calor = ifelse((conf_sta_SALA$REF_temp >= c) & (conf_sta_SALA$ocup > 0),1,0)
conf_sta_SALA$REF_frio = ifelse((conf_sta_SALA$REF_temp <= f) & (conf_sta_SALA$ocup > 0),1,0)
conf_sta_SALA$REF_conf = ifelse(((conf_sta_SALA$REF_temp < c) & (conf_sta_SALA$REF_temp > f) & (conf_sta_SALA$ocup > 0)),1,0)
conf_sta_SALA$REF_cooling = ifelse((conf_sta_SALA$REF_calor > 0),conf_sta_SALA$REF_cooling,0)
conf_sta_SALA$REF_heating = ifelse((conf_sta_SALA$REF_frio > 0),conf_sta_SALA$REF_heating,0)

conf_sta_SALA$Caso2_calor = ifelse((conf_sta_SALA$Caso2_temp >= c) & (conf_sta_SALA$ocup > 0),1,0)
conf_sta_SALA$Caso2_frio = ifelse((conf_sta_SALA$Caso2_temp <= f) & (conf_sta_SALA$ocup > 0),1,0)
conf_sta_SALA$Caso2_conf = ifelse(((conf_sta_SALA$Caso2_temp < c) & (conf_sta_SALA$Caso2_temp > f) & (conf_sta_SALA$ocup > 0)),1,0)
conf_sta_SALA$Caso2_cooling = ifelse((conf_sta_SALA$Caso2_calor > 0),conf_sta_SALA$Caso2_cooling,0)
conf_sta_SALA$Caso2_heating = ifelse((conf_sta_SALA$Caso2_frio > 0),conf_sta_SALA$Caso2_heating,0)

conf_sta_SALA$Caso3_calor = ifelse((conf_sta_SALA$Caso3_temp >= c) & (conf_sta_SALA$ocup > 0),1,0)
conf_sta_SALA$Caso3_frio = ifelse((conf_sta_SALA$Caso3_temp <= f) & (conf_sta_SALA$ocup > 0),1,0)
conf_sta_SALA$Caso3_conf = ifelse(((conf_sta_SALA$Caso3_temp < c) & (conf_sta_SALA$Caso3_temp > f) & (conf_sta_SALA$ocup > 0)),1,0)
conf_sta_SALA$Caso3_cooling = ifelse((conf_sta_SALA$Caso3_calor > 0),conf_sta_SALA$Caso3_cooling,0)
conf_sta_SALA$Caso3_heating = ifelse((conf_sta_SALA$Caso3_frio > 0),conf_sta_SALA$Caso3_heating,0)

conf_sta_SALA$Caso4_calor = ifelse((conf_sta_SALA$Caso4_temp >= c) & (conf_sta_SALA$ocup > 0),1,0)
conf_sta_SALA$Caso4_frio = ifelse((conf_sta_SALA$Caso4_temp <= f) & (conf_sta_SALA$ocup > 0),1,0)
conf_sta_SALA$Caso4_conf = ifelse(((conf_sta_SALA$Caso4_temp < c) & (conf_sta_SALA$Caso4_temp > f) & (conf_sta_SALA$ocup > 0)),1,0)
conf_sta_SALA$Caso4_cooling = ifelse((conf_sta_SALA$Caso4_calor > 0),conf_sta_SALA$Caso4_cooling,0)
conf_sta_SALA$Caso4_heating = ifelse((conf_sta_SALA$Caso4_frio > 0),conf_sta_SALA$Caso4_heating,0)

conf_sta_SALA$Caso5_calor = ifelse((conf_sta_SALA$Caso5_temp >= c) & (conf_sta_SALA$ocup > 0),1,0)
conf_sta_SALA$Caso5_frio = ifelse((conf_sta_SALA$Caso5_temp <= f) & (conf_sta_SALA$ocup > 0),1,0)
conf_sta_SALA$Caso5_conf = ifelse(((conf_sta_SALA$Caso5_temp < c) & (conf_sta_SALA$Caso5_temp > f) & (conf_sta_SALA$ocup > 0)),1,0)
conf_sta_SALA$Caso5_cooling = ifelse((conf_sta_SALA$Caso5_calor > 0),conf_sta_SALA$Caso5_cooling,0)
conf_sta_SALA$Caso5_heating = ifelse((conf_sta_SALA$Caso5_frio > 0),conf_sta_SALA$Caso5_heating,0)

caso = c('EXT','REF','Caso2','Caso3','Caso4','Caso5')
caso1 = c('REF','Caso2','Caso3','Caso4','Caso5')
app = c('DORM1','DORM2','SALA')
tipo = c('frio','calor','conf')
hvac = c('cooling','heating')

conf_sta = data.frame(CASO = rep(c('EXT','REF','Caso2','Caso3','Caso4','Caso5'),each=3), APP = c('DORM1','DORM2','SALA'), PHFT=c(1:1),Tipo=rep(c('frio','calor','conf'),each=18))

df = rbind(conf_sta_DORM1,conf_sta_DORM2,conf_sta_SALA)

for(d in caso){
  for(a in app){
    for(t in tipo){
      conf_sta$PHFT[conf_sta$APP == a & conf_sta$CASO == d & conf_sta$Tipo == t] = round(((sum(df[,grepl(paste0(d,'_',t),colnames(df))][df$APP == a])/sum(df$ocup[df$APP == a]))*100),1)
    }
  }
}

ref_sta = data.frame(CASO = rep(c('REF','Caso2','Caso3','Caso4','Caso5'),each=3), APP = c('DORM1','DORM2','SALA'), Cgt=rep(c('cooling','heating'),each=15), Cgt_kwh=c(1:1))

for(d in caso1){
  for(a in app){
    for(h in hvac){
      ref_sta$Cgt_kwh[ref_sta$APP == a & ref_sta$CASO == d & ref_sta$Cgt == h] = round((sum(df[,grepl(paste0(d,'_',h),colnames(df))][df$APP == a])/3600000),1)
    }
  }
}

conf_sta_uh = data.frame(CASO = c('EXT','REF','Caso2','Caso3','Caso4','Caso5'), PHFT=c(1:1), Tipo=rep(c('frio','calor','conf'),each=6))
for(d in caso){
  for(t in tipo){
    conf_sta_uh$PHFT[conf_sta_uh$CASO == d & conf_sta_uh$Tipo == t] = round((sum(conf_sta$PHFT[conf_sta$CASO == d & conf_sta$Tipo == t])/3),1)
  }
}

ref_sta_uh = data.frame(CASO = c('REF','Caso2','Caso3','Caso4','Caso5'), Cgt=rep(c('cooling','heating'),each=15),Cgt_kwh=c(1:1))

for(d in caso1){
  for(h in hvac){
    ref_sta_uh$Cgt_kwh[ref_sta_uh$CASO == d & ref_sta_uh$Cgt == h] = round((sum(ref_sta$Cgt_kwh[ref_sta$CASO == d & ref_sta$Cgt == h])),1) 
  }
}

setwd('D:/Frentes de Trabalho/simu_NBR/novos_graphs')

write.csv(ref_sta, 'cgt_sta.csv')
write.csv(ref_sta_uh, 'cgt_sta_uh.csv')
write.csv(conf_sta, 'conf_sta.csv')
write.csv(conf_sta_uh, 'conf_sta_uh.csv')

rm(list = ls())

## GRAFICOSS  ----

# chamando os arquivos de phft

setwd('D:/Frentes de Trabalho/simu_NBR/novos_graphs')

conf_mac_uh = read.csv('conf_mac_uh.csv')
conf_rj_uh = read.csv('conf_rj_uh.csv')
conf_sp_uh = read.csv('conf_sp_uh.csv')
conf_sta_uh = read.csv('conf_sta_uh.csv')
conf_co_uh = read.csv('conf_co_uh.csv')

conf_mac_uh$cidade = 'Macapá'
conf_rj_uh$cidade = 'Rio de Janeiro'
conf_sp_uh$cidade = 'São Paulo'
conf_sta_uh$cidade = 'Santa Maria'
conf_co_uh$cidade = 'Curitiba'

df = rbind(conf_mac_uh, conf_rj_uh, conf_sp_uh, conf_sta_uh, conf_co_uh)
df$Tipo1[df$Tipo == "frio"] = 'Limite Inferior'
df$Tipo1[df$Tipo == "calor"] = 'Limite Superior'
df$Tipo1[df$Tipo == "conf"] = 'PHFT'

df$caso_nome[df$CASO == "EXT"] = 'T. EXT.'
df$caso_nome[df$CASO == "REF"] = 'REF.'
df$caso_nome[df$CASO == "Caso2"] = '1'
df$caso_nome[df$CASO == "Caso3"] = '2'
df$caso_nome[df$CASO == "Caso4"] = '3'
df$caso_nome[df$CASO == "Caso5"] = '4'

df$caso_nome = factor(df$caso_nome, levels=c("T. EXT.","REF.","1","2","3","4"))
df$Tipo1 = factor(df$Tipo1, levels=c("Limite Superior", "Limite Inferior", "PHFT"))
# df$CASO = factor(df$CASO, levels=c("R","Caso 1","Caso 2","Caso 3","Caso 4"))


library(ggplot2)

# png(filename = 'PHFT_casos_r01.png',                                                             #MUDAR - Nome da figura 
#     width = 32, height = 20, units = "cm", res = 500)                                                         #Dimensoes da figura
# plot(ggplot(data=df, aes(x=caso_nome,y=PHFT, fill=Tipo1))+                                            #MUDAR - Nome do dataframe e variaveis
#        geom_bar(stat="identity")+                                                                             #Grafico de barras
#        geom_text(aes(label=(round(PHFT, digits=0)),                                                          #Plota valores do PHOCT 
#                      alpha=Tipo1),                                                                         #Transparencia do texto conforme variavel 
#                  color="black",                                                                               #Cor do texto
#                  size=3.5,                                                                                    #Tamanho do texto
#                  position = position_stack(vjust = 0.5))+                                                     #Texto no meio da barra
#        coord_cartesian(ylim=(0:100))+                                                                         #Limites do eixo y
#        labs(x=NULL,                                                    #MUDAR - Titulo do eixo x
#             y="Percentual de Horas Ocupadas na Faixa de Temperatura Operativa (PHFT)",                                                                         #MUDAR - Titulo do eixo y
#             title="Unidade habitacional"                                                                 #MUDAR - Titulo do grafico
#             # subtitle="Ventilação variável"
#             )+                                                              #MUDAR - Subtitulo do grafico
#        scale_alpha_manual(values = c(0, 0, 1))+                                                               #Transparencia do texto - apenas PHOCT nao eh transparente
#        scale_fill_manual("",                                                                                  #Titulo da legenda de cores - nao tem 
#                          values=c("#FFA05C",                                                                  #Cor para Calor - vermelho
#                                   "#A8DDFF",                                                             #Cor para Frio - azul
#                                   "#96D5C0"),                                                                 #Cor para PHOCT - verde
#                          labels=c("Calor", "Frio", "PHFT"))+                                                 #Nome dos itens da legenda                              
#        # scale_x_discrete(labels=c("1,0","1,2", "1,5","1,8","2,0","2,2", "2,5", "2,8","3,0","3,2","3,5"))+  #MUDAR - texto dos itens do eixo x
#        # scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+                  #Adiciona divisao de milhar
#        guides(alpha=FALSE)+                                                                                   #Remove legenda para transparencias
#        theme(legend.position= "right",                                                                        #Posicao da legenda
#              legend.text = element_text(size=19),                                                             #Tamanho do texto da legenda
#              plot.title = element_text(size=19),                                                              #Tamanho do titulo do grafico
#              plot.subtitle = element_text(size=15),                                                           #Tamanho do subtitulo do grafico
#              # axis.title.x = element_text(size=19),                                                            #Tamanho do titulo do eixo x
#              axis.title.y = element_text(size=15),                                                            #Tamanho do titulo do eixo y
#              # axis.text.x = element_text(size=15, angle=90, vjust = 0.5),                                      #Tamanho do texto do eixo x, rotacionado 90 graus, alinhado com o centro da barra
#              axis.text.y = element_text(size=15),                                                             #Tamanho do texto do eixo y
#              strip.text.x = element_text(size = 19),                                                          #Tamanho do texto do facet grid no eixo x
#              strip.text.y = element_text(size = 15))+                                                         #Tamanho do texto do facet grid no eixo y
#        facet_grid(cidade~.)                                                      #Corrige nomes das divisoes do grafico - tem que rodar o Labels_tipo
# )
# dev.off()

png(filename = 'PHFT_casos_r04.png',                                                             #MUDAR - Nome da figura 
    width = 33.8, height = 19, units = "cm", res = 500)                                                         #Dimensoes da figura
plot(ggplot(data=df, aes(x=caso_nome,y=PHFT, fill=Tipo1))+                                            #MUDAR - Nome do dataframe e variaveis
       geom_bar(stat="identity")+                                                                             #Grafico de barras
       geom_text(aes(label=(round(PHFT, digits=0)),                                                          #Plota valores do PHOCT 
                     alpha=Tipo1),                                                                         #Transparencia do texto conforme variavel 
                 color="black",                                                                               #Cor do texto
                 size=3.5,                                                                                    #Tamanho do texto
                 position = position_stack(vjust = 0.5))+                                                     #Texto no meio da barra
       coord_cartesian(ylim=(0:100))+                                                                         #Limites do eixo y
       labs(x=NULL,                                                    #MUDAR - Titulo do eixo x
            y="Percentual de Horas Ocupadas na Faixa\n de Temperatura Operativa (PHFT) - (%)",                                                                         #MUDAR - Titulo do eixo y
            title="Unidade habitacional"                                                                 #MUDAR - Titulo do grafico
            # subtitle="Ventilação variável"
       )+                                                              #MUDAR - Subtitulo do grafico
       scale_alpha_manual(values = c(0, 0, 1))+                                                               #Transparencia do texto - apenas PHOCT nao eh transparente
       scale_fill_manual("",                                                                                  #Titulo da legenda de cores - nao tem 
                         values=c("#FFA05C",                                                                  #Cor para Calor - vermelho
                                  "#A8DDFF",                                                             #Cor para Frio - azul
                                  "#96D5C0"))+                                                 #Nome dos itens da legenda                              
       # scale_x_discrete(labels=c("1,0","1,2", "1,5","1,8","2,0","2,2", "2,5", "2,8","3,0","3,2","3,5"))+  #MUDAR - texto dos itens do eixo x
       # scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+                  #Adiciona divisao de milhar
       guides(alpha=FALSE)+                                                                                   #Remove legenda para transparencias
       theme(legend.position= "bottom",                                                                        #Posicao da legenda
             legend.text = element_text(size=15),                                                             #Tamanho do texto da legenda
             plot.title = element_text(size=19),                                                              #Tamanho do titulo do grafico
             # plot.subtitle = element_text(size=15),                                                           #Tamanho do subtitulo do grafico
             # axis.title.x = element_text(size=19),                                                            #Tamanho do titulo do eixo x
             axis.title.y = element_text(size=15),                                                            #Tamanho do titulo do eixo y
             axis.text.x = element_text(size=15, angle=90, vjust = 0.5),                                      #Tamanho do texto do eixo x, rotacionado 90 graus, alinhado com o centro da barra
             axis.text.y = element_text(size=15),                                                             #Tamanho do texto do eixo y
             strip.text.x = element_text(size = 19),                                                          #Tamanho do texto do facet grid no eixo x
             strip.text.y = element_text(size = 15))+                                                         #Tamanho do texto do facet grid no eixo y
       facet_grid(.~cidade)                                                      #Corrige nomes das divisoes do grafico - tem que rodar o Labels_tipo
)
dev.off()

# df$CASO1 = 'CASO'

# png(filename = 'PHFT_casos_r03.png',                                                             #MUDAR - Nome da figura 
#     width = 32, height = 20, units = "cm", res = 500)                                                         #Dimensoes da figura
# plot(ggplot(data=df, aes(x=CASO1,y=PHFT, fill=Tipo1))+                                            #MUDAR - Nome do dataframe e variaveis
#        geom_bar(stat="identity")+                                                                             #Grafico de barras
#        geom_text(aes(label=(round(PHFT, digits=0)),                                                          #Plota valores do PHOCT 
#                      alpha=Tipo1),                                                                         #Transparencia do texto conforme variavel 
#                  color="black",                                                                               #Cor do texto
#                  size=3.5,                                                                                    #Tamanho do texto
#                  position = position_stack(vjust = 0.5))+                                                     #Texto no meio da barra
#        coord_cartesian(ylim=(0:100))+                                                                         #Limites do eixo y
#        labs(x=NULL,                                                    #MUDAR - Titulo do eixo x
#             y="Percentual de Horas Ocupadas na Faixa de Temperatura Operativa (PHFT)",                                                                         #MUDAR - Titulo do eixo y
#             title="Unidade Habitacional Unifamiliar"                                                                 #MUDAR - Titulo do grafico
#             # subtitle="Ventilação variável"
#        )+                                                              #MUDAR - Subtitulo do grafico
#        scale_alpha_manual(values = c(0, 0, 1))+                                                               #Transparencia do texto - apenas PHOCT nao eh transparente
#        scale_fill_manual("",                                                                                  #Titulo da legenda de cores - nao tem 
#                          values=c("#FFA05C",                                                                  #Cor para Calor - vermelho
#                                   "#A8DDFF",                                                             #Cor para Frio - azul
#                                   "#96D5C0"),                                                                 #Cor para PHOCT - verde
#                          labels=c("Calor", "Frio", "PHFT"))+                                                 #Nome dos itens da legenda                              
#        # scale_x_discrete(labels=c("1,0","1,2", "1,5","1,8","2,0","2,2", "2,5", "2,8","3,0","3,2","3,5"))+  #MUDAR - texto dos itens do eixo x
#        # scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+                  #Adiciona divisao de milhar
#        guides(alpha=FALSE)+                                                                                   #Remove legenda para transparencias
#        theme(legend.position= "right",                                                                        #Posicao da legenda
#              legend.text = element_text(size=19),                                                             #Tamanho do texto da legenda
#              plot.title = element_text(size=19),                                                              #Tamanho do titulo do grafico
#              plot.subtitle = element_text(size=15),                                                           #Tamanho do subtitulo do grafico
#              axis.title.x = element_blank(),                                                            #Tamanho do titulo do eixo x
#              axis.title.y = element_text(size=15),                                                            #Tamanho do titulo do eixo y
#              axis.text.x = element_blank(),                                      #Tamanho do texto do eixo x, rotacionado 90 graus, alinhado com o centro da barra
#              axis.text.y = element_text(size=15),                                                             #Tamanho do texto do eixo y
#              strip.text.x = element_text(size = 19),                                                          #Tamanho do texto do facet grid no eixo x
#              strip.text.y = element_text(size = 15))+                                                         #Tamanho do texto do facet grid no eixo y
#        facet_grid(cidade~caso_nome)                                                      #Corrige nomes das divisoes do grafico - tem que rodar o Labels_tipo
# )
# dev.off()

## chamando os arquivos de carga

cgt_mac_uh = read.csv('cgt_mac_uh.csv')
cgt_rj_uh = read.csv('cgt_rj_uh.csv')
cgt_sp_uh = read.csv('cgt_sp_uh.csv')
cgt_sta_uh = read.csv('cgt_sta_uh.csv')
cgt_co_uh = read.csv('cgt_co_uh.csv')

cgt_mac_uh$cidade = 'Macapá'
cgt_rj_uh$cidade = 'Rio de Janeiro'
cgt_sp_uh$cidade = 'São Paulo'
cgt_sta_uh$cidade = 'Santa Maria'
cgt_co_uh$cidade = 'Curitiba'

df = rbind(cgt_mac_uh, cgt_rj_uh, cgt_sp_uh, cgt_sta_uh, cgt_co_uh)

df$caso_nome[df$CASO == "REF"] = 'REF.'
df$caso_nome[df$CASO == "Caso2"] = '1'
df$caso_nome[df$CASO == "Caso3"] = '2'
df$caso_nome[df$CASO == "Caso4"] = '3'
df$caso_nome[df$CASO == "Caso5"] = '4'

df$Tipo1[df$Cgt == "cooling"] = "Refrigeração"
df$Tipo1[df$Cgt == "heating"] = "Aquecimento"

df$caso_nome = factor(df$caso_nome, levels=c("REF.","1","2","3","4"))
df$Tipo1 = factor(df$Tipo1, levels=c("Aquecimento","Refrigeração"))

# df$CASO1 = 'CASO'

# png(filename = 'cgt_casos_r03.png',                                                             #MUDAR - Nome da figura 
#     width = 32, height = 20, units = "cm", res = 500)                                                         #Dimensoes da figura
# plot(ggplot(data=df, aes(x=CASO1,y=Refrig))+                                            #MUDAR - Nome do dataframe e variaveis
#        geom_bar(stat="identity",fill="steelblue")+                                                                             #Grafico de barras
#        # geom_text(aes(label=(round(PHFT, digits=0)),                                                          #Plota valores do PHOCT 
#        #               alpha=Tipo1),                                                                         #Transparencia do texto conforme variavel 
#        #           color="black",                                                                               #Cor do texto
#        #           size=3.5,                                                                                    #Tamanho do texto
#        #           position = position_stack(vjust = 0.5))+                                                     #Texto no meio da barra
#        # coord_cartesian(ylim=(0:100))+                                                                         #Limites do eixo y
#        labs(x=NULL,                                                    #MUDAR - Titulo do eixo x
#             y="Carga Térmica de Refrigeração (kWh/ano)",                                                                         #MUDAR - Titulo do eixo y
#             title="Unidade Habitacional Unifamiliar"                                                                 #MUDAR - Titulo do grafico
#             # subtitle="Ventilação variável"
#        )+                                                              #MUDAR - Subtitulo do grafico
#        scale_alpha_manual(values = c(0, 0, 1))+                                                               #Transparencia do texto - apenas PHOCT nao eh transparente
#        # scale_color_manual("",                                                                                  #Titulo da legenda de cores - nao tem 
#        #                    values=c("#A8DDFF"))+                                                                  #Cor para Calor - vermelho
#        #                            "#A8DDFF",                                                             #Cor para Frio - azul
#        #                            "#96D5C0"),                                                                 #Cor para PHOCT - verde
#        #                   labels=c("Calor", "Frio", "PHFT"))+                                                 #Nome dos itens da legenda                              
#        # scale_x_discrete(labels=c("1,0","1,2", "1,5","1,8","2,0","2,2", "2,5", "2,8","3,0","3,2","3,5"))+  #MUDAR - texto dos itens do eixo x
#        # scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+                  #Adiciona divisao de milhar
#        guides(alpha=FALSE)+                                                                                   #Remove legenda para transparencias
#        theme(legend.position= "right",                                                                        #Posicao da legenda
#              legend.text = element_text(size=19),                                                             #Tamanho do texto da legenda
#              plot.title = element_text(size=19),                                                              #Tamanho do titulo do grafico
#              plot.subtitle = element_text(size=15),                                                           #Tamanho do subtitulo do grafico
#              axis.title.x = element_blank(),                                                            #Tamanho do titulo do eixo x
#              axis.title.y = element_text(size=15),                                                            #Tamanho do titulo do eixo y
#              axis.text.x = element_blank(),                                      #Tamanho do texto do eixo x, rotacionado 90 graus, alinhado com o centro da barra
#              axis.text.y = element_text(size=15),                                                             #Tamanho do texto do eixo y
#              strip.text.x = element_text(size = 19),                                                          #Tamanho do texto do facet grid no eixo x
#              strip.text.y = element_text(size = 15))+                                                         #Tamanho do texto do facet grid no eixo y
#        facet_grid(cidade~caso_nome)                                                      #Corrige nomes das divisoes do grafico - tem que rodar o Labels_tipo
# )
# dev.off()

# png(filename = 'Cgt_casos_r04.png',                                                             #MUDAR - Nome da figura 
#     width = 32, height = 20, units = "cm", res = 500)                                                         #Dimensoes da figura
# plot(ggplot(data=df, aes(x=caso_nome,y=Cgt_kwh, fill=Tipo1))+                                            #MUDAR - Nome do dataframe e variaveis
#        geom_bar(stat="identity")+                                                                             #Grafico de barras
#        # geom_text(aes(label=(round(PHFT, digits=0)),                                                          #Plota valores do PHOCT 
#        #               alpha=Tipo1),                                                                         #Transparencia do texto conforme variavel 
#        #           color="black",                                                                               #Cor do texto
#        #           size=3.5,                                                                                    #Tamanho do texto
#        #           position = position_stack(vjust = 0.5))+                                                     #Texto no meio da barra
#        coord_cartesian(ylim=(0:100))+                                                                         #Limites do eixo y
#        labs(x=NULL,                                                    #MUDAR - Titulo do eixo x
#             y="Carga Térmica de Refrigeração (kWh/ano)",                                                                         #MUDAR - Titulo do eixo y
#             title="Unidade habitacional"                                                                 #MUDAR - Titulo do grafico
#             # subtitle="Ventilação variável"
#        )+                                                              #MUDAR - Subtitulo do grafico
#        scale_alpha_manual(values = c(0, 0, 1))+                                                               #Transparencia do texto - apenas PHOCT nao eh transparente
#        scale_fill_manual("",                                                                                  #Titulo da legenda de cores - nao tem 
#                          values=c("#FFA05C",                                                                  #Cor para Calor - vermelho
#                                   "#A8DDFF",                                                             #Cor para Frio - azul
#                                   "#96D5C0"),                                                                 #Cor para PHOCT - verde
#                          labels=c("Limite Sup.", "Limite Inf.", "PHFT"))+                                                 #Nome dos itens da legenda                              
#        # scale_x_discrete(labels=c("1,0","1,2", "1,5","1,8","2,0","2,2", "2,5", "2,8","3,0","3,2","3,5"))+  #MUDAR - texto dos itens do eixo x
#        # scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+                  #Adiciona divisao de milhar
#        guides(alpha=FALSE)+                                                                                   #Remove legenda para transparencias
#        theme(legend.position= "right",                                                                        #Posicao da legenda
#              legend.text = element_text(size=19),                                                             #Tamanho do texto da legenda
#              plot.title = element_text(size=19),                                                              #Tamanho do titulo do grafico
#              plot.subtitle = element_text(size=15),                                                           #Tamanho do subtitulo do grafico
#              # axis.title.x = element_text(size=19),                                                            #Tamanho do titulo do eixo x
#              axis.title.y = element_text(size=15),                                                            #Tamanho do titulo do eixo y
#              axis.text.x = element_text(size=15, angle=90, vjust = 0.5),                                      #Tamanho do texto do eixo x, rotacionado 90 graus, alinhado com o centro da barra
#              axis.text.y = element_text(size=15),                                                             #Tamanho do texto do eixo y
#              strip.text.x = element_text(size = 19),                                                          #Tamanho do texto do facet grid no eixo x
#              strip.text.y = element_text(size = 15))+                                                         #Tamanho do texto do facet grid no eixo y
#        facet_grid(.~cidade)                                                      #Corrige nomes das divisoes do grafico - tem que rodar o Labels_tipo
# )
# dev.off()
# 
# 
# png(filename = 'cgt_casos_r04.png',                                                             #MUDAR - Nome da figura 
#     width = 32, height = 20, units = "cm", res = 500)                                                         #Dimensoes da figura
# plot(ggplot(data=df, aes(x=caso_nome,y=Cgt_kw, fill=Cgt))+                                            #MUDAR - Nome do dataframe e variaveis
#        geom_bar(stat="identity")+                                                                             #Grafico de barras
#        geom_text(aes(label=(round(PHFT, digits=0)),                                                          #Plota valores do PHOCT 
#                      alpha=Tipo1),                                                                         #Transparencia do texto conforme variavel 
#                  color="black",                                                                               #Cor do texto
#                  size=3.5,                                                                                    #Tamanho do texto
#                  position = position_stack(vjust = 0.5))+                                                     #Texto no meio da barra
#        coord_cartesian(ylim=(0:100))+                                                                         #Limites do eixo y
#        labs(x=NULL,                                                    #MUDAR - Titulo do eixo x
#             y="Carga Térmica Anual na UH (kWh)",                                                                         #MUDAR - Titulo do eixo y
#             title="Unidade habitacional"                                                                 #MUDAR - Titulo do grafico
#             # subtitle="Ventilação variável"
#        )+                                                              #MUDAR - Subtitulo do grafico
#        scale_alpha_manual(values = c(0, 0, 1))+                                                               #Transparencia do texto - apenas PHOCT nao eh transparente
#        scale_fill_manual("",                                                                                  #Titulo da legenda de cores - nao tem 
#                          values=c("#FFA05C",                                                                  #Cor para Calor - vermelho
#                                   "#A8DDFF"))+                                                                 #Cor para PHOCT - verde
#        # scale_x_discrete(labels=c("1,0","1,2", "1,5","1,8","2,0","2,2", "2,5", "2,8","3,0","3,2","3,5"))+  #MUDAR - texto dos itens do eixo x
#        # scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+                  #Adiciona divisao de milhar
#        guides(alpha=FALSE)+                                                                                   #Remove legenda para transparencias
#        theme(legend.position= "right",                                                                        #Posicao da legenda
#              legend.text = element_text(size=19),                                                             #Tamanho do texto da legenda
#              plot.title = element_text(size=19),                                                              #Tamanho do titulo do grafico
#              plot.subtitle = element_text(size=15),                                                           #Tamanho do subtitulo do grafico
#              # axis.title.x = element_text(size=19),                                                            #Tamanho do titulo do eixo x
#              axis.title.y = element_text(size=15),                                                            #Tamanho do titulo do eixo y
#              axis.text.x = element_text(size=15, angle=90, vjust = 0.5),                                      #Tamanho do texto do eixo x, rotacionado 90 graus, alinhado com o centro da barra
#              axis.text.y = element_text(size=15),                                                             #Tamanho do texto do eixo y
#              strip.text.x = element_text(size = 19),                                                          #Tamanho do texto do facet grid no eixo x
#              strip.text.y = element_text(size = 15))+                                                         #Tamanho do texto do facet grid no eixo y
#        facet_grid(.~cidade)                                                      #Corrige nomes das divisoes do grafico - tem que rodar o Labels_tipo
# )
# dev.off()

png(filename = 'Cgt_casos_r04.png',                                                             #MUDAR - Nome da figura 
    width = 33.8, height = 19, units = "cm", res = 500)                                                         #Dimensoes da figura
plot(ggplot(data=df, aes(x=caso_nome,y=Cgt_kwh, fill=Tipo1))+                                            #MUDAR - Nome do dataframe e variaveis
       geom_bar(stat="identity")+                                                                             #Grafico de barras
       # geom_text(aes(label=(round(PHFT, digits=0)),                                                          #Plota valores do PHOCT 
       #               alpha=Tipo1),                                                                         #Transparencia do texto conforme variavel 
       #           color="black",                                                                               #Cor do texto
       #           size=3.5,                                                                                    #Tamanho do texto
       #           position = position_stack(vjust = 0.5))+                                                     #Texto no meio da barra
       # coord_cartesian(ylim=(0:100))+                                                                         #Limites do eixo y
       labs(x=NULL,                                                    #MUDAR - Titulo do eixo x
            y="Carga Térmica Anual - (kWh)",                                                                         #MUDAR - Titulo do eixo y
            title="Unidade habitacional"                                                                 #MUDAR - Titulo do grafico
            # subtitle="Ventilação variável"
       )+                                                              #MUDAR - Subtitulo do grafico
       scale_alpha_manual(values = c(0, 0, 1))+                                                               #Transparencia do texto - apenas PHOCT nao eh transparente
       scale_fill_manual("",                                                                                  #Titulo da legenda de cores - nao tem 
                         values=c("#FF3333",                                                                  #Cor para Calor - vermelho
                                  "#6699FF"))+                                                 #Nome dos itens da legenda                              
       # scale_x_discrete(labels=c("1,0","1,2", "1,5","1,8","2,0","2,2", "2,5", "2,8","3,0","3,2","3,5"))+  #MUDAR - texto dos itens do eixo x
       # scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+                  #Adiciona divisao de milhar
       guides(alpha=FALSE)+                                                                                   #Remove legenda para transparencias
       theme(legend.position= "bottom",                                                                        #Posicao da legenda
             legend.text = element_text(size=15),                                                             #Tamanho do texto da legenda
             plot.title = element_text(size=19),                                                              #Tamanho do titulo do grafico
             # plot.subtitle = element_text(size=15),                                                           #Tamanho do subtitulo do grafico
             # axis.title.x = element_text(size=19),                                                            #Tamanho do titulo do eixo x
             axis.title.y = element_text(size=15),                                                            #Tamanho do titulo do eixo y
             axis.text.x = element_text(size=15, angle=90, vjust = 0.5),                                      #Tamanho do texto do eixo x, rotacionado 90 graus, alinhado com o centro da barra
             axis.text.y = element_text(size=15),                                                             #Tamanho do texto do eixo y
             strip.text.x = element_text(size = 19),                                                          #Tamanho do texto do facet grid no eixo x
             strip.text.y = element_text(size = 15))+                                                         #Tamanho do texto do facet grid no eixo y
       facet_grid(.~cidade)                                                      #Corrige nomes das divisoes do grafico - tem que rodar o Labels_tipo
)
dev.off()