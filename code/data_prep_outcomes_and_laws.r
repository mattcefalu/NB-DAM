data_prep = function(file,law.names){
  
  # coding of laws
  d1 = read.csv(file="data/CAPSYGSI_laws_12072021_exp2019.csv")
  d2 = read.csv(file="data/BCMAWP_laws_12132021_exp2019.csv")
  d3 = read.csv(file="data/PC_laws_12072021.csv")
  
  laws=merge(d1,d2,by=c("State","Year"))
  laws=merge(laws,d3 , by=c("State","Year"))
  rm(d1,d2,d3)
  
  # lagged coding of laws
  d1 = read.csv(file="data/BCMAWP_laws_12132021_lagged.csv")
  d2 = read.csv(file="data/RTCCAPSYG_laws_12072021_lagged.csv")
  d3 = read.csv(file="data/PC_laws_12072021_lagged.csv")
  
  # drop unused column
  d1 = d1[,!colnames(d1) %in% c("levels.coding.slow.BCds","levels.coding.instant.BCds")]
  
  lagged.laws=merge(d1,d2,by=c("State","Year"))
  lagged.laws=merge(lagged.laws,d3 , by=c("State","Year"))
  rm(d1,d2,d3)
  
  # recode AZ for WP
  setDT(laws)
  setkey(laws , State , Year)
  
  laws[ State=="Arizona" & Year== 1994 , levels.coding.instant.WP7d := 9/12]
  laws[ State=="Arizona" & Year== 1994 , levels.coding.slow.WP7d := 113/120/12]
  laws[ State=="Arizona" & Year== 1995 , levels.coding.slow.WP7d := 49/120/12]
  
  laws[ , (paste0("levels.coding.slow.",law.names,"_1")) := lapply(.SD,data.table::shift , n = 1 , type = "lag") , by=.(State) , .SDcols=paste0("levels.coding.slow.",law.names)]
  laws[ , (paste0("ch.levels.coding.slow.",law.names,"_1")) := lapply(.SD,data.table::shift , n = 1 , type = "lag") , by=.(State) , .SDcols=paste0("ch.levels.coding.slow.",law.names)]
  
  laws[ , (paste0("levels.coding.slow.",law.names,"_2")) := lapply(.SD,data.table::shift , n = 2 , type = "lag") , by=.(State) , .SDcols=paste0("levels.coding.slow.",law.names)]
  laws[ , (paste0("ch.levels.coding.slow.",law.names,"_2")) := lapply(.SD,data.table::shift , n = 2 , type = "lag") , by=.(State) , .SDcols=paste0("ch.levels.coding.slow.",law.names)]
  
  laws[ , (paste0("levels.coding.instant.",law.names,"_1")) := lapply(.SD,data.table::shift , n = 1 , type = "lag") , by=.(State) , .SDcols=paste0("levels.coding.instant.",law.names)]
  laws[ , (paste0("ch.levels.coding.instant.",law.names,"_1")) := lapply(.SD,data.table::shift , n = 1 , type = "lag") , by=.(State) , .SDcols=paste0("ch.levels.coding.instant.",law.names)]
  
  laws[ , (paste0("levels.coding.instant.",law.names,"_2")) := lapply(.SD,data.table::shift , n = 2 , type = "lag") , by=.(State) , .SDcols=paste0("levels.coding.instant.",law.names)]
  laws[ , (paste0("ch.levels.coding.instant.",law.names,"_2")) := lapply(.SD,data.table::shift , n = 2 , type = "lag") , by=.(State) , .SDcols=paste0("ch.levels.coding.instant.",law.names)]
  
  setDT(lagged.laws)
  setkey(lagged.laws , State , Year)
  setnames(lagged.laws , old = paste0("levels.coding.slow.",law.names,".lag") , new=paste0("levels.coding.slow.lagged.",law.names))
  
  lagged.laws[ State=="Arizona" & Year== 1999 , levels.coding.slow.lagged.WP7d := 113/120/12]
  lagged.laws[ State=="Arizona" & Year== 2000 , levels.coding.slow.lagged.WP7d := 49/120/12]
  
  lagged.laws[ , (paste0("levels.coding.slow.lagged.",law.names,"_1")) := lapply(.SD,data.table::shift , n = 1 , type = "lag") , by=.(State) , .SDcols=paste0("levels.coding.slow.lagged.",law.names)]
  lagged.laws[ , (paste0("levels.coding.slow.lagged.",law.names,"_2")) := lapply(.SD,data.table::shift , n = 2 , type = "lag") , by=.(State) , .SDcols=paste0("levels.coding.slow.lagged.",law.names)]
  
  load(file)#read.table("data/ALLLAWS_other_02152019.csv",sep=",",h=T)
  dta = model.data.std
  rm(model.data.std)
  
  #dta = subset(dta , select = c("State","Year","Population","Total.Firearm.Deaths","Firearm.Suicides.v2",
  #                              "Firearm.Homicides.v2","Total.Suicides","Total.Homicides.v2"))
  
  setDT(dta)
  setnames(dta , old = c("Total.Firearm.Deaths","Firearm.Suicides.v2",
                         "Firearm.Homicides.v2","Total.Suicides","Total.Homicides.v2") , 
           new = c("Total.Firearm.Deaths","Firearm.Suicides",
                   "Firearm.Homicides","Total.Suicides","Total.Homicides"))
  
  ###################################
  ###  need to remove 9/11 deaths ###
  
  d911 = data.table( State = c("California","Connecticut","District of Columbia",
                               "Illinois","Maryland","Massachusetts","New Jersey","New York",
                               "Pennsylvania","Virginia") , value911 = as.integer(c(45,63,15,10,50,90,691,1769,28,104)) , Year=2001)
  
  # merge in 9/11 deaths
  setDT(dta)
  setkey(dta, State, Year)
  setkey(d911, State, Year)
  dta = d911[dta]
  
  # clean up
  rm(d911)
  
  # subtract out 9/11 deaths
  dta[ !is.na(value911) , Total.Homicides := Total.Homicides - value911]
  
  
  # merge
  dta=merge(dta,laws,by=c("State","Year"))
  dta=merge(dta,lagged.laws,by=c("State","Year"))
  
  rm(laws,lagged.laws)
  
  # lag outcomes / populations
  outcome.names = c("Total.Firearm.Deaths","Firearm.Suicides","Firearm.Homicides","Total.Suicides","Total.Homicides","Population")
  dta[ , (paste0(outcome.names,"_1")) := lapply(.SD,data.table::shift , n = 1 , type = "lag") , by=.(State) , .SDcols=outcome.names]
  dta[ , (paste0(outcome.names,"_2")) := lapply(.SD,data.table::shift , n = 2 , type = "lag") , by=.(State) , .SDcols=outcome.names]

  
  return(dta)
}