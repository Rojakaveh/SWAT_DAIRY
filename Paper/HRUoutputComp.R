##############################################################
##----------------Allandrafarm----------------------------------------
###################################################################
####-----------Standard Fert---------------------------------------------------

load("~/FarmScenarios/standardfertHRU.Rdata")
outdata_hru_stanfert=subset(outdata_hru_stanfert, outdata_hru_stanfert$LULC=="HYAF")

roja=matrix(NA, ncol = 7, nrow = 365)
roja=data.frame(roja)
colnames(roja)=c("Date","ORGN","ORGP","MINP","MINN","N_APP","P_APP")



for (i in 1:365){
  roja$Date[i]=i
  roja$ORGN[i]=sum(outdata_hru_stanfert$ORGNkg_per_ha[outdata_hru_stanfert$MON==i])
  roja$ORGP[i]=sum(outdata_hru_stanfert$ORGPkg_per_ha[outdata_hru_stanfert$MON==i])
  roja$MINP[i]=sum(outdata_hru_stanfert$SEDPkg_per_ha[outdata_hru_stanfert$MON==i])
  roja$MINN[i]=sum(outdata_hru_stanfert$NSURQkg_per_ha[outdata_hru_stanfert$MON==i])
  roja$N_APP[i]=sum(outdata_hru_stanfert$N_APPkg_per_ha[outdata_hru_stanfert$MON==i])
  roja$P_APP[i]=sum(outdata_hru_stanfert$P_APPkg_per_ha[outdata_hru_stanfert$MON==i])
}

###------------ Dairy Fert----------------------------------------------
load("~/FarmScenarios/dairyHRU.Rdata")

outdata_hru_dairy=subset(outdata_hru_dairy, outdata_hru_dairy$LULC=="HYAF")

amir=matrix(NA, ncol = 7, nrow = 365)
amir=data.frame(amir)
colnames(amir)=c("Date","ORGN","ORGP","MINP","MINN","N_APP","P_APP")



for (i in 1:365){
  amir$Date[i]=i
  amir$ORGN[i]=sum(outdata_hru_dairy$ORGNkg_per_ha[outdata_hru_dairy$MON==i])
  amir$ORGP[i]=sum(outdata_hru_dairy$ORGPkg_per_ha[outdata_hru_dairy$MON==i])
  amir$MINP[i]=sum(outdata_hru_dairy$SEDPkg_per_ha[outdata_hru_dairy$MON==i])
  amir$MINN[i]=sum(outdata_hru_dairy$NSURQkg_per_ha[outdata_hru_dairy$MON==i])
  amir$N_APP[i]=sum(outdata_hru_dairy$N_APPkg_per_ha[outdata_hru_dairy$MON==i])
  amir$P_APP[i]=sum(outdata_hru_dairy$P_APPkg_per_ha[outdata_hru_dairy$MON==i])
}
sum(roja$ORGN)
sum(amir$ORGN)
sum(roja$ORGP)
sum(amir$ORGP)
sum(roja$MINP)
sum(amir$MINP)
sum(roja$MINN)
sum(amir$MINN)
sum(roja$N_APP)
sum(amir$N_APP)
sum(roja$P_APP)
sum(amir$P_APP)
############################################################################################
#################------------------Four Hills Farm---------------------------------------
###########################################################################################

load("~/FarmScenarios/standardfertHRU.Rdata")
outdata_hru_stanfert=subset(outdata_hru_stanfert, outdata_hru_stanfert$LULC=="HYFF")

roja=matrix(NA, ncol = 7, nrow = 365)
roja=data.frame(roja)
colnames(roja)=c("Date","ORGN","ORGP","MINP","MINN","N_APP","P_APP")



for (i in 1:365){
  roja$Date[i]=i
  roja$ORGN[i]=sum(outdata_hru_stanfert$ORGNkg_per_ha[outdata_hru_stanfert$MON==i])
  roja$ORGP[i]=sum(outdata_hru_stanfert$ORGPkg_per_ha[outdata_hru_stanfert$MON==i])
  roja$MINP[i]=sum(outdata_hru_stanfert$SEDPkg_per_ha[outdata_hru_stanfert$MON==i])
  roja$MINN[i]=sum(outdata_hru_stanfert$NSURQkg_per_ha[outdata_hru_stanfert$MON==i])
  roja$N_APP[i]=sum(outdata_hru_stanfert$N_APPkg_per_ha[outdata_hru_stanfert$MON==i])
  roja$P_APP[i]=sum(outdata_hru_stanfert$P_APPkg_per_ha[outdata_hru_stanfert$MON==i])
}

###------------ Dairy Fert----------------------------------------------
load("~/FarmScenarios/dairyHRU.Rdata")

outdata_hru_dairy=subset(outdata_hru_dairy, outdata_hru_dairy$LULC=="HYFF")

amir=matrix(NA, ncol = 7, nrow = 365)
amir=data.frame(amir)
colnames(amir)=c("Date","ORGN","ORGP","MINP","MINN","N_APP","P_APP")



for (i in 1:365){
  amir$Date[i]=i
  amir$ORGN[i]=sum(outdata_hru_dairy$ORGNkg_per_ha[outdata_hru_dairy$MON==i])
  amir$ORGP[i]=sum(outdata_hru_dairy$ORGPkg_per_ha[outdata_hru_dairy$MON==i])
  amir$MINP[i]=sum(outdata_hru_dairy$SEDPkg_per_ha[outdata_hru_dairy$MON==i])
  amir$MINN[i]=sum(outdata_hru_dairy$NSURQkg_per_ha[outdata_hru_dairy$MON==i])
  amir$N_APP[i]=sum(outdata_hru_dairy$N_APPkg_per_ha[outdata_hru_dairy$MON==i])
  amir$P_APP[i]=sum(outdata_hru_dairy$P_APPkg_per_ha[outdata_hru_dairy$MON==i])
}
sum(roja$ORGN)
sum(amir$ORGN)
sum(roja$ORGP)
sum(amir$ORGP)
sum(roja$MINP)
sum(amir$MINP)
sum(roja$MINN)
sum(amir$MINN)
sum(roja$N_APP)
sum(amir$N_APP)
sum(roja$P_APP)
sum(amir$P_APP)
#########################################################################################
#####----------------------Monkton Rd Farm
##########################################################################################
load("~/FarmScenarios/standardfertHRU.Rdata")
outdata_hru_stanfert=subset(outdata_hru_stanfert, outdata_hru_stanfert$LULC=="HYMF")

roja=matrix(NA, ncol = 7, nrow = 365)
roja=data.frame(roja)
colnames(roja)=c("Date","ORGN","ORGP","MINP","MINN","N_APP","P_APP")



for (i in 1:365){
  roja$Date[i]=i
  roja$ORGN[i]=sum(outdata_hru_stanfert$ORGNkg_per_ha[outdata_hru_stanfert$MON==i])
  roja$ORGP[i]=sum(outdata_hru_stanfert$ORGPkg_per_ha[outdata_hru_stanfert$MON==i])
  roja$MINP[i]=sum(outdata_hru_stanfert$SEDPkg_per_ha[outdata_hru_stanfert$MON==i])
  roja$MINN[i]=sum(outdata_hru_stanfert$NSURQkg_per_ha[outdata_hru_stanfert$MON==i])
  roja$N_APP[i]=sum(outdata_hru_stanfert$N_APPkg_per_ha[outdata_hru_stanfert$MON==i])
  roja$P_APP[i]=sum(outdata_hru_stanfert$P_APPkg_per_ha[outdata_hru_stanfert$MON==i])
}


###------------ Dairy Fert----------------------------------------------
load("~/FarmScenarios/dairyHRU.Rdata")

outdata_hru_dairy=subset(outdata_hru_dairy, outdata_hru_dairy$LULC=="HYMF")

amir=matrix(NA, ncol = 7, nrow = 365)
amir=data.frame(amir)
colnames(amir)=c("Date","ORGN","ORGP","MINP","MINN","N_APP","P_APP")



for (i in 1:365){
  amir$Date[i]=i
  amir$ORGN[i]=sum(outdata_hru_dairy$ORGNkg_per_ha[outdata_hru_dairy$MON==i])
  amir$ORGP[i]=sum(outdata_hru_dairy$ORGPkg_per_ha[outdata_hru_dairy$MON==i])
  amir$MINP[i]=sum(outdata_hru_dairy$SEDPkg_per_ha[outdata_hru_dairy$MON==i])
  amir$MINN[i]=sum(outdata_hru_dairy$NSURQkg_per_ha[outdata_hru_dairy$MON==i])
  amir$N_APP[i]=sum(outdata_hru_dairy$N_APPkg_per_ha[outdata_hru_dairy$MON==i])
  amir$P_APP[i]=sum(outdata_hru_dairy$P_APPkg_per_ha[outdata_hru_dairy$MON==i])
}
sum(roja$ORGN)
sum(amir$ORGN)
sum(roja$ORGP)
sum(amir$ORGP)
sum(roja$MINP)
sum(amir$MINP)
sum(roja$MINN)
sum(amir$MINN)
sum(roja$N_APP)
sum(amir$N_APP)
sum(roja$P_APP)
sum(amir$P_APP)
