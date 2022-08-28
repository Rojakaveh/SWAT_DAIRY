######------------------------------Baseline Scenario-----------------------------------------
load("~/DairyFeedMGT/SWATScen_NEW2/FarmLEVEL/Base.Rda")
outdata_hru_Base=subset(outdata_hru_Base,outdata_hru_Base$LULC=="HYAF")

outdata_hru_Base=outdata_hru_Base %>% select(HRU,AREAkm2,mdate,ORGNkg_per_ha,ORGPkg_per_ha,SEDPkg_per_ha,NSURQkg_per_ha,NLATQkg_per_ha,
                                             NO3Lkg_per_ha,NO3GWkg_per_ha,SOLPkg_per_ha,P_GWkg_per_ha)

outdata_hru_Base$AreaHa=outdata_hru_Base$AREAkm2*100
outdata_hru_Base$ORGNkg=outdata_hru_Base$ORGNkg_per_ha*outdata_hru_Base$AreaHa
outdata_hru_Base$ORGPkg=outdata_hru_Base$ORGPkg_per_ha*outdata_hru_Base$AreaHa
outdata_hru_Base$SEDPkg=outdata_hru_Base$SEDPkg_per_ha*outdata_hru_Base$AreaHa
outdata_hru_Base$NSURQkg=outdata_hru_Base$NSURQkg_per_ha*outdata_hru_Base$AreaHa
outdata_hru_Base$NLATQkg=outdata_hru_Base$NLATQkg_per_ha*outdata_hru_Base$AreaHa
outdata_hru_Base$NO3Lkg=outdata_hru_Base$NO3Lkg_per_ha*outdata_hru_Base$AreaHa
outdata_hru_Base$NO3GWkg=outdata_hru_Base$NO3GWkg_per_ha*outdata_hru_Base$AreaHa
outdata_hru_Base$SOLPkg=outdata_hru_Base$SOLPkg_per_ha*outdata_hru_Base$AreaHa
outdata_hru_Base$P_GWkg=outdata_hru_Base$P_GWkg_per_ha*outdata_hru_Base$AreaHa

#annually
ORGNkg=sum(outdata_hru_Base$ORGNkg)
ORGPkg=sum(outdata_hru_Base$ORGPkg)
SEDPkg=sum(outdata_hru_Base$SEDPkg)
NSURQkg=sum(outdata_hru_Base$NSURQkg)
NLATQkg=sum(outdata_hru_Base$NLATQkg)
NO3Lkg=sum(outdata_hru_Base$NO3Lkg)
NO3GWkg=sum(outdata_hru_Base$NO3GWkg)
SOLPkg=sum(outdata_hru_Base$SOLPkg)
P_GWkg=sum(outdata_hru_Base$P_GWkg)

##----------------------------------------Reduce TP-------------------------------------------------
load("~/DairyFeedMGT/SWATScen_NEW2/FarmLEVEL/ReduceTP.Rda")

outdata_hru_ReduceTP=subset(outdata_hru_ReduceTP,outdata_hru_ReduceTP$LULC=="HYAF")

outdata_hru_ReduceTP=outdata_hru_ReduceTP %>% select(HRU,AREAkm2,mdate,ORGNkg_per_ha,ORGPkg_per_ha,SEDPkg_per_ha,NSURQkg_per_ha,NLATQkg_per_ha,
                                             NO3Lkg_per_ha,NO3GWkg_per_ha,SOLPkg_per_ha,P_GWkg_per_ha)

outdata_hru_ReduceTP$AreaHa=outdata_hru_ReduceTP$AREAkm2*100
outdata_hru_ReduceTP$ORGNkg=outdata_hru_ReduceTP$ORGNkg_per_ha*outdata_hru_ReduceTP$AreaHa
outdata_hru_ReduceTP$ORGPkg=outdata_hru_ReduceTP$ORGPkg_per_ha*outdata_hru_ReduceTP$AreaHa
outdata_hru_ReduceTP$SEDPkg=outdata_hru_ReduceTP$SEDPkg_per_ha*outdata_hru_ReduceTP$AreaHa
outdata_hru_ReduceTP$NSURQkg=outdata_hru_ReduceTP$NSURQkg_per_ha*outdata_hru_ReduceTP$AreaHa
outdata_hru_ReduceTP$NLATQkg=outdata_hru_ReduceTP$NLATQkg_per_ha*outdata_hru_ReduceTP$AreaHa
outdata_hru_ReduceTP$NO3Lkg=outdata_hru_ReduceTP$NO3Lkg_per_ha*outdata_hru_ReduceTP$AreaHa
outdata_hru_ReduceTP$NO3GWkg=outdata_hru_ReduceTP$NO3GWkg_per_ha*outdata_hru_ReduceTP$AreaHa
outdata_hru_ReduceTP$SOLPkg=outdata_hru_ReduceTP$SOLPkg_per_ha*outdata_hru_ReduceTP$AreaHa
outdata_hru_ReduceTP$P_GWkg=outdata_hru_ReduceTP$P_GWkg_per_ha*outdata_hru_ReduceTP$AreaHa

#annually
ORGNkg=sum(outdata_hru_ReduceTP$ORGNkg)
ORGPkg=sum(outdata_hru_ReduceTP$ORGPkg)
SEDPkg=sum(outdata_hru_ReduceTP$SEDPkg)
NSURQkg=sum(outdata_hru_ReduceTP$NSURQkg)
NLATQkg=sum(outdata_hru_ReduceTP$NLATQkg)
NO3Lkg=sum(outdata_hru_ReduceTP$NO3Lkg)
NO3GWkg=sum(outdata_hru_ReduceTP$NO3GWkg)
SOLPkg=sum(outdata_hru_ReduceTP$SOLPkg)
P_GWkg=sum(outdata_hru_ReduceTP$P_GWkg)

######------------Increase TP-------------------------------
load("~/DairyFeedMGT/SWATScen_NEW2/FarmLEVEL/IncreaseTP.Rda")

outdata_hru_IncreaseTP=subset(outdata_hru_IncreaseTP,outdata_hru_IncreaseTP$LULC=="HYAF")

outdata_hru_IncreaseTP=outdata_hru_IncreaseTP %>% select(HRU,AREAkm2,mdate,ORGNkg_per_ha,ORGPkg_per_ha,SEDPkg_per_ha,NSURQkg_per_ha,NLATQkg_per_ha,
                                                     NO3Lkg_per_ha,NO3GWkg_per_ha,SOLPkg_per_ha,P_GWkg_per_ha)

outdata_hru_IncreaseTP$AreaHa=outdata_hru_IncreaseTP$AREAkm2*100
outdata_hru_IncreaseTP$ORGNkg=outdata_hru_IncreaseTP$ORGNkg_per_ha*outdata_hru_IncreaseTP$AreaHa
outdata_hru_IncreaseTP$ORGPkg=outdata_hru_IncreaseTP$ORGPkg_per_ha*outdata_hru_IncreaseTP$AreaHa
outdata_hru_IncreaseTP$SEDPkg=outdata_hru_IncreaseTP$SEDPkg_per_ha*outdata_hru_IncreaseTP$AreaHa
outdata_hru_IncreaseTP$NSURQkg=outdata_hru_IncreaseTP$NSURQkg_per_ha*outdata_hru_IncreaseTP$AreaHa
outdata_hru_IncreaseTP$NLATQkg=outdata_hru_IncreaseTP$NLATQkg_per_ha*outdata_hru_IncreaseTP$AreaHa
outdata_hru_IncreaseTP$NO3Lkg=outdata_hru_IncreaseTP$NO3Lkg_per_ha*outdata_hru_IncreaseTP$AreaHa
outdata_hru_IncreaseTP$NO3GWkg=outdata_hru_IncreaseTP$NO3GWkg_per_ha*outdata_hru_IncreaseTP$AreaHa
outdata_hru_IncreaseTP$SOLPkg=outdata_hru_IncreaseTP$SOLPkg_per_ha*outdata_hru_IncreaseTP$AreaHa
outdata_hru_IncreaseTP$P_GWkg=outdata_hru_IncreaseTP$P_GWkg_per_ha*outdata_hru_IncreaseTP$AreaHa

#annually
ORGNkg=sum(outdata_hru_IncreaseTP$ORGNkg)
ORGPkg=sum(outdata_hru_IncreaseTP$ORGPkg)
SEDPkg=sum(outdata_hru_IncreaseTP$SEDPkg)
NSURQkg=sum(outdata_hru_IncreaseTP$NSURQkg)
NLATQkg=sum(outdata_hru_IncreaseTP$NLATQkg)
NO3Lkg=sum(outdata_hru_IncreaseTP$NO3Lkg)
NO3GWkg=sum(outdata_hru_IncreaseTP$NO3GWkg)
SOLPkg=sum(outdata_hru_IncreaseTP$SOLPkg)
P_GWkg=sum(outdata_hru_IncreaseTP$P_GWkg)
##-----------------------Reduce TN___________________________________
load("~/DairyFeedMGT/SWATScen_NEW2/FarmLEVEL/ReduceTN.Rda")


outdata_hru_ReduceTN=subset(outdata_hru_ReduceTN,outdata_hru_ReduceTN$LULC=="HYAF")

outdata_hru_ReduceTN=outdata_hru_ReduceTN %>% select(HRU,AREAkm2,mdate,ORGNkg_per_ha,ORGPkg_per_ha,SEDPkg_per_ha,NSURQkg_per_ha,NLATQkg_per_ha,
                                                         NO3Lkg_per_ha,NO3GWkg_per_ha,SOLPkg_per_ha,P_GWkg_per_ha)

outdata_hru_ReduceTN$AreaHa=outdata_hru_ReduceTN$AREAkm2*100
outdata_hru_ReduceTN$ORGNkg=outdata_hru_ReduceTN$ORGNkg_per_ha*outdata_hru_ReduceTN$AreaHa
outdata_hru_ReduceTN$ORGPkg=outdata_hru_ReduceTN$ORGPkg_per_ha*outdata_hru_ReduceTN$AreaHa
outdata_hru_ReduceTN$SEDPkg=outdata_hru_ReduceTN$SEDPkg_per_ha*outdata_hru_ReduceTN$AreaHa
outdata_hru_ReduceTN$NSURQkg=outdata_hru_ReduceTN$NSURQkg_per_ha*outdata_hru_ReduceTN$AreaHa
outdata_hru_ReduceTN$NLATQkg=outdata_hru_ReduceTN$NLATQkg_per_ha*outdata_hru_ReduceTN$AreaHa
outdata_hru_ReduceTN$NO3Lkg=outdata_hru_ReduceTN$NO3Lkg_per_ha*outdata_hru_ReduceTN$AreaHa
outdata_hru_ReduceTN$NO3GWkg=outdata_hru_ReduceTN$NO3GWkg_per_ha*outdata_hru_ReduceTN$AreaHa
outdata_hru_ReduceTN$SOLPkg=outdata_hru_ReduceTN$SOLPkg_per_ha*outdata_hru_ReduceTN$AreaHa
outdata_hru_ReduceTN$P_GWkg=outdata_hru_ReduceTN$P_GWkg_per_ha*outdata_hru_ReduceTN$AreaHa

#annually
ORGNkg=sum(outdata_hru_ReduceTN$ORGNkg)
ORGPkg=sum(outdata_hru_ReduceTN$ORGPkg)
SEDPkg=sum(outdata_hru_ReduceTN$SEDPkg)
NSURQkg=sum(outdata_hru_ReduceTN$NSURQkg)
NLATQkg=sum(outdata_hru_ReduceTN$NLATQkg)
NO3Lkg=sum(outdata_hru_ReduceTN$NO3Lkg)
NO3GWkg=sum(outdata_hru_ReduceTN$NO3GWkg)
SOLPkg=sum(outdata_hru_ReduceTN$SOLPkg)
P_GWkg=sum(outdata_hru_ReduceTN$P_GWkg)
###-------------------------Increase TN------------------------------------------
load("~/DairyFeedMGT/SWATScen_NEW2/FarmLEVEL/IncreaseTN.Rda")

outdata_hru_IncreaseTN=subset(outdata_hru_IncreaseTN,outdata_hru_IncreaseTN$LULC=="HYAF")

outdata_hru_IncreaseTN=outdata_hru_IncreaseTN %>% select(HRU,AREAkm2,mdate,ORGNkg_per_ha,ORGPkg_per_ha,SEDPkg_per_ha,NSURQkg_per_ha,NLATQkg_per_ha,
                                                     NO3Lkg_per_ha,NO3GWkg_per_ha,SOLPkg_per_ha,P_GWkg_per_ha)

outdata_hru_IncreaseTN$AreaHa=outdata_hru_IncreaseTN$AREAkm2*100
outdata_hru_IncreaseTN$ORGNkg=outdata_hru_IncreaseTN$ORGNkg_per_ha*outdata_hru_IncreaseTN$AreaHa
outdata_hru_IncreaseTN$ORGPkg=outdata_hru_IncreaseTN$ORGPkg_per_ha*outdata_hru_IncreaseTN$AreaHa
outdata_hru_IncreaseTN$SEDPkg=outdata_hru_IncreaseTN$SEDPkg_per_ha*outdata_hru_IncreaseTN$AreaHa
outdata_hru_IncreaseTN$NSURQkg=outdata_hru_IncreaseTN$NSURQkg_per_ha*outdata_hru_IncreaseTN$AreaHa
outdata_hru_IncreaseTN$NLATQkg=outdata_hru_IncreaseTN$NLATQkg_per_ha*outdata_hru_IncreaseTN$AreaHa
outdata_hru_IncreaseTN$NO3Lkg=outdata_hru_IncreaseTN$NO3Lkg_per_ha*outdata_hru_IncreaseTN$AreaHa
outdata_hru_IncreaseTN$NO3GWkg=outdata_hru_IncreaseTN$NO3GWkg_per_ha*outdata_hru_IncreaseTN$AreaHa
outdata_hru_IncreaseTN$SOLPkg=outdata_hru_IncreaseTN$SOLPkg_per_ha*outdata_hru_IncreaseTN$AreaHa
outdata_hru_IncreaseTN$P_GWkg=outdata_hru_IncreaseTN$P_GWkg_per_ha*outdata_hru_IncreaseTN$AreaHa

#annually
ORGNkg=sum(outdata_hru_IncreaseTN$ORGNkg)
ORGPkg=sum(outdata_hru_IncreaseTN$ORGPkg)
SEDPkg=sum(outdata_hru_IncreaseTN$SEDPkg)
NSURQkg=sum(outdata_hru_IncreaseTN$NSURQkg)
NLATQkg=sum(outdata_hru_IncreaseTN$NLATQkg)
NO3Lkg=sum(outdata_hru_IncreaseTN$NO3Lkg)
NO3GWkg=sum(outdata_hru_IncreaseTN$NO3GWkg)
SOLPkg=sum(outdata_hru_IncreaseTN$SOLPkg)
P_GWkg=sum(outdata_hru_IncreaseTN$P_GWkg)
