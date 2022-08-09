load("~/FarmScenarios/dairyfertsub.Rdata")
load("~/FarmScenarios/standardfertsub.Rdata")
load("~/FarmScenarios/standardsub.Rdata")

#####------------------------------------Subbasin 1--------------------------------------
#outdata_sub_stan=subset(outdata_sub_stan, outdata_sub_stan$SUB==1)
outdata_sub_stanfert1=subset(outdata_sub_stanfert,outdata_sub_stanfert$SUB==1)
outdata_sub_dairy1=subset(outdata_sub_dairy,outdata_sub_dairy$SUB==1)
########ORGN
#orgN_stand_sub1=sum(outdata_sub_stan$ORGNkg_per_ha)*outdata_sub_stan$AREAkm2[1]*100 #kg
orgN_standfert_sub1=sum(outdata_sub_stanfert1$ORGNkg_per_ha)*outdata_sub_stanfert1$AREAkm2[1]*100
orgN_dairy_sub1=sum(outdata_sub_dairy1$ORGNkg_per_ha)*outdata_sub_dairy1$AREAkm2[1]*100
#########ORGP
#orgP_satand_sub1=sum(outdata_sub_stan$ORGPkg_per_ha)*outdata_sub_stan$AREAkm2[1]*100
orgP_standfert_sub1=sum(outdata_sub_stanfert1$ORGPkg_per_ha)*outdata_sub_stanfert1$AREAkm2[1]*100
orgP_dairy_sub1=sum(outdata_sub_dairy1$ORGPkg_per_ha)*outdata_sub_dairy1$AREAkm2[1]*100
#########NSURQ NO3 surface runoff
#NSURQ_stand_sub1=sum(outdata_sub_stan$NSURQkg_per_ha)*outdata_sub_stan$AREAkm2[1]*100
NSURQ_standfert_sub1=sum(outdata_sub_stanfert1$NSURQkg_per_ha)*outdata_sub_stanfert1$AREAkm2[1]*100
NSURQ_dairy_sub1=sum(outdata_sub_dairy1$NSURQkg_per_ha)*outdata_sub_dairy1$AREAkm2[1]*100
########SEDP mineral P
#SEDP_satnd_sub1=sum(outdata_sub_stan$SEDPkg_per_ha)*outdata_sub_stan$AREAkm2[1]*100
SEDP_standfert_sub1=sum(outdata_sub_stanfert1$SEDPkg_per_ha)*outdata_sub_stanfert1$AREAkm2[1]*100
SEDP_dairy_sub1=sum(outdata_sub_dairy1$SEDPkg_per_ha)*outdata_sub_dairy1$AREAkm2[1]*100

#####----------------------Subbasin 2---------------------------------------------------
#outdata_sub_stan=subset(outdata_sub_stan, outdata_sub_stan$SUB==1)
outdata_sub_stanfert2=subset(outdata_sub_stanfert,outdata_sub_stanfert$SUB==2)
outdata_sub_dairy2=subset(outdata_sub_dairy,outdata_sub_dairy$SUB==2)

########ORGN
#orgN_stand_sub1=sum(outdata_sub_stan$ORGNkg_per_ha)*outdata_sub_stan$AREAkm2[1]*100 #kg
orgN_standfert_sub2=sum(outdata_sub_stanfert2$ORGNkg_per_ha)*outdata_sub_stanfert2$AREAkm2[1]*100
orgN_dairy_sub2=sum(outdata_sub_dairy2$ORGNkg_per_ha)*outdata_sub_dairy2$AREAkm2[1]*100
#########ORGP
#orgP_satand_sub1=sum(outdata_sub_stan$ORGPkg_per_ha)*outdata_sub_stan$AREAkm2[1]*100
orgP_standfert_sub2=sum(outdata_sub_stanfert2$ORGPkg_per_ha)*outdata_sub_stanfert2$AREAkm2[1]*100
orgP_dairy_sub2=sum(outdata_sub_dairy2$ORGPkg_per_ha)*outdata_sub_dairy2$AREAkm2[1]*100
#########NSURQ NO3 surface runoff
#NSURQ_stand_sub1=sum(outdata_sub_stan$NSURQkg_per_ha)*outdata_sub_stan$AREAkm2[1]*100
NSURQ_standfert_sub2=sum(outdata_sub_stanfert2$NSURQkg_per_ha)*outdata_sub_stanfert2$AREAkm2[1]*100
NSURQ_dairy_sub2=sum(outdata_sub_dairy2$NSURQkg_per_ha)*outdata_sub_dairy2$AREAkm2[1]*100
########SEDP mineral P
#SEDP_satnd_sub1=sum(outdata_sub_stan$SEDPkg_per_ha)*outdata_sub_stan$AREAkm2[1]*100
SEDP_standfert_sub2=sum(outdata_sub_stanfert2$SEDPkg_per_ha)*outdata_sub_stanfert2$AREAkm2[1]*100
SEDP_dairy_sub2=sum(outdata_sub_dairy2$SEDPkg_per_ha)*outdata_sub_dairy2$AREAkm2[1]*100



#################################output.hru####################################
