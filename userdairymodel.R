Dairy_df=function(kMortality_calf,kMortality_yearling,kMortality_bredHeifer,kMortality_LH,
                  kMortality_BLH,kMortality_DBH,kMortality_SecondLC,kMortality_BredSecLC,
                  kMortality_DSecondLC,kMortality_ThirdLC,kMortality_BredThirdLC,kMortality_DThirdLC,
                  kMortality_LC,kMotality_BredLC,kMortality_DC,HFCR,HSCR,C2CR,C3CR,
                  C4CR,PropF,PropKeep,iCalf,iHeifer_first_lact,iHeifer_second_lact,
                  iHeifer_third_lact,iHeifer_first_dry,iHeifer_second_dry,iHeifer_third_dry,
                  iLact,iDry,iyearling,ibredHeifer,iBLH,iBredSecLC,iBredThirdLC,iBredLC,
                  Barn_capacity,Max_capacity,Breed,FCM ,calf_ME,calf_CP,fed_calf_P,heifer_ME,
                  heifer_CP,fed_heifer_P,lact_CP,fed_lact_P,dry_CP,fed_dry_P,
                  HRS,Temp,RHMD,WS){
  if (is.na(Max_capacity)==TRUE){
    Max_capacity=(35*Barn_capacity*365)/4   #35 kg/day-animal, 365 day, each 3 months 
    assign(paste0("Max_capacity"),Max_capacity,envir=parent.frame())
  } 
  Animal_df= data.frame(matrix(ncol=60, nrow=nrow(Date)))
  x=c("Date","calves_born","calves_to_yearling","calves_death","calves",
      "yearling_to_bredHeifers","yearling_death","yearling","bredHeifer_death",
      "bredHeifer_to_LactHeif","bredHeifer","LH_to_BLH","lactatingHeifers_death","DB1HC",
      "LactatingHeifers","BLH_to_DBH","BLH_death","BLH","DBH_to_SecondLC",
      "DBH_death","DBH","SecondLC_death","SecondLC_to_BredSecLC",
      "SecondLC","DBHC","BredSecLC_death","BredSecLC_to_DSecondLC","BredSecLC",
      "DSecondLC_death","DSecondLC_to_ThirdLC","DSecondLC","ThirdLC_death","ThirdLC_to_BredThirdLC",
      "ThirdLC","DB2LCC","BredThirdLC_death","BredThirdLC_to_DThirdLC",
      "BredThirdLC","DThirdLC_death","DThirdLC_to_LC","DThirdLC","LC_death",
      "LC_to_BredLC","DC_to_LC","LC","DB3LCC",
      "BredLC_death","BredLC_to_DC","BredLC","DC_death",
      "DC","BCC","total_number","yearling_cull","LactHeif_cull","SecondLC_cull",
      "ThirdLC_cull","LC_cull","excessive_cull","total_barn")
  colnames(Animal_df)=x
  
  #------------------------------------ initial numbers---------------------------------------------
  Animal_df$Date=Date
  Animal_df$calves[1]=iCalf #calves
  Animal_df$yearling[1]=iyearling #yearling
  Animal_df$bredHeifer[1]=ibredHeifer # bred heifer
  Animal_df$LactatingHeifers[1]=iHeifer_first_lact # first lactating heifers
  Animal_df$BLH[1]=iBLH # 1st bred lactating heifer 
  Animal_df$DBH[1]=iHeifer_first_dry 
  Animal_df$SecondLC[1]=iHeifer_second_lact
  Animal_df$BredSecLC[1]=iBredSecLC
  Animal_df$DSecondLC[1]=iHeifer_second_dry
  Animal_df$ThirdLC[1]=iHeifer_third_lact
  Animal_df$BredThirdLC[1]=iBredThirdLC
  Animal_df$DThirdLC[1]=iHeifer_third_dry
  Animal_df$LC[1]=iLact
  Animal_df$BredLC[1]=iBredLC
  Animal_df$DC[1]=iDry
  Animal_df$excessive_cull=0
  Animal_df$total_number=NA #total number of animal
  Animal_df$total_barn=NA #number of animal in the barn
  #-----------------------------capacity of the farm----------------------------------------------------
  # Barn_capacity=310 ##capacity of the barn, assume that calves, yearling, 1st lact, 2nd lact, 3rd lact, lc are gept in the barn
  #-----------------------------------loop to calculate daily population of each pool-----------------------------
  for (k in (1:nrow(Date))){
    Animal_df$total_number[k]=Animal_df$calves[k]+Animal_df$yearling[k]+Animal_df$bredHeifer[k]+
      Animal_df$LactatingHeifers[k]+Animal_df$BLH[k]+Animal_df$DBH[k]+Animal_df$SecondLC[k]+
      Animal_df$BredSecLC[k]+Animal_df$DSecondLC[k]+Animal_df$ThirdLC[k]+Animal_df$BredThirdLC[k]+
      Animal_df$DThirdLC[k]+Animal_df$LC[k]+Animal_df$BredLC[k]+Animal_df$DC[k]
    Animal_df$total_barn[k]=Animal_df$calves[k]+Animal_df$yearling[k]+Animal_df$LactatingHeifers[k]+Animal_df$SecondLC[k]+Animal_df$ThirdLC[k]+Animal_df$LC[k]
    if (Animal_df$total_barn[k]>Barn_capacity){
      Animal_df$excessive_cull[k]=(Animal_df$total_barn[k]-Barn_capacity)/6
    }
    Animal_df$calves_death[k]=Animal_df$calves[k]*kMortality_calf/365  ## calves death loss daily
    Animal_df$calves_to_yearling[k]=Animal_df$calves[k]/365 ##calves to yearling daily
    Animal_df$yearling_death[k]=Animal_df$yearling[k]*kMortality_yearling/365  ##  yearling death loss
    Animal_df$yearling_cull[k]=Animal_df$yearling[k]*(1-HFCR)/365  ## yearling culled
    Animal_df$yearling_to_bredHeifers[k]=(Animal_df$yearling[k]/90) ##yearling to bred heifer
    Animal_df$bredHeifer_death[k]=Animal_df$bredHeifer[k]*kMortality_bredHeifer/365 ##bread heifer 
    Animal_df$bredHeifer_to_LactHeif[k]=Animal_df$bredHeifer[k]/285
    if (Animal_df$bredHeifer_to_LactHeif[k]<0){
      Animal_df$bredHeifer_to_LactHeif[k]==0
    }
    Animal_df$DB1HC[k]=Animal_df$bredHeifer_to_LactHeif[k]
    Animal_df$LactHeif_cull[k]=Animal_df$LactatingHeifers[k]*(1-HSCR)/365 #lactating heifer to unbred lactating heifer
    Animal_df$lactatingHeifers_death[k]=Animal_df$LactatingHeifers[k]*kMortality_LH/365  #DL lactating heifers
    Animal_df$LH_to_BLH[k]=Animal_df$LactatingHeifers[k]/85 ##lactating Heifers to bred lactation heifers
    Animal_df$BLH_death[k]=Animal_df$BLH[k]*kMortality_BLH/365 # bred lactating heifers death loss
    Animal_df$BLH_to_DBH[k]=Animal_df$BLH[k]/220 ## bred lactating heifer to dry bred heifers
    Animal_df$DBH_death[k]=Animal_df$DBH[k]*kMortality_DBH/365 ## dry bred heifer death loss
    Animal_df$DBH_to_SecondLC[k]=Animal_df$DBH[k]/60 #dry bred heifer to 2nd lactation cows
    if (Animal_df$DBH_to_SecondLC[k]< 0){
      Animal_df$DBH_to_SecondLC[k]=0
    }
    Animal_df$DBHC[k]= Animal_df$DBH_to_SecondLC[k]
    Animal_df$SecondLC_death[k]=Animal_df$SecondLC[k]*kMortality_SecondLC/365 # 2nd lactating cows death loss
    Animal_df$SecondLC_cull[k]=Animal_df$SecondLC[k]*(1-C2CR)/365 ##2nd lactation cows cull
    Animal_df$SecondLC_to_BredSecLC[k]=(Animal_df$SecondLC[k]/85) ## 2nd lactating cows to bred 2nd lactating cows
    Animal_df$BredSecLC_death[k]=Animal_df$BredSecLC[k]*kMortality_BredSecLC/365 # Bred 2nd lactating cows death loss
    Animal_df$BredSecLC_to_DSecondLC[k]=Animal_df$BredSecLC[k]/220  # bred 2nd lactating cows to dry 2nd loc cows 
    Animal_df$DSecondLC_death[k]=Animal_df$DSecondLC[k]*kMortality_DSecondLC/365 ##dry 2nd lac cows death loss
    Animal_df$DSecondLC_to_ThirdLC[k]=Animal_df$DSecondLC[k]/60 # dry 2nd lac cows to 3rd lactation cows
    if (Animal_df$DSecondLC_to_ThirdLC[k]<0){
      Animal_df$DSecondLC_to_ThirdLC[k]=0  
    }
    Animal_df$DB2LCC[k]=Animal_df$DSecondLC_to_ThirdLC[k] #DB2LCC
    Animal_df$ThirdLC_death[k]=Animal_df$ThirdLC[k]*kMortality_ThirdLC/365 # 3rd lactation cows death loss
    Animal_df$ThirdLC_cull[k]=Animal_df$ThirdLC[k]*(1-C3CR)/365 # 3rd lactation cows cull
    Animal_df$ThirdLC_to_BredThirdLC[k]=(Animal_df$ThirdLC[k]/85) # 3rd lactation cows to bred 3rd lactation cows 
    Animal_df$BredThirdLC_death[k]=Animal_df$BredThirdLC[k]*kMortality_BredThirdLC/365 #Bred 3rd lactation cows death loss
    Animal_df$BredThirdLC_to_DThirdLC[k]=Animal_df$BredThirdLC[k]/220 #Bred 3rd lactation cows to dry 2rd lac cows
    Animal_df$DThirdLC_death[k]=Animal_df$DThirdLC[k]*kMortality_DThirdLC/365 ##Dry 3rd lactation cows death loss
    Animal_df$DThirdLC_to_LC[k]=Animal_df$DThirdLC[k]/60 ##Dry 3rd lactation cows to lactating cows
    if (Animal_df$DThirdLC_to_LC[k]<0){
      Animal_df$DThirdLC_to_LC[k]=0
    }
    Animal_df$DB3LCC[k]=Animal_df$DThirdLC_to_LC[k] #DB3LCC
    Animal_df$LC_death[k]=Animal_df$LC[k]*kMortality_LC/365 #Lactating cows death loss
    Animal_df$LC_cull[k]=Animal_df$LC[k]*(1-C4CR)/365
    Animal_df$LC_to_BredLC[k]=(Animal_df$LC[k]/85)##Lactating cows to bred lactating cows
    Animal_df$BredLC_death[k]=Animal_df$BredLC[k]*kMotality_BredLC/365 #Bred lactating cows death loss
    Animal_df$BredLC_to_DC[k]=Animal_df$BredLC[k]/220 
    Animal_df$DC_death[k]=Animal_df$DC[k]*kMortality_DC/365 ###Dry cows death loss
    Animal_df$DC_to_LC[k]=Animal_df$DC[k]/60 ## dry cow to lactating cows
    if(Animal_df$DC_to_LC[k]<0){
      Animal_df$DC_to_LC[k]=0
    }
    Animal_df$BCC[k]=Animal_df$DC_to_LC[k]
    Animal_df$calves_born[k]=(Animal_df$DB1HC[k]+Animal_df$DBHC[k]+Animal_df$DB2LCC[k]+Animal_df$DB3LCC[k]+Animal_df$BCC[k])*PropF*PropKeep#CALVES BORN
    if (Animal_df$calves_born[k]<0){
      Animal_df$calves_born[k]=0
    }
    if (k+1 <= nrow(Date)){
      Animal_df$calves[k+1]=Animal_df$calves[k]+Animal_df$calves_born[k]-Animal_df$calves_death[k]-Animal_df$calves_to_yearling[k]-Animal_df$excessive_cull[k]##calf pool
      if (Animal_df$calves[k+1]<0){
        Animal_df$calves[k+1]=0
      }
      Animal_df$yearling[k+1]=Animal_df$yearling[k]+Animal_df$calves_to_yearling[k]-Animal_df$yearling_death[k]-Animal_df$yearling_to_bredHeifers[k]-Animal_df$yearling_cull[k]-Animal_df$excessive_cull[k]#yearling pool
      if(Animal_df$yearling[k+1]<0){
        Animal_df$yearling[k+1]=0
      }
      Animal_df$bredHeifer[k+1]=Animal_df$bredHeifer[k]+Animal_df$yearling_to_bredHeifers[k]-Animal_df$bredHeifer_death[k]-Animal_df$bredHeifer_to_LactHeif[k] #bred heifer pool
      if (Animal_df$bredHeifer[k+1]<0){
        Animal_df$bredHeifer[k+1]=0
      }
      Animal_df$LactatingHeifers[k+1]=Animal_df$LactatingHeifers[k]+Animal_df$bredHeifer_to_LactHeif[k]-Animal_df$LH_to_BLH[k]-Animal_df$lactatingHeifers_death[k]-Animal_df$LactHeif_cull[k]-Animal_df$excessive_cull[k] ##lactating heifer pool
      if (Animal_df$LactatingHeifers[k+1]<0){
        Animal_df$LactatingHeifers[k+1]=0
      }
      Animal_df$BLH[k+1]=Animal_df$BLH[k]+Animal_df$LH_to_BLH[k]-Animal_df$BLH_to_DBH[k]-Animal_df$BLH_death[k] #bred lactating heifers
      if (Animal_df$BLH[k+1]<0){
        Animal_df$BLH[k+1]=0
      }
      Animal_df$DBH[k+1]=Animal_df$DBH[k]+Animal_df$BLH_to_DBH[k]-Animal_df$DBH_to_SecondLC[k]-Animal_df$DBH_death[k] #dry bred heifer 
      if (Animal_df$DBH[k+1]<0){
        Animal_df$DBH[k+1]=0
      }
      Animal_df$SecondLC[k+1]=Animal_df$SecondLC[k]+Animal_df$DBH_to_SecondLC[k]-Animal_df$SecondLC_death[k]-Animal_df$SecondLC_to_BredSecLC[k]-Animal_df$SecondLC_cull[k]-Animal_df$excessive_cull[k]  ##2nd lactation cows
      if (Animal_df$SecondLC[k+1]<0){
        Animal_df$SecondLC[k+1]=0
      }
      Animal_df$BredSecLC[k+1]=Animal_df$BredSecLC[k]+Animal_df$SecondLC_to_BredSecLC[k]-Animal_df$BredSecLC_death[k]-Animal_df$BredSecLC_to_DSecondLC[k] # bred 2nd lactating cows
      if (Animal_df$BredSecLC[k+1]<0){
        Animal_df$BredSecLC[k+1]=0
      }
      Animal_df$DSecondLC[k+1]=Animal_df$DSecondLC[k]+Animal_df$BredSecLC_to_DSecondLC[k]-Animal_df$DSecondLC_death[k]-Animal_df$DSecondLC_to_ThirdLC[k] #Dry 2nd lac cows
      if (Animal_df$DSecondLC[k+1]<0){
        Animal_df$DSecondLC[k+1]=0
      }
      Animal_df$ThirdLC[k+1]=Animal_df$ThirdLC[k]+Animal_df$DSecondLC_to_ThirdLC[k]-Animal_df$ThirdLC_death[k]-Animal_df$ThirdLC_to_BredThirdLC[k]-Animal_df$ThirdLC_cull[k]-Animal_df$excessive_cull[k]#3rd lactation cows
      if (Animal_df$ThirdLC[k+1]<0){
        Animal_df$ThirdLC[k+1]=0
      }
      Animal_df$BredThirdLC[k+1]=Animal_df$BredThirdLC[k]+Animal_df$ThirdLC_to_BredThirdLC[k]-Animal_df$BredThirdLC_death[k]-Animal_df$BredThirdLC_to_DThirdLC[k] #bred 3rd lac cows 
      if(Animal_df$BredThirdLC[k+1]<0){
        Animal_df$BredThirdLC[k+1]=0
      }
      Animal_df$DThirdLC[k+1]=Animal_df$DThirdLC[k]+Animal_df$BredThirdLC_to_DThirdLC[k]-Animal_df$DThirdLC_to_LC[k]-Animal_df$DThirdLC_death[k] #dry 3rd lac cows
      if (Animal_df$DThirdLC[k+1]<0){
        Animal_df$DThirdLC[k+1]=0
      }
      Animal_df$LC[k+1]=Animal_df$LC[k]+Animal_df$DThirdLC_to_LC[k]-Animal_df$LC_to_BredLC[k]-Animal_df$LC_death[k]+Animal_df$DC_to_LC[k]-Animal_df$LC_cull[k]-Animal_df$excessive_cull[k] ##lactating cows
      if (Animal_df$LC[k+1]<0){
        Animal_df$LC[k+1]=0
      }
      Animal_df$BredLC[k+1]=Animal_df$BredLC[k]+Animal_df$LC_to_BredLC[k]-Animal_df$BredLC_death[k]-Animal_df$BredLC_to_DC[k] # bred lactating cows
      if (Animal_df$BredLC[k+1]<0){
        Animal_df$BredLC[k+1]=0
      }
      Animal_df$DC[k+1]=Animal_df$DC[k]+Animal_df$BredLC_to_DC[k]-Animal_df$DC_death[k]-Animal_df$DC_to_LC[k] #dry cows
      if (Animal_df$DC[k+1]<0){
        Animal_df$DC[k+1]=0
      }
    } 
  }
  
  Animal_df=Animal_df[c((365*SWATnsky):nrow(Animal_df)),]
  ##########################################################################
  ##calculation of    CETI | Current month’s effective temperature index, C  (fox et al 2004)
  #####################################################################
  CETI = 27.88 - (0.456 * Temp) + (0.010754 * Temp**2)-
    (0.4905 * RHMD) + (0.00088 * RHMD**2)+ (1.1507 * WS) -
    (0.126447 * WS**2)+ (0.019876 * Temp * RHMD)-
    (0.046313 * Temp * WS)+ (0.4167 * HRS)
  #######################################################################
  #calculation of DMINC |  DMI (DRY MATTER INTAKE) night cooling adjustment, dimensionless
  #########################################################################
  DMINC = (119.62 - 0.9708 * CETI)/100
  ################################################# DMIF_temp is DMI adjustment factor with night cooling, dimensionless
  if(Temp>20){ 
    DMIAF_temp = DMINC
  }else{
    DMIAF_temp = 1.0433 - (0.0044 * Temp) + (0.0001 * Temp**2)
  }
  ######################################################################
  ########NEma is Dietary content of net energy for maintenance (kcal/kg)
  #######################################################################
  calf_NEma = (1.37 * calf_ME) - (0.138 * calf_ME**2) + (0.0105 * calf_ME**3) - 1.12
  heifer_NEma = (1.37 * heifer_ME) - (0.138 * heifer_ME**2) + (0.0105 * heifer_ME**3) - 1.12
  
  ##correction for breed index (fox et al 2004)
  if(Breed == 1){
    BI = 1.08
  }else{ 
    BI = 1
  }
  ##########################################
  ###functions
  #####################################
  ###############################################
  ###1-BODY WEIGHT function ##input t and retuen BW
  ###########################################
  dairy_ADG=function(t){
    A = 619 #asymptotic weight, kg
    k = 0.0020 #Rate parameter
    b = 0.905 # integration constant
    M = 1.2386 #inflection parameter
    BW = A*(1-(b*exp(-k*t)))^M
    return(BW)
  }
  ######################################################################
  ##calculations of weights using dairy_ADG function
  ###############################################################
  #wt updating which is a function of t (time)
  wtLact = dairy_ADG(2500) #wt of lact kg
  wtDry = dairy_ADG(2500)  #wt of dry
  wtCalf = dairy_ADG(365)  #wt of calf
  wtyearling=dairy_ADG(455) #wt yearling
  wtbreadHeifer=dairy_ADG(740) #wt bred heifers
  wtHeifer_first_lact = dairy_ADG(825) #wt of HFL
  wtBredLactHeif=dairy_ADG(1045) #wt bred 1st lactating heifers
  wtHeifer_second_lact = dairy_ADG(1190) #wt of HSL
  wtBred2ndLC=dairy_ADG(1410) #wt bred 2nd lactating cows
  wtHeifer_third_lact = dairy_ADG(1555) #wt of HTL
  wtBred3rdLC=dairy_ADG(1775) # wt bred 3rd lactating cows
  WTHFD = dairy_ADG(1105) #wt of HFD
  WTSFD = dairy_ADG(1470) #wt of HSD
  WTHTD = dairy_ADG(1835) #wt of HTD
  wtbredLC=dairy_ADG(2500) #wt of fourth or greater bred lactating cows
  #################################################################################
  ######### DMI dry matter intak function for calf, BW is an argument and dry matter intake (DMI) is an output
  ##############################################################################
  calfDMI= function(BW){
    SBW = 0.94*BW #SHRUNK BODY WEIGHT
    DMI = (SBW**0.75)*(((0.2435*calf_NEma)-(0.0466*calf_NEma**2)-0.1128)/calf_NEma)*DMIAF_temp*BI
    return(DMI)
  }
  ###############################################
  ####DMI function for heifers, BW is an argument and dry matter intake (DMI) is an output
  ######################################
  heiferDMI=function(BW){
    SBW = 0.94*BW
    DMI = (SBW**0.75)*(((0.2435*calf_NEma)-(0.0466*calf_NEma**2)-0.0869)/heifer_NEma)*DMIAF_temp*BI
    return(DMI)
  }
  #################################################
  #####DMI function for lact,BW is an argument and dry matter intake (DMI) is an output
  ###################################################
  lactDMI=function(BW){
    SBW = 0.94*BW
    DMI = ((0.0185 * BW) + (0.305 * FCM))*DMIAF_temp*BI
    return(DMI)
  }
  ##############################################################
  ################ DMI function for dry cows, BW is an argument and dry matter intake (DMI) is an output
  #############################################
  dryDMI=function(BW){
    SBW = 0.94*BW
    DMI = (0.0185 * SBW)*DMIAF_temp*BI
    return(DMI)
  }
  
  #########################################################
  ###calculation of DMIs for each pool based on above functions, kg
  ########################################################
  calf_DMI = calfDMI(wtCalf) #dmi calf
  yearling_DMI=heiferDMI(wtyearling) #dmi yearling
  bredHeifer_DMI= heiferDMI(wtbreadHeifer) #dmi bred heifers
  heifer_first_lact_DMI = lactDMI(wtHeifer_first_lact) #dmi 1st lactating
  bredlactatingHeifer_DMI= lactDMI(wtBredLactHeif) #dmi bred 1st lactating heifer
  heifer_first_dry_DMI = dryDMI(WTHFD) #dmi heifer 1st dry
  heifer_second_lact_DMI = lactDMI(wtHeifer_second_lact) #dmi 2nd lact
  bred2ndlact_DMI=lactDMI(wtBred2ndLC) #dmi bred 2nd lact cows
  heifer_second_dry_DMI = dryDMI(WTSFD) #dmi second dry cows
  heifer_third_lact_DMI = lactDMI(wtHeifer_third_lact) #dmi 3rd lactating cows
  bred3rdlact_DMI=lactDMI(wtBred3rdLC) #dmi 3rd bred lactating cows
  heifer_third_dry_DMI = dryDMI(WTHTD) #dmi 3rd lactating cows
  lact_DMI = lactDMI(wtLact) #dmi 4th lact or greater
  bredLactatCow_DMI=lactDMI(wtbredLC) #dmi bred lactating cows
  dry_DMI = dryDMI(wtDry) #dmi 4th dry or greater
  #####################################################################
  ##############Nitrogen Equations per cow , gr
  #####################################################################
  ###-------------------------------1) N excretion from heifer , DMI is an argument and Nexc is output, gr-------------------------
  heiferNexc=function(DMI){
    Nexc = ((((heifer_CP/100)*(DMI*1000))/(DMI*1000)*DMI)*78.39+51.4) #per cow , heifer_CP and DMI
    return(Nexc)
  }
  
  ###-------------------------------2) N excretion from calf, DMI is an argument and Nexc is output,gr---------------------------
  calfNexc=function(DMI){
    Nexc = ((((calf_CP/100)*(DMI*1000))/(DMI*1000)*DMI)*112.55) # calf_CP and DMI
    return(Nexc)
    
  }
  ###------------------------------3) N excretion from lact---------------------------------------
  lactNexc=function(DMI){
    Nexc = (((((lact_CP/100)*(DMI*1000))/(DMI*1000)*DMI)*84.1)+(wtLact*0.196)) #lact_cp and DMI
    return(Nexc)
  }
  
  #####----------------------------4)N excretion from dry ,----------------------------------------
  dryNexc=function(DMI){
    Nexc = ((((dry_CP/100)*(DMI*1000))/(DMI*1000)*DMI)*78.39+51.4) ##dry_CP and DMI
    return(Nexc)
  }
  
  #####################################################
  # Phosphorus Equations per cow,gr 
  ######################################################
  ###----------------------------------------1) P excretion from calf---------------------------
  calfPexc=function(DMI, fed_P){
    Pexc = ((((fed_P/100)*(DMI*1000))/(DMI*1000)*DMI)*622.03) # fed_calf_P and DMI 
    return(Pexc)
  }
  
  ###----------------------------------------2) P excreation from heifer or  lact----------------
  cowPexc=function(DMI, fed_P){
    Pexc = 7.5 + ((((fed_P/100)*(DMI*1000))/(DMI*1000)*DMI)*560.7+2.1 ) #fed_P and DMI
    return(Pexc)
  }
  
  ###----------------------------------------3) P excretion from dry -------------------------------
  dryPexc=function(DMI, fed_P){
    Pexc = ((fed_P/100)*(DMI*1000)*0.76)
    return(Pexc)
  }
  
  ###############adding a column of amount of fertilizer applied to that hru
  Animal_df$Fert_applied=0
  for (i in mgt_datafram$HRU_number[mgt_datafram$Barn_number==j]){
    #i=10114
    Animal_df$Fert_applied[Animal_df$Date==mgt_datafram$Date_app[mgt_datafram$HRU_number==i]]=Animal_df$Fert_applied[Animal_df$Date==mgt_datafram$Date_app[mgt_datafram$HRU_number==i]]+mgt_datafram$FERT_APPLIED[mgt_datafram$HRU_number==i]
  }
  ##########################################################################
  ##################################################
  #N and P produced by 1 cow,kg
  ###################################################
  #----------------------1)N , kg--------------------------------------------------
  N_calf = calfNexc(calf_DMI)/1000  #kg N by calf
  N_yearling=heiferNexc(yearling_DMI)/1000 #kg N by yearling
  N_bredheifer=heiferNexc(bredHeifer_DMI)/1000 #kg N by bred heifers
  N_heifer_first_lact = lactNexc(heifer_first_lact_DMI)/1000 #kg N by HFL
  N_bred1stlact= lactNexc(bredlactatingHeifer_DMI)/1000 #kg N by bred lactating heifers
  N_heifer_first_dry = dryNexc(heifer_first_dry_DMI)/1000 #kg N by HFD
  N_heifer_second_lact= lactNexc(heifer_second_lact_DMI)/1000 #kg N by HSL
  N_bred2nlact=lactNexc(bred2ndlact_DMI)/1000 #kg N by bred 2nd lactating cows
  N_heifer_second_dry = dryNexc(heifer_second_dry_DMI)/1000 #kg N by HSD
  N_heifer_third_lact= lactNexc(heifer_third_lact_DMI)/1000 #kg N by HTL
  N_bred3rdlact= lactNexc(bred3rdlact_DMI) #kg N by bred 3rd lactating cows
  N_heifer_third_dry = dryNexc(heifer_third_dry_DMI)/1000 #kg N by HTD
  N_lact= lactNexc(lact_DMI)/1000 #kg N by lact 
  N_bredLC=lactNexc(bredLactatCow_DMI) #kg N by bred lactating cows
  N_dry = dryNexc(dry_DMI)/1000 #  kg N by dry cow
  #-----------------------2)P, kg -----------------------------------------------
  P_calf = calfPexc(calf_DMI,fed_calf_P)/1000 #kg P by calf
  P_yearling= calfPexc(yearling_DMI,fed_heifer_P)/1000 #kg P by yearling
  P_bredheifer= cowPexc(bredHeifer_DMI,fed_heifer_P)/1000 #kg P by bred heifer
  P_heifer_first_lact= cowPexc(heifer_first_lact_DMI, fed_lact_P)/1000 #kg P by HFL
  P_bred1stlact=cowPexc(bredlactatingHeifer_DMI,fed_lact_P )/1000 #kg P by bred 1st lactating cows
  P_heifer_first_dry = dryPexc(heifer_first_dry_DMI, fed_dry_P)/1000 #kg P by HFD
  P_heifer_second_lact = cowPexc(heifer_second_lact_DMI,  fed_lact_P)/1000 #kg P by HSL
  P_bred2nlact=cowPexc(bred2ndlact_DMI,fed_lact_P)/1000 #kg P by bred 2nd lactating cows
  P_heifer_second_dry = dryPexc(heifer_second_dry_DMI, fed_dry_P)/1000 #kg P by HSD
  P_heifer_third_lact = cowPexc(heifer_third_lact_DMI,  fed_lact_P)/1000 #kg P by HTL
  P_bred3rdlact = cowPexc(bred3rdlact_DMI, fed_lact_P)/1000 #kg P by bred 3rd lactating cows
  P_heifer_third_dry = dryPexc(heifer_third_dry_DMI, fed_dry_P)/1000 #kg P by HTD
  P_lact = cowPexc(lact_DMI,fed_lact_P)/1000 #kg P by lact
  P_bredLC=cowPexc(bredLactatCow_DMI,fed_lact_P )/1000 #kg P by bred lactating cows 
  P_dry = dryPexc(dry_DMI,fed_dry_P)/1000 #kg P by dry 
  #####################################################
  #N and P updates for the number of animals multiply by the number of animal in each pool
  #####################################################
  #---------------------------------------N is just a function of DMI--------------------------------------
  Animal_df$calf_N=N_calf*Animal_df$calves # kg of total N produced by the numbers of calves, daily
  Animal_df$yearling_N=N_yearling*Animal_df$yearling #kg of total N produced by the number of yearling, daily
  Animal_df$bredHeifer_N=N_bredheifer*Animal_df$bredHeifer #kg of total N produced by the umber of bred heifer, daily 
  Animal_df$heifer_first_lact_N=N_heifer_first_lact*Animal_df$LactatingHeifers #kg of total N produced by the number of HFL
  Animal_df$BLH_N=N_bred1stlact*Animal_df$BLH #kg of total N produced by the number of Bred lactating heifer
  Animal_df$heifer_first_dry_N=N_heifer_first_dry*Animal_df$DBH #kg of total N produced by number of HFD
  Animal_df$heifer_second_lact_N=N_heifer_second_lact*Animal_df$SecondLC #kg of total N produced by number of HSL
  Animal_df$bred2ndlact_N=N_bred2nlact*Animal_df$BredSecLC #kg of total N produced by number of bred 2nd lactating cows
  Animal_df$heifer_second_dry_N=N_heifer_second_dry*Animal_df$DSecondLC #kg of total N produced by HSD
  Animal_df$heifer_third_lact_N=N_heifer_third_lact*Animal_df$ThirdLC #kg of total N produced by number of HTL
  Animal_df$bred3rdlact_N=N_bred3rdlact*Animal_df$BredThirdLC #kg of total N produced by number of bred 3rd lactating cows
  Animal_df$heifer_third_dry_N=N_heifer_third_dry*Animal_df$DThirdLC #kg of total N produced by HTD
  Animal_df$lact_N=N_lact*Animal_df$LC#kg of total N produced by the number of lact cows
  Animal_df$bredLC_N=N_bredLC*Animal_df$BredLC #kg of N  produced by the number of bred lactating cows 
  Animal_df$dry_N= N_dry*Animal_df$DC # kg of total N produced by the number of dry cows
  #--------------------------------P is a fuction of DMI and fed_P	except calves----------------------- 
  Animal_df$calf_P = P_calf*Animal_df$calves #kg of total P calves
  Animal_df$yearling_P= P_yearling*Animal_df$yearling #kg of total P by yearlings
  Animal_df$bredHeifer_P=P_bredheifer*Animal_df$bredHeifer # kg of total P by bred heifer
  Animal_df$heifer_first_lact_P=P_heifer_first_lact*Animal_df$LactatingHeifers #kg of total P HFL
  Animal_df$BLH_P=P_bred1stlact*Animal_df$BLH #kg of total P by bred 1st lactating cows
  Animal_df$heifer_first_dry_P = P_heifer_first_dry*Animal_df$DBH #kg of total P produced by number of HFD
  Animal_df$heifer_second_lact_P = P_heifer_second_lact*Animal_df$SecondLC #kg of total P produced by number of HSL
  Animal_df$bred2ndlact_P=P_bred2nlact*Animal_df$BredSecLC #kg of total P produced by number of bred 2nd lactating cows
  Animal_df$heifer_second_dry_P = P_heifer_second_dry*Animal_df$DSecondLC #kg of total P produced by HSD
  Animal_df$heifer_third_lact_P = P_heifer_third_lact*Animal_df$ThirdLC #kg of total P produced by number of HTL
  Animal_df$bred3rdlact_P=P_bred3rdlact*Animal_df$BredThirdLC #kg of total P produced by number of bred 3rd lactating cows
  Animal_df$heifer_third_dry_P = P_heifer_third_dry*Animal_df$DThirdLC #kg of total P produced by HTD
  Animal_df$lact_P=P_lact*Animal_df$LC#kg of total P produced by the number of lact cows
  Animal_df$bredLC_P=P_bredLC*Animal_df$BredLC #kg of P  produced by the number of bred lactating cows 
  Animal_df$dry_P = P_dry*Animal_df$DC # kg of total P produced by the number of dry cows
  ########################################################################
  ######daily TOTAL N and total P in Barn AND PASTURE, lactation cows are in the barn and dry cows+calves in the pasture
  #################################################################
  #------------------------Total N in Barn, kg------------------------------------------------------
  Animal_df$total_N_barn= Animal_df$calf_N+ Animal_df$yearling_N+Animal_df$heifer_first_lact_N+Animal_df$heifer_second_lact_N+
    Animal_df$heifer_third_lact_N+
    Animal_df$lact_N  ## daily kg total N produced in the barn
  #---------------Total N in pasture, kg-------------------------------------------------------------
  Animal_df$total_N_Past =Animal_df$bredHeifer_N+Animal_df$BLH_N+Animal_df$heifer_first_dry_N+Animal_df$bred2ndlact_N+
    Animal_df$heifer_second_dry_N+Animal_df$bredLC_N+Animal_df$heifer_third_dry_N+Animal_df$dry_N+Animal_df$bred3rdlact_N ##daily kg of total N produced in the pasture
  #-----------------------Total P in Barn, kg--------------------------------------------------
  Animal_df$total_P_barn = Animal_df$calf_P+Animal_df$heifer_first_lact_P+Animal_df$heifer_second_lact_P+Animal_df$yearling_P+
    Animal_df$heifer_third_lact_P+
    Animal_df$lact_P #daily kg of total P produced in the barn
  #-----------------------Total P in pasture, kg--------------------------------------------------
  Animal_df$total_P_Past = Animal_df$BLH_P+Animal_df$bredHeifer_P+Animal_df$bred2ndlact_P+Animal_df$heifer_first_dry_P+Animal_df$bred3rdlact_P+
    Animal_df$heifer_second_dry_P+Animal_df$heifer_third_dry_P+Animal_df$dry_P+Animal_df$bredLC_P #daily kg of total P produced in the pasture
  #################################################################
  ########daily totalNP in the barn daily N +daily P
  #######################################################################
  Animal_df$totalNP=Animal_df$total_N_barn+Animal_df$total_P_barn
  ##############################################################
  #####################1) TOTAL N AND P STORED IN THE BARN/ #########################################################
  ######2) TOTAL N+P FROM STORED MANURE IN BARN, kg 
  #############################################################
  ##############################################################
  Animal_df$Total_N_barn_stored=0
  Animal_df$Total_P_barn_stored=0
  Animal_df$Total_N_barn_stored[1]=Animal_df$total_N_barn[1]
  Animal_df$Total_P_barn_stored[1]=Animal_df$total_P_barn[1]
  Animal_df$TotalNP_barn=0
  Animal_df$DMmanure=0
  Animal_df$stored_manure=0
  Animal_df$TotalNP_barn[1]=Animal_df$totalNP[1]
  ##################################################
  ####getting stored manure from stored total N + total P (Jokela et al., 2010)
  #################################################
  Animal_df$DMmanure[1]=Animal_df$TotalNP_barn[1]/0.046 # dry matter basis for fraction
  Animal_df$stored_manure[1]=Animal_df$TotalNP_barn[1]/0.007 # semisolid as fertilizer applied for maximum capacity
  ####################################
  for(k in 1:nrow(Animal_df)){
    if (k+1 <= nrow(Animal_df)){
      if (Animal_df$Fert_applied[k] > Animal_df$stored_manure[k]){
        Animal_df$Fert_applied[k] = Animal_df$stored_manure[k]
        message(paste0("for HRU that have fert app on",Animal_df$Date[k], "the amount of fertilization is more than stored manure, applied fert in kg is set to", Animal_df$stored_manure[k]))
      }
      if (Animal_df$stored_manure[k]>= Max_capacity){
        Animal_df$Fert_applied[k]=Max_capacity
        Animal_df$TotalNP_barn[k]=Max_capacity*0.007
        Animal_df$stored_manure[k]=Max_capacity
        Animal_df$DMmanure[k]=Max_capacity*0.152
        for (i in unique(mgt_datafram$HRU_number[mgt_datafram$Barn_number==j])){
          #print(k)
          nam2=paste0("new_auto_fert",i,k)
          assign(paste0("new_auto_fert",i,k), assign(nam2,data.frame(Barn_number=mgt_datafram$Barn_number[mgt_datafram$HRU_number==i][1],
                                                                     HRU_number=as.numeric(i),
                                                                     year_app=year(Animal_df$Date[k]),
                                                                     month_app=month(Animal_df$Date[k]),
                                                                     day_app=day(Animal_df$Date[k]),
                                                                     MGT_OP=as.numeric(3),
                                                                     FRT_KG=(Max_capacity/FARMarea),
                                                                     Area_ha=mgt_datafram$Area_ha[mgt_datafram$HRU_number==i][1],
                                                                     Date_app=Animal_df$Date[k],
                                                                     FERT_NAME=paste0(mgt_datafram$Barn_number[mgt_datafram$HRU_number==i][1],year(Animal_df$Date[k]) %% 100,yday(Animal_df$Date[k])),
                                                                     FRT_SURFACE=format(round(0.00, 2), nsmall = 2),
                                                                     FERT_ID=nrow(mgt_datafram)+mgt_datafram$FERT_ID[1],
                                                                     FERT_APPLIED=(Max_capacity/FARMarea)*mgt_datafram$Area_ha[mgt_datafram$HRU_number==i][1])),envir=parent.frame()) 
          mgt_datafram=rbind(mgt_datafram,get(paste0("new_auto_fert",i,k)))
          rm(list=ls(pattern ="new_auto_fert*" ),envir=parent.frame())
        }
        message(paste0("Auto-fertilization on ",Animal_df$Date[k]," for farm ",j ," the rate of fertilizer application in kg/ha is set to ",(Max_capacity/FARMarea)))                                                                             
      }
      Animal_df$TotalNP_barn[k+1]=Animal_df$TotalNP_barn[k]+Animal_df$totalNP[k]-(Animal_df$Fert_applied[k]*0.007)
      Animal_df$DMmanure[k+1]=Animal_df$TotalNP_barn[k+1]/0.046
      Animal_df$stored_manure[k+1]=Animal_df$TotalNP_barn[k+1]/0.007
      Animal_df$Total_N_barn_stored[k+1]=Animal_df$Total_N_barn_stored[k] + Animal_df$total_N_barn[k+1]-(Animal_df$Total_N_barn_stored[k]/Animal_df$TotalNP_barn[k])*Animal_df$Fert_applied[k]*0.007
      Animal_df$Total_P_barn_stored[k+1] = Animal_df$Total_P_barn_stored[k] + Animal_df$total_P_barn[k+1]-(Animal_df$Total_P_barn_stored[k]/Animal_df$TotalNP_barn[k])*Animal_df$Fert_applied[k]*0.007
    }
  }
  ###################################################
  ###getting mineralized and organ N and P (daily kg)
  #############################################
  ##----------------------------eghball 2002, P------------------------------------------------------
  Animal_df$Pmin = Animal_df$total_P_barn * 0.75  #kgams mineralized P daily produced
  Animal_df$Porg = Animal_df$total_P_barn * 0.25 ##kgams organic P
  Animal_df$Pmin_stored =Animal_df$Total_P_barn_stored*0.75 #kgams of mineralzied P in stored manure in the barn
  Animal_df$Porg_stored = Animal_df$Total_P_barn_stored*0.25 #kgams of organic P in stored manure in the barn
  
  ###-----------------Van Kessel 2002, N-------------------------------------------------
  Animal_df$Nmin= Animal_df$total_N_barn * 0.4 #kgams mineralized N daily
  Animal_df$Norg= Animal_df$total_N_barn * 0.6 #kgams organic N daily 
  Animal_df$Nmin_stored =Animal_df$Total_N_barn_stored * 0.4 #kgams of mineralized N in stored manure in the barn
  Animal_df$Norg_stored = Animal_df$Total_N_barn_stored * 0.6 #kgams of organic N in stored manure in the barn
  #########################################
  ####get fractions 
  #####################################
  Animal_df$Pmin_frac=0 ## fraction of mineral P in fertilizer (dairy manure) (kg min_P/kg fertilizer)
  Animal_df$Porg_frac=0 ## fraction of organic P in fertilizer (dairy manure) (kg org-P/kg fertilizer)
  Animal_df$Norg_frac=0 ## fraction of organic N in fertilizer (dairy manure) (kg org-N/kg fertilizer)
  Animal_df$Nmin_frac=0 ## fraction mineral N in fertilizer (dairy manure) (kg min_N/kg fertilizer)
  for (k in 1:nrow(Animal_df)){
    if (Animal_df$DMmanure[k] == 0){
      Animal_df$Pmin_frac[k] = 0.
      Animal_df$Porg_frac[k] = 0.
      Animal_df$Norg_frac[k] = 0.
      Animal_df$Nmin_frac [k]= 0.	           
    }else {  
      Animal_df$Pmin_frac[k] = Animal_df$Pmin_stored[k]/Animal_df$DMmanure[k]
      Animal_df$Porg_frac[k] = Animal_df$Porg_stored[k]/Animal_df$DMmanure[k]
      Animal_df$Norg_frac[k] = Animal_df$Norg_stored[k]/Animal_df$DMmanure[k]
      Animal_df$Nmin_frac[k] = Animal_df$Nmin_stored[k]/Animal_df$DMmanure[k]
    }
  }
  assign("mgt_datafram",mgt_datafram,envir=parent.frame())
  return(Animal_df)
}

