
Sys.unsetenv("http_proxy"); Sys.unsetenv("https_proxy")

flowgage_id="04282650" #Little Otter Creek at Ferrisburg, VT.
flowgage=get_usgs_gage(flowgage_id,begin_date = "2010-01-01",end_date= "2021-12-31")
flowgage$flowdata$Qmm=(flowgage$flowdata$flow)/(flowgage$area*1000)#mm
#max(flowgage$flowdata$Qmm)
#flowgage$flowdata=subset(flowgage$flowdata, flowgage$flowdata$Qmm<18)

#dir.create("Dan_VSA_Calib") #set your working directory to txtinout file
setwd("~/FarmScenarios/TxtInOut/")

#So, this routine will enter into a VSA initialized TxtInOut folder,
#and then for each HRU, look at the first line of the .hru file to
#determine the TI Class 1-10, and then rename the each of the HRU files
#to start with t followed by the 2 character TI, 01-10, and then will
#update the sub basin files with the new file name with the TI Class in
#the front.

TItoFile=data.frame(filepre=substr(list.files(pattern="[:0:].*\\.hru",path="."),0,9))
length(TItoFile[,1])
TItoFile$tmpline=""
TItoFile$TI=""
for (i in 1:length(TItoFile$filepre)){
  #i=1
  tmpline=readLines(paste0(TItoFile[i,1],".hru"),n=1)
  TItoFile$tmpline[i]=tmpline
  TItoFile$TI[i]=sub(".*TI([0-9][0-9])A.*","t\\1",tmpline)
  filepre=TItoFile$filepre[i]
  filepost=paste0(TItoFile$TI[i],substr(filepre,4,10))
  for(swatpost in substr(list.files(pattern=TItoFile$filepre[i]),10,20)){
    print(swatpost)
    oldname=paste0(TItoFile$filepre[i],swatpost)
    print(filepre)
    print(filepost)
    print(oldname)
    newname=paste0(filepost,swatpost)
    file.rename(oldname,newname)
  }
  subbasinfile=paste0(substr(filepre,0,5),"0000.sub")
  print(subbasinfile)
  tmpfile=readLines(subbasinfile)
  tmpfile=gsub(filepre,filepost,tmpfile)
  print(tmpfile)
  writeLines(tmpfile,subbasinfile)
}

TItoFile=data.frame(filepre=substr(list.files(pattern="[:0:].*\\.hru",path="."),0,9))
length(TItoFile[,1])
TItoFile$tmpline=""
for (i in 1:length(TItoFile$filepre)){
  #i=1
  tmpline=readLines(paste0(TItoFile[i,1],".hru"),n=1)
  TItoFile$tmpline[i]=tmpline
  NN=sub(".*Luse:([*][*][*])*","\\1",tmpline)
  PP=sub("* Soil.*","\\1",NN)
  TItoFile$LU[i]=PP
}

Allandrafarm=subset(TItoFile,TItoFile$LU=="HYAF" )
Fourhillsfarm=subset(TItoFile,TItoFile$LU=="HYFF")
MonktonRdfarm=subset(TItoFile,TItoFile$LU=="HYMF")
