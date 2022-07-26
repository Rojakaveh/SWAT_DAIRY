setwd("~/SWAT_Dairy_New/TxtInOutfert/")

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
