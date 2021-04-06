library(readxl)
library(writexl)
library(dplyr)
library(stringr)
library(tidyr)

setwd("C:/Users/Weilun_Chiu/Documents/3PAR/iReturn")
filename<-list.files()
iReturn_Data<-do.call(rbind, lapply(filename, read.csv, header=TRUE))

myda<-iReturn_Data[, c("DISP.LOCATION", "DATE.SENT", "PART.SERNO")]
myda$DATE.SENT<-as.POSIXct(strptime(myda$DATE.SENT, "%m/%d/%Y %H:%M"))
myda<-myda %>% arrange(desc(DATE.SENT))
myda$PART.SERNO<-substr(myda$PART.SERNO, 6, 14)
myda<-myda %>% distinct(PART.SERNO, .keep_all = TRUE)
mydaF<-data.frame(myda$PART.SERNO, myda$DISP.LOCATION)
mydaF$myda.PART.SERNO<-as.character(mydaF$myda.PART.SERNO)
mydaF$myda.DISP.LOCATION<-as.character(mydaF$myda.DISP.LOCATION)

setwd("C:/Users/Weilun_Chiu/Documents/3PAR/Assy")
file<-list.files()
Assy<-read.csv(file)

wcheck<-function(SN){
        Product_Code<-substr(SN, 1, 5)
        logic<-Product_Code %in% c("PCMBU", "PDHWN", "PDSET", "PFLKQ")
        
        if(!logic){
        SN1<-substr(SN, 6, 14)
        loc<-grep(SN1, mydaF$myda.PART.SERNO)
        warranty<-mydaF$myda.DISP.LOCATION[loc]
        final_w<-ifelse(warranty=="RT23", "IW", "OOW")
        if(length(final_w)<1){
                data.frame(Serial_Number=SN, Node_SN="NA", Warranty="No Record Found")
        }
        else{ 
                data.frame(Serial_Number=SN, Node_SN="NA", Warranty=final_w)
        }
  
        }
        else{
             new_SN<-Assy$SPS.[loc<-which(Assy$Assy.==SN)]
             loc<-which(mydaF$myda.PART.SERNO==new_SN)
             warranty<-mydaF$myda.DISP.LOCATION[loc]
             final_w<-ifelse(warranty=="RT23", "IW", "OOW")
        
        if(length(new_SN)<1){
                data.frame(Serial_Number=SN, Node_SN="No Record Found", Warranty="No Record Found")
        }else if(length(final_w)<1){
                data.frame(Serial_Number=SN, Node_SN=new_SN, Warranty="No Record Found")
        }else{
                data.frame(Serial_Number=SN, Node_SN=new_SN, Warranty=final_w)
        }
        }
}




