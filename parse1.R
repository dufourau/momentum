dataFilename=paste(getwd(),"/data_final.csv", sep="")
mydata = read.csv(dataFilename,stringsAsFactors=FALSE) 
stockVector<-NULL;
currentStockNumber<-mydata$stock_number[1];
minYear<-1991;
maxYear<-1992;

currentYear<- minYear;
currentMonth<- min(mydata$month)-1;
count= 0 ;
#Loop over all rows
for(i in 2:nrow(mydata)-1){

  #Same asset
  if(mydata$stock_number[i]==currentStockNumber){
    #Check if the date are fi    
    count = count + 1 ;
    
    
  }else{
    stockVector<- c(stockVector, count);
    currentStockNumber = currentStockNumber + 1 ;
    count = 0;
  }
}
#print("Write in data.csv");
#write.csv(mydata,file=paste(getwd(),"/data.csv", sep=""));