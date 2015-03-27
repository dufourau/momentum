dataFilename=paste(getwd(),"/data_final.csv", sep="")
mydata = read.csv(dataFilename,stringsAsFactors=FALSE) 
stockVector<-NULL;
currentStockNumber<-mydata$stock_number[1];
minYear<-1991;
maxYear<-1992;

currentYear<- minYear;
currentMonth<- min(mydata$month)-1;

#Loop over all rows
for(i in 2:nrow(mydata)-1){
  
  #Same asset
  if(mydata$stock_number[i]==currentStockNumber){
    #Check if the date are fi
    if((currentYear == mydata$year[i]-1 && currentMonth == 12) 
       || (currentYear == mydata$year[i] && currentMonth == mydata$month[i]-1) ){
      #Final date
      if(currentYear == maxYear && currentMonth == max(mydata$month)){

        stockVector = c(stockVector, currentStockNumber);
        currentYear<- minYear;
        currentMonth<- min(mydata$month);
        currentStockNumber = currentStockNumber + 1;
        
      }else{
        currentYear<- mydata$year[i];
        currentMonth<- mydata$month[i];
      }
    }else{
      currentYear<- minYear;
      currentMonth<- min(mydata$month)-1;
      currentStockNumber = currentStockNumber + 1;
      
    }
 
  }else{
    if(mydata$stock_number[i]>currentStockNumber){
      currentStockNumber = currentStockNumber + 1;
    }
    
    
    
  }
}
#print("Write in data.csv");
#write.csv(mydata,file=paste(getwd(),"/data.csv", sep=""));