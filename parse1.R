dataFilename=paste(getwd(),"/data_final.csv", sep="")
mydata = read.csv(dataFilename,stringsAsFactors=FALSE) 
stockVector<-NULL;
indexVector<-NULL;
currentStockNumber<-mydata$stock_number[1];
minYear<-1966;
minMonth<-7;

count= 0 ;
#Loop over all rows
for(i in 2:nrow(mydata)-1){

  #Same asset
  if(mydata$stock_number[i]==currentStockNumber){
    
    if(count == 0 && mydata$year[i] == minYear && mydata$month[i] == minMonth){
      count = count + 1 ;
    }else if(count !=0){
      count = count + 1 ;
    }
    
    
    
  }else{
      if(count !=0){
        stockVector<- c(stockVector, count);
        indexVector<- c(indexVector, currentStockNumber);
        count = 0;
        currentStockNumber = currentStockNumber + 1 ;
      }else{
        currentStockNumber = currentStockNumber + 1 ;
        if(mydata$year[i] == minYear && mydata$month[i] == minMonth){
          count = count + 1 ;
        }
      }
      
        
      
  }
}

sortVector<-sort(stockVector)
maxDate = sortVector[length(sortVector)-99]
selectedIndex<-NULL;
for(i in 1:length(stockVector)){
  if(stockVector[i] >= maxDate){
    selectedIndex <- c(selectedIndex, indexVector[i])
  }
  
}


#print("Write in data.csv");
#write.csv(mydata,file=paste(getwd(),"/data.csv", sep=""));