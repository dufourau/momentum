dataFilenameFacteurs=paste(getwd(),"/facteurs.csv", sep="")
dataFacteurs = read.csv(dataFilenameFacteurs,stringsAsFactors=FALSE) 
mydata[["rf"]] = 0;
#date de début est une année
#on considère la renta arithmétique sur les 6 premiers mois donnés pour cette année
#Final result here
matrixBottom<-matrix(nrow=20, ncol=10)
matrixTop<-matrix(nrow=20, ncol=10)

firstYear= 1966;
firstMonth = 7;

#Loop for 20 year and create the best and worstportfolio
for(j in 0:19){
  renta <- NULL;
  renta_top<-NULL;
  renta_bottom<-NULL;
  renta_arithmetique<-NULL;
  indexVector<- NULL;
  indexVectorTop <- NULL;
  indexVectorBottom<- NULL;
  currentYear<- firstYear+j;
  currentMonth<- firstMonth-1;
  currentStockNumber<- 1;
  #For each asset loop over 6 months
  date = 0;
	for(i in 2:nrow(mydata)-1)
	{
    
		
		   # l'actif courant appartient aux 100 meilleurs actifs et les dates se suivent
		   if(mydata$stock_number[i] %in% selectedIndex && currentYear == mydata$year[i] )
		   {
           #First month
  		     if(mydata$month[i] == 7 ){
  		       indexVector<-c(indexVector,currentStockNumber);  
  		       currentStockNumber= mydata$stock_number[i];
             date = 0 ;
  		       
  		     }
         
           # On passe à l'actif suivant
           if(date == 6 && mydata$stock_number[i] != currentStockNumber){                
             currentStockNumber = mydata$stock_number[i];
             currentYear<- firstYear+j;
             currentMonth<- firstMonth;
             date = 0;
           }
           
           if(date<6){		        
    	       indFacteur = intersect(which(dataFacteurs$year == mydata$year[i]),which(dataFacteurs$month ==mydata$month[i]))
    	       rf= as.numeric(dataFacteurs$Risk.Free.Return[indFacteur]);
    	       mydata$rf[i] = rf;
               
      			
    				  if (!is.na(mydata$return_rf[i]))
    				  {
    					#on concatène dans le vecteur renta les rentabilités successives 
    					#les renta sont alors triées par numéros d'actifs et il y a 6 rentas par actif 
    				  	renta <- c(renta, (as.numeric(mydata$return_rf[i]) + as.numeric(mydata$rf[i])));	
    				  }else
    				  {
    					  renta <- c(renta, (as.numeric(mydata$return_rf[i-date]) + as.numeric(mydata$rf[i-1])));	
    				  } 			
				      date = date + 1 ;
    					currentYear<- mydata$year[i];
    					currentMonth<- mydata$month[i];
  		      }
    					
  		        
	    }
      		  
  }
		  
	for (actif in 1:100)
	{
	  renta_arithmetique[actif]= 1 ;
		for (mois in 1:6)
		{	
			#renta_actif[actif] contient les rentabilité de l'actif courant sur les 6 mois soit 6 valeurs
			#renta_actif[actif] <- c(renta_actif[actif],renta[(actif-1)*mois + mois]);
			#contient la renta arithmétique de l'actif courant
			renta_arithmetique[actif] = renta_arithmetique[actif] * (1 + renta[(actif-1)*6 + mois] ) ;
		}
		renta_arithmetique[actif] = renta_arithmetique[actif] - 1;

	}

	#on trie les rentabilités arithmétiques des 100 actifs
	renta_triees <- sort(renta_arithmetique);

	#on récupère les renta arithmétiques des 10 meilleurs actifs et des 10 moins bons
	for (tri in 1:10)
	{
		renta_top <- c(renta_top, renta_triees[tri+90] );
		renta_bottom <- c(renta_bottom, renta_triees[tri] );
	}

  for(i in 1:100){
    if(renta_arithmetique[i] > renta_triees[90]){
      indexVectorTop = c(indexVectorTop, indexVector[i]);
    }
    if(renta_arithmetique[i] <= renta_triees[10]){
      indexVectorBottom = c(indexVectorBottom, indexVector[i]);
    }
    
  }
  matrixBottom[j+1,] = indexVectorBottom;
  matrixTop[j+1,] = indexVectorTop;
  
  
}

  