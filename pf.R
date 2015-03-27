
dataFilenameFacteurs=paste(getwd(),"/facteurs.csv", sep="")
dataFacteurs = read.csv(dataFilenameFacteurs,stringsAsFactors=FALSE) 
currentStockNumber<-mydata$stock_number[1];
currentYear<- mydata$year[1];
currentMonth<- mydata$month[1];
mydata[["rf"]] = 0;
#Récurépération des taux sans risque à chacun des mois de chaque année
#for(i in 2:nrow(mydata)-1)
#{ 
#  indFacteur = intersect(which(dataFacteurs$year ==currentYear),which(dataFacteurs$month ==currentMonth))
#  rf= as.numeric(dataFacteurs$Risk.Free.Return[indFacteur]);
#  mydata$rf[i] = rf;

#  currentYear<- mydata$year[i];
#  currentMonth<- mydata$month[i];
#}

#date de début est une année
#on considère la renta arithmétique sur les 6 premiers mois donnés pour cette année
firstYear= 1966;
firstMonth = 7;
renta <- NULL;
renta_top<-NULL;
renta_bottom<-NULL;
renta_arithmetique<-NULL;
currentYear<- firstYear;
currentMonth<- firstMonth;
currentStockNumber<- 1;
indexVector<- NULL;
indexVectorTop <- NULL;
indexVectorBottom<- NULL;
	date = 0;
	for(i in 2:nrow(mydata)-1)
	{ 
		
		   # l'actif courant appartient aux 100 meilleurs actifs
		   if(mydata$stock_number[i] %in% selectedIndex &&((currentYear == mydata$year[i]-1 && currentMonth == 12) 
		      || (currentYear == mydata$year[i] && currentMonth == mydata$month[i]-1)))
		   {
           
           if(date == 6 && mydata$stock_number[i] != currentStockNumber){                
             currentStockNumber = mydata$stock_number[i];
             date = 0;
           }
           if(date == 0){
             indexVector<-c(indexVector,currentStockNumber);  
             currentStockNumber= mydata$stock_number[i];
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
  		        }
  		        
		        }
      		  currentYear<- mydata$year[i];
      		  currentMonth<- mydata$month[i];
	
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

	#on crée le premier pf avec les actifs aux renta les plus fortes
	portefeuille1 <- 0.1 * (sum(renta_top));

	#on crée le premier pf avec les actifs aux renta les plus faibles
	portefeuille10 <- 0.1 * (sum(renta_bottom));
  