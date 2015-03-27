#fonction qui donne les rentabilit?s des 10 actifs pour l'ann?e - matrice 10*12
compute_rent<-function(mydata,pf,debut,rf){
  RES<-matrix(nrow=10, ncol=12); #matrice 10*12 actif*mois 
  nb<-matrix(nrow=2,ncol=10); #pour mettre le rang des actifs
  for (i in 1:10){
   nb[1,i]<-pf[i]; 
   nb[2,i]<-i;
  }
  nbb<-0;
  #Loop over all rows
  for(i in 2:nrow(mydata)-1){
    if(!(is.na(mydata$year[i])) && !(is.na(mydata$month[i])) && mydata$stock_number[i] %in% pf){
      if(debut == mydata$year[i]){
        for (j in 1:10){
          if (nb[1,j] == mydata$stock_number[i]){
            nbb<-nb[2,j];
          }
        }
        RES[nbb,mydata$month[i]]<-as.numeric(mydata$return_rf[i]) + rf[mydata$month[i]];
      }
    }
  }
  return(RES)
}

#fonction qui donne les rentabilit?s des 10 actifs pour l'ann?e - matrice 10*12
compute_rent_2pf<-function(pf1, pf10){
  RES<-matrix(nrow=10, ncol=12); #matrice 10*12 actif*mois 
  RES<-pf1-pf10;
  return(RES)
}

#fonction qui donne les beta des 10 actifs pour l'ann?e - matrice 10*12
compute_beta<-function(mydata,pf,debut){
  RES<-matrix(nrow=10, ncol=12); #matrice 10*12 actif*mois 
  nb<-matrix(nrow=2,ncol=10); #pour mettre le rang des actifs
  for (i in 1:10){
    nb[1,i]<-pf[i]; 
    nb[2,i]<-i;
  }
  nbb<-0;
  #Loop over all rows
  for(i in 2:nrow(mydata)-1){
    if(!(is.na(mydata$year[i])) && !(is.na(mydata$month[i])) && mydata$stock_number[i] %in% pf){
      if(debut == mydata$year[i]){
        for (j in 1:10){
          if (nb[1,j] == mydata$stock_number[i]){
            nbb<-nb[2,j];
          }
        }
        RES[nbb,mydata$month[i]]<-mydata$beta[i];
      }
    }
  }
  return(RES)
}

#fonction qui donne les rm-rf des 10 actifs pour l'ann?e - matrice 10*12
compute_rmrf<-function(mydata,pf,debut){
  RES<-matrix(nrow=10, ncol=12); #matrice 10*12 actif*mois 
  nb<-matrix(nrow=2,ncol=10); #pour mettre le rang des actifs
  for (i in 1:10){
    nb[1,i]<-pf[i]; 
    nb[2,i]<-i;
  }
  nbb<-0;
  #Loop over all rows
  for(i in 2:nrow(mydata)-1){
    if(!(is.na(mydata$year[i])) && !(is.na(mydata$month[i])) && mydata$stock_number[i] %in% pf){
      for (j in 1:10){
        if (nb[1,j] == mydata$stock_number[i]){
          nbb<-nb[2,j];
        }
      }
      if(debut == mydata$year[i]){
        RES[nbb,mydata$month[i]]<-as.numeric(mydata$return_rf[i]);
      }
    }
  }
  return(RES)
}

#fonction qui donne le log(btm) des 10 actifs pour l'ann?e - matrice 10*12
compute_logbtm<-function(mydata,pf,debut){
  RES<-matrix(nrow=10, ncol=12); #matrice 10*12 actif*mois 
  #Loop over all rows
  for(i in 2:nrow(mydata)-1){
    if(!(is.na(mydata$year[i])) && !(is.na(mydata$month[i])) && mydata$stock_number[i] %in% pf){
      if(debut == mydata$year[i]){
        RES[mydata$stock_number[i],mydata$month[i]]<-as.numeric(mydata$log_btm[i]);
      }
    }
  }
  return(RES)
}

#fonction qui donne le AIM des 10 actifs pour l'ann?e - matrice 10*12
compute_aim<-function(mydata,pf,debut){
  RES<-matrix(nrow=10, ncol=12); #matrice 10*12 actif*mois 
  #Loop over all rows
  for(i in 2:nrow(mydata)-1){
    if(!(is.na(mydata$year[i])) && !(is.na(mydata$month[i])) && mydata$stock_number[i] %in% pf){
      if(debut == mydata$year[i]){
        RES[mydata$stock_number[i],mydata$month[i]]<-as.numeric(mydata$AIM[i]);
      }
    }
  }
  return(RES)
}

#fonction qui donne les RF mensuels pour une ann?e
compute_rf<-function(myrf, debut){
  res<-c(0,0,0,0,0,0,0,0,0,0,0,0);
  #Loop over all rows
  for(i in 2:nrow(myrf)-1){
    if(!(is.na(myrf$year[i])) && debut == myrf$year[i]){
      res[myrf$month[i]]<-as.numeric(myrf$Risk.Free.Return[i]);
    }  
  }
  return(res)
}

#fonction qui calcule les moyennes mensuelle de mat
compute_moy_mat<-function(mat){
  rent<-c(0,0,0,0,0,0,0,0,0,0,0,0);
  #Loop over all rows
  for(l in 1:10){
    for(c in 1:12){
      if(is.na(mat[l,c])){ 
      }else{
        rent[l]<-rent[l]+0.1*mat[l,c];
      } 
    }
  }
  return(rent)
}


#calcul le ratio de sharpe pour un pf P (liste de 10 indices d'actifs) et une ann?e (debut)
sharpe<-function( debut, P,rf){
  sharpe<-0
  rent_port_an<-compute_moy_mat(P)
  moy_rent_pf<-mean(rent_port_an)
  sigma_pf_an<-sd(rent_port_an)
  moy_rf<-mean(rf)
  sharpe<-(moy_rent_pf-moy_rf)/sigma_pf_an
  return(sharpe)
}

#calcul le ratio de treynor pour un pf P (liste de 10 indices d'actifs) et une ann?e (debut)
treynor<-function(mydata, debut, P1, Pmat,rf, P10){
  treynor<-0 
  moy_rf<-mean(rf)
  if(missing(P10)){
    rent_port_an<-compute_moy_mat(Pmat) #donne les rentabilit?s du pf pour les 12 mois
    moy_rent_pf<-mean(rent_port_an)
    mat_beta<-compute_beta(mydata,P1,debut) #matrice 10*12 des betas des actifs i pour les 12 mois de l'ann?e
  } else {
    P10_mat<-compute_rent(mydata,P10,debut,rf)
    P10P1_mat<-compute_rent_2pf(Pmat, P10_mat)
    rent_port_an<-compute_moy_mat(P10P1_mat) #donne les rentabilit?s du pf pour les 12 mois
    moy_rent_pf<-mean(rent_port_an)
    mat_beta1<-compute_beta(mydata,P1,debut) #matrice 10*12 des betas des actifs i pour les 12 mois de l'ann?e
    mat_beta10<-compute_beta(mydata,P10,debut) #matrice 10*12 des betas des actifs i pour les 12 mois de l'ann?e
    mat_beta<-mat_beta10-mat_beta1
  }
  beta_port_an<-compute_moy_mat(mat_beta)
  moy_beta<-mean(beta_port_an)
  treynor<-(moy_rent_pf-moy_rf)/moy_beta
  return(treynor)
}

fooBar <- function(x,y){
  if(missing(y)) {
    x
  } else {
    x + y
  }
}


#MAIN
#dataFilename=paste("Z:/Documents/Cours-3A/GestionPF/data.csv", sep="")
#mydata = read.csv(dataFilename,stringsAsFactors=FALSE) 
#dataFilename=paste("Z:/Documents/Cours-3A/GestionPF/facteurs.csv", sep="")
#myrf= read.csv(dataFilename,stringsAsFactors=FALSE) 
debut<-1967;
#vecteur des winners  indexVectorTop
#vecteur des loosers indexVectorBottom;
rf<-compute_rf(dataFacteurs, debut) #compute les rf mensuels de l'ann?e
P1_mat<-compute_rent(mydata,indexVectorTop,debut,rf) #donne les rentabilit?s des 10 actifs pour l'ann?e matrice 10*12
#P10_mat<-compute_rent(mydata,indexVectorBottom,debut,rf) #donne les rentabilit?s des 10 actifs pour l'ann?e matrice 10*12
#P10P1_mat<-compute_rent_2pf(P1_mat, P10_mat) #donne les rentabilit?s des 10 actifs pour l'ann?e matrice 10*12
#sharpe_<-sharpe(debut, P10_mat,rf)
treynor_<-treynor(mydata, debut,indexVectorTop, P1_mat,rf) #treynor pour P1
#treynor_<-treynor(mydata, debut,indexVectorTop, P1_mat,rf, P10) #treynor pour P1-P10