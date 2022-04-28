datamatrix<- as.matrix(data)
baharhist <- function(baslik, xlabel, ylabel, xlimiti, ylimiti, renk){
  #3,39 -3,5 -- 3,51-4,0 -- 4,1-4,8
  
  limitlermatrixi<-matrix(0, nrow=2, ncol=3)
  grouplarmatrixi<-matrix(0, nrow=3, ncol=1)
  limitlermatrixi[1,1]<-65.36
  limitlermatrixi[2,1]<-70.0
  limitlermatrixi[1,2]<-70.01
  limitlermatrixi[2,2]<-75.0
  limitlermatrixi[1,3]<-75.01
  limitlermatrixi[2,3]<-79.78
  
  plot(NULL, xlim=c(64,80), ylim=c(0,70), main = "Var6-Histogram", xlab = xlabel, ylab=ylabel)
  for(j in 1:100){
    if(limitlermatrixi[1,1] <=as.numeric(datamatrix[j,3]) && as.numeric(datamatrix[j,3])<= limitlermatrixi[2,1]){
      grouplarmatrixi[1,1]<-grouplarmatrixi[1,1]+1
    }
    else if (limitlermatrixi[1,2] <=as.numeric(datamatrix[j,3])&& as.numeric(datamatrix[j,3])<=limitlermatrixi[2,2]){
      grouplarmatrixi[2,1]<-grouplarmatrixi[2,1]+1
    }
    else if (limitlermatrixi[1,3] <=as.numeric(datamatrix[j,3])&& as.numeric(datamatrix[j,3])<=limitlermatrixi[2,3]){
      grouplarmatrixi[3,1]<-grouplarmatrixi[3,1]+1
    }
    else{
      
    }
  }
  for(i in 1:3){ #3 sayýsý kaç group'un varsa
    yindex<-0.0
    while(yindex<=grouplarmatrixi[i,1]){
      segments(x0=limitlermatrixi[1,i],y0=yindex,x1=limitlermatrixi[2,i],y1=yindex, col=renk)
      yindex<-yindex+0.1
    }
  }
  
}

baharhist(baslik = "HÝSTOGRAM", ylabel =" y ekseni", xlabel="x ekseni", xlimiti = c(64,80), ylimiti=c(0,70), renk="Purple")




