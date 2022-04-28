datamatrix<- as.matrix(data)
baharhist <- function(baslik, xlabel, ylabel, xlimiti, ylimiti, renk){
  #3,39 -3,5 -- 3,51-4,0 -- 4,1-4,8
  
  limitlermatrixi<-matrix(0, nrow=5, ncol=2)
  grouplarmatrixi<-matrix(0, nrow=5, ncol=1)
  limitlermatrixi[1,1]<-16.16 
  limitlermatrixi[2,1]<-18.0
  limitlermatrixi[1,2]<-19.0
  limitlermatrixi[2,2]<-20.0
  limitlermatrixi[1,3]<-21.0
  limitlermatrixi[2,3]<-22.0
  limitlermatrixi[1,4]<-23.0
  limitlermatrixi[2,4]<-24.0
  limitlermatrixi[1,5]<-25.0
  limitlermatrixi[2,5]<-25.11
  plot(NULL, xlim=c(16,27), ylim=c(0,50), main = "Var2-Histogram", xlab = xlabel, ylab=ylabel)
  for(j in 1:100){
    if(limitlermatrixi[1,1] <=as.numeric(datamatrix[j,5]) && as.numeric(datamatrix[j,5])<= limitlermatrixi[2,1]){
      grouplarmatrixi[1,1]<-grouplarmatrixi[1,1]+1
    }
    else if (limitlermatrixi[1,2] <=as.numeric(datamatrix[j,5])&& as.numeric(datamatrix[j,5])<=limitlermatrixi[2,2]){
      grouplarmatrixi[2,1]<-grouplarmatrixi[2,1]+1
    }
    else if (limitlermatrixi[1,3] <=as.numeric(datamatrix[j,5])&& as.numeric(datamatrix[j,5])<=limitlermatrixi[2,3]){
      grouplarmatrixi[3,1]<-grouplarmatrixi[3,1]+1
    }
    else if (limitlermatrixi[1,4] <=as.numeric(datamatrix[j,5])&& as.numeric(datamatrix[j,5])<=limitlermatrixi[2,4]){
      grouplarmatrixi[4,1]<-grouplarmatrixi[4,1]+1
    }
    else if (limitlermatrixi[1,5] <=as.numeric(datamatrix[j,5])&& as.numeric(datamatrix[j,5])<=limitlermatrixi[2,5]){
      grouplarmatrixi[5,1]<-grouplarmatrixi[5,1]+1
    }
    
  }
  for(i in 1:5){ #3 sayýsý kaç group'un varsa
    yindex<-0.0
    while(yindex<=grouplarmatrixi[i,1]){
      segments(x0=limitlermatrixi[1,i],y0=yindex,x1=limitlermatrixi[2,i],y1=yindex, col=renk)
      yindex<-yindex+0.1
    }
  }
  
}

baharhist(baslik = "HÝSTOGRAM", ylabel =" y ekseni", xlabel="x ekseni", xlimiti = c(16,27), ylimiti=c(0,50), renk="Purple")




