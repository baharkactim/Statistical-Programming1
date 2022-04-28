datamatrix<- as.matrix(data)
baharhist <- function(baslik, xlabel, ylabel, xlimiti, ylimiti, renk){

  limitlermatrixi<-matrix(0, nrow=2, ncol=3)
  grouplarmatrixi<-matrix(0, nrow=3, ncol=1)
  limitlermatrixi[1,1]<-3.39
  limitlermatrixi[2,1]<-3.5
  limitlermatrixi[1,2]<-3.5
  limitlermatrixi[2,2]<-4.0
  limitlermatrixi[1,3]<-4.0
  limitlermatrixi[2,3]<-4.8
  plot(NULL, xlim=c(3,5), ylim=c(0,70), main = "Var1-Histogram", xlab = xlabel, ylab=ylabel)
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
      yindex<-yindex+0.01
    }
  }

}

baharhist(baslik = "HÝSTOGRAM ", ylabel = "y ekseni", xlabel="x ekseni", xlimiti = c(0,3), ylimiti=c(0,100), renk="Purple")
  
  
  
 
  