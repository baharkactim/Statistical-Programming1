datamatrix<- as.matrix(data)
baharhist <- function(baslik, xlabel, ylabel, xlimiti, ylimiti, renk){
  #3,39 -3,5 -- 3,51-4,0 -- 4,1-4,8
  
  limitlermatrixi<-matrix(0, nrow=2, ncol=4)
  grouplarmatrixi<-matrix(0, nrow=4, ncol=1)
  limitlermatrixi[1,1]<-13.89
  limitlermatrixi[2,1]<-15.0
  limitlermatrixi[1,2]<-15.01
  limitlermatrixi[2,2]<-16.0
  limitlermatrixi[1,3]<-16.01
  limitlermatrixi[2,3]<-17.0
  limitlermatrixi[1,4]<-17.01
  limitlermatrixi[2,4]<-18.03
  
  plot(NULL, xlim=c(13,19), ylim=c(0,100), main = "Var8-Histogram", xlab = xlabel, ylab=ylabel)
  for(j in 1:100){
    if(limitlermatrixi[1,1] <=as.numeric(datamatrix[j,4]) && as.numeric(datamatrix[j,4])<= limitlermatrixi[2,1]){
      grouplarmatrixi[1,1]<-grouplarmatrixi[1,1]+1
    }
    else if (limitlermatrixi[1,2] <=as.numeric(datamatrix[j,4])&& as.numeric(datamatrix[j,4])<=limitlermatrixi[2,2]){
      grouplarmatrixi[2,1]<-grouplarmatrixi[2,1]+1
    }
    else if (limitlermatrixi[1,3] <=as.numeric(datamatrix[j,4])&& as.numeric(datamatrix[j,4])<=limitlermatrixi[2,3]){
      grouplarmatrixi[3,1]<-grouplarmatrixi[3,1]+1
    }
    else if (limitlermatrixi[1,4] <=as.numeric(datamatrix[j,4])&& as.numeric(datamatrix[j,4])<=limitlermatrixi[2,4]){
      grouplarmatrixi[4,1]<-grouplarmatrixi[4,1]+1
    }
    else{
      
    }
  }
  for(i in 1:4){ #3 sayýsý kaç group'un varsa
    yindex<-0.0
    while(yindex<=grouplarmatrixi[i,1]){
      segments(x0=limitlermatrixi[1,i],y0=yindex,x1=limitlermatrixi[2,i],y1=yindex, col=renk)
      yindex<-yindex+0.1
    }
  }
  
}

baharhist(baslik = "HÝSTOGRAM", ylabel =" y ekseni", xlabel="x ekseni", xlimiti = c(13,19), ylimiti=c(0,100), renk="Purple")




