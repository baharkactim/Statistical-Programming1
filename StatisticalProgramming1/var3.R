datamatrix<- as.matrix(data)
baharhist <- function(baslik, xlabel, ylabel, xlimiti, ylimiti, renk){
  #3,39 -3,5 -- 3,51-4,0 -- 4,1-4,8
  
  limitlermatrixi<-matrix(0, nrow=2, ncol=7)
  grouplarmatrixi<-matrix(0, nrow=7, ncol=1)
  limitlermatrixi[1,1]<-54.84 
  limitlermatrixi[2,1]<-56.0
  limitlermatrixi[1,2]<-56.01
  limitlermatrixi[2,2]<-58.0
  limitlermatrixi[1,3]<-58.01
  limitlermatrixi[2,3]<-60.0
  limitlermatrixi[1,4]<-60.01
  limitlermatrixi[2,4]<-62.0
  limitlermatrixi[1,5]<-62.01
  limitlermatrixi[2,5]<-64.0
  limitlermatrixi[1,6]<-64.01
  limitlermatrixi[2,6]<-66.0
  limitlermatrixi[1,7]<-66.01
  limitlermatrixi[2,7]<-67.21
  plot(NULL, xlim=c(50,70), ylim=c(0,100), main = "Var3-Histogram", xlab = xlabel, ylab=ylabel)
  for(j in 1:100){
    if(limitlermatrixi[1,1] <=as.numeric(datamatrix[j,7]) && as.numeric(datamatrix[j,7])<= limitlermatrixi[2,1]){
      grouplarmatrixi[1,1]<-grouplarmatrixi[1,1]+1
    }
    else if (limitlermatrixi[1,2] <=as.numeric(datamatrix[j,7])&& as.numeric(datamatrix[j,7])<=limitlermatrixi[2,2]){
      grouplarmatrixi[2,1]<-grouplarmatrixi[2,1]+1
    }
    else if (limitlermatrixi[1,3] <=as.numeric(datamatrix[j,7])&& as.numeric(datamatrix[j,7])<=limitlermatrixi[2,3]){
      grouplarmatrixi[3,1]<-grouplarmatrixi[3,1]+1
    }
    else if (limitlermatrixi[1,4] <=as.numeric(datamatrix[j,7])&& as.numeric(datamatrix[j,7])<=limitlermatrixi[2,4]){
      grouplarmatrixi[4,1]<-grouplarmatrixi[4,1]+1
    }
    else if (limitlermatrixi[1,5] <=as.numeric(datamatrix[j,7])&& as.numeric(datamatrix[j,7])<=limitlermatrixi[2,5]){
      grouplarmatrixi[5,1]<-grouplarmatrixi[5,1]+1
    }
    else if (limitlermatrixi[1,6] <=as.numeric(datamatrix[j,7])&& as.numeric(datamatrix[j,7])<=limitlermatrixi[2,4]){
      grouplarmatrixi[6,1]<-grouplarmatrixi[6,1]+1
    }
    else if (limitlermatrixi[1,7] <=as.numeric(datamatrix[j,7])&& as.numeric(datamatrix[j,7])<=limitlermatrixi[2,5]){
      grouplarmatrixi[7,1]<-grouplarmatrixi[7,1]+1
    }
    else{
      
    }
    
  }
  for(i in 1:7){ #3 sayýsý kaç group'un varsa
    yindex<-0.0
    while(yindex<=grouplarmatrixi[i,1]){
      segments(x0=limitlermatrixi[1,i],y0=yindex,x1=limitlermatrixi[2,i],y1=yindex, col=renk)
      yindex<-yindex+0.01
    }
  }
  
}

baharhist(baslik = "HÝSTOGRAM ", ylabel = "y ekseni", xlabel="x ekseni", xlimiti = c(50,70), ylimiti=c(0,100), renk="Purple")




