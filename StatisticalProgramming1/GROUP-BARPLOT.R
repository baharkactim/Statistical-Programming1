Group1<-0
Group2<-0
Group3<-0
Group4<-0

for(j in 1:100){
  if(datamatrix[j,4]=="Group1"){
    Group1<-Group1+1
  }
  else if (datamatrix[j,4]=="Group2"){
    Group2<-Group2+1
  }
  else if (datamatrix[j,4]=="Group3"){
    Group3<-Group3+1
  }
  else if (datamatrix[j,4]=="Group4"){
    Group4<-Group4+1
  }
  else{
    
  }
}

plot(NULL, xlim=c(0,3), ylim=c(0,70))

text(x=1, y=Group1+2, labels = "Group1")
segments(x0=1, y0=0, x1=1, y1=Group1, lwd=10, lend=1, col="Blue")


text(x=2, y=Group2+2, labels = "Group2")
segments(x0=2, y0=0, x1=2, y1=Group2, lwd=10, lend=1, col="Red")
plot(NULL, xlim=c(0,3), ylim=c(0,70))

text(x=3, y=Group3+2, labels = "Group3")
segments(x0=1, y0=0, x1=1, y1=Group3, lwd=10, lend=1, col="Purple")


text(x=4, y=Group4+2, labels = "Group4")
segments(x0=2, y0=0, x1=2, y1=Group4, lwd=10, lend=1, col="Yellow")

