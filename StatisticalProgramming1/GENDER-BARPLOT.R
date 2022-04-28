datamatrix <- as.matrix(data)


male<-0
female<-0
for(i in 1:100){
  if(datamatrix[i,2]=="Male"){
    male<-male+1
  }
  else if (datamatrix[i,2]=="Female"){
    female<-female+1
  }
  else{
    
  }
}

plot(NULL, xlim=c(0,100), ylim=c(0,70))

text(x=1, y=male+2, labels = "male")
segments(x0=1, y0=0, x1=1, y1=male, lwd=10, lend=1, col="Blue")


text(x=2, y=female+2, labels = "female")
segments(x0=2, y0=0, x1=2, y1=female, lwd=10, lend=1, col="Red")

