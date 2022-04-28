#(171805076-Bahar KAÇTIM)(171805057-FURKAN Gümrükçü)(191805075-Ali biçici)
data <- as.matrix(read.table(file.choose(), header=TRUE, stringsAsFactors = FALSE, sep = " "))

ikisinidebiliyorsam<-function(varnumber, group, gender){
  mtrx<-matrix(0, nrow=1, ncol=1)
  groupstring<-paste("Group",group,sep="")
  if(gender=="m"&& !is.na(gender)){
    genderstring<-"Male"
  }
  else if(gender=="f"&& !is.na(gender)){
    genderstring<-"Female"
  }
  else{
  }

  for(i in 1:100){
    if(data[i,2]==groupstring && data[i,3]==genderstring){
      mtrx<-rbind(mtrx, data[i, varnumber+3])
    }
  }
  
  stablematrix<-matrix(0, nrow=length(mtrx)-1, ncol=1)
  k<-1
  for(i in 2:length(mtrx)){
    stablematrix[k]<-as.numeric(mtrx[i])
    k<-k+1
  }
  return(stablematrix)
}

grububiliyorum<-function(varnumber, group){
  mtrx<-matrix(0, nrow=1, ncol=1)
  groupstring<-paste("Group",group,sep="")
  
  for(i in 1:100){
    if(data[i,2]==groupstring){
      mtrx<-rbind(mtrx, data[i, varnumber+3])
    }
  }
  
  stablematrix<-matrix(0, nrow=length(mtrx)-1, ncol=1)
  k<-1
  for(i in 2:length(mtrx)){
    stablematrix[k]<-as.numeric(mtrx[i])
    k<-k+1
  }
  return(stablematrix)
}

cinsiyetibiliyorum<-function(varnumber, gender){
  mtrx<-matrix(0, nrow=1, ncol=1)
  if(gender=="m" && !is.na(gender)){
    genderstring<-"Male"
  }
  else if(gender=="f" && !is.na(gender)){
    genderstring<-"Female"
  }
  else{
  }
  for(i in 1:100){
    if(data[i,3]==genderstring){
      mtrx<-rbind(mtrx, data[i, varnumber+3])
    }
  }
  
  stablematrix<-matrix(0, nrow=length(mtrx)-1, ncol=1)
  k<-1
  for(i in 2:length(mtrx)){
    stablematrix[k]<-as.numeric(mtrx[i])
    k<-k+1
  }
  return(stablematrix)
}

ikisinidebilmiyorum <- function(varnumber){ #O filtrelemem yok
  mtrx<-as.numeric(data[,varnumber+3])
  return(mtrx)
}

matrix_creator<-function(varnumber, group=NA, gender=NA){
  if(is.na(group) && is.na(gender)){ # ikisini de bilmiyosam
    mtrx<-ikisinidebilmiyorum(varnumber)
  }
  else if(!is.na(group) && is.na(gender)){ # grubu biliyorsam, cinsiyeti bilmiyorsam
    mtrx<-grububiliyorum(varnumber, group)
  }
  else if(is.na(group) && !is.na(gender)){ # grubu bilmiyorsam, cinsiyeti biliyorsam
    mtrx<-cinsiyetibiliyorum(varnumber, gender)
  }
  else{ # ikisini de biliyorsam
    mtrx<-ikisinidebiliyorsam(varnumber, group, gender)
  }
  return(mtrx)
}

#Number of observation 
number_of_observation<-function(varnumber, group=NA, gender=NA){
  matrix<-matrix_creator(varnumber, group, gender)
  na<-0
  for(i in matrix){
    if(is.na(i)){
      na<-na+1
    }
  }
  return(length(matrix)-na)
}


minimum<-function(varnumber, group=NA, gender=NA){ #minumum
  matrix<-matrix_creator(varnumber, group, gender)
  sortedMatrix<-sort(matrix)
  minimumEl<-sortedMatrix[1]
  return(minimumEl)
}

maximum<-function(varnumber, group=NA, gender=NA){ #maximum
  matrix<-matrix_creator(varnumber, group, gender)
  sortedMatrix<-sort(matrix)
  maximumEl<-sortedMatrix[length(sortedMatrix)]
  return(maximumEl)
}


sum<-function(varnumber, group=NA, gender=NA){ #sum
  matrix<-deleteNA(matrix_creator(varnumber, group, gender))
  sum<-0
  for(i in matrix){
    sum<-sum+i
  }
  return(sum)
} 


######

mean<-function(varnumber, group=NA, gender=NA){
  sum<-sum(varnumber, group, gender)
  mean<-(sum/number_of_observation(varnumber, group, gender))
  return(mean)
}

median<-function(varnumber, group=NA, gender=NA){ #median
  matrix<-matrix_creator(varnumber, group, gender)
  sortedMatrix<-sort(matrix)
  len<-length(sortedMatrix)
  if(len%%2==0){
    return((sortedMatrix[len/2]+sortedMatrix[len/2+1])/2)
  }
  else{
    return(sortedMatrix[(len+1)/2])
  }
}

rangee<-function(varnumber, group=NA, gender=NA){ #range
  min<-minimum(varnumber=varnumber)
  max<-maximum(varnumber=varnumber)
  rangex<-c(min,max)
  return(rangex)
} 

##
sumOfSquares<-function(varnumber, group=NA, gender=NA){ #sum of squares
  matrix<-na2zero(array=matrix_creator(varnumber, group, gender))
  sos<-0
  for(i in matrix){
    sos<-sos+i*i
  }
  return(sos)
}

  
na2zero<-function(array){ 
  for(i in 1:length(array)){
    if(is.na(array[i])){
      array[i]<-0
    }
  }
  return(array)
}

deleteNA<-function(array){
  NAs<-0
  for(i in array){ # toplam NA sayýsýný buluyoruz
    if(is.na(i)){ # i degeri na olursa
      NAs<-NAs+1
    }
  }
  bosmatrix<-matrix(0, nrow=length(array)-NAs, ncol=1)
  k<-1
  for(i in 1:length(array)){
    if(!is.na(array[i])){ # NA olmadýðýnda
      bosmatrix[k]<-array[i]
      k<-k+1
    }
  }
  return(bosmatrix)
}

#standard deviation
standardd<-function(varnumber, group=NA, gender=NA){
  varyans<-variance(varnumber, group, gender)
  standart_deviation<-varyans**0.5
  return(standart_deviation)
}

variance<-function(varnumber, group=NA, gender=NA){
  matrix<-deleteNA(matrix_creator(varnumber, group, gender))
  mean<-mean(varnumber, group, gender)
  sum<-0
  for(i in matrix){
    sum<-sum+((i-mean)*(i-mean))
  }
  return(sum/(length(matrix)-1))
}
  
cross<-function(varnumber1,varnumber2, group=NA, gender=NA){ #cross-product
  matrix1<-na2zero(matrix_creator(varnumber1, group, gender))
  matrix2<-na2zero(matrix_creator(varnumber2, group, gender))
  mean<-mean(varnumber1, group, gender)
  mean<-mean(varnumber2, group, gender)
  sum<-0
  for (i in 1:length(matrix1)){
    sum<-sum+((matrix1[i]-mean)*(matrix2[i]-mean))
  }
  return(sum)
}

  
covariance<-function(varnumber1,varnumber2, group=NA, gender=NA){ #covariance
  matrix1<-na2zero(matrix_creator(varnumber1, group, gender))
  matrix2<-na2zero(matrix_creator(varnumber2, group, gender))
  mean<-mean(varnumber1, group, gender)
  mean<-mean(varnumber2, group, gender)
  sum<-0
  for (i in 1:length(matrix1)){
    sum<-sum+((matrix1[i]-mean)*(matrix2[i]-mean))
  }
  return(sum/(length(matrix1)-1))
}
  

#(171805076-Bahar Kaçtým)(191805075-Ali Biçici)(171805057-Furkan Gümrükçü)

corelation<-function(varnumber1,varnumber2, group=NA, gender=NA){ #correlation
  matrix1<-na2zero(matrix_creator(varnumber1, group, gender))
  matrix2<-na2zero(matrix_creator(varnumber2, group, gender))
  mean<-mean(varnumber1, group, gender)
  mean<-mean(varnumber2, group, gender)
  standardd<-standardd(varnumber1, group, gender)
  standardd<-standardd(varnumber2, group, gender)
  sum<-0
    for (i in 1:length(matrix1)){
      sum<-sum+((matrix1[i]-mean)/standardd)*((matrix2[i]-mean)/standardd)
    }
  return(sum/(length(matrix1)-1))
}



scattr<-function(varnumber1,varnumber2, group=NA, gender=NA){
 mtrx1<-matrix_creator(varnumber1, group, gender)
 mtrx2<-matrix_creator(varnumber2, group, gender)
 if(length(mtrx1)==length(mtrx2)){
   rangee1<-rangee(varnumber1, group, gender)
   rangee2<-rangee(varnumber2, group, gender)
   par(mfrow=c(1,1))
   plot(NULL,xlim =rangee1,ylim = rangee2,xlab =paste("var",varnumber1,sep=""),ylab =paste("var",varnumber2,sep="") )
   for(i in 1:length(mtrx1)){
     points(mtrx1[i],mtrx2[i])
   }
 }
}
 
scale2<-function(varnumber, group=NA, gender=NA){
  matrix<-matrix_creator(varnumber, group, gender)
  crossproduct<-cross(varnumber1=varnumber, varnumber2=varnumber, group=group, gender=gender)
  return(sqrt(crossproduct/(length(deleteNA(matrix))-1)))
}

 sorgu1<-function(){
     
       cat("
       1-number of observations
       2-minimum
       3-maximum
       4-sum
       5-mean
       ")
     secim<-as.numeric(readline(prompt="birini seç: "))
     if(secim==1){
         input<-as.numeric(readline(prompt="enter varnumber: "))
         number_of_observation(varnumber = input)
       }
     else if(secim==2){
         input<-as.numeric(readline(prompt="enter varnumber: "))
         minimum(varnumber = input)
         
         }
     else if(secim==3){
         input<-as.numeric(readline(prompt="enter varnumber: "))
         maximum(varnumber = input)
       }
     else if(secim==4){
         input<-as.numeric(readline(prompt="enter varnumber: "))
         sum(varnumber = input)
       }
     else if(secim==5){
         input<-as.numeric(readline(prompt="enter varnumber: "))
         mean(varnumber = input)
       }

     else{
       
     }
}
sorgu2<-function(){
  
cat("  6-median
       7-range
       8-sum Of Squares
       9-standard deviation
       10-variance
       ")
  secim<-as.numeric(readline(prompt="birini seç: "))
     if(secim==6){
         input<-as.numeric(readline(prompt="enter varnumber: "))
         median(varnumber = input)
       }
     else if(secim==7){
         input<-as.numeric(readline(prompt="enter varnumber: "))
         rangee(varnumber = input)
       }
     else if(secim==8){
         input<-as.numeric(readline(prompt="enter varnumber: "))
         sumOfSquares(varnumber = input)
       }
     else if(secim==9){
         input<-as.numeric(readline(prompt="enter varnumber: "))
         standardd(varnumber = input)
       }
     else if(secim==10){
        input<-as.numeric(readline(prompt="enter varnumber: "))
         variance(varnumber = input)
     }
     else{
       
     }
}
sorgu3<-function(){
  
  cat("11-cross-product
      12-covariance
      13-correlation
      14-scatter-plot
      15-scale")
  secim<-as.numeric(readline(prompt="birini seç: "))
  if(secim==11){
    input1<-as.numeric(readline(prompt="enter varnumber: "))
    input2<-as.numeric(readline(prompt="enter varnumber: "))
    cross(varnumber1 =input1 ,varnumber2 =input2)
  }
  else if(secim==12){
    input1<-as.numeric(readline(prompt="enter varnumber: "))
    input2<-as.numeric(readline(prompt="enter varnumber: "))
    covariance(varnumber1 =input1 ,varnumber2 =input2)
  }
  else if(secim==13){
    input1<-as.numeric(readline(prompt="enter varnumber: "))
    input2<-as.numeric(readline(prompt="enter varnumber: "))
    corelation(varnumber1 =input1 ,varnumber2 =input2)
  }
  else if(secim==14){
    input1<-as.numeric(readline(prompt="enter varnumber: "))
    input2<-as.numeric(readline(prompt="enter varnumber: "))
    scattr(varnumber1 =input1 ,varnumber2 =input2)
  }
  else if(secim==15){
    input1<-as.numeric(readline(prompt="enter varnumber: "))
    scale2(varnumber = input1 )
  }
  else{
    
  }
}