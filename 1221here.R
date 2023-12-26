library(data.table)
one=data.table(a=1:6,b=7:12,c=13:18)
one
one[,2]<-c(0,0,0,0,0,0)
one

changefrom=list(1,2,3,4,5,6)
changeto=list(-1,-2,-3,-4,-5,-6)
