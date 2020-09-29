source("1.2L-VectorLibrary.R")  #for dot and cross products
library("pracma") #for rref()

w1<-c(1,2,3,-6)
w2<-c(3,-2,2,-3)
w3<-c(3,1,-2,-2)
y<-c(1,1,-2,0)
A<-cbind(w1,w2,w3,y)
rref(A)

v1<-w1/Norm(w1)
x<-w2-(w2%.%v1)*v1
v2<-x/Norm(x)
x_<-w3-(w3%.%v1)*v1-(w3%.%v2)*v2
v3<-x_/Norm(x_)
B<-cbind(v1,v2,v3,y); B
rref(B)

#2.
c1<-c(2,3,1)
c2<-c(1,4,4)
c3<-c(3,0,2)
c4<-c(1,3,4)
c5<-c(3,1,2)
C<-cbind(c1,c2,c3,c4,c5); C

#Let's work in Z_5
"%+5%" <- function(x,y) (x+y) %%5  #addition
"%-5%" <- function(x,y) (x-y) %%5  #subtraction
"%*5%" <- function(x,y) (x*y) %%5  #multiplication
"%/5%" <- function(x,y) (x*y*y*y) %%5  #division

C[1,]<-C[1,]%-5%(2%*5%C[2,])
C[2,]<-C[2,]%-5%(3%*5%C[1,])
C[3,]<-C[3,]%-5%C[1,]
C[1,]<-C[1,]%-5%(3%*5%C[3,])
tmp<-C[2,]
C[2,]<-C[3,]
C[3,]<-tmp
C[1,]<-C[1,]%-5%C[3,]
C[2,]<-C[2,]%-5%(4%*5%C[3,])
#if w = 1;
#x<-0; y = 4-2*1, y=2; z = 3-3*1, z=0; w =1
C1<-cbind(c1,c2,c3,c4)
C1%*%c(0,2,0,1)%*5%1

#if w=0
#x=0, y=4, z=3, w=0
C1%*%c(0,4,3,0)%*5%1


#3. 
d1<-c(0,2,-2,3,-1,4)
d2<-c(3,1,5,2,2,-4)
d3<-c(-1,0,-2,1,-2,-1)
d4<-c(3,1,5,0,4,1)
d5<-c(-4,2,-10,-1,-1,0)
D<-rbind(d1,d2,d3,d4,d5)
rref(D)


#the basis of the image are col 1,2,4,6 of the original matrix 
c(0,3,-1,3,-4); c(2,1,0,1,2); c(3,2,1,0,-1); c(4,-4,-1,1,0)

#two free variables in column 3, 5
#(-,-,1,-,0,-) & (-,-,0,-,1,-)
#first basis x3=1, x5=0; x1+2(x3)+(x5)=0, x1 = 0-2, x1 = -2
#x2-1(x3)+1(x5)=0, x2-1=0, x2=1
#x4-x5 = 0, x4 = 0
#x6 = 0 
#first basis: 
k1<-c(-2,1,1,0,0,0)
D%*%k1


#second basis x3=0, x5=1; x1+2(x3)+(x5)=0, x1 = 0-1, x1 = -1
#x2-1(x3)+1(x5)=0, x2=-1
#x4-x5 = 0, x4 = 1
#x6 = 0 
#second basis: 
k2<-c(-1,-1,0,1,1,0)
D%*%k2
