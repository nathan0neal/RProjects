###rendutp3proba
N=1000#nb echantillons
n1=1#taille des ech
n2=2
n3=3
n5=5
n10=10
n20=20
###############################################################################
#loi binomiale
###############################################################################
n=100 
p=0.3
X1=rep(NA,N)
X2=rep(NA,N)
X3=rep(NA,N)
X5=rep(NA,N)
X10=rep(NA,N)
X20=rep(NA,N)
for(i in 1:N) 
{
  X1[i]=mean(rbinom(n1,n,p))
  X2[i]=mean(rbinom(n2,n,p))
  X3[i]=mean(rbinom(n3,n,p))
  X5[i]=mean(rbinom(n5,n,p))
  X10[i]=mean(rbinom(n10,n,p))
  X20[i]=mean(rbinom(n20,n,p))
}
par(mfrow=c(2,3)) #6 graph sur la meme fenetre
hist(X1,br=15,freq=FALSE,main="1000 simu Binom n=1",xlab="nombre",ylab="freq",xlim=c(0,50),ylim=c(0,0.1),col="lightblue")
lines(density(X1), col="green", lwd=2)#densite estime empiriquement
x=X1
curve(dnorm(x,mean=n*p,sd=sqrt(n*p*(1-p)/n1)),add=TRUE,col="red")#densite de loi normale 

hist(X2,br=15,freq=FALSE,main="1000 simu Binom n=2",xlab="nombre",ylab="freq",xlim=c(0,50),ylim=c(0,0.15),col="lightblue")
lines(density(X2), col="green", lwd=2)#densite estime empiriquement
x=X2
curve(dnorm(x,mean=n*p,sd=sqrt(n*p*(1-p)/n2)),add=TRUE,col="red")

hist(X3,br=15,freq=FALSE,main="1000 simu Binom n=3",xlab="nombre",ylab="freq",xlim=c(0,50),ylim=c(0,0.20),col="lightblue")
lines(density(X3), col="green", lwd=2)#densite estime empiriquement
x=X3
curve(dnorm(x,mean=n*p,sd=sqrt(n*p*(1-p)/n3)),add=TRUE,col="red")

hist(X5,br=15,freq=FALSE,main="1000 simu Binom n=5",xlab="nombre",ylab="freq",xlim=c(0,50),ylim=c(0,0.25),col="lightblue")
lines(density(X5), col="green", lwd=2)#densite estime empiriquement
x=X5
curve(dnorm(x,mean=n*p,sd=sqrt(n*p*(1-p)/n5)),add=TRUE,col="red")

hist(X10,br=10,freq=FALSE,main="1000 simu Binom n=10",xlab="nombre",ylab="freq",xlim=c(0,50),ylim=c(0,0.3),col="lightblue")
lines(density(X10), col="green", lwd=2)#densite estime empiriquement
x=X10
curve(dnorm(x,mean=n*p,sd=sqrt(n*p*(1-p)/n10)),add=TRUE,col="red")

hist(X20,br=5,freq=FALSE,main="1000 simu Binom n=20",xlab="nombre",ylab="freq",xlim=c(0,50),ylim=c(0,0.4),col="lightblue")
lines(density(X20), col="green", lwd=2)#densite estime empiriquement
x=X20
curve(dnorm(x,mean=n*p,sd=sqrt(n*p*(1-p)/n20)),add=TRUE,col="red")
par(mfrow=c(1,2)) 
qqnorm(X20,main="Student QQPlot et droite de Henry",xlab="Binom n=20")
qqline(X20, col = "green")
plot(density(X1),ylim=c(0,0.5),col="black")
lines(density(X2),col="red")
lines(density(X3),col="blue")
lines(density(X5),col="green")
lines(density(X10),col="pink")
lines(density(X20),col="lightblue")
legend( "topright", c("n=1", "n=2", "n=3", "n=5","n=10","n=20"), 
        text.col=c("black", "red", "blue", "green","pink","lightblue") )

##############################################################################
#loi Geometrique G(0.2)
##############################################################################
p=0.2
X1=rep(NA,N)
X2=rep(NA,N)
X3=rep(NA,N)
X5=rep(NA,N)
X10=rep(NA,N)
X20=rep(NA,N)
for(i in 1:N) 
{
  X1[i]=mean(rgeom(n1,p))
  X2[i]=mean(rgeom(n2,p))
  X3[i]=mean(rgeom(n3,p))
  X5[i]=mean(rgeom(n5,p))
  X10[i]=mean(rgeom(n10,p))
  X20[i]=mean(rgeom(n20,p))
}
par(mfrow=c(2,3)) 
hist(X1,br=15,freq=FALSE,main="1000 simu geo n=1",xlab="nombre",ylab="freq",xlim=c(0,35),ylim=c(0,0.3),col="lightblue")
lines(density(X1), col="green", lwd=2)#densite estime empiriquement
x=X1
curve(dnorm(x,mean=(1/p)-1,sd=sqrt((1-p)/(p^2*n1))),add=TRUE,col="red")#densite de loi normale 

hist(X2,br=15,freq=FALSE,main="1000 simu geo n=2",xlab="nombre",ylab="freq",xlim=c(0,35),ylim=c(0,0.3),col="lightblue")
lines(density(X2), col="green", lwd=2)#densite estime empiriquement
x=X2
curve(dnorm(x,mean=(1/p)-1,sd=sqrt((1-p)/(p^2*n2))),add=TRUE,col="red")

hist(X3,br=15,freq=FALSE,main="1000 simu geo n=3",xlab="nombre",ylab="freq",xlim=c(0,35),ylim=c(0,0.3),col="lightblue")
lines(density(X3), col="green", lwd=2)#densite estime empiriquement
x=X3
curve(dnorm(x,mean=(1/p)-1,sd=sqrt((1-p)/(p^2*n3))),add=TRUE,col="red")

hist(X5,br=15,freq=FALSE,main="1000 simu geo n=5",xlab="nombre",ylab="freq",xlim=c(0,35),ylim=c(0,0.3),col="lightblue")
lines(density(X5), col="green", lwd=2)#densite estime empiriquement
x=X5
curve(dnorm(x,mean=(1/p)-1,sd=sqrt((1-p)/(p^2*n5))),add=TRUE,col="red")

hist(X10,br=15,freq=FALSE,main="1000 simu geo n=10",xlab="nombre",ylab="freq",xlim=c(0,35),ylim=c(0,0.3),col="lightblue")
lines(density(X10), col="green", lwd=2)#densite estime empiriquement
x=X10
curve(dnorm(x,mean=(1/p)-1,sd=sqrt((1-p)/(p^2*n10))),add=TRUE,col="red")

hist(X20,br=15,freq=FALSE,main="1000 simu geo n=20",xlab="nombre",ylab="freq",xlim=c(0,35),ylim=c(0,0.4),col="lightblue")
lines(density(X20), col="green", lwd=2)#densite estime empiriquement
x=X20
curve(dnorm(x,mean=(1/p)-1,sd=sqrt((1-p)/(p^2*n20))),add=TRUE,col="red")
par(mfrow=c(1,2)) 
qqnorm(X20,main="Student QQPlot et droite de Henry",xlab="geom n=20")
qqline(X20, col = "green")
plot(density(X1),ylim=c(0,1.5),xlim=c(0,6),col="black")
lines(density(X2),col="red")
lines(density(X3),col="blue")
lines(density(X5),col="green")
lines(density(X10),col="pink")
lines(density(X20),col="lightblue")
legend( "topright", c("n=1", "n=2", "n=3", "n=5","n=10","n=20"), 
        text.col=c("black", "red", "blue", "green","pink","lightblue") )

###############################################################################
#loi Poisson P(5)
###############################################################################
l=5 #lambda
X1=rep(NA,N)
X2=rep(NA,N)
X3=rep(NA,N)
X5=rep(NA,N)
X10=rep(NA,N)
X20=rep(NA,N)
for(i in 1:N) 
{
  X1[i]=mean(rpois(n1,l))
  X2[i]=mean(rpois(n2,l))
  X3[i]=mean(rpois(n3,l))
  X5[i]=mean(rpois(n5,l))
  X10[i]=mean(rpois(n10,l))
  X20[i]=mean(rpois(n20,l))
}
par(mfrow=c(2,3))
hist(X1,br=15,freq=FALSE,main="1000 simu Pois n=1",xlab="nombre",ylab="freq",xlim=c(0,15),ylim=c(0,0.4),col="lightblue")
lines(density(X1), col="green", lwd=2)#densite estime empiriquement
x=X1
curve(dnorm(x,mean=l,sd=sqrt(l/n1)),add=TRUE,col="red")#densite de loi normale 

hist(X2,br=15,freq=FALSE,main="1000 simu Pois n=2",xlab="nombre",ylab="freq",xlim=c(0,15),ylim=c(0,0.4),col="lightblue")
lines(density(X2), col="green", lwd=2)#densite estime empiriquement
x=X2
curve(dnorm(x,mean=l,sd=sqrt(l/n2)),add=TRUE,col="red")

hist(X3,br=15,freq=FALSE,main="1000 simu Pois n=3",xlab="nombre",ylab="freq",xlim=c(0,15),ylim=c(0,0.5),col="lightblue")
lines(density(X3), col="green", lwd=2)#densite estime empiriquement
x=X3
curve(dnorm(x,mean=l,sd=sqrt(l/n3)),add=TRUE,col="red")

hist(X5,br=15,freq=FALSE,main="1000 simu Pois n=5",xlab="nombre",ylab="freq",xlim=c(0,15),ylim=c(0,0.5),col="lightblue")
lines(density(X5), col="green", lwd=2)#densite estime empiriquement
x=X5
curve(dnorm(x,mean=l,sd=sqrt(l/n5)),add=TRUE,col="red")

hist(X10,br=15,freq=FALSE,main="1000 simu Pois n=10",xlab="nombre",ylab="freq",xlim=c(0,10),ylim=c(0,0.7),col="lightblue")
lines(density(X10), col="green", lwd=2)#densite estime empiriquement
x=X10
curve(dnorm(x,mean=l,sd=sqrt(l/n10)),add=TRUE,col="red")

hist(X20,br=15,freq=FALSE,main="1000 simu Pois n=20",xlab="nombre",ylab="freq",xlim=c(0,10),ylim=c(0,0.9),col="lightblue")
lines(density(X20), col="green", lwd=2)#densite estime empiriquement
x=X20
curve(dnorm(x,mean=l,sd=sqrt(l/n20)),add=TRUE,col="red")
par(mfrow=c(1,2)) 
qqnorm(X20,main="Student QQPlot et droite de Henry",xlab="Poisson n=20")
qqline(X20, col = "green")
plot(density(X1),ylim=c(0,1.2),xlim=c(0,8),col="black")
lines(density(X2),col="red")
lines(density(X3),col="blue")
lines(density(X5),col="green")
lines(density(X10),col="pink")
lines(density(X20),col="lightblue")
legend( "topright", c("n=1", "n=2", "n=3", "n=5","n=10","n=20"), 
        text.col=c("black", "red", "blue", "green","pink","lightblue") )

###############################################################################
#loi uniforme U(0,1)
###############################################################################
X1=rep(NA,N)
X2=rep(NA,N)
X3=rep(NA,N)
X5=rep(NA,N)
X10=rep(NA,N)
X20=rep(NA,N)
for (i in 1 : N){ 
  X1[i]=sum(runif(n1))
  X2[i]=sum(runif(n2))
  X3[i]=sum(runif(n3))
  X5[i]=sum(runif(n5))
  X10[i]=sum(runif(n10))
  X20[i]=sum(runif(n20))
}
par(mfrow=c(2,3)) 
hist(X1,br=30,freq=FALSE,main="1000 simu unif n=1",xlab="nombre",ylab="freq",xlim=c(0,3),ylim=c(0,1.5),col="lightblue")
lines(density(X1), col="green", lwd=2)#densite estime empiriquement
x=X1
curve(dnorm(x,mean=n1/2,sd=sqrt(n1/12)),add=TRUE,col="red")

hist(X2,br=30,freq=FALSE,main="1000 simu unif n=2",xlab="nombre",ylab="freq",xlim=c(0,3),ylim=c(0,1.5),col="lightblue")
lines(density(X2), col="green", lwd=2)#densite estime empiriquement
x=X2
curve(dnorm(x,mean=n2/2,sd=sqrt(n2/12)),add=TRUE,col="red")

hist(X3,br=30,freq=FALSE,main="1000 simu unif n=3",xlab="nombre",ylab="freq",xlim=c(0,3),ylim=c(0,1),col="lightblue")
lines(density(X3), col="green", lwd=2)#densite estime empiriquement
x=X3
curve(dnorm(x,mean=n3/2,sd=sqrt(n3/12)),add=TRUE,col="red")

hist(X5,br=30,freq=FALSE,main="1000 simu unif n=5",xlab="nombre",ylab="freq",xlim=c(0,6),ylim=c(0,1),col="lightblue")
lines(density(X5), col="green", lwd=2)#densite estime empiriquement
x=X5
curve(dnorm(x,mean=n5/2,sd=sqrt(n5/12)),add=TRUE,col="red")

hist(X10,br=30,freq=FALSE,main="1000 simu unif n=10",xlab="nombre",ylab="freq",xlim=c(2,8),ylim=c(0,0.6),col="lightblue")
lines(density(X10), col="green", lwd=2)#densite estime empiriquement
x=X10
curve(dnorm(x,mean=n10/2,sd=sqrt(n10/12)),add=TRUE,col="red")

hist(X20,br=30,freq=FALSE,main="1000 simu unif n=20",xlab="nombre",ylab="freq",xlim=c(4,n20-5),ylim=c(0,0.5),col="lightblue")
lines(density(X20), col="green", lwd=2)#densite estime empiriquement
x=X20
curve(dnorm(x,mean=n20/2,sd=sqrt(n20/12)),add=TRUE,col="red")
par(mfrow=c(1,2)) 
qqnorm(X20,main="Student QQPlot et droite de Henry",xlab="Uniforme n=20")
qqline(X20, col = "green")
plot(density(X1),ylim=c(0,1.3),xlim=c(0,7),col="black")
lines(density(X2),col="red")
lines(density(X3),col="blue")
lines(density(X5),col="green")
lines(density(X10),col="pink")
lines(density(X20),col="lightblue")
legend( "topright", c("n=1", "n=2", "n=3", "n=5","n=10","n=20"), 
        text.col=c("black", "red", "blue", "green","pink","lightblue") )

###############################################################################
#loi exponentielle E(2)
###############################################################################
p=2
X1=rep(NA,N)
X2=rep(NA,N)
X3=rep(NA,N)
X5=rep(NA,N)
X10=rep(NA,N)
X20=rep(NA,N)
for(i in 1:N) 
{
  X1[i]=mean(rexp(n1,p))
  X2[i]=mean(rexp(n2,p))
  X3[i]=mean(rexp(n3,p))
  X5[i]=mean(rexp(n5,p))
  X10[i]=mean(rexp(n10,p))
  X20[i]=mean(rexp(n20,p))
}
par(mfrow=c(2,3)) 
hist(X1,br=30,freq=FALSE,main="1000 simu expo n=1",xlab="nombre",ylab="freq",xlim=c(0,2),ylim=c(0,1.8),col="lightblue")
lines(density(X1), col="green", lwd=2)#densite estime empiriquement
x=X1
curve(dnorm(x,mean=1/p,sd=sqrt(1/(p^2*n1))),add=TRUE,col="red")

hist(X2,br=30,freq=FALSE,main="1000 simu expo n=2",xlab="nombre",ylab="freq",xlim=c(0,2),ylim=c(0,2),col="lightblue")
lines(density(X2), col="green", lwd=2)#densite estime empiriquement
x=X2
curve(dnorm(x,mean=1/p,sd=sqrt(1/(p^2*n2))),add=TRUE,col="red")

hist(X3,br=30,freq=FALSE,main="1000 simu expo n=3",xlab="nombre",ylab="freq",xlim=c(0,2),ylim=c(0,2),col="lightblue")
lines(density(X3), col="green", lwd=2)#densite estime empiriquement
x=X3
curve(dnorm(x,mean=1/p,sd=sqrt(1/(p^2*n3))),add=TRUE,col="red")

hist(X5,br=30,freq=FALSE,main="1000 simu expo n=5",xlab="nombre",ylab="freq",xlim=c(0,2),ylim=c(0,2.5),col="lightblue")
lines(density(X5), col="green", lwd=2)#densite estime empiriquement
x=X5
curve(dnorm(x,mean=1/p,sd=sqrt(1/(p^2*n5))),add=TRUE,col="red")

hist(X10,br=30,freq=FALSE,main="1000 simu expo n=10",xlab="nombre",ylab="freq",xlim=c(0,1.5),ylim=c(0,3),col="lightblue")
lines(density(X10), col="green", lwd=2)#densite estime empiriquement
x=X10
curve(dnorm(x,mean=1/p,sd=sqrt(1/(p^2*n10))),add=TRUE,col="red")

hist(X20,br=30,freq=FALSE,main="1000 simu expo n=20",xlab="nombre",ylab="freq",xlim=c(0,1.5),ylim=c(0,4),col="lightblue")
lines(density(X20), col="green", lwd=2)#densite estime empiriquement
x=X20
curve(dnorm(x,mean=1/p,sd=sqrt(1/(p^2*n20))),add=TRUE,col="red")
par(mfrow=c(1,2)) 
qqnorm(X20,main="Student QQPlot et droite de Henry",xlab="Exponentielle n=20")
qqline(X20, col = "green")
plot(density(X1),ylim=c(0,4),col="black")
lines(density(X2),col="red")
lines(density(X3),col="blue")
lines(density(X5),col="green")
lines(density(X10),col="pink")
lines(density(X20),col="lightblue")
legend( "topright", c("n=1", "n=2", "n=3", "n=5","n=10","n=20"), 
        text.col=c("black", "red", "blue", "green","pink","lightblue") )


###############################################################################
#loi gamma g(3,1)
###############################################################################
a=3
b=1
X1=rep(NA,N)
X2=rep(NA,N)
X3=rep(NA,N)
X5=rep(NA,N)
X10=rep(NA,N)
X20=rep(NA,N)
for(i in 1:N) 
{
  X1[i]=mean(rgamma(n1,a,b))
  X2[i]=mean(rgamma(n2,a,b))
  X3[i]=mean(rgamma(n3,a,b))
  X5[i]=mean(rgamma(n5,a,b))
  X10[i]=mean(rgamma(n10,a,b))
  X20[i]=mean(rgamma(n20,a,b))
}
par(mfrow=c(2,3)) 
hist(X1,br=30,freq=FALSE,main="1000 simu gamma n=1",xlab="nombre",ylab="freq",xlim=c(0,12),ylim=c(0,0.5),col="lightblue")
lines(density(X1), col="green", lwd=2)#densite estime empiriquement
x=X1
curve(dnorm(x,mean=a*b,sd=sqrt((a*b^2)/n1)),add=TRUE,col="red")

hist(X2,br=30,freq=FALSE,main="1000 simu gamma n=2",xlab="nombre",ylab="freq",xlim=c(0,9),ylim=c(0,0.5),col="lightblue")
lines(density(X2), col="green", lwd=2)#densite estime empiriquement
x=X2
curve(dnorm(x,mean=a*b,sd=sqrt((a*b^2)/n2)),add=TRUE,col="red")

hist(X3,br=30,freq=FALSE,main="1000 simu gamma n=3",xlab="nombre",ylab="freq",xlim=c(0,8),ylim=c(0,0.5),col="lightblue")
lines(density(X3), col="green", lwd=2)#densite estime empiriquement
x=X3
curve(dnorm(x,mean=a*b,sd=sqrt((a*b^2)/n3)),add=TRUE,col="red")

hist(X5,br=30,freq=FALSE,main="1000 simu gamma n=5",xlab="nombre",ylab="freq",xlim=c(0,6),ylim=c(0,0.6),col="lightblue")
lines(density(X5), col="green", lwd=2)#densite estime empiriquement
x=X5
curve(dnorm(x,mean=a*b,sd=sqrt((a*b^2)/n5)),add=TRUE,col="red")

hist(X10,br=30,freq=FALSE,main="1000 simu gamma n=10",xlab="nombre",ylab="freq",xlim=c(0,5),ylim=c(0,0.9),col="lightblue")
lines(density(X10), col="green", lwd=2)#densite estime empiriquement
x=X10
curve(dnorm(x,mean=a*b,sd=sqrt((a*b^2)/n10)),add=TRUE,col="red")

hist(X20,br=30,freq=FALSE,main="1000 simu gamma n=20",xlab="nombre",ylab="freq",xlim=c(0,5),ylim=c(0,1.2),col="lightblue")
lines(density(X20), col="green", lwd=2)#densite estime empiriquement
x=X20
curve(dnorm(x,mean=a*b,sd=sqrt((a*b^2)/n20)),add=TRUE,col="red")
par(mfrow=c(1,2)) 
qqnorm(X20,main="Student QQPlot et droite de Henry",xlab="gamma n=20")
qqline(X20, col = "green")
plot(density(X1),ylim=c(0,2),xlim=c(0,8),col="black")
lines(density(X2),col="red")
lines(density(X3),col="blue")
lines(density(X5),col="green")
lines(density(X10),col="pink")
lines(density(X20),col="lightblue")
legend( "topright", c("n=1", "n=2", "n=3", "n=5","n=10","n=20"), 
        text.col=c("black", "red", "blue", "green","pink","lightblue") )
