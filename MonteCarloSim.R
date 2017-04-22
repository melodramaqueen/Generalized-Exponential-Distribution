GeneralisedDistribution <- function(n) {
  Generalised_Distribution<-c()
  alpha<-12.9
  u<-runif(n,0,1)
  x<-(-log(1-u**(1/alpha)))
  
  
  m=log(mean(x))-0.5*log(1+(var(x)/(mean(x)**2)))
  s=sqrt(log(1+var(x)/(mean(x)**2)))
  #print(m)
  #print(s)
  Generalised_Distribution<-(log(x)-m)/s
  #z<-ks.test(Generalised_Distribution,ecdf(Generalised_Distribution))
  return(Generalised_Distribution)
}

BoxMuller<-function(n) {
	Box_Muller<-c()
	
	#x<-ks.test(Box_Muller,ecdf(Box_Muller))
	while (j<=n) {
	  u<-runif(2,0,1)
	  r<--2*log(u[1])
	  v<-2*pi*u[2]
	  Box_Muller[j]<-sqrt(r)*cos(v)
	  Box_Muller[j+1]<-sqrt(r)*sin(v)
	  j<-j+2
	}
	return(Box_Muller)		
}

MarsagliaBay<-function(n) {
	j<-1
	Marsaglia_Bay<-c()
	j<-1
	while (j<=n) {
	  u<-runif(2,0,1)
	  v<-2*u-1
	  if (v[1]**2+v[2]**2<1) {
	    Marsaglia_Bay[j]<-(sqrt(-log(v[1]**2+v[2]**2))*v[1])/sqrt(v[1]**2+v[2]**2)
	    Marsaglia_Bay[j+1]<-(sqrt(-log(v[1]**2+v[2]**2))*v[2])/sqrt(v[1]**2+v[2]**2)
	    j<-j+2
	  }
	}
	#x<-(ks.test(Marsaglia_Bay,ecdf(Marsaglia_Bay)))
	return(Marsaglia_Bay)
}

AcceptanceRejection<-function(n) {
	j<-1
	Acceptance_Rejection<-c()
	while (j<=n) {
		u<-runif(2,0,1)
		y<--log(u)
		if (y[2]>=((y[1]-1)**2)/2){
			v<-runif(1,0,1)
			if (v<=0.5) {
				Acceptance_Rejection[j]<-y[1]
			}
			else {
				Acceptance_Rejection[i]<--y[i]
			}
			j<-j+1
		}
	}
	#t<-ks.test(Acceptance_Rejection,ecdf(Acceptance_Rejection))
	return(Acceptance_Rejection)
}


LMoment<-function(n) {
  alpha<-12.9
  u <-runif(n,0,1)
  gd <--log(1-u**(1/alpha))
  A0 <-mean(gd)
  sig <-sqrt(log( (var(gd)/(mean(gd)^2)) + 1 ))
  B1 <-mean(gd)*(2*pnorm(sqrt(2)*sig/2)-1)
  sig1 <-sqrt(2)*qnorm(0.5*(1+B1/A0))
  mu1 <-log(A0)-sig1*sig1/2
  Z <-(log(gd)-mu1)/sig1
  #cat("n = ",n,", alpha = ",alpha,"\n")
  #print(mu1)
  #print(sig1)
  return(Z)
}


n<-c(10,20,30,40,50,100)
for (i in 1:6) {
	bm<-BoxMuller(n[i])
	mb<-MarsagliaBay(n[i])
	ar<-AcceptanceRejection(n[i])
	ge<-GeneralisedDistribution(n[i])
	lm<-LMoment(n[i])
  print(n[i])
	a<-rnorm(n[i])
  v<-ks.test(ge,a)
  w<-ks.test(lm,a)
	x<-ks.test(bm,a)
	y<-ks.test(mb,a)
	z<-ks.test(ar,a)
	
	
	print(v)
	print(w)
	print(x)
	print(y)
	print(z)
	#ad<-AhrenDiester(n[i])
	#mm<-MomentMethod(n[i])
	#lm<LMoment(n[i])
	
}



u <-runif(1000,0,1)
gd <--log(1-u**(1/alpha))
A0 <-mean(gd)
sig <-sqrt(log( (var(gd)/(mean(gd)^2)) + 1 ))
B1 <-mean(gd)*(2*pnorm(sqrt(2)*sig/2)-1)

sig1 <-sqrt(2)*qnorm(0.5*(1+B1/A0))
mu1 <-log(A0)-sig1*sig1/2

mu0=log(mean(gd))-0.5*log(1+(var(gd)/(mean(gd)**2)))
sig0=sqrt(log(1+var(gd)/(mean(gd)**2)))

comp<-c()
for(i in 1:3 1)
{
  comp[i]<-(i-1)/10.0
}
tabl<-matrix(nrow = 31,ncol = 4)
for(i in 1:41)
{
  tabl[i,1]=comp[i]
  tabl[i,2]=(1-exp(-exp(comp[i]*sig0 + mu0)))^12.9
  tabl[i,3]=pnorm(comp[i])
  tabl[i,4]=(1-exp(-exp(comp[i]*sig1 + mu1)))^12.8
}
print(tabl)
#print(ad)
#print(mm)
#print(lm)

#t<-sort(t)
#png("M.png");
#plot.ecdf(t)
#par(new=TRUE)
#plot(pnorm(t),col="red")
#dev.off()