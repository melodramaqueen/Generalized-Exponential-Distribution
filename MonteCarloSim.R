GeneralisedDistribution <- function(n) {
	Generalised_Distribution<-c()
	alpha<-1
	u<-runif(n,0,1)
	x<--log(1-u**(1/alpha))
	Generalised_Distribution<-(log(x)-mean(x))/sd(x)
	#z<-ks.test(Generalised_Distribution,ecdf(Generalised_Distribution))
	return(Generalised_Distribution)
}

BoxMuller<-function(n) {
	Box_Muller<-c()
	j<-1
	while (j<=n) {
		u<-runif(2,0,1)
	    v<-2*u-1
		if (v[1]**2+v[2]**2<1) {
			Box_Muller[j]<-(sqrt(-log(v[1]**2+v[2]**2))*v[1])/sqrt(v[1]**2+v[2]**2)
			Box_Muller[j+1]<-(sqrt(-log(v[1]**2+v[2]**2))*v[2])/sqrt(v[1]**2+v[2]**2)
			j<-j+2
			}
		}
	#x<-ks.test(Box_Muller,ecdf(Box_Muller))
	return(Box_Muller)		
}

MarsagliaBay<-function(n) {
	j<-1
	Marsaglia_Bay<-c()
	while (j<=n) {
		u<-runif(2,0,1)
	    r<--2*log(u[1])
	    v<-2*pi*u[2]
		Marsaglia_Bay[j]<-sqrt(r)*cos(v)
		Marsaglia_Bay[j+1]<-sqrt(r)*sin(v)
		j<-j+2
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

AhrenDiester<-function(n) {

}

MomentMethod<-function(n) {

}

LMoment<-function(n) {

}


n<-c(10,20,30,40,50,100)
for (i in 1:6) {
	bm<-BoxMuller(n[i])
	mb<-MarsagliaBay(n[i])
	ar<-AcceptanceRejection(n[i])
	ge<-GeneralisedDistribution(n[i])

	x<-ks.test(bm,ge)
	y<-ks.test(mb,ge)
	z<-ks.test(ar,ge)
	print(x)
	print(y)
	print(z)
	#ad<-AhrenDiester(n[i])
	#mm<-MomentMethod(n[i])
	#lm<LMoment(n[i])
	
}

#print(ad)
#print(mm)
#print(lm)

#t<-sort(t)
#png("M.png");
#plot.ecdf(t)
#par(new=TRUE)
#plot(pnorm(t),col="red")
#dev.off()