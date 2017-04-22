GeneralisedDistribution <- function(n,alpha) {
	Generalised_Distribution<-c()
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

n<-c(10,20,30,40,50,100)
alpha<-c(1,4,7,10,12.8)


for (i in 1:6){
	for (j in 1:5) {
		#print(n,alpha)
		numbers<-GeneralisedDistribution(n[i],alpha[j])
	  #plot(numbers)
		print(numbers)
		print(mean(numbers))
    print(sd(numbers))
    png(paste("graph_", n[i],".png"))
    plot(ecdf(numbers))
    dev.off()
		}
}