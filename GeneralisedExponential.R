GeneralisedDistribution <- function(n,alpha) {
	Generalised_Distribution<-c()
	u<-runif(n,0,1)
	x<--log(1-u**(1/alpha))
	Generalised_Distribution<-(log(x)-mean(x))/sd(x)
	#z<-ks.test(Generalised_Distribution,ecdf(Generalised_Distribution))
	return(Generalised_Distribution)
}

n<-c(10,100,1000,10000,100000)
alpha<-c(1,2,3,4,5,6,7,8,9,10,11,12)

for (i in 1:2){
	for (j in 1:12) {
		print(n[i],alpha[j])
		numbers<-GeneralisedDistribution(n[i],alpha[j])
		#print(numbers)
		print(mean(numbers))
	}
}