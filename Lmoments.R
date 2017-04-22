GD_lmom <- function(n,alpha) {
  u <-runif(n,0,1)
  gd <--log(1-u**(1/alpha))
  A0 <-mean(gd)
  sig <-sqrt(log( (var(gd)/(mean(gd)^2)) + 1 ))
  B1 <-mean(gd)*(2*pnorm(sqrt(2)*sig/2)-1)
  sig1 <-sqrt(2)*qnorm(0.5*(1+B1/A0))
  mu1 <-log(A0)-sig1*sig1/2
  Z <-(log(gd)-mu1)/sig1
  cat("n = ",n,", alpha = ",alpha,"\n")
  print(mu1)
  print(sig1)
  return(Z)
}

a<-GD_lmom(n,alpha)
print(var(a))
print(mean(a))

n<-c(10,20,30,40,50,100)
alpha<-c(1,4,7,10,12.9)
for (i in 1:6){
  for (j in 1:5) {
    a<-GD_lmom(n[i],alpha[j])
    #print(var(a))
    #print(mean(a))
    png(paste("lgraph_", n[i],".png"))
    plot(ecdf(numbers))
    dev.off()
  }
}


