t<-list()
m<-1
v<-2
alpha<-10
a<-proc.time()
n<-c()
n[1]<-100
n[2]<-10000
n[3]<-1000000
for (j in 1:3)
{

for (i in 1:n[j]) 
{
	u<-runif(1,0,1)
	x<-(-log(1-u**(1/alpha)))
	t[j]<-append(t[j],(log(x)-m)/v)
} 
}
print (t)
print(proc.time()-a)
#t<-sort(t)
#png("M.png");
#plot.ecdf(t)
#par(new=TRUE)
#plot(pnorm(t),col="red")
#dev.off()