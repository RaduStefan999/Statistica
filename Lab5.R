#Laboratorul 5
t = seq(-6, 6, length = 400)
f = 1/sqrt(2*pi)*exp(-t^2/2)
plot(t, f, type = 'l', lwd = 1)

#I.1
d_norm=function(miu,sigmap) #sigmap=sigma patrat
{
  
  t = seq(miu-6, miu+6, length = 400)
  f = (1/(sqrt(sigmap)*sqrt(2*pi)))*exp(-(t-miu)^2/(2*sigmap)) #aici am modificat
  plot(t, f, type = 'l', lwd = 1)
}
d_norm(1,4)
d_norm(3,4)
d_norm(3,1)

x = 3.2
y = pnorm(x, mean= 3, sd=2)
y
qnorm(y, mean=3, sd=2)

interval = function(alfa, sample_mean, n, sigmap)
{
  sigma = sqrt(sigma)
  critical_z = qnorm(1 - alfa/2, 0, 1)
  a = sample_mean - critical_z*sigma/sqrt(n)
  b = sample_mean + critical_z*sigma/sqrt(n)
  return (c(a, b))
}


interval(0, 1, 67.53, 25, 100)
interval(0.05, 5, 550, 0.25)


interval(0.1, 60, 35, 25)
interval(0.05, 60, 35, 25)
interval(0.01, 60, 35, 25)

interval(0, 60, 35, 25)

interval1 = function(alfa, sample_mean, n, s)
{
  se = s/sqrt(n)
  critical_t = qt(1 - alfa/2, n - 1)
  a = sample_mean - critical_t*se;
  b = sample_mean + critical_t*se;
  
  return(c(a, b))
}

interval1(0.01, 44.65, 196, )
}
