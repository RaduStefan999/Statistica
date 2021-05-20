#A1
Poisson_f = function(lambda, k, l)
{
  x_val = seq(k, l, 1)
  y_val = dpois(x_val, lambda);
  barplot(y_val, space = 0, main='Poisson', xlab ='axa x', ylab = 'axa y', names.arg=k:l)
  
  return (max(y_val))
}

print(Poisson_f(0.5, 3, 50))


Geometric_f = function(k, l, p)
{
  x_val = seq(k, l, 1)
  y_val = dgeom(x_val, p)
  barplot(y_val, space = 0, main='Geometric', xlab ='axa x', ylab = 'axa y')
  
  return (pgeom(k, p, lower.tail = FALSE))
}

print(Geometric_f(7, 70, 0.3))

#A2
problema_a = function(fisier)
{
  x_val = scan(fisier)
  med = median(x_val) 
  m = mean(x_val)
  sd = sd(x_val)
  
  q1 = as.vector(quantile(x_val))[2]
  q3 = as.vector(quantile(x_val))[4]
  
  return (c(med, m, sd, q1, q3))
}

problema_a("esantion.txt")

problema_b = function(fisier)
{
  x_val = scan(fisier)
  q1 = as.vector(quantile(x_val))[2]
  q3 = as.vector(quantile(x_val))[4]
  
  iqr = q3 - q1
  j = 0;
  v = vector()
  left = q1 - 1.5*iqr
  right = q3 + 1.5*iqr
  
  for(i in 1:length(x_val))
  {
    if(x_val[i] < left | x_val[i] > right)
    {
      j = j + 1
      v[j] = x_val[i]
    }
  }
  x_val = x_val[! x_val %in% v]
  
  return(x_val)
}

problema_b("esantion.txt")

problema_c = function(fisier)
{
  x = problema_c(fisier)
  interval = seq(5, 10, 1)
  hist(x, breaks = interval, right = F, freq = T)
}

problema_c("esantion.txt")