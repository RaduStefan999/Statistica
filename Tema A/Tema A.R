#A1
Poisson_f = function(lambda, k, l)
{
  x_val = seq(k, l, 1)
  y_val = dpois(x_val, lambda);
  plot(y_val, xlab ='axa x', ylab = 'axa y')
  
  return (max(y_val))
}

print(Poisson_f(0.5, 3, 50))


Geometric_f = function(k, l, p)
{
  x_val = seq(k, l, 1)
  y_val = dgeom(x_val, p)
  plot(y_val, xlab ='axa x', ylab = 'axa y')
  
  return (pgeom(k, p, lower.tail = FALSE))
}

print(Geometric_f(7, 70, 0.3))

#A2
problema_a = function(fisier)
{
  x_val = scan(fisier)
  med = median(x_val) 
  m = mean(x_val)
  devi_s = sd(x_val)
  
  q_l = as.vector(quantile(x_val))[2]
  q_r = as.vector(quantile(x_val))[4]
  
  return (c(med, m, devi_s, q_l, q_r))
}

print(problema_a("esantion.txt"))

problema_b = function(fisier)
{
  x_val = scan(fisier)
  q_l = as.vector(quantile(x_val))[2]
  q_r = as.vector(quantile(x_val))[4]
  
  eror = q_r - q_l
  
  
  mic = q_l - 1.5*eror
  mare = q_r + 1.5*eror
  
  returnat = vector()
  j = 0;
  
  for(i in 1:length(x_val))
  {
    if(x_val[i] >= mic && x_val[i] <= mare)
    {
      j = j + 1
      returnat[j] = x_val[i]
    }
  }
  
  return(returnat)
}

print(problema_b("esantion.txt"))

problema_c = function(fisier)
{
  x = problema_b(fisier)
  interval = seq(50, 200, 15)
  hist(x, breaks = interval, right = F, freq = T)
}

problema_c("esantion.txt")