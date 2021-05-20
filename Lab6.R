Nr_days = function() {
  nr_days = 1;
  last_errors = c(18, 22, 28);
  nr_errors = 18;
  while(nr_errors > 0) {
    lambda = min(last_errors);
    nr_errors = rpois(1, lambda);
    last_errors = c(nr_errors, last_errors[1 : 2]);
    nr_days = nr_days + 1;
  }
  return(nr_days);
}

Mc_nr_days_21 = function(n)
{
  s = 0
  for(i in 1 : n)
  {
    if(Nr_days() > 21)
      s = s + 1
  }
  return(s / n)
}

Mc_nr_days_21(5000)

alfa = 1 - 0.95
z = qnorm(alfa / 2)
epsilon = 0.01
p = 0.354
N_min = p * (1 - p) * (z / epsilon) ^ 2
N_min
Mc_nr_days_21(N_min + 1)


N_min = (1 / 4) * (z / epsilon) ^ 2
N_min
Mc_nr_days_21(N_min + 1)


##############


a_g = function(n)
{
  k = 0
  for(i in 1 : n)
  {
    x = rgeom(1, 0.3)
    y = rgeom(1, 0.5)
    if(x < y * y)
      k = k + 1
  }
  
  return (k / n)    
}

a_g(5000)


alfa = 1 - 0.95
z = qnorm(alfa / 2)
epsilon = 0.005
p = 0.5
N_min = p * (1 - p) * (z / epsilon) ^ 2
N_min
a_g(N_min + 1)

######################

runif(2, 4, 7)
sample(12 : 15, 8, replace = TRUE)

x = c(2.1, 3.2, 2.3, 2.5, 3.1, 2.9, 2.6, 2.2, 3.3)
sample(x, 4, replace = TRUE)

#####################

x = c(1, 3, 1, 4, 12, 7)
M = matrix(x, 3, 2)
M
N= matrix(x, 2, 3)
N

matrix_product = function(A, B, C) {
  n = nrow(A);
  r = matrix( , nrow = n, ncol = 1);
  x = matrix( , nrow = n, ncol = 1);
  y = matrix( , nrow = n, ncol = 1);
  r = sample(0:1, n, replace = TRUE);
  for(i in 1:n) {# x = Br
    x[i] = 0;
    for(j in 1:nrow(B))
      x[i] = (x[i]+ B[i,j]*r[j])%%2;
  }
  for(i in 1:nrow(B)) {# y = Ax = ABr
    y[i] = 0;
    for(j in 1:n)
      y[i] = (y[i]+ A[i,j]*x[j])%%2;
  }
  for(i in 1:n) {# x = Cr
    x[i] = 0;
    for(j in 1:n)
      x[i] = (x[i]+ C[i,j]*r[j])%%2;
  }
  for(i in 1:n) {# verify if ABr==Cr
    if(y[i] !=x[i])
      return(FALSE);
  }
  return(TRUE);
}

A = matrix(c(1, 5, 2, 3), 2, 2)
C = A
C = matrix(c(1, 0, 0, 1), 2, 2)


matrix_product_reduce = function(A, B, C, k) {
  for(i in 1:k)
    if(!matrix_product(A, B, C))
      return(FALSE);
  return(TRUE);
}

matrix_product_reduce(A, B, C, 1)


########################

#14ex

a_repartitie = function(x, p)
{
  n = length(p)
  S = rep(0, n + 1)
  for (k in 1:n)
  {
    S[k+1] = sum(p[1:k])
  }
  
  r=runif(1, 0, 1)
  for (i in 1:n)
  {
    if ((S[i] < r) && (r <=S[i+1]))
      return(x[i])
  }
}

x=c(1, 4, 5, 7, 12, 111)
p_n=c(12, 33, 24, 12, 10, 1)
p=p_n/sum(p_n)
p
a_repartitie(x, p)

########################################


alfa = 0.1
n = 100
succese = 63
p_prim = succes/n
p0 = 0.6
z_score = (p_prim - p0)/sqrt(p0*(1-p0)/n)
critic_z = qnorm(1 - alfa, 0, 1)
z_score
critic_z



alfa = 0.05
population_mean = 810
sample_mean = 816
n = 200
sigma = 50
critical_z = qnorm(1 - alfa)
z_score = (sample_mean - population_mean)/(sigma/sqrt(n))
critical_z
z_score


z_test = function(alfa, tip_ipoteza, population_mean, sample_mean, n, sigma)
{
  
  z_score = (sample_mean - population_mean)/(sigma/sqrt(n))

  if (tip_ipoteza == 1)
  {
    critic_z = qnorm(1 - alfa)
    if (z_score > critical_z)
    {
      return(T)
    }
  }
  
  if (tip_ipoteza == -1)
  {
    critic_z = qnorm(alfa)
    if (z_score > critical_z)
    {
      return(T)
    }
  }
}

z_test(0.05, -1, 90, 88, 49, 12)



#######################################################


alfa = 0.05

x = c(55, 95, 68.24, 52.73, 21.5, 23.78)
population_mean = 40

sample_mean = mean(x)
n = 5
s = sd(x)
se = s/sqrt(n)
critical_t = qt(1 - alfa, n - 1)
t_score = (sample_mean - population_mean)/se
critical_t
t_score














t_test1=function(alfa, tip_ipoteza, population_mean, sample_mean, n, s)
{
  se = s/sqrt(n)
  t_score = (sample_mean - population_mean)/se
  
  #ipoteza asimetrica la dreapta
  if(tip_ipoteza == 1)
  {
    critical_t = qt(1- alfa, n-1)
    if(t_score > critical_t)
    {
      return(T)
    }
  }
  
  #ipoteza asimetrica la stanga
  if(tip_ipoteza == -1)
  {
    critical_t = qt(alfa, n-1)
    if(t_score < critical_t)
    {
      return(T)
    }
  }
  
  #ipoteza simetrica
  if(tip_ipoteza == 0)
  {
    critical_t = qt(1- alfa/2, n-1)
    if(abs(t_score) > abs(critical_t))
    {
      return(T)
    }
  }
  return(F)
}
t_test2=function(alfa, tip_ipoteza, population_mean, x)
{
  n=length(x)
  s=sd(x)
  se = s/sqrt(n)
  sample_mean=mean(x)
  t_score = (sample_mean - population_mean)/se
  
  #ipoteza asimetrica la dreapta
  if(tip_ipoteza == 1)
  {
    critical_t = qt(1- alfa, n-1)
    if(t_score > critical_t)
    {
      return(T)
    }
  }
  
  #ipoteza asimetrica la stanga
  if(tip_ipoteza == -1)
  {
    critical_t = qt(alfa, n-1)
    if(t_score < critical_t)
    {
      return(T)
    }
  }
  
  #ipoteza simetrica
  if(tip_ipoteza == 0)
  {
    critical_t = qt(1- alfa/2, n-1)
    if(abs(t_score) > abs(critical_t))
    {
      return(T)
    }
  }
  return(F)
}
x = c(36, 32, 28, 33, 41, 28, 31, 26, 29, 34)
t_test2(0.01, 0, 34, x)






