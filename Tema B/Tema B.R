#B1

volum_elipsoid = function(a, b, c, D)
{
  nr = 0;
  
  for (i in 0: D)
  {
    x = runif(1, -a, a);
    y = runif(1, -b, b);
    z = runif(1, -c, c);
    
    if ((x*x)/(a*a) + (y*y)/(b*b) + (z*z)/(c*c) <= 1)
    {
      nr = nr + 1;
    }
  }
  
  return ((nr/D)*(8*a*b*c));
}

sfera_statistica = volum_elipsoid(3, 2, 4, 50000);
sfera_reala = ((4*pi*3*2*4)/3);

print(sfera_statistica);
print(sfera_reala);

#B2
#[0,2]*[0,4/3]

arie_triunghi = function(a, b, c, d, D)
{
  nr = 0;
  
  for (i in 0: D)
  {
    x = runif(1, a, b);
    y = runif(1, c, d);
    
    if ((x >= 0) && (y >= 0) && (y <= x) && (2*x + y <= 4))
    {
      nr = nr + 1;
    }
  }
  
  return ((nr/D)*((b-a)*(d-c)));
}

triunghi_statistic = arie_triunghi(0, 2, 0, 4/3, 20000);
print(triunghi_statistic);

#B3
#a)

integrare_functie_1 = function(a, b, D)
{
  sum = 0;
  
  for (i in 1: D)
  {
    u = runif(1, a, b);
    sum = sum + (1/(1 + sqrt(u)));
  }
  
  return ((sum/D)*(b-a));
}

arie_functie_1 = integrare_functie_1(0, 1, 50000);
print(arie_functie_1);


#b)
integrare_functie_2 = function(D)
{
  sum = 0;
  
  for (i in 1: D)
  {
    u = rexp(1, 1);
    sum = sum + ((1/(1 + u*u))/(exp(-u)));
  }
  
  return (sum/D);
}

arie_functie_2 = integrare_functie_2(50000);
print(arie_functie_2);

#B4

server_request_timp = function()
{
  val_dist = c(rgamma(1, 5, 3), rgamma(1, 5, 2), rgamma(1, 4, 3), rgamma(1, 6, 4));
  
  prob_dist = c(0.35, 0.15, 0.20, 0.30);
  
  medie = 0;
  
  for (i in 1: 4)
  {
    medie = medie + prob_dist[c(i)]*val_dist[c(i)];
  }
  
  latenta = rexp(1, 4);
  
  return(medie + latenta);
}

server_request_timp_val = server_request_timp();
print(server_request_timp_val);

#B5

#infection probability p
p = 0.1
#number of chosen computers
k = 12
#probability of clean computer
q = 0.9

#subpunctul a

SYS_Infectare = function(nr_infecte)
{
  nr_curate = 20 - nr_infecte;
  
  for (i in 1:nr_infecte)
  {
    for (j in 1:nr_curate)
    {
      chanse = runif(1, 0, 1);
      
      if (chanse <= p)
      {
        nr_infecte = nr_infecte + 1;
        nr_curate = nr_curate - 1;
      }
    }
  }
  
  return(nr_infecte);
}

SYS_Curatare = function(nr_infecte)
{
  nr_of_cleaning_computers = min(nr_infecte, k)
  
  for (i in 1:nr_of_cleaning_computers)
  {
    chanse = runif(1, 0, 1);
    
    if (chanse < q)
    {
      nr_infecte = nr_infecte - 1
    }
  }
  
  return(nr_infecte);
}


SYS_situation_nr_zile = function()
{
  day = 1;
  nr_infecte = 3
  
  while (nr_infecte != 0)
  {
    day = day + 1;
    
    nr_infecte = SYS_Infectare(nr_infecte);
    nr_infecte = SYS_Curatare(nr_infecte);
    
    if (nr_infecte == 20) return -1;
  }
  
  return(day);
}

SYS_MonteCarlo=function(nr)
{
  sum=0
  for(i in 1:nr)
  {
    sum=sum+SYS_situation_nr_zile()
  }
  return(sum/nr)
}

print(SYS_MonteCarlo(500))

#subpunctul b

SYS_situation_10_zile = function()
{
  nr_infecte = 3
  
  for (i in 2:10)
  {
    nr_infecte = SYS_Infectare(nr_infecte);
    nr_infecte = SYS_Curatare(nr_infecte);
  }
  
  if (nr_infecte == 0)
  {
    return(1);
  }
  else
  {
    return(0);
  }
}

SYS_MonteCarlo_10_zile=function(nr)
{
  sum=0
  for(i in 1:nr)
  {
    sum=sum+SYS_situation_10_zile()
  }
  return(sum/nr)
}

print(SYS_MonteCarlo_10_zile(500))

#subpunctul c

alfa=1-0.95
z=qnorm(alfa/2)
epsilon=0.01
p=0.706
N_min=(1/4)*(z/epsilon)^2
N_min
SYS_MonteCarlo_10_zile(N_min+1)



