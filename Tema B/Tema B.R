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
