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
