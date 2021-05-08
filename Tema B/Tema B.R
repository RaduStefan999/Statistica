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
  
  return (nr/D);
}

prob = volum_elipsoid(3, 2, 4, 50000);
sfera_statistica = prob*(8*3*2*4);
sfera_reala = ((4*pi*3*2*4)/3);

print(sfera_statistica);
print(sfera_reala);

