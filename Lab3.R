l = function(a, b, N)
{
  k = 0;
  for (i in 1: N)
  {
    x = runif(1, 0);
    
    if (a < x && x < b)
    {
      k = k + 1;
    }
  }
  
  return (k/N);
}

disc_area = function (N)
{
  nc = 0;
  
  for (i in 1: N)
  {
    x = runif(1, -1, 1);
    y = runif(1, -1, 1);
    if (x*x + y*y <= 1)
      nc = nc + 1
  }
  
  return(4*nc/N);
}


v_bila = function (N)
{
  nc = 0;
  
  for (i in 1: N)
  {
    x = runif(1, -1, 1);
    y = runif(1, -1, 1);
    z = runif(1, -1, 1);
    if (x*x + y*y + z*z <= 1)
      nc = nc + 1
  }
  
  return(8*nc/N);
}

v = v_bila(100000);
v
4*pi/3
era=abs(v - 4*pi/3)
era
eral=era/(4*pi/3)
eral




para_arie = function (N)
{
  nc = 0;
  
  for (i in 1: N)
  {
    x = runif(1, 0, 2);
    y = runif(1, 0, 2);

    if (y <= -2*x*x + 5*x - 2)
      nc = nc + 1
  }
  
  return(4*nc/N);
}

para_arie(50000)


MC_integration = function (N)
{
  sum = 0;
  
  for (i in i:N)
  {
    u = runif(1, 0, 10);
    sum = sum + exp(-u*u/2);
  }
  
  return(10*sum/N)
}

MC_integration(10000)


MC_d = function(N)
{
  sum = 0;
  for (i in 1:N)
  {
    u = rexp(1, 1);
    sum=sum+f(u)/exp(-u);
  }
  return (sum/N);
}

MC_d(5000)
