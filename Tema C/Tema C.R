#C1

get_unused_colums = function (tabel, n)
{
  #elimin coloanele luate de celelalte regine
  refacuta = 1:n;
  refacuta = refacuta[!refacuta %in% tabel];
  
  return(refacuta)
}

n = 8

unused_colums = 1:8

selected_colums = vector()

index = 1

while (length(unused_colums) != 0)
{
  column = sample(unused_colums, 1);
  
  if (length(unused_colums) == 1)
  {
    column = unused_colums[1];
  }
  
  selected_colums[index] = column;
  
  index = index + 1;
  
  unused_colums = get_unused_colums(selected_colums, n);
}

if(length(selected_colums) < n)
{
  print("Failure")
} else
{
  print("Succes")
  print(selected_colums)
}



#C4

MonteCarloPrim=function(n)
{
  p=n-1
  r=0
  
  while((p%%2)==0)
  {
    p=p/2
    r=r+1
  }
  
  a=sample(1:(n-1),1)
  
  y=((a^p)%%n)
  
  i = 1;
  
  while (i <= r)
  {
    if(((y^2)%%n)==1&&y!=1&&y!=n-1)
    {
      return(FALSE)
    }
    
    y=(y^2)%%n
    
    i=i+1;
  }
  
  if(y!=1)
  {
    return(FALSE)
  }else
  {
    return(TRUE)
  }
}

MonteCarloPrim(113)