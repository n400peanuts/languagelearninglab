Bf_powercalc<-function(sd, obtained, uniform, lower=0, upper=1, meanoftheory=0, sdtheory=1, tail=1, N, min, max)
{
  
  x = c(0)
  y = c(0)
  # note: working out what the difference between N and df is (for the contrast between two groups, this is 2; for constraints where there is 4 groups this will be 3, etc.)  
  for(newN in min : max)
  {
    B = as.numeric(Bf(sd = sd*sqrt(N/newN), obtained, uniform, lower, upper, meanoftheory, sdtheory, tail)[3])
    x= append(x,newN)  
    y= append(y,B)
    output = cbind(x,y)
    
  } 
  output = output[-1,] 
  return(output) 
}
