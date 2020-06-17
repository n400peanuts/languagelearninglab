Bf_range<-function(sd, obtained, meanoftheory=0, sdtheoryrange, tail=1)
{
  
  x = c(0)
  y = c(0)
  
  for(sdi in sdtheoryrange)
  {
    #sdi = sdtheoryrange[1]
    B = as.numeric(Bf(sd, obtained, meanoftheory=0, uniform = 0, sdtheory=sdi, tail)[3])
    
    #following line corrects for the fact that the calcuator does not correctly compute BF when sdtheory==0; this code ensures that if sdtheory ==0, BF=1
    
    if (sdi ==0 ) {B=1}
    
    x= append(x,sdi)  
    y= append(y,B)
    output = cbind(x,y)
    
  } 
  output = output[-1,] 
  colnames(output) = c("sdtheory", "BF")
  return(output) 
}
