Bf_range_updated <- function(sd, obtained, dfdata = 1, likelihood = c("normal", "t"), 
         modeloftheory= c("normal","t","cauchy") ,
         modeoftheory = 0, sdtheoryrange, dftheory = 1, tail = 1)
{
  x = c(0)
  y = c(0)
  
  for(sdi in sdtheoryrange)
  {
    #sdi = sdtheoryrange[1]
    B = as.numeric(Bf(sd = sd, obtained = obtained, dfdata = dfdata, likelihood = likelihood, 
                      modeloftheory = modeloftheory, modeoftheory=modeoftheory, scaleoftheory=sdi, 
                      dftheory = dftheory, tail = tail))
  
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
