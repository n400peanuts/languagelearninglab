# updated version of Bf_range function that can be used with both old and new versions of Bf function
# by default, uses old version (for backward compatibility)
# if using new version, specify method = "new"

Bf_range <- function(sd, obtained, dfdata = 1, likelihood = c("normal", "t"), 
         modeloftheory= c("normal","t","cauchy") , meanoftheory = 0,
         modeoftheory = 0, sdtheoryrange, dftheory = 1, tail = 1, method = "old")
{
  if (method == "old") {
    
    x = c(0)
    y = c(0)
    
    for(sdi in sdtheoryrange)
    {
      #sdi = sdtheoryrange[1]
      # uses old Bf method
      B = as.numeric(Bf(sd, obtained, meanoftheory=0, uniform = 0, sdtheory=sdi, tail)[3])
      
      #following line corrects for the fact that the calcuator does not correctly compute BF when sdtheory==0; this code ensures that if sdtheory ==0, BF=1
      
      if (sdi ==0 ) {B=1}
      
      x= append(x,sdi)  
      y= append(y,B)
      output = cbind(x,y)

  }
    
  }
  else if (method == "new") {
    
    x = c(0)
    y = c(0)
    
    for(sdi in sdtheoryrange)
    {
      #sdi = sdtheoryrange[1]
      # uses new Bf method
      B = as.numeric(Bf(sd = sd, obtained = obtained, dfdata = dfdata, likelihood = likelihood, 
                        modeloftheory = modeloftheory, modeoftheory=modeoftheory, scaleoftheory=sdi, 
                        dftheory = dftheory, tail = tail, method="new"))
      
      #following line corrects for the fact that the calcuator does not correctly compute BF when sdtheory==0; this code ensures that if sdtheory ==0, BF=1
      
      if (sdi ==0 ) {B=1}
      
      x= append(x,sdi)  
      y= append(y,B)
      output = cbind(x,y)

    }
    
  }
  output = output[-1,] 
  colnames(output) = c("sdtheory", "BF")
  return(output) 
}
