selectCenter= function(x, listfname) 
{
  for (i in 1:length(listfname)) 
  {
    fname = as.character(listfname[i])
    x[paste(fname,".ct", sep="")] = myCenter(x[fname])
  }
  
  return(x)
}