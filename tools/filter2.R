filter2 = function(im, svn, fn, lim)
{
  ## work out means lisfor each subject for each word
  
  x = list()
  y = ""
  
  for(n in svn) x=append(im[n],x)
  for(n in svn) y=paste(y,n,sep="_")
  
  means = aggregate(im[fn], by = x, mean, na.rm=T)
  head(means)
  nocols = dim(means)[2]
  colnames(means)[nocols] = "means"
  
  sds = aggregate(im[fn], by = x, sd, na.rm=T)
  head(sds)
  nocols = dim(sds)[2]
  colnames(sds)[nocols] = "sds"
  
  gs = merge(means,sds)
  
  ## because if there is just one value it doesn't have a stand deviation and don't want to just disregard all of these
  gs$sds[is.na(gs$sds)] = 0 
  
  gs$max = gs$means + lim*gs$sds
  gs$min = gs$means- lim*gs$sds
  
  im2 = merge(im,gs, sort=F)
  
  
  im2[paste(fn,"filt",sep="")] = im2[fn]
  cn= dim(im2)[2] ## get colnumber (last one added)
  
  im2[,cn][im2[,fn]> im2$max] = ""
  
  im2[,cn][im2[,fn]< im2$min] = ""
  
  im2[,cn]= as.numeric(im2[,cn])
  
  
  names(im2)[names(im2)=="means"] = paste("mean", y, sep="_") 
  names(im2)[names(im2)=="sds"] = paste("sd", y, sep="_") 
  names(im2)[names(im2)=="max"] = paste("max", y, sep="_") 
  names(im2)[names(im2)=="min"] = paste("min", y, sep="_") 
  
  return(im2)
}