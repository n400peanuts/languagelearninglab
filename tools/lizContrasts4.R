lizContrasts4= function(d, condition, baselevel) 
{
  #condition = pictest$condition
  condition = factor(condition)
  condition = relevel(condition, baselevel)
  a= (contrasts(condition)-apply(contrasts(condition),2,mean))
  
  d$dummy1[condition== rownames(a)[1]] <- a[1] 
  d$dummy1[condition== rownames(a)[2]] <- a[2] 
  d$dummy1[condition== rownames(a)[3]] <- a[3] 
  d$dummy1[condition== rownames(a)[4]] <- a[4] 
  d$dummy2[condition== rownames(a)[1]] <- a[5] 
  d$dummy2[condition== rownames(a)[2]] <- a[6] 
  d$dummy2[condition== rownames(a)[3]] <- a[7] 
  d$dummy2[condition== rownames(a)[4]] <- a[8] 
  d$dummy3[condition== rownames(a)[1]] <- a[9] 
  d$dummy3[condition== rownames(a)[2]] <- a[10] 
  d$dummy3[condition== rownames(a)[3]] <- a[11] 
  d$dummy3[condition== rownames(a)[4]] <- a[12] 
  
  name1 = paste(baselevel, rownames(a)[2],sep="_VERSUS_")
  name2 = paste(baselevel, rownames(a)[3],sep="_VERSUS_")
  name3 = paste(baselevel, rownames(a)[4],sep="_VERSUS_")
  
  d[name1] = d$dummy1 
  d[name2] = d$dummy2 
  d[name3] = d$dummy3 
  
  d$dummy1 <-NULL 
  d$dummy2 <-NULL 
  d$dummy3 <-NULL 
  
  return(d)
}