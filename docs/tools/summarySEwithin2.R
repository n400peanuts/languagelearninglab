summarySEwithin2<- function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL,
                            idvar=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE){
  
  data2 = summarySEwithin(data=data, measurevar= measurevar, betweenvars=betweenvars, withinvars=withinvars,
                          idvar=idvar,na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  colnames(data2)[colnames(data2)==measurevar] <- "groupmean"
  out = merge(data,data2)
  return(out)
} 
