round_df <- function(df, digits) {
  for(i in 1: ncol(df)){
    
    if (is.numeric(df[,i])) { 
      df[,i]= round(df[,i],digits) }
  }
  
  return(df)
}