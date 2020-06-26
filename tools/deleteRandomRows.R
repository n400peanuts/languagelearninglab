deleteRandomRows = function(df,n){
  indices <- sample(1:nrow(df), n)
  df <- df[-indices,]
  return(df)
}