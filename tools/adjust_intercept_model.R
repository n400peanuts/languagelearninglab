adjust_intercept_model<- function(model, chance, intercept_list = c("(Intercept)"))
{
  summary = summary(model)$coefficients
  
  for (i in 1:length(intercept_list)){
    summary[intercept_list[i], "Estimate"] = summary[intercept_list[i], "Estimate"]- chance
    summary[intercept_list[i], "z value"] =  summary[intercept_list[i], "Estimate"]/summary[intercept_list[i], "Std. Error"]
    summary[intercept_list[i], "Pr(>|z|)"] =  p =2*pnorm(-abs(summary[intercept_list[i], "z value"])) }
  
  
  summary[intercept_list[i], "Estimate"]
  
  
  return(summary)
  
}