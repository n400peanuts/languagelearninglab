Bf_set <-function(names_list, meandiff_list, sd_list,  h1_list, tail_list)
{
  Bfs = vector('double')
  for (i in 1:length(meandiff_list)){
    Bfs[i] = Bf(sd_list[i], meandiff_list[i], uniform = 0, meanoftheory = 0, sdtheory=h1_list[i] , tail = tail_list[i])$BayesFactor
    
  }
  
  df = data.frame(names_list, cbind(round(meandiff_list,3), round(sd_list,3),  h1_list, round(Bfs,3)))
  colnames(df) = c("Contrast", "Mean difference", "SE", "H1 estimate", "BF" )
  return(df)
  
  kable(df)
}