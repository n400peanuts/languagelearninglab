## this function takes a table output by the Bfs_ranges function (or the BF_model function and for any rows where the tails are set as 1 adjusts the pvalues to be one-tailed. This means that the tails for BFs and p values are consistent)  

add_p_tails_match_Bf <- function(Bfs.table){
  Bfs.table$p_tails_match_Bf=Bfs.table$p
  Bfs.table$p_tails_match_Bf[Bfs.table$BFtail==1 & Bfs.table$z>0 ]=Bfs.table$p[Bfs.table$BFtail==1 & Bfs.table$z>0 ]/2
  Bfs.table$p_tails_match_Bf[Bfs.table$BFtail==1 & Bfs.table$z<0 ]=1-Bfs.table$p[Bfs.table$BFtail==1 & Bfs.table$z<0 ]/2
  Bfs.table$p_tails_match_Bf[Bfs.table$BFtail==1 & Bfs.table$t>0 ]=Bfs.table$p[Bfs.table$BFtail==1 & Bfs.table$t>0 ]/2
  Bfs.table$p_tails_match_Bf[Bfs.table$BFtail==1 & Bfs.table$t<0 ]=1-Bfs.table$p[Bfs.table$BFtail==1 & Bfs.table$t<0 ]/2
  
  Bfs.table$p_tails_match_Bf=round(Bfs.table$p_tails_match_Bf,3)
  Bfs.table$p=Bfs.table$p_tails_match_Bf
  Bfs.table$p_tails_match_Bf=NULL
  return(Bfs.table)
}