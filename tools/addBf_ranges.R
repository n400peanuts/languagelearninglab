addBf_ranges <-function(Bf_df, sdtheoryrange)
{
  
  BFranges = vector()  
  for (b in 1:dim(Bf_df)[1]){
    range = Bf_range(sd=as.numeric(as.character(Bf_df$SE[b])), obtained=as.numeric(as.character(Bf_df$'Mean difference'[b])), meanoftheory=0, sdtheoryrange=sdtheoryrange)
    
    from_table = vector()
    to_table = vector()
    cat = vector()
    category_table = vector()
    
    for(i in 1:dim(range)[1]) {       # go through each value in the range and categorize it
      
      #categorize current BF 
      if (range[i,2] <= (1/3)) { 
        cat[i] = "H0"      ## below or equal to 1/3
      } else if (range[i,2] < 3) { ## NOT below or equal to 1/3, IS below 3 
        cat[i] = "ambig"
      } else {                ## NOT below or equal to 1/3, NOT below 3
        cat[i] = "substH1"
      }
      
      # adjust the table
      j = length(category_table) 
      
      if (i==1){                      # first one
        category_table[j+1] = cat[i]   
        from_table[j+1] = range[i,1]
        
      } else if (cat[i] != cat[i-1]) { # NOT the first one, IS one where need to start new range 
        to_table[j] = range[i-1,1]
        category_table[j+1] = cat[i]
        from_table[j+1] = range[i,1]
      } 
      
      if (i==dim(range)[1]){        # if its the last one, finish off the table
        to_table[j] = range[i,1]
      }
    }
    
    # go through the little table and turn it int a string of ranges  
    string = ""
    for(i in 1: length(category_table)){
      string = paste(string, category_table[i],":", round(from_table[i],3),"-", round(to_table[i],3))
    }
    
    BFranges[b] = string
  }
  out = cbind(Bf_df, BFranges)
  return(out)
}