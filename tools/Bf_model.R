Bf_model = function(coeff_summary, coeff_list, h1_list, 
                    h1_motivation, tail_list, digits = "", method = "old") {
  if(method=="old") {
    Bfs = vector('double')
    estimates = vector('double')
    sterrors = vector('double')
    sdtheorys = vector('double')
    pvalues = vector('double')
    tzvalues = vector('double')
    
    
    #  i=1
    for (i in 1:length(coeff_list)) {
      
      sd_error = coeff_summary[coeff_list[i], "Std. Error"]
      obtained = coeff_summary[coeff_list[i], "Estimate"]
      stdtheory = h1_list[i]
      
      
      if(h1_list[i] < 0) {
        stdtheory = h1_list[i] * -1
        obtained = (coeff_summary[coeff_list[i], "Estimate"] * -1)
      }
      
      Bfs[i] = Bf(sd_error, obtained, uniform = 0, meanoftheory = 0, 
                  sdtheory = stdtheory, tail = tail_list[i])$BayesFactor
      estimates[i] = obtained
      sdtheorys[i] = stdtheory
      sterrors[i] = sd_error
      
      if(colnames(coeff_summary)[3] == "z value") {
        tzlabel = "z"
        tzvalues[i] = abs(coeff_summary[coeff_list[i], 3]) * sign(obtained)
        pvalues[i] = coeff_summary[coeff_list[i], 4]  
      } else if(colnames(coeff_summary)[3] == "df") {
        tzlabel = "t"
        tzvalues[i] = abs(coeff_summary[coeff_list[i], 4]) * sign(obtained)
        pvalues[i] = coeff_summary[coeff_list[i], 5]  
      } 
    }
    
    df = data.frame(cbind(coeff_list, estimates, sterrors, tzvalues, 
                          pvalues, sdtheorys,  tail_list, Bfs,h1_motivation ))
    colnames(df) = c("coefficient", "estimate", "std.Error", tzlabel, "p", 
                     "sdtheory", "BFtail", "Bf", "h1 motivation")
    df$estimate = as.numeric(as.character(df$estimate))
    df$std.Error = as.numeric(as.character(df$std.Error))
    df$sdtheory = as.numeric(as.character(df$sdtheory))
    
    df$Bf = as.numeric(as.character(df$Bf))
    df$p = as.numeric(as.character(df$p))
    if (tzlabel == "z") {df$z = as.numeric(as.character(df$z))}
    if (tzlabel == "t") {df$t = as.numeric(as.character(df$t))}
    if (digits !=  "") {df = round_df(df, digits)}
    return(df)
  } 
  
  if(method == "new") {
    Bfs = vector('double')
    estimates = vector('double')
    sterrors = vector('double')
    sdtheorys = vector('double')
    pvalues = vector('double')
    tzvalues = vector('double')
    dfvalues = vector('double')
    
    if(colnames(coeff_summary)[3] == "df"){tzlabel = "t"}
    if(colnames(coeff_summary)[3] == "z value"){tzlabel = "z"}
    
    for (i in 1:length(coeff_list)) {
      
      sd = coeff_summary[coeff_list[i], "Std. Error"]
      obtained = coeff_summary[coeff_list[i], "Estimate"]
      stdtheory = h1_list[i]
      
      if(h1_list[i] < 0) {
        stdtheory = h1_list[i] * -1
        obtained = (coeff_summary[coeff_list[i], "Estimate"] * -1)}
      
      
      if(tzlabel == "t") {
        dfdata = coeff_summary[coeff_list[i], "df"]  
        Bfs[i] = Bf(sd, obtained, dfdata, likelihood = c("t"), 
                    modeloftheory = c("normal"), modeoftheory = 0, 
                    scaleoftheory = stdtheory, tail = tail_list[i], 
                    method = "new")
      }
      if(tzlabel == "z") {
        Bfs[i] = Bf(sd, obtained, likelihood = c("normal"), 
                    modeloftheory = c("normal"), modeoftheory = 0, 
                    scaleoftheory = stdtheory, tail = tail_list[i], 
                    method = "new")
      }
      
      estimates[i] = obtained
      sdtheorys[i] = stdtheory
      sterrors[i] = sd
      
      if(tzlabel == "z") {
        
        tzvalues[i] = abs(coeff_summary[coeff_list[i], 3]) * sign(obtained)
        pvalues[i] = coeff_summary[coeff_list[i], 4]  
      } else if(tzlabel == "t") {
        dfvalues[i] = dfdata
        tzvalues[i] = abs(coeff_summary[coeff_list[i], 4]) * sign(obtained)
        pvalues[i] = coeff_summary[coeff_list[i], 5]  
      } 
    }
    
    if(tzlabel == "z") {
      df = data.frame(cbind(coeff_list, estimates, sterrors, tzvalues, pvalues, sdtheorys,  tail_list, Bfs, h1_motivation))
      colnames(df) = c("coefficient", "estimate", "std.Error", tzlabel, "p", "sdtheory", "BFtail", "Bf", "h1 motivation")
      
    }
    if(tzlabel == "t") {df = data.frame(cbind(coeff_list, estimates, sterrors, tzvalues, dfvalues, pvalues, sdtheorys,  tail_list, Bfs, h1_motivation))
    colnames(df) = c("coefficient", "estimate", "std.Error", tzlabel, "df", "p", "sdtheory", "BFtail", "Bf", "h1 motivation")
    
    }
    
    df$estimate = as.numeric(as.character(df$estimate))
    df$std.Error = as.numeric(as.character(df$std.Error))
    df$sdtheory = as.numeric(as.character(df$sdtheory))
    
    df$Bf = as.numeric(as.character(df$Bf))
    df$p = as.numeric(as.character(df$p))
    if (tzlabel == "z") {df$z = as.numeric(as.character(df$z))}
    if (tzlabel == "t") {df$t = as.numeric(as.character(df$t))}
    if (digits !=  "") {df = round_df(df, digits)}
    return(df)
  } 
}
