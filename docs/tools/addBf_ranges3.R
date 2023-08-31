addBf_ranges3 = function(Bf_df, stepsizes, maxs, method = "old") {
  
  if(method=="old"){  
    RRminV = vector()
    RRmaxV = vector()
    for (b in 1:dim(Bf_df)[1]){
      
      if (length(stepsizes)==1){stepsize = stepsizes} else {stepsize = stepsizes[b]}
      if (length(maxs)==1){max = maxs} else {max = maxs[b]}
      
      
      Ndp = num.decimals(stepsize)
      BF = as.numeric(as.character(Bf_df$Bf[b]))  
      BType= BfClassify(BF) 
      sdtheory = as.numeric(as.character(Bf_df$sdtheory[b]))  
      sd = as.numeric(as.character(Bf_df$std.Error[b]))
      obtained = as.numeric(as.character(Bf_df$estimate[b]))
      tail = as.numeric(as.character(Bf_df$BFtail[b]))
      
      # get the max RRmax 
      RRmax = ""
      lastsdtheory = sdtheory
      newsdtheory = ""
      while(newsdtheory<= max){
        newsdtheory = lastsdtheory + stepsize
        newBF = Bf(sd=sd, obtained=obtained, uniform = NULL, meanoftheory=0,sdtheory=newsdtheory, tail=tail)$BayesFactor
        
        if (BfClassify(newBF)!= BType) {
          RRmax = format(round(lastsdtheory, Ndp))
          break }
        lastsdtheory = newsdtheory
      }
      
      if(RRmax == "" & BType == "h0") {RRmax = "inf"}  
      if(RRmax == "" & BType == "h1") {RRmax = paste(">", max, sep="")}  
      if(RRmax == "" & BType == "ambig") {RRmax = paste(">", max, sep="")}  
      
      # get the min RRmin 
      RRmin = ""
      lastsdtheory = sdtheory
      newsdtheory = lastsdtheory - stepsize
      
      while(newsdtheory > 0){
        newBF = Bf(sd=sd, obtained=obtained, uniform = NULL, meanoftheory=0,sdtheory=newsdtheory, tail=tail)$BayesFactor
        
        if (BfClassify(newBF)!= BType) {
          RRmin = format(round(lastsdtheory, Ndp))
          break }
        lastsdtheory = newsdtheory
        newsdtheory = lastsdtheory-stepsize
      }
      
      if(RRmin== "" & BType == "ambig") {RRmin = 0}  
      if(RRmin== "" & BType == "h1") {RRmin = paste(" 0< & <", format(round(lastsdtheory,  Ndp), nsmall=Ndp), sep="")} 
      if(RRmin== "" & BType == "h0") {RRmin = paste(" 0< & <", format(round(lastsdtheory,  Ndp), nsmall=Ndp), sep="")}  
      
      RRminV[b] = RRmin
      RRmaxV[b] = RRmax
      
    }
    out = cbind(Bf_df, RRminV, RRmaxV)
    
    colnames(out)[10:11]= c("RRmin","RRmax")
    
    return(out)
  }
  if(method=="new"){  
    RRminV = vector()
    RRmaxV = vector()
    for (b in 1:dim(Bf_df)[1]){
      
      
      if (length(stepsizes)==1){stepsize = stepsizes} else {stepsize = stepsizes[b]}
      if (length(maxs)==1){max = maxs} else {max = maxs[b]}
      
      
      Ndp = num.decimals(stepsize)
      BF = as.numeric(as.character(Bf_df$Bf[b]))  
      BType= BfClassify(BF) 
      sdtheory = as.numeric(as.character(Bf_df$sdtheory[b]))  
      sd = as.numeric(as.character(Bf_df$std.Error[b]))
      obtained = as.numeric(as.character(Bf_df$estimate[b]))
      tail = as.numeric(as.character(Bf_df$BFtail[b]))
      
      # get the max RRmax 
      RRmax = ""
      lastsdtheory = sdtheory
      newsdtheory = ""
      while(newsdtheory<= max){
        newsdtheory = lastsdtheory + stepsize
        if(colnames(Bf_df)[4]=="t"){
          dfdata=as.numeric(as.character(Bf_df$df[b]))
          newBF = Bf(sd=sd, obtained=obtained, dfdata=dfdata, likelihood = c("t"), modeloftheory= c("normal"),modeoftheory=0,scaleoftheory=newsdtheory, tail=tail, method="new")
        }
        if(colnames(Bf_df)[4]=="z"){
          dfdata=as.numeric(as.character(Bf_df$df[b]))
          newBF = Bf(sd=sd, obtained=obtained,  likelihood = c("normal"), modeloftheory= c("normal"),modeoftheory=0,scaleoftheory=newsdtheory, tail=tail, method="new")
        }
        
        if (BfClassify(newBF)!= BType) {
          RRmax = format(round(lastsdtheory, Ndp))
          break }
        lastsdtheory = newsdtheory
      }
      
      if(RRmax == "" & BType == "h0") {RRmax = "inf"}  
      if(RRmax == "" & BType == "h1") {RRmax = paste(">", max, sep="")}  
      if(RRmax == "" & BType == "ambig") {RRmax = paste(">", max, sep="")}  
      
      # get the min RRmin 
      RRmin = ""
      lastsdtheory = sdtheory
      newsdtheory = lastsdtheory - stepsize
      
      while(newsdtheory > 0){
        if(colnames(Bf_df)[4]=="t"){
          dfdata=as.numeric(as.character(Bf_df$df[b]))
          newBF = Bf(sd=sd, obtained=obtained, dfdata=dfdata, likelihood = c("t"), modeloftheory= c("normal"),modeoftheory=0,scaleoftheory=newsdtheory, tail=tail, method="new")
        }
        
        
        if(colnames(Bf_df)[4]=="z"){
          dfdata=as.numeric(as.character(Bf_df$df[b]))
          newBF = Bf(sd=sd, obtained=obtained,  likelihood = c("normal"), modeloftheory= c("normal"),modeoftheory=0,scaleoftheory=newsdtheory, tail=tail, method="new")
        }
        
        if (is.nan(newBF)) {
          RRmin = format(round(lastsdtheory, Ndp))
          break }
        if (BfClassify(newBF)!= BType) {
          RRmin = format(round(lastsdtheory, Ndp))
          break }
        lastsdtheory = newsdtheory
        newsdtheory = lastsdtheory-stepsize
      }
      
      if(RRmin== "" & BType == "ambig") {RRmin = 0}  
      if(RRmin== "" & BType == "h1") {RRmin = paste(" 0< & <", format(round(lastsdtheory,  Ndp), nsmall=Ndp), sep="")} 
      if(RRmin== "" & BType == "h0") {RRmin = paste(" 0< & <", format(round(lastsdtheory,  Ndp), nsmall=Ndp), sep="")}  
      
      RRminV[b] = RRmin
      RRmaxV[b] = RRmax
      
    }
    out = cbind(Bf_df, RRminV, RRmaxV)
    
    colnames(out)[10:11]= c("RRmin", "RRmax")
    
    return(out)
  }
}  
