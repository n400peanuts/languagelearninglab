# this function is the same as stat_string except that z and p aren't reported

stat_string2 <-function(Bfrange, coefficientName, dpbeta=2,dpse=2,dpz=2, dpp=3, dpseH1=2,dpB=3, dpRRmin=2, dpRRmax=2)
{
  
  
  beta = subset(Bfrange,coefficient==coefficientName)$estimate
  se = subset(Bfrange,coefficient==coefficientName)$std.Error
  B = subset(Bfrange,coefficient==coefficientName)$Bf
  RRmin = subset(Bfrange,coefficient==coefficientName)$RRmin
  RRmax = subset(Bfrange,coefficient==coefficientName)$RRmax
  seH1 = subset(Bfrange,coefficient==coefficientName)$sdtheory
  
  
  
  if (B>1000) {B= signif(B,digits=dpB)}
  else {B= round(B,dpB)}  
  outstring = paste("_&beta;_ = ", 
                    round(beta,dpbeta), 
                    ", _B_", 
                    "~(0,",round(seH1,dpseH1), ")~",
                    " = ",
                    signif(B,digits=dpB),
                    ", _RR_ = [", 
                    RRmin,
                    ",",
                    RRmax,  
                    "]",
                    sep="")
  return(outstring)
}
