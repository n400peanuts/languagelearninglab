#summarySEwithin2

## this function is a wrapper around summarySEwithin2, from http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/#Helper above
## It takes a dataframe and uses summarySEwithin2 to get the group means and variances for conditions and merges these into the original dataframe

summarySEwithin2 <- function(data = NULL,
                             measurevar,
                             betweenvars = NULL,
                             withinvars = NULL,
                             idvar = NULL,
                             na.rm = FALSE,
                             conf.interval = .95,
                             .drop = TRUE) {
  data2 = summarySEwithin(
    data = data,
    measurevar = measurevar,
    betweenvars = betweenvars,
    withinvars = withinvars,
    idvar = idvar,
    na.rm = na.rm,
    conf.interval = conf.interval,
    .drop = .drop
  )
  colnames(data2)[colnames(data2) == measurevar] <- "groupmean"
  out = merge(data, data2)
  return(out)
}


# stat_string
# this function takes summary output from addBf_ranges3 and the name of a coefficient from that summary and formats so that can be reported as a string in the main text.  Default is to round all stats to 2pd, apart from p values which are rounded to 3dp (as per APA format)). Beta, SE, z p, Bayes factor (with (0,H1) as a subscript, where H1 is the predicted of H1) and Robustness regions are all reported
#note that as this stands, if the ranges are strings (e.g. ">4.5") they won't be rounded
stat_string <- function(Bfrange,
                        coefficientName,
                        dpbeta = 2,
                        dpse = 2,
                        dpz = 2,
                        dpp = 3,
                        dpseH1 = 2,
                        dpB = 3,
                        dpRRmin = 2,
                        dpRRmax = 2)
{
  beta = subset(Bfrange, coefficient == coefficientName)$estimate
  se = subset(Bfrange, coefficient == coefficientName)$std.Error
  z = subset(Bfrange, coefficient == coefficientName)$z
  p = subset(Bfrange, coefficient == coefficientName)$p
  seH1 = subset(Bfrange, coefficient == coefficientName)$sdtheory
  B = subset(Bfrange, coefficient == coefficientName)$Bf
  RRmin = subset(Bfrange, coefficient == coefficientName)$RRmin
  RRmax = subset(Bfrange, coefficient == coefficientName)$RRmax
  
  
  pstring = if (p < .001) {
    ", _p_ <.001"
  } else {
    paste(", _p_ =", round(p, dpp))
  }
  
  if (B > 1000) {
    B = signif(B, digits = dpB)
  }
  else {
    B = round(B, dpB)
  }
  outstring = paste(
    "_&beta;_ = ",
    round(beta, dpbeta),
    ", _SE_ = ",
    round(se, dpse),
    ", _z_ = " ,
    round(z, dpz),
    pstring ,
    ", _B_",
    "~(0,",
    round(seH1, dpseH1),
    ")~",
    " = ",
    signif(B, digits = dpB),
    ", _RR_ = [",
    RRmin,
    ",",
    RRmax,
    "]",
    sep = ""
  )
  return(outstring)
}