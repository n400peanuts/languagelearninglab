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