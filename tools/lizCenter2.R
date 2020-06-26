# This function provides a wrapper around lizCenter which removes rows with missing data in the the DV before centering- note the df will have less rows as well as teh 
# cnetered variables (so will need to be careful with this is working with multiple dvs)
#   * dv - a string which is the column name of the column in the dataframe with the dv
#   * x: data frame
#   * listfname: a list of the variables to be centered (e.g. list(variable1,variable2))
# The output is a copy of the data frame with a column (always a numeric variable) added for each of the centered variables. These columns are labelled with the column's previous name, but with ".ct" appended (e.g., "variable1" will become "variable1.ct").




lizCenter2= function(x, listfname, dv) 
{
  x= droplevels(subset(x, is.na(x[as.character(dv)])==FALSE))
  x = lizCenter(x, listfname)
  
  
  return(x)
}