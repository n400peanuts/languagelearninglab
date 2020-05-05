loadFunctionsGithub <-function(urlFolder, urlRaw){
  if (!require(httr)) {
    stop("httr not installed")
  } 
  else if (!require(RCurl)){
    stop("RCurl not installed") 
  }
  else {
    print('----loading. Please wait----')
  };
  httr::GET(urlFolder)-> req
  stop_for_status(req)
  filelist <- unlist(lapply(content(req)$tree, "[", "path"), use.names = F)
  urlFunctions <- grep("docs/tools/", filelist, value = TRUE, fixed = TRUE)
  gsub("docs/tools/", "", urlFunctions) -> functions
  for (i in 1:length(functions)){
    RCurl::getURL(paste0(urlRaw, functions[i]), ssl.verifypeer = FALSE)-> temp
    eval(parse(text = temp), envir = .GlobalEnv)
  };
}