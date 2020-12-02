# function below
download_from_Gorilla <-function(output, url){
  if (!require(downloader)) {
    stop("downloader not installed")
  } else if (!require(keyring)){
    stop("keyring not installed") 
  } else {
    print("------ download -------")
  };
  
  #---------------------- get credentials and login ----------------------------#
  login <- list(
    email = keyring::key_list("Gorilla")[2], #extract username
    password = keyring::key_get("Gorilla", "eva.viviani@ox.ac.uk"))
  login_res <- POST("https://gorilla.sc/api/login", body = login, encode = "form")
  for (i in 1:length(url)){
    downloader::download(url[i], paste0(output, substring(as.character(url[i]),54,nchar(as.character(url[i])))))
  }
}
