##' setConfig
##'
##' creates a list of configuration settings that will be used across the package
##'
##' @param year numeric. Year of interest
##' @param month numeric. Month of interest
##' @param basedir path to an existing working directory
##' 
##' @export

setConfig <- function(year, month, basedir){

  if(file.exists("config.yml")){
  config = config::get()
  config$year = year
  config$month = month
  config$basedir = basedir
  }else{
    config = list(year = year, month = month, basedir = basedir)
  }  
  return(config)

}
