##' setConfig
##'
##' creates a list of configuration settings that will be used across the package
##'
##' @param basedir path to an existing working directory
##' @param year numeric. Year of interest
##' @param month numeric. Month of interest
##' @param region char. Region of analysis
##' 
##' @export

setConfig <- function(basedir, year, month, region){

  if(file.exists("config.yml")){
  config = config::get()
  config$basedir = basedir
  config$year = year
  config$month = month
  config$region = region
  }else{
    config = list(basedir = basedir, year = year, month = month, region = region)
  }  
  return(config)

}
