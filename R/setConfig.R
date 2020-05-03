##' setConfig
##'
##' creates a list of configuration settings that will be used across the package
##'
##' @param rootdir path to an existing working directory
##' @param year numeric. Year of interest
##' @param month numeric. Month of interest
##' @param region char. Region of analysis
##' 
##' @export

setConfig <- function(rootdir, year, month, region){

  config = list(rootdir = rootdir, year = year, month = month, region = region)
  
  return(config)

}
