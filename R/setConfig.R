##' setConfig
##'
##' creates a list of configuration settings that will be used across the package
##'
##' @param rootdir path to an existing working directory
##' @param year numeric. Year of interest
##' @param month numeric. Month of interest, one of 2018 and 2019
##' @param region char. Region of analysis
##' 
##' @return The function returns a list containing the following information, as relative to the month and year selected:
##'  - rootdir
##'  - year
##'  - month
##'  - region
##'  - plpdName
##'  - demogName
##'  - demogMapName 
##'  - bnfName
##'  - qofNames
##'  
##' @export

setConfig <- function(rootdir, year, month, region){
  
  if(!as.character(year)%in%c("2018","2019")){
    stop(paste0("Only 2018 and 2019 implemented. Please select one of these years"))
  }
  
  month=as.character(stringr::str_pad(month,width = 2,side = "left",pad = "0"))
  prefix = paste0(year,month)
  
  plpdName = paste0("plpd_",prefix)
  demogName = paste0("demog_",prefix)
  demogMapName = paste0("demogMap_",prefix)
  
  bnfName = switch(as.character(year),
                   "2018" = "bnf_201901",
                   "2019" = "bnf_202001")
  
  qofprefix = switch(as.character(year),
                     "2018" = "qofGP_1819",
                     "2019" = "qofGP_1819")
  
  qofNames = paste0(qofprefix,c("_CardioVascular","_dependency",
                                "_lifestyle","_mental","_muscul","_respiratory"))
  

  
  config = list(rootdir = rootdir, year = year, month = month, region = region,
                plpdName = plpdName, demogName = demogName, demogMapName = demogMapName, bnfName = bnfName,
                qofNames = qofNames)
  
  
  return(config)

}
