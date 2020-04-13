##' plpdDemogBridge
##'
##' creates summaries at regional level
##'
##' @param plpd prescription data.table
##' @param demogMap demogMap data.table
##' @param settings name of settings variable.
##' @export


plpdDemogBridge = function(plpd = NULL, demogMap = NULL, settings = NULL){

  data = merge.data.table(plpd,demogMap,by.x="PRACTICE", by.y = "PRACTICE_CODE",all.x = T)[,mget(c("PRACTICE","ONS_CCG_CODE"))]

  message(paste0(length(data[is.na(ONS_CCG_CODE),unique(PRACTICE)])," unique PRACTICE code unmatched"))
  
  return(data)
}
