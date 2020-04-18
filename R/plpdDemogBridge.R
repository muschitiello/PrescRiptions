##' plpdDemogBridge
##'
##' creates summaries at regional level
##'
##' @param plpd prescription data.table
##' @param demogMap demogMap data.table
##' @param settings name of settings variable.
##' @export


plpdDemogBridge = function(plpd = NULL, demogMap = NULL, settings = NULL){

  setnames(demogMap,"PRACTICE_CODE","PRACTICE")
  data = base::merge(plpd,demogMap,by="PRACTICE",all.x = T)[,mget(c("PRACTICE","ONS_CCG_CODE"))]
  setnames(demogMap,"PRACTICE","PRACTICE_CODE")
  
  message(paste0(length(data[is.na(ONS_CCG_CODE),unique(PRACTICE)])," unique PRACTICE code unmatched"))
  
  return(data)
}
