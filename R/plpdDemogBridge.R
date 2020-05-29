##' plpdDemogBridge
##'
##' links PRACTICE codes and CCG codes
##'
##' @param plpd prescription data.table
##' @param demogMap demogMap data.table
##' @param settings analysis settings generated with \emph{setConfig()}
##' 
##' @details Some records are not matched and this is due to the fact that PRactice level Prescribing data are more "generic" and include also GP that are not mapped by the CCG system.
##' 
##' @return the function returns a data table composed on 2 columns, one for PRACTICE code and one for CCG code. 
##'  Moreover, a message is returned, with the number of unmatched records. 
##' 
##' @export


plpdDemogBridge = function(plpd = NULL, demogMap = NULL, settings = NULL){
  

  
  setnames(demogMap,"PRACTICE_CODE","PRACTICE")
  on.exit(setnames(demogMap,"PRACTICE","PRACTICE_CODE",skip_absent=TRUE))
  data = base::merge(plpd,demogMap,by="PRACTICE",all.x = T)[,mget(c("PRACTICE","ONS_CCG_CODE"))]
  setnames(demogMap,"PRACTICE","PRACTICE_CODE")
  
  message(paste0(length(data[is.na(ONS_CCG_CODE),unique(PRACTICE)])," unique PRACTICE code unmatched"))
  
  data=unique(data)
  
  return(data)
}
