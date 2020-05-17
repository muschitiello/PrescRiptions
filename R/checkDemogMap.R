##' checkDemogMap
##'
##' Checks if demogMap is coherent for the specified month
##' @details For some months, the order and label of demogMap file has been found to be different.
##' This function checks if it is the case and correct it, if needed
##'
##' @param demogMap settings with parameters
##' @param settings analysis settings
##' @return The function returns the file corrected
##' @export
##' 

checkDemogMap = function(demogMap,settings){
  
  nMonths = c("JAN","FEB","MAR","APR","MAJ","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
  
  
  
  EXTRACT_DATE = paste0("01",nMonths[settings$month],settings$year)
  
  if(!"PUBLICATION"%in%colnames(demogMap)){
    setnames(demogMap,c("CODE",
                        "NAME",
                        "POSTCODE",
                        "CCG_ONS_CODE",
                        "CCG_CODE",
                        "CCG_NAME",
                        "STP_ONS_CODE",
                        "STP_NAME",
                        "REGIONAL_LOCAL_OFFICE_ONS_CODE",
                        "REGIONAL_LOCAL_OFFICE_CODE",
                        "REGIONAL_LOCAL_OFFICE_NAME",
                        "REGION_ONS_CODE",
                        "REGION_CODE",
                        "REGON_NAME"),c("PRACTICE_CODE",
                                           "PRACTICE_NAME",
                                           "PRACTICE_POSTCODE",
                                           "ONS_CCG_CODE",
                                           "CCG_CODE",
                                           "CCG_NAME",
                                           "STP_CODE",
                                           "STP_NAME",
                                           "ONS_REGION_CODE",
                                           "REGION_CODE",
                                           "REGION_NAME",
                                           "ONS_COMM_REGION_CODE",
                                           "COMM_REGION_CODE",
                                           "COMM_REGION_NAME"))
    
    demogMap = data.table(demogMap)                                        
    demogMap[,PUBLICATION:="GP_PRAC_PAT_LIST"]
    demogMap[,EXTRACT_DATE:=EXTRACT_DATE]
    demogMap =  demogMap[,mget(c("PUBLICATION",	"EXTRACT_DATE",	"PRACTICE_CODE",	"PRACTICE_NAME",	
                                 "PRACTICE_POSTCODE", "ONS_CCG_CODE",	"CCG_CODE",	"CCG_NAME",	
                                 "STP_CODE",	"STP_NAME",	"ONS_REGION_CODE",
                                 "REGION_CODE",	"REGION_NAME",	"ONS_COMM_REGION_CODE",	
                                 "COMM_REGION_CODE",	"COMM_REGION_NAME"))]
  }else{
    
    demogMap = demogMap
  }
  
  return(demogMap)
  
}
