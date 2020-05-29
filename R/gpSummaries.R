##' gpSummaries
##'
##' creates aggregated values and calclate Cost rates at GP level
##'
##' @param plpd practice level prescribing data.table
##' @param demog demog data.table
##' @param demogMap demogMap data.table
##' @param settings analysis settings generated with \emph{setConfig()}
##' 
##' @return a summary data.table
##' 
##' @export


gpSummaries = function(plpd,demog,demogMap,settings){
  
  dirs = dirsGen(settings)
  month=as.character(stringr::str_pad(settings$month,width = 2,side = "left",pad = "0"))
  year = settings$year
  rootdir = settings$rootdir

  plpd = data.table(plpd)

  integers2numericplpd = c("ITEMS","QUANTITY","PERIOD","NIC","ACT.COST")

  setDT(plpd)[,(integers2numericplpd):=lapply(.SD, function(x) as.numeric(x)),
             .SDcols = integers2numericplpd]

  demog[,ONS_CODE:=as.character(ONS_CODE)]
  demog[,ORG_CODE:=as.character(ORG_CODE)]
  
  practiceBridge = suppressMessages(PrescRiptions::plpdDemogBridge(plpd, demogMap))
  
  #### CCG-level aggregation
  
  plpd_ccg <- data.table(dplyr::left_join(plpd, practiceBridge, by = c("PRACTICE")))
  plpd_ccg=plpd_ccg[!is.na(ONS_CCG_CODE)]
  
  # set relevant Variables (the same in both cases)
  
  measures <- c("ITEMS", "NIC", "ACT.COST", "QUANTITY", "PERIOD", "ONS_CCG_CODE", "PRACTICE")
  groupVar <- c("PERIOD", "PRACTICE")
  
  gp_summary <-  plpd_ccg %>%
    dplyr::select(PERIOD,measures) %>%
    dplyr::group_by_at(groupVar) %>%
    dplyr::summarise(ITEMS = sum(ITEMS), NIC = sum(NIC), ACT.COST = sum(ACT.COST), QUANTITY = sum(QUANTITY))%>%
    data.table # CCG level prescription data
  
  demog <- demog %>% 
    dplyr::filter(ORG_TYPE == "GP",SEX == "ALL", AGE_GROUP_5 == "ALL") %>%
    dplyr::select(ORG_CODE, NUMBER_OF_PATIENTS) %>%
    data.table
  
  gp_summary <- data.table(dplyr::left_join(gp_summary, demog, by = c("PRACTICE" = "ORG_CODE")))
  gp_summary[,NIC_RATE:=(NIC/NUMBER_OF_PATIENTS)/1000]
  gp_summary[,NIC_RATE_Z:=scale(NIC_RATE)]

  return(gp_summary)

}
