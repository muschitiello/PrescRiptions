##' ccgSummaries
##'
##' creates aggregated values and calclate Cost rates at CCG level
##'
##' @param plpd practice level prescribing data.table
##' @param bnf bnf data.table
##' @param demog demog data.table
##' @param demogMap demogMap data.table
##' @param settings analysis settings generated with \emph{setConfig()}
##' 
##' @return a summary data.table
##' 
##' @export



ccgSummaries = function(plpd,bnf,demog,demogMap,settings){
  
  dirs = dirsGen(settings)
  month=as.character(stringr::str_pad(settings$month,width = 2,side = "left",pad = "0"))
  year = settings$year
  rootdir = settings$rootdir

  bnf = data.table(bnf)
  plpd = data.table(plpd)
  demogMap = data.table(demogMap)
  demog=data.table(demog)
  
  integers2numericBnf = c("BNF.Chapter.Code","BNF.Section.Code",
                       "BNF.Paragraph.Code","BNF.Subparagraph.Code")
  integers2numericplpd = c("ITEMS","QUANTITY","PERIOD","NIC","ACT.COST")

  setDT(demogMap)[,(colnames(demogMap)):=lapply(.SD, function(x) as.character(x)),
             .SDcols = colnames(demogMap)]
  setDT(demog)[,(colnames(demog)):=lapply(.SD, function(x) as.character(x)),
                  .SDcols = colnames(demog)]
  
  
  setDT(bnf)[,(colnames(bnf)):=lapply(.SD, function(x) as.character(x)),
             .SDcols = colnames(bnf)]
  
  setDT(bnf)[,(integers2numericBnf):=lapply(.SD, function(x) as.numeric(x)),
             .SDcols = integers2numericBnf]
  
  setDT(plpd)[,(integers2numericplpd):=lapply(.SD, function(x) as.numeric(x)),
             .SDcols = integers2numericplpd]

  demog[,ONS_CODE:=as.character(ONS_CODE)]
  demog[,NUMBER_OF_PATIENTS:=as.numeric(NUMBER_OF_PATIENTS)]
  
  practiceBridge = suppressMessages(PrescRiptions::plpdDemogBridge(plpd, demogMap))
  
  #### CCG-level aggregation
  
  plpd_ccg <- data.table(dplyr::left_join(plpd, practiceBridge, by = c("PRACTICE")))
  plpd_ccg=plpd_ccg[!is.na(ONS_CCG_CODE)]
  
  # set relevant Variables (the same in both cases)
  
  measures <- c("ITEMS", "NIC", "ACT.COST", "QUANTITY", "PERIOD", "ONS_CCG_CODE", "BNF.CODE")
  groupVar <- c("PERIOD", "ONS_CCG_CODE", "BNF.CODE")
  
  ccg_summary <-  plpd_ccg %>%
    dplyr::select(PERIOD,measures) %>%
    dplyr::group_by_at(groupVar) %>%
    dplyr::summarise(ITEMS = sum(ITEMS), NIC = sum(NIC), ACT.COST = sum(ACT.COST), QUANTITY = sum(QUANTITY))%>%
    data.table # CCG level prescription data
  
  ccg_bnf <- data.table(dplyr::left_join(ccg_summary, bnf, by = c("BNF.CODE"="BNF.Presentation.Code")))
  
  groupVar <- c("PERIOD", "ONS_CCG_CODE", "BNF.Chapter")
  
  ccg_bnf <-  ccg_bnf %>%
    dplyr::select(PERIOD,measures, BNF.Chapter, BNF.Chapter.Code) %>%
    dplyr::group_by_at(groupVar) %>%
    dplyr::summarise(ITEMS = sum(ITEMS), NIC = sum(NIC), ACT.COST = sum(ACT.COST), QUANTITY = sum(QUANTITY))%>%
    data.table # CCG level prescription data by BNF chapter
  
  groupVar <- c("PERIOD", "ONS_CCG_CODE")
  
  ccg_summary <-  ccg_summary %>%
    dplyr::select(PERIOD,measures) %>%
    dplyr::group_by_at(groupVar) %>%
    dplyr::summarise(ITEMS = sum(ITEMS), NIC = sum(NIC), ACT.COST = sum(ACT.COST), QUANTITY = sum(QUANTITY))%>%
    data.table # CCG level prescription data by BNF chapter
  
  
#### Rates
  
  demog <- demog %>% 
    dplyr::filter(SEX == "ALL", AGE_GROUP_5 == "ALL") %>%
    dplyr::select(ONS_CODE, NUMBER_OF_PATIENTS) %>%
    data.table
  
  measures <- c("ITEMS", "NIC", "ACT.COST", "QUANTITY", "PERIOD", "ONS_CCG_CODE")
  groupVar <- c("PERIOD", "ONS_CCG_CODE")
  
  ccg_aggr <-  plpd_ccg %>%
    dplyr::select(PERIOD,measures) %>%
    dplyr::group_by_at(groupVar) %>%
    dplyr::summarise(ITEMS = sum(ITEMS), NIC = sum(NIC), ACT.COST = sum(ACT.COST), QUANTITY = sum(QUANTITY))%>%
    data.table # CCG level prescription data
  
  ccg_aggr <- data.table(dplyr::left_join(ccg_aggr, demog, by = c("ONS_CCG_CODE" = "ONS_CODE")))
  
  ccg_aggr[,NIC_RATE:=(NIC/NUMBER_OF_PATIENTS)/1000]

  
  return(list(ccg_summary=ccg_summary,ccg_bnf=ccg_bnf,ccg_aggr=ccg_aggr))

}
