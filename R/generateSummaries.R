##' generateSummaries
##'
##' creates costs and quantities aggregates at REGIONAL or BNF level
##'
##' @param plpd practice level prescribing data.table
##' @param bnf bnf data.table
##' @param on one of \emph{"REGION"} or \emph{"BNF"} indicating the variables to summarise on. Default is "REGION"
##' @param settings analysis settings generated with \emph{setConfig()}
##' 
##' @details The function aggregates costs and quantities. The "REGION" version creates total aggregates at region level
##' while the "BNF" option produces aggregates at BNF level.
##' @return the function returns a data.table
##' @export


generateSummaries = function(plpd,bnf, on = "REGION",settings){
  
  dirs = dirsGen(settings)
  month=as.character(stringr::str_pad(settings$month,width = 2,side = "left",pad = "0"))
  year = settings$year
  rootdir = settings$rootdir

  bnf = data.table(bnf)
  plpd = data.table(plpd)
  
  integers2numericBnf = c("BNF.Chapter.Code","BNF.Section.Code",
                       "BNF.Paragraph.Code","BNF.Subparagraph.Code")
  integers2numericplpd = c("ITEMS","QUANTITY","PERIOD")
  
  setDT(bnf)[,(integers2numericBnf):=lapply(.SD, function(x) as.numeric(x)),
             .SDcols = integers2numericBnf]
  setDT(plpd)[,(integers2numericplpd):=lapply(.SD, function(x) as.numeric(x)),
             .SDcols = integers2numericplpd]
  
  setnames(bnf,"BNF.Presentation.Code","BNF.CODE")
  data = base::merge(plpd, bnf,by = "BNF.CODE" ,all.x = T)
  setnames(bnf,"BNF.CODE","BNF.Presentation.Code")
  
  data[is.na(BNF.Chapter),BNF.Chapter:="Not Defined"]
  
  # set relevant Variables (the same in both cases)
  
  relVars = c("PERIOD", "BNF.Chapter", "BNF.Chapter.Code", 
              "BNF.Section", "BNF.Section.Code",
              "ITEMS", "NIC", "ACT.COST", "QUANTITY")
  

  data=data[,mget(relVars)]
  
  
  measures = switch(on,
                    "REGION" = c("ITEMS", "NIC", "ACT.COST", "QUANTITY"),
                    "BNF" = c("ITEMS", "NIC", "ACT.COST", "QUANTITY",
                              "BNF.Chapter", "BNF.Chapter.Code", "BNF.Section", "BNF.Section.Code")
  )  
  
  groupVar = switch(on,
                    "REGION" = "PERIOD",
                    "BNF" = "BNF.Chapter")


  data1 = data %>%
    dplyr::select(PERIOD,measures) %>%
    dplyr::group_by_at(groupVar) %>%
    dplyr::summarise(ITEMS = sum(ITEMS), NIC = sum(NIC), ACT.COST = sum(ACT.COST), QUANTITY = sum(QUANTITY))%>%
    data.table %>%
    cbind(REGION = settings$region)%>%
    dplyr::mutate(PERIOD = paste0(stringr::str_pad(month,2,"left",0),year))%>%
    data.table
  
  return(data1)

}
