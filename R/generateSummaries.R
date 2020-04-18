##' generateSummaries
##'
##' creates summaries at regional level
##'
##' @param plpd prescription data.table
##' @param bnf bnf data.table
##' @param on one of \emph{"REGION"} or \emph{"BNF"} indicating the variables to summarise on. Default is "REGION"
##' @param settings name of settings variable.
##' @export


generateSummaries = function(plpd = NULL, bnf = NULL, on = "REGION",settings = NULL){
  
  setnames(bnf,"BNF.Presentation.Code","BNF.CODE")
  data = base::merge(plpd, bnf,by = "BNF.CODE" ,all.x = T)
  setnames(bnf,"BNF.CODE","BNF.Presentation.Code")
  
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
    dplyr::mutate(PERIOD = paste0(stringr::str_pad(settings$month,2,"left",0),settings$year))%>%
    data.table
  

}
