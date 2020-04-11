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
  
  data = merge.data.table(plpd, bnf,by.x = "BNF.CODE", by.y = "BNF.Presentation.Code",all.x = T)
  
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
    select(PERIOD,measures) %>%
    group_by_at(groupVar) %>%
    summarise(ITEMS = sum(ITEMS), NIC = sum(NIC), ACT.COST = sum(ACT.COST), QUANTITY = sum(QUANTITY))%>%
    data.table %>%
    cbind(REGION = settings$region)%>%
    mutate(PERIOD = paste0(stringr::str_pad(settings$month,2,"left",0),settings$year))%>%
    data.table
  

}
