##' generateTS
##'
##' generate Time series for plpd and bnf data
##'
##' @param from start date in the format "mm-yyyy"
##' @param to end date in the format "mm-yyyy"
##' @param settings name of settings variable.
##' @param on one of \emph{"REGION"}, \emph{"BNF"} and \emph{"BOTH"} indicating which ts to produce,
##' default = "BOTH"
##' @param save logic, if TRUE, an external file will be saved in basedir. Default = TRUE
##' @details The function summaries plpd or bnf data on ITEMS, NIC, ACT.COST, QUANTITY and REGION
##' and generates TS over the period of time indicated trought the \emph{from} and \emph{to} arguments
##' @export


generateTS = function(from = "mm/yyyy", to = "mm/yyyy", settings = NULL, on = c("BOTH"), save = TRUE){
  
  if(!save%in%c(TRUE,FALSE)){
    stop("'save' must be logic: TRUE or FALSE")
  }
  
  onAll = c("REGION","BNF","BOTH")
  
  if(any(!on%in%onAll)){
    stop(paste0("Error in 'on' Admitted values are: ",paste0(onAll, collapse = ", ")," quoted"))
  }
  
  if(length(on)>1 & "BOTH" %in% on){
    on = "BOTH"
  }
  
  if(class(from)!="character"|class(to)!="character"){
    stop("from and to must be character vectors")
  }
  
  if(grepl("/",substr(from,1,2))|grepl("/",substr(to,1,2))){
    stop("Errore. Il formato mese deve essere 'mm'")}
  
  fmm = as.numeric(substr(from,1,2))
  fyy = as.numeric(substr(from,4,7))
  
  tmm = as.numeric(substr(to,1,2))
  tyy = as.numeric(substr(to,4,7))
  
  # starting checks
  
  if(!all(c(fmm,tmm) %in% 1:12)){
    stop("Error in month format")
  }
  
  if(!all(c(fyy,tyy) %in% 2018:2019)){
    stop("Error in year: only years 2018 and 2019 admitted")
  }
  
  fromDate = as.numeric(paste0(fyy,stringr::str_pad(fmm,2,"left",0)))
  toDate = as.numeric(paste0(tyy,stringr::str_pad(tmm,2,"left",0)))
  
  if(fromDate>toDate){
    stop("'from' date must be prior to 'to' date")
  }
  
  REGION_TF = FALSE
  BNF_TS = FALSE
  
  if(on == "REGION"){
    REGION_TF = TRUE
  }
  if(on=="BNF"){
    BNF_TF = TRUE
  }
  if(on=="BOTH"){
    REGION_TF = TRUE
    BNF_TF = TRUE
  }
  
  
  # create vector of times to download 
  
  times = data.table::data.table(expand.grid(2018:2019,stringr::str_pad(1:12,2,"left",0)))[
    order(Var1)][,times:=paste0(Var1,Var2)][,mget(c("Var2","Var1","times"))]
  data.table::setnames(times,colnames(times),c("mm","yyyy","times"))
  
  times2extract = times[data.table::between(times,fromDate,toDate)]
  
  # extract bnf data
  message("Import bnf data: ")
  year2extract = times2extract[,unique(yyyy)]
  times2extractBnf=times[yyyy %in%year2extract &  mm=="01"]
  for (t in times2extractBnf[,times]){
    assign("bnfData",
           monthlyData_import(year = times2extractBnf[times==t,yyyy], month = times2extractBnf[times==t,mm],whichData = "bnf"))
    
    for (i in 1:length(bnfData)){
      print(names(bnfData)[i])
      assign(names(bnfData)[i],bnfData[[i]])
    }
  }
  rm(bnfData)
  
  message("Import plpd data and generate TS: ")
  for (t in times2extract[,times]){
    print(paste0(times2extract[times==t,yyyy]," - ",times2extract[times==t,mm]))
    assign("plpdData",
           monthlyData_import(year = times2extract[times==t,yyyy], month = times2extract[times==t,mm],whichData = "plpd"))
    
    for (i in 1:length(plpdData)){
      if(i==1){print(names(plpdData)[i])}
      assign(names(plpdData)[i],plpdData[[i]])
    }
    
    bnfDat = switch(as.character(times2extract[times==t,yyyy]),
                     "2018" = "bnf201901",
                     "2019" = "bnf202001"
    )
    bnfDat=mget(bnfDat)
    
    plpdDat = names(plpdData)[1]
    plpdDat = mget(plpdDat)
    
    if(REGION_TF){
      if(exists("regionPresc_ts")){
      regionPresc_ts = rbind(regionPresc_ts,generateSummaries(plpdDat[[1]],bnfDat[[1]],on = "REGION",
                                                              year = times2extract[times==t,yyyy],
                                                              month = times2extract[times==t,mm]))
    }else{
      regionPresc_ts = generateSummaries(plpdDat[[1]],bnfDat[[1]],on = "REGION",
                                                        year = times2extract[times==t,yyyy],
                                                        month = times2extract[times==t,mm]) 
    }
      }
    
    if(BNF_TF){
      if(exists("bnfPresc_ts")){
      bnfPresc_ts = unique(rbind(bnfPresc_ts,suppressWarnings(generateSummaries(plpdDat[[1]],bnfDat[[1]],on = "BNF",
                                    year = times2extract[times==t,yyyy],
                                    month = times2extract[times==t,mm]))))
    }else{
      bnfPresc_ts = suppressWarnings(generateSummaries(plpdDat[[1]],bnfDat[[1]],on = "BNF",
                                      year = times2extract[times==t,yyyy],
                                      month = times2extract[times==t,mm]))
    }
      }
    
    for (i in 1:length(plpdData)){
      rm(list = names(plpdData)[i])
    }
    rm(plpdData)
  }
  
  if(all(BNF_TF, REGION_TF)){
    out = list(regionTS = regionPresc_ts,
               bnfTS = bnfPresc_ts)
  }
  if(BNF_TF&!REGION_TF){
    out = list(bnfTS = bnfPresc_ts)
  }

  if(REGION_TF&!BNF_TF){
    out = list(regionTS = regionPresc_ts)
  }
  
  if(save){
    
    if(!dir.exists(paste0(settings$basedir,"timeSeries"))){
      dir.create(paste0(settings$basedir,"timeSeries"),recursive = T)
    }
    
    if("regionTS"%in%names(out)){
      write_feather(x = out$regionTS,path = paste0(settings$basedir,"timeSeries/regionTS_",fromDate,"_",toDate,".feather"))
    }
    if("bnfTS"%in%names(out)){
      write_feather(x = out$bnfTS,path = paste0(settings$basedir,"timeSeries/bnfTS_",fromDate,"_",toDate,".feather"))
    }
    message(paste0("Time series: ",
                   "regionTs_",fromDate,"_",toDate,".feather \n ",
                   "bnfTS_",fromDate,"_",toDate,".feather \n", 
                   "saved in ",settings$basedir,"timeSeries/" ))
    
  }
  
  return(out)
}
