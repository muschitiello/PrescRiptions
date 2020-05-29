##' timeSeries_generate
##'
##' generate Time series for regional and bnf data
##'
##' @param from start date in the format "mm-yyyy"
##' @param to end date in the format "mm-yyyy"
##' @param settings analysis settings generated with \emph{setConfig()}
##' @param on one of \emph{"REGION"}, \emph{"BNF"} and \emph{"BOTH"} indicating which ts to produce,
##' default = "BOTH"
##' @param save logic, if TRUE, an external file will be saved in rootdir. Default = TRUE
##' @details The function summaries plpd or bnf data on ITEMS, NIC, ACT.COST, QUANTITY and REGION
##' and generates TS over the period of time indicated trought the \emph{from} and \emph{to} arguments
##' @return The function returns a data.table with time series data for the selected period, as generated, in the time range selected, 
##'  via  \emph{generateSummaries()}
##' @export


timeSeries_generate = function(from = "mm/yyyy", to = "mm/yyyy", settings = NULL, on = c("BOTH"), save = TRUE){
  
  dirs = dirsGen(settings)
  
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
    BNF_TF = FALSE
    
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
    
    #Check if times series files already exist
    if(all(REGION_TF,BNF_TF)){
      existsTF = all(file.exists(paste0(dirs$timeSeriesDir,"regionTS_",fromDate,"_",toDate,".csv")),
          file.exists(paste0(dirs$timeSeriesDir,"bnfTS_",fromDate,"_",toDate,".csv")))
    }
    if(REGION_TF&!BNF_TF){
      existsTF = file.exists(paste0(dirs$timeSeriesDir,"regionTS_",fromDate,"_",toDate,".csv"))
    }
    if(BNF_TF&!REGION_TF){
      existsTF = file.exists(paste0(dirs$timeSeriesDir,"bnfTS_",fromDate,"_",toDate,".csv"))
    }
    
    if(existsTF){
      return(message(paste("TS file/s already exist/s in", dirs$timeSeriesDir,sep='\n')))
    }else{
      
    # create vector of times to download 
    
    times = data.table::data.table(expand.grid(2018:2019,stringr::str_pad(1:12,2,"left",0)))[
      order(Var1)][,times:=paste0(Var1,Var2)][,mget(c("Var2","Var1","times"))]
    data.table::setnames(times,colnames(times),c("mm","yyyy","times"))
    
    times2extract = times[data.table::between(times,fromDate,toDate)]
    
    #######################################
    ## DOWNLOAD DATA
    tempSet = settings
    message("DOWNLOAD DATA")
    
    # Download BNF Data
    year2extract = times2extract[,unique(yyyy)]
    times2extractBnf=times[yyyy %in%year2extract &  mm=="01"]
    for (t in times2extractBnf[,times]){
      tempSet$year=times2extractBnf[times==t,yyyy]
      tempSet$month=times2extractBnf[times==t,mm]
      monthlyData_download(tempSet,whichData = "bnf")
    }
    
    # Download PLPD Data
    for (t in times2extract[,times]){
      print(paste0(times2extract[times==t,yyyy]," - ",times2extract[times==t,mm]))
      tempSet$year=times2extract[times==t,yyyy]
      tempSet$month=times2extract[times==t,mm]
      monthlyData_download(tempSet,whichData = "plpd")
    }
    
    #######################################################
    ### IMPORT DATA IN WS, GENERATE & AGGREGATE SUMMARIES
    message("GENERATE TS")
    
    for (t in times2extract[,times]){
      
      print(paste0(times2extract[times==t,yyyy]," - ",times2extract[times==t,mm]))
      
      ### IMPORT
      # BNF Data
      year2extract = times2extract[times==t,unique(yyyy)]
      times2extractBnf=times[yyyy %in%year2extract &  mm=="01"]
      tempSet$year=times2extractBnf[,unique(yyyy)]
      tempSet$month=as.character(times2extractBnf[,mm])
      bnfDat = monthlyData_import(tempSet,whichData = "bnf")[[1]]
      
      #  PLPD Data
      tempSet$year=times2extract[times==t,yyyy]
      tempSet$month=as.character(times2extract[times==t,mm])
      plpdDat = suppressMessages(monthlyData_import(tempSet,whichData = "plpd"))[[1]]
      
      ### AGGREGATE
      
      if(REGION_TF){
        tempSet$year=times2extract[times==t,yyyy]
        tempSet$month=as.character(times2extract[times==t,mm])
        if(exists("regionPresc_ts")){
          regionPresc_ts = rbind(regionPresc_ts,generateSummaries(plpdDat,bnfDat,on = "REGION",settings = tempSet))
        }else{
          regionPresc_ts = generateSummaries(plpdDat,bnfDat,on = "REGION",settings = tempSet)
        }
      }
      
      if(BNF_TF){
        tempSet$year=times2extract[times==t,yyyy]
        tempSet$month=as.character(times2extract[times==t,mm])
        
        if(exists("bnfPresc_ts")){
          bnfPresc_ts = unique(rbind(bnfPresc_ts,suppressWarnings(generateSummaries(plpdDat,bnfDat,on = "BNF",settings = tempSet))))
        }else{
          bnfPresc_ts = suppressWarnings(generateSummaries(plpdDat,bnfDat,on = "BNF",settings = tempSet))
        }
      }
      ### REMOVE
      
      rm(plpdDat)
      rm(bnfDat)
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
    
    #######################################################
    ### SAVE OUTPUT
    
    if(save){
      
      if(!dir.exists(dirs$timeSeriesDir)){
        dir.create(dirs$timeSeriesDir,recursive = T)
      }
      
      if("regionTS"%in%names(out)){
        write.csv2(out$regionTS,paste0(dirs$timeSeriesDir,"regionTS_",fromDate,"_",toDate,".csv"),row.names = F)
      }
      if("bnfTS"%in%names(out)){
        write.csv2(out$bnfTS,paste0(dirs$timeSeriesDir,"bnfTS_",fromDate,"_",toDate,".csv"),row.names = F)
      }
      message(paste0("Time series: ",
                     "regionTs_",fromDate,"_",toDate,"csv \n ",
                     "bnfTS_",fromDate,"_",toDate,"csv \n",
                     "saved in ",settings$rootdir,"/timeSeries/" ))
      
    }
    
    return(out)
    
  }
}

