##' importMonthlyData
##'
##' import in the workspace plpd, bnf, demog and qof data for the specified month and year as settend in the yml file
##'
##' @param yyyy numerical 4 digit year, default = 2019
##' @param mm numerical 2 digit month, no default
##' @param whichData string indicatin which Data to download. One of "plpd", "bnf", "demog" and "all"
##' default = "all"
##'
##' @details Import all data for one month analysis in the workspace. data are imported from the following sources:
##'  - plpd: website 
##'  - bnf: github/csv
##'  - demog: github/csv
##'  - qof: github/csv
##' github repository at this link: \emph{https://github.com/muschitiello/PrescRiptionsData}
##' 
##' @seealso \code{\link{downloadData_Github}}, \code{\link{downloadDemog}}, \code{\link{downloadPLPDzip}}
##' 
##' @return the function returns all data upoaded in the workspace
##' 
##' @export
##'


importMonthlyData = function(yyyy = 2019, mm = NULL, whichData = "all"){
  
  whichDataAll = c("all","plpd","bnf","demog","qof")
  
  if(any(!whichData%in%whichDataAll)){
    stop(paste0("Error in whichData. Admitted values are: ",paste0(whichDataAll, collapse = ", ")," quoted"))
  }
  
  if(length(whichData)>1 & "all" %in% whichData){
    whichData = "all"
  }
  
  if(all(length(whichData)==1 & whichData=="all")){
    plpdTF = TRUE
    bnfTF = TRUE
    demogTF = TRUE
    qofTF = TRUE
  }else{
    plpdTF = FALSE
    bnfTF = FALSE
    demogTF = FALSE
    qofTF = FALSE
    
    if("plpd" %in% whichData){
      plpdTF = TRUE
    }
    if("bnf" %in% whichData){
      bnfTF = TRUE
    }
    if("demog" %in% whichData){
      demogTF = TRUE
    }
    if("qof" %in% whichData){
      qofTF = TRUE
    }
  }
  
  mm=as.character(stringr::str_pad(mm,width = 2,side = "left",pad = "0"))
  
  ##############################################################
  #### plpd from source
  
  plpdurl = paste0("plpd",yyyy,mm)
  prefix = paste0(yyyy,mm)
  # folder = paste0("plpd_",prefix)
  
  url = switch(plpdurl,
               # Data URL
               "plpd201801" = "https://files.digital.nhs.uk/11/1E8A59/2018_01_Jan.zip",
               "plpd201802" = "https://files.digital.nhs.uk/35/292E2A/2018_02_Feb.zip",
               "plpd201803" = "https://files.digital.nhs.uk/6F/CE775A/2018_03_Mar.zip",
               "plpd201804" = "https://files.digital.nhs.uk/E3/801EA5/2018_04_Apr.zip",
               "plpd201805" = "https://files.digital.nhs.uk/B0/B15E0B/2018_05_May.zip",
               "plpd201806" = "https://files.digital.nhs.uk/07/697711/2018_06_Jun.zip",
               "plpd201807" = "https://files.digital.nhs.uk/7E/FC3950/2018_07_Jul.zip",
               "plpd201808" = "https://files.digital.nhs.uk/43/C6644B/2018_08_Aug.zip",
               "plpd201809" = "https://files.digital.nhs.uk/5C/FE61C4/2018_09_Sep.zip",
               "plpd201810" = "https://files.digital.nhs.uk/33/3EE982/2018_10_Oct.zip",
               "plpd201811" = "https://files.digital.nhs.uk/96/A7878A/2018_11_Nov.zip",
               "plpd201812" = "https://files.digital.nhs.uk/94/405A94/2018_12_Dec.zip",
               "plpd201901" = "https://files.digital.nhs.uk/EC/D8DF0F/2019_01_Jan.zip",
               "plpd201902" = "https://files.digital.nhs.uk/6E/DD7444/2019_02_Feb.zip",
               "plpd201903" = "https://files.digital.nhs.uk/49/2988AF/2019_03_MarV2.zip",
               "plpd201904" = "https://files.digital.nhs.uk/85/3C25E0/2019_04_APR.zip",
               "plpd201905" = "https://files.digital.nhs.uk/CA/9D94E7/2019_05_May.zip",
               "plpd201906" = "https://files.digital.nhs.uk/9B/047263/2019_06_Jun.zip",
               "plpd201907" = "https://files.digital.nhs.uk/B9/369C6C/2019_07_Jul.zip",
               "plpd201908" = "https://files.digital.nhs.uk/50/A23B13/2019_08_Aug.zip",
               "plpd201909" = "https://files.digital.nhs.uk/E4/D720CC/2019_09_Sep.zip",
               "plpd201910" = "https://files.digital.nhs.uk/0B/E73CF4/2019_10_Oct.zip",
               "plpd201911" = "https://files.digital.nhs.uk/66/972E65/2019_11_Nov.zip",
               "plpd201912" = "https://files.digital.nhs.uk/5A/CA6C2E/2019_12_Dec.zip"
  )
  if(plpdTF){
    # create the temporary file
    td=paste0(Sys.getenv("TEMP"),"\\PrescRtemp")
    on.exit(unlink(td))
    tf = tempfile(tmpdir = td,fileext = ".zip")
    if(!dir.exists(td)){
      dir.create(td)
    }
    utils::download.file(url = url, destfile = tf)
    # unzip in temporary folder
    unzip(zipfile = tf, exdir = td)
    plpdFiles = list.files(td)[which(grepl("ADDR|CHEM|PDPI|addr|chem|pdpi",list.files(td)))]
    
    for(j in plpdFiles){
      if(grepl("ADDR|addr",j)){
        assign(paste0("addr",prefix),data.table(read.csv(paste0(td,"/",j),stringsAsFactors = FALSE)))
      }
      if(grepl("CHEM|chem",j)){
        assign(paste0("chem",prefix),data.table(read.csv(paste0(td,"/",j),stringsAsFactors = FALSE)))
        
      }
      if(grepl("PDPI|pdpi",j)){
        assign(paste0("plpd",prefix),data.table(read.csv(paste0(td,"/",j),stringsAsFactors = FALSE)))
      }
      
    }
    
    # rm tmp file
    unlink(tf)
    unlink(td,recursive = T)

    out = mget(c(paste0("plpd",prefix),paste0("chem",prefix),paste0("addr",prefix)))
  }
  ##############################################################
  #### bnf, demog, qof from github
  
  
  # define Repo url
  url = "https://raw.githubusercontent.com/muschitiello/PrescRiptionsData/master/"
  
  # switch to main folder
  ghData = c("02_bnf/csv","03_demog/csv","04_qof/csv")
  
  # chose year for bnf file
  bnffile = switch(as.character(yyyy),
                   "2018" = "bnf201901.csv",
                   "2019" = "bnf202001.csv")
  
  # chose biennium for qof file
  qoffolder = switch(as.character(yyyy),
                     "2018" = "qof1819csv",
                     "2019" = "qof1819csv")
  qofprefix = switch(as.character(yyyy),
                     "2018" = "qofGP1819",
                     "2019" = "qofGP1819")
  
  # define qof file names
  qofFiles = paste0(qofprefix,c("_CardioVascular","_dependency",
                                "_lifestyle","_mental","_muscul","_respiratory"),".csv")
  
  # define subfolder name
  inFolder = c("/",paste0("/demog_",prefix,"/"),paste0("/",qoffolder,"/"))
  
  # define final folder name for data to be downloaded
  inFolderFinal = paste0(ghData,inFolder)
  
  # add filenames to path
  files = list( bnffile,c(paste0("demog_",prefix,".csv"),paste0("demogMap_",prefix,".csv")),
                qofFiles)
  
  if(is.list(files)){
    names(files) = inFolderFinal
  }
  
  if(any(bnfTF,demogTF,qofTF)){
    if(all(bnfTF&demogTF&qofTF)){filesN=1:3}
    if(all(bnfTF&demogTF&!qofTF)){filesN=1:2}
    if(all(bnfTF&!demogTF&qofTF)){filesN=c(1,3)}
    if(all(!bnfTF&demogTF&qofTF)){filesN=2:3}
    if(all(bnfTF&!demogTF&!qofTF)){filesN=1}
    if(all(!bnfTF&demogTF&!qofTF)){filesN=2}
    if(all(!bnfTF&!demogTF&qofTF)){filesN=3}
    
    
    files = files[filesN]
    inFolderFinal = inFolderFinal[filesN]
    # define folder name for final download
    # folder = c("bnf","demog","qof")
    
    if(exists("out")){
      k=length(out)+1
    }else{
      out=list()
      k=1
    }
    
    
    # final download 
    for (i in 1:length(files)){
      
      for (j in files[[i]]){
        nameF = sub(".csv","",j)
        if(grepl("demog|qof",j)){
          sepCsv = ";"
        }else{
          sepCsv =","
        }
        assign(sub(".csv","",j),data.table(read.csv(text = RCurl::getURL(paste0(url,inFolderFinal[i],j )),sep = sepCsv),stringsAsFactors = FALSE))
        
        out[k] = mget(c(nameF))
        names(out)[k] = nameF
        k=k+1
        
      }
    }
  }
  return(out)
  
}

