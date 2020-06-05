##' monthlyData_import
##'
##' import plpd, bnf, demog and qof data in the workspace for the specified month and year as setted with \emph{setConfig()}
##'
##' @param settings analysis settings generated with \emph{setConfig()}
##' @param whichData string indicatin which Data to download. One of "plpd", "bnf", "demog" and "all"
##' default = "all"
##' @param sample logic. if TRUE (default) only 500.000 rows will be uploaded in the workspace. 
##'
##' @details Import all data for one month analysis in the workspace. If altready downloaded, data are retrived from the folder where they have been downloaded.
##' If not already downloaded, data are downloaded from:
##'  - plpd: website if sample = FALSE, github if sample = TRUE (default)
##'  - bnf: github/csv
##'  - demog: github/csv
##'  - qof: github/csv
##' github repository at this link: \emph{https://github.com/muschitiello/PrescRiptionsData}
##' 
##' @seealso \code{\link{monthlyData_download}}
##' 
##' @return the function returns all data upoaded in the workspace in list format
##' 
##' @export
##'


monthlyData_import = function(settings,whichData = "all",sample){
 
   if(!exists("sample")){
    message("sample argument not specified, SAMPLE DATA will be downloaded")
    sample = TRUE
   }
  
  dirs = dirsGen(settings)
  
  month=as.character(stringr::str_pad(settings$month,width = 2,side = "left",pad = "0"))
  year = settings$year
  rootdir = settings$rootdir
  
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
  
  ##############################################################
  #### IMPORT PLPD
  plpdurl = paste0("plpd",year,month)
  prefix = paste0(year,month)
  folder = paste0("plpd_",prefix)
  sampleFolder = paste0("plpd_",prefix,"_SAMPLE")
  
  
  
  
  #### If already exists on PC, import from there
  
  if(plpdTF){
    message(paste(folder))
    if(sample){
      # check if file already exists
      # if not, create folder
      if(!(dir.exists(paste0(dirs$plpdRootDirSample,"/",sampleFolder)) & length(list.files(paste0(dirs$plpdRootDirSample,"/",sampleFolder)))==3)){
        message("download Data")
        monthlyData_download(settings = settings,whichData = "plpd",sample=TRUE)
      }
      
      message(paste0("File downloaded. Import from rootDir"))
      plpdFiles = list.files(paste0(dirs$plpdRootDirSample,"/",sampleFolder))[
        which(grepl("ADDR|CHEM|PDPI|addr|chem|pdpi",list.files(paste0(dirs$plpdRootDirSample,"/",sampleFolder))))]
      
      for(j in plpdFiles){
        message(j)
        if(grepl("ADDR|addr",j)){
          assign(paste0("addr_",prefix),data.table(read.csv(paste0(paste0(dirs$plpdRootDirSample,"/",sampleFolder),"/",j),stringsAsFactors = FALSE,header = FALSE)))
          setnames(mget(paste0("addr_",prefix))[[1]],colnames(mget(paste0("addr_",prefix))[[1]]),c("PERIOD","Practice.Code","Practice.Name",
                                                                                                   "Address.1","Address.2","Address.3","Address.4","Postcode"))
        }
        if(grepl("CHEM|chem",j)){
          assign(paste0("chem_",prefix),data.table(read.csv(paste0(paste0(dirs$plpdRootDirSample,"/",sampleFolder),"/",j),stringsAsFactors = FALSE)))
          
        }
        if(grepl("PDPI|pdpi",j)){

            assign(paste0("plpd_",prefix),data.table(read.csv(paste0(paste0(dirs$plpdRootDirSample,"/",sampleFolder),"/",j),stringsAsFactors = FALSE)))
             
          
        }
        
      }
      
      
    }else{
      if(!(dir.exists(paste0(dirs$plpdRootDir,folder)) & length(list.files(paste0(dirs$plpdRootDir,folder)))==3)){
        message("download Data")
        monthlyData_download(settings = settings,whichData = "plpd",sample=FALSE)
      }
      
      plpdFiles = list.files(paste0(dirs$plpdRootDir,folder))[
        which(grepl("ADDR|CHEM|PDPI|addr|chem|pdpi",list.files(paste0(dirs$plpdRootDir,folder))))]
      
      for(j in plpdFiles){
        message(j)
        if(grepl("ADDR|addr",j)){
          assign(paste0("addr_",prefix),data.table(read.csv(paste0(paste0(dirs$plpdRootDir,folder),"/",j),stringsAsFactors = FALSE,header = FALSE)))
          setnames(mget(paste0("addr_",prefix))[[1]],colnames(mget(paste0("addr_",prefix))[[1]]),c("PERIOD","Practice.Code","Practice.Name",
                                                                                                   "Address.1","Address.2","Address.3","Address.4","Postcode"))
        }
        if(grepl("CHEM|chem",j)){
          assign(paste0("chem_",prefix),data.table(read.csv(paste0(paste0(dirs$plpdRootDir,folder),"/",j),stringsAsFactors = FALSE)))
          
        }
        if(grepl("PDPI|pdpi",j)){
          if(sample){
            assign(paste0("plpd_",prefix),data.table(read.csv(paste0(paste0(dirs$plpdRootDir,folder),"/",j),stringsAsFactors = FALSE,nrows = 500000)))
          }else{
            assign(paste0("plpd_",prefix),data.table(read.csv(paste0(paste0(dirs$plpdRootDir,folder),"/",j),stringsAsFactors = FALSE)))
          }     
          
        }
        
      }
    }
    out = mget(c(paste0("plpd_",prefix),paste0("chem_",prefix),paste0("addr_",prefix)))
  }
  
  ##############################################################
  #### bnf, demog, qof from github
  
  
  # define Repo url
  url = "https://raw.githubusercontent.com/muschitiello/PrescRiptionsData/master/"
  
  # switch to main folder
  ghData = c("02_bnf/csv","03_demog/csv","04_qof/csv")
  
  # chose year for bnf file
  bnffile = switch(as.character(year),
                   "2018" = "bnf_201901.csv",
                   "2019" = "bnf_202001.csv")
  
  # chose biennium for qof file
  qoffolder = switch(as.character(year),
                     "2018" = "qof_1819",
                     "2019" = "qof_1819")
  qofprefix = switch(as.character(year),
                     "2018" = "qofGP_1819",
                     "2019" = "qofGP_1819")
  
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
        message(j)
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
  
  if(any(grepl("plpd",names(out)))){
    if("X" %in% colnames(out[which(grepl("plpd",names(out)))][[1]])){
      suppressWarnings(out[which(grepl("plpd",names(out)))][[1]][,X:=NULL])
    }
  }
  
  if(any(grepl("demogMap",names(out)))){
    out[which(grepl("demogMap",names(out)))][[1]]=checkDemogMap(out[which(grepl("demogMap",names(out)))][[1]],settings)
  }
  
  return(out)
  
}

