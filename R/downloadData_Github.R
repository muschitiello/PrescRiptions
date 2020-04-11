##' downloadData_Github
##'
##' Download prescriptions data from PrescRiptionsData github repository where all data are saved
##' needed for the use of this R package
##'
##' @param data which data to download. One of "all","demog","bnf","qof", default = "all"
##' @param yyyy numerical 4 digit year, default = 2019
##' @param mm numerical 2 digit month, no default
##' @param basedir working directory, no default
##'
##' @details The function is used to download BNF (British National Formulary) codes, 
##' practice size for demographic groups and quality outcome framework data from the github repository
##' called PrescRiptionsData available at the following link:
##' https://github.com/muschitiello/PrescRiptionsData. 
##' While the repo contains data in different formats, only feather data are downloadable from this function.
##' This because feather data are more manageable and compressed.
##' 
##' @return the function does not return anything, but download the specified files from github to the basedir folder
##' 
##' @seealso \code{\link{importMonthlyData}}, \code{\link{downloadDemog}}, \code{\link{downloadPLPDzip}}
##' 
##' @export
##'


downloadData_Github = function(data = c("all","bnf","demog","qof"),
                              yyyy = 2019, mm = NULL ,basedir = NULL){
  mm=as.character(stringr::str_pad(mm,width = 2,side = "left",pad = "0"))

  if (str_sub(basedir,start = (nchar(basedir)),end = nchar(basedir))!="/"){
    basedir = paste0(basedir,"/")
  }
  
  # define Repo url
  url = "https://raw.githubusercontent.com/muschitiello/PrescRiptionsData/master/"
  
  # switch to main folder
  ghData = switch(data,
                  "all" = c("02_bnf/feather","03_demog/feather","04_qof/feather"),
                  "bnf" = "02_bnf/feather",
                  "demog" = "03_demog/feather",
                  "qof" = "04_qof/feather"
  )
  
  # define prefix for file names
  prefix = paste0(yyyy,mm)
  
  # chose year for bnf file
  bnffile = switch(as.character(yyyy),
              "2018" = "bnf201901.feather",
              "2019" = "bnf202001.feather")
  
  # chose biennium for qof file
  qoffolder = switch(as.character(yyyy),
                   "2018" = "qof1819feather",
                   "2019" = "qof1819feather")
  qofprefix = switch(as.character(yyyy),
                     "2018" = "qofGP1819",
                     "2019" = "qofGP1819")
  
  # define qof file names
  qofFiles = paste0(qofprefix,c("_CardioVascular","_dependency",
                                    "_lifestyle","_mental","_muscul","_respiratory"),".feather")
  
  # define subfolder name
  inFolder = switch(data,
                    "all" = c("/",paste0("/demog_",prefix,"/"),paste0("/",qoffolder,"/")),
                    "bnf" = "/",
                    "demog" = paste0("/demog_",prefix,"/"),
                    "qof" = paste0("/",qoffolder,"/"))

  # define final folder name for data to be downloaded
  inFolderFinal = paste0(ghData,inFolder)
  
  # add filenames to path
  files = switch(data,
                 "all"= list( bnffile,
                             c(paste0(prefix,"_demog.feather"),paste0(prefix,"_demogMap.feather")),
                          qofFiles),
                 "bnf"= list(bnffile),
                 "demog"= list(c(paste0(prefix,"_demog.feather"),paste0(prefix,"_demogMap.feather"))),
                 "qof"= list(qofFiles)
                   )
  if(is.list(files)){
    names(files) = inFolderFinal
  }
  
  # define folder name for final download
  folder = switch(data,
                  "all"= c("bnf","demog","qof"),
                  "bnf"= "bnf",
                  "demog"= "demog" ,
                  "qof"= "qof")
  
  # set output Folder
  
  outFolder = paste0(basedir,"PrescRiptionsData_dowload/")

  # final download 
  for (i in 1:length(files)){
    
    toFolder = paste0(outFolder,names(files[i]))
    
    if(!dir.exists(toFolder)){
      dir.create(toFolder,recursive = T)
    }
    
    for (j in files[[i]]){
    
    download.file(url = paste0(url,inFolderFinal[i],j ),
                  destfile = paste0(toFolder,j))
    }
  }


  }
