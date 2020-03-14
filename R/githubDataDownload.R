##' githubDataDownload
##'
##' Download prescriptions data from PrescRiptionsData github repository where all data are saved
##' needed for the use of this R package
##'
##' @param data which data to download. One of "all","plpd","demog","bnf","qof", default = "all"
##' @param yyyy numerical 4 digit year, default = 2019
##' @param mm numerical 2 digit month, no default
##' @param basedir working directory, no default
##' @param outFormat desired output format. One of "csv", "feather" or "both", default = "feather"
##'
##' @details The function is used to download Practice Level PRescriprion data foor
##' years 2018 and 2019. Links for the download are specifyed internally.
##' The function checks if the file already exists and in that case, does NOT download it.
##' FIle are in csv format. Feather format can be chosen. In that case the csv file is converted and then removed, so to
##' save internal space. If "both" \emph{outFotmat} is selected, csv is kept and the feather is generated.
##'
##' @export
##'


githubDataDownload = function(data = c("all","plpd","demog","bnf","qof"),
                              yyyy = 2019, mm = NULL ,basedir = NULL, outFormat = "feather"){
  mm=as.character(stringr::str_pad(mm,width = 2,side = "left",pad = "0"))

  url = "https://github.com/muschitiello/PrescRiptionsData/tree/master/"
  ghData = switch(data,
                    "all" = c("01_plpd","03_demog","02_bnf","04_qof"),
                    "plpd" = "01_plpd",
                    "demog" = "03_demog",
                    "bnf" = "02_bnf",
                    "qof" = "04_qof"
                    )
  ghFolder = paste0(ghData,"/",outFormat)

  prefix = paste0(yyyy,mm)
  yy = switch(as.character(yyyy),
              "2018" = "2019",
              "2019" = "2020")

  inFolder = switch(data,
                    "all" = c(paste0("/plpd_",prefix,"/"),paste0("/demog_",prefix,"/"),"/","/"),
                    "plpd" = paste0("/plpd_",prefix,"/"),
                    "demog" = paste0("demog_",prefix,"/"),
                    "bnf" = paste0("/bnf",yy,"01"),
                    "qof" = "/")


  inFolderFinal = paste0("PrescRiptionsData/",ghFolder,inFolder)

  for (i in inFolderFinal){
    if(grepl("plpd",i)){
      readr::readr(paste0(url,))
      paste0(i,list.files(i))
    }
  }


  }
