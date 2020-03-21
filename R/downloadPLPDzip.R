##' downloadPLPDzip
##'
##' Download monthly plpd zip file for years 2018, 2019 in csv or feather format
##' and extract them on a specified folder.
##'
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
##' @seealso \code{\link{importMonthlyData}}, \code{\link{downloadDemog}}, \code{\link{downloadData_Github}}
##' 
##' @return the function does not return anything, but download the specified files to the basedir folder
##' @export
##'


downloadPLPDzip = function(yyyy = 2019, mm = NULL ,basedir = NULL, outFormat = "feather"){
  mm=as.character(stringr::str_pad(mm,width = 2,side = "left",pad = "0"))

  plpdurl = paste0("plpd",yyyy,mm)
  prefix = paste0(yyyy,mm)
  folder = paste0("plpd_",prefix)
  exDir = paste0(basedir,"dataInput/01_plpd/")
  csvDir = paste0(basedir,"dataInput/01_plpd/csv/")
  featherDir = paste0(basedir,"dataInput/01_plpd/feather/")
  

  if (outFormat == "csv"){
    outDir = csvDir
  }else if(outFormat == "feather"){
    outDir = featherDir
  } else {
    outDir = c(csvDir,featherDir)
    outFormat = c("csv","feather")
  }

  # Data URL
  plpd201801 = "https://files.digital.nhs.uk/11/1E8A59/2018_01_Jan.zip"
  plpd201802 = "https://files.digital.nhs.uk/35/292E2A/2018_02_Feb.zip"
  plpd201803 = "https://files.digital.nhs.uk/6F/CE775A/2018_03_Mar.zip"
  plpd201804 = "https://files.digital.nhs.uk/E3/801EA5/2018_04_Apr.zip"
  plpd201805 = "https://files.digital.nhs.uk/B0/B15E0B/2018_05_May.zip"
  plpd201806 = "https://files.digital.nhs.uk/07/697711/2018_06_Jun.zip"
  plpd201807 = "https://files.digital.nhs.uk/7E/FC3950/2018_07_Jul.zip"
  plpd201808 = "https://files.digital.nhs.uk/43/C6644B/2018_08_Aug.zip"
  plpd201809 = "https://files.digital.nhs.uk/5C/FE61C4/2018_09_Sep.zip"
  plpd201810 = "https://files.digital.nhs.uk/33/3EE982/2018_10_Oct.zip"
  plpd201811 = "https://files.digital.nhs.uk/96/A7878A/2018_11_Nov.zip"
  plpd201812 = "https://files.digital.nhs.uk/94/405A94/2018_12_Dec.zip"
  plpd201901 = "https://files.digital.nhs.uk/EC/D8DF0F/2019_01_Jan.zip"
  plpd201902 = "https://files.digital.nhs.uk/6E/DD7444/2019_02_Feb.zip"
  plpd201903 = "https://files.digital.nhs.uk/49/2988AF/2019_03_MarV2.zip"
  plpd201904 = "https://files.digital.nhs.uk/85/3C25E0/2019_04_APR.zip"
  plpd201905 = "https://files.digital.nhs.uk/CA/9D94E7/2019_05_May.zip"
  plpd201906 = "https://files.digital.nhs.uk/9B/047263/2019_06_Jun.zip"
  plpd201907 = "https://files.digital.nhs.uk/B9/369C6C/2019_07_Jul.zip"
  plpd201908 = "https://files.digital.nhs.uk/50/A23B13/2019_08_Aug.zip"
  plpd201909 = "https://files.digital.nhs.uk/E4/D720CC/2019_09_Sep.zip"
  plpd201910 = "https://files.digital.nhs.uk/0B/E73CF4/2019_10_Oct.zip"
  plpd201911 = "https://files.digital.nhs.uk/66/972E65/2019_11_Nov.zip"
  plpd201912 = "https://files.digital.nhs.uk/5A/CA6C2E/2019_12_Dec.zip"

  # create the temporary file
  tf = tempfile(fileext = ".zip")

  # check if file already exists
  # if not, create folder
  for (i in outDir){

    outF = ifelse(i == csvDir,"csv","feather")

    if(dir.exists(paste0(i,folder)) & length(list.files(paste0(i,folder)))==3){
        message(paste0("File already downloaded for the selected year, month and format"))
        message(paste0("in the ",i," folder"))
      }else{
        suppressWarnings(dir.create(i))
        suppressWarnings(dir.create(paste0(i,folder)))

      # check if a csv already exists (valid only for the feather case)
      if(i == featherDir & dir.exists(paste0(csvDir,folder))&length(list.files(paste0(csvDir,folder)))==3){
        td = paste0(csvDir,folder,"/")
      }else{
        # if not, download into the placeholder file
        utils::download.file(url = get(plpdurl), destfile = tf)
        # unzip in temporary folder
        td = tempdir()
        unzip(zipfile = tf, exdir = td)
      }
      plpdFiles = list.files(td)[which(grepl("ADDR|CHEM|PDPI|addr|chem|pdpi",list.files(td)))]

      # save file in the specified format
      for(j in plpdFiles){
        if(grepl("ADDR|addr",j)){
          data = read.csv2(paste0(td,"/",j))
          path = paste0(i,folder,"/",prefix,"_addr.",outF)
        }
        if(grepl("CHEM|chem",j)){
          data = read.csv2(paste0(td,"/",j))
          path = paste0(i,folder,"/",prefix,"_chem.",outF)
        }
        if(grepl("PDPI|pdpi",j)){
          data = read.csv2(paste0(td,"/",j))
          path = paste0(i,folder,"/",prefix,"_pdpi.",outF)
        }

        if(i == csvDir){
          write.csv2(data,path,row.names = FALSE)
        }else{
          feather::write_feather(data,path)
        }
      }

      # rm tmp file
      if(td == paste0(csvDir,folder,"/")){
        unlink(tf)
      }else{
        unlink(tf)
        unlink(td)
      }

    }
  }
}
