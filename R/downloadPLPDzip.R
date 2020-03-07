##' downloadPLPDzip
##'
##' Download monthly plpd zip file for years 2018, 2019 in csv or feather format
##' and extract them on a specified folder.
##'
##' @param yyyy numerical 4 digit year, default = 2019
##' @param mm numerical 2 digit month, no default
##' @param basedir working directory, no default
##' @param outFormat desired output format. One of "csv" or "feather", default = "feather"
##'
##' @export
##'


downloadPLPDzip = function(yyyy = 2019, mm = NULL ,basedir = NULL, outFormat = "feather"){

  plpdurl = paste0("plpd",yyyy,mm)
  prefix = paste0(yyyy,mm)
  folder = paste0("plpd_",prefix)
  exDir = paste0(basedir,"dataInput/01_plpd/")
  csvDir = paste0(basedir,"dataInput/01_plpd/csv/")
  featherDir = paste0(basedir,"dataInput/01_plpd/feather/")

  if (outFormat == "csv"){
    outDir = csvDir
  }else{
    outDir = featherDir
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
  # download into the placeholder file
  download.file(url = get(plpdurl), destfile = tf)
  # unzip in temporary folder
  td = tempdir()

  unzip(zipfile = tf, exdir = td)

  plpdFiles = list.files(td)[which(grepl("ADDR|CHEM|PDPI",list.files(td)))]

  # create folder
    if(!dir.exists(paste0(outDir,folder))){
      dir.create(outDir)
      dir.create(paste0(outDir,folder))
    }


  # save file in the specified format
  for(i in plpdFiles){
    if(grepl("ADDR",i)){
      data = read.csv(paste0(td,"/",i))
      path = paste0(outDir,folder,"/",prefix,"_addr.",outFormat)
    }
    if(grepl("CHEM",i)){
      data = read.csv(paste0(td,"/",i))
      path = paste0(outDir,folder,"/",prefix,"_chem.",outFormat)
    }
    if(grepl("PDPI",i)){
      data = read.csv(paste0(td,"/",i))
      path = paste0(outDir,folder,"/",prefix,"_pdpi.",outFormat)
    }

    if(outFormat == "csv"){
      write.csv2(data,path,row.names = FALSE)
    }else{
      write_feather(data,path)
    }
  }

  # rm tmp file
  unlink(tf)
  unlink(td)

}
