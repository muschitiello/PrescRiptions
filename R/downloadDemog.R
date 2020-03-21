##' downloadDemog
##'
##' Download monthly demographic data file for years 2018, 2019 in csv or feather format
##' and extract them on a specified folder.
##'
##' @param yyyy numerical 4 digit year, default = 2019
##' @param mm numerical 2 digit month, no default
##' @param basedir working directory, no default
##' @param outFormat desired output format. One of "csv", "feather" or "both", default = "feather"
##'
##' @seealso \code{\link{importMonthlyData}}, \code{\link{downloadPLPDzip}}, \code{\link{downloadData_Github}}
##'
##' @return the function does not return anything, but download the specified files to the basedir folder
##' @export
##'


downloadDemog = function(yyyy = 2019, mm = NULL ,basedir = NULL, outFormat = "feather"){
  mm=as.character(stringr::str_pad(mm,width = 2,side = "left",pad = "0"))

  url1 = paste0("demog",yyyy,mm)
  url2 = paste0("demogMap",yyyy,mm)
  prefix = paste0(yyyy,mm)
  folder = paste0("demog_",prefix)
  exDir = paste0(basedir,"dataInput/03_demog/")
  csvDir = paste0(basedir,"dataInput/03_demog/csv/")
  featherDir = paste0(basedir,"dataInput/03_demog/feather/")

  if (outFormat == "csv"){
    outDir = csvDir
  }else if(outFormat == "feather"){
    outDir = featherDir
  } else {
    outDir = c(csvDir,featherDir)
    outFormat = c("csv","feather")
  }

  # Data URL
  demog201801 = "https://files.digital.nhs.uk/excel/d/i/gp-reg-pat-prac-quin-age-jan-18.csv"
  demogMap201801 = "https://files.digital.nhs.uk/excel/f/i/gp-reg-pat-prac-map-jan-18.csv"
  demog201802 = "https://files.digital.nhs.uk/excel/p/o/gp-reg-pat-prac-quin-age-feb-18.csv"
  demogMap201802 = "https://files.digital.nhs.uk/excel/q/s/gp-reg-pat-prac-map-feb-18.csv"
  demog201803 = "https://files.digital.nhs.uk/excel/l/m/gp-reg-pat-prac-quin-age-mar-18.csv"
  demogMap201803 = "https://files.digital.nhs.uk/excel/l/6/gp-reg-pat-prac-map-mar-18.csv"
  demog201804 = "https://files.digital.nhs.uk/A5/ACF934/gp-reg-pat-prac-quin-age.csv"
  demogMap201804 = "https://files.digital.nhs.uk/5F/21E948/gp-reg-pat-prac-map.csv"
  demog201805 = "https://files.digital.nhs.uk/A1/880A64/gp-reg-pat-prac-quin-age-may-18.csv"
  demogMap201805 = "https://files.digital.nhs.uk/81/F0F611/gp-reg-pat-prac-map-may-18.csv"
  demog201806 = "https://files.digital.nhs.uk/9F/2BA586/gp-reg-pat-prac-quin-age-jun-18.csv"
  demogMap201806 = "https://files.digital.nhs.uk/1C/21F84E/gp-reg-pat-prac-map-jun-18.csv"
  demog201807 = "https://files.digital.nhs.uk/3E/2830D3/gp-reg-pat-prac-quin-age.csv"
  demogMap201807 = "https://files.digital.nhs.uk/BA/206EF1/gp-reg-pat-prac-map.csv"
  demog201808 = "https://files.digital.nhs.uk/40/28B9C0/gp-reg-pat-prac-quin-age-aug-18.csv"
  demogMap201808 = "https://files.digital.nhs.uk/AC/85DF3F/gp-reg-pat-prac-map-aug-18.csv"
  demog201809 = "https://files.digital.nhs.uk/5F/3AFDC5/gp-reg-pat-prac-quin-age-sep-18.csv"
  demogMap201809 = "https://files.digital.nhs.uk/E1/6D7E08/gp-reg-pat-prac-map-sep-18.csv"
  demog201810 = "https://files.digital.nhs.uk/3A/7BDE0B/gp-reg-pat-prac-quin-age-oct-18.csv"
  demogMap201810 = "https://files.digital.nhs.uk/B7/149E2E/gp-reg-pat-prac-map-oct-18.csv"
  demog201811 = "https://files.digital.nhs.uk/05/BD3DA7/gp-reg-pat-prac-quin-age-nov-18.csv"
  demogMap201811 = "https://files.digital.nhs.uk/0B/D8EB59/gp-reg-pat-prac-map-nov-18.csv"
  demog201812 = "https://files.digital.nhs.uk/47/C94440/gp-reg-pat-prac-quin-age.csv"
  demogMap201812 = "https://files.digital.nhs.uk/33/175D59/gp-reg-pat-prac-map.csv"
  demog201901 = "https://files.digital.nhs.uk/83/37D523/gp-reg-pat-prac-quin-age.csv"
  demogMap201901 = "https://files.digital.nhs.uk/90/230527/gp-reg-pat-prac-map.csv"
  demog201902 = "https://files.digital.nhs.uk/03/49C752/gp-reg-pat-prac-quin-age.csv"
  demogMap201902 = "https://files.digital.nhs.uk/26/EAD80F/gp-reg-pat-prac-map.csv"
  demog201903 = "https://files.digital.nhs.uk/7E/A5739C/gp-reg-pat-prac-quin-age.csv"
  demogMap201903 = "https://files.digital.nhs.uk/97/6DE258/gp-reg-pat-prac-map.csv"
  demog201904 = "https://files.digital.nhs.uk/EC/F091EE/gp-reg-pat-prac-quin-age.csv"
  demogMap201904 = "https://files.digital.nhs.uk/B7/AF0B69/gp-reg-pat-prac-map.csv"
  demog201905 = "https://files.digital.nhs.uk/5A/22D4C1/gp-reg-pat-prac-quin-age.csv"
  demogMap201905 = "https://files.digital.nhs.uk/45/DD643F/gp-reg-pat-prac-map.csv"
  demog201906 = "https://files.digital.nhs.uk/D0/F67AE6/gp-reg-pat-prac-quin-age.csv"
  demogMap201906 = "https://files.digital.nhs.uk/04/8A26B4/gp-reg-pat-prac-map.csv"
  demog201907 = "https://files.digital.nhs.uk/FB/823A3D/gp-reg-pat-prac-quin-age.csv"
  demogMap201907 = "https://files.digital.nhs.uk/D8/5F3570/gp-reg-pat-prac-map.csv"
  demog201908 = "https://files.digital.nhs.uk/34/609406/gp-reg-pat-prac-quin-age.csv"
  demogMap201908 = "https://files.digital.nhs.uk/7E/EAB63C/gp-reg-pat-prac-map.csv"
  demog201909 = "https://files.digital.nhs.uk/5A/A5B350/gp-reg-pat-prac-quin-age.csv"
  demogMap201909 = "https://files.digital.nhs.uk/82/F07BFC/gp-reg-pat-prac-map.csv"
  demog201910 = "https://files.digital.nhs.uk/49/FB72CA/gp-reg-pat-prac-quin-age.csv"
  demogMap201910 = "https://files.digital.nhs.uk/03/F3381F/gp-reg-pat-prac-map.csv"
  demog201911 = "https://files.digital.nhs.uk/CB/C94127/gp-reg-pat-prac-quin-age.csv"
  demogMap201911 = "https://files.digital.nhs.uk/60/5960B5/gp-reg-pat-prac-map.csv"
  demog201912 = "https://files.digital.nhs.uk/56/1EA3F2/gp-reg-pat-prac-quin-age.csv"
  demogMap201912 = "https://files.digital.nhs.uk/F4/FCE14C/gp-reg-pat-prac-map.csv"

  # create the temporary file


  # check if file already exists
  # if not, create folder
  for (i in outDir){

    outF = ifelse(i == csvDir,"csv","feather")

    if(dir.exists(paste0(i,folder)) & length(list.files(paste0(i,folder)))==2){
      message(paste0("File already downloaded for the selected year, month and format"))
      message(paste0("in the ",i," folder"))
    }else{
      suppressWarnings(dir.create(i))
      suppressWarnings(dir.create(paste0(i,folder)))

      # check if a csv already exists (valid only for the feather case)
      if(i == featherDir & dir.exists(paste0(csvDir,folder))&length(list.files(paste0(csvDir,folder)))==2){
        td1 = paste0(csvDir,folder,"/",prefix,"_demog.",outF)
        td2 = paste0(csvDir,folder,"/",prefix,"_demogMap.",outF)
      }else{
        # if not, download into the placeholder file
        td1 = tempfile(fileext = ".csv")
        td2 = tempfile(fileext = ".csv")

        # save file in the specified format
        # download into the placeholder file
        utils::download.file(url = get(url1), destfile = td1)
        utils::download.file(url = get(url2), destfile = td2)
      }
      # save file in the specified format
      data1 = read.csv(td1)
      path1 = paste0(i,folder,"/",prefix,"_demog.",outF)

      data2 = read.csv(td2)
      path2 = paste0(i,folder,"/",prefix,"_demogMap.",outF)

      if(i == csvDir){
        write.csv2(data1,path1,row.names = FALSE)
        write.csv2(data2,path2,row.names = FALSE)
      }else{
        feather::write_feather(data1,path1)
        feather::write_feather(data2,path2)
      }


      # rm tmp file
      if(td1 != paste0(csvDir,folder,"/",prefix,"_demog.",outF)){
        unlink(td1)
        unlink(td2)
      }
    }
  }
}

