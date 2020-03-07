##' structureParameters
##'
##' @param settings analysis settings as uploaded form a pesi.yml file
##'
##' @return folder & parameters as a list
##' 
##' @export
##'


structureParameters = function(settings=NULL){
  
  # Set month and year 
  if(!is.null(settings[["year"]])){
    if(!(is.numeric(settings[["year"]])|is.integer(settings[["year"]]))){
      stop("year must be numeric")
    }else{
      if(!(nchar(settings[["year"]])%in%c(2,4))){
        stop("year must be a 4 digits numeric value")
      }else{
        if(nchar(settings[["year"]])==4){
          year = substr(settings[["year"]],3,4)
        }
      }
    }
  }else{
    year = substr(settings[["year"]](Sys.Date()),3,4)
  }
  yearOrig = as.integer(settings[["year"]])
  
  # check mese
  if(!(is.numeric(settings[["month"]])|is.integer(settings[["month"]]))){
    stop("month deve essere numerico")
  }
  
  # creation object for ruture settings
  month0 = stringr:: str_pad(settings[["month"]],2,"left",0)
  
  # set Folder name and adress
  nomiMesi = c("gennaio", "febbraio", "marzo", "aprile", "maggio", "giugno", "luglio",
               "agosto","settembre", "ottobre", "novembre","dicembre")  
  
  folderKey = paste0(month0,nomiMesi[settings[["month"]]],year)
  folderSumTablesKey = paste0(folderKey,"/summaryTables")
  folderFinalCsvKey = paste0(folderKey,"/outputFinal_csv")
  folderFinalTxtKey = paste0(folderKey,"/outputFinal_txt")
  folderFinalRdKey = paste0(folderKey,"/outputFinal_RData")
  folderFeatherAllKey = paste0("_featherInputData/",folderKey)
  
  dataNameKey = paste0("data_aSoci_",substr(nomiMesi[SETTINGS[["month"]]],1,3),year)
  
  # Create and empty folder
  basedirKey=paste0(Sys.getenv("R_USER"),"/OutputAnagraficaPesi")
  
  list(
    dataName = dataNameKey,
    folder = folderKey,
    folderSumTables = folderSumTablesKey,
    folderFinalCsv = folderFinalCsvKey,
    folderFinalTxt = folderFinalTxtKey,
    folderFinalRd = folderFinalRdKey,
    folderFeatherAll = folderFeatherAllKey,
    basedir = basedirKey,
    yearNum = yearOrig,
    yearChar = year,
    month = settings[["month"]],
    month0 = month0,
    jarFolder = settings[["jarfolder"]],
    extractionDate = Sys.Date()
    )
  
}
