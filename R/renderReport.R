##' renderReport
##'
##' Automatically generates a Markdown pdf report for a single month
##'
##' @param year year of interest.
##' @param month month of interest.
##' @param outFolder Folder where to save the Report
##' @return The function generates a html file
##' @details If some encoding problem occurs, please set UTF-8 as saving enconding from \emph{File/Save with Encoding...}
##' @export
##' 

renderMonthlyReport = function(month, year= NULL, geo = "England",
                        outFolder = settings$basedir) {

    # calculate Year
  if(!is.null(year)){
    if(!(is.numeric(year)|is.integer(year))){
      stop("year must be numeric")
    }else{
      if(!(nchar(year)%in%c(2,4))){
        stop("year incorretto")
      }else{
        if(nchar(year)==4){
          yearOrig = as.integer(year)
          year = substr(year,3,4)
        }
      }
    }
  }else{
    yearOrig = as.integer(year)
    year = substr(year(Sys.Date()),3,4)
  }
  
  # check month
  if(!(is.numeric(month)|is.integer(month))){
    stop("month must be numeric")
  }
  
  
  # creazione oggetto per successiva denominazione file finali
  month0 = stringr::str_pad(month,2,"left",0)
  
  #definizione nome folder
  monthNames = c("January", "February", "March", "April", "May", "June", "July",
               "August","September", "October", "November","December")  
  
  folderOut = paste0(outFolder,month0,"_",monthNames[month],"_",year)
  prefixName = paste0(year,month0)
  inFolder = paste0(folderOut,"/")
  monthTit = paste0(monthNames[month]," ",yearOrig)
   

  
  render(
    system.file("rmd",input="ReportMonth.Rmd",package = "PrescRiptions"), 
    output_format = prettydoc::html_pretty("architect"),
    output_dir = folderOut,
    params = list(
      monthTitle = monthTit,
      prefix = prefixName,
      geoArea = geo,
      outputFolder = folderOut,
      inputFolder = inFolder
    ),
    encoding = "UTF8",
    run_pandoc = TRUE,
    output_file = paste0(outFolder,prefixName,"_Report.html")
  )
  
}

