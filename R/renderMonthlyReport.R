##' renderMonthlyReport
##'
##' Automatically generates a Markdown pdf report for a single month
##'
##' @param settings settings with parameters
##' @param monthData list with all data for month, as returded by \emph{monthlyData_import()}
##' 
##' @return The function generates a html file
##' @details If some encoding problem occurs, please set UTF-8 as saving enconding from \emph{File/Save with Encoding...}
##' @export
##' 

renderMonthlyReport = function(settings,monthData) {

  dirs = dirsGen(settings)
  year = settings$year
  month = settings$month
  geo = settings$region
  outFolder = dirs$outputDir
  month0=as.character(stringr::str_pad(settings$month,width = 2,side = "left",pad = "0"))
  year2 = substr(year,3,4)

  #definizione nome folder
  monthNames = c("January", "February", "March", "April", "May", "June", "July",
               "August","September", "October", "November","December")  
  
  folderOut = paste0(outFolder,month0,"_",monthNames[month],"_",year)
  prefixName = paste0(year,month0)
  inFolder = paste0(folderOut,"/")
  monthTit = paste0(monthNames[month]," ",yearOrig)
  NameMonth = monthNames[month]
  
  
  
  render(
    system.file("rmd",input="ReportMonth.Rmd",package = "PrescRiptions"), 
    output_format = prettydoc::html_pretty("architect"),
    output_dir = folderOut,
    params = list(
      year = year,
      month = month,
      monthName = NameMonth,
      monthTitle = monthTit,
      prefix = prefixName,
      geoArea = geo,
      outputFolder = folderOut,
      inputFolder = inFolder,
      monthData = monthData,
      settings = settings
    ),
    encoding = "UTF8",
    run_pandoc = TRUE,
    output_file = paste0(outFolder,prefixName,"_Report.html")
  )
  
}

