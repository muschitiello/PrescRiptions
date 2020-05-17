##' renderMonthlyReport
##'
##' Automatically generates a Markdown pdf report for a single month
##'
##' @param settings settings with parameters
##' @param monthData list with all data for month, as returded by \emph{monthlyData_import()}
##' @param ccg CCG to run the analysis and the report for
##' @param gp GP to run the analysis and the report for
##' @param sample if sample data have been used, use this for making a message appearing in the report.
##' 
##' @return The function generates a html file
##' @details If some encoding problem occurs, please set UTF-8 as saving enconding from \emph{File/Save with Encoding...}
##' @export
##' 

renderMonthlyReport = function(settings,monthData,ccg=NULL,gp=NULL,sample=F) {

  dirs = dirsGen(settings)
  year = settings$year
  month = as.integer(settings$month)
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
  monthTit = paste0(monthNames[month]," ",year)
  NameMonth = monthNames[month]
  
  
  ccgTF=ifelse(is.null(ccg),"F","T")
  gpTF=ifelse(is.null(gp),"F","T")
 
  chooseInput  = paste0(ccgTF,gpTF)

  inputRmd = switch (chooseInput,
                  "TT" = "ReportMonth_ccg_gp.Rmd",
                  "TF" = "ReportMonth_ccg.Rmd",
                  "FT" = "ReportMonth_gp.Rmd",
                  "FF" = "ReportMonth.Rmd"
  )
  
  rmarkdown::render(
    system.file("rmd",input=inputRmd,package = "PrescRiptions"), 
    output_format = "html_document",
    output_dir = folderOut,
    params = list(
      year = year,
      month = month,
      monthName = NameMonth,
      monthTitle = monthTit,
      prefix = prefixName,
      geoArea = geo,
      selccg = ccg,
      selgp = gp,
      outputFolder = folderOut,
      inputFolder = inFolder,
      monthData = monthData,
      settings = settings,
      sample = sample
    ),
    encoding = "UTF8",
    run_pandoc = TRUE,
    output_file = paste0(outFolder,prefixName,"_Report.html")
  )
  
}

