##' renderMonthlyReport
##'
##' Automatically generates a Markdown pdf report for a single month.
##'
##' @param settings analysis settings generated with \emph{setConfig()}
##' @param monthData list with all data for month, as returded by \emph{monthlyData_import()}
##' @param ccg In case a focus is neded for a specific CCG, the CCG to run the analysis and the report for.
##' @param gp In case a focus is neded for a specific GP, the GP to run the analysis and the report for
##' 
##' @return The function generates an html file.
##' @details The fucntion generate a monthly report composed of different sections: 
##'   - Report by Region: summary aggregations at England Level for the selected month.
##'   - Analysis by BNF codes: aggregations at BNF Code Level for the selected month.
##'   - Cost Summary by CCG: Net Ingredient cost for 1K patiensts in England for the selected month.
##'   - Costlier GPs:  list of practices who were in the higher end of the distribution of Net Ingredient Cost per 1K patients 
##'   - Report by CCG (customised section): aggregation for a single CCG, if selected.
##'   - Report by GP (customised section): aggregations for a single GP, if selected.
##'   If some encoding problem occurs, please set UTF-8 as saving enconding from \emph{File/Save with Encoding...}
##' @export
##' 

renderMonthlyReport = function(settings,monthData,ccg=NULL,gp=NULL) {

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
  
  postScr= switch (chooseInput,
                   "TT" = "_ccg_gp",
                   "TF" = "_ccg",
                   "FT" = "_gp",
                   "FF" = "_base"
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
      settings = settings
    ),
    encoding = "UTF8",
    run_pandoc = TRUE,
    output_file = paste0(outFolder,prefixName,"_Report",postScr,".html")
  )
  
}

