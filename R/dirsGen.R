##' dirsGen
##'
##' generate directories to be used in the package
##'
##' @param settings settings where to take the root dir
##' 
##' @export 

dirsGen = function(settings){
  rootdir = settings$rootdir
  inputdir = paste0(rootdir,"/dataInput/")
  outputDir = paste0(rootdir,"/output/")
  timeSeriesDir = paste0(rootdir,"/timeSeries/")
  plpdRootDir = paste0(inputdir,"01_plpd/")
  bnfRootDir = paste0(inputdir,"02_bnf/")
  demogRootDir = paste0(inputdir,"03_demog/")
  qofRootDir = paste0(inputdir,"04_qof/")
  gitHubCsvDir = c("02_bnf/csv","03_demog/csv","04_qof/csv")
  urlGH = "https://raw.githubusercontent.com/muschitiello/PrescRiptionsData/master/"
  
  out=list(
    rootdir = rootdir,
    inputdir = inputdir,
    outputDir = outputDir,
    timeSeriesDir = timeSeriesDir,
    plpdRootDir =plpdRootDir,
    bnfRootDir = bnfRootDir,
    demogRootDir = demogRootDir,
    qofRootDir = qofRootDir,
    gitHubCsvDir = gitHubCsvDir,
    urlGH = urlGH
  )
  
  return(out)
}

