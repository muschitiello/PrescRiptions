##' dirsGen
##'
##' Reads paths and parameters from Settings and creates directories 
##' that will be used in the package for storing data and outputs
##'
##' @param settings setting object created via \emph{setConfig()} function containing paths, month and year of analysis and file names.
##' @return The function does not return anything but creates paths under the rootPath specified in the settings object.
##'  - 
##' @export 

dirsGen = function(settings){
  rootdir = settings$rootdir
  inputdir = paste0(rootdir,"/dataInput/")
  outputDir = paste0(rootdir,"/reports/")
  timeSeriesDir = paste0(rootdir,"/timeSeries/")
  plpdRootDir = paste0(inputdir,"01_plpd/")
  plpdRootDirSample = paste0(inputdir,"01_plpd_SAMPLE")
  bnfRootDir = paste0(inputdir,"02_bnf/")
  demogRootDir = paste0(inputdir,"03_demog/")
  qofRootDir = paste0(inputdir,"04_qof/")
  gitHubCsvDir = c("02_bnf/csv","03_demog/csv","04_qof/csv")
  gitPlpdZipDir = "01_plpd/01_plpd_SAMPLE"
  urlGH = "https://raw.githubusercontent.com/muschitiello/PrescRiptionsData/master/"
  
  out=list(
    rootdir = rootdir,
    inputdir = inputdir,
    outputDir = outputDir,
    timeSeriesDir = timeSeriesDir,
    plpdRootDir =plpdRootDir,
    plpdRootDirSample = plpdRootDirSample,
    bnfRootDir = bnfRootDir,
    demogRootDir = demogRootDir,
    qofRootDir = qofRootDir,
    gitHubCsvDir = gitHubCsvDir,
    gitPlpdZipDir = gitPlpdZipDir,
    urlGH = urlGH
  )
  
  return(out)
}

