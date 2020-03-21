##' \code{PrescRiptions}
##' 
##' @docType package
##' @name PrescRiptions
##' @description aggregations and reports on Practice Level Prescription Data in England
##' @import utils
##' @import feather
##' @import data.table
##' @import stringr
##' @import readr
##' @import config
##' @importFrom RCurl getURL
NULL


utils::globalVariables(c("plpd","chem","plpdFiles","addr","td"))