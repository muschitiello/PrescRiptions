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
##' @import ggthemes
##' @import magrittr
##' @importFrom dplyr group_by_at summarise mutate select
##' @importFrom RCurl getURL
NULL


utils::globalVariables(c("plpd","chem","plpdFiles","addr","td",
                         "PERIOD","ITEMS","NIC","ACT.COST",
                         "QUANTITY","ONS_CCG_CODE","PRACTICE","EXTRACT_DATE",
                         "PUBLICATION"))