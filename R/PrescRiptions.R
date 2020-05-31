##' \code{PrescRiptions}
##' 
##' @docType package
##' @name PrescRiptions
##' @description aggregations and reports on Practice Level Prescription Data in England
##' @import utils
##' @import kableExtra
##' @import data.table
##' @import stringr
##' @import readr
##' @import config
##' @import ggthemes
##' @import rmarkdown
##' @import ggplot2
##' @import tidyverse
##' @importFrom dplyr group_by_at select filter summarise mutate left_join
##' @importFrom scales unit_format
##' @importFrom RCurl getURL
NULL


utils::globalVariables(c("plpd","chem","plpdFiles","addr","td",
                         "PERIOD","ITEMS","NIC","ACT.COST",
                         "QUANTITY","ONS_CCG_CODE","PRACTICE","EXTRACT_DATE",
                         "PUBLICATION","settings","Var1","Var2","yyyy","mm","BNF.Chapter","X",
                         "AGE_GROUP_5","ONS_CODE","SEX","NIC_RATE","NIC_RATE_Z","ORG_CODE",
                         "BNF.Chapter.Code","ORG_TYPE","NUMBER_OF_PATIENTS"))