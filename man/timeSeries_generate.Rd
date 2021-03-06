% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/timeSeries_generate.R
\name{timeSeries_generate}
\alias{timeSeries_generate}
\title{timeSeries_generate}
\usage{
timeSeries_generate(
  from = "mm/yyyy",
  to = "mm/yyyy",
  settings = NULL,
  on = c("BOTH"),
  save = TRUE
)
}
\arguments{
\item{from}{start date in the format "mm-yyyy"}

\item{to}{end date in the format "mm-yyyy"}

\item{settings}{analysis settings generated with \emph{setConfig()}}

\item{on}{one of \emph{"REGION"}, \emph{"BNF"} and \emph{"BOTH"} indicating which ts to produce,
default = "BOTH"}

\item{save}{logic, if TRUE, an external file will be saved in rootdir. Default = TRUE}
}
\value{
The function returns a data.table with time series data for the selected period, as generated, in the time range selected, 
 via  \emph{generateSummaries()}
}
\description{
generate Time series for regional and bnf data
}
\details{
The function summaries plpd or bnf data on ITEMS, NIC, ACT.COST, QUANTITY and REGION
and generates TS over the period of time indicated trought the \emph{from} and \emph{to} arguments
}
