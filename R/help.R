#' The Renewable Energy Probabilistic Resource Assessment tool
#'
#' This package is the R implementation of the Renewable Energy Probabilistic Resource Assessment (REPRA)
#' tool, developed at the National Renewable Energy Laboratory (NREL).
#'
#' More information is available in the help of the package functions. Additionally, two vignettes
#' are included with this package and can be accessed with \code{browseVignettes("repra")} or
#' \code{browseVignettes("sliding_window")}.
#' 
#' @references Eduardo Ibanez, Michael Milligan (2012). Impact of Transmission on Resource
#'   Adequacy in Systems with Wind and Solar Power. IEEE Power& Energy Society General
#'   Meeting, 22-26 July 2012, San Diego, CA. 
#'   \url{http://www.nrel.gov/docs/fy12osti/53482.pdf}.
#' 
#' @docType package
#' @import dplyr reshape2 ggplot2 Rcpp assertthat
#' @importFrom data.table data.table := dcast.data.table setnames setcolorder setkey rbindlist
#' @name repra-package
#' @aliases repra repra-package
NULL

#' Generator and time series data sets
#'
#' \code{repra} includes sample data to showcase typical calculations. This data is contained in four
#' data frames called \code{repratime}, \code{repragen}, \code{repraareas}, \code{repra capacity}. The
#' data was adapted from the Western Electricity Coordinating Council's (WECC) Transmission Expansion
#' Planning Policy Committee (TEPPC) 2024 Common Case (version 1.0). The data should be considered only
#' for illustrative purposes and no conclusions should be made based on it.
#' 
#' The data is presented for 8 regions in the Western Interconnection: Alberta, AZ-NM-NV (Arizona-New
#' Mexico-Nevada), Basin, British Columbia, California North and South, NWPP (Northwest Power Pool), and
#' RMPA (Rocky Mountain Power Authority). It is presented through four data frames, which are described
#' in the following paragraphs.
#'
#' \code{repratime} includes the load, wind, PV and CSP time series for the eight regions.
#'
#' \code{repragen} includes the name, nameplate capacity, expected forced outage rate (EFOR) and area for
#' all the generators (that are not wind, PV or CSP).
#'
#' \code{repraareas} includes a little example of how the eight areas can be aggregated up to the
#' interconnection level.
#'
#' Finally, \code{repracapacities} summarize the installed capacities for wind, PV and CSP (which are
#' useful to normalize capacity value, for example).
#'
#' @docType data
#' @keywords datasets
#' @name repra.data
#' @aliases repradata repra-data repraareas repragen repratime repracapacity
#' @references Western Electricity Coordinating Council (2014). Transmission Expansion
#'   Planning Policy Committee (TEPPC) 2024 Common Case (version 1.0). 
#'   \url{https://www.wecc.biz/TransmissionExpansionPlanning/Pages/Datasets.aspx}.
NULL
