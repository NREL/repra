#' Calculate and summarize loss of load probability metrics
#'
#' Given a time data object, summarize probabilities into metrics. The function automatically
#' applies these calculation for each scenario, level of aggregation and area.
#'
#' The time data object must have a column called \code{NetLoad} (see the examples for an easy
#' method to generate it).
#'
#' Summary metrics include daily loss-of-load expectation (\code{LOLE}), loss-of-load
#' hours (\code{LOLH}), maximum LOLP (\code{PeakLOLP}) and expected unserved energy
#' (\code{EUE}).
#'
#' @param time.data Time series data formatted with \code{\link{format_timedata}}
#'        (must have a \code{NetLoad} column, see details)
#' @param outage.table Outage table used in the lookup, created with \code{\link{outage_table}}
#' @param raw Return summary metrics (\code{FALSE}, default) or raw hourly output (\code{TRUE})
#' @param ... Additional parameters passed to \code{\link{sliding_window}}
#' 
#' @export
#' @seealso \code{\link{format_timedata}} and \code{\link{outage_table}} to create \code{time.data}
#'          and \code{outage.table} objects, respectively
#' @seealso \code{\link{sliding_window}} is used internally to extend \code{time.data}
#' @seealso \code{\link{calculate_elcc}} uses this function to calculate ELCC
#' 
#' @examples
#' # Create outage table with 200 5-MW units
#' gens <- data.frame(Capacity = rep(5, 200),
#'                    EFOR = rep(0.08, 200))
#' out.table <- outage_table(gens)
#' 
#' # Create random load and wind data and format
#' tdata <- data.frame(Time = 1:8760,
#'                     Load = runif(8760, 450, 850),
#'                     Wind = runif(8760, 0, 100))
#' td <- format_timedata(tdata)
#' 
#' # Get metrics for net load (load - wind)
#' td2 <- td
#' td2$NetLoad <- td2$Load - td2$Wind
#' calculate_metrics(td2, out.table)
#' 
#' # Get metrics for just load
#' td3 <- td
#' td3$NetLoad <- td3$Load - td3$Wind
#' calculate_metrics(td3, out.table)
#' 
#' # Get raw data (i.e., not summarized)
#' calculate_metrics(td2, out.table, raw = TRUE)
calculate_metrics <- function(time.data, outage.table, raw = FALSE, ...) {
  # Check input data
  assert_that(time.data %has_name% "NetLoad")
  assert_that(is.numeric(time.data$NetLoad))
  assert_that(is.time.data(time.data))
  assert_that(is.outage.table(outage.table))
  assert_that(is.flag(raw))
  assert_that(check_level_areas(time.data, outage.table))
  
  # If necessary, expand time.data with a sliding window
  time.data2 <- sliding_window(time.data, ...)
  
  # Run the calculations for each scenario and area
  time.data2 %>%
    group_by_char(c(scenario_cols(time.data2), "Level", "Area")) %>%
    do(calculate_metrics_area(., outage.table, raw = raw))
}

# Internal calculations for calculate_metrics for one area
calculate_metrics_area <- function(time.data, outage.table, raw = FALSE) {
  # Convert time.data into a new data.table and sort by Level, Area and NetLoad
  time.data.dt <- data.table(time.data, key = "Level,Area,NetLoad")
  
  # Perform the lookup of time.data on the outage table
  out <- outage.table[time.data.dt, roll = TRUE]
  out[is.na(Capacity2), Capacity2 := 0]
  out[is.na(Prob), Prob := 0]
  out[is.na(LOLP), LOLP := 0]
  out[is.na(BaseEUE), BaseEUE := 0]
  out[, EUE := BaseEUE + LOLP * (Capacity - Capacity2)]
  
  # Summarize results if requested
  if (!raw) {
    # Aggregate up to the hour first (in case there is a sliding window)
    out <- out[, list(LOLP = sum(LOLP * WinProb, na.rm = TRUE),
                      EUE  = sum(EUE * WinProb, na.rm = TRUE)),
               by = "Day,Time"]
    
    # Aggregate up to the day second
    out <- out[, list(LOLE = max(LOLP, 0, na.rm = TRUE),
                      LOLH = sum(LOLP, na.rm = TRUE),
                      EUE  = sum(EUE)),
               by = "Day"]
    
    # Aggregate to g
    out <- out[, list(LOLE     = sum(LOLE, na.rm = TRUE),
                      LOLH     = sum(LOLH, na.rm = TRUE),
                      PeakLOLP = max(LOLE, 0, na.rm = TRUE),
                      EUE      = sum(EUE, na.rm = TRUE))]
  }
  
  out
}
