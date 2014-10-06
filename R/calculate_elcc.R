#' Calculate effective load carrying capability (ELCC)
#'
#' Given a time data object (with net load data), find the effective load carrying capability (ELCC) to achieve
#' the desired reliability metric. The function automatically
#' applies these calculation for each scenario, level or aggregation and area.
#'
#' The following metrics can be selected as an objective (through \code{obj.metric}):
#' \itemize{
#'   \item{Daily loss of load expectation (\code{LOLE}): Calculated as the sum of the maximum daily LOLP}
#'   \item{Loss of load hours (\code{LOLH}): Sum of all the LOLP}
#'   \item{Maximum daily LOLP (\code{PeakLOLP}): Maximum LOLP value observed}
#'   \item{Expected unserved energy (\code{EUE}): Sum of EUE for all time steps}
#' }
#'
#' ELCC is calculated by modifying the load profile so that the final profile yields a certain reliability
#' level. This can be done in two ways:
#' \itemize{
#'   \item{by scaling the entire profile (e.g., by multiplying the entire time series by 1.05)}
#'   \item{by adding a constant number to the entire profiles (e.g., adding 100 MW)}
#' }
#' 
#' \code{scale.load} needs to be set to \code{TRUE} or \code{FALSE} to achieve either one, respectively.
#'
#' @param time.data Time series data formatted with \code{\link{format_timedata}}
#' @param outage.table Outage table used in the lookup, created with \code{\link{outage_table}}
#' @param obj.metric Metric to be used as objective: \code{LOLE, LOLH, PeakLOLP, EUE} (see details)
#' @param obj.value Objective value to achieve for the selected metric
#' @param scale.load During the iterations is the load scaled (\code{TRUE}) or the same value added to all times (\code{FALSE})
#' @param max.iter Maximum iterations to perform before stopping
#' @param ignore Column names in \code{time.data} to ignore in the calculation of total VG data
#' @param ... Additional parameters passed to \code{\link{sliding_window}}
#' 
#' @export
#' @seealso \code{\link{format_timedata}} and \code{\link{outage_table}} to create \code{time.data}
#'          and \code{outage.table} objects, respectively
#' @seealso \code{\link{sliding_window}} is used internally to extend \code{time.data}
#' @seealso \code{\link{calculate_metrics}} is used internally to evaluate the metrics
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
#'                     Wind = runif(8760, 0, 100),
#'                     Wind2 = runif(8760, 0, 200))
#' td <- format_timedata(tdata)
#' 
#' # Calculate ELCC with both Wind and Wind2
#' calculate_elcc(td, out.table)
#' 
#' # Calculate ELCC with only Wind
#' calculate_elcc(td, out.table, ignore = "Wind2")
calculate_elcc <- function(time.data, outage.table, obj.metric = "LOLE", obj.value = 0.1,
                           scale.load = FALSE, max.iter = 20, ignore = NULL, ...) {
  # Check input data
  assert_that(is.time.data(time.data))
  assert_that(is.outage.table(outage.table))
  assert_that(is.string(obj.metric))
  assert_that(is_valid_obj(obj.metric))
  assert_that(is.scalar(obj.value))
  assert_that(obj.value > 0)
  assert_that(is.flag(scale.load))
  assert_that(is.count(max.iter))
  if (!is.null(ignore)) {
    assert_that(is.character(ignore))
    for (i in ignore)
      assert_that(time.data %has_name% i)
  }
  assert_that(check_level_areas(time.data, outage.table))

  
  # Check what VG is available
  ignore.defaults <- c("Level", "Area", "Time", "Day", "WinProb", "Load", "VG", "NetLoad")
  VG.cols <- setdiff(names(time.data),
                     c(scenario_cols(time.data), ignore.defaults, ignore))
  
  # Check that all columns in VG.cols are numeric
  for (i in VG.cols)
    assert_that(is_VG_numeric(time.data, i))
  
  # If necessary, expand time.data with a sliding window
  time.data2 <- sliding_window(time.data, ...)
  
  # Run calculations for each scenario and area
  time.data2 %>%
    group_by_char(c(scenario_cols(time.data2), "Level", "Area")) %>%
    do(calculate_elcc_area(., outage.table, obj.metric, obj.value, scale.load, max.iter, VG.cols))
}

# ELCC calculations for one area
calculate_elcc_area <- function(time.data, outage.table, obj.metric, obj.value, scale.load, max.iter, VG.cols) {
  # Calculate total VG time series and column to save data
  VG.report <- ""
  time.data$VG <- 0
  if (length(VG.cols) > 0) {
    VG.report <- paste(VG.cols, collapse = " + ")
    for (i in VG.cols)
      time.data$VG <- time.data$VG + time.data[[i]]
  }
  
  # Initial guess for load multiplier
  mult1 <- guess_multiplier(outage.table, time.data, 0.1)
  res <- calculate_metrics_mult(time.data, outage.table, mult1, scale.load)
  
  # Save the current value for the metric
  val <- res[[obj.metric]]
  
  # Second guess before initializing the loop
  if (val < obj.value) {
    min.mult <- mult1
    min.val  <- val
    max.mult <- guess_multiplier(outage.table, time.data, 0.5)
    res <- calculate_metrics_mult(time.data, outage.table, max.mult, scale.load)
    max.val  <- res[[obj.metric]]
  } else {
    max.mult <- mult1
    max.val  <- val
    min.mult <- guess_multiplier(outage.table, time.data, 0.001)
    res <- calculate_metrics_mult(time.data, outage.table, min.mult, scale.load)
    min.val  <- res[[obj.metric]]
  }
  
  # Iterate until convergence or maximum number of iterations
  iter = 1
  iter.log <- res
  val <- res[[obj.metric]]
  
  while(iter <= max.iter & (abs(val - obj.value) > 0.001 * obj.value)) {
    # If multiplier is zero and the objective cannot be met, stop iterations
    if (min.mult == 0 & (min.val > obj.value)) {
      curr.mult <- 0
      break
    }
    
    # Calculate new multiplier doing a exponential interpolation
    #     Current best values need to be positive and not identical
    if (max.val < obj.value) {
      curr.mult <- max.mult * 1.2
    } else if (min.val > obj.value) {
      curr.mult <- min.mult / 1.2
    } else if (min.val == 0) {
      curr.mult <- mean(c(min.mult, max.mult))
    } else {
      curr.mult <- min.mult + (max.mult - min.mult) *
                              (log(obj.value) - log(min.val)) /
                              (log(max.val) - log(min.val))
    }
    
    # Avoid negative multipliers
    curr.mult <- max(0, curr.mult)
    
    # Calculate results with new load multiplier
    res <- calculate_metrics_mult(time.data, outage.table, curr.mult, scale.load)
    val <- res[[obj.metric]]
    
    if (min.val > obj.value) {
      min.val <- val
      min.mult <- curr.mult
    } else if (max.val < obj.value) {
      max.val <- val
      max.mult <- curr.mult
    } else if (val < obj.value) {
      min.val <- val
      min.mult <- curr.mult
    } else {
      max.val <- val
      max.mult <- curr.mult
    }
    
    # Save iterations in a log
    iter.log <- rbind(iter.log, res)
    iter <- iter + 1
  }
  
  # Issue a warning if maximum number of iterations was reached
  if (abs(val - obj.value) > 0.001 * obj.value)
    warning("Could not converge in ", max.iter, " iterations ",
            "(Level: ", time.data$Level[1], ", Area: ", time.data$Area[1],
            ", VG: ", VG.report, "). Reported error is ", val / obj.value - 1,
            call. = FALSE)
  
  # Format output after exiting the loop
  res$mult <- NULL
  data.frame(Metric     = obj.metric,
             Objective  = obj.value,
             ActualObj  = val,
             ErrorObj   = val / obj.value - 1,
             Multiplier = curr.mult,
             ELCC       = curr.mult * max(time.data$Load),
             VG         = VG.report,
             LoadMethod = ifelse(scale.load, "Load scaled", "Flat block"),
             res)
}

# Used to create a guess based on the convolution table
guess_multiplier <- function(outage.table, time.data, level) {
  cap <- outage.table[data.table(time.data$Level[1], time.data$Area[1])][LOLP >= level]$Capacity[1]
  max(0, cap / max(time.data$Load - time.data$VG))
}

# Calculate metrics after scaling load
calculate_metrics_mult <- function(time.data, outage.table, multiplier, scale.load) {
  if (scale.load) {
    time.data$NetLoad <- time.data$Load * multiplier - time.data$VG
  } else {
    time.data$NetLoad <- time.data$Load - time.data$VG + max(time.data$Load) * (multiplier - 1)
  }
  out <- calculate_metrics_area(time.data, outage.table)
  out[, mult := multiplier]
  out
}
