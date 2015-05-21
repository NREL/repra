#' Calculate capacity value for variable generation
#'
#' Given time data series and an outage table, calculate the capacity value of variable generation (VG) time series.
#' Capacity value is calculated by iteratively removing one type of VG at a time and recalculating effective
#' load carrying capability (ELCC).
#'
#' This function calculates ELCC for the system with different combinations of VG. The metric and desired reliability
#' metric can be set; see \code{\link{calculate_elcc}} for details and a list of parameters. Additionally, a sliding
#' window can be used in the calculation (see \code{\link{calculate_elcc}}).
#'
#' When calculating the capacity value of VG, the results depends on the order in which the different technologies
#' are added into the system. The variable \code{VG.cols} is used to determine this order manually. If not determined
#' the order will default to the order in which the VG columns appear in \code{time.data}.
#'
#' This \code{marginal} parameter determines if the capacity value is calculated by taking each VG profile out
#' of the total mix (when is set to \code{TRUE}) or by progressively stacking the VG profiles on top the conventional
#' generators (when is set to \code{FALSE}). The latter option is the default and the order is determined by the
#' \code{VG.cols} parameter or, if not set, by the order in which the columns appear in the \code{time.data} object.
#'
#' If \code{scale.first} is set to \code{TRUE} the capacity value a special calculation is triggered. Before perfoming the
#' calculations, the load is scaled so that the total mix (conventional generators and VG) provide the desired adequacy
#' level, which is specified with the parameters passed to \code{\link{calculate_elcc}}. Once the load is scaled, the
#' capacity value calculations are performed using the flat block method.
#'
#' @param time.data Time series data (or subset) formatted with \code{\link{format_timedata}}
#' @param outage.table Outage table used in the lookup, created with \code{\link{outage_table}}
#' @param VG.cols Order in which VG capacity value is calculated (see details for more info)
#' @param marginal Should the VG capacity value be calculated as the last resource (\code{TRUE}) or added progressively to the load (\code{FALSE})?
#' @param scale.first This triggers a special case to calculate capacity value. See details for more information.
#' @param ... Additional parameters passed to \code{\link{calculate_elcc}} and \code{\link{sliding_window}}
#' 
#' @export
#' @seealso \code{\link{format_timedata}} and \code{\link{outage_table}} to create \code{time.data}
#'          and \code{outage.table} objects, respectively
#' @seealso \code{\link{sliding_window}} is used internally to extend \code{time.data}
#' @seealso \code{\link{calculate_elcc}} is used internally to evaluate ELCC
#' 
#' @examples
#' \dontrun{
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
#' # Calculate capacity value (both are equivalent)
#' capacity_value(td, out.table)
#' capacity_value(td, out.table, c("Wind", "Wind2"))
#' 
#' # Calculate capacity value of Wind2 first and Wind second
#' capacity_value(td, out.table, c("Wind", "Wind2"))
#' 
#' # Calculate capacity value with a sliding window that uses adjacent hours
#' capacity_value(td, out.table, win.h.size = c(-1, 1))
#' }
capacity_value <- function(time.data, outage.table, VG.cols = NULL, marginal = FALSE,
                           scale.first = FALSE, ...) {
  # Check input data
  assert_that(is.time.data(time.data))
  assert_that(is.outage.table(outage.table))
  assert_that(is.flag(marginal))
  if (!is.null(VG.cols)) {
    assert_that(is.character(VG.cols))
    assert_that(not_empty(VG.cols))
    for (i in VG.cols)
      assert_that(time.data %has_name% i)
  }
  assert_that(check_level_areas(time.data, outage.table))
  
  # Check what VG is available
  if (is.null(VG.cols)) {
    ignore.defaults <- c("Level", "Area", "Time", "Day", "WinProb", "Load", "VG", "NetLoad")
    VG.cols <- setdiff(names(time.data),
                       c(scenario_cols(time.data), ignore.defaults))
  } else {
    VG.cols <- VG.cols
  }
  
  # Check that there are sufficient VG cols
  if (length(VG.cols) == 0)
    stop("Not enough VG columns provided", call. = FALSE)
  
  # Capture other parameters passed to ELCC calculation
  dots <- list(...)
  dots$time.data <- time.data
  dots$outage.table <- outage.table
  
  # Scale load so that total mix meets the objective value
  if (scale.first == TRUE) {
    dots$scale.load <- TRUE
    
    multipliers <- do.call(calculate_elcc, dots)
    cat("Multipliers used in scalation of load\n")
    print(multipliers %>%
           select(Level, Area, Multiplier, Objective, ActualObj, ErrorObj) %>%
           as.data.frame)
    
    multipliers.join <- multipliers %>% select(Level, Area, Multiplier)
    multipliers.name <- setdiff(names(multipliers.join), "Multiplier")
    
    dots$time.data <- time.data %>%
      inner_join(multipliers.join, by = multipliers.name) %>%
      mutate(Load = Load * Multiplier) %>%
      select(-Multiplier)
    
    dots$scale.load <- FALSE
  }
  
  # Calculate ELCC adding one VG at a time
  out <- NULL
  for (i in 1:(length(VG.cols) + 1)) {
    if (i > length(VG.cols)) {
      dots$ignore <- NULL
    } else if (marginal) {
      dots$ignore <- VG.cols[i]
    } else {
      dots$ignore <- VG.cols[i:length(VG.cols)]
    }
    out.temp <- do.call(calculate_elcc, dots)
    out <- rbind(out, out.temp)
  }
  
  # Calculate capacity values
  if (marginal) {
    out$CVTech <- rep(c(VG.cols, NA), each = nrow(out) / (length(VG.cols) + 1))
    out <- out %>%
      group_by_char(.dots = c(scenario_cols(time.data), "Level", "Area")) %>%
      mutate(CV = max(ELCC) - ELCC) %>%
      filter(!is.na(CVTech))
  } else {
    n.each <- nrow(out) / (length(VG.cols) + 1)
    out <- out %>%
      ungroup %>%
      mutate(CVTech = rep(c("BaseELCC", VG.cols), each = n.each),
             CV = ELCC - lag(ELCC, n.each, 0))
  }
  
  out
}
