#' Format time data
#'
#' Create valid data container that has aggregated load and variable generation (VG) time series for
#' different areas and levels of aggregation.
#'
#' The columns identified by \code{scenario} are used to perform calculations separately. This way one can run
#' different sensitivities in the same calculation (e.g., to estimate the capacity value of wind with different
#' penetration levels).
#'
#' Requirements for \code{data}:
#' \itemize{
#'   \item{The number of entries needs to be a multiple of \code{day.steps}}
#'   \item{A \code{Time} column that can be ordered (integer or time stamps)}
#'   \item{A \code{Load} column with load time series}
#'   \item{No column named \code{Level}, \code{NetLoad}, \code{VG}, \code{WinProb} or \code{Multiplier} (they are reserved name)}
#'   \item{Optionally, if a column called \code{Area} exists, it will be used to separate areas}
#'   \item{All columns must contain numbers except for \code{Time}, \code{Area} and those in \code{scenario}}
#' }
#'
#' Requirements for \code{levels}:
#' \itemize{
#'   \item{This parameter is optional}
#'   \item{If \code{levels} is provided, \code{data} must contain a column called \code{Area}}
#'   \item{The first column must contain the most granular level of data}
#'   \item{The column names in \code{levels} will become the names of the different levels of aggregation}
#' }
#'
#' @return A data frame with load and VG data aggregated by scenario and different areas and levels of aggregation
#'
#' @param data Data frame with the load and VG time series (see details for requirements)
#' @param levels Optional data frame that contains levels of aggregation, e.g., BAA, transmission
#'        area or interconnection (see details for requirements)
#' @param scenario Name of columns in \code{data} used to denote different scenarios
#' @param day.steps Number of data points in a day (defaults to 24)
#'
#' @seealso \code{\link{outage_table}} is the function that creates outage tables
#' @seealso \code{\link{sliding_window}} is used internally by several functions to extend
#'          time data objects
#'
#' @importFrom stats as.formula
#' @export
#'
#' @examples
#' # Create data for two days
#' tdata <- data.frame(Area = c(rep("A", 48), rep("B", 48)),
#'                     Time = 1:48,
#'                     Load = c(runif(48, 200, 250), runif(48, 400, 450)),
#'                     Wind = c(runif(48, 20, 25), runif(48, 40, 45)))
#' levs <- data.frame(BAA = c("A", "B"), Region = c("All", "All"))
#' 
#' # Format time data without and with different levels of aggregation
#' td1 <- format_timedata(tdata)
#' head(td1)
#' td2 <- format_timedata(tdata, levs)
#' head(td2)
#' 
#' # Format time data with a scenario column
#' tdata2 <- tdata
#' tdata2$Scenario <- "Scenario 1"
#' td3 <- format_timedata(tdata2, scenario = "Scenario")
#' head(td3)
#' 
#' # Format time data without Area column (minimum example)
#' tdata3 <- tdata
#' tdata2$Area <- NULL
#' td4 <- format_timedata(tdata3)
#' head(td4)
format_timedata <- function(data, levels = NULL, scenario = NULL, day.steps = 24) {
  # Check inputs
  for (i in c("Time", "Load"))
    assert_that(data %has_name% i)
  for (i in c("Level", "VG", "NetLoad", "WinProb", "Multiplier"))
    assert_that(data %>% has_no_name(i))
  
  assert_that(is.numeric(data$Load))
  if (any(is.na(data$Load)))
    stop("NA value found in column 'Load'", call. = FALSE)
  
  if (!is.null(levels)) {
    assert_that(is.data.frame(levels))
    assert_that(data %has_name% "Area")
    assert_that(check_areas(data, levels))
  }
  if (!is.null(scenario)) {
    assert_that(is.character(scenario))
    assert_that(not_empty(scenario))
    for (i in scenario)
      assert_that(data %has_name% i)
    for (i in c("Area", "Time", "Load"))
      assert_that(scenario %>% has_not_item(i))
  }
  assert_that(is.number(day.steps))
  assert_that(data %>% rows_multiple_of(day.steps))
  
  # Separate list of VG columns and check that they are numeric and that no NA's are present
  VG.cols <- setdiff(names(data), c("Time", "Area", "Load", scenario))
  for (i in VG.cols) {
    if (!inherits(data[[i]], "numeric"))
      stop("Column '", i, "' in data is not numeric.\n",
           "HINT: If the column is a scenario column, use 'scenario'. Otherwise, remove it.",
           call. = FALSE)
    
    if (any(is.na(data[[i]])))
      stop("NA value found in column '", i, "'", call. = FALSE)
  }
  
  # Time data doesn't have Area column, add it
  if (!"Area" %in% names(data))
    data$Area <- "default"
  
  # Make Load and VG columns into rows
  data2 <- data.table(data, key = "Area")
  data2 <- melt(data2, c(scenario, "Area", "Time"), variable.name = "Type", value.name = "Value")
  
  # If necessary, join time data with levels information
  if (!is.null(levels)) {
    levels2 <- data.table(levels, key = names(levels)[1])
    data2 <- levels2[data2]
  }
  
  # Make levels of aggregation into rows
  data3 <- melt(data2, c(scenario, "Time", "Type", "Value"), variable.name = "Level", value.name = "Area")
  data3$Area <- as.character(data3$Area)
  
  # Sum across levels of aggregation
  cols.temp <- paste(c(scenario, "Level", "Area", "Time", "Type"), collapse = ",")
  data4 <- data3[, list(Value = sum(Value)), by = cols.temp]
  
  # Make Load and VG columns again
  #   dcast.data.table returns a data.frame, but it's likely to change in the future
  form.temp <- paste(paste(c(scenario, "Level", "Area", "Time"), collapse = " + "), "~ Type")
  data5 <- data.table(dcast(data4, as.formula(form.temp), fun.aggregate = sum, value.var = "Value"))
  
  # Add Day
  cols.temp2 <- paste(c(scenario, "Level", "Area"), collapse = ",")
  data5[, Day := (order(Time) - 1) %/% day.steps + 1, by = cols.temp2]
  
  # Add probability column (this is modified when using a sliding window)
  data5[, WinProb := 1]
  
  # Drop class data.table for the output
  class(data5) <- "data.frame"
  
  # Change column order of the output
  first.cols <- c(scenario, "Level", "Area", "Time", "Day", "WinProb", "Load")
  rest.cols  <- setdiff(names(data5), first.cols)
  data5[, c(first.cols, rest.cols)]
}
