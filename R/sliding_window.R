#' Expand time data series with a sliding window
#'
#' This function allows to expand the time data series to include adjacent time steps and days, using a sliding window.
#' The procedure is used to shift the variable generation time series, so calculations are not limited to the observed
#' load and variable generation pairs. The objective is to further explore how the variable behaviour of these generators
#' affect reliability calculations.
#'
#' The sliding window can include adjacent time steps (typically adjacent hours) and/or adjacent days. Table 1
#' shows and example, with the current hour being hour 6 in day 5. If an hour is withing the extent of the sliding window
#' it is marked with an x.
#'
#'
#' Table 1: Example of sliding window
#'
#' \tabular{lcccccccccc}{
#'      \tab H1 \tab H2 \tab H3 \tab H4 \tab H5 \tab H6 \tab H7 \tab H8 \tab H9 \tab H10\cr
#'     Day 1 \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \cr
#'     Day 2 \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \cr
#'     Day 3 \tab \tab \tab x \tab x \tab x \tab x \tab x \tab x \tab x \tab \cr
#'     Day 4 \tab \tab \tab x \tab x \tab x \tab x \tab x \tab x \tab x \tab \cr
#'     Day 5 \tab \tab \tab x \tab x \tab x \tab Current \tab x \tab x \tab x \tab \cr
#'     Day 6 \tab \tab \tab x \tab x \tab x \tab x \tab x \tab x \tab x \tab \cr
#'     Day 7 \tab \tab \tab x \tab x \tab x \tab x \tab x \tab x \tab x \tab \cr
#'     Day 8 \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \cr
#'     Day 9 \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab
#' }
#'
#' \code{win.h.size} determines how many adjacent time steps are included in the sliding window. By default, the window
#' only includes the current hour. If specified, \code{win.h.size} must be a numeric vector with 2 values.
#' 
#' Similarly, \code{win.d.size} determines how many adjacent days are used in the window and defaults to the current day.
#' If defined, it must be a vector with 2 integers.
#'
#' For example, to reproduce the window in Table 1 set:
#' \itemize{
#'    \item{\code{win.h.size = c(-3, 3)} }
#'    \item{\code{win.d.size = c(-2, 2)} }
#' }
#' 
#' By default, all the data points in the sliding window are given the same probability in the calculations. It is possible
#' to provide different weights to adjacent hours and adjacent days. This is achieved by using the \code{wind.h.weight} and
#' \code{wind.d.weight} parameters, respectively. The lengths of these needs to be consistent with the corresponding sizes
#' parameters. Table 2 shows an example of how the final weights are calculated, by
#' setting:
#'
#' \itemize{
#'     \item{\code{wind.h.weight = c(0.1, 0.1, 0.2, 0.2, 0.2, 0.1, 0.1)} }
#'     \item{\code{wind.d.weight = c(0.1, 0.2, 0.4, 0.2 0.1)} }
#' }
#' 
#' Figure 2: Example of sliding window with different weights
#'
#' \tabular{rccccccccccc}{
#'      \tab H1 \tab H2 \tab H3 \tab H4 \tab H5 \tab H6 \tab H7 \tab H8 \tab H9 \tab H10 \tab Day weight \cr
#'     Day 1 \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \cr
#'     Day 2 \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \cr
#'     Day 3 \tab \tab \tab 0.01 \tab 0.01 \tab 0.02 \tab 0.02 \tab 0.02 \tab 0.01 \tab 0.01 \tab \tab 0.1\cr
#'     Day 4 \tab \tab \tab 0.02 \tab 0.02 \tab 0.04 \tab 0.04 \tab 0.04 \tab 0.02 \tab 0.02 \tab \tab 0.2\cr
#'     Day 5 \tab \tab \tab 0.04 \tab 0.04 \tab 0.08 \tab \bold{0.08} \tab 0.08 \tab 0.04 \tab 0.04 \tab \tab \bold{0.4}\cr
#'     Day 6 \tab \tab \tab 0.02 \tab 0.02 \tab 0.04 \tab 0.04 \tab 0.04 \tab 0.02 \tab 0.02 \tab \tab 0.2\cr
#'     Day 7 \tab \tab \tab 0.01 \tab 0.01 \tab 0.02 \tab 0.02 \tab 0.02 \tab 0.01 \tab 0.01 \tab \tab 0.1\cr
#'     Day 8 \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \cr
#'     Day 9 \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \cr
#'      \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \tab \cr
#'     Hour weight \tab \tab \tab 0.1 \tab 0.1 \tab 0.2 \tab \bold{0.2} \tab 0.2 \tab 0.1 \tab 0.1 \tab \tab
#' }
#'
#' Use of a sliding window is currently an open area of research. The authors suggest to use it as a sensitivitiy tool,
#' especially when only a few years of data is available. Special care should be taken with solar data. Because of its daily
#' pattern, a window with too many adjacent hours could return erroneous results.
#'
#' A vignette in the package presents some additional examples and graphs for the use of this function. It can
#' be accessed with \code{browseVignettes("sliding_window")}.
#'
#' @return An expanded data frame with the same format as \code{data.time}, but with a sliding window applied to the
#' variable generation columns
#'
#' @param time.data Time series data formatted with \code{\link{format_timedata}}
#' @param win.h.size Vector to determine the size of the window for adjacent time steps
#' @param win.h.weight Weights associated with adjacent hours
#' @param win.d.size Vector to determine the size of the window for adjacent days
#' @param win.d.weight Weights associated with adjacent days
#'
#' @seealso \code{\link{format_timedata}} to create the \code{time.data} object
#' @seealso This function is called from \code{\link{calculate_metrics}} and \code{\link{calculate_elcc}}
#'
#' @importFrom stats as.formula na.omit
#' @export
#'
#' @examples
#' # Create time data object
#' tdata <- data.frame(Area = c(rep("A", 48), rep("B", 48)),
#'                     Time = 1:48,
#'                     Load = c(runif(48, 200, 250), runif(48, 400, 450)),
#'                     Wind = c(runif(48, 20, 25), runif(48, 40, 45)))
#' td <- format_timedata(tdata)
#' head(td)
#' 
#' # If no data is provided, results remain intact
#' td2 <- sliding_window(td)
#' 
#' # Expand data for adjacent time steps (with equal and different weights)
#' td3 <- sliding_window(td, win.h.size = c(-1, 1))
#' td4 <- sliding_window(td, win.h.size = c(-1, 1), win.h.weight = c(1, 2, 1))
#' 
#' # Expand data for ajdacent days
#' td5 <- sliding_window(td, win.d.size = c(0, 1))
#' 
#' # Expand data for both adjacent times and days
#' td6 <- sliding_window(td, win.h.size = c(-1, 1), win.d.size = c(0, 1))
sliding_window <- function(time.data, win.h.size = NULL, win.h.weight = NULL, win.d.size = NULL, win.d.weight = NULL) {
  # If nothing needs to be done, return original data
  if (is.null(win.h.size) & is.null(win.d.size))
    return(time.data)
  
  # Check time data input
  assert_that(is.time.data(time.data))
  
  # Check hourly parameters
  if(!is.null(win.h.size)) {
    assert_that(is.numeric(win.h.size))
    assert_that(length(win.h.size) == 2)
    assert_that(win.h.size[2] >= win.h.size[1])
    
    if (is.null(win.h.weight)) {
      # If weight not provided, return equal probabilities
      temp.size = win.h.size[2] - win.h.size[1] + 1
      win.h.weight = rep(1 / temp.size, temp.size)
    } else {
      # Check validity of weight
      assert_that(is.numeric(win.h.weight))
      assert_that(all(win.h.weight >= 0))
      assert_that(sum(win.h.weight) > 0)
      assert_that(weight_is_right_size(win.h.weight, win.h.size))
      
      # Convert weights to probabilities
      win.h.weight = win.h.weight / sum(win.h.weight)
    }
  }
  
  # Check daily parameters
  if(!is.null(win.d.size)) {
    assert_that(is.numeric(win.d.size))
    assert_that(length(win.d.size) == 2)
    assert_that(win.d.size[2] >= win.d.size[1])
    
    if (is.null(win.d.weight)) {
      # If weight not provided, return equal probabilities
      temp.size = win.d.size[2] - win.d.size[1] + 1
      win.d.weight = rep(1 / temp.size, temp.size)
    } else {
      # Check validity of weight
      assert_that(is.numeric(win.d.weight))
      assert_that(all(win.d.weight >= 0))
      assert_that(sum(win.d.weight) > 0)
      assert_that(weight_is_right_size(win.d.weight, win.d.size))
      
      # Convert weights to probabilities
      win.d.weight = win.d.weight / sum(win.d.weight)
    }
  }
  
  # Expand for adjacent time steps
  if (is.null(win.h.size)) {
    h.data <- data.frame(h.size = 0, h.weight = 1)
  } else {
    h.data <- data.frame(h.size = seq(win.h.size[1], win.h.size[2]), h.weight = win.h.weight)
  }
  
  # Expand for adjacent days
  if (is.null(win.d.size)) {
    d.data <- data.frame(d.size = 0, d.weight = 1)
  } else {
    temp.step <- max(time.data$Time) / max(time.data$Day)
    d.data <- data.frame(d.size = seq(win.d.size[1], win.d.size[2]) * temp.step, d.weight = win.d.weight)
  }
  
  # Combine both
  expand.grid.df <- function(...) Reduce(function(...) merge(..., by = NULL), list(...))
  trans <- expand.grid.df(h.data, d.data)
  trans$size   <- trans$h.size   + trans$d.size
  trans$weight <- trans$h.weight * trans$d.weight
  
  # Melt data to treat all VG columns at once
  cols <- c(scenario_cols(time.data), "Level", "Area", "Time", "Day", "WinProb", "Load")
  td.melt <- melt(time.data, cols, variable.name = "VarGen", value.name = "value")
  
  # Lead/lag data
  lag.cols <- c(scenario_cols(time.data), "Level", "Area")
  for (i in 1:nrow(trans)) {
    col.name <- paste0("value_", i)
    td.melt <- td.melt %>%
      group_by_char(c(scenario_cols(time.data), "Level", "Area")) %>%
      do(lead_lag_column(., col.name, trans$size[i]))
  }
  
  # Melt again to get a line per item in the window
  td.melt$value <- NULL
  td.melt2 <- melt(td.melt, c(cols, "VarGen"), variable.name = "CaseProb", value.name = "value")
  
  # Rearrange the data to its original format
  temp.formula <- paste(paste(c(cols, "CaseProb"), collapse = " + "), "~ VarGen")
  td.cast <- dcast(td.melt2, as.formula(temp.formula))
  
  # Probabilities are saved in the CaseProb column, with some data that we need to eliminate
  td.cast$WinProb <- trans$weight[as.numeric(gsub(".+_", "", td.cast$CaseProb))]
  td.cast$CaseProb <- NULL
  
  # Remove NA's
  na.omit(td.cast)
}

# Auxiliary function to shift data inside sliding_window
lead_lag_column <- function(x, col.name, pos) {
  if (pos >= 0) {
    x[[col.name]] <- lead(x$value, abs(pos))
  } else {
    x[[col.name]] <- lag(x$value, abs(pos))
  }
  x
}
