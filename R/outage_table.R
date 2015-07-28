#' Convolve generators to create outage tables
#'
#' Allows to create outage tables for different areas and levels of aggregation for a list of capacities
#' and equivalent forced outage rates (EFOR) for generators. The assumption is that the
#' generator outages are independent from each other. The table can be used
#' to quickly compute loss-of-load probability (LOLP) and expected unserved
#' energy (EUE).
#'
#' Requirements for \code{generators}:
#' \itemize{
#'   \item{The data frame must contain two columns: \code{Capacity} and \code{EFOR} (Other columns will
#'         be ignored)}
#'   \item{Optionally if a column called \code{Area} exists, it will be used to create separate tables
#'         within each area}
#' }
#'
#' Requirements for \code{levels}:
#' \itemize{
#'   \item{This parameter is optional}
#'   \item{If \code{levels} is provided, \code{generators} must contain a column called \code{Area}}
#'   \item{The first column must contain the most granular level of data and compatible with columns
#'         \code{Area} in \code{generators}}
#'   \item{The column names in \code{levels} will become the names of the different levels of aggregation}
#' }
#'
#' @return The result is a \link[data.table]{data.table} object that is used to calculate
#' LOLP metrics. For each area, the output presents the following columns: \code{Capacity},
#' \code{Prob}, \code{LOLP} and \code{BaseEUE}. The column \code{Capacity} is
#' duplicated as (\code{Capacity2}) and is used internally by the LOLP calculation functions.
#'
#' Given a load level \code{L}, the probability parameters are calculated as follows:
#' \itemize{
#'   \item{Choose the first row with \code{L <= Capacity}}
#'   \item{The loss-of-load probability is the value in the \code{LOLP} for that row}
#'   \item{Expected unserved energy can be quickly calculated as
#'         \code{EUE = BaseEUE + (L - Capacity) * LOLP}. This is 
#'         equivalent to calculating the sum of \code{(L - Capacity) * Prob} for all
#'         rows with \code{L < Capacity}}
#' }
#'
#' @param generators Data frame with the list of generators (see details for requirements)
#' @param levels Optional data frame that contains levels of aggregation, e.g., BAA, transmission
#'        area or interconnection (see details for requirements)
#' @param threshold Table entries with LOLP below this value are ignored
#' @param round Used to round capacities (default is 1 MW)
#'
#' @seealso \code{\link{format_timedata}} is the function that creates compatible time data objects
#'
#' @references R. Billinton, and R. N. Allan, Reliability evaluation of power systems,
#'     New York: Plenum Press 1996.
#' @importFrom stats weighted.mean
#' @export
#' @useDynLib repra
#'
#' @examples
#' # Outage table with one 10-MW generator with 2% EFOR
#' outage_table(data.frame(Capacity = 10, EFOR = 0.02))
#'
#' # Outage table with two generators (10 MW with 2% EFOR and 20 MW with 1% EFOR)
#' outage_table(data.frame(Capacity = c(10, 20), EFOR = c(0.02, 0.01)))
#' 
#' # List of generators and areas
#' gens <- data.frame(Area = c(rep("A", 10), rep("B", 5)),
#'                    Capacity = rep(60, 15),
#'                    EFOR = rep(0.08, 15))
#' levs <- data.frame(BAA = c("A", "B"), Region = c("All", "All"))
#' 
#' # Create a single outage table (without specifying area)
#' outage_table(gens[, c("Capacity", "EFOR")])
#' 
#' # Create an outage table for each 'Area'
#' outage_table(gens)
#' 
#' # Create table for each 'Area' and different and levels of aggregation in 'levs'
#' outage_table(gens, levs)
outage_table <- function(generators, levels = NULL, threshold = 1e-06, round = 1) {
  # Check that 'generators has Capacity and EFOR columns
  assert_that(generators %has_name% "Capacity", generators %has_name% "EFOR")
  assert_that(is.numeric(generators$Capacity), is.numeric(generators$EFOR))
  assert_that(is.scalar(threshold), is.scalar(round))
  assert_that(is_valid_capacity(generators$Capacity))
  assert_that(is_valid_efor(generators$EFOR))
  if (!is.null(levels)) {
    assert_that(is.data.frame(levels))
    assert_that(generators %has_name% "Area")
    assert_that(check_areas(generators, levels))
  }
  
  if (!is.null (levels)) {
    # If levels provided, join it with the generators data
    levels.list <- names(levels)
    levels2 <- data.table(levels, key = names(levels)[1])
    generators2 <- data.table(generators, key = "Area")
    generators2 <- levels2[generators2]
  } else {
    if (!"Area" %in% names(generators)) {
      # List of generators doesn't have area data
      generators$Area <- "default"
    }
    
    generators2 <- data.table(generators, key = "Area")
    levels.list <- "Area"
  }
  
  # Calculate table for each aggreagation level
  out <- list()
  generators2[, RoundedCapacity := round * round(Capacity / round)]
  for (i in levels.list) {
    out[[i]] <- generators2[, convolution_table(RoundedCapacity, EFOR, threshold), by = i]
    setnames(out[[i]], i, "Area")
    out[[i]][, Level := i]
  }
  
  # Join all the levels in one single data.table, change column order and set keys
  out2 <- rbindlist(out)
  out2[, Level := factor(Level, levels = levels.list)]
  setcolorder(out2, c("Level", names(out2)[-length(names(out2))]))
  setkey(out2, Level, Area, Capacity)
  class(out2) <- c("outagetable", class(out2)) 
  out2
}

# Shortcut to plot outage tables
#' @export
#' @method plot outagetable
plot.outagetable <- function(x, ..., level = NULL) {
  if (!is.null(level))
    x <- x[level]
  
  ggplot(x, aes(x = Capacity / 1e3, y = LOLP)) +
    geom_line() +
    facet_wrap(~ Level + Area, scales = "free_x") +
    labs(title = "Outage curves", x = "Capacity (GW)")
}

# Shortcut to get information summary from outage tables
#' @export
#' @method summary outagetable
summary.outagetable <- function(object, ...) {
  object[, list(Max  = max(Capacity),
                Mean = weighted.mean(Capacity, Prob),
                SD   = sqrt(sum(Prob * (weighted.mean(Capacity, Prob) - Capacity)^2))),
         by = "Level,Area"]
}
