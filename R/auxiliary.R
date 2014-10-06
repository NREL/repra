# *** assert_that validation functions ***

# Test to make sure that "which" is not in the names of "x"
has_no_name <- function(x, which) !has_name(x, which)
on_failure(has_no_name) <- function(call, env) {
  paste0(deparse(call$x), " cannot have name ", eval(call$which, env))
}

# Test to make sure that x doesn't contain "which"
has_not_item <- function(x, which) !which %in% x
on_failure(has_not_item) <- function(call, env) {
  paste0(deparse(call$x), " cannot contain ", eval(call$which, env))
}

# Check that the number of rows in a is a multiple of b
rows_multiple_of <- function(a, b) nrow(a) %% b == 0
on_failure(rows_multiple_of) <- function(call, env) {
  paste0("Numbers of rows in ", deparse(call$a), " must be a multiple of ", deparse(call$b),
         " (currently ", eval(call$b, env), ")")
}

# Test that VG columns are numeric
is_VG_numeric <- function(x, col) {
  is.numeric(x[[col]])
}
on_failure(is_VG_numeric) <- function(call, env) {
  paste0(eval(call$col, env), " column in ", deparse(call$x), " is not numeric.\n",
         "HINT: If ", eval(call$col, env), " is a scenario column, use 'scenario'. Otherwise, remove it.")
}

# Is it an outage table?
is.outage.table <- function(x) "outagetable" %in% class(x)
on_failure(is.outage.table) <- function(call, env) {
  paste0(deparse(call$x), " must be an outage table created with the function outage_table.")
}

# Is is a valid time.data object?
is.time.data <- function(x) all(c("Level", "Area", "Day", "WinProb", "Load") %in% names(x))
on_failure(is.time.data) <- function(call, env) {
  paste0(deparse(call$x), " must be formatted with the function format_timedata.")
}

# Check that weight size is compative to the corresponding weight
weight_is_right_size <- function(weight, size) length(weight) == size[2] - size[1] + 1
on_failure(weight_is_right_size) <- function(call, env) {
  paste0(deparse(call$weight), " length must be ", eval(call$size, env)[2] - eval(call$size, env)[1] + 1,
         " to be compatible with ", deparse(call$size))
}

# Check that objective metric is valid
is_valid_obj <- function(x) x %in% c("LOLE", "LOLH", "PeakLOLP", "EUE")
on_failure(is_valid_obj) <- function(call, env) {
  paste0("Invalid ", deparse(call$x), ". Possible values: LOLE, LOLH, PeakLOLP, EUE")
}

# Test valid capacity vector
is_valid_capacity <- function(x) all(!is.na(x) & (x > 0))
on_failure(is_valid_capacity) <- function(call, env) {
  paste0("Invalid ", deparse(call$x), ". Must be positive and cannot contain NA's")
}

# Test valid EFOR vector
is_valid_efor <- function(x) all(!is.na(x) & (x >= 0) & (x <= 1))
on_failure(is_valid_efor) <- function(call, env) {
  paste0("Invalid ", deparse(call$x), ". Must be between [0, 1] and cannot contain NA's")
}

# Test areas and levels in time.data are in outage table
check_level_areas_res <- function(t, g) {
  t %>% select(Level, Area) %>% unique %>%
    anti_join(g[, list(Level, Area)] %>% unique, by = c("Level", "Area"))
}
check_level_areas <- function(t, g) nrow(check_level_areas_res(t, g)) == 0
on_failure(check_level_areas) <- function(call, env) {
  res <- check_level_areas_res(eval(call$t, env), eval(call$g, env))
  paste(c("The following Areas exist in time.data but not in the outage table.",
          capture.output(print(res))),
        collapse = "\n")
}

# Test areas in first data.frame are all in the second
check_areas_res <- function(x1, x2) {
  x1 %>% select(Area) %>% unique %>%
    anti_join(x2 %>% select(Area = 1) %>% unique, by = c("Area"))
}
check_areas <- function(x1, x2) nrow(check_areas_res(x1, x2)) == 0
on_failure(check_areas) <- function(call, env) {
  res <- check_areas_res(eval(call$x1, env), eval(call$x2, env))
  paste(c(paste0("The following Areas exist in ", deparse(call$x1),
                 " but not in ", deparse(call$x2), "."),
          capture.output(print(res))),
        collapse = "\n")
}


# *** Other ***
# Regroup with characters
group_by_char <- function(x, .dots) {
  dots <- .dots %>%
    as.list %>%
    lapply(as.symbol)
  group_by_(x, .dots = dots)
}

# Return the name of the columns that are designated as scenarios (before "Level")
scenario_cols <- function(d) {
  level.col <- which(names(d) == "Level")
  
  if(level.col > 1) {
    out <- names(d)[1:(level.col - 1)]
  } else {
    out <- NULL
  }
  
  out
}
