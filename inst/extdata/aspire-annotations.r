library(dplyr, warn.conflicts = FALSE)
library(utils)
library(rlang)
library(stats)
library(lubridate)

# Annotator functions for MPOG ASPIRE measurements

############
# Run Once #
############

setup_cache <- function(data, spek){
  cache <- list()

  # Hardcode column names.  Extract from spek in subsequent versions.
  cache$id_colname      <- 'id'
  cache$denom_colname   <- 'Denominator'
  cache$numer_colname   <- 'Passed_Count'
  cache$time_colname    <- 'Month'
  cache$measure_colname <- 'Measure_Name'

  # Make a symbol of the numerator and denominator columns
  cache$denom_col_sym   <- rlang::sym(cache$denom_colname)
  cache$numer_col_sym   <- rlang::sym(cache$numer_colname)
  cache$id_col_sym      <- rlang::sym(cache$id_colname)
  cache$time_col_sym    <- rlang::sym(cache$time_colname)
  cache$measure_col_sym <- rlang::sym(cache$measure_colname)

  # Calculate peer average by measure
  #cache$peer_mean <- calc_peer_measure_means(data, cache)
  cache$guideline <- 0.90

  return(cache)
}

####################
# Helper Functions #
####################

# Cache is only going to be in the top level context.... fuck.
calc_peer_measure_means <- function(data, cache){
  numer <- cache$numer_col_sym
  denom <- cache$denom_col_sym
  measure <- cache$measure_col_sym

  df <- data %>% 
    group_by(!!measure) %>%
    mutate(rate = !!numer / !!denom) %>%
    summarize(mean = mean(rate))

  res <- as.list( df[['mean']] )
  names(res) <- df[[measure_colname]]
  return(res)
}

eval_positive_trend <- function(x){
  if(length(x) < 3){ return(FALSE) }
  is_tail_ascending <- !is.unsorted(utils::tail(x,3), strictly=T)
  return(is_tail_ascending)
}

eval_negative_trend <- function(x){
  if(length(x) < 3){ return(FALSE) }
  is_tail_descending <- !is.unsorted(rev(utils::tail(x,3)), strictly=T)
  return(is_tail_descending)
}

########################
# Annotation Functions #
########################

annotate_negative_gap_guideline <- function(data, spek){
  time <- cache$time_col_sym
  denom <- cache$denom_col_sym
  numer <- cache$numer_col_sym
  id <- cache$id_col_sym

  data %>%
    dplyr::filter(!!time == max(!!time)) %>%
    mutate(rate = !!numer / !!denom) %>%
    group_by(!!id) %>%
    summarize(negative_gap = rate < cache$guideline)
}

annotate_positive_gap_guideline <- function(data, spek){
  time <- cache$time_col_sym
  denom <- cache$denom_col_sym
  numer <- cache$numer_col_sym
  id <- cache$id_col_sym

  data %>%
    dplyr::filter(!!time == max(!!time)) %>%
    mutate(rate = !!numer / !!denom) %>%
    group_by(!!id) %>%
    summarize(positive_gap = rate > cache$guideline)
}

annotate_negative_trend <- function(data, spek){
  time <- cache$time_col_sym
  denom <- cache$denom_col_sym
  numer <- cache$numer_col_sym
  id <- cache$id_col_sym

  max_month <- max(data[[time]])
  min_month <- max_month %m-% months(2)

  data %>%
    dplyr::filter(!!time >= min_month) %>%
    mutate(rate = !!numer / !!denom) %>%
    group_by(!!id) %>%
    summarize(negative_trend = eval_negative_trend(rate))
}

annotate_positive_trend <- function(data, spek){
  time <- cache$time_col_sym
  denom <- cache$denom_col_sym
  numer <- cache$numer_col_sym
  id <- cache$id_col_sym

  max_month <- as_date(max(data[[time]]))
  min_month <- max_month %m-% months(2)

  data %>%
    dplyr::filter(!!time >= min_month) %>%
    mutate(rate = !!numer / !!denom) %>%
    group_by(!!id) %>%
    summarize(positive_trend = eval_positive_trend(rate))
}
