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

  # Id column will always be id
  cache$id_colname      <- 'id'
  cache$id_col_sym      <- rlang::sym(cache$id_colname)

  # Hardcode column names.  Extract from spek in subsequent versions.
  cache$denom_colname   <- 'Denominator'
  cache$numer_colname   <- 'Passed_Count'
  cache$time_colname    <- 'Month'
  cache$measure_colname <- 'Measure_Name'

  # Make a symbol of the numerator and denominator columns
  cache$denom_col_sym   <- rlang::sym(cache$denom_colname)
  cache$numer_col_sym   <- rlang::sym(cache$numer_colname)
  cache$time_col_sym    <- rlang::sym(cache$time_colname)
  cache$measure_col_sym <- rlang::sym(cache$measure_colname)

  # Calculate peer average by measure
  #cache$peer_mean <- calc_peer_measure_means(data, cache)
  cache$comparator <- 0.90

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

eval_achievement <- function(x, comp){
  # Last timepoint is at or above comparator and all others are below
  bools <- x >= comp
  return( sum(bools)==1 && dplyr::last(bools) == TRUE)
}

eval_comparator_type <- function(match_iri, spek){
  # measure_id provided by running environment
  c_type <- spekex::comparator_type_of_measure(
    spekex::lookup_measure(measure_id, spek) )
  identical(c_type, match_iri)
}

eval_consec_neg_gap <- function(x, comp){
  if(length(x) < 2){ return(FALSE) }
  bools <- x < comp
  return( sum(bools)==2 )
}

eval_consec_pos_gap <- function(x, comp){
  if(length(x) < 2){ return(FALSE) }
  bools <- x > comp
  return( sum(bools)==2 )
}

########################
# Annotation Functions #
########################

annotate_negative_gap <- function(data, spek){
  time <- cache$time_col_sym
  denom <- cache$denom_col_sym
  numer <- cache$numer_col_sym
  id <- cache$id_col_sym

  data %>%
    dplyr::filter(!!time == max(!!time)) %>%
    mutate(rate = !!numer / !!denom) %>%
    group_by(!!id) %>%
    summarize(negative_gap = rate < cache$comparator)
}

annotate_positive_gap <- function(data, spek){
  time <- cache$time_col_sym
  denom <- cache$denom_col_sym
  numer <- cache$numer_col_sym
  id <- cache$id_col_sym

  data %>%
    dplyr::filter(!!time == max(!!time)) %>%
    mutate(rate = !!numer / !!denom) %>%
    group_by(!!id) %>%
    summarize(positive_gap = (rate > cache$comparator))
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

annotate_performance_gap <- function(data, spek){
  time <- cache$time_col_sym
  denom <- cache$denom_col_sym
  numer <- cache$numer_col_sym
  id <- cache$id_col_sym

  data %>%
    dplyr::filter(!!time == max(!!time)) %>%
    mutate(rate = !!numer / !!denom) %>%
    group_by(!!id) %>%
    summarize(performance_gap = (rate != cache$comparator))
}

annotate_acheivement <- function(data, spek){
  time <- cache$time_col_sym
  denom <- cache$denom_col_sym
  numer <- cache$numer_col_sym
  id <- cache$id_col_sym

  data %>%
    mutate(rate = !!numer / !!denom) %>%
    group_by(!!id) %>%
    summarize( acheivement = eval_achievement(rate,cache$comparator))
}

annotate_consec_neg_gap <- function(data, spek){
  id <- cache$id_col_sym
  data %>% group_by(!!id) %>% summarize( eval_consec_neg_gap = FALSE)
}

annotate_consec_pos_gap <- function(data, spek){
  id <- cache$id_col_sym
  data %>% group_by(!!id) %>% summarize( consec_pos_gap = FALSE)
}

annotate_goal_comparator <- function(data, spek){
  is_type <- eval_comparator_type(spekex::SE$GOAL_COMPARATOR_IRI, spek)
  id <- cache$id_col_sym
  data %>% group_by(!!id) %>% summarize( goal_comparator = is_type)
}

annotate_social_comparator <- function(data, spek){
  is_type <- eval_comparator_type(spekex::SE$SOCIAL_COMPARATOR_IRI, spek)
  id <- cache$id_col_sym
  data %>% group_by(!!id) %>% summarize( social_comparator = is_type)
}

annotate_standard_comparator <- function(data, spek){
  is_type <- eval_comparator_type(spekex::SE$STANDARD_COMPARATOR_IRI, spek)
  id <- cache$id_col_sym
  data %>% group_by(!!id) %>% summarize( standard_comparator = is_type)
}

# No-op Annotations
annotate_capability_barrier <- function(data, spek){
  id <- cache$id_col_sym
  data %>% group_by(!!id) %>% summarize( capability_barrier = FALSE)
}

annotate_large_gap <- function(data, spek){
  id <- cache$id_col_sym
  data %>% group_by(!!id) %>% summarize( large_gap = FALSE)
}
