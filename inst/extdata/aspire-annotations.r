library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(utils)
library(rlang)
library(stats)
library(lubridate)

# Annotator functions for MPOG ASPIRE measurements

############
# Run Once #
############

# Setup cache is run once per measure-comparator
#  The top level env will include comparator_id and measure_id
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
  cache$measure_id <- measure_id
  cache$comparator_id <- comparator_id
  cache$comparator <- calc_comparator_value(data, spek, measure_id, comparator_id)

  return(cache)
}

calc_comparator_value <- function(data, spek, m_id, c_id){
  if(is.null(c_id)){
    return(NA)
  }else{
    comparator <- spekex::lookup_comparator(c_id, spek)
  }

  val <- spekex::comparison_value_of_comparator(comparator)
  # Assume comparators without values are social comparator.
  # TODO: Lookup compartor type
  if(is.null(val)){
    # TODO: Do symbol lookup and substitution via quasiquotation
    measure <- spekex::lookup_measure(m_id, spek)
    m_name <- spekex::identifier_of_measure(measure)
    val <- data %>%
      dplyr::filter(Measure_Name == m_name) %>%
      summarize(value=mean(Passed_Count / Denominator)) %>%
      pull(value)
  }

  return(val)
}

####################
# Helper Functions #
####################

# Only pass in time points to be evaluated, as this will check for trend across all points given
eval_positive_trend <- function(x){
  len <- length(x)
  if(len < 2){ return(FALSE) }
  is_tail_ascending <- !is.unsorted(x, strictly=T)
  return(is_tail_ascending)
}

eval_negative_trend <- function(x){
  len <- length(x)
  if(len < 2){ return(FALSE) }
  is_tail_descending <- !is.unsorted(rev(x), strictly=T)
  return(is_tail_descending)
}

# This does not handle a missing most recent time point.  That needs to be accounted for elsewhere.
# Most recent (greatest) timepoint is at or above comparator and all others are below
# All FALSE with single TRUE at end
eval_achievement <- function(x, comp){
  if(length(x) < 2){ return(FALSE)}

  bools <- x >= comp
  return( sum(bools)==1 & dplyr::last(bools) == TRUE)
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

  # DEBUG:Catch bad data
  if(max(data$Month) == -Inf){
    rlang::abort(data)
  }

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
  min_month <- max_month %m-% months(1)

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
  min_month <- max_month %m-% months(1)

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

annotate_achievement <- function(data, spek){
  time <- cache$time_col_sym
  denom <- cache$denom_col_sym
  numer <- cache$numer_col_sym
  id <- cache$id_col_sym

  all_ids_df <- data %>% select(!!id) %>% distinct

  elidgibile_ids <- data %>%
    dplyr::filter(!!time == max(!!time)) %>%
    pull(!!id) %>%
    unique

  data %>%
    dplyr::filter(!!id %in% elidgibile_ids) %>%
    mutate(rate = !!numer / !!denom) %>%
    arrange(!!time) %>%
    group_by(!!id) %>%
    summarize( achievement = eval_achievement(rate,cache$comparator)) %>%
    right_join(all_ids_df)

  id <- cache$id_col_sym
  data %>% group_by(!!id) %>% summarize( achievement = FALSE)
}

annotate_comparators <- function(data, spek){
  # comparator_id provided by running environment into cache
  cache$comparator_id
  comp_type <- spekex::type_of_comparator( spekex::lookup_comparator(cache$comparator_id, spek))

  is_std <- identical(spekex::SE$STANDARD_COMPARATOR_IRI, comp_type)
  is_soc <- identical(spekex::SE$SOCIAL_COMPARATOR_IRI, comp_type)
  is_gol <- identical(spekex::SE$GOAL_COMPARATOR_IRI, comp_type)

  id <- cache$id_col_sym

  # Do three annotations at once.
  data %>% group_by(!!id) %>% summarize(standard_comparator = is_std,
                                        social_comparator = is_soc,
                                        goal_comparator = is_gol)
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

annotate_consec_neg_gap <- function(data, spek){
  id <- cache$id_col_sym
  data %>% group_by(!!id) %>% summarize( consec_neg_gap = FALSE)
}

annotate_consec_pos_gap <- function(data, spek){
  id <- cache$id_col_sym
  data %>% group_by(!!id) %>% summarize( consec_pos_gap = FALSE)
}
