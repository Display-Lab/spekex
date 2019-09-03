library(dplyr, warn.conflicts = FALSE)
library(utils)


# Setup is run once
setup <- function(data, spec){
  cat(paste("\nSETUP\n", names(data), "\n\n"), file=stderr())
}


# Helper functions used by annotations
eval_negative_gap <- function(doc, ndoc){
  rate <- doc/(ndoc+doc)
  return(dplyr::last(rate) < 0.9)
}

eval_positive_gap <- function(doc, ndoc){
  rate <- doc/(ndoc+doc)
  return(dplyr::last(rate) >= 0.9)
}

eval_positive_trend <- function(doc, ndoc){
  raw_rate <- doc/(ndoc+doc)
  rate <- ifelse(is.nan(raw_rate), 0, raw_rate)

  tail_sorted <- !is.unsorted(utils::tail(rate,3), strictly=T)
  is_tail_ascending <- !is.na(tail_sorted) && tail_sorted
  return(is_tail_ascending)
}

eval_negative_trend <- function(doc, ndoc){
  raw_rate <- doc/(ndoc+doc)
  rate <- ifelse(is.nan(raw_rate), 0, raw_rate)

  tail_rev_sorted <- !is.unsorted(rev(utils::tail(rate,3)), strictly=T)
  is_tail_descending <- !is.na(tail_rev_sorted) && tail_rev_sorted
  return(is_tail_descending)
}

# Annotation functions
annotate_negative_gap <- function(data, spek){
  data %>% group_by(id) %>%
    dplyr::filter(report_month == max(report_month)) %>%
    summarize(negative_gap = eval_negative_gap(documented, not_documented))
}

annotate_positive_gap <- function(data, spek){
  data %>% group_by(id) %>%
    dplyr::filter(report_month == max(report_month)) %>%
    summarize(positive_gap = eval_positive_gap(documented, not_documented))
}

annotate_positive_trend <- function(data, spek){
  data %>%
    group_by(id) %>%
    arrange(report_month) %>%
    summarize(positive_trend = eval_positive_trend(documented, not_documented))
}

annotate_negative_trend <- function(data, spek){
  data %>%
    group_by(id) %>%
    arrange(report_month) %>%
    summarize(negative_trend = eval_negative_trend(documented, not_documented))
}
