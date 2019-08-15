library(dplyr, warn.conflicts = FALSE)
library(utils)


# Setup is run once
setup <- function(data, spec){
  cat(paste("\nSETUP\n", names(data), "\n\n"), file=stderr())
}


# Helper functions used by annotations
eval_negative_gap <- function(doc, ndoc){
  ratio <- doc/ndoc
  return(dplyr::last(ratio) < 0.9)
}


# Annotation functions
annotate_negative_gap <- function(data, spek){
  data %>% group_by(id) %>%
    dplyr::filter(report_month == max(report_month)) %>%
    summarize(negative_gap = eval_negative_gap(documented, not_documented))
}
