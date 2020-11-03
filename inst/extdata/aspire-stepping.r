# The following is the stub to add to aspire annotations
# IMPLEMENT STEPPING BEHAVIOR

stepping_helper <- function(n, data, spek){
  slowmo_base <- "http://example.com/slowmo#"
  step_back_data <- data %>% dplyr::filter(Month <= (max(Month) %m-% months(n)))

  results <- list()

  # Handle case where measure or data doesn't go back n months
  if(nrow(step_back_data) == 0){
    return(list(data.frame(id=character(0), stringsAsFactors = F)))
  }else{
    results[['pg_res']] <- annotate_positive_gap(step_back_data, spek) %>%
      rename_at(vars(-id), ~ paste0(slowmo_base, . , "_",n))
    results[['ng_res']] <- annotate_negative_gap(step_back_data, spek) %>%
      rename_at(vars(-id), ~ paste0(slowmo_base, . , "_",n))
    results[['pt_res']] <- annotate_positive_trend(step_back_data, spek) %>%
      rename_at(vars(-id), ~ paste0(slowmo_base, . , "_",n))
    results[['nt_res']] <- annotate_negative_trend(step_back_data, spek) %>%
      rename_at(vars(-id), ~ paste0(slowmo_base, . , "_",n))
    results[['achiev']] <- annotate_achievement(step_back_data, spek) %>%
      rename_at(vars(-id), ~ paste0(slowmo_base, . , "_",n))
  }
  return(results)
}

# Stepping annotations back one month at a time.
#  Some super bespoke shit here.
annotate_stepping <- function(data, spek){
  # Drop max month five times and run all annotations
  #  merge the results together.
  #  ship them off as if they were a single annotation set

  # For each month, filter data. Step back through 5 prior months.
  step_results <- unlist( lapply(c(1:5), stepping_helper, data=data, spek=spek), recursive=F)
  # Combine all results by joining them on id.
  joined_results <- Reduce(function(x,y){left_join(x,y,by="id")}, step_results) %>%
    mutate_all( ~ replace(., is.na(.), FALSE))

  return(joined_results)
}
