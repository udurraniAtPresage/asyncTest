create_survey_list_stu <- function(collection_paths, uid) {
  lapply(collection_paths, function(path) {
    selected_field <- "Instructor"
    list(
      collection_path = path,
      selected_fields = selected_field,
      filter_by = "uid",
      operation = "EQUAL",
      value_type = "stringValue",
      value = uid
    )
  })
}

get_all_event_names_for_a_student <- function(collection_paths_e, uid, accessToken){

  if (is.null(uid) | is.null(accessToken)){
    return(NULL)
  }

  query_list_stu <- create_survey_list_stu(collection_paths_e, uid)
  res <- tryCatch(
    {
      # Try querying in parallel
      frstore::frstore_run_query_parallel(query_list_stu, accessToken)
    },
    error = function(e) {
      # If querying in parallel fails, fall back to querying sequentially
      message("Querying in parallel failed! Trying querying sequentially...")
      frstore::frstore_run_query_sequential(query_list_stu, accessToken)
    }
  )
  if (is.null(res$successful_results) | length(res$successful_results) < 1){
    return(NULL)
  }
  res_list <- purrr::map(res$successful_results, function(day_list) {
    purrr::map(day_list, ~ .x$document |>
                 purrr::keep_at(c("fields")) )
  })

  get_value <- function(x) {
    tryCatch({
      # Navigate to the deepest level and extract stringValue
      value <- x[[1]]$fields[[1]]$stringValue
      if (!is.null(value)) {
        return(value)
      } else {
        return("no name")
      }
    }, error = function(e) {
      return("no name")
    })
  }

  event_namez <- lapply(res_list, get_value)

  stats::setNames(event_namez, collection_paths_e)
}
