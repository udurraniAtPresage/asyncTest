PlotCache <- R6::R6Class(
  "PlotCache",

  public = list(
    initialize = function() {
      private$cache <- new.env(parent = emptyenv())
      private$last_update_time <- new.env(parent = emptyenv())
    },

    get_cached_plot = function(student_id, current_data_hash) {
      if (!private$has_valid_cache(student_id, current_data_hash)) {
        return(NULL)
      }
      return(private$cache[[student_id]])
    },

    update_cache = function(student_id, progress_plot, data_hash) {
      private$cache[[student_id]] <- progress_plot
      private$last_update_time[[student_id]] <- data_hash
      invisible(NULL)
    }
  ),

  private = list(
    cache = NULL,
    last_update_time = NULL,

    has_valid_cache = function(student_id, current_data_hash) {
      # Check if we have a cache for this student
      if (!exists(student_id, envir = private$cache)) {
        return(FALSE)
      }

      # Check if the data hash matches
      if (!exists(student_id, envir = private$last_update_time) ||
          private$last_update_time[[student_id]] != current_data_hash) {
        return(FALSE)
      }

      return(TRUE)
    }
  )
)
