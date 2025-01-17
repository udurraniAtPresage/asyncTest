collection_paths_e <- c("assign1", "assign2", "assign3",
                        "done1", "done2", "done3")

mod_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    textOutput(ns("status")),
    girafeOutput(ns("student_progress"))
  )
}


mod_plot_server <- function(id, accessToken, uid, this_email,
                                             parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize cache manager
    cache_manager <- PlotCache$new()

    event_names_list_stu <- reactivePoll(
      10000,
      session,
      checkFunc = function() {
        get_all_event_names_for_a_student(collection_paths_e, uid(), accessToken())
      },
      valueFunc = function() {
        get_all_event_names_for_a_student(collection_paths_e, uid(), accessToken())
      }
    )

    # Create a hash of the current data
    current_data_hash <- reactive({
      req(event_names_list_stu())
      digest::digest(event_names_list_stu())
    })

    status_message <- function(n) {
      if (n > 0) {
        paste(format(Sys.time()), "tasks in progress ⏳ :", n)
      } else {
        paste(format(Sys.time()), "All tasks completed 🚀")
      }
    }

    reactive_result <- reactiveVal()
    reactive_status <- reactiveVal("No task submitted yet")
    status <- reactiveVal()
    dat <- reactiveVal()
    reactive_poll <- reactiveVal(FALSE)
    progress_cache <- reactiveValues()

    output$student_progress <- renderGirafe({
      req(reactive_result())
      reactive_result()
    })

    output$status <- renderText(reactive_status())



    # CREW Part ---------------------------------------------------------------
    curr_controller <- NULL
    controller <- reactive({
      req(uid())
      if (!is.null(curr_controller)) curr_controller$terminate()
      curr_controller <<- crew_controller_local(workers = 1, seconds_idle = 10)
      curr_controller$start()
      curr_controller
    })

    onStop(function() if (!is.null(curr_controller)) curr_controller$terminate())

    observeEvent(event_names_list_stu(), {
      req(uid())
      student_id <- uid()
      data_hash <- current_data_hash()

      # Check cache first
      cached_result <- cache_manager$get_cached_plot(student_id, data_hash)

      if (!is.null(cached_result)) {
        # Use cached result
        reactive_result(cached_result)
        reactive_status(paste(format(Sys.time()), "Retrieved from cache 📋"))
      } else {
        # Create new progress plot
        reactive_result(NULL)
        controller()$push(
          command = create_inst_progress_visual(event_names_list_stu, accessToken, uid),
          data = list(
            create_inst_progress_visual = create_inst_progress_visual,
            event_names_list_stu = event_names_list_stu(),
            accessToken = accessToken(),
            uid = uid()
          ),
          packages = c("ggplot2", "ggiraph", "dplyr", "httr2", "frstore", "ggimage", "here")
        )
        reactive_poll(TRUE)
      }
    })

    # Modified observe block to update cache
    observe({
      req(reactive_poll())
      invalidateLater(millis = 100)
      task <- controller()$pop()

      if (!is.null(task) && "result" %in% names(task)) {
        if (!is.null(task$result) && "progress_plot" %in% names(task$result[[1]])) {
          progress_map <- (task$result)[[1]]$progress_plot
          statuz <- (task$result)[[1]]$status
          datz <- (task$result)[[1]]$dat

          # Update cache with new result
          cache_manager$update_cache(
            uid(),
            progress_map,
            current_data_hash()
          )

          reactive_result(progress_map)
          status(statuz)
          dat(datz)
        }
      }

      if (sum(controller()$client$summary()$assigned) > 0) {
        reactive_status(status_message(n = sum(controller()$client$summary()$assigned) -
                                         sum(controller()$client$summary()$complete)))
        reactive_poll(controller()$nonempty())
      } else {
        reactive_status(paste(format(Sys.time()), "launching necessary workers..."))
      }
    })

    # The rest of your existing event handlers and reactive expressions
    observeEvent(input$student_progress_selected, {
      req(status())
      selected_event <- input$student_progress_selected

      if (grepl("Event", selected_event) & status()[readr::parse_number(selected_event)] %in% c("Not Assigned", "Outstanding")) {
        event_num <- readr::parse_number(selected_event)
        showModal(
          modalDialog(
            title = paste0("Event ", event_num),
            "Information"
          )
        )
      }
    }, ignoreInit = TRUE)


  })
}
