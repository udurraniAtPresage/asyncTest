collection_paths_e <- c("assign1", "assign2", "assign3",
                        "done1", "done2", "done3")

mod_plot_promises_ui <- function(id){
  ns <- NS(id)
  tagList(
    girafeOutput(ns("student_progress"))
  )
}


mod_plot_promises_server <- function(id, accessToken, student_email, this_email,
                                                     parent_session
){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    event_names_list_stu <- reactivePoll(
      10000,
      session,
      checkFunc = function() {
        get_all_event_names_for_a_student(collection_paths_e, student_email(), accessToken())
      },
      valueFunc = function() {
        get_all_event_names_for_a_student(collection_paths_e, student_email(), accessToken())
      }
    )

    # Reactive value to store the plot promise
    progress_map_promise <- reactiveVal()

    observeEvent(event_names_list_stu(), {
      create_inst_progress_visual_async(
          event_names_list_stu(),
          accessToken(),
          student_email()
        ) %>%
        then(\(result){
          cli::cat_line("Yeay")
          progress_map_promise(result)
        }) %>%
        catch(\(error){
          cli::cat_line("Ouch")
          progress_map_promise(
                girafe(
                  ggobj = ggplot() +
                    annotate("text", x = 0.5, y = 0.5,
                             label = "Error loading plot",
                             size = 5, hjust = 0.5) +
                    theme_void()
                )
          )
        })
    })

    output$student_progress <- renderGirafe({
      progress_map_promise()
    })
  })
}
