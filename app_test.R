library(shiny)
library(bslib)
library(frbs)
library(frstore)
library(R6)
library(digest)
library(systemfonts)
library(ggplot2)
library(ggiraph)
library(ggimage)
library(readr)
library(crew)

ui <- page_fluid(
  selectizeInput("user_select_map", "Student ID:",
                 choices = c("stu1", "stu2"),
                 options = list(
                   onInitialize = I('function() { this.setValue(""); }')
                 )),
  uiOutput("plot")
)

server <- function(input, output, session) {
  PROJECT_NAME <- reactive(frstore_project_id())
  accessToken <- reactive({
    admin_account <- frbs_sign_in(Sys.getenv("ADMIN_EMAIL"), Sys.getenv("PASS"))
    admin_account$idToken
  })


  output$plot <- renderUI({
    tagList(
      div(
        id = paste0("stu_idmap"),
        tags$h4(input$user_select_map),
        mod_plot_ui("stu_progress")
      )
    )
  })

  mod_plot_server(
    "stu_progress",
    accessToken,
    reactive(input$user_select_map),
    this_email,
    session
  )
}

shinyApp(ui, server)
