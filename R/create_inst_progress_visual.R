create_inst_progress_visual <- function(event_names_list_stu, accessToken, this_email){
  my_green <- "darkgreen"
  another_grey <- "#D3D3D3"
  infoc <- "#f7d782"
  my_black <- "black"
  base_font <- "queensides"
  tryCatch({
    if (!("queensides" %in% systemfonts::system_fonts()$family)) {
      systemfonts::register_font(
        name = "queensides",
        plain = here::here("www/Queensides-3z7Ey.ttf")
      )
    }

  # Status--------------
  num_events <- 3L
  eventz <- sapply(1:num_events, function(x) paste0("Event ", x))
  assigns <- sapply(1:num_events, function(x) paste0("assign", x))
  dones <- sapply(1:num_events, function(x) paste0("done", x))

  stu_enames <- event_names_list_stu
  status <- vector(mode = "character", length = num_events)

  for (i in 1:num_events){
    assi_done <- stu_enames[c(paste0("assign", i), paste0("done", i))]
    if ((assi_done[1] == "no name") & (assi_done[2] == "no name")){
      status[i] <- "Not Assigned"
    } else if ((assi_done[1] != "no name") & (assi_done[2] != "no name")){
      status[i] <- "Complete"
    } else if ((assi_done[1] != "no name") & (assi_done[2] == "no name")){
      status[i] <- "Outstanding"
    }
  }

    # Plot data----------------
    if (is.null(status)){
      dat <- NULL
    } else {
      x_pos <- seq(3, length.out = num_events)
      scaling_factor <- ifelse(num_events > 5, 30, 20)
      y_pos <- seq(0, 1, length.out = num_events) * scaling_factor
      events <- eventz
      # }

      dat <- tibble::tibble(
        x_pos = x_pos,
        y_pos = y_pos,
        events = events,
        Status = status
      )
      dat <- dat |>
        dplyr::mutate(
          imagee = dplyr::case_match(
            Status,
            "Complete" ~ 'checkmark-circle',
            c("Not Assigned") ~ 'ellipse',
            "Outstanding" ~ 'alert-circle'
          ),
          filll = dplyr::case_match(
            Status,
            "Complete" ~ my_green,
            c("Not Assigned") ~ another_grey,
            "Outstanding" ~ infoc
          ),
          colorr = dplyr::case_match(
            Status,
            c("Not Assigned") ~ "white",
            "Complete" ~ my_black,
            "Outstanding" ~ "brown" #"red"
          ),
          text_position = ifelse(dplyr::row_number() %% 2 == 0, -2.5, 3.5)
        )
    }

    # Plot
    if (is.null(dat)) {
      p <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No data yet", size = 5, hjust = 0.5) +
        theme_void() +
        theme(plot.margin = margin(0, 0, 0, 0))

      return(
        list(
          status = status,
          progress_plot = girafe(ggobj = p, width_svg = 5, height_svg = 3)
        )
      )
    } else {

      p <- ggplot(data = dat, aes(x_pos, y_pos)) +
        geom_path(color = "#D3D3D3", linewidth = 1.5) +
        geom_point(data = dat |>
                     dplyr::filter(Status %in% c("Complete", "Outstanding")),

                   size = 10, aes(color = I(colorr))) +
        geom_icon(aes(image = imagee, color = I(filll)), size = 0.08) +
        geom_point_interactive(data = dat |>
                                 dplyr::filter(Status %in% c("Not Assigned", "Outstanding")
                                 ),
                               aes(tooltip = events,
                                   data_id = events),
                               size = 12, alpha = 0) + # Transparent points for interaction
        geom_text(aes(label = events),
                  color = "#041C2C",
                  size = 6,
                  nudge_x = 0.3,
                  nudge_y = 0,
                  family = base_font
        ) +
        theme_void(base_family = base_font) +
        theme(plot.margin = margin(t = 20, unit = "pt"),
              text = element_text(family = base_font)) +
        coord_cartesian(clip = "off")

      list(
        status = status,
        dat = dat,
        progress_plot = girafe(ggobj = p, options = list(
          opts_selection(type = "single", css = "cursor:pointer;")
        ))
      )
    }
  }, error = function(e){
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "Failed to fetch data", size = 5, hjust = 0.5) +
      theme_void() +
      theme(plot.margin = margin(0, 0, 0, 0))
    list(
      status = "Failed to get status",
      dat = "dat",
      progress_plot = girafe(ggobj = p, options = list(
        opts_selection(type = "single", css = "cursor:pointer;")
      ))
    )
  }
  )
}
