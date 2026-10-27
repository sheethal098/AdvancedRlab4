
#' Visualize average flight delays by airport
#'
#' Calculates average departure delays for New York City
#' origin airports and displays their geographic locations.
#'
#' @return A ggplot object showing average departure delays.
#' @export
visualize_airport_delays <- function() {

  # Remove flights with missing departure delays
  flight_data <- dplyr::filter(
    nycflights13::flights,
    !is.na(.data$dep_delay)
  )

  # Calculate average departure delay for each airport
  airport_delays <- dplyr::summarise(
    dplyr::group_by(flight_data, .data$origin),
    avg_delay = mean(.data$dep_delay),
    .groups = "drop"
  )

  # Add airport location information
  airport_delays <- dplyr::left_join(
    airport_delays,
    nycflights13::airports,
    by = c("origin" = "faa")
  )

  # Keep airports with valid geographic coordinates
  airport_delays <- dplyr::filter(
    airport_delays,
    is.finite(.data$lon),
    is.finite(.data$lat)
  )

  # Plot average delays with airport labels
  ggplot2::ggplot(
    airport_delays,
    ggplot2::aes(
      x = .data$lon,
      y = .data$lat,
      color = .data$avg_delay
    )
  ) +
    ggplot2::geom_point(size = 5, alpha = 0.9) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$origin),
      nudge_x = 0.025,
      nudge_y = 0.002,
      size = 4,
      fontface = "bold",
      show.legend = FALSE
    ) +
    ggplot2::scale_color_gradient2(
      low = "#2C7BB6",
      mid = "#FFFFBF",
      high = "#D7191C",
      midpoint = mean(airport_delays$avg_delay, na.rm = TRUE),
      name = "Avg. departure\ndelay (min)"
    ) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      title = "Average Departure Delays by NYC Airport",
      subtitle = "Based on flights in the nycflights13 dataset",
      x = "Longitude",
      y = "Latitude",
      caption = "Each point represents an origin airport."
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 16),
      plot.subtitle = ggplot2::element_text(color = "grey40"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "right"
    )
}


