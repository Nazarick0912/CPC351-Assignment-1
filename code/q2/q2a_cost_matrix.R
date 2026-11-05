# CPC351 Assignment 01 - Question 2(a)
# Build the Rainy Morning travel-time cost matrix.

# Default delivery point coordinates in the order P1 to P10.
q2_delivery_points <- function() {
  data.frame(
    name = paste0("P", 1:10),
    x = c(60, 180, 80, 140, 20, 100, 200, 140, 40, 100),
    y = c(200, 200, 180, 180, 160, 160, 160, 140, 120, 120),
    stringsAsFactors = FALSE
  )
}

# Constructs the 10 x 10 cost matrix and penalty metadata from a travel log CSV.
# Returns a named list containing:
#   $cost_matrix   : 10 x 10 numeric matrix with matching P1..P10 row and column labels
#   $penalty_matrix: 10 x 10 logical matrix (TRUE where route has no valid history)
#   $penalty_value : numeric penalty value assigned to impassable routes
#   $maximum_observed_cost : largest finite cost observed across all valid routes
#   $filtered_logs : data frame of filtered Rainy Morning records
#   $stats         : list of cleaning and filtering summary counts
build_q2a_cost_matrix <- function(input_path, points = NULL) {
  if (missing(input_path) || !is.character(input_path) ||
      length(input_path) != 1L || !nzchar(trimws(input_path))) {
    stop("input_path must be a single non-empty character string.", call. = FALSE)
  }
  if (!file.exists(input_path)) {
    stop("Travel log CSV not found: ", input_path, call. = FALSE)
  }

  if (is.null(points)) {
    points <- q2_delivery_points()
  }

  logs <- utils::read.csv(input_path, check.names = FALSE, stringsAsFactors = FALSE)

  # Keep only Rainy Morning records. Change missing weather/time conditions to FALSE.
  rainy_morning_rows <- logs$Weather == "Rainy" & logs$Time_of_Day == "Morning"
  rainy_morning_rows[is.na(rainy_morning_rows)] <- FALSE
  rainy_logs <- logs[rainy_morning_rows, ]
  rainy_morning_row_count <- nrow(rainy_logs)

  n_points <- nrow(points)
  point_names <- if (!is.null(points$name)) points$name else paste0("P", seq_len(n_points))

  # Start an empty matrix with matching P1..P10 row/column labels.
  # Rows are origins; columns are destinations.
  cost_matrix <- matrix(
    NA_real_,
    nrow = n_points,
    ncol = n_points,
    dimnames = list(point_names, point_names)
  )

  used_record_count <- 0L
  observed_route_count <- 0L
  outside_point_count <- 0L
  valid_point_rows <- rep(FALSE, nrow(rainy_logs))

  # Keep only records whose origin and destination are among the ten points.
  for (record in seq_len(nrow(rainy_logs))) {
    origin_found <- FALSE
    destination_found <- FALSE

    for (point in seq_len(n_points)) {
      if (!is.na(rainy_logs$Origin_X[record]) &&
          !is.na(rainy_logs$Origin_Y[record]) &&
          rainy_logs$Origin_X[record] == points$x[point] &&
          rainy_logs$Origin_Y[record] == points$y[point]) {
        origin_found <- TRUE
      }

      if (!is.na(rainy_logs$Dest_X[record]) &&
          !is.na(rainy_logs$Dest_Y[record]) &&
          rainy_logs$Dest_X[record] == points$x[point] &&
          rainy_logs$Dest_Y[record] == points$y[point]) {
        destination_found <- TRUE
      }
    }

    if (origin_found && destination_found) {
      valid_point_rows[record] <- TRUE
    } else {
      outside_point_count <- outside_point_count + 1L
    }
  }

  rainy_logs <- rainy_logs[valid_point_rows, ]

  travel_times <- rainy_logs$Travel_Time_Minutes
  valid_time_rows <- !is.na(travel_times) & travel_times >= 0
  filtered_logs <- rainy_logs[valid_time_rows, ]

  # Calculate the 75th percentile for every directed origin-destination route.
  for (origin in seq_len(n_points)) {
    for (destination in seq_len(n_points)) {
      if (origin != destination) {
        route_rows <- filtered_logs$Origin_X == points$x[origin] &
          filtered_logs$Origin_Y == points$y[origin] &
          filtered_logs$Dest_X == points$x[destination] &
          filtered_logs$Dest_Y == points$y[destination]

        # Missing route matches are excluded.
        route_rows[is.na(route_rows)] <- FALSE

        route_times <- filtered_logs$Travel_Time_Minutes[route_rows]
        used_record_count <- used_record_count + length(route_times)

        if (length(route_times) > 0L) {
          cost_matrix[origin, destination] <- stats::quantile(route_times, 0.75)
          observed_route_count <- observed_route_count + 1L
        }
      } else {
        # Travelling from a point to itself has zero cost.
        cost_matrix[origin, destination] <- 0
      }
    }
  }

  # Construct penalty_matrix directly from conditions that leave a route with
  # no valid travel history (cells remaining NA), setting diagonal to FALSE.
  penalty_matrix <- is.na(cost_matrix)
  diag(penalty_matrix) <- FALSE
  dimnames(penalty_matrix) <- list(point_names, point_names)

  # Calculate severe penalty: 11 * M + 1 minute (M = max observed valid cost).
  maximum_observed_cost <- max(cost_matrix, na.rm = TRUE)
  impassable_penalty <- maximum_observed_cost * 11 + 1

  # Assign the high penalty to routes with no valid history.
  cost_matrix[penalty_matrix] <- impassable_penalty
  attr(cost_matrix, "penalty_matrix") <- penalty_matrix

  penalty_route_count <- sum(penalty_matrix)

  stats <- list(
    rows_in_file = nrow(logs),
    rainy_morning_rows_before_coord = rainy_morning_row_count,
    outside_point_rows = outside_point_count,
    rainy_morning_rows_after_coord = nrow(rainy_logs),
    missing_travel_times = sum(is.na(travel_times)),
    closure_records_minus_one = sum(travel_times == -1, na.rm = TRUE),
    other_negative_travel_times = sum(travel_times < 0 & travel_times != -1, na.rm = TRUE),
    valid_travel_records_used = used_record_count,
    observed_directed_routes = observed_route_count,
    penalized_route_count = penalty_route_count,
    maximum_observed_cost = maximum_observed_cost,
    impassable_penalty = impassable_penalty
  )

  list(
    cost_matrix = cost_matrix,
    penalty_matrix = penalty_matrix,
    penalty_value = impassable_penalty,
    maximum_observed_cost = maximum_observed_cost,
    filtered_logs = filtered_logs,
    stats = stats,
    input_path = input_path,
    point_names = point_names
  )
}

# Exports Q2(a) filtered log, matrix CSV, and heatmap plot to specified directories.
export_q2a_artifacts <- function(q2a_result,
                                 output_dir = file.path("outputs", "q2"),
                                 processed_dir = file.path("data", "processed", "q2"),
                                 write_filtered_log = TRUE,
                                 write_matrix_csv = TRUE,
                                 write_heatmap = TRUE) {
  if (isTRUE(write_filtered_log) && !is.null(processed_dir)) {
    dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(
      q2a_result$filtered_logs,
      file.path(processed_dir, "ahmad_travel_logs_filtered.csv"),
      row.names = FALSE
    )
  }

  if ((isTRUE(write_matrix_csv) || isTRUE(write_heatmap)) && !is.null(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  if (isTRUE(write_matrix_csv) && !is.null(output_dir)) {
    cost_table <- data.frame(
      Origin = q2a_result$point_names,
      as.data.frame(q2a_result$cost_matrix, check.names = FALSE),
      stringsAsFactors = FALSE
    )
    utils::write.table(
      cost_table,
      file.path(output_dir, "q2a_cost_matrix.csv"),
      sep = ",",
      row.names = FALSE
    )
  }

  if (isTRUE(write_heatmap) && !is.null(output_dir)) {
    grDevices::png(
      file.path(output_dir, "q2a_cost_matrix_heatmap.png"),
      width = 900,
      height = 800
    )
    cost_matrix <- q2a_result$cost_matrix
    impassable_penalty <- q2a_result$penalty_value
    point_names <- q2a_result$point_names

    observed_costs <- cost_matrix[cost_matrix != impassable_penalty]
    highest_observed_cost <- max(observed_costs, na.rm = TRUE)
    if (highest_observed_cost <= 0) highest_observed_cost <- 1

    travel_time_colors <- grDevices::colorRampPalette(
      c("#fff8df", "#f2dda9", "#e7ad8c", "#cf8888")
    )(100)

    graphics::image(
      1:10, 1:10, cost_matrix,
      col = travel_time_colors,
      zlim = c(0, highest_observed_cost),
      axes = FALSE,
      xlab = "Origin point",
      ylab = "Destination point",
      main = "Synthetic Rainy Morning: 75th-percentile travel time (minutes)"
    )
    graphics::axis(1, at = 1:10, labels = point_names, las = 2, cex.axis = 0.8)
    graphics::axis(2, at = 1:10, labels = point_names, las = 1, cex.axis = 0.8)
    graphics::box()
    graphics::title(sub = "Blue = same-point zero; P = penalty; redder = higher travel time")

    for (origin in 1:10) {
      for (destination in 1:10) {
        if (origin == destination) {
          graphics::rect(
            origin - 0.5, destination - 0.5, origin + 0.5, destination + 0.5,
            col = "#77a6b5", border = NA
          )
          graphics::text(origin, destination, "0", col = "black", font = 2)
        } else if (cost_matrix[origin, destination] == impassable_penalty) {
          graphics::rect(
            origin - 0.5, destination - 0.5, origin + 0.5, destination + 0.5,
            col = "#8b78a1", border = NA
          )
          graphics::text(origin, destination, "P", col = "white", font = 2)
        } else {
          graphics::text(origin, destination, round(cost_matrix[origin, destination], 1))
        }
      }
    }
    grDevices::dev.off()
  }

  invisible(q2a_result)
}

# Standalone CLI execution entry point for Question 2(a).
run_q2a_cli <- function(arguments = commandArgs(trailingOnly = TRUE)) {
  if (length(arguments) > 1L) {
    stop("Usage: Rscript code/q2/q2a_cost_matrix.R [travel_csv]", call. = FALSE)
  }

  raw_input_path <- file.path("data", "raw", "q2", "ahmad_travel_logs.csv")
  synthetic_input_path <- file.path("data", "synthetic", "q2", "ahmad_travel_logs.csv")

  if (length(arguments) == 1L) {
    input_path <- path.expand(arguments[[1L]])
  } else if (file.exists(raw_input_path)) {
    input_path <- raw_input_path
  } else {
    input_path <- synthetic_input_path
  }

  if (!file.exists(input_path)) {
    stop("Travel log CSV not found: ", input_path, call. = FALSE)
  }
  if (identical(input_path, synthetic_input_path)) {
    message("Q2(a): using synthetic development data from ", input_path)
  } else {
    message("Q2(a): reading travel logs from ", input_path)
  }

  result <- build_q2a_cost_matrix(input_path)

  st <- result$stats
  print(paste("Rows in file:", st$rows_in_file))
  print(paste("Rainy Morning rows before coordinate check:", st$rainy_morning_rows_before_coord))
  print(paste("Rows removed for outside points:", st$outside_point_rows))
  print(paste("Rainy Morning rows after coordinate check:", st$rainy_morning_rows_after_coord))
  print(paste("Missing travel times:", st$missing_travel_times))
  print(paste("Road-closure records (-1):", st$closure_records_minus_one))
  print(paste("Other negative travel times:", st$other_negative_travel_times))
  print(paste("Valid travel records used:", st$valid_travel_records_used))
  print(paste("Observed directed routes:", st$observed_directed_routes))
  print(paste("Routes given the high penalty:", st$penalized_route_count))
  print(paste("High penalty in minutes:", st$impassable_penalty))

  print("Cost matrix: rows are origins, columns are destinations; order is P1 to P10")
  unlabelled_view <- result$cost_matrix
  dimnames(unlabelled_view) <- NULL
  attr(unlabelled_view, "penalty_matrix") <- NULL
  print(unlabelled_view)

  export_q2a_artifacts(result)
  invisible(result)
}

# Only execute CLI workflow when run directly as a script.
if (sys.nframe() == 0L) {
  run_q2a_cli()
}
