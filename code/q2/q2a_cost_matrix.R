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

required_travel_columns <- c(
  "Origin_X", "Origin_Y", "Dest_X", "Dest_Y",
  "Weather", "Time_of_Day", "Travel_Time_Minutes"
)

# Constructs the 10 x 10 cost matrix and penalty metadata from a travel log CSV.
#
# Arguments:
#   input_path       : Path to the travel log CSV (mandatory).
#   points           : Delivery points table; defaults to q2_delivery_points().
#   fallback_penalty : Penalty value used when no usable history exists across
#                      the entire dataset (default: 99999).
#
# Returns a named list containing:
#   $cost_matrix           : 10 x 10 numeric matrix with matching P1..P10 labels
#   $penalty_matrix        : 10 x 10 logical matrix (TRUE where route has no usable history)
#   $penalty_value         : numeric penalty value assigned to impassable/unusable routes
#   $maximum_observed_cost : largest finite 75th-percentile cost from usable routes (or NA)
#   $has_usable_history    : logical; FALSE if entire dataset has zero usable travel times
#   $route_status_matrix   : 10 x 10 character matrix describing evidence for each route
#   $route_metadata        : data frame with route-by-route counts and status breakdown
#   $filtered_logs         : data frame of usable cleaned records under Rainy Morning
#   $stats                 : list of cleaning and filtering counts
build_q2a_cost_matrix <- function(input_path,
                                  points = NULL,
                                  fallback_penalty = 99999) {
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

  # 1. Validate required columns and check for duplicates
  missing_columns <- setdiff(required_travel_columns, names(logs))
  if (length(missing_columns) > 0L) {
    stop("Q2 CSV is missing required columns: ",
         paste(missing_columns, collapse = ", "), call. = FALSE)
  }
  if (anyDuplicated(names(logs)) > 0L) {
    stop("Q2 CSV contains duplicate column names.", call. = FALSE)
  }

  # 2. Filter strictly for Rainy Morning records
  # Missing weather or time conditions are safely treated as FALSE
  rainy_morning_rows <- logs$Weather == "Rainy" & logs$Time_of_Day == "Morning"
  rainy_morning_rows[is.na(rainy_morning_rows)] <- FALSE
  rainy_logs <- logs[rainy_morning_rows, , drop = FALSE]
  rainy_morning_row_count <- nrow(rainy_logs)

  n_points <- nrow(points)
  point_names <- if (!is.null(points$name)) points$name else paste0("P", seq_len(n_points))

  # 3. Validate coordinates against supplied delivery points
  outside_point_count <- 0L
  valid_point_rows <- rep(FALSE, nrow(rainy_logs))

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

  rainy_logs <- rainy_logs[valid_point_rows, , drop = FALSE]

  # 4. Parse and classify travel times explicitly
  # Distinguish:
  #   - is_missing            : NA or empty string
  #   - is_closure_minus_one  : numeric -1 or string "-1"
  #   - is_malformed          : non-empty text that fails numeric coercion (e.g. "error")
  #   - is_non_finite         : NaN, Inf, -Inf
  #   - is_other_negative     : finite negative values other than -1
  #   - is_zero               : travel time exactly 0 (unusable for distinct delivery points)
  #   - is_usable             : finite positive travel times (> 0)
  raw_tt <- rainy_logs$Travel_Time_Minutes
  raw_as_char <- trimws(as.character(raw_tt))
  parsed_tt <- suppressWarnings(as.numeric(raw_as_char))

  is_missing <- is.na(raw_tt) | raw_as_char %in% c("", "NA")
  is_closure_minus_one <- !is_missing & (parsed_tt == -1 | raw_as_char == "-1")
  is_closure_minus_one[is.na(is_closure_minus_one)] <- FALSE

  is_malformed <- !is_missing & is.na(parsed_tt)
  is_non_finite <- !is_missing & !is_malformed & (!is.finite(parsed_tt) | is.nan(parsed_tt))
  is_other_negative <- !is_missing & !is_malformed & is.finite(parsed_tt) &
                       parsed_tt < 0 & parsed_tt != -1
  is_zero <- !is_missing & !is_malformed & is.finite(parsed_tt) & parsed_tt == 0
  is_usable <- !is_missing & !is_malformed & is.finite(parsed_tt) & parsed_tt > 0

  # Cleaned logs for aggregation contain only usable positive finite records
  filtered_logs <- rainy_logs[is_usable, , drop = FALSE]
  if (nrow(filtered_logs) > 0L) {
    filtered_logs$Travel_Time_Minutes <- parsed_tt[is_usable]
  }

  # 5. Route-by-route aggregation, quantile calculation, and status tracking
  cost_matrix <- matrix(
    NA_real_,
    nrow = n_points,
    ncol = n_points,
    dimnames = list(point_names, point_names)
  )

  route_status_matrix <- matrix(
    "no_observations",
    nrow = n_points,
    ncol = n_points,
    dimnames = list(point_names, point_names)
  )

  used_record_count <- 0L
  observed_route_count <- 0L

  meta_origin <- character(n_points * n_points)
  meta_dest <- character(n_points * n_points)
  meta_status <- character(n_points * n_points)
  meta_usable_n <- integer(n_points * n_points)
  meta_closure_n <- integer(n_points * n_points)
  meta_other_invalid_n <- integer(n_points * n_points)
  meta_cost <- numeric(n_points * n_points)
  idx <- 1L

  for (origin in seq_len(n_points)) {
    for (destination in seq_len(n_points)) {
      meta_origin[[idx]] <- point_names[[origin]]
      meta_dest[[idx]] <- point_names[[destination]]

      if (origin == destination) {
        cost_matrix[origin, destination] <- 0
        route_status_matrix[origin, destination] <- "self_loop"
        meta_status[[idx]] <- "self_loop"
        meta_cost[[idx]] <- 0
      } else {
        # Check all Rainy Morning observations for this directed coordinate pair
        pair_mask <- rainy_logs$Origin_X == points$x[origin] &
                     rainy_logs$Origin_Y == points$y[origin] &
                     rainy_logs$Dest_X == points$x[destination] &
                     rainy_logs$Dest_Y == points$y[destination]
        pair_mask[is.na(pair_mask)] <- FALSE

        n_pair_obs <- sum(pair_mask)
        n_pair_usable <- sum(pair_mask & is_usable)
        n_pair_closure <- sum(pair_mask & (is_missing | is_closure_minus_one))
        n_pair_other_inv <- sum(pair_mask & (is_zero | is_other_negative |
                                             is_non_finite | is_malformed))

        meta_usable_n[[idx]] <- n_pair_usable
        meta_closure_n[[idx]] <- n_pair_closure
        meta_other_invalid_n[[idx]] <- n_pair_other_inv

        if (n_pair_usable > 0L) {
          # Route has usable history: calculate 75th percentile (Type 7)
          usable_times <- parsed_tt[pair_mask & is_usable]
          used_record_count <- used_record_count + length(usable_times)
          p75 <- stats::quantile(usable_times, probs = 0.75, type = 7, names = FALSE)
          cost_matrix[origin, destination] <- p75
          route_status_matrix[origin, destination] <- "usable_history"
          meta_status[[idx]] <- "usable_history"
          meta_cost[[idx]] <- p75
          observed_route_count <- observed_route_count + 1L
        } else if (n_pair_obs == 0L) {
          route_status_matrix[origin, destination] <- "no_observations"
          meta_status[[idx]] <- "no_observations"
          meta_cost[[idx]] <- NA_real_
        } else if (n_pair_closure > 0L && n_pair_other_inv == 0L) {
          # All records are pure closures (NA or -1)
          route_status_matrix[origin, destination] <- "closure_only"
          meta_status[[idx]] <- "closure_only"
          meta_cost[[idx]] <- NA_real_
        } else {
          # Other invalid records present (or mixed closure + other invalid)
          route_status_matrix[origin, destination] <- "other_invalid"
          meta_status[[idx]] <- "other_invalid"
          meta_cost[[idx]] <- NA_real_
        }
      }
      idx <- idx + 1L
    }
  }

  route_metadata <- data.frame(
    origin = meta_origin,
    destination = meta_dest,
    status = meta_status,
    usable_count = meta_usable_n,
    closure_count = meta_closure_n,
    other_invalid_count = meta_other_invalid_n,
    cost_minutes = meta_cost,
    is_penalized = is.na(meta_cost) & meta_origin != meta_dest,
    stringsAsFactors = FALSE
  )

  # 6. Penalty matrix construction and severe penalty calculation
  penalty_matrix <- is.na(cost_matrix)
  diag(penalty_matrix) <- FALSE
  dimnames(penalty_matrix) <- list(point_names, point_names)

  usable_off_diag <- cost_matrix[row(cost_matrix) != col(cost_matrix) & !is.na(cost_matrix)]

  if (length(usable_off_diag) > 0L) {
    maximum_observed_cost <- max(usable_off_diag)
    impassable_penalty <- maximum_observed_cost * 11 + 1

    if (is.infinite(impassable_penalty)) {
      stop("Maximum observed cost is too large; penalty calculation (11 * M + 1) overflows to Inf.",
           call. = FALSE)
    }
    has_usable_history <- TRUE
  } else {
    maximum_observed_cost <- NA_real_
    impassable_penalty <- as.numeric(fallback_penalty)

    if (!is.finite(impassable_penalty) || impassable_penalty <= 0) {
      stop("fallback_penalty must be a positive finite number.", call. = FALSE)
    }

    has_usable_history <- FALSE
    warning(
      "Q2(a): No usable travel times found under Rainy Morning conditions across any route. ",
      "Applying fallback penalty (", impassable_penalty, "). ",
      "Matrix contains only artificial penalties.",
      call. = FALSE
    )
  }

  # Assign the high penalty to all routes without usable history
  cost_matrix[penalty_matrix] <- impassable_penalty
  attr(cost_matrix, "penalty_matrix") <- penalty_matrix
  attr(cost_matrix, "has_usable_history") <- has_usable_history

  # Update route_metadata cost for penalized routes
  route_metadata$cost_minutes[route_metadata$is_penalized] <- impassable_penalty

  # 7. Summary statistics
  stats <- list(
    rows_in_file = nrow(logs),
    rainy_morning_rows_before_coord = rainy_morning_row_count,
    outside_point_rows = outside_point_count,
    rainy_morning_rows_after_coord = nrow(rainy_logs),
    missing_travel_times = sum(is_missing),
    closure_records_minus_one = sum(is_closure_minus_one),
    zero_duration_records = sum(is_zero),
    other_negative_travel_times = sum(is_other_negative),
    non_finite_travel_times = sum(is_non_finite),
    malformed_travel_times = sum(is_malformed),
    total_invalid_travel_times = sum(!is_usable),
    valid_travel_records_used = nrow(filtered_logs),
    observed_directed_routes = observed_route_count,
    penalized_route_count = sum(penalty_matrix),
    maximum_observed_cost = maximum_observed_cost,
    impassable_penalty = impassable_penalty,
    has_usable_history = has_usable_history
  )

  list(
    cost_matrix = cost_matrix,
    penalty_matrix = penalty_matrix,
    penalty_value = impassable_penalty,
    maximum_observed_cost = maximum_observed_cost,
    has_usable_history = has_usable_history,
    route_status_matrix = route_status_matrix,
    route_metadata = route_metadata,
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
                                 prefix = "q2a",
                                 write_filtered_log = TRUE,
                                 write_matrix_csv = TRUE,
                                 write_heatmap = TRUE) {
  filtered_log_filename <- if (prefix == "q2a") {
    "ahmad_travel_logs_filtered.csv"
  } else {
    paste0(prefix, "_travel_logs_filtered.csv")
  }

  if (isTRUE(write_filtered_log) && !is.null(processed_dir)) {
    dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(
      q2a_result$filtered_logs,
      file.path(processed_dir, filtered_log_filename),
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
      file.path(output_dir, paste0(prefix, "_cost_matrix.csv")),
      sep = ",",
      row.names = FALSE
    )
  }

  if (isTRUE(write_heatmap) && !is.null(output_dir)) {
    grDevices::png(
      file.path(output_dir, paste0(prefix, "_cost_matrix_heatmap.png")),
      width = 900,
      height = 800
    )
    cost_matrix <- q2a_result$cost_matrix
    impassable_penalty <- q2a_result$penalty_value
    point_names <- q2a_result$point_names

    observed_costs <- cost_matrix[cost_matrix != impassable_penalty]
    highest_observed_cost <- if (length(observed_costs) > 0L) {
      max(observed_costs, na.rm = TRUE)
    } else {
      1
    }
    if (is.na(highest_observed_cost) || highest_observed_cost <= 0) {
      highest_observed_cost <- 1
    }

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

  if (length(arguments) == 1L) {
    input_path <- path.expand(arguments[[1L]])
    if (!file.exists(input_path)) {
      stop("Travel log CSV not found: ", input_path, call. = FALSE)
    }
  } else if (file.exists(raw_input_path)) {
    input_path <- raw_input_path
  } else {
    message("Q2(a): default travel CSV not found at ", raw_input_path)
    message("Q2(a): skipped. Supply the grading dataset or pass synthetic data explicitly, e.g.:")
    message("  Rscript code/q2/q2a_cost_matrix.R \"data/synthetic/q2/ahmad_travel_logs.csv\"")
    return(invisible(list(status = "missing_input", path = raw_input_path)))
  }

  is_synthetic <- grepl("synthetic", tolower(gsub("\\\\", "/", input_path)))
  if (is_synthetic) {
    message("Q2(a): EXPLICIT SYNTHETIC DEVELOPMENT INPUT: ", input_path)
    message("Q2(a): (Results are from synthetic development data, NOT unseen grading data)")
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
