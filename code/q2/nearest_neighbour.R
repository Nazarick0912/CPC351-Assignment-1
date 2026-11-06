# Q2(b): Nearest Neighbour routing on a directed 10 x 10 cost matrix.
# Consumes the cost matrix (and optional logical penalty matrix) from Q2(a).

expected_q2_points <- paste0("P", 1:10)

q2_delivery_coordinates <- function() {
  data.frame(
    point = expected_q2_points,
    x = c(60, 180, 80, 140, 20, 100, 200, 140, 40, 100),
    y = c(200, 200, 180, 180, 160, 160, 160, 140, 120, 120),
    stringsAsFactors = FALSE
  )
}

# Returns point labels sorted in deterministic canonical order.
# Labels of the form P<integer> (such as P1..P10) are ordered numerically by
# their integer index so that P2 precedes P10 regardless of input row/column
# permutation. Non-matching labels fall back to standard character sort.
canonical_point_order <- function(labels, canonical_order = NULL) {
  if (!is.character(labels) || length(labels) == 0L || anyNA(labels) ||
      any(!nzchar(trimws(labels)))) {
    stop("Point labels must be a non-empty character vector without NA or blank values.",
         call. = FALSE)
  }
  if (anyDuplicated(labels) > 0L) {
    stop("Point labels must be unique.", call. = FALSE)
  }

  if (!is.null(canonical_order)) {
    if (!is.character(canonical_order) || anyNA(canonical_order) ||
        anyDuplicated(canonical_order) > 0L) {
      stop("canonical_order must be a character vector of unique, non-NA labels.",
           call. = FALSE)
    }
    missing_from_order <- setdiff(labels, canonical_order)
    if (length(missing_from_order) > 0L) {
      stop("canonical_order is missing matrix labels: ",
           paste(missing_from_order, collapse = ", "), call. = FALSE)
    }
    return(canonical_order[canonical_order %in% labels])
  }

  is_p_number <- grepl("^P[0-9]+$", labels)
  p_numbers <- ifelse(
    is_p_number,
    suppressWarnings(as.integer(sub("^P", "", labels))),
    NA_integer_
  )

  # Group P<number> labels first in ascending numeric order, then any other
  # labels in standard lexicographical order.
  labels[order(!is_p_number, p_numbers, labels)]
}

# Validates the cost matrix, starting point, and optional logical penalty matrix.
# Returns a normalized validation bundle without mutating the caller's matrix.
validate_cost_matrix <- function(cost_matrix,
                                 start_point = "P1",
                                 penalty_matrix = NULL,
                                 require_assignment_points = FALSE,
                                 canonical_order = NULL) {
  if (!is.matrix(cost_matrix) || !is.numeric(cost_matrix)) {
    stop("cost_matrix must be a numeric matrix.", call. = FALSE)
  }

  n_rows <- nrow(cost_matrix)
  n_cols <- ncol(cost_matrix)
  if (is.null(n_rows) || is.null(n_cols) || n_rows != n_cols || n_rows < 2L) {
    stop("cost_matrix must be a square matrix with at least 2 points.",
         call. = FALSE)
  }

  row_labels <- rownames(cost_matrix)
  col_labels <- colnames(cost_matrix)
  if (is.null(row_labels) || is.null(col_labels)) {
    stop("cost_matrix must have both rownames and colnames.", call. = FALSE)
  }
  if (anyNA(row_labels) || anyNA(col_labels) ||
      any(!nzchar(trimws(row_labels))) || any(!nzchar(trimws(col_labels)))) {
    stop("cost_matrix row and column labels must be non-empty and non-NA.",
         call. = FALSE)
  }
  if (anyDuplicated(row_labels) > 0L || anyDuplicated(col_labels) > 0L) {
    stop("cost_matrix row and column labels must be unique.", call. = FALSE)
  }
  if (!identical(row_labels, col_labels)) {
    stop("cost_matrix rownames and colnames must match in the same order.",
         call. = FALSE)
  }

  if (isTRUE(require_assignment_points)) {
    if (length(row_labels) != length(expected_q2_points) ||
        !setequal(row_labels, expected_q2_points)) {
      stop("Assignment cost_matrix must contain the exact 10 points: ",
           paste(expected_q2_points, collapse = ", "), call. = FALSE)
    }
  }

  if (!is.character(start_point) || length(start_point) != 1L ||
      is.na(start_point) || !nzchar(trimws(start_point))) {
    stop("start_point must be a single non-empty character label.",
         call. = FALSE)
  }
  if (!start_point %in% row_labels) {
    stop("start_point '", start_point, "' does not exist in cost_matrix labels.",
         call. = FALSE)
  }

  off_diagonal <- row(cost_matrix) != col(cost_matrix)
  off_diag_values <- cost_matrix[off_diagonal]
  if (any(!is.finite(off_diag_values))) {
    stop("All off-diagonal entries in cost_matrix must be finite numbers (no NA, NaN, or Inf).",
         call. = FALSE)
  }
  if (any(off_diag_values < 0)) {
    stop("All off-diagonal entries in cost_matrix must be nonnegative (>= 0).",
         call. = FALSE)
  }

  # Resolve penalty matrix from argument or matrix attribute if present.
  resolved_penalty <- penalty_matrix
  if (is.null(resolved_penalty)) {
    resolved_penalty <- attr(cost_matrix, "penalty_matrix", exact = TRUE)
  }

  aligned_penalty <- NULL
  if (!is.null(resolved_penalty)) {
    if (!is.matrix(resolved_penalty) || !is.logical(resolved_penalty)) {
      stop("penalty_matrix must be a logical matrix when supplied.",
           call. = FALSE)
    }
    if (!identical(dim(resolved_penalty), dim(cost_matrix))) {
      stop("penalty_matrix must have the same dimensions as cost_matrix.",
           call. = FALSE)
    }
    pen_rows <- rownames(resolved_penalty)
    pen_cols <- colnames(resolved_penalty)
    if (is.null(pen_rows) || is.null(pen_cols) ||
        anyNA(pen_rows) || anyNA(pen_cols) ||
        anyDuplicated(pen_rows) > 0L || anyDuplicated(pen_cols) > 0L ||
        !identical(pen_rows, pen_cols) ||
        !setequal(pen_rows, row_labels)) {
      stop("penalty_matrix must have unique, matching row and column labels aligned with cost_matrix.",
           call. = FALSE)
    }

    aligned_penalty <- resolved_penalty[row_labels, col_labels, drop = FALSE]
    if (anyNA(aligned_penalty[off_diagonal])) {
      stop("Off-diagonal entries in penalty_matrix must be TRUE or FALSE (not NA).",
           call. = FALSE)
    }
  }

  ordered_labels <- canonical_point_order(row_labels, canonical_order = canonical_order)

  list(
    labels = row_labels,
    canonical_labels = ordered_labels,
    aligned_penalty = aligned_penalty,
    penalty_metadata_available = !is.null(aligned_penalty)
  )
}

# Executes the Nearest Neighbour Hamiltonian tour algorithm on a directed cost matrix.
# - Rows are origins; columns are destinations.
# - Preserves directed costs without symmetrizing.
# - Excludes the current point and already-visited points at every step.
# - Breaks equal-cost ties deterministically using canonical_labels (P1, P2, ..., P10).
# - Visits all N points exactly once before returning to start_point.
# - Leaves cost_matrix unchanged.
run_nearest_neighbour <- function(cost_matrix,
                                  start_point = "P1",
                                  penalty_matrix = NULL,
                                  canonical_order = NULL,
                                  require_assignment_points = FALSE) {
  validation <- validate_cost_matrix(
    cost_matrix = cost_matrix,
    start_point = start_point,
    penalty_matrix = penalty_matrix,
    require_assignment_points = require_assignment_points,
    canonical_order = canonical_order
  )

  canonical_labels <- validation$canonical_labels
  aligned_penalty <- validation$aligned_penalty
  has_metadata <- validation$penalty_metadata_available
  n_points <- length(canonical_labels)

  visited <- stats::setNames(rep(FALSE, n_points), canonical_labels)
  visited[[start_point]] <- TRUE

  tour_points <- character(n_points + 1L)
  tour_points[[1L]] <- start_point

  origins <- character(n_points)
  destinations <- character(n_points)
  edge_costs <- numeric(n_points)
  edge_penalized <- rep(NA, n_points)

  current_point <- start_point

  # Select N - 1 unvisited points greedily
  for (step in seq_len(n_points - 1L)) {
    # Candidate destinations in canonical order ensures permutation-invariant tie-breaking
    unvisited_candidates <- canonical_labels[!visited[canonical_labels]]
    candidate_costs <- as.numeric(cost_matrix[current_point, unvisited_candidates])

    # which.min returns the first index achieving the minimum cost in canonical order
    best_idx <- which.min(candidate_costs)
    next_point <- unvisited_candidates[[best_idx]]
    step_cost <- candidate_costs[[best_idx]]

    origins[[step]] <- current_point
    destinations[[step]] <- next_point
    edge_costs[[step]] <- step_cost
    if (has_metadata) {
      edge_penalized[[step]] <- isTRUE(aligned_penalty[current_point, next_point])
    }

    visited[[next_point]] <- TRUE
    tour_points[[step + 1L]] <- next_point
    current_point <- next_point
  }

  # Final closing edge returning to start_point
  origins[[n_points]] <- current_point
  destinations[[n_points]] <- start_point
  edge_costs[[n_points]] <- as.numeric(cost_matrix[current_point, start_point])
  if (has_metadata) {
    edge_penalized[[n_points]] <- isTRUE(aligned_penalty[current_point, start_point])
  }
  tour_points[[n_points + 1L]] <- start_point

  edge_status_label <- if (has_metadata) {
    ifelse(edge_penalized, "penalized", "unpenalized")
  } else {
    rep("unknown", n_points)
  }

  edge_table <- data.frame(
    step = seq_len(n_points),
    origin = origins,
    destination = destinations,
    cost_minutes = edge_costs,
    is_penalized = as.logical(edge_penalized),
    penalty_status = edge_status_label,
    stringsAsFactors = FALSE
  )

  total_cost <- sum(edge_costs)

  if (has_metadata) {
    penalized_edges <- edge_table[edge_table$is_penalized, , drop = FALSE]
    rownames(penalized_edges) <- NULL
    used_penalty <- nrow(penalized_edges) > 0L
    overall_penalty_status <- if (used_penalty) "penalized" else "none"
    has_history <- attr(cost_matrix, "has_usable_history", exact = TRUE)
    total_cost_label <- if (identical(has_history, FALSE)) {
      paste0(
        "Total matrix cost consists entirely of artificial fallback penalties ",
        "(NO usable historical travel times exist under Rainy Morning conditions); ",
        "NOT a physically verified travel-time estimate."
      )
    } else if (used_penalty) {
      paste0(
        "Total matrix cost includes artificial penalty costs (",
        nrow(penalized_edges),
        " penalized edge(s) used); NOT a physically verified travel-time estimate."
      )
    } else {
      "Total expected travel time in minutes (0 penalized edges used)."
    }
  } else {
    penalized_edges <- edge_table[ FALSE, , drop = FALSE]
    used_penalty <- NA
    overall_penalty_status <- "unknown"
    total_cost_label <- paste0(
      "Total matrix cost (penalty status UNKNOWN because no penalty_matrix ",
      "was supplied by Q2(a))."
    )
  }

  list(
    start_point = start_point,
    canonical_order = canonical_labels,
    route = tour_points,
    route_string = paste(tour_points, collapse = " -> "),
    edge_table = edge_table,
    total_cost = total_cost,
    total_cost_label = total_cost_label,
    penalty_metadata_available = has_metadata,
    penalty_status = overall_penalty_status,
    has_penalized_edges = used_penalty,
    penalized_edges = penalized_edges
  )
}

# Formats a Nearest Neighbour result into human-readable lines for console/logs.
format_q2b_result <- function(result) {
  lines <- c(
    "=== Q2(b) Nearest Neighbour Routing Result ===",
    sprintf("Starting Point      : %s", result$start_point),
    sprintf("Canonical Tie Order : %s", paste(result$canonical_order, collapse = ", ")),
    sprintf("Complete Tour       : %s", result$route_string),
    sprintf("Points Visited      : %d unique points + return to %s",
            length(result$route) - 1L, result$start_point),
    "",
    "Edge-by-Edge Breakdown:",
    sprintf("  %-4s  %-6s  %-11s  %12s  %-12s",
            "Step", "Origin", "Destination", "Cost (min)", "Penalty"),
    paste(rep("-", 55), collapse = "")
  )

  for (i in seq_len(nrow(result$edge_table))) {
    row <- result$edge_table[i, ]
    lines <- c(
      lines,
      sprintf("  %-4d  %-6s  %-11s  %12.2f  %-12s",
              row$step, row$origin, row$destination,
              row$cost_minutes, row$penalty_status)
    )
  }

  lines <- c(
    lines,
    paste(rep("-", 55), collapse = ""),
    sprintf("Total Matrix Cost   : %.2f minutes", result$total_cost),
    sprintf("Cost Interpretation : %s", result$total_cost_label),
    paste0("Rounding Note       : Edge costs are displayed to 2 decimal places. ",
           "The total uses unrounded values, so displayed edges may not sum exactly ",
           "to the displayed total.")
  )

  if (!result$penalty_metadata_available) {
    lines <- c(
      lines,
      "Penalty Report      : UNKNOWN (no logical penalty_matrix was supplied)."
    )
  } else if (isTRUE(result$has_penalized_edges)) {
    pen_labels <- sprintf(
      "%s -> %s (step %d, cost = %.2f)",
      result$penalized_edges$origin,
      result$penalized_edges$destination,
      result$penalized_edges$step,
      result$penalized_edges$cost_minutes
    )
    lines <- c(
      lines,
      sprintf("Penalized Edges (%d) : %s",
              nrow(result$penalized_edges),
              paste(pen_labels, collapse = "; ")),
      paste0(
        "Penalty Note        : A penalty discourages an impassable route during ",
        "selection, but does not make a closed road physically traversable. ",
        "Because Nearest Neighbour makes greedy local choices, traversing a ",
        "penalized edge (especially on the final steps) does not prove that no ",
        "penalty-free tour exists."
      )
    )
  } else {
    lines <- c(
      lines,
      "Penalized Edges (0) : None (all traversed edges have valid historical travel times)."
    )
  }

  lines
}

print_q2b_result <- function(result) {
  cat(paste(format_q2b_result(result), collapse = "\n"), "\n")
  invisible(result)
}

# Assignment-level integration function for Q2(b).
# Enforces that all 10 delivery points (P1..P10) are present in cost_matrix,
# computes the Nearest Neighbour tour, optionally writes CSV outputs under
# output_dir, and prints the formatted tour summary.
run_q2b_routing <- function(cost_matrix,
                            start_point = "P1",
                            penalty_matrix = NULL,
                            output_dir = NULL,
                            output_prefix = "q2b_tour",
                            verbose = TRUE) {
  result <- run_nearest_neighbour(
    cost_matrix = cost_matrix,
    start_point = start_point,
    penalty_matrix = penalty_matrix,
    canonical_order = expected_q2_points,
    require_assignment_points = TRUE
  )

  if (isTRUE(verbose)) {
    print_q2b_result(result)
  }

  if (!is.null(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    edges_path <- file.path(output_dir, paste0(output_prefix, "_edges.csv"))
    summary_path <- file.path(output_dir, paste0(output_prefix, "_summary.csv"))

    utils::write.csv(result$edge_table, edges_path, row.names = FALSE, na = "NA")

    summary_table <- data.frame(
      start_point = result$start_point,
      complete_route = result$route_string,
      unique_points_visited = length(result$route) - 1L,
      total_matrix_cost_minutes = result$total_cost,
      penalty_metadata_available = result$penalty_metadata_available,
      penalty_status = result$penalty_status,
      penalized_edge_count = if (result$penalty_metadata_available) {
        nrow(result$penalized_edges)
      } else {
        NA_integer_
      },
      penalized_edges = if (result$penalty_metadata_available &&
                            nrow(result$penalized_edges) > 0L) {
        paste(
          sprintf("%s->%s", result$penalized_edges$origin, result$penalized_edges$destination),
          collapse = "; "
        )
      } else if (result$penalty_metadata_available) {
        "none"
      } else {
        "unknown"
      },
      cost_interpretation = result$total_cost_label,
      stringsAsFactors = FALSE
    )
    utils::write.csv(summary_table, summary_path, row.names = FALSE, na = "NA")

    result$output_files <- list(
      edges_csv = edges_path,
      summary_csv = summary_path
    )
  }

  invisible(result)
}
