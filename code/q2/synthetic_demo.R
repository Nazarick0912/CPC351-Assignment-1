# Standalone synthetic demonstration for Q2(b) Nearest Neighbour routing.
#
# IMPORTANT: All data in this script is SYNTHETIC and constructed strictly to
# demonstrate and verify Q2(b) independently of Q2(a) and the unseen grading
# dataset (ahmad_travel_logs.csv).
#
# Usage from repository root:
#   Rscript code/q2/synthetic_demo.R

locate_repo_root <- function() {
  file_argument <- grep("^--file=", commandArgs(), value = TRUE)
  starting_points <- getwd()

  if (length(file_argument) > 0L) {
    script_path <- sub("^--file=", "", file_argument[[1L]])
    starting_points <- c(dirname(script_path), starting_points)
  }

  for (start in starting_points) {
    candidate <- normalizePath(start, winslash = "/", mustWork = TRUE)
    repeat {
      if (file.exists(file.path(candidate, "CPC351-Assignment-1.Rproj")) &&
          file.exists(file.path(candidate, "code", "run.R"))) {
        return(candidate)
      }
      parent <- dirname(candidate)
      if (identical(parent, candidate)) break
      candidate <- parent
    }
  }

  stop("Cannot find repository root.", call. = FALSE)
}

# Constructs a deterministic 10 x 10 directed (asymmetric) synthetic cost matrix
# and aligned logical penalty matrix across P1..P10.
#
# Key features of this synthetic fixture:
# 1. Directed asymmetry: C[i, j] != C[j, i] for all off-diagonal pairs.
#    Notably, P5 -> P1 is open (16 min), whereas P1 -> P5 is blocked by a
#    one-way rainy-morning road closure (penalty = 10000 min).
# 2. Equal-cost tie: From P3, destinations P6 and P10 both cost 14 min;
#    canonical tie-breaking (P1..P10) deterministically selects P6 before P10.
# 3. Blocked routes (penalty_matrix == TRUE, cost == penalty_value):
#    P1 -> P5, P2 -> P1, P3 -> P4, P6 -> P8, and P7 -> P8.
build_synthetic_q2_matrix <- function(penalty_value = 10000) {
  labels <- paste0("P", 1:10)

  # Rows = Origin (P1..P10), Columns = Destination (P1..P10)
  cost_matrix <- matrix(
    c(
      # P1    P2    P3    P4    P5    P6    P7    P8    P9   P10
         0,   28,   12,   24,   18,   21,   42,   36,   31,   33, # from P1
        30,    0,   27,   14,   39,   25,   13,   21,   41,   32, # from P2
        15,   26,    0,   18,   19,   14,   35,   25,   27,   14, # from P3 (tie: P6=14, P10=14)
        22,   11,   16,    0,   32,   15,   18,   14,   35,   23, # from P4
        16,   38,   17,   30,    0,   20,   40,   33,   15,   25, # from P5
        24,   29,   17,   16,   22,    0,   31,   19,   19,   11, # from P6
        44,   16,   33,   19,   37,   28,    0,   20,   29,   22, # from P7
        38,   23,   26,   12,   34,   16,   17,    0,   28,   15, # from P8
        29,   43,   24,   33,   14,   18,   34,   25,    0,   16, # from P9
        35,   34,   22,   21,   26,   14,   24,   13,   15,    0  # from P10
    ),
    nrow = 10L,
    ncol = 10L,
    byrow = TRUE,
    dimnames = list(labels, labels)
  )

  penalty_matrix <- matrix(
    FALSE,
    nrow = 10L,
    ncol = 10L,
    dimnames = list(labels, labels)
  )

  # Mark 5 directed edges as impassable road closures under rainy morning conditions
  blocked_edges <- rbind(
    c("P1", "P5"),
    c("P2", "P1"),
    c("P3", "P4"),
    c("P6", "P8"),
    c("P7", "P8")
  )

  for (k in seq_len(nrow(blocked_edges))) {
    orig <- blocked_edges[k, 1L]
    dest <- blocked_edges[k, 2L]
    penalty_matrix[orig, dest] <- TRUE
    cost_matrix[orig, dest] <- penalty_value
  }

  attr(cost_matrix, "penalty_matrix") <- penalty_matrix

  list(
    cost_matrix = cost_matrix,
    penalty_matrix = penalty_matrix,
    penalty_value = penalty_value
  )
}

run_synthetic_q2b_demo <- function(project_root = locate_repo_root(),
                                   write_outputs = TRUE,
                                   verbose = TRUE) {
  source(file.path(project_root, "code", "q2", "nearest_neighbour.R"), local = TRUE)

  synthetic <- build_synthetic_q2_matrix(penalty_value = 10000)
  out_dir <- if (isTRUE(write_outputs)) {
    file.path(project_root, "outputs", "q2")
  } else {
    NULL
  }

  if (isTRUE(verbose)) {
    cat("=================================================================\n")
    cat("SYNTHETIC Q2(b) DEMONSTRATION (NOT FROM ahmad_travel_logs.csv)\n")
    cat("=================================================================\n\n")
    cat("--- Scenario 1: Start at P1 (Avoids all penalized routes) ---\n")
  }

  tour_p1 <- run_q2b_routing(
    cost_matrix = synthetic$cost_matrix,
    start_point = "P1",
    penalty_matrix = synthetic$penalty_matrix,
    output_dir = out_dir,
    output_prefix = "synthetic_demo_start_P1",
    verbose = verbose
  )

  if (isTRUE(verbose)) {
    cat("\n--- Scenario 2: Start at P5 (Forced onto penalized return edge P1 -> P5) ---\n")
  }

  tour_p5 <- run_q2b_routing(
    cost_matrix = synthetic$cost_matrix,
    start_point = "P5",
    penalty_matrix = synthetic$penalty_matrix,
    output_dir = out_dir,
    output_prefix = "synthetic_demo_start_P5",
    verbose = verbose
  )

  if (!is.null(out_dir)) {
    matrix_df <- data.frame(
      Origin = rownames(synthetic$cost_matrix),
      as.data.frame(synthetic$cost_matrix, check.names = FALSE),
      row.names = NULL,
      stringsAsFactors = FALSE
    )
    penalty_df <- data.frame(
      Origin = rownames(synthetic$penalty_matrix),
      as.data.frame(synthetic$penalty_matrix, check.names = FALSE),
      row.names = NULL,
      stringsAsFactors = FALSE
    )
    utils::write.csv(
      matrix_df,
      file.path(out_dir, "synthetic_demo_cost_matrix.csv"),
      row.names = FALSE
    )
    utils::write.csv(
      penalty_df,
      file.path(out_dir, "synthetic_demo_penalty_matrix.csv"),
      row.names = FALSE
    )
    if (isTRUE(verbose)) {
      cat("\nSynthetic demonstration files written to:", out_dir, "\n")
    }
  }

  invisible(list(
    synthetic = synthetic,
    tour_start_P1 = tour_p1,
    tour_start_P5 = tour_p5
  ))
}

if (sys.nframe() == 0L) {
  run_synthetic_q2b_demo()
}
