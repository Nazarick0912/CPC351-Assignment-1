# Unit and integration tests for Q2(b) Nearest Neighbour routing.
#
# Usage from repository root:
#   Rscript code/q2/test_nearest_neighbour.R

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

assert_raises_error <- function(expr, description) {
  caught <- tryCatch(
    {
      force(expr)
      FALSE
    },
    error = function(e) TRUE
  )
  if (!caught) {
    stop("Expected error did not occur for: ", description, call. = FALSE)
  }
}

run_q2b_tests <- function(project_root = locate_repo_root()) {
  source(file.path(project_root, "code", "q2", "nearest_neighbour.R"), local = TRUE)
  source(file.path(project_root, "code", "q2", "synthetic_demo.R"), local = TRUE)

  cat("=== Running Q2(b) Nearest Neighbour Test Suite ===\n")

  # 1. Known directed-cost route with manually verified total
  labels_4 <- c("P1", "P2", "P3", "P4")
  m_directed <- matrix(
    c(
      # P1  P2  P3  P4
         0, 10, 25, 30, # from P1 -> cheapest is P2 (10)
        50,  0, 40, 15, # from P2 -> cheapest unvisited is P4 (15); note P4->P2 is 5
         8, 35,  0, 20, # from P3 -> closing edge P3->P1 is 8 (whereas P1->P3 is 25)
        22,  5, 12,  0  # from P4 -> only unvisited is P3 (12)
    ),
    nrow = 4L, byrow = TRUE, dimnames = list(labels_4, labels_4)
  )
  p_directed <- matrix(FALSE, nrow = 4L, ncol = 4L, dimnames = list(labels_4, labels_4))

  res_dir <- run_nearest_neighbour(m_directed, start_point = "P1", penalty_matrix = p_directed)
  stopifnot(
    identical(res_dir$route, c("P1", "P2", "P4", "P3", "P1")),
    identical(res_dir$edge_table$cost_minutes, c(10, 15, 12, 8)),
    identical(res_dir$total_cost, 45),
    isFALSE(res_dir$has_penalized_edges),
    identical(res_dir$penalty_status, "none")
  )
  cat("[PASS] 1. Directed-cost route and manually verified total (45 min)\n")

  # 2. Equal-cost ties resolved by canonical order (P2 before P10, not alphabetical)
  labels_tie <- c("P1", "P2", "P3", "P10")
  m_tie <- matrix(
    c(
      # P1  P2  P3 P10
         0, 15, 20, 15, # from P1: tie between P2 (15) and P10 (15) -> picks P2
        18,  0, 12, 12, # from P2: tie between P3 (12) and P10 (12) -> picks P3
        14, 16,  0, 10, # from P3: only unvisited is P10 (10)
         5, 11,  9,  0  # from P10: return to P1 (5)
    ),
    nrow = 4L, byrow = TRUE, dimnames = list(labels_tie, labels_tie)
  )
  res_tie <- run_nearest_neighbour(m_tie, start_point = "P1")
  stopifnot(
    identical(res_tie$route, c("P1", "P2", "P3", "P10", "P1")),
    identical(res_tie$edge_table$cost_minutes, c(15, 12, 10, 5)),
    identical(res_tie$total_cost, 42)
  )
  cat("[PASS] 2. Equal-cost ties broken by canonical order (P2 precedes P10)\n")

  # 3. Prevention of revisits and self-loops
  m_revisit <- matrix(
    c(
      # P1  P2  P3  P4
         0, 10, 50, 60, # P1 -> P2 (10)
         1,  0, 20, 30, # P2 -> P1 is 1 (visited!) and P2->P2 is 0; must pick P3 (20)
         1,  1,  0, 25, # P3 -> P1=1, P3->P2=1 (both visited!); must pick P4 (25)
        15,  1,  1,  0  # P4 -> return to P1 (15)
    ),
    nrow = 4L, byrow = TRUE, dimnames = list(labels_4, labels_4)
  )
  res_revisit <- run_nearest_neighbour(m_revisit, start_point = "P1")
  stopifnot(
    identical(res_revisit$route, c("P1", "P2", "P3", "P4", "P1")),
    anyDuplicated(res_revisit$route[1:4]) == 0L,
    identical(res_revisit$total_cost, 10 + 20 + 25 + 15)
  )
  cat("[PASS] 3. Prevention of revisits and exclusion of current point\n")

  # 4. Inclusion of the closing edge in the total
  labels_3 <- c("P1", "P2", "P3")
  m_close <- matrix(
    c(
       0, 10, 40,
      50,  0, 20,
      35, 15,  0
    ),
    nrow = 3L, byrow = TRUE, dimnames = list(labels_3, labels_3)
  )
  res_close <- run_nearest_neighbour(m_close, start_point = "P1")
  stopifnot(
    identical(res_close$route, c("P1", "P2", "P3", "P1")),
    nrow(res_close$edge_table) == 3L,
    identical(res_close$edge_table$origin[[3L]], "P3"),
    identical(res_close$edge_table$destination[[3L]], "P1"),
    identical(res_close$edge_table$cost_minutes[[3L]], 35),
    identical(res_close$total_cost, 65)
  )
  cat("[PASS] 4. Closing return edge included in route, edge table, and total\n")

  # 5. Penalized edges (including penalized intermediate edge & penalized return edge,
  #    and verification that penalty status comes from penalty_matrix rather than a cost threshold)
  m_pen <- matrix(
    c(
      #  P1     P2     P3     P4
          0,    10,    30,    40,
         20,     0,    15, 10000,
         25,    18,     0, 10000, # P3 -> P4 is the only unvisited option and is penalized
      10000,    12,    14,     0  # P4 -> P1 return edge is also penalized
    ),
    nrow = 4L, byrow = TRUE, dimnames = list(labels_4, labels_4)
  )
  p_pen <- matrix(FALSE, nrow = 4L, ncol = 4L, dimnames = list(labels_4, labels_4))
  p_pen["P2", "P4"] <- TRUE
  p_pen["P3", "P4"] <- TRUE
  p_pen["P4", "P1"] <- TRUE

  res_pen <- run_nearest_neighbour(m_pen, start_point = "P1", penalty_matrix = p_pen)
  stopifnot(
    identical(res_pen$route, c("P1", "P2", "P3", "P4", "P1")),
    isTRUE(res_pen$penalty_metadata_available),
    isTRUE(res_pen$has_penalized_edges),
    identical(res_pen$penalty_status, "penalized"),
    identical(res_pen$edge_table$is_penalized, c(FALSE, FALSE, TRUE, TRUE)),
    nrow(res_pen$penalized_edges) == 2L,
    identical(res_pen$penalized_edges$origin, c("P3", "P4")),
    identical(res_pen$penalized_edges$destination, c("P4", "P1")),
    identical(res_pen$total_cost, 20025),
    grepl("artificial penalty", res_pen$total_cost_label, ignore.case = TRUE)
  )

  # Verify no arbitrary threshold inference when penalty_matrix says FALSE on a large cost
  p_all_false <- matrix(FALSE, nrow = 4L, ncol = 4L, dimnames = list(labels_4, labels_4))
  res_no_thresh <- run_nearest_neighbour(m_pen, start_point = "P1", penalty_matrix = p_all_false)
  stopifnot(
    isFALSE(res_no_thresh$has_penalized_edges),
    identical(res_no_thresh$penalty_status, "none")
  )
  cat("[PASS] 5. Penalized intermediate and return edges accurately tracked without threshold guessing\n")

  # 6. Missing penalty metadata reported as unknown
  res_unknown <- run_nearest_neighbour(m_directed, start_point = "P1", penalty_matrix = NULL)
  stopifnot(
    isFALSE(res_unknown$penalty_metadata_available),
    identical(res_unknown$penalty_status, "unknown"),
    is.na(res_unknown$has_penalized_edges),
    all(is.na(res_unknown$edge_table$is_penalized)),
    all(res_unknown$edge_table$penalty_status == "unknown"),
    grepl("UNKNOWN", res_unknown$total_cost_label)
  )
  cat("[PASS] 6. Missing penalty metadata reported as 'unknown'\n")

  # 7. Validation of invalid matrix labels, starting points, and off-diagonal values
  m_non_square <- matrix(1:6, nrow = 2L, ncol = 3L)
  assert_raises_error(run_nearest_neighbour(m_non_square), "non-square matrix")

  m_no_names <- matrix(c(0, 1, 1, 0), nrow = 2L)
  assert_raises_error(run_nearest_neighbour(m_no_names), "unlabelled matrix")

  m_mismatch <- m_directed
  colnames(m_mismatch) <- c("P1", "P3", "P2", "P4")
  assert_raises_error(run_nearest_neighbour(m_mismatch), "mismatched row/col labels")

  m_dup <- m_directed
  rownames(m_dup) <- colnames(m_dup) <- c("P1", "P1", "P3", "P4")
  assert_raises_error(run_nearest_neighbour(m_dup), "duplicate labels")

  assert_raises_error(
    run_nearest_neighbour(m_directed, start_point = "P99"),
    "non-existent starting point"
  )

  m_neg <- m_directed
  m_neg["P1", "P2"] <- -1
  assert_raises_error(run_nearest_neighbour(m_neg), "negative off-diagonal cost (-1)")

  m_na <- m_directed
  m_na["P1", "P2"] <- NA_real_
  assert_raises_error(run_nearest_neighbour(m_na), "NA off-diagonal cost")

  m_inf <- m_directed
  m_inf["P1", "P2"] <- Inf
  assert_raises_error(run_nearest_neighbour(m_inf), "Inf off-diagonal cost")

  assert_raises_error(
    run_q2b_routing(m_directed, start_point = "P1", verbose = FALSE),
    "assignment wrapper called with fewer than 10 points"
  )
  cat("[PASS] 7. Invalid matrices, labels, start points, and non-finite/negative costs rejected\n")

  # 8. Permutation invariance when rows and columns are permuted together
  perm_order <- c("P10", "P3", "P1", "P2")
  m_tie_perm <- m_tie[perm_order, perm_order]
  res_tie_perm <- run_nearest_neighbour(m_tie_perm, start_point = "P1")
  stopifnot(
    identical(res_tie$route, res_tie_perm$route),
    identical(res_tie$total_cost, res_tie_perm$total_cost),
    identical(res_tie$edge_table, res_tie_perm$edge_table)
  )
  cat("[PASS] 8. Route and costs are invariant to simultaneous row/column permutation\n")

  # 9. Input cost matrix and penalty matrix remain completely unchanged
  m_before <- unserialize(serialize(m_pen, NULL))
  p_before <- unserialize(serialize(p_pen, NULL))
  invisible(run_nearest_neighbour(m_pen, start_point = "P1", penalty_matrix = p_pen))
  stopifnot(
    identical(m_pen, m_before),
    identical(p_pen, p_before)
  )
  cat("[PASS] 9. Input cost_matrix and penalty_matrix remain untouched\n")

  # 10. Ten-point synthetic demonstration visits all 10 points once and returns to start
  demo <- run_synthetic_q2b_demo(
    project_root = project_root,
    write_outputs = FALSE,
    verbose = FALSE
  )
  tour_p1 <- demo$tour_start_P1
  tour_p5 <- demo$tour_start_P5

  stopifnot(
    length(tour_p1$route) == 11L,
    identical(tour_p1$route[[1L]], "P1"),
    identical(tour_p1$route[[11L]], "P1"),
    setequal(tour_p1$route[1:10], expected_q2_points),
    anyDuplicated(tour_p1$route[1:10]) == 0L,
    identical(
      tour_p1$route,
      c("P1", "P3", "P6", "P10", "P8", "P4", "P2", "P7", "P9", "P5", "P1")
    ),
    identical(tour_p1$total_cost, 145),
    isFALSE(tour_p1$has_penalized_edges),
    # Scenario 2 (start at P5) visits all 10 points and hits penalized return edge P1 -> P5
    length(tour_p5$route) == 11L,
    setequal(tour_p5$route[1:10], expected_q2_points),
    isTRUE(tour_p5$has_penalized_edges),
    nrow(tour_p5$penalized_edges) == 1L,
    identical(tour_p5$penalized_edges$origin[[1L]], "P1"),
    identical(tour_p5$penalized_edges$destination[[1L]], "P5"),
    identical(tour_p5$total_cost, 10140)
  )
  cat("[PASS] 10. Ten-point synthetic tours verified (clean 145 min from P1; penalized 10140 min from P5)\n")

  cat("\nALL 10 Q2(b) TEST SUITES PASSED.\n")
  invisible(TRUE)
}

if (sys.nframe() == 0L) {
  run_q2b_tests()
}
