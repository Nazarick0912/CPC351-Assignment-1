# Q2 module: input schema check, Q2(a) integration hook, and Q2(b) Nearest Neighbour routing.
# No dataset is provided with the assignment materials.

required_travel_columns <- c(
  "Origin_X", "Origin_Y", "Dest_X", "Dest_Y",
  "Weather", "Time_of_Day", "Travel_Time_Minutes"
)

# Load Q2(b) Nearest Neighbour implementation into the active environment
load_q2b_module <- function(project_root = NULL) {
  candidates <- character(0)
  if (!is.null(project_root)) {
    candidates <- c(candidates, file.path(project_root, "code", "q2", "nearest_neighbour.R"))
  }
  candidates <- c(candidates, file.path("code", "q2", "nearest_neighbour.R"), "nearest_neighbour.R")
  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0L) {
    stop("Cannot locate code/q2/nearest_neighbour.R.", call. = FALSE)
  }
  source(existing[[1L]], local = parent.frame())
}

load_q2b_module(if (exists("project_root", inherits = TRUE)) get("project_root", inherits = TRUE) else NULL)

# Connects Q2(a) output (either a 10x10 matrix or list(cost_matrix, penalty_matrix))
# directly to Q2(b) Nearest Neighbour routing.
run_q2b_from_q2a <- function(q2a_result,
                             start_point = "P1",
                             output_dir = NULL,
                             output_prefix = "q2b_tour",
                             verbose = TRUE) {
  if (is.matrix(q2a_result)) {
    cost_matrix <- q2a_result
    penalty_matrix <- attr(q2a_result, "penalty_matrix", exact = TRUE)
  } else if (is.list(q2a_result) && !is.null(q2a_result$cost_matrix)) {
    cost_matrix <- q2a_result$cost_matrix
    penalty_matrix <- q2a_result$penalty_matrix
  } else {
    stop(
      "q2a_result must be either a 10x10 numeric cost_matrix or a list containing ",
      "$cost_matrix (and optionally $penalty_matrix).",
      call. = FALSE
    )
  }

  run_q2b_routing(
    cost_matrix = cost_matrix,
    start_point = start_point,
    penalty_matrix = penalty_matrix,
    output_dir = output_dir,
    output_prefix = output_prefix,
    verbose = verbose
  )
}

inspect_q2_inputs <- function(project_root, input_path, start_point = "P1") {
  load_q2b_module(project_root)

  if (!file.exists(input_path)) {
    message("Q2: no travel CSV found at ", input_path)
    message("Q2: skipped. Supply the CSV path when a dataset is available.")
    message(
      "Q2(b): Nearest Neighbour routing is implemented. ",
      "Run `Rscript code/q2/synthetic_demo.R` for the standalone synthetic demo ",
      "or `Rscript code/q2/test_nearest_neighbour.R` for unit tests."
    )
    return(invisible(list(
      status = "missing_input",
      path = input_path,
      q2b_ready = TRUE
    )))
  }

  header <- utils::read.csv(input_path, nrows = 0L, check.names = FALSE,
                            stringsAsFactors = FALSE)
  missing_columns <- setdiff(required_travel_columns, names(header))

  if (length(missing_columns) > 0L) {
    stop("Q2 CSV is missing required columns: ",
         paste(missing_columns, collapse = ", "), call. = FALSE)
  }
  if (anyDuplicated(names(header)) > 0L) {
    stop("Q2 CSV contains duplicate column names.", call. = FALSE)
  }

  message("Q2: required CSV column names are present.")

  # Integration hook: if Q2(a) defines build_q2a_cost_matrix(input_path),
  # execute Q2(a) -> Q2(b) automatically. Otherwise report honestly that Q2(a)
  # is still pending without substituting synthetic data.
  if (exists("build_q2a_cost_matrix", mode = "function", inherits = TRUE)) {
    q2a_fn <- get("build_q2a_cost_matrix", mode = "function", inherits = TRUE)
    q2a_out <- q2a_fn(input_path)
    out_dir <- file.path(project_root, "outputs", "q2")
    q2b_out <- run_q2b_from_q2a(
      q2a_result = q2a_out,
      start_point = start_point,
      output_dir = out_dir,
      output_prefix = "q2b_tour",
      verbose = TRUE
    )
    return(invisible(list(
      status = "completed",
      path = input_path,
      columns = names(header),
      q2a = q2a_out,
      q2b = q2b_out
    )))
  }

  message("Q2(a): value checks, cleaning, and 75th-percentile cost matrix generation are pending.")
  message("Q2(b): Nearest Neighbour routing (`run_q2b_routing`) is implemented and awaiting Q2(a)'s cost matrix.")
  invisible(list(
    status = "schema_checked_awaiting_q2a",
    path = input_path,
    columns = names(header),
    q2b_ready = TRUE
  ))
}
