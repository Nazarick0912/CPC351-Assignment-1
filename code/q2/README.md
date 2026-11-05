# Question 2: travel costs and routing

## Current status

- **Q2(a) (Data cleaning & 75th-percentile cost matrix)**: Implemented in
  `code/q2/q2a_cost_matrix.R`. Reads raw travel logs, filters for Rainy Morning,
  validates coordinates, cleans road closure records (`NA` and `-1`), calculates
  the 75th percentile per directed route, assigns severe penalties, and outputs
  the cost matrix, filtered log, and heatmap.
- **Q2(b) (Nearest Neighbour routing)**: Implemented in
  `code/q2/nearest_neighbour.R`. Solves the Hamiltonian tour from any starting
  point (default `P1`), visits all points, handles deterministic tie-breaking,
  adds the closing return edge, tracks penalties, and formats output summaries.
  Verified by 10 unit test suites in `code/q2/test_nearest_neighbour.R` and a
  standalone in-memory demonstration in `code/q2/synthetic_demo.R`.
- **End-to-end pipeline integration**: Pending. `code/run.R q2` currently
  performs baseline schema checks. Connecting `q2a_cost_matrix.R` directly into
  `nearest_neighbour.R` in the shared runner is scheduled for the next stage.

## Commands

Run from the repository root:

```powershell
# Project baseline runner (schema check)
Rscript code/run.R q2
Rscript code/run.R q2 "C:/path/to/ahmad_travel_logs.csv"

# Q2(a) prototype: builds cost matrix, filtered logs, and heatmap
Rscript code/q2/q2a_cost_matrix.R
Rscript code/q2/q2a_cost_matrix.R "C:/path/to/ahmad_travel_logs.csv"

# Q2(b) test suite (10 unit & integration test suites)
Rscript code/q2/test_nearest_neighbour.R

# Q2(b) standalone in-memory synthetic demonstration
Rscript code/q2/synthetic_demo.R
```

## Running Q2(b) against an in-memory cost matrix

```r
source("code/q2/nearest_neighbour.R")

# cost_matrix: 10 x 10 numeric matrix with row/col names P1..P10
# penalty_matrix: optional 10 x 10 logical matrix (TRUE where edge is penalized)
result <- run_q2b_routing(
  cost_matrix = cost_matrix,
  start_point = "P1",
  penalty_matrix = penalty_matrix,
  output_dir = "outputs/q2",
  output_prefix = "q2b_tour"
)
```

For smaller test fixtures ($N \ge 2$), call:
`run_nearest_neighbour(cost_matrix, start_point = "P1", penalty_matrix = NULL)`.

## Q2(a) Data Cleaning and Severe Penalty Calculation

The Q2(a) script filters rows where `Weather == "Rainy"` and `Time_of_Day == "Morning"`,
retaining records whose coordinates match the 10 assigned points and whose travel
times are non-negative and non-missing. Cleaned records are saved to
`data/processed/q2/ahmad_travel_logs_filtered.csv`.

For each directed pair $(P_i, P_j)$, the 75th percentile travel time is computed.
If a route has no valid travel history (all records missing, closures, or
unobserved), it is assigned a severe penalty:

```text
penalty = 11 * M + 1 minute
```
where `M` is the maximum valid observed route cost in the matrix. Because a
valid 10-point tour consists of 10 legs with total cost at most $10 \times M$,
a single penalty edge ($11 \times M + 1$) costs more than any conceivable
all-valid tour, making impassable routes strongly discouraged.

## Q2(a) -> Q2(b) Integration Contract

To connect Q2(a) directly to Q2(b):

1. **Cost Matrix (`cost_matrix`)**:
   - Base R numeric `10 x 10` matrix (`is.matrix(M) && is.numeric(M)`).
   - Rows represent **origins**; columns represent **destinations**.
   - `rownames(M)` and `colnames(M)` must match in the same order and contain
     all 10 points `P1, P2, ..., P10`.
   - Off-diagonal entries must be finite and nonnegative (`>= 0`).
   - Diagonal entries (`Pi -> Pi`) represent self-travel and are set to `0`.

2. **Penalty Metadata (`penalty_matrix`)**:
   - A `10 x 10` logical matrix (`TRUE`/`FALSE`) with matching row and column
     names, where `TRUE` marks impassable routes assigned artificial penalties.
   - May be passed via the `penalty_matrix` argument or as an attribute
     `attr(M, "penalty_matrix")`. If omitted, penalty status is reported as
     `"unknown"`.

3. **Automatic Pipeline Hook**:
   - Wrap the Q2(a) logic in a function `build_q2a_cost_matrix(input_path)`
     returning `list(cost_matrix = M, penalty_matrix = P)` or `M`.
   - `inspect_q2_inputs()` in `code/q2/main.R` will automatically execute
     Q2(a) and pass its matrix directly to `run_q2b_from_q2a()`.

Write cleaned or aggregated data to `data/processed/q2/` and cost matrices,
route summaries, validation output, and figures to `outputs/q2/`. Insert selected
results into the Question 2 section of the
[shared Google Docs report](https://docs.google.com/document/d/1GAB7_rqw0hhjneBch7KhtM-GNt_AlFTzSP1gphYUyOE/edit?usp=drive_link)
and explain the cleaning, penalties, and route selection there.
