# Question 2: travel costs and routing

## Current status

- **Q2(a) (Data cleaning & 75th-percentile cost matrix)**: Pending. `main.R` validates the 7 required CSV columns when a file is present and skips cleanly when the file is absent.
- **Q2(b) (Nearest Neighbour routing)**: Implemented in `code/q2/nearest_neighbour.R`, with standalone unit tests in `code/q2/test_nearest_neighbour.R` and an explicitly separated synthetic demonstration in `code/q2/synthetic_demo.R`.

## Commands

Run from the repository root:

```powershell
# Normal pipeline runner (never substitutes synthetic data when input or Q2(a) is missing)
Rscript code/run.R q2
Rscript code/run.R q2 "C:/path/to/ahmad_travel_logs.csv"

# Run Q2(b) unit and integration tests (10 test suites)
Rscript code/q2/test_nearest_neighbour.R

# Run standalone 10-point synthetic Q2(b) demonstration and export synthetic CSVs to outputs/q2/
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

For smaller test fixtures ($N \ge 2$), call `run_nearest_neighbour(cost_matrix, start_point = "P1", penalty_matrix = NULL)`.

## Q2(a) -> Q2(b) Integration Contract

To connect Q2(a) directly to Q2(b):

1. **Cost Matrix (`cost_matrix`)**:
   - Base R numeric `10 x 10` matrix (`is.matrix(M) && is.numeric(M)`).
   - Rows represent **origins** (`Origin_X`, `Origin_Y`); columns represent **destinations** (`Dest_X`, `Dest_Y`).
   - `rownames(M)` and `colnames(M)` must match in the same order and contain all 10 points `P1, P2, ..., P10`.
   - Off-diagonal entries must be finite and nonnegative (`>= 0`), representing the 75th-percentile `Travel_Time_Minutes` under `Weather == "Rainy"` and `Time_of_Day == "Morning"`, or a finite penalty value for impassable/unobserved routes.
   - Diagonal entries (`Pi -> Pi`) are ignored during candidate selection (recommended value: `0`).

2. **Penalty Metadata (`penalty_matrix`)**:
   - A `10 x 10` logical matrix (`TRUE`/`FALSE`) with row and column names aligned to `cost_matrix`, where `TRUE` indicates that the directed route had no valid rainy-morning travel history and was assigned an artificial penalty cost.
   - May be supplied either as `list(cost_matrix = M, penalty_matrix = P)`, via the `penalty_matrix` argument, or attached as `attr(M, "penalty_matrix") <- P`.
   - If omitted (`NULL`), Q2(b) reports penalty status as `"unknown"` rather than guessing from a cost threshold.

3. **Automatic Pipeline Hook**:
   - Define `build_q2a_cost_matrix <- function(input_path)` in `code/q2/` returning either `list(cost_matrix = M, penalty_matrix = P)` or `M` (with `attr(M, "penalty_matrix")`).
   - `inspect_q2_inputs()` in `code/q2/main.R` automatically detects `build_q2a_cost_matrix`, executes it on `input_path`, and passes the result to `run_q2b_from_q2a()`.
