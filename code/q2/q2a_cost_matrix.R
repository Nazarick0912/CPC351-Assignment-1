# CPC351 Assignment 01 - Question 2(a)
# Build the Rainy Morning travel-time cost matrix.

# Delivery point coordinates, in the order P1 to P10.
point_names <- paste("P", 1:10, sep = "")
points <- data.frame(
  x = c(60, 180, 80, 140, 20, 100, 200, 140, 40, 100),
  y = c(200, 200, 180, 180, 160, 160, 160, 140, 120, 120)
)

# Use an explicitly supplied CSV, the real input when present, or the synthetic
# development copy as a fallback when no real input is available.
arguments <- commandArgs(trailingOnly = TRUE)
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
logs <- read.csv(input_path, check.names = FALSE, stringsAsFactors = FALSE)

# Keep only Rainy Morning records. Change missing weather/time conditions to FALSE.
rainy_morning_rows <- logs$Weather == "Rainy" & logs$Time_of_Day == "Morning"

rainy_morning_rows[is.na(rainy_morning_rows)] <- FALSE
rainy_logs <- logs[rainy_morning_rows, ]


# Start an empty 10 by 10 matrix. Rows are origins; columns are destinations.
cost_matrix <- matrix(NA, nrow = 10, ncol = 10)
used_record_count <- 0
observed_route_count <- 0
outside_point_count <- 0
rainy_morning_row_count <- nrow(rainy_logs)
valid_point_rows <- rep(FALSE, nrow(rainy_logs))

# Keep only records whose origin and destination are among the ten points.
for (record in seq_len(nrow(rainy_logs))) {
  origin_found <- FALSE
  destination_found <- FALSE

  for (point in 1:10) {
    if (!is.na(rainy_logs$Origin_X[record]) &
        !is.na(rainy_logs$Origin_Y[record]) &
        rainy_logs$Origin_X[record] == points$x[point] &
        rainy_logs$Origin_Y[record] == points$y[point]) {
      origin_found <- TRUE
    }

    if (!is.na(rainy_logs$Dest_X[record]) &
        !is.na(rainy_logs$Dest_Y[record]) &
        rainy_logs$Dest_X[record] == points$x[point] &
        rainy_logs$Dest_Y[record] == points$y[point]) {
      destination_found <- TRUE
    }
  }

  if (origin_found & destination_found) {
    valid_point_rows[record] <- TRUE
  } else {
    outside_point_count <- outside_point_count + 1
  }
}

rainy_logs <- rainy_logs[valid_point_rows, ]

travel_times <- rainy_logs$Travel_Time_Minutes
valid_time_rows <- !is.na(travel_times) & travel_times >= 0
filtered_logs <- rainy_logs[valid_time_rows, ]

processed_dir <- file.path("data", "processed", "q2")
dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(filtered_logs,
          file.path(processed_dir, "ahmad_travel_logs_filtered.csv"),
          row.names = FALSE)

# Calculate the 75th percentile for every directed origin-destination route.
for (origin in 1:10) {
  for (destination in 1:10) {
    if (origin != destination) {
      route_rows <- filtered_logs$Origin_X == points$x[origin] &
        filtered_logs$Origin_Y == points$y[origin] &
        filtered_logs$Dest_X == points$x[destination] &
        filtered_logs$Dest_Y == points$y[destination]

      # Missing route matches are excluded.
      route_rows[is.na(route_rows)] <- FALSE

      route_times <- filtered_logs$Travel_Time_Minutes[route_rows]
      used_record_count <- used_record_count + length(route_times)

      if (length(route_times) > 0) {
        cost_matrix[origin, destination] <- quantile(route_times, 0.75)
        observed_route_count <- observed_route_count + 1
      }
    } else {
      # Travelling from a point to itself has zero cost.
      cost_matrix[origin, destination] <- 0
    }
  }
}

# Give routes with no valid history a large cost.
maximum_observed_cost <- max(cost_matrix, na.rm = TRUE)
impassable_penalty <- maximum_observed_cost * 11 + 1
penalty_route_count <- 0

for (origin in 1:10) {
  for (destination in 1:10) {
    if (is.na(cost_matrix[origin, destination])) {
      cost_matrix[origin, destination] <- impassable_penalty
      penalty_route_count <- penalty_route_count + 1
    }
  }
}

# Print the cleaning results and matrix.
print(paste("Rows in file:", nrow(logs)))
print(paste("Rainy Morning rows before coordinate check:", rainy_morning_row_count))
print(paste("Rows removed for outside points:", outside_point_count))
print(paste("Rainy Morning rows after coordinate check:", nrow(rainy_logs)))
print(paste("Missing travel times:", sum(is.na(travel_times))))
print(paste("Road-closure records (-1):", sum(travel_times == -1, na.rm = TRUE)))
print(paste("Other negative travel times:",
            sum(travel_times < 0 & travel_times != -1, na.rm = TRUE)))
print(paste("Valid travel records used:", used_record_count))
print(paste("Observed directed routes:", observed_route_count))
print(paste("Routes given the high penalty:", penalty_route_count))
print(paste("High penalty in minutes:", impassable_penalty))

print("Cost matrix: rows are origins, columns are destinations; order is P1 to P10")
print(cost_matrix)

# Save generated results under outputs/q2.
output_dir <- file.path("outputs", "q2")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Save a CSV table with origin names and P1-P10 destination columns.
cost_table <- data.frame(
  Origin = point_names,
  P1 = cost_matrix[, 1], P2 = cost_matrix[, 2],
  P3 = cost_matrix[, 3], P4 = cost_matrix[, 4],
  P5 = cost_matrix[, 5], P6 = cost_matrix[, 6],
  P7 = cost_matrix[, 7], P8 = cost_matrix[, 8],
  P9 = cost_matrix[, 9], P10 = cost_matrix[, 10]
)
write.table(cost_table, file.path(output_dir, "q2a_cost_matrix.csv"),
            sep = ",", row.names = FALSE)

# Plot the matrix values with point names centered on each cell.
png(file.path(output_dir, "q2a_cost_matrix_heatmap.png"), width = 900, height = 800)
observed_costs <- cost_matrix[cost_matrix != impassable_penalty]
highest_observed_cost <- max(observed_costs, na.rm = TRUE)
if (highest_observed_cost <= 0) highest_observed_cost <- 1
travel_time_colors <- grDevices::colorRampPalette(
  c("#fff8df", "#f2dda9", "#e7ad8c", "#cf8888")
)(100)
image(1:10, 1:10, cost_matrix, col = travel_time_colors,
      zlim = c(0, highest_observed_cost), axes = FALSE,
      xlab = "Origin point", ylab = "Destination point",
      main = "Synthetic Rainy Morning: 75th-percentile travel time (minutes)")
axis(1, at = 1:10, labels = point_names, las = 2, cex.axis = 0.8)
axis(2, at = 1:10, labels = point_names, las = 1, cex.axis = 0.8)
box()
title(sub = "Blue = same-point zero; P = penalty; redder = higher travel time")

for (origin in 1:10) {
  for (destination in 1:10) {
    if (origin == destination) {
      rect(origin - 0.5, destination - 0.5, origin + 0.5, destination + 0.5,
           col = "#77a6b5", border = NA)
      text(origin, destination, "0", col = "black", font = 2)
    } else if (cost_matrix[origin, destination] == impassable_penalty) {
      rect(origin - 0.5, destination - 0.5, origin + 0.5, destination + 0.5,
           col = "#8b78a1", border = NA)
      text(origin, destination, "P", col = "white", font = 2)
    } else {
      text(origin, destination, round(cost_matrix[origin, destination], 1))
    }
  }
}
dev.off()
