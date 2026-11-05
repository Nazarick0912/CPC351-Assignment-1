# CPC351 Assignment 01

USM CPC351/CPM351 Principles of Data Analytics, Semester 1, 2026/2027.

R project scaffold for exam scheduling (Q1) and delivery routing (Q2).
The baseline inventories Q1 inputs and checks Q2 CSV column names;
the assignment algorithms are still pending.

## Project links

- [Assignment brief](S261_CPC351_Assignment_01_v01.pdf)
- [Shared Drive](https://drive.google.com/drive/u/0/folders/19dXocKj3UaWQLkAHBNtxY1Q45Rxe5zI_)
- [Google Docs report](https://docs.google.com/document/d/1GAB7_rqw0hhjneBch7KhtM-GNt_AlFTzSP1gphYUyOE/edit?usp=drive_link)

## Structure

| Location | Contents |
| --- | --- |
| `code/` | Runner, shared helpers, Q1/Q2 modules, and learning examples |
| `data/raw/` | Original inputs and file hashes |
| `data/synthetic/` | Clearly labeled development-only inputs |
| `data/processed/` | Generated cleaned data |
| `outputs/` | Generated tables, schedules, routes, and figures |
| `submission/` | Final PDF export and assignment package |

Generated outputs and submission artifacts are ignored by Git. Synthetic
development inputs are kept separate from raw and grading data.

## Run

Install R and run from the repository root. Only base R is required.
For RStudio, open `CPC351-Assignment-1.Rproj`.

```sh
Rscript code/run.R
Rscript code/run.R q1
Rscript code/run.R q2
Rscript code/run.R q2 "path/to/ahmad_travel_logs.csv"
```

The default runs both questions. Q1 writes `outputs/q1/input_inventory.csv`.
Q2 skips when its input CSV is absent; no Q2 dataset is supplied.

See the [Q1 plan](code/q1/README.md), [Q2 plan](code/q2/README.md),
[data notes](data/README.md), and [submission guide](submission/README.md).
Agent guidance is in [AGENTS.md](AGENTS.md).
