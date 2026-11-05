# Data layout and provenance

## Supplied Question 1 data

The original distribution archive, `Data-20261030.zip`, contains six registration
lists under its `Q1/` directory. They are stored in `data/raw/q1/` and match
that archive's contents apart from line endings. The ZIP is not duplicated in
Git; the extracted inputs are sufficient to run the project.

The original archive uses CRLF line endings. The initial repository commit
normalized the lists to LF. This layout preserves those committed bytes and
uses `.gitattributes` to prevent further conversion. Student identifiers have
not been edited.

| Course | File | Raw rows | Exact duplicate rows |
| --- | --- | ---: | ---: |
| CCS592 | `raw/q1/Q1_CCS592.txt` | 38 | 0 |
| CDS501 | `raw/q1/Q1_CDS501.txt` | 54 | 0 |
| CDS506 | `raw/q1/Q1_CDS506.txt` | 38 | 0 |
| CDS512 | `raw/q1/Q1_CDS512.txt` | 64 | 0 |
| CDS521 | `raw/q1/Q1_CDS521.txt` | 66 | 1 |
| CDS523 | `raw/q1/Q1_CDS523.txt` | 6 | 0 |

These are raw row counts, not cleaned student counts. In CDS521, `NAME029`
appears twice. Other observed inconsistencies include hyphens, underscores,
spaces, varying zero padding, and the spelling `NEMA` in some records. No
interpretation or correction has been applied yet.

`raw/manifest.json` records the source archive hash, the original entry names,
the repository paths, byte counts, raw row counts, and SHA-256 hashes. Compare
against this manifest if the integrity of a raw file is in doubt.

## Question 2 input

No Q2 dataset is supplied. The default future input path is
`data/raw/q2/ahmad_travel_logs.csv`; the R runner also accepts an explicit path.
The assignment defines these columns:

| Column | Expected meaning |
| --- | --- |
| `Origin_X`, `Origin_Y` | Numeric origin coordinates |
| `Dest_X`, `Dest_Y` | Numeric destination coordinates |
| `Weather` | Weather string, including `Rainy` |
| `Time_of_Day` | Time string, including `Morning` |
| `Travel_Time_Minutes` | Numeric historical time; `NA` or `-1` can mark closures |

The fixed locations are P1 (60, 200), P2 (180, 200), P3 (80, 180),
P4 (140, 180), P5 (20, 160), P6 (100, 160), P7 (200, 160), P8 (140, 140),
P9 (40, 120), and P10 (100, 120).

Synthetic development data must be identified as synthetic and kept separate
from any supplied or grading data.

The Q2(a) prototype's synthetic development CSV is
`data/synthetic/q2/ahmad_travel_logs.csv`. It contains eight records for each
directed route under all combinations of Clear, Rainy, and Storm weather and
Morning, Afternoon, and Evening. P3 to P8 is intentionally blocked during Rainy
Morning: its eight records contain only `NA` and `-1`, so the prototype assigns
that route the severe penalty. It also includes one Rainy Morning record with
an unlisted origin coordinate to demonstrate that such rows are removed before
aggregation. This is a demonstration case, not a supplied or grading dataset.

The prototype writes its cleaned synthetic rows to
`data/processed/q2/ahmad_travel_logs_filtered.csv`. This file contains only
Rainy Morning records with listed endpoints and valid, non-negative travel
times; generated processed data is ignored by Git.

## Derived data

Use `processed/q1/` and `processed/q2/` for cleaned or aggregated datasets.
These folders are initially empty and their generated contents are ignored by
Git. Use `outputs/` for matrices, schedules, routes, and visual results.
Keep report figures under `outputs/q1/` or `outputs/q2/` and insert the selected
figures into the shared Google Docs report linked from the root README.
