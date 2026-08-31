## Weekly metrics for the group's packages and papers.
##
## Appends one row per package and per paper to two CSVs under _data/. The
## history is the point: these numbers cannot be reconstructed later, so the
## job records them as it goes.

source("_automation/get_stats.R")

`%||%` <- function(x, y) if (is.null(x)) y else x

today <- Sys.Date()

## Papers first: the two halves share no data, and collecting citations
## before touching the package registries means a registry problem cannot
## cost a week of citations as well.
message("Collecting paper citations")
papers <- collect_paper_citations(today)
message(sprintf("  %d papers", nrow(papers)))
append_stats(papers, "_data/paper-citations.csv", key = "doi")

message("Collecting package metrics")
packages <- collect_package_stats(today)
message(sprintf("  %d packages", nrow(packages)))
append_stats(packages, "_data/package-stats.csv", key = "package")
