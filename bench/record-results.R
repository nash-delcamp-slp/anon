#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
source("bench/utils.R")
load_dev_package(".")

history_path <- parse_flag(args, "--history", "bench/results/history.csv")
latest_path <- parse_flag(args, "--latest", "bench/results/latest.csv")
iterations <- as.integer(parse_flag(args, "--iterations", "3"))

latest <- collect_benchmark_results(iterations = iterations)
write_history(latest, latest_path)
combined <- append_history(history_path, latest)

print(latest[, c("expression", "median_sec", "itr_sec", "mem_alloc_bytes", "gc_sec")])
cat("\nWrote latest results to ", latest_path, sep = "")
cat("\nUpdated history at ", history_path, sep = "")
cat("\nTotal recorded rows: ", nrow(combined), "\n", sep = "")
