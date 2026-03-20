#!/usr/bin/env Rscript

source("bench/utils.R")
load_dev_package(".")

results <- run_benchmarks()
print(results[, c("expression", "median", "itr/sec", "mem_alloc", "gc/sec")])
