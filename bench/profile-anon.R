#!/usr/bin/env Rscript

source("bench/utils.R")
load_dev_package(".")

inputs <- make_bench_text()

prof_file <- tempfile(fileext = ".out")

Rprof(prof_file, interval = 0.01)
invisible(
  anon(
    inputs$data,
    pattern_list = inputs$patterns,
    check_approximate = TRUE
  )
)
Rprof(NULL)

profile <- summaryRprof(prof_file)

cat("-- PROFILE BY SELF --\n")
print(utils::head(profile$by.self, 10))

cat("\n-- PROFILE BY TOTAL --\n")
print(utils::head(profile$by.total, 10))
