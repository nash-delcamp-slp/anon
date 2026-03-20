#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
source("bench/utils.R")

history_path <- parse_flag(args, "--history", "bench/results/history.csv")
site_dir <- parse_flag(args, "--site-dir", "bench/site")

dir.create(site_dir, recursive = TRUE, showWarnings = FALSE)
history <- read_history(history_path)

if (nrow(history) == 0) {
  writeLines(
    c(
      "<!doctype html>",
      "<html><head><meta charset='utf-8'><title>Benchmark History</title></head>",
      "<body><h1>Benchmark History</h1><p>No benchmark history recorded yet.</p></body></html>"
    ),
    file.path(site_dir, "index.html")
  )
  quit(save = "no")
}

history$timestamp_posix <- as.POSIXct(history$timestamp_utc, tz = "UTC")
history <- history[order(history$timestamp_posix, history$expression), ]
run_order <- unique(history$run_id)
latest_run_id <- tail(run_order, 1)
previous_run_id <- if (length(run_order) > 1) run_order[[length(run_order) - 1]] else NA_character_

latest <- history[history$run_id == latest_run_id, ]
if (is.na(previous_run_id)) {
  previous <- data.frame(expression = character(), previous_median_sec = numeric(), stringsAsFactors = FALSE)
} else {
  previous <- history[history$run_id == previous_run_id, c("expression", "median_sec")]
  colnames(previous)[2] <- "previous_median_sec"
}
latest <- merge(latest, previous, by = "expression", all.x = TRUE, sort = FALSE)
latest$delta_vs_previous <- mapply(format_delta_pct, latest$median_sec, latest$previous_median_sec)
latest$median_display <- format_metric(latest$median_sec, digits = 3, suffix = " s")
latest$itr_display <- format_metric(latest$itr_sec, digits = 3)
latest$mem_display <- format_metric(latest$mem_alloc_bytes / 1024^2, digits = 2, suffix = " MB")
latest$gc_display <- format_metric(latest$gc_sec, digits = 3)

recent_runs <- unique(history[order(history$timestamp_posix, decreasing = TRUE), c(
  "run_id", "timestamp_utc", "run_context", "machine_label", "git_sha",
  "git_ref", "r_version", "system", "system_release", "cpu_model"
)])
recent_runs <- head(recent_runs, 20)

latest_meta <- latest[1, c(
  "run_id", "timestamp_utc", "run_context", "machine_label", "git_sha",
  "git_ref", "r_version", "system", "system_release", "cpu_model"
)]

make_table_rows <- function(df, columns) {
  apply(df[, columns, drop = FALSE], 1, function(row) {
    cells <- paste0("<td>", html_escape(as.character(row)), "</td>", collapse = "")
    paste0("<tr>", cells, "</tr>")
  })
}

latest_rows <- make_table_rows(
  latest[, c("expression", "median_display", "itr_display", "mem_display", "gc_display", "delta_vs_previous")],
  c("expression", "median_display", "itr_display", "mem_display", "gc_display", "delta_vs_previous")
)

recent_metric_rows <- make_table_rows(
  tail(history[, c("timestamp_utc", "machine_label", "expression", "median_sec", "itr_sec")], 40),
  c("timestamp_utc", "machine_label", "expression", "median_sec", "itr_sec")
)

recent_run_rows <- make_table_rows(
  recent_runs[, c("timestamp_utc", "machine_label", "git_ref", "git_sha", "r_version", "cpu_model")],
  c("timestamp_utc", "machine_label", "git_ref", "git_sha", "r_version", "cpu_model")
)

html <- c(
  "<!doctype html>",
  "<html>",
  "<head>",
  "<meta charset='utf-8'>",
  "<meta name='viewport' content='width=device-width, initial-scale=1'>",
  "<title>anon Benchmark History</title>",
  "<style>",
  "body{font-family:ui-sans-serif,system-ui,sans-serif;margin:2rem auto;max-width:1100px;padding:0 1rem;color:#1f2937;background:#f8fafc;}",
  "h1,h2{color:#0f172a;} .card{background:white;border:1px solid #dbe3ec;border-radius:12px;padding:1rem 1.25rem;margin:1rem 0;box-shadow:0 1px 2px rgba(15,23,42,.04);}",
  "table{width:100%;border-collapse:collapse;font-size:.95rem;} th,td{padding:.55rem .65rem;border-bottom:1px solid #e5e7eb;text-align:left;vertical-align:top;} th{background:#f1f5f9;font-weight:600;} code{background:#eef2ff;padding:.15rem .3rem;border-radius:4px;} .muted{color:#64748b;} .grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(240px,1fr));gap:.75rem;} .stat{background:#f8fafc;border:1px solid #e2e8f0;border-radius:10px;padding:.75rem;} ",
  "</style>",
  "</head>",
  "<body>",
  "<h1>anon Benchmark History</h1>",
  "<p class='muted'>Non-blocking benchmark history for manual review over time.</p>",
  "<div class='card'><div class='grid'>",
  paste0("<div class='stat'><strong>Latest run</strong><br><code>", html_escape(latest_meta$run_id), "</code></div>"),
  paste0("<div class='stat'><strong>Timestamp</strong><br>", html_escape(latest_meta$timestamp_utc), "</div>"),
  paste0("<div class='stat'><strong>Machine</strong><br>", html_escape(latest_meta$machine_label), "</div>"),
  paste0("<div class='stat'><strong>Git</strong><br>", html_escape(paste(latest_meta$git_ref, latest_meta$git_sha)), "</div>"),
  paste0("<div class='stat'><strong>R</strong><br>", html_escape(latest_meta$r_version), " on ", html_escape(paste(latest_meta$system, latest_meta$system_release)), "</div>"),
  paste0("<div class='stat'><strong>CPU</strong><br>", html_escape(latest_meta$cpu_model), "</div>"),
  "</div></div>",
  "<div class='card'><h2>Latest Results</h2><table><thead><tr><th>Benchmark</th><th>Median</th><th>Itr/sec</th><th>Mem alloc</th><th>GC/sec</th><th>Delta vs previous</th></tr></thead><tbody>",
  latest_rows,
  "</tbody></table></div>",
  "<div class='card'><h2>Recent Runs</h2><table><thead><tr><th>Timestamp</th><th>Machine</th><th>Ref</th><th>SHA</th><th>R</th><th>CPU</th></tr></thead><tbody>",
  recent_run_rows,
  "</tbody></table></div>",
  "<div class='card'><h2>Recent Measurements</h2><table><thead><tr><th>Timestamp</th><th>Machine</th><th>Benchmark</th><th>Median (sec)</th><th>Itr/sec</th></tr></thead><tbody>",
  recent_metric_rows,
  "</tbody></table></div>",
  "<p class='muted'>Raw CSV: <a href='history.csv'>history.csv</a></p>",
  "</body></html>"
)

writeLines(html, file.path(site_dir, "index.html"))
utils::write.csv(history[, setdiff(names(history), "timestamp_posix")], file.path(site_dir, "history.csv"), row.names = FALSE)
utils::write.csv(latest[, setdiff(names(latest), c("timestamp_posix", "previous_median_sec", "median_display", "itr_display", "mem_display", "gc_display", "delta_vs_previous"))], file.path(site_dir, "latest.csv"), row.names = FALSE)
