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
recent_runs$git_sha_short <- substr(recent_runs$git_sha, 1, 7)

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
  recent_runs[, c("timestamp_utc", "machine_label", "git_ref", "git_sha_short", "r_version", "cpu_model")],
  c("timestamp_utc", "machine_label", "git_ref", "git_sha_short", "r_version", "cpu_model")
)

# -- ggplot history chart -----------------------------------------------------
chart_html <- ""
if (requireNamespace("ggplot2", quietly = TRUE)) {
  plot_data <- history[, c("timestamp_posix", "expression", "median_sec")]
  plot_data$expression <- sub("^anon_", "", plot_data$expression)

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(
    x = timestamp_posix, y = median_sec,
    colour = expression, group = expression
  )) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::scale_colour_brewer(palette = "Set2") +
    ggplot2::labs(
      x = NULL, y = "Median time (seconds)",
      colour = "Benchmark"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA)
    )

  chart_path <- file.path(site_dir, "history.png")
  ggplot2::ggsave(chart_path, p, width = 9, height = 4.5, dpi = 150, bg = "white")
  chart_html <- paste0(
    "<div class='card'><h2>Performance Over Time</h2>",
    "<img src='history.png' alt='Benchmark history chart' style='width:100%;height:auto;border-radius:8px;'>",
    "</div>"
  )
}

html <- c(
  "<!doctype html>",
  "<html>",
  "<head>",
  "<meta charset='utf-8'>",
  "<meta name='viewport' content='width=device-width, initial-scale=1'>",
  "<title>anon Benchmark History</title>",
  "<style>",
  paste0(
    "body{font-family:ui-sans-serif,system-ui,sans-serif;margin:2rem auto;max-width:1100px;padding:0 1rem;color:#1f2937;background:#f8fafc;}",
    "h1,h2{color:#0f172a;}",
    ".card{background:white;border:1px solid #dbe3ec;border-radius:12px;padding:1rem 1.25rem;margin:1rem 0;box-shadow:0 1px 2px rgba(15,23,42,.04);}",
    "table{width:100%;border-collapse:collapse;font-size:.95rem;}",
    "th,td{padding:.55rem .65rem;border-bottom:1px solid #e5e7eb;text-align:left;vertical-align:top;overflow-wrap:anywhere;word-break:break-all;}",
    "th{background:#f1f5f9;font-weight:600;}",
    "code{background:#eef2ff;padding:.15rem .3rem;border-radius:4px;word-break:break-all;}",
    ".muted{color:#64748b;}",
    ".grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(240px,1fr));gap:.75rem;}",
    ".stat{background:#f8fafc;border:1px solid #e2e8f0;border-radius:10px;padding:.75rem;overflow-wrap:anywhere;word-break:break-word;}"
  ),
  "</style>",
  "</head>",
  "<body>",
  "<h1>anon Benchmark History</h1>",
  "<p class='muted'>Non-blocking benchmark history for manual review over time.</p>",
  "<div class='card'><div class='grid'>",
  paste0("<div class='stat'><strong>Latest run</strong><br><code>", html_escape(latest_meta$run_id), "</code></div>"),
  paste0("<div class='stat'><strong>Timestamp</strong><br>", html_escape(latest_meta$timestamp_utc), "</div>"),
  paste0("<div class='stat'><strong>Machine</strong><br>", html_escape(latest_meta$machine_label), "</div>"),
  paste0("<div class='stat'><strong>Git</strong><br><code>", html_escape(latest_meta$git_ref), "</code> <code>", html_escape(substr(latest_meta$git_sha, 1, 7)), "</code></div>"),
  paste0("<div class='stat'><strong>R</strong><br>", html_escape(latest_meta$r_version), " on ", html_escape(paste(latest_meta$system, latest_meta$system_release)), "</div>"),
  paste0("<div class='stat'><strong>CPU</strong><br>", html_escape(latest_meta$cpu_model), "</div>"),
  "</div></div>",
  chart_html,
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
