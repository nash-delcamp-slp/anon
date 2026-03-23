# Changelog

## anon 0.0.0.9000

- Pattern-based anonymization with custom replacement rules and global
  options via [`anon()`](../reference/anon.md).
- NLP-powered entity detection that automatically identifies and redacts
  sensitive entities without manual pattern definitions.
- Specialized anonymization helpers for common data types: ID sequences,
  date shifting, emails, phone numbers, and distribution-preserving
  numeric anonymization.
- Environment summarization with
  [`anon_report()`](../reference/anon_report.md) and
  [`anon_data_summary()`](../reference/anon_data_summary.md) to generate
  safe structural overviews suitable for sharing in prompts or bug
  reports.
- Interactive Shiny app via
  [`run_anon_app()`](../reference/run_anon_app.md) for guided
  anonymization workflows, pattern preview, and prompt building.
