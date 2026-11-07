# Changelog

## anon 0.0.0.9000

- Pattern-based anonymization with custom replacement rules and global
  options via
  [`anon()`](https://nash-delcamp-slp.github.io/anon/reference/anon.md).
- NLP-powered entity detection that automatically identifies and redacts
  sensitive entities without manual pattern definitions.
- Specialized anonymization helpers for common data types: ID sequences,
  date shifting, emails, phone numbers, and distribution-preserving
  numeric anonymization.
- Clinical de-identification:
  [`anon_risk()`](https://nash-delcamp-slp.github.io/anon/reference/anon_risk.md)
  measures re-identification risk, and
  [`anon_subject_ids()`](https://nash-delcamp-slp.github.io/anon/reference/anon_subject_ids.md)
  and
  [`anon_subject_dates()`](https://nash-delcamp-slp.github.io/anon/reference/anon_subject_dates.md)
  pseudonymize subjects and offset each subject’s dates consistently
  across datasets.
- Environment summarization with
  [`anon_report()`](https://nash-delcamp-slp.github.io/anon/reference/anon_report.md)
  and
  [`anon_data_summary()`](https://nash-delcamp-slp.github.io/anon/reference/anon_data_summary.md)
  to generate safe structural overviews suitable for sharing in prompts
  or bug reports.
- Interactive Shiny app via
  [`run_anon_app()`](https://nash-delcamp-slp.github.io/anon/reference/run_anon_app.md)
  for guided anonymization workflows, pattern preview, and prompt
  building.
