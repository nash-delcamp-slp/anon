# anon 0.0.0.9000

* Pattern-based anonymization with custom replacement rules and global options
  via `anon()`.
* NLP-powered entity detection that automatically identifies and redacts
  sensitive entities without manual pattern definitions.
* Specialized anonymization helpers for common data types: ID sequences, date
  shifting, emails, phone numbers, and distribution-preserving numeric
  anonymization.
* Environment summarization with `anon_report()` and `anon_data_summary()` to
  generate safe structural overviews suitable for sharing in prompts or bug
  reports.
* Interactive Shiny app via `run_anon_app()` for guided anonymization
  workflows, pattern preview, and prompt building.
