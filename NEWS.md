# anon 0.0.0.9000

## Bug fixes

* `anon()` replaces all patterns in a single pass. Previously each replacement
  was held as a hexadecimal token until the end, and later patterns (e.g. NLP
  date fragments or `"\\S+"`) could match inside those tokens, leaving
  output such as `df[DATE][DATE]f7c[DATE]7[DATE]4`.
* `df_classes = "double"` now matches plain double columns. It previously
  matched nothing because `class()` of a double vector is `"numeric"`.

## NLP

* NLP entities are matched only as whole values: an entity is not replaced
  when it sits next to a letter, a digit, or a decimal separator followed or
  preceded by a digit. Spaces in an entity match any run of whitespace, no
  longer any character.
* Only name-like entity types (`PERSON`, `ORG`, `NORP`, `FAC`, `GPE`, `LOC`,
  `PRODUCT`, `EVENT`, `WORK_OF_ART`, `PROPN`) also redact the entity's
  individual words, as whole words, excluding stopwords and number-only
  words. Dates, times and numbers are no longer split into fragments.
* `nlp_auto(date = TRUE)` and `nlp_auto(time = TRUE)` also redact ISO 8601
  dates (`YYYY-MM`, `YYYY-MM-DD`, optionally with time) and times (`HH:MM`,
  `HH:MM:SS`). A bare year is not redacted.
* Text in data frame columns replaced by `df_variable_names` or `df_classes`
  is no longer used to find NLP entities.
* NLP runs once per distinct string, in batches, with spaCy's dependency
  parser and lemmatizer disabled. Values without letters or digits, and
  hexadecimal identifiers (8+ characters of `0-9a-f` with at least one digit
  and one letter), are not sent to spaCy.

## Data frames

* `df_classes` matches character columns as `"double"`/`"numeric"`,
  `"integer"`, `"Date"` or `"POSIXct"` when every non-missing, non-blank value
  parses as that type. Replacement functions receive the parsed values.

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
