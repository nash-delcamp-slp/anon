# Package index

## Anonymize objects

Redact sensitive text in vectors, data frames, lists and labels using
patterns, column rules and NLP, and set package-wide defaults.

- [`anon()`](https://nash-delcamp-slp.github.io/anon/reference/anon.md)
  : Anonymize sensitive information in R objects
- [`more_patterns()`](https://nash-delcamp-slp.github.io/anon/reference/more_patterns.md)
  : Create expanded pattern vector for text matching
- [`anon_options()`](https://nash-delcamp-slp.github.io/anon/reference/anon_options.md)
  : Get or set package options used by anon

## Anonymize specific data types

Replace or transform identifiers, dates, contact details and numbers
while keeping their structure useful.

### Identifiers

- [`anon_id_chr_sequence()`](https://nash-delcamp-slp.github.io/anon/reference/anon_id_chr_sequence.md)
  : Convert unique identifiers to sequential character IDs
- [`anon_id_num_sequence()`](https://nash-delcamp-slp.github.io/anon/reference/anon_id_num_sequence.md)
  : Convert unique identifiers to sequential numeric IDs

### Dates

- [`anon_date_shift()`](https://nash-delcamp-slp.github.io/anon/reference/anon_date_shift.md)
  : Shift dates to a new time period

### Contact details

- [`anon_email()`](https://nash-delcamp-slp.github.io/anon/reference/anon_email.md)
  : Anonymize email addresses
- [`anon_phone_number()`](https://nash-delcamp-slp.github.io/anon/reference/anon_phone_number.md)
  : Anonymize phone numbers

### Numbers

- [`anon_num_preserve_distribution()`](https://nash-delcamp-slp.github.io/anon/reference/anon_num_preserve_distribution.md)
  : Anonymize numeric data while preserving distribution
- [`anon_num_range()`](https://nash-delcamp-slp.github.io/anon/reference/anon_num_range.md)
  : Convert numeric values into range categories

## Named entity recognition

Find and redact people, organizations, places, dates and other entities
with spaCy, and configure which entity types
[`anon()`](https://nash-delcamp-slp.github.io/anon/reference/anon.md)
redacts automatically.

- [`anon_nlp_entities()`](https://nash-delcamp-slp.github.io/anon/reference/anon_nlp_entities.md)
  [`anon_nlp_proper_nouns()`](https://nash-delcamp-slp.github.io/anon/reference/anon_nlp_entities.md)
  [`anon_nlp_dates()`](https://nash-delcamp-slp.github.io/anon/reference/anon_nlp_entities.md)
  [`anon_nlp_dates_and_times()`](https://nash-delcamp-slp.github.io/anon/reference/anon_nlp_entities.md)
  [`anon_nlp_named()`](https://nash-delcamp-slp.github.io/anon/reference/anon_nlp_entities.md)
  [`anon_nlp_numbers()`](https://nash-delcamp-slp.github.io/anon/reference/anon_nlp_entities.md)
  [`anon_nlp_organizations()`](https://nash-delcamp-slp.github.io/anon/reference/anon_nlp_entities.md)
  [`anon_nlp_people()`](https://nash-delcamp-slp.github.io/anon/reference/anon_nlp_entities.md)
  [`anon_nlp_places()`](https://nash-delcamp-slp.github.io/anon/reference/anon_nlp_entities.md)
  : Anonymize Named Entities in Text Using NLP
- [`nlp_get_entities()`](https://nash-delcamp-slp.github.io/anon/reference/nlp_get_entities.md)
  [`nlp_get_dates()`](https://nash-delcamp-slp.github.io/anon/reference/nlp_get_entities.md)
  [`nlp_get_dates_and_times()`](https://nash-delcamp-slp.github.io/anon/reference/nlp_get_entities.md)
  [`nlp_get_named()`](https://nash-delcamp-slp.github.io/anon/reference/nlp_get_entities.md)
  [`nlp_get_numbers()`](https://nash-delcamp-slp.github.io/anon/reference/nlp_get_entities.md)
  [`nlp_get_organizations()`](https://nash-delcamp-slp.github.io/anon/reference/nlp_get_entities.md)
  [`nlp_get_people()`](https://nash-delcamp-slp.github.io/anon/reference/nlp_get_entities.md)
  [`nlp_get_places()`](https://nash-delcamp-slp.github.io/anon/reference/nlp_get_entities.md)
  [`nlp_get_proper_nouns()`](https://nash-delcamp-slp.github.io/anon/reference/nlp_get_entities.md)
  : Extract Named Entities from Text
- [`nlp_entity_sets`](https://nash-delcamp-slp.github.io/anon/reference/nlp_entity_sets.md)
  : Named Entity Type Collections
- [`nlp_auto()`](https://nash-delcamp-slp.github.io/anon/reference/nlp_auto.md)
  : Generate NLP auto-anonymization options list
- [`nlp_default_replacements()`](https://nash-delcamp-slp.github.io/anon/reference/nlp_default_replacements.md)
  : Generate default replacement options list for NLP entities

## Summarize data for sharing

Describe the objects in an environment or list without exposing
sensitive values.

- [`anon_inventory()`](https://nash-delcamp-slp.github.io/anon/reference/anon_inventory.md)
  : Build an anonymized inventory of objects in an environment or list

- [`anon_data_summary()`](https://nash-delcamp-slp.github.io/anon/reference/anon_data_summary.md)
  : Generate anonymized summary of data objects in an environment

- [`anon_example_rows()`](https://nash-delcamp-slp.github.io/anon/reference/anon_example_rows.md)
  :

  Build an example-row specification for
  [`anon_data_summary()`](https://nash-delcamp-slp.github.io/anon/reference/anon_data_summary.md)

- [`anon_report()`](https://nash-delcamp-slp.github.io/anon/reference/anon_report.md)
  : Build an anonymized report for selected objects

## Prepare text and prompts

Import documents, clean and compare text, and assemble anonymized
context into a prompt bundle.

- [`read_content()`](https://nash-delcamp-slp.github.io/anon/reference/read_content.md)
  [`read_content_docx()`](https://nash-delcamp-slp.github.io/anon/reference/read_content.md)
  [`read_content_pptx()`](https://nash-delcamp-slp.github.io/anon/reference/read_content.md)
  [`read_content_xlsx()`](https://nash-delcamp-slp.github.io/anon/reference/read_content.md)
  [`read_content_pdf()`](https://nash-delcamp-slp.github.io/anon/reference/read_content.md)
  [`read_content_text()`](https://nash-delcamp-slp.github.io/anon/reference/read_content.md)
  : Read content from files as text
- [`anon_clean_text()`](https://nash-delcamp-slp.github.io/anon/reference/anon_clean_text.md)
  : Clean text for safer prompt input
- [`anon_compare_text()`](https://nash-delcamp-slp.github.io/anon/reference/anon_compare_text.md)
  : Compare text before and after cleaning or redaction
- [`anon_prompt_bundle()`](https://nash-delcamp-slp.github.io/anon/reference/anon_prompt_bundle.md)
  : Build prompt-ready text from anonymized report objects

## Export to JSON

- [`as_anon_json()`](https://nash-delcamp-slp.github.io/anon/reference/as_anon_json.md)
  : Serialize anon objects as JSON
- [`as_anon_payload()`](https://nash-delcamp-slp.github.io/anon/reference/as_anon_payload.md)
  : Build structured anon payloads

## Interactive app

- [`run_anon_app()`](https://nash-delcamp-slp.github.io/anon/reference/run_anon_app.md)
  :

  Launch the local `anon` Shiny app

## Results and printing

Combine and print the `anon_context` objects returned by
[`anon()`](https://nash-delcamp-slp.github.io/anon/reference/anon.md)
and the summary functions.

- [`c(`*`<anon_context>`*`)`](https://nash-delcamp-slp.github.io/anon/reference/c.anon_context.md)
  : Combine anon_context objects
- [`print(`*`<anon_context>`*`)`](https://nash-delcamp-slp.github.io/anon/reference/print.anon_context.md)
  : Print method for single anon_context objects
- [`print(`*`<anon_context_collection>`*`)`](https://nash-delcamp-slp.github.io/anon/reference/print.anon_context_collection.md)
  : Print method for anon_context_collection
- [`print(`*`<anon_data_summary>`*`)`](https://nash-delcamp-slp.github.io/anon/reference/print.anon_data_summary.md)
  : Print method for anonymized environment summary
