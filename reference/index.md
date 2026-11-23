# Package index

## Redact and replace

Redact sensitive text in vectors, data frames, lists and labels using
patterns, column rules and NLP, and set package-wide defaults.

- [`anon()`](https://nash-delcamp-slp.github.io/anon/reference/anon.md)
  : Redact and replace sensitive information in R objects
- [`more_patterns()`](https://nash-delcamp-slp.github.io/anon/reference/more_patterns.md)
  : Create expanded pattern vector for text matching
- [`anon_options()`](https://nash-delcamp-slp.github.io/anon/reference/anon_options.md)
  : Get or set package options used by anon

## Techniques for specific data types

Pseudonymize identifiers and contact details, shift dates, and
generalize, shuffle or add noise to numbers while keeping their
structure useful.

### Identifiers

- [`anon_id_chr_sequence()`](https://nash-delcamp-slp.github.io/anon/reference/anon_id_chr_sequence.md)
  : Pseudonymize identifiers as sequential character IDs
- [`anon_id_num_sequence()`](https://nash-delcamp-slp.github.io/anon/reference/anon_id_num_sequence.md)
  : Pseudonymize identifiers as sequential numeric IDs

### Dates

- [`anon_date_shift()`](https://nash-delcamp-slp.github.io/anon/reference/anon_date_shift.md)
  : Shift dates to a new time period

### Contact details

- [`anon_email()`](https://nash-delcamp-slp.github.io/anon/reference/anon_email.md)
  : Replace email addresses with pseudonymous addresses
- [`anon_phone_number()`](https://nash-delcamp-slp.github.io/anon/reference/anon_phone_number.md)
  : Replace phone numbers with pseudonymous numbers

### Numbers

- [`anon_num_preserve_distribution()`](https://nash-delcamp-slp.github.io/anon/reference/anon_num_preserve_distribution.md)
  : Shuffle, add noise to, or resample numeric values
- [`anon_num_range()`](https://nash-delcamp-slp.github.io/anon/reference/anon_num_range.md)
  : Generalize numeric values into range categories

## De-identification

Measure re-identification risk from quasi-identifiers across linked
datasets, pseudonymize subjects and offset their dates consistently
across datasets, and generalize or suppress quasi-identifiers to bring
the risk under a target, step by step or in one call with an SDTM or
ADaM profile.

- [`anon_deidentify()`](https://nash-delcamp-slp.github.io/anon/reference/anon_deidentify.md)
  : De-identify linked datasets with a profile
- [`anon_check_fidelity()`](https://nash-delcamp-slp.github.io/anon/reference/anon_check_fidelity.md)
  : Check that code tested on a de-identified copy should run on the
  original
- [`anon_profile()`](https://nash-delcamp-slp.github.io/anon/reference/anon_profile.md)
  [`anon_profile_rule()`](https://nash-delcamp-slp.github.io/anon/reference/anon_profile.md)
  [`anon_profile_sdtm()`](https://nash-delcamp-slp.github.io/anon/reference/anon_profile.md)
  [`anon_profile_adam()`](https://nash-delcamp-slp.github.io/anon/reference/anon_profile.md)
  [`anon_profile_combine()`](https://nash-delcamp-slp.github.io/anon/reference/anon_profile.md)
  [`c(`*`<anon_profile>`*`)`](https://nash-delcamp-slp.github.io/anon/reference/anon_profile.md)
  [`as.data.frame(`*`<anon_profile>`*`)`](https://nash-delcamp-slp.github.io/anon/reference/anon_profile.md)
  : De-identification profiles
- [`anon_risk()`](https://nash-delcamp-slp.github.io/anon/reference/anon_risk.md)
  : Measure re-identification risk from quasi-identifiers
- [`anon_risk_subjects()`](https://nash-delcamp-slp.github.io/anon/reference/anon_risk_subjects.md)
  : Per-subject re-identification risk
- [`anon_quasi_values()`](https://nash-delcamp-slp.github.io/anon/reference/anon_quasi_values.md)
  : Specify quasi-identifier values that vary by row
- [`anon_quasi_duration()`](https://nash-delcamp-slp.github.io/anon/reference/anon_quasi_duration.md)
  : Specify a duration between two dates as a quasi-identifier
- [`anon_subject_ids()`](https://nash-delcamp-slp.github.io/anon/reference/anon_subject_ids.md)
  : Pseudonymize subject identifiers consistently across datasets
- [`anon_subject_dates()`](https://nash-delcamp-slp.github.io/anon/reference/anon_subject_dates.md)
  : Shift each subject's dates by a random offset
- [`anon_generalize()`](https://nash-delcamp-slp.github.io/anon/reference/anon_generalize.md)
  : Generalize quasi-identifiers across datasets
- [`anon_band()`](https://nash-delcamp-slp.github.io/anon/reference/generalize_rules.md)
  [`anon_round()`](https://nash-delcamp-slp.github.io/anon/reference/generalize_rules.md)
  [`anon_fold()`](https://nash-delcamp-slp.github.io/anon/reference/generalize_rules.md)
  : Rules that generalize quasi-identifier values
- [`anon_rule()`](https://nash-delcamp-slp.github.io/anon/reference/anon_rule.md)
  : Generalize the results of a test
- [`anon_suppress()`](https://nash-delcamp-slp.github.io/anon/reference/anon_suppress.md)
  : Suppress quasi-identifiers until every subject reaches the target
- [`anon_secrets()`](https://nash-delcamp-slp.github.io/anon/reference/anon_secrets.md)
  : Read the secrets kept by a de-identification step

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
  : Redact Named Entities in Text Using NLP
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
  : Generate NLP auto-redaction options list
- [`nlp_default_replacements()`](https://nash-delcamp-slp.github.io/anon/reference/nlp_default_replacements.md)
  : Generate default replacement options list for NLP entities

## Summarize data for sharing

Describe the objects in an environment or list without exposing
sensitive values.

- [`anon_inventory()`](https://nash-delcamp-slp.github.io/anon/reference/anon_inventory.md)
  : Build an inventory of objects, applying your redaction rules

- [`anon_data_summary()`](https://nash-delcamp-slp.github.io/anon/reference/anon_data_summary.md)
  : Summarize data objects in an environment, applying your redaction
  rules

- [`anon_example_rows()`](https://nash-delcamp-slp.github.io/anon/reference/anon_example_rows.md)
  :

  Build an example-row specification for
  [`anon_data_summary()`](https://nash-delcamp-slp.github.io/anon/reference/anon_data_summary.md)

- [`anon_report()`](https://nash-delcamp-slp.github.io/anon/reference/anon_report.md)
  : Build a report for selected objects, applying your redaction rules

## Prepare text and prompts

Import documents, clean and compare text, and assemble redacted context
into a prompt bundle.

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
  : Build prompt-ready text from report objects

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
  : Print method for environment data summaries
