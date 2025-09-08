require_spacy <- function() {
  # Skip if cleanNLP is not available
  skip_if_not_installed("cleanNLP")
  skip_if_not_installed("reticulate")

  # Skip if spaCy backend is not available
  skip_if(
    reticulate::py_module_available("cleannlp"),
    "spaCy backend not available for cleanNLP"
  )

  # Initialize cleanNLP with spaCy backend
  suppressMessages(cleanNLP::cnlp_init_spacy())
}

test_that("anon_nlp functions work with cleanNLP and spaCy", {
  require_spacy()

  # Test text
  text <- c(
    "John Smith works at Microsoft in Seattle.",
    "The deal was worth $1.2 million in 2023.",
    "He was the first employee to make 100% of his 3rd quarter targets."
  )

  # Test anon_nlp_people
  result_people <- anon_nlp_people(text)
  expect_s3_class(result_people, "anon_context")
  expect_type(result_people$anonymized_text, "character")
  expect_length(result_people$anonymized_text, length(text))

  # Test anon_nlp_organizations
  result_orgs <- anon_nlp_organizations(text)
  expect_s3_class(result_orgs, "anon_context")
  expect_type(result_orgs$anonymized_text, "character")
  expect_length(result_orgs$anonymized_text, length(text))

  # Test anon_nlp_places
  result_places <- anon_nlp_places(text)
  expect_s3_class(result_places, "anon_context")
  expect_type(result_places$anonymized_text, "character")
  expect_length(result_places$anonymized_text, length(text))

  # Test anon_nlp_dates
  result_dates <- anon_nlp_dates(text)
  expect_s3_class(result_dates, "anon_context")
  expect_type(result_dates$anonymized_text, "character")
  expect_length(result_dates$anonymized_text, length(text))

  # Test anon_nlp_numbers
  result_numbers <- anon_nlp_numbers(text)
  expect_s3_class(result_numbers, "anon_context")
  expect_type(result_numbers$anonymized_text, "character")
  expect_length(result_numbers$anonymized_text, length(text))

  # Test anon_nlp_entities with specific entity types
  result_entities <- anon_nlp_entities(text, entity_types = c("PERSON", "ORG"))
  expect_s3_class(result_entities, "anon_context")
  expect_type(result_entities$anonymized_text, "character")
  expect_length(result_entities$anonymized_text, length(text))

  # Test anon_nlp_proper_nouns
  result_proper <- anon_nlp_proper_nouns(text)
  expect_s3_class(result_proper, "anon_context")
  expect_type(result_proper$anonymized_text, "character")
  expect_length(result_proper$anonymized_text, length(text))
})

test_that("anon_nlp functions handle custom replacements", {
  require_spacy()

  text <- "John Smith works at Microsoft."

  # Test custom replacement
  result <- anon_nlp_people(text, default_replacement = "[REDACTED_NAME]")
  expect_true(grepl("\\[REDACTED_NAME\\]", result$anonymized_text))

  # Test that original patterns are preserved
  expect_type(result$pattern_context, "list")
})

test_that("nlp_default_replacements() options system works correctly", {
  require_spacy()

  text <- "John Smith works at Microsoft in Seattle."

  # Store original options
  old_nlp_options <- getOption("anon.nlp_default_replacements")
  old_default_option <- getOption("anon.default_replacement")

  # Test with nlp_default_replacements option
  custom_replacements <- nlp_default_replacements(
    person = "[PERSON_REDACTED]",
    org = "[ORGANIZATION_REDACTED]",
    gpe = "[LOCATION_REDACTED]"
  )
  options(anon.nlp_default_replacements = custom_replacements)

  # Test person replacement
  result_person <- anon_nlp_people(text)
  expect_true(grepl("\\[PERSON_REDACTED\\]", result_person$anonymized_text))

  # Test organization replacement
  result_org <- anon_nlp_organizations(text)
  expect_true(grepl("\\[ORGANIZATION_REDACTED\\]", result_org$anonymized_text))

  # Test places replacement (GPE entities)
  result_places <- anon_nlp_places(text)
  expect_true(grepl("\\[LOCATION_REDACTED\\]", result_places$anonymized_text))

  # Test that entities function uses the same replacements
  result_entities <- anon_nlp_entities(
    text,
    entity_types = c("PERSON", "ORG", "GPE")
  )
  expect_true(grepl("\\[PERSON_REDACTED\\]", result_entities$anonymized_text))
  expect_true(grepl(
    "\\[ORGANIZATION_REDACTED\\]",
    result_entities$anonymized_text
  ))
  expect_true(grepl("\\[LOCATION_REDACTED\\]", result_entities$anonymized_text))

  # Restore original options
  options(anon.nlp_default_replacements = old_nlp_options)
  options(anon.default_replacement = old_default_option)
})

test_that("options hierarchy works correctly", {
  require_spacy()

  text <- "John Smith works at Microsoft."

  # Store original options
  old_nlp_options <- getOption("anon.nlp_default_replacements")
  old_default_option <- getOption("anon.default_replacement")

  # Test 1: Function argument takes priority over global options
  custom_replacements <- nlp_default_replacements(person = "[GLOBAL_PERSON]")
  options(anon.nlp_default_replacements = custom_replacements)

  result <- anon_nlp_people(text, default_replacement = "[ARG_PERSON]")
  expect_true(grepl("\\[ARG_PERSON\\]", result$anonymized_text))
  expect_false(grepl("\\[GLOBAL_PERSON\\]", result$anonymized_text))

  # Test 2: nlp_default_replacements takes priority over anon.default_replacement
  options(anon.default_replacement = "[FALLBACK]")
  options(anon.nlp_default_replacements = custom_replacements)

  result <- anon_nlp_people(text)
  expect_true(grepl("\\[GLOBAL_PERSON\\]", result$anonymized_text))
  expect_false(grepl("\\[FALLBACK\\]", result$anonymized_text))

  # Test 3: anon.default_replacement used when entity not in nlp_default_replacements
  incomplete_replacements <- nlp_default_replacements()
  incomplete_replacements$person <- NULL
  options(anon.nlp_default_replacements = incomplete_replacements)
  options(anon.default_replacement = "[FALLBACK]")

  result <- anon_nlp_people(text)
  expect_true(grepl("\\[FALLBACK\\]", result$anonymized_text))

  # Test 4: Built-in defaults used when no options set
  options(anon.nlp_default_replacements = NULL)
  options(anon.default_replacement = NULL)

  result <- anon_nlp_people(text)
  expect_true(grepl("\\[PERSON\\]", result$anonymized_text))

  # Restore original options
  options(anon.nlp_default_replacements = old_nlp_options)
  options(anon.default_replacement = old_default_option)
})

test_that("nlp_default_replacements() creates correct structure", {
  # Test default replacements
  defaults <- nlp_default_replacements()

  expect_type(defaults, "list")
  expect_named(defaults)

  # Check that all expected entity types are present
  expected_names <- c(
    "cardinal",
    "date",
    "event",
    "fac",
    "gpe",
    "language",
    "law",
    "loc",
    "money",
    "norp",
    "ordinal",
    "org",
    "percent",
    "person",
    "product",
    "quantity",
    "time",
    "work_of_art",
    "propn"
  )
  expect_setequal(names(defaults), expected_names)

  # Check default values
  expect_equal(defaults$person, "[PERSON]")
  expect_equal(defaults$org, "[ORG]")
  expect_equal(defaults$gpe, "[GPE]")
  expect_equal(defaults$propn, "[PROPN]")

  # Test custom replacements
  custom <- nlp_default_replacements(
    person = "[CUSTOM_PERSON]",
    org = "[CUSTOM_ORG]"
  )

  expect_equal(custom$person, "[CUSTOM_PERSON]")
  expect_equal(custom$org, "[CUSTOM_ORG]")
  expect_equal(custom$gpe, "[GPE]") # Should remain default
})

test_that("get_nlp_default_replacement() helper function works", {
  # Store original options
  old_nlp_options <- getOption("anon.nlp_default_replacements")
  old_default_option <- getOption("anon.default_replacement")

  # Test with nlp_default_replacements option set
  custom_replacements <- nlp_default_replacements(person = "[TEST_PERSON]")
  options(anon.nlp_default_replacements = custom_replacements)

  # Test case insensitive matching
  expect_equal(get_nlp_default_replacement("PERSON"), "[TEST_PERSON]")
  expect_equal(get_nlp_default_replacement("person"), "[TEST_PERSON]")
  expect_equal(get_nlp_default_replacement("Person"), "[TEST_PERSON]")

  # Test fallback to anon.default_replacement
  options(anon.nlp_default_replacements = NULL)
  options(anon.default_replacement = "[FALLBACK]")
  expect_equal(get_nlp_default_replacement("UNKNOWN_ENTITY"), "[FALLBACK]")

  # Test fallback to built-in defaults
  options(anon.default_replacement = NULL)
  expect_equal(get_nlp_default_replacement("PERSON"), "[PERSON]")
  expect_equal(get_nlp_default_replacement("ORG"), "[ORG]")

  # Test final fallback
  expect_equal(get_nlp_default_replacement("NONEXISTENT"), "[REDACTED]")

  # Restore original options
  options(anon.nlp_default_replacements = old_nlp_options)
  options(anon.default_replacement = old_default_option)
})

test_that("anon_nlp functions handle empty input", {
  require_spacy()

  # Test empty character vector
  result <- anon_nlp_people(character(0))
  expect_s3_class(result, "anon_context")
  expect_length(result$anonymized_text, 0)

  # Test text with no entities
  result <- anon_nlp_people("The quick brown fox.")
  expect_s3_class(result, "anon_context")
  expect_equal(result$anonymized_text, "The quick brown fox.")
})

test_that("anon_nlp functions validate input", {
  # Test non-character input
  expect_error(anon_nlp_people(123), "x must be a character vector")
  expect_error(
    anon_nlp_organizations(list("text")),
    "x must be a character vector"
  )
})

test_that("all anon_nlp functions exist and are callable", {
  # Test that all functions exist
  expect_true(exists("anon_nlp_entities"))
  expect_true(exists("anon_nlp_proper_nouns"))
  expect_true(exists("anon_nlp_dates"))
  expect_true(exists("anon_nlp_dates_and_times"))
  expect_true(exists("anon_nlp_people"))
  expect_true(exists("anon_nlp_organizations"))
  expect_true(exists("anon_nlp_places"))
  expect_true(exists("anon_nlp_numbers"))
  expect_true(exists("anon_nlp_named"))

  # Test that functions are callable (will fail if cleanNLP not available, but that's expected)
  expect_type(anon_nlp_entities, "closure")
  expect_type(anon_nlp_people, "closure")
  expect_type(anon_nlp_organizations, "closure")
})

test_that("anon_nlp_entities accepts entity_types parameter", {
  require_spacy()

  text <- "John Smith works at Microsoft in Seattle."

  # Test with specific entity types
  result1 <- anon_nlp_entities(text, entity_types = "PERSON")
  result2 <- anon_nlp_entities(text, entity_types = c("PERSON", "ORG"))

  expect_s3_class(result1, "anon_context")
  expect_s3_class(result2, "anon_context")
  expect_type(result1$anonymized_text, "character")
  expect_type(result2$anonymized_text, "character")
})
