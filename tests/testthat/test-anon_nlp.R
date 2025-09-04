test_that("anon_nlp functions work with basic text", {
  skip_if_not_installed("NLP")
  skip_if_not_installed("openNLP")
  skip_if_not_installed("openNLPmodels.en")
  
  text <- c("John Smith works at Microsoft in Seattle.", 
            "The deal was worth $1.2 million in 2023.")
  
  # Test person anonymization
  result_people <- anon_nlp_people(text)
  expect_s3_class(result_people, "anon_context")
  expect_true(is.character(as.character(result_people)))
  
  # Test organization anonymization  
  result_orgs <- anon_nlp_organizations(text)
  expect_s3_class(result_orgs, "anon_context")
  expect_true(is.character(as.character(result_orgs)))
  
  # Test location anonymization
  result_locations <- anon_nlp_locations(text)
  expect_s3_class(result_locations, "anon_context")
  expect_true(is.character(as.character(result_locations)))
  
  # Test money anonymization
  result_money <- anon_nlp_money(text)
  expect_s3_class(result_money, "anon_context")
  expect_true(is.character(as.character(result_money)))
})

test_that("anon_nlp functions handle empty input gracefully", {
  # Test with empty character vector
  result <- anon_nlp_people(character(0))
  expect_s3_class(result, "anon_context")
  expect_equal(length(result), 0)
  
  # Test with NA values
  result_na <- anon_nlp_organizations(c(NA, "", NA))
  expect_s3_class(result_na, "anon_context")
  expect_equal(length(result_na), 3)
})

test_that("anon_nlp functions accept additional arguments", {
  skip_if_not_installed("NLP")
  skip_if_not_installed("openNLP") 
  skip_if_not_installed("openNLPmodels.en")
  
  text <- "John Smith works at Microsoft."
  
  # Test with custom default_replacement
  result <- anon_nlp_people(text, default_replacement = "[PERSON]")
  expect_s3_class(result, "anon_context")
  
  # Test with check_approximate disabled
  result2 <- anon_nlp_organizations(text, check_approximate = FALSE)
  expect_s3_class(result2, "anon_context")
})
