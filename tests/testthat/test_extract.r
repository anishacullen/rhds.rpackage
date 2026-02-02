library(testthat)

test_that("extract.participant extracts participant ID correctly", {
  # Test basic TCGA ID format
  expect_equal(extract.participant("TCGA-AB-1234-01A"), "1234")
  
  # Test with different project code
  expect_equal(extract.participant("TCGA-XY-5678-11B"), "5678")
  
  # Test with longer suffix
  expect_equal(extract.participant("TCGA-CD-9999-01A-01D-1234-08"), "9999")
  
  # Test with minimal valid format
  expect_equal(extract.participant("TCGA-AB-123-01"), "123")
  
  # Test that non-matching format returns original string
  expect_equal(extract.participant("invalid-id"), "invalid-id")
})