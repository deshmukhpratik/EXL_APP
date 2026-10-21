library(testthat)
source("../../R/services/policy_extractor_service.R", local = .GlobalEnv)

test_that("extractor functions exist and are callable", {
  expect_true(is.function(extract_text_from_file))
  expect_true(is.function(extract_levers))
})

test_that("extract_text_from_file reads PDF text", {
  txt <- extract_text_from_file("fixtures/sample_policy.pdf", "pdf")
  expect_type(txt, "character")
  expect_length(txt, 1L)
  expect_match(txt, "FICO", ignore.case = TRUE)
  expect_match(txt, "DTI",  ignore.case = TRUE)
})

test_that("extract_text_from_file returns NULL on missing file", {
  expect_null(extract_text_from_file("does/not/exist.pdf", "pdf"))
})

test_that("extract_text_from_file returns NULL on file < 1 KB", {
  tmp <- tempfile(fileext = ".pdf")
  writeBin(raw(10), tmp)  # 10 bytes
  on.exit(unlink(tmp), add = TRUE)
  expect_null(extract_text_from_file(tmp, "pdf"))
})

test_that("extract_text_from_file returns NULL on unsupported extension", {
  tmp <- tempfile(fileext = ".xls")
  writeBin(raw(2000), tmp)
  on.exit(unlink(tmp), add = TRUE)
  expect_null(extract_text_from_file(tmp, "xls"))
})

test_that("extract_text_from_file normalizes Unicode minus to ASCII hyphen", {
  # R's pdf() device emits U+2212 MINUS SIGN for ASCII '-'.
  # Downstream regex tests rely on ASCII hyphens (e.g., "Auto-approve",
  # "700-740"), so the extractor must normalize at the boundary.
  txt <- extract_text_from_file("fixtures/sample_policy.pdf", "pdf")
  expect_false(grepl("−", txt))    # no Unicode minus remains
  expect_match(txt, "Auto-approve", fixed = TRUE)
  expect_match(txt, "700-740",      fixed = TRUE)
})

test_that("extract_text_from_file reads DOCX text", {
  txt <- extract_text_from_file("fixtures/sample_policy.docx", "docx")
  expect_type(txt, "character")
  expect_match(txt, "FICO", ignore.case = TRUE)
  expect_match(txt, "DTI",  ignore.case = TRUE)
})

test_that("extract_text_from_file returns NULL on broken DOCX (not a zip)", {
  tmp <- tempfile(fileext = ".docx")
  writeBin(charToRaw(strrep("not a zip", 200)), tmp)  # > 1 KB so size check passes
  on.exit(unlink(tmp), add = TRUE)
  expect_null(extract_text_from_file(tmp, "docx"))
})

test_that("extract_levers finds FICO floor", {
  res <- extract_levers("Eligibility: FICO >= 700; DTI <= 35%")
  expect_equal(res$values$fico_min, 700L)
  expect_match(res$lines$grading, "FICO >= 700")
})

test_that("extract_levers finds FICO floor in compact 'FICO>=700' form", {
  res <- extract_levers("FICO>=700; ThinFile<=24mo")
  expect_equal(res$values$fico_min, 700L)
})

test_that("extract_levers finds DTI cap with percent", {
  res <- extract_levers("DTI <= 35%")
  expect_equal(res$values$dti_max, 35)
  expect_match(res$lines$grading, "DTI <= 35%")
})

test_that("extract_levers finds thin-file months", {
  res <- extract_levers("Min credit history: 24 months")
  expect_equal(res$values$min_credit_history_months, 24L)
})

test_that("extract_levers flags bankruptcy presence", {
  res <- extract_levers("No bankruptcies in last 7 years")
  expect_true(isTRUE(res$values$no_bankruptcies))
  expect_match(res$lines$grading, "bankruptc", ignore.case = TRUE)
})

test_that("extract_levers counts found levers correctly", {
  res <- extract_levers("FICO >= 700, DTI <= 35%, Min credit history: 24 months")
  expect_equal(res$values$fico_min, 700L)
  expect_equal(res$values$dti_max, 35)
  expect_equal(res$values$min_credit_history_months, 24L)
  expect_gte(res$n_levers, 3L)
})

test_that("extract_levers returns empty result on text with no levers", {
  res <- extract_levers("This is a generic memo with no policy content.")
  expect_equal(res$n_levers, 0L)
  expect_true(is.na(res$values$fico_min))
})

test_that("extract_levers finds hard inquiries cap", {
  res <- extract_levers("Hard inquiries (6M) <= 3")
  expect_equal(res$values$hard_inq_max, 3L)
  expect_match(res$lines$r_logic, "Hard inquiries.*3")
})

test_that("extract_levers finds compact 'HardInq6M max=3' form", {
  res <- extract_levers("Standard waterfall. ThinFile<=24mo; HardInq6M max=3")
  expect_equal(res$values$hard_inq_max, 3L)
})

test_that("extract_levers finds utilization cap", {
  res <- extract_levers("Utilization <= 80%")
  expect_equal(res$values$util_max, 80)
  expect_match(res$lines$r_logic, "Utilization <= 80%")
})

test_that("extract_levers finds base APR", {
  res <- extract_levers("Base APR: 11%")
  expect_equal(res$values$base_apr, 11)
  expect_match(res$lines$pricing, "Base APR: 11%")
})

test_that("extract_levers finds decision cutoffs", {
  res <- extract_levers("Auto-approve: score > 740. Manual review: 700-740. Decline: < 700.")
  expect_equal(res$values$auto_approve, 740L)
  expect_equal(res$values$manual_review, c(700L, 740L))
  expect_equal(res$values$decline, 700L)
  expect_match(res$lines$decision, "Auto-approve")
  expect_match(res$lines$decision, "Manual review")
  expect_match(res$lines$decision, "Decline")
})

test_that("extract_levers handles full sample policy text", {
  txt <- extract_text_from_file("fixtures/sample_policy.pdf", "pdf")
  res <- extract_levers(txt)
  expect_gte(res$n_levers, 6L)
  expect_equal(res$values$fico_min, 700L)
  expect_equal(res$values$dti_max, 35)
  expect_equal(res$values$hard_inq_max, 3L)
  expect_equal(res$values$base_apr, 11)
})

test_that("extract_levers on no_levers.pdf returns 0 levers", {
  txt <- extract_text_from_file("fixtures/no_levers.pdf", "pdf")
  res <- extract_levers(txt)
  expect_equal(res$n_levers, 0L)
})
