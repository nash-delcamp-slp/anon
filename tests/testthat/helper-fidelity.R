# Small linked SDTM and ADaM-like datasets for the anon_check_fidelity() and
# De-identification report tests: 12 subjects with keys and dates that must
# never appear in a report, a weight test in vs, adverse events with full and
# partial dates, and a BDS dataset with CHG, PCHG and study days.
fid_keys <- sprintf("ZZSTUDY-%03d", 1:12)
fid_starts <- as.Date("2021-03-01") + (0:11) * 9

fid_data <- function() {
  n <- length(fid_keys)
  dm <- data.frame(
    USUBJID = fid_keys, SUBJID = sprintf("%03d", 1:n),
    AGE = c(31, 34, 38, 42, 45, 47, 52, 55, 58, 61, 66, 69),
    SEX = rep(c("F", "M"), 6), RACE = c(rep("WHITE", 4), rep("ASIAN", 4), rep("BLACK", 3), "OTHER RACE"),
    RFSTDTC = format(fid_starts), stringsAsFactors = FALSE
  )
  attr(dm$AGE, "label") <- "Age"
  vs <- data.frame(
    USUBJID = rep(fid_keys, each = 2), VSSEQ = rep(1:2, n), VSTESTCD = "WEIGHT",
    VSSTRESN = round(60 + seq_len(2 * n) * 1.3, 1), VSBLFL = rep(c("Y", ""), n),
    VSDTC = format(rep(fid_starts, each = 2) + rep(c(0, 14), n)), stringsAsFactors = FALSE
  )
  vs$VSSTRESC <- as.character(vs$VSSTRESN)
  vs$VSDY <- as.integer(as.Date(vs$VSDTC) - rep(fid_starts, each = 2)) + 1L
  ae <- data.frame(
    USUBJID = fid_keys[c(1, 1, 3, 5, 7)], AESEQ = c(1, 2, 1, 1, 1),
    AETERM = c("HEADACHE", "NAUSEA", "RASH", "FATIGUE", "COUGH"),
    AESTDTC = c(format(fid_starts[1] + 3), "2021-04", paste0(format(fid_starts[3] - 2), "T10:30"), "2021", format(fid_starts[7] + 20)),
    stringsAsFactors = FALSE
  )
  start <- fid_starts[match(ae$USUBJID, fid_keys)]
  full <- nchar(ae$AESTDTC) >= 10
  ae$AESTDY <- NA_integer_
  d <- as.integer(as.Date(substr(ae$AESTDTC[full], 1, 10)) - start[full])
  ae$AESTDY[full] <- d + (d >= 0)
  adlb <- data.frame(
    USUBJID = rep(fid_keys, each = 3), PARAMCD = "ALT", AVISITN = rep(0:2, n),
    AVAL = round(20 + seq_len(3 * n) * 0.7, 1), TRTSDT = rep(fid_starts, each = 3),
    ADT = rep(fid_starts, each = 3) + rep(c(-1, 7, 28), n), stringsAsFactors = FALSE
  )
  adlb$BASE <- rep(adlb$AVAL[adlb$AVISITN == 0], each = 3)
  adlb$CHG <- adlb$AVAL - adlb$BASE
  adlb$PCHG <- 100 * adlb$CHG / adlb$BASE
  d <- as.integer(adlb$ADT - adlb$TRTSDT)
  adlb$ADY <- d + (d >= 0)
  list(dm = dm, vs = vs, ae = ae, adlb = adlb)
}

fid_run <- function(data = fid_data(), keep_secrets = FALSE) {
  suppressWarnings(anon_deidentify(
    data, anon_profile_sdtm(min_n = 3), max_risk = 1/3, keep_secrets = keep_secrets, seed = 7
  ))
}

# The row of a fidelity result for a dataset and check.
fid_row <- function(f, dataset, check) f[f$dataset == dataset & f$check == check, , drop = FALSE]
