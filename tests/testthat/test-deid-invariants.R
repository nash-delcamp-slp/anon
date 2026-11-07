# Privacy and fidelity invariants for every de-identification pipeline. The
# fixture, pipelines and exits live in helper-deid.R; register new functions
# and new ways out of the package there.

for (pipeline_name in names(deid_pipelines)) {
  pipeline <- deid_pipelines[[pipeline_name]]

  test_that(paste0(pipeline_name, ": what can't be de-identified is named in a warning"), {
    run <- deid_run(pipeline, deid_fixture())
    for (f in deid_flagged) {
      expect_true(any(grepl(f$pattern, run$warnings)), info = f$pattern)
    }
  })

  test_that(paste0(pipeline_name, ": no original ID or real date leaves the package"), {
    run <- deid_run(pipeline, deid_fixture())
    expect_true(!is.null(run$step$secrets))
    step <- deid_drop_flagged(run$step)
    texts <- lapply(deid_exits, function(exit) {
      tryCatch(exit(step), error = function(e) conditionMessage(e))
    })
    texts$warnings <- run$warnings
    expect_equal(deid_leaks(texts), character(0))
  })

  test_that(paste0(pipeline_name, ": redacted copies made with anon() carry no secrets"), {
    step <- deid_run(pipeline, deid_fixture())$step
    expect_null(anon(step)$secrets)
    expect_null(anon(list(a = list(step = step)))$a$step$secrets)
  })

  test_that(paste0(pipeline_name, ": the shape of the data is kept"), {
    data <- deid_fixture()
    out <- deid_run(pipeline, data)$step$data
    expect_named(out, names(data))
    for (ds in names(data)) {
      expect_named(out[[ds]], names(data[[ds]]))
      expect_equal(nrow(out[[ds]]), nrow(data[[ds]]), info = ds)
      for (col in names(data[[ds]])) {
        expect_identical(class(out[[ds]][[col]]), class(data[[ds]][[col]]), info = paste0(ds, ".", col))
        expect_identical(
          attr(out[[ds]][[col]], "label", exact = TRUE), attr(data[[ds]][[col]], "label", exact = TRUE),
          info = paste0(ds, ".", col)
        )
      }
    }
    expect_identical(attr(out$vs$VSDTM, "tzone"), attr(data$vs$VSDTM, "tzone"))
    expect_identical(attr(out$ad$TRTSDT, "format.sas"), "DATE9")
  })

  test_that(paste0(pipeline_name, ": datasets still join, IDs stay aligned and study days stay consistent"), {
    out <- deid_run(pipeline, deid_fixture())$step$data
    keys <- out$dm$USUBJID
    expect_false(anyDuplicated(keys) > 0)
    for (ds in c("vs", "ex", "suppdm", "relsub", "ad")) {
      expect_true(all(as.character(out[[ds]]$USUBJID) %in% keys), info = ds)
    }
    expect_true(all(out$relsub$RSUBJID %in% keys))
    expect_equal(sub("^SUBJ-", "", as.vector(keys)), as.vector(out$dm$SUBJID))

    study_day <- function(date, ref) as.integer(date - ref) + (date >= ref)
    ref <- as.Date(out$dm$RFSTDTC[match(as.character(out$vs$USUBJID), keys)])
    expect_equal(study_day(as.Date(substr(out$vs$VSDTC, 1, 10)), ref), out$vs$VSDY)
    ref_ex <- as.Date(out$dm$RFSTDTC[match(out$ex$USUBJID, keys)])
    expect_equal(study_day(as.Date(out$ex$EXSTDTC), ref_ex), out$ex$EXSTDY)

    # Dates of one subject move together, in every column type.
    expect_equal(as.Date(out$dm$RFSTDTC), out$dm$RFSTDT)
    expect_equal(as.Date(out$vs$VSDTM), as.Date(substr(out$vs$VSDTC, 1, 10)))
    expect_equal(format(out$vs$VSDTM, "%H:%M"), rep("08:30", nrow(out$vs)))
    # Partial dates keep their precision and leave their real month.
    expect_match(out$ex$EXENDTC, "^[0-9]{4}-[0-9]{2}$")
    expect_false(any(out$ex$EXENDTC %in% substr(format(deid_start + 20), 1, 7)))
  })
}
