make_bench_text <- function(
  n = 4000,
  sentence_words = 14,
  seed = 1
) {
  set.seed(seed)

  names_pool <- c(
    "Alice Smith",
    "Bob Jones",
    "Carol Taylor",
    "David Brown",
    "Eve Wilson",
    "Frank Thomas",
    "Grace Lee",
    "Henry Clark"
  )
  org_pool <- c(
    "Acme Bio",
    "Northwind Labs",
    "Blue River Analytics",
    "Delta Health"
  )
  city_pool <- c("Boston", "Seattle", "Chicago", "Austin")
  filler_pool <- c(
    "patient",
    "trial",
    "analysis",
    "met",
    "called",
    "emailed",
    "reviewed",
    "dataset"
  )

  sentence <- function(words) {
    paste(
      sample(
        c(names_pool, org_pool, city_pool, filler_pool),
        words,
        replace = TRUE
      ),
      collapse = " "
    )
  }

  data <- data.frame(
    notes = replicate(n, sentence(sentence_words)),
    email = sprintf(
      "%s%04d@example.com",
      sample(c("alice", "bob", "carol", "david"), n, replace = TRUE),
      sample.int(9999, n, replace = TRUE)
    ),
    location = sample(city_pool, n, replace = TRUE),
    stringsAsFactors = FALSE
  )

  patterns <- c(names_pool, org_pool, city_pool)

  list(
    data = data,
    patterns = patterns
  )
}

make_starwars_bench_data <- function(multiplier = 8) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Install 'dplyr' to build the starwars benchmark fixture.", call. = FALSE)
  }

  data("starwars", package = "dplyr", envir = environment())

  base_df <- as.data.frame(starwars, stringsAsFactors = FALSE)
  pieces <- replicate(multiplier, base_df, simplify = FALSE)
  result <- dplyr::bind_rows(pieces)
  result <- as.data.frame(result, stringsAsFactors = FALSE)

  rownames(result) <- sprintf("sw-%04d", seq_len(nrow(result)))
  attr(result, "label") <- paste(
    "Starwars benchmark frame with repeated records,",
    "list columns, labels, and row names"
  )
  attr(result$name, "label") <- "Character name"
  attr(result$homeworld, "label") <- "Planet of origin"
  attr(result$species, "label") <- "Species"

  result
}

make_starwars_patterns <- function() {
  c(
    "Luke Skywalker",
    "Leia Organa",
    "Darth Vader",
    "Han Solo",
    "Tatooine",
    "Naboo",
    "Millennium Falcon",
    "The Empire Strikes Back"
  )
}

make_nested_bench_object <- function() {
  text_inputs <- make_bench_text(n = 800, sentence_words = 12, seed = 11)
  starwars_df <- make_starwars_bench_data(multiplier = 4)

  list(
    primary = starwars_df,
    notes = text_inputs$data,
    metadata = list(
      title = "Starwars mixed benchmark object",
      owner = "Leia Organa",
      locations = c("Tatooine", "Alderaan", "Naboo"),
      tags = c("rebellion", "empire", "jedi")
    ),
    appendix = list(
      starships = starwars_df$starships[seq_len(60)],
      vehicles = starwars_df$vehicles[seq_len(60)]
    )
  )
}

make_starwars_nlp_bench_object <- function(multiplier = 4, rows = 24, reports_n = 24) {
  starwars_df <- make_starwars_bench_data(multiplier = multiplier)
  if (nrow(starwars_df) > rows) {
    starwars_df <- starwars_df[seq_len(rows), , drop = FALSE]
  }

  mission_brief <- vapply(seq_len(nrow(starwars_df)), function(i) {
    films <- starwars_df$films[[i]]
    film <- if (length(films) > 0) films[[1]] else "an unknown story"
    starships <- starwars_df$starships[[i]]
    starship <- if (length(starships) > 0) starships[[1]] else "civilian transport"
    homeworld <- ifelse(is.na(starwars_df$homeworld[[i]]), "an unknown world", starwars_df$homeworld[[i]])
    species <- ifelse(is.na(starwars_df$species[[i]]), "unknown origin", starwars_df$species[[i]])

    paste(
      starwars_df$name[[i]],
      "from", homeworld,
      "is a", species,
      "featured in", film,
      "and linked to", starship,
      "during a Rebel Alliance intelligence review."
    )
  }, character(1))

  starwars_df$mission_brief <- mission_brief
  attr(starwars_df$mission_brief, "label") <- "Narrative mission brief"

  reports <- data.frame(
    report = make_nlp_bench_corpus(reports_n),
    analyst = rep(c("Leia Organa", "Mon Mothma", "Cassian Andor"), length.out = reports_n),
    stringsAsFactors = FALSE
  )
  attr(reports$report, "label") <- "Operations report"

  list(
    roster = starwars_df,
    reports = reports,
    metadata = list(
      title = "Starwars nested auto NLP benchmark object",
      contact = "Leia Organa",
      bases = c("Tatooine", "Yavin 4", "Alderaan")
    )
  )
}

make_nlp_bench_corpus <- function(n = 200) {
  base <- c(
    "Luke Skywalker met Leia Organa on Tatooine before boarding the Millennium Falcon.",
    "Han Solo and Chewbacca flew from Corellia to Alderaan with C-3PO.",
    "Darth Vader reported to Emperor Palpatine aboard the Death Star.",
    "Padme Amidala returned to Naboo after speaking with Obi-Wan Kenobi.",
    "Rey visited Jakku before joining Finn near the Resistance base."
  )

  rep(base, length.out = n)
}

load_dev_package <- function(path = ".") {
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop("Install 'pkgload' to run development benchmarks.", call. = FALSE)
  }

  pkgload::load_all(path, quiet = TRUE)
}

nlp_benchmark_available <- function() {
  if (!requireNamespace("cleanNLP", quietly = TRUE) ||
      !requireNamespace("reticulate", quietly = TRUE)) {
    return(FALSE)
  }

  if (!reticulate::py_module_available("cleannlp")) {
    return(FALSE)
  }

  tryCatch(
    {
      suppressMessages(cleanNLP::cnlp_init_spacy(model_name = "en_core_web_sm"))
      TRUE
    },
    error = function(...) FALSE
  )
}

run_with_nlp_auto_option <- function(nlp_auto_option, code) {
  old_options <- options(anon.nlp_auto = nlp_auto_option)
  on.exit(options(anon.nlp_auto = old_options$anon.nlp_auto), add = TRUE)
  force(code)
}

run_benchmarks <- function(iterations = 5, check = FALSE) {
  if (!requireNamespace("bench", quietly = TRUE)) {
    stop("Install 'bench' to run benchmarks.", call. = FALSE)
  }

  text_inputs <- make_bench_text()
  starwars_df <- make_starwars_bench_data()
  starwars_patterns <- make_starwars_patterns()
  nested_obj <- make_nested_bench_object()
  starwars_nlp_obj <- make_starwars_nlp_bench_object(multiplier = 1, rows = 8, reports_n = 4)

  if (nlp_benchmark_available()) {
    nlp_corpus <- make_nlp_bench_corpus()

    return(bench::mark(
      anon_vector_exact = anon(
        text_inputs$data$notes,
        pattern_list = text_inputs$patterns,
        check_approximate = FALSE
      ),
      anon_vector_approx = anon(
        text_inputs$data$notes,
        pattern_list = text_inputs$patterns,
        check_approximate = TRUE
      ),
      anon_data_frame_exact = anon(
        text_inputs$data,
        pattern_list = text_inputs$patterns,
        check_approximate = FALSE
      ),
      anon_data_frame_approx = anon(
        text_inputs$data,
        pattern_list = text_inputs$patterns,
        check_approximate = TRUE
      ),
      anon_starwars_exact = anon(
        starwars_df,
        pattern_list = starwars_patterns,
        check_approximate = FALSE,
        check_labels = TRUE,
        check_names = TRUE
      ),
      anon_starwars_mixed = anon(
        starwars_df,
        pattern_list = list(
          SIDE = c("Empire", "Jedi", "Sith"),
          SHIP = c("Millennium Falcon", "Imperial shuttle")
        ),
        df_variable_names = list(
          name = anon_id_chr_sequence,
          homeworld = ~ anon_id_chr_sequence(.x, start = "Planet ")
        ),
        df_classes = list(
          integer = ~ anon_num_range(.x, n_breaks = 8),
          numeric = anon_num_preserve_distribution
        ),
        check_approximate = TRUE,
        check_labels = TRUE,
        check_names = TRUE
      ),
      anon_nested_mixed = anon(
        nested_obj,
        pattern_list = c(text_inputs$patterns, starwars_patterns),
        df_variable_names = list(name = anon_id_chr_sequence),
        check_approximate = TRUE,
        check_labels = TRUE,
        check_names = TRUE
      ),
      anon_nlp_entities_people_org_place = anon_nlp_entities(
        nlp_corpus,
        entity_types = c("PERSON", "ORG", "GPE")
      ),
      anon_auto_nlp_core = anon(
        nlp_corpus,
        nlp_auto = nlp_auto(person = TRUE, org = TRUE, gpe = TRUE),
        check_approximate = FALSE
      ),
      anon_nested_option_auto_nlp = run_with_nlp_auto_option(
        nlp_auto(
          person = TRUE,
          org = TRUE,
          gpe = TRUE,
          work_of_art = TRUE
        ),
        anon(
          starwars_nlp_obj,
          pattern_list = list(
            SIDE = c("Empire", "Jedi", "Sith"),
            SHIP = c("Millennium Falcon", "Imperial shuttle")
          ),
          df_variable_names = list(
            name = anon_id_chr_sequence,
            homeworld = ~ anon_id_chr_sequence(.x, start = "Planet ")
          ),
          df_classes = list(
            integer = ~ anon_num_range(.x, n_breaks = 8),
            numeric = anon_num_preserve_distribution
          ),
          check_approximate = FALSE,
          check_labels = TRUE,
          check_names = TRUE
        )
      ),
      iterations = iterations,
      check = check
    ))
  }

  bench::mark(
    anon_vector_exact = anon(
      text_inputs$data$notes,
      pattern_list = text_inputs$patterns,
      check_approximate = FALSE
    ),
    anon_vector_approx = anon(
      text_inputs$data$notes,
      pattern_list = text_inputs$patterns,
      check_approximate = TRUE
    ),
    anon_data_frame_exact = anon(
      text_inputs$data,
      pattern_list = text_inputs$patterns,
      check_approximate = FALSE
    ),
    anon_data_frame_approx = anon(
      text_inputs$data,
      pattern_list = text_inputs$patterns,
      check_approximate = TRUE
    ),
    anon_starwars_exact = anon(
      starwars_df,
      pattern_list = starwars_patterns,
      check_approximate = FALSE,
      check_labels = TRUE,
      check_names = TRUE
    ),
    anon_starwars_mixed = anon(
      starwars_df,
      pattern_list = list(
        SIDE = c("Empire", "Jedi", "Sith"),
        SHIP = c("Millennium Falcon", "Imperial shuttle")
      ),
      df_variable_names = list(
        name = anon_id_chr_sequence,
        homeworld = ~ anon_id_chr_sequence(.x, start = "Planet ")
      ),
      df_classes = list(
        integer = ~ anon_num_range(.x, n_breaks = 8),
        numeric = anon_num_preserve_distribution
      ),
      check_approximate = TRUE,
      check_labels = TRUE,
      check_names = TRUE
    ),
    anon_nested_mixed = anon(
      nested_obj,
      pattern_list = c(text_inputs$patterns, starwars_patterns),
      df_variable_names = list(name = anon_id_chr_sequence),
      check_approximate = TRUE,
      check_labels = TRUE,
      check_names = TRUE
    ),
    iterations = iterations,
    check = check
  )
}

parse_flag <- function(args, flag, default = NULL) {
  idx <- match(flag, args)
  if (is.na(idx) || idx == length(args)) {
    return(default)
  }
  args[[idx + 1]]
}

trim_or_na <- function(x) {
  x <- trimws(x)
  if (length(x) == 0 || identical(x, "")) {
    return(NA_character_)
  }
  x
}

get_git_sha <- function() {
  trim_or_na(tryCatch(
    system2("git", c("rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE),
    error = function(...) NA_character_
  ))
}

get_git_ref <- function() {
  trim_or_na(tryCatch(
    system2("git", c("rev-parse", "--abbrev-ref", "HEAD"), stdout = TRUE, stderr = FALSE),
    error = function(...) NA_character_
  ))
}

get_cpu_model <- function() {
  sysname <- Sys.info()[["sysname"]]

  if (identical(sysname, "Linux")) {
    cpuinfo <- tryCatch(readLines("/proc/cpuinfo", warn = FALSE), error = function(...) character())
    model <- sub("^[^:]+:[[:space:]]*", "", grep("model name", cpuinfo, value = TRUE)[1])
    return(trim_or_na(model))
  }

  if (identical(sysname, "Darwin")) {
    model <- tryCatch(system2("sysctl", c("-n", "machdep.cpu.brand_string"), stdout = TRUE, stderr = FALSE), error = function(...) character())
    return(trim_or_na(model[1]))
  }

  NA_character_
}

get_machine_label <- function() {
  env_label <- Sys.getenv("BENCH_MACHINE", unset = "")
  if (nzchar(env_label)) {
    return(env_label)
  }

  paste(
    Sys.info()[["nodename"]],
    Sys.info()[["sysname"]],
    Sys.info()[["release"]],
    sep = " | "
  )
}

get_run_context <- function() {
  if (nzchar(Sys.getenv("GITHUB_ACTIONS", unset = ""))) {
    return("github-actions")
  }
  "local"
}

collect_benchmark_results <- function(iterations = 5, check = FALSE) {
  marks <- run_benchmarks(iterations = iterations, check = check)
  timestamp <- as.POSIXct(Sys.time(), tz = "UTC")
  timestamp_chr <- format(timestamp, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  git_sha <- get_git_sha()
  run_id <- paste(
    format(timestamp, "%Y%m%dT%H%M%SZ", tz = "UTC"),
    substr(ifelse(is.na(git_sha), "nogit", git_sha), 1, 7),
    sep = "-"
  )

  data.frame(
    run_id = run_id,
    timestamp_utc = timestamp_chr,
    run_context = get_run_context(),
    machine_label = get_machine_label(),
    git_sha = git_sha,
    git_ref = get_git_ref(),
    r_version = as.character(getRversion()),
    system = Sys.info()[["sysname"]],
    system_release = Sys.info()[["release"]],
    cpu_model = get_cpu_model(),
    expression = as.character(marks$expression),
    median_sec = as.numeric(marks$median),
    itr_sec = as.numeric(marks$`itr/sec`),
    mem_alloc_bytes = as.numeric(marks$mem_alloc),
    gc_sec = as.numeric(marks$`gc/sec`),
    stringsAsFactors = FALSE
  )
}

read_history <- function(path) {
  if (!file.exists(path) || isTRUE(file.info(path)$size == 0)) {
    return(data.frame(stringsAsFactors = FALSE))
  }

  utils::read.csv(path, stringsAsFactors = FALSE)
}

write_history <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(data, path, row.names = FALSE)
}

append_history <- function(path, latest) {
  history <- read_history(path)
  combined <- rbind(history, latest)
  write_history(combined, path)
  combined
}

html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

format_metric <- function(x, digits = 2, suffix = "") {
  ifelse(is.na(x), "NA", paste0(formatC(x, digits = digits, format = "f"), suffix))
}

format_delta_pct <- function(current, previous) {
  if (is.na(current) || is.na(previous) || previous == 0) {
    return("NA")
  }

  delta <- ((current / previous) - 1) * 100
  sprintf("%+.1f%%", delta)
}
