rm(list = ls())
while (sink.number() > 0) sink()
options(stringsAsFactors = FALSE, warn = 1, scipen = 999)

## --------------------------- MASTER: input -------------------------------- ##
master_args <- commandArgs(trailingOnly = TRUE)
MASTER_DATA_FILE <- if (length(master_args) >= 1L && file.exists(master_args[1L])) {
  normalizePath(master_args[1L], mustWork = TRUE)
} else {
  candidates <- c("data(3).xlsx", "data.xlsx")
  hit <- candidates[file.exists(candidates)]
  if (length(hit) > 0L) {
    normalizePath(hit[1L], mustWork = TRUE)
  } else if (interactive()) {
    message("Select data(3).xlsx ...")
    normalizePath(file.choose(), mustWork = TRUE)
  } else {
    stop(
      "Raw workbook not found. Place data(3).xlsx beside this script or pass ",
      "its full path as the first command-line argument.", call. = FALSE
    )
  }
}

MASTER_TIMESTAMP <- format(Sys.time(), "%Y%m%d_%H%M%S")
MASTER_OUT_DIR <- file.path(
  dirname(MASTER_DATA_FILE),
  paste0("outputs_unified_return_connectedness_", MASTER_TIMESTAMP)
)
dir.create(MASTER_OUT_DIR, recursive = TRUE, showWarnings = FALSE)
MASTER_DERIVED_DIR <- file.path(MASTER_OUT_DIR, "derived_from_raw")
dir.create(MASTER_DERIVED_DIR, recursive = TRUE, showWarnings = FALSE)

cat("Unified raw-data pipeline\n")
cat("Input :", MASTER_DATA_FILE, "\n")
cat("Output:", MASTER_OUT_DIR, "\n\n")

MASTER_REQUIRED_PACKAGES <- c(
  "readxl", "writexl", "zoo", "vars", "tseries", "strucchange", "urca",
  "dplyr", "tidyr", "sandwich", "lmtest", "ggplot2"
)
master_missing <- MASTER_REQUIRED_PACKAGES[
  !vapply(MASTER_REQUIRED_PACKAGES, requireNamespace, logical(1), quietly = TRUE)
]
if (length(master_missing) > 0L) {
  stop(
    "Missing required R package(s): ", paste(master_missing, collapse = ", "),
    "\nInstall once with:\ninstall.packages(c(",
    paste(sprintf('"%s"', master_missing), collapse = ", "), "))",
    call. = FALSE
  )
}

## ---------------------- MASTER: raw-input audit --------------------------- ##
master_sheets <- readxl::excel_sheets(MASTER_DATA_FILE)
required_sheets <- c("series", "eom", "contextual", "gsdb", "gsdb_description")
missing_sheets <- setdiff(required_sheets, master_sheets)
if (length(missing_sheets) > 0L) {
  stop("Missing raw sheet(s): ", paste(missing_sheets, collapse = ", "), call. = FALSE)
}

MASTER_SERIES <- suppressMessages(readxl::read_excel(MASTER_DATA_FILE, sheet = "series"))
MASTER_EOM <- suppressMessages(readxl::read_excel(MASTER_DATA_FILE, sheet = "eom"))
MASTER_CONTEXT <- suppressMessages(readxl::read_excel(MASTER_DATA_FILE, sheet = "contextual"))
MASTER_GSDB <- suppressMessages(readxl::read_excel(MASTER_DATA_FILE, sheet = "gsdb"))
MASTER_GSDB_DESCRIPTION <- suppressMessages(
  readxl::read_excel(MASTER_DATA_FILE, sheet = "gsdb_description")
)

required_series_cols <- c("date", "tedpix", "usd", "goldcoin", "housing", "vehicle", "cpi")
required_eom_cols <- c(
  "month_jalali", "usd_last_observation_date_jalali", "usd_eom_rial",
  "gold_last_observation_date_jalali", "gold_coin_eom_rial",
  "tedpix_last_observation_date_jalali", "tedpix_eom",
  "vehicle_cpi_1400_100", "housing_price_irr_per_sqm"
)
required_context_cols <- c(
  "date", "interbank", "cpi", "oil_exports_mbd",
  "days_in_jalali_month", "iran_heavy_usd_per_bbl"
)
required_gsdb_cols <- c(
  "case_id", "sanctioned_state", "sanctioning_state", "begin", "end",
  "trade", "financial"
)

miss_cols <- list(
  series = setdiff(required_series_cols, names(MASTER_SERIES)),
  eom = setdiff(required_eom_cols, names(MASTER_EOM)),
  contextual = setdiff(required_context_cols, names(MASTER_CONTEXT)),
  gsdb = setdiff(required_gsdb_cols, names(MASTER_GSDB))
)
bad_col_groups <- names(miss_cols)[lengths(miss_cols) > 0L]
if (length(bad_col_groups) > 0L) {
  msg <- paste(vapply(bad_col_groups, function(z) {
    paste0(z, ": ", paste(miss_cols[[z]], collapse = ", "))
  }, character(1)), collapse = " | ")
  stop("Missing required raw columns: ", msg, call. = FALSE)
}

normalize_month <- function(x) {
  substr(gsub("-", "/", trimws(as.character(x)), fixed = TRUE), 1L, 7L)
}
month_to_index <- function(x) {
  p <- strsplit(normalize_month(x), "/", fixed = TRUE)
  vapply(p, function(z) {
    yy <- suppressWarnings(as.integer(z[1L]))
    mm <- suppressWarnings(as.integer(z[2L]))
    if (is.na(yy) || is.na(mm) || mm < 1L || mm > 12L) return(NA_integer_)
    yy * 12L + mm
  }, integer(1))
}

series_month <- normalize_month(MASTER_SERIES$date)
eom_month <- normalize_month(MASTER_EOM$month_jalali)
context_month <- normalize_month(MASTER_CONTEXT$date)

audit_rows <- list()
master_add_audit <- function(check, observed, expected, pass, note) {
  audit_rows[[length(audit_rows) + 1L]] <<- data.frame(
    check = check,
    observed = as.character(observed),
    expected = as.character(expected),
    status = if (isTRUE(pass)) "PASS" else "FAIL",
    note = note,
    stringsAsFactors = FALSE
  )
}

master_add_audit(
  "raw_sheets",
  paste(required_sheets, collapse = ";"),
  paste(required_sheets, collapse = ";"),
  all(required_sheets %in% master_sheets),
  "All five raw-data sheets are present."
)
master_add_audit(
  "series_rows", nrow(MASTER_SERIES), 122L, nrow(MASTER_SERIES) == 122L,
  "Expected price/index-level sample."
)
master_add_audit(
  "eom_rows", nrow(MASTER_EOM), 122L, nrow(MASTER_EOM) == 122L,
  "Expected end-of-month level sample."
)
master_add_audit(
  "contextual_rows", nrow(MASTER_CONTEXT), 122L, nrow(MASTER_CONTEXT) == 122L,
  "Expected contextual raw-input sample."
)
master_add_audit(
  "month_alignment",
  paste(series_month[1L], tail(series_month, 1L), sep = " to "),
  "1395/01 to 1405/02",
  identical(series_month, eom_month) && identical(series_month, context_month) &&
    length(series_month) == 122L &&
    series_month[1L] == "1395/01" && tail(series_month, 1L) == "1405/02",
  "series, eom and contextual month keys must be exactly aligned."
)
idx <- month_to_index(series_month)
master_add_audit(
  "complete_month_sequence",
  if (anyNA(idx)) "invalid key" else paste(range(diff(idx)), collapse = " to "),
  "all adjacent differences = 1",
  !anyNA(idx) && all(diff(idx) == 1L),
  "No monthly gap or duplicate is allowed."
)

series_numeric <- as.data.frame(lapply(
  MASTER_SERIES[c("tedpix", "usd", "goldcoin", "housing", "vehicle", "cpi")],
  as.numeric
))
master_add_audit(
  "series_complete_positive",
  "checked", "all finite and > 0",
  !anyNA(series_numeric) &&
    all(is.finite(as.matrix(series_numeric))) &&
    all(as.matrix(series_numeric) > 0),
  "All market/CPI levels used for log transformations must be positive."
)

eom_numeric <- as.data.frame(lapply(
  MASTER_EOM[c(
    "usd_eom_rial", "gold_coin_eom_rial", "tedpix_eom",
    "vehicle_cpi_1400_100", "housing_price_irr_per_sqm"
  )], as.numeric
))
master_add_audit(
  "eom_complete_positive",
  "checked", "all finite and > 0",
  !anyNA(eom_numeric) &&
    all(is.finite(as.matrix(eom_numeric))) &&
    all(as.matrix(eom_numeric) > 0),
  "All end-of-month levels used for log transformations must be positive."
)
master_add_audit(
  "shared_vehicle_housing",
  "checked", "exact equality",
  isTRUE(all.equal(
    as.numeric(MASTER_SERIES$vehicle),
    as.numeric(MASTER_EOM$vehicle_cpi_1400_100), tolerance = 0
  )) &&
    isTRUE(all.equal(
      as.numeric(MASTER_SERIES$housing),
      as.numeric(MASTER_EOM$housing_price_irr_per_sqm), tolerance = 0
    )),
  "Only USD, GOLD and STOCK sampling changes in the EOM specification."
)
master_add_audit(
  "contextual_cpi_matches_series",
  "checked", "exact equality",
  isTRUE(all.equal(
    as.numeric(MASTER_SERIES$cpi),
    as.numeric(MASTER_CONTEXT$cpi), tolerance = 0
  )),
  "The contextual CPI level must be the same CPI series used for CPI adjustment."
)

ctx_oil <- as.data.frame(lapply(
  MASTER_CONTEXT[c("oil_exports_mbd", "days_in_jalali_month", "iran_heavy_usd_per_bbl")],
  as.numeric
))
master_add_audit(
  "oil_inputs_complete_positive",
  "checked", "all finite and > 0",
  !anyNA(ctx_oil) &&
    all(is.finite(as.matrix(ctx_oil))) &&
    all(as.matrix(ctx_oil) > 0),
  "The oil-export-value proxy is constructed inside R from these raw columns."
)
master_add_audit(
  "jalali_day_counts",
  paste(sort(unique(as.numeric(MASTER_CONTEXT$days_in_jalali_month))), collapse = ";"),
  "29;30;31",
  all(as.numeric(MASTER_CONTEXT$days_in_jalali_month) %in% c(29, 30, 31)),
  "Valid day counts for the raw monthly oil-value construction."
)

interbank <- suppressWarnings(as.numeric(MASTER_CONTEXT$interbank))
ib_start <- which(context_month == "1396/01")
ib_ok <- length(ib_start) == 1L &&
  all(is.finite(interbank[ib_start:nrow(MASTER_CONTEXT)])) &&
  all(interbank[ib_start:nrow(MASTER_CONTEXT)] > 0)
master_add_audit(
  "interbank_coverage",
  if (length(ib_start) == 1L)
    paste(context_month[ib_start], "to", tail(context_month, 1L))
  else "start not found",
  "1396/01 to 1405/02 complete",
  ib_ok,
  "Missing values before 1396/01 are permitted because Layer 4 starts later."
)

for (v in c(
  "usd_last_observation_date_jalali",
  "gold_last_observation_date_jalali",
  "tedpix_last_observation_date_jalali"
)) {
  date_key <- normalize_month(MASTER_EOM[[v]])
  master_add_audit(
    paste0(v, "_month_key"),
    "checked", "same month as month_jalali",
    identical(date_key, eom_month),
    "The final available traded-market observation must belong to its reported Jalali month."
  )
}

master_add_audit(
  "gsdb_rows", nrow(MASTER_GSDB), 1547L, nrow(MASTER_GSDB) == 1547L,
  "Raw GSDB Release 4 case table."
)

MASTER_INPUT_AUDIT <- do.call(rbind, audit_rows)
utils::write.csv(
  MASTER_INPUT_AUDIT,
  file.path(MASTER_OUT_DIR, "raw_input_audit.csv"),
  row.names = FALSE
)
if (any(MASTER_INPUT_AUDIT$status != "PASS")) {
  print(MASTER_INPUT_AUDIT)
  stop("Raw input audit failed. No econometric analysis was run.", call. = FALSE)
}

## ------------------ MASTER: transparent raw transformations ---------------- ##
MASTER_LOGRET <- function(x) 100 * diff(log(as.numeric(x)))
MASTER_VARS <- c("USD", "GOLD", "STOCK", "VEHICLE", "HOUSING")

nominal_levels <- data.frame(
  USD = as.numeric(MASTER_SERIES$usd),
  GOLD = as.numeric(MASTER_SERIES$goldcoin),
  STOCK = as.numeric(MASTER_SERIES$tedpix),
  VEHICLE = as.numeric(MASTER_SERIES$vehicle),
  HOUSING = as.numeric(MASTER_SERIES$housing)
)
eom_levels <- data.frame(
  USD = as.numeric(MASTER_EOM$usd_eom_rial),
  GOLD = as.numeric(MASTER_EOM$gold_coin_eom_rial),
  STOCK = as.numeric(MASTER_EOM$tedpix_eom),
  VEHICLE = as.numeric(MASTER_EOM$vehicle_cpi_1400_100),
  HOUSING = as.numeric(MASTER_EOM$housing_price_irr_per_sqm)
)

MASTER_RET_NOMINAL <- sapply(nominal_levels, MASTER_LOGRET)
MASTER_INFLATION <- MASTER_LOGRET(MASTER_SERIES$cpi)
MASTER_RET_CPI_ADJUSTED <- MASTER_RET_NOMINAL - MASTER_INFLATION
MASTER_RET_EOM <- sapply(eom_levels, MASTER_LOGRET)
colnames(MASTER_RET_NOMINAL) <- colnames(MASTER_RET_CPI_ADJUSTED) <-
  colnames(MASTER_RET_EOM) <- MASTER_VARS

MASTER_OIL_VALUE_MUSD <- as.numeric(MASTER_CONTEXT$oil_exports_mbd) *
  as.numeric(MASTER_CONTEXT$days_in_jalali_month) *
  as.numeric(MASTER_CONTEXT$iran_heavy_usd_per_bbl)
MASTER_OIL_GROWTH <- c(NA_real_, 100 * diff(log(MASTER_OIL_VALUE_MUSD)))
MASTER_OIL_VOLUME_GROWTH <- c(
  NA_real_, 100 * diff(log(as.numeric(MASTER_CONTEXT$oil_exports_mbd)))
)
MASTER_IRAN_HEAVY_RETURN <- c(
  NA_real_, 100 * diff(log(as.numeric(MASTER_CONTEXT$iran_heavy_usd_per_bbl)))
)
MASTER_D_INTERBANK <- c(NA_real_, diff(interbank))
MASTER_INFLATION_FULL <- c(NA_real_, MASTER_INFLATION)

derived_returns <- data.frame(
  month_jalali = series_month[-1L],
  nominal_USD = MASTER_RET_NOMINAL[, "USD"],
  nominal_GOLD = MASTER_RET_NOMINAL[, "GOLD"],
  nominal_STOCK = MASTER_RET_NOMINAL[, "STOCK"],
  nominal_VEHICLE = MASTER_RET_NOMINAL[, "VEHICLE"],
  nominal_HOUSING = MASTER_RET_NOMINAL[, "HOUSING"],
  cpi_adjusted_USD = MASTER_RET_CPI_ADJUSTED[, "USD"],
  cpi_adjusted_GOLD = MASTER_RET_CPI_ADJUSTED[, "GOLD"],
  cpi_adjusted_STOCK = MASTER_RET_CPI_ADJUSTED[, "STOCK"],
  cpi_adjusted_VEHICLE = MASTER_RET_CPI_ADJUSTED[, "VEHICLE"],
  cpi_adjusted_HOUSING = MASTER_RET_CPI_ADJUSTED[, "HOUSING"],
  eom_USD = MASTER_RET_EOM[, "USD"],
  eom_GOLD = MASTER_RET_EOM[, "GOLD"],
  eom_STOCK = MASTER_RET_EOM[, "STOCK"],
  eom_VEHICLE = MASTER_RET_EOM[, "VEHICLE"],
  eom_HOUSING = MASTER_RET_EOM[, "HOUSING"],
  inflation_monthly_pct = MASTER_INFLATION,
  stringsAsFactors = FALSE
)

derived_context <- data.frame(
  month_jalali = context_month,
  interbank_rate_pct = interbank,
  d_interbank = MASTER_D_INTERBANK,
  cpi = as.numeric(MASTER_CONTEXT$cpi),
  inflation_monthly_pct = MASTER_INFLATION_FULL,
  oil_exports_mbd = as.numeric(MASTER_CONTEXT$oil_exports_mbd),
  days_in_jalali_month = as.numeric(MASTER_CONTEXT$days_in_jalali_month),
  iran_heavy_usd_per_bbl = as.numeric(MASTER_CONTEXT$iran_heavy_usd_per_bbl),
  iran_heavy_return_pct = MASTER_IRAN_HEAVY_RETURN,
  oil_volume_growth_pct = MASTER_OIL_VOLUME_GROWTH,
  oil_export_revenue_proxy_musd = MASTER_OIL_VALUE_MUSD,
  oil_export_revenue_proxy_growth_pct = MASTER_OIL_GROWTH,
  stringsAsFactors = FALSE
)

utils::write.csv(
  derived_returns,
  file.path(MASTER_DERIVED_DIR, "market_returns_from_raw.csv"),
  row.names = FALSE, na = ""
)
utils::write.csv(
  derived_context,
  file.path(MASTER_DERIVED_DIR, "contextual_transforms_from_raw.csv"),
  row.names = FALSE, na = ""
)

cat("Raw-input audit passed. Running validated Layers 1 and 2...\n\n")


## ========================================================================
## INTERNAL LOCKED ENGINE: LAYERS 1 AND 2
## ========================================================================

local({
  ###############################################################################
  ##  layer1_layer2_Q1_corrected.R
  ##  Q1.2.2: fixes extraction of the full Bai-Perron BIC path and adds an
  ##          internal cross-check against the package's minimum-BIC partition.
  ##  Q1.2: targeted final addendum for Layer 2 only. Adds break-adjusted
  ##         stationarity/structural-change diagnostics for real HOUSING and
  ##         full VAR(2) diagnostics. The validated baseline estimation,
  ##         bootstrap, TVP, frequency, GIRF, figures, and data are unchanged.
  ##  Q1.1: targeted revision of the validated V10.3 pipeline.
  ##         Static point-estimation formulas are unchanged. Corrections add:
  ##         (i) stable TVP state handling with an auditable carry-forward flag,
  ##         (ii) a genuinely complete common calendar window for TVP sensitivity,
  ##         (iii) wild-block bootstrap and simultaneous NET/NPDC bands,
  ##         (iv) paired moving-block inference for Layer 2 minus Layer 1,
  ##         (v) exact input/output names and non-mutating package checks.
  ##  V10.1: frequency normalization fixed; workbook via writexl.
  ##  V10.3: (referee round 3) added sensitivity summaries for TCI level/trend,
  ##         peak/trough timing, and market roles across specifications; the
  ##         terminology is corrected below so tuning agreement is not presented
  ##         as statistical robustness.
  ##  V10.2: (referee round 2) strict TVP training/post-training split (no
  ##         double use of training data); TVP sensitivity over training
  ##         windows {24,36,48} and (kappa1,kappa2) combos with full stats;
  ##         all figures device-safe with proper Date axes; redesigned
  ##         network figure; added frequency-band and sensitivity figures.
  ##
  ##  Corrected replication of Layer 1 (nominal monthly returns) and
  ##  Layer 2 (CPI-adjusted returns) connectedness analysis.
  ##
  ##  In the unified pipeline, this locked engine reads the raw `series` sheet
  ##  directly from data(3).xlsx and performs the return transformations internally.
  ##
  ##  FIXES relative to V9 (referee report):
  ##   [F1] TVP-VAR prior is estimated ONLY on the first TRAIN_N observations
  ##        (expanding Kalman filter with forgetting factors). No look-ahead:
  ##        the reported TVP path starts AFTER the training window.
  ##   [F2] Event robustness NO LONGER splices the series across gaps.
  ##        Instead: (a) exogenous event dummies in the VAR (full sample),
  ##        (b) one long contiguous post-2020 subsample. Short segments that
  ##        cannot support a 5-variable VAR are NOT estimated (documented).
  ##   [F3] No pseudo "TVP confidence bands" from a constant-parameter DGP.
  ##        (Optional fixed-DGP sensitivity exists but is OFF by default and
  ##        explicitly labelled as sensitivity, not inference.)
  ##   [F4] Diagnostics: multivariate normality extracted correctly;
  ##        multivariate ARCH-LM is NOT reported (df = lags*225 > T, invalid
  ##        for K=5, T~121) -- per-equation ARCH tests are reported instead,
  ##        with honest status labels. KPSS included in unit-root screen.
  ##   [F5] NPDC uses the explicit package definition NPDC(i,j)=C[i,j]-C[j,i]
  ##        (net transmission j -> i when positive); the 10 UNIQUE pairs are
  ##        reported once, no duplicated mirror rows, no data-driven
  ##        orientation search.
  ##   [F6] Network figure is fully data-driven: edges = bootstrap-significant
  ##        NPDC pairs (any direction), node colours by the SIGN of estimated
  ##        NET, positions on a circle. Nothing hard-coded.
  ##   [F7] Output workbooks are built fresh with writexl (values only):
  ##        no drawing parts, no broken relationships.
  ##   [F8] GFEVD/TCI/NET/NPDC/GIRF/frequency computed by transparent local
  ##        functions (auditable formulas); optional cross-check against
  ##        ConnectednessApproach if that package is installed.
  ##
  ##  Bootstrap: residual-based (iid, wild, or wild-block), B = 1000 (configurable),
  ##  gives percentile CIs for TCI, NET, NPDC and GIRF of the STATIC model.
  ##
  ##  Author: prepared for the replication package, 2026.
  ###############################################################################

  ## --------------------------- 0. Settings ---------------------------------- ##
  rm(list = ls())
  while (sink.number() > 0) sink()          # reset any open log from a failed run
  options(stringsAsFactors = FALSE, warn = 1)

  ## Unified wrapper: the only data input is MASTER_DATA_FILE = data(3).xlsx.
  ## The locked Layer-1/2 engine auto-detects the raw `series` sheet below.
  DATA_FILE <- MASTER_DATA_FILE
  cat("Selected data file:", DATA_FILE, "\n")
  N_LAG          <- 1                   # baseline VAR lag (confirmed by VARselect)
  H_GFEVD        <- 10                  # GFEVD horizon
  BOOT_REPS      <- 1000                # static bootstrap replications
  BOOT_TYPE      <- "wild_block"        # "wild_block", "wild", or "iid"
  BOOT_BLOCK_LEN <- 6                     # months; common sign within each block
  BOOT_SEED      <- 12345
  PAIRED_BOOT_REPS      <- 1000           # Layer 2 minus Layer 1 inference
  PAIRED_BOOT_BLOCK_LEN <- 6              # circular moving blocks, in months
  PAIRED_BOOT_SEED      <- 24680

  TRAIN_N        <- 36                  # baseline TVP prior training window     [F1]
  TVP_KAPPA1     <- 0.99                # baseline forgetting factor, coefficients
  TVP_KAPPA2     <- 0.99                # baseline decay factor, error covariance
  TVP_TRAIN_SET  <- c(36, 48)           # 24-month prior excluded: unstable in Layer 1
  TVP_KAPPA_SET  <- list(c(0.99, 0.96), # (kappa1, kappa2) sensitivity (referee 2.2)
                         c(0.99, 0.98),
                         c(0.99, 0.99),
                         c(0.98, 0.96))
  TVP_ROOT_MAX   <- 0.999                # local VAR stability threshold
  TVP_STATE_SHRINK <- 0.99               # next-state shrink after rejected update

  GIRF_H         <- 10

  ## Targeted final robustness checks (Layer 2 only). These do not replace the
  ## lag-1 baseline selected by the information criteria.
  BREAK_TRIM     <- 0.15                # Bai-Perron/supF minimum segment share
  ZA_LAG         <- 2                   # Zivot-Andrews augmentation order
  ROBUST_VAR_LAG <- 2                   # residual-diagnostic robustness model

  RUN_FREQUENCY  <- TRUE                # Barunik-Krehlik style band decomposition
  FREQ_GRID      <- 500                 # frequency grid points on (0, pi]
  ## Band definitions in MONTHS (period). Nyquist = 2 months.
  FREQ_BANDS     <- list("short_2_6m"  = c(2, 6),
                         "medium_6_12m" = c(6, 12),
                         "long_12m_Inf" = c(12, Inf))

  RUN_TVP_DGP_SENSITIVITY <- FALSE      # [F3] optional, honest label, OFF by default
  TVP_DGP_REPS            <- 300

  ## Event windows in Gregorian reporting months, inclusive. Dates represent
  ## Jalali period ends, so the full stated Gregorian months are included.
  EVENT_2018 <- as.Date(c("2018-04-01", "2018-10-31"))
  EVENT_2020 <- as.Date(c("2020-02-01", "2020-09-30"))

  MIN_SUBSAMPLE_OBS <- 60               # minimum obs to estimate a 5-var VAR(1)

  ## --------------------------- Packages ------------------------------------- ##
  if (is.null(getOption("repos")) || is.na(getOption("repos")["CRAN"]) ||
      getOption("repos")["CRAN"] == "@CRAN@")
    options(repos = c(CRAN = "https://cloud.r-project.org"))
  need <- c("readxl", "writexl", "zoo", "vars", "tseries", "strucchange", "urca")
  inst <- rownames(installed.packages())
  miss <- setdiff(need, inst)
  if (length(miss) > 0)
    stop("Missing required R packages: ", paste(miss, collapse = ", "),
         ". Install once with install.packages(c(",
         paste(sprintf("'%s'", miss), collapse = ", "), ")).")
  HAS_CA <- requireNamespace("ConnectednessApproach", quietly = TRUE)  # optional cross-check

  OUT_ROOT <- file.path(MASTER_OUT_DIR, "layers_1_2")
  dir.create(OUT_ROOT, recursive = TRUE, showWarnings = FALSE)
  cat("Outputs will be written to:", OUT_ROOT, "\n")
  LOG_FILE <- file.path(OUT_ROOT, "00_run_log.txt")
  sink(LOG_FILE, split = TRUE)
  cat("Run started:", format(Sys.time()), "\n")
  cat("Data file  :", DATA_FILE, "\n")
  cat("ConnectednessApproach available for cross-check:", HAS_CA, "\n\n")

  ## --------------------------- 1. Load data --------------------------------- ##
  ## Auto-detect the sheet and the columns (handles both 'markets' and 'Sheet1'
  ## style layouts of the supplied workbook).
  sheets <- readxl::excel_sheets(DATA_FILE)
  pick_sheet <- NULL
  for (s in sheets) {
    d <- suppressMessages(readxl::read_excel(DATA_FILE, sheet = s, n_max = 5))
    nm <- tolower(names(d))
    if (any(grepl("tedpix|stock", nm)) && any(grepl("cpi", nm))) { pick_sheet <- s; break }
  }
  if (is.null(pick_sheet)) stop("Could not find a sheet containing market data in ", DATA_FILE)
  raw <- suppressMessages(readxl::read_excel(DATA_FILE, sheet = pick_sheet))
  nm  <- tolower(names(raw))
  cat("Using sheet:", pick_sheet, " | columns:", paste(names(raw), collapse = ", "), "\n")

  find_col <- function(patterns) {
    hit <- which(Reduce(`|`, lapply(patterns, function(p) grepl(p, nm))))
    if (length(hit) == 0) stop("Column not found for patterns: ", paste(patterns, collapse = "|"))
    hit[1]
  }
  col_date    <- find_col(c("^date$", "month", "jalali"))
  col_stock   <- find_col(c("tedpix", "stock"))
  col_usd     <- find_col(c("^usd", "dollar"))
  col_gold    <- find_col(c("gold", "sekeh", "coin"))
  col_housing <- find_col(c("housing"))
  col_vehicle <- find_col(c("vehicle", "car"))
  col_cpi     <- find_col(c("^cpi"))

  levels_df <- data.frame(
    month_jalali = as.character(raw[[col_date]]),
    USD     = as.numeric(raw[[col_usd]]),
    GOLD    = as.numeric(raw[[col_gold]]),
    STOCK   = as.numeric(raw[[col_stock]]),
    VEHICLE = as.numeric(raw[[col_vehicle]]),
    HOUSING = as.numeric(raw[[col_housing]]),
    CPI     = as.numeric(raw[[col_cpi]])
  )
  if (anyNA(levels_df)) stop("Missing values detected in the input data workbook.")
  if (anyDuplicated(levels_df$month_jalali)) stop("Duplicate months detected in the input data.")
  if (any(levels_df[, c("USD", "GOLD", "STOCK", "VEHICLE", "HOUSING", "CPI")] <= 0))
    stop("All price/index levels must be strictly positive before log transformation.")
  varnames <- c("USD", "GOLD", "STOCK", "VEHICLE", "HOUSING")
  K <- length(varnames)

  ## Jalali "yyyy/mm" -> exact Gregorian date of the END of the Jalali month.
  ## This is the reporting calendar used throughout the paper: each Jalali month
  ## is labelled by the Gregorian month containing its final day. The resulting
  ## Date is also used for calendar-based event windows and Layer-4 year alignment.
  jalali_year_start <- function(jy) {
    breaks <- c(-61L, 9L, 38L, 199L, 426L, 686L, 756L, 818L, 1111L,
                1181L, 1210L, 1635L, 2060L, 2097L, 2192L, 2262L,
                2324L, 2394L, 2456L, 3178L)
    if (jy < breaks[1L] || jy >= tail(breaks, 1L))
      stop("Jalali year is outside the supported conversion range.")

    idiv <- function(a, b) trunc(a / b)
    imod <- function(a, b) a - idiv(a, b) * b

    gy <- jy + 621L
    leap_j <- -14L
    jp <- breaks[1L]
    jump <- NA_integer_

    for (k in 2:length(breaks)) {
      jm_break <- breaks[k]
      jump <- jm_break - jp
      if (jy < jm_break) break
      leap_j <- leap_j + idiv(jump, 33L) * 8L +
        idiv(imod(jump, 33L), 4L)
      jp <- jm_break
    }

    n <- jy - jp
    leap_j <- leap_j + idiv(n, 33L) * 8L +
      idiv(imod(n, 33L) + 3L, 4L)
    if (imod(jump, 33L) == 4L && (jump - n) == 4L)
      leap_j <- leap_j + 1L

    leap_g <- idiv(gy, 4L) -
      idiv((idiv(gy, 100L) + 1L) * 3L, 4L) - 150L
    march_day <- 20L + leap_j - leap_g

    as.Date(sprintf("%04d-03-%02d", gy, march_day))
  }

  jalali_to_date <- function(s) {
    p <- strsplit(s, "/", fixed = TRUE)[[1]]
    if (length(p) != 2L) stop("Jalali month must use yyyy/mm format.")
    jy <- suppressWarnings(as.integer(p[1L]))
    jm <- suppressWarnings(as.integer(p[2L]))
    if (is.na(jy) || is.na(jm) || jm < 1L || jm > 12L)
      stop("Invalid Jalali month: ", s)

    off <- c(0L, 31L, 62L, 93L, 124L, 155L,
             186L, 216L, 246L, 276L, 306L, 336L)

    next_month_start <- if (jm < 12L) {
      jalali_year_start(jy) + off[jm + 1L]
    } else {
      jalali_year_start(jy + 1L)
    }

    next_month_start - 1L
  }
  dates_levels <- as.Date(vapply(levels_df$month_jalali, function(s) as.numeric(jalali_to_date(s)),
                                 numeric(1)), origin = "1970-01-01")
  if (any(diff(dates_levels) <= 0)) stop("Input months must be strictly increasing.")
  if (any(diff(dates_levels) > 40)) stop("A gap larger than one month was detected in the input data.")
  cat("Sample (levels):", format(min(dates_levels)), "to", format(max(dates_levels)),
      "| n =", nrow(levels_df), "months\n")

  ## --------------------------- 2. Returns ----------------------------------- ##
  logret <- function(x) 100 * diff(log(x))
  ret_nominal <- sapply(levels_df[, varnames], logret)
  infl        <- logret(levels_df$CPI)
  ret_real    <- ret_nominal - infl                     # real = nominal - inflation
  dates_ret   <- dates_levels[-1]
  jal_ret     <- levels_df$month_jalali[-1]
  Tn <- nrow(ret_nominal)
  cat("Return sample: n =", Tn, "months\n\n")

  LAYERS <- list(
    layer1_nominal = list(Y = ret_nominal, label = "Layer 1: nominal monthly-average log returns (x100)"),
    layer2_real    = list(Y = ret_real,    label = "Layer 2: CPI-adjusted monthly-average log returns (x100)")
  )

  ## ====================== 3. Core connectedness functions ==================== ##
  ## Transparent, auditable implementations. [F8]

  ## MA (Wold) matrices Psi_0..Psi_{H-1} from a vars::VAR object (any lag p).
  get_psi <- function(var_model, H) {
    Ph <- vars::Phi(var_model, nstep = H - 1)   # array K x K x H, Phi_0 = I
    Ph
  }
  get_sigma <- function(var_model) {
    e <- stats::residuals(var_model)
    crossprod(e) / (nrow(e) - ncol(e) * var_model$p - 1)   # df-adjusted
  }

  ## Generalized FEVD (Pesaran-Shin), row-normalized to 100.
  ## Entry [i, j] = share of variable i's H-step FEV due to shocks in j.
  gfevd <- function(Psi, Sigma, H) {
    K <- ncol(Sigma)
    num <- matrix(0, K, K); den <- numeric(K)
    sjj <- diag(Sigma)
    for (h in 1:H) {
      P  <- Psi[, , h]
      PS <- P %*% Sigma
      num <- num + (PS^2) / matrix(sjj, K, K, byrow = TRUE)
      den <- den + diag(P %*% Sigma %*% t(P))
    }
    th <- num / den
    th <- th / rowSums(th)          # row normalization
    100 * th
  }

  ## Connectedness summary from a GFEVD table.
  conn_summary <- function(CT, varnames) {
    K <- ncol(CT)
    dimnames(CT) <- list(varnames, varnames)
    FROM <- rowSums(CT) - diag(CT)
    TO   <- colSums(CT) - diag(CT)
    NET  <- TO - FROM
    TCI  <- sum(FROM) / K
    ## NPDC: unique pairs, explicit orientation [F5]
    ## npdc_ij = CT[i, j] - CT[j, i] > 0  <=>  j is a NET transmitter to i.
    pairs <- t(combn(K, 2))
    NPDC <- data.frame(
      Var_i = varnames[pairs[, 1]],
      Var_j = varnames[pairs[, 2]],
      NPDC_ij = CT[cbind(pairs[, 1], pairs[, 2])] - CT[cbind(pairs[, 2], pairs[, 1])]
    )
    NPDC$Net_transmitter <- ifelse(NPDC$NPDC_ij > 0, NPDC$Var_j, NPDC$Var_i)
    NPDC$Net_receiver    <- ifelse(NPDC$NPDC_ij > 0, NPDC$Var_i, NPDC$Var_j)
    NPDC$Abs_NPDC        <- abs(NPDC$NPDC_ij)
    list(table = CT, FROM = FROM, TO = TO, NET = NET, TCI = TCI, NPDC = NPDC)
  }

  static_connectedness <- function(Y, p = N_LAG, H = H_GFEVD, exogen = NULL) {
    vm <- vars::VAR(Y, p = p, type = "const", exogen = exogen)
    cs <- conn_summary(gfevd(get_psi(vm, H), get_sigma(vm), H), colnames(Y))
    cs$model <- vm
    cs
  }

  ## Generalized impulse responses (in return units, one s.d. shocks).
  girf <- function(var_model, H) {
    Psi <- get_psi(var_model, H + 1)
    Sig <- get_sigma(var_model)
    K <- ncol(Sig); sj <- sqrt(diag(Sig))
    out <- array(NA_real_, dim = c(H + 1, K, K),
                 dimnames = list(paste0("h", 0:H), colnames(Sig), colnames(Sig)))
    for (h in 0:H) out[h + 1, , ] <- Psi[, , h + 1] %*% Sig %*% diag(1 / sj)
    out   # [horizon, response, shock]
  }

  ## ====================== 4. TVP-VAR with a clean prior ====================== ##
  ## Koop-Korobilis forgetting-factor Kalman filter with a STRICT split:
  ##   * prior = OLS on observations 1..train_n ONLY;
  ##   * Kalman updates start at t = train_n + 1 (training data are NOT fed
  ##     through the update step a second time);
  ##   * the first reported point is exactly the first post-training month.
  ##   * an unstable local coefficient update is not reported and is not turned
  ##     into NA. The last stable coefficient state is carried forward for that
  ##     date, while the internal next-state slopes are shrunk by 0.99. The raw
  ##     and used roots and every intervention are exported for audit.
  ## No look-ahead, no double use of the training sample, and no silent deletion
  ## of unstable months.                                                [F1]
  tvp_var_filter <- function(Y, p = 1, train_n = TRAIN_N,
                             kappa1 = 0.99, kappa2 = 0.99, H = H_GFEVD,
                             root_max = TVP_ROOT_MAX,
                             state_shrink = TVP_STATE_SHRINK) {
    if (p != 1) stop("The custom TVP filter is implemented for p = 1 only.")
    K <- ncol(Y); Tn <- nrow(Y)
    X <- cbind(1, Y[1:(Tn - p), , drop = FALSE])         # p = 1 regressors
    Yt <- Y[(p + 1):Tn, , drop = FALSE]
    m  <- ncol(X); d <- K * m; Te <- nrow(Yt)
    if (train_n <= m + 5) stop("train_n too small for the prior OLS.")
    if (train_n >= Te - 5) stop("train_n leaves too few post-training observations.")
    ## ---- prior from training window only ----
    Xtr <- X[1:train_n, , drop = FALSE]; Ytr <- Yt[1:train_n, , drop = FALSE]
    XtXi <- solve(crossprod(Xtr))
    Btr  <- XtXi %*% crossprod(Xtr, Ytr)                 # m x K
    Etr  <- Ytr - Xtr %*% Btr
    S0   <- crossprod(Etr) / (train_n - m)
    prior_A <- t(Btr[2:m, , drop = FALSE])
    prior_root <- max(Mod(eigen(prior_A, only.values = TRUE)$values))
    if (!is.finite(prior_root) || prior_root >= root_max)
      stop(sprintf("TVP prior is unstable for train_n=%d (root=%.4f >= %.3f).",
                   train_n, prior_root, root_max))
    b_filter <- as.vector(Btr)                            # internal state for next update
    b_report_prev <- b_filter                             # last coefficient state reported
    P    <- kronecker(S0, XtXi)                          # Var(vec(B_ols))
    Sig  <- S0
    ## ---- filter over POST-TRAINING observations only ----
    TCI_path <- rep(NA_real_, Te)
    NET_path <- matrix(NA_real_, Te, K, dimnames = list(NULL, colnames(Y)))
    root_raw <- root_used <- rep(NA_real_, Te)
    action <- rep("training", Te)
    IK <- diag(K)
    for (t in (train_n + 1):Te) {
      b_before <- b_filter
      Zt <- kronecker(IK, matrix(X[t, ], nrow = 1))      # K x d
      P  <- P / kappa1                                   # forgetting (state noise)
      Sig_prev <- Sig
      e_pred <- as.numeric(Yt[t, ]) - as.numeric(Zt %*% b_before)
      Sig_pred <- kappa2 * Sig_prev + (1 - kappa2) * tcrossprod(e_pred)
      Sig_pred <- (Sig_pred + t(Sig_pred)) / 2
      Ft <- Zt %*% P %*% t(Zt) + Sig_pred
      Kt <- P %*% t(Zt) %*% solve(Ft)
      b_candidate <- b_before + as.numeric(Kt %*% e_pred)
      P <- P - Kt %*% Zt %*% P
      P <- (P + t(P)) / 2

      ## Complete the two-stage EWMA recursion: the one-step-ahead covariance
      ## above enters the Kalman gain, while the filtered covariance carried to
      ## the next date is based on the post-update residual.
      e_filt <- as.numeric(Yt[t, ]) - as.numeric(Zt %*% b_candidate)
      Sig <- kappa2 * Sig_prev + (1 - kappa2) * tcrossprod(e_filt)
      Sig <- (Sig + t(Sig)) / 2

      B_candidate <- matrix(b_candidate, m, K)
      A_candidate <- t(B_candidate[2:m, , drop = FALSE])
      root_candidate <- tryCatch(
        max(Mod(eigen(A_candidate, only.values = TRUE)$values)),
        error = function(e) Inf)
      root_raw[t] <- root_candidate

      if (is.finite(root_candidate) && root_candidate < root_max) {
        b_report <- b_candidate
        b_report_prev <- b_candidate
        b_filter <- b_candidate
        action[t] <- "updated"
      } else {
        ## Same substantive control as the package implementation: report the
        ## previous stable coefficients. Only the internal slope state used for
        ## the next prediction is shrunk; the intercept is left unchanged.
        b_report <- b_report_prev
        B_next <- matrix(b_before, m, K)
        B_next[2:m, ] <- state_shrink * B_next[2:m, ]
        b_filter <- as.vector(B_next)
        action[t] <- "carried_forward_unstable_update"
      }

      B_used <- matrix(b_report, m, K)
      A_used <- t(B_used[2:m, , drop = FALSE])
      root_used[t] <- max(Mod(eigen(A_used, only.values = TRUE)$values))
      if (!is.finite(root_used[t]) || root_used[t] >= root_max)
        stop("Internal error: the TVP state used for connectedness is not stable.")
      Psi <- array(NA_real_, dim = c(K, K, H)); Ph <- diag(K)
      for (h in 1:H) { Psi[, , h] <- Ph; Ph <- A_used %*% Ph }
      cst <- conn_summary(gfevd(Psi, Sig, H), colnames(Y))
      TCI_path[t] <- cst$TCI; NET_path[t, ] <- cst$NET
    }
    keep <- (train_n + 1):Te                             # exactly the filtered points
    if (any(!is.finite(TCI_path[keep])) || any(!is.finite(NET_path[keep, ])))
      stop("TVP filter produced a missing/non-finite reported value.")
    list(TCI = TCI_path, NET = NET_path, keep = keep,
         train_n = train_n, kappa1 = kappa1, kappa2 = kappa2,
         prior_root = prior_root,
         stability = data.frame(Filter_row = seq_len(Te), Raw_root = root_raw,
                                Used_root = root_used, Action = action),
         n_carried_forward = sum(action[keep] != "updated"))
  }

  ## Reference-recursion sensitivity check.
  ## Identical to the validated baseline TVP filter except for covariance timing:
  ## baseline = two-stage prediction-error gain + filtered-error carried state;
  ## reference = one-stage prediction-error covariance for gain/current GFEVD/next state.
  tvp_var_filter_reference <- function(Y, p = 1, train_n = TRAIN_N,
                             kappa1 = 0.99, kappa2 = 0.99, H = H_GFEVD,
                             root_max = TVP_ROOT_MAX,
                             state_shrink = TVP_STATE_SHRINK) {
    if (p != 1) stop("The TVP reference filter is implemented for p = 1 only.")
    K <- ncol(Y); Tn <- nrow(Y)
    X <- cbind(1, Y[1:(Tn - p), , drop = FALSE])         # p = 1 regressors
    Yt <- Y[(p + 1):Tn, , drop = FALSE]
    m  <- ncol(X); d <- K * m; Te <- nrow(Yt)
    if (train_n <= m + 5) stop("train_n too small for the prior OLS.")
    if (train_n >= Te - 5) stop("train_n leaves too few post-training observations.")
    ## ---- prior from training window only ----
    Xtr <- X[1:train_n, , drop = FALSE]; Ytr <- Yt[1:train_n, , drop = FALSE]
    XtXi <- solve(crossprod(Xtr))
    Btr  <- XtXi %*% crossprod(Xtr, Ytr)                 # m x K
    Etr  <- Ytr - Xtr %*% Btr
    S0   <- crossprod(Etr) / (train_n - m)
    prior_A <- t(Btr[2:m, , drop = FALSE])
    prior_root <- max(Mod(eigen(prior_A, only.values = TRUE)$values))
    if (!is.finite(prior_root) || prior_root >= root_max)
      stop(sprintf("TVP prior is unstable for train_n=%d (root=%.4f >= %.3f).",
                   train_n, prior_root, root_max))
    b_filter <- as.vector(Btr)                            # internal state for next update
    b_report_prev <- b_filter                             # last coefficient state reported
    P    <- kronecker(S0, XtXi)                          # Var(vec(B_ols))
    Sig  <- S0
    ## ---- filter over POST-TRAINING observations only ----
    TCI_path <- rep(NA_real_, Te)
    NET_path <- matrix(NA_real_, Te, K, dimnames = list(NULL, colnames(Y)))
    root_raw <- root_used <- rep(NA_real_, Te)
    action <- rep("training", Te)
    IK <- diag(K)
    for (t in (train_n + 1):Te) {
      b_before <- b_filter
      Zt <- kronecker(IK, matrix(X[t, ], nrow = 1))      # K x d
      P  <- P / kappa1                                   # forgetting (state noise)
      Sig_prev <- Sig
      e_pred <- as.numeric(Yt[t, ]) - as.numeric(Zt %*% b_before)

      ## One-stage prediction-error covariance reference:
      ## the same EWMA covariance enters the Kalman gain, is used for current
      ## connectedness, and is carried to the next date.
      Sig <- kappa2 * Sig_prev + (1 - kappa2) * tcrossprod(e_pred)
      Sig <- (Sig + t(Sig)) / 2
      Ft <- Zt %*% P %*% t(Zt) + Sig
      Kt <- P %*% t(Zt) %*% solve(Ft)
      b_candidate <- b_before + as.numeric(Kt %*% e_pred)
      P <- P - Kt %*% Zt %*% P
      P <- (P + t(P)) / 2

      B_candidate <- matrix(b_candidate, m, K)
      A_candidate <- t(B_candidate[2:m, , drop = FALSE])
      root_candidate <- tryCatch(
        max(Mod(eigen(A_candidate, only.values = TRUE)$values)),
        error = function(e) Inf)
      root_raw[t] <- root_candidate

      if (is.finite(root_candidate) && root_candidate < root_max) {
        b_report <- b_candidate
        b_report_prev <- b_candidate
        b_filter <- b_candidate
        action[t] <- "updated"
      } else {
        ## Same substantive control as the package implementation: report the
        ## previous stable coefficients. Only the internal slope state used for
        ## the next prediction is shrunk; the intercept is left unchanged.
        b_report <- b_report_prev
        B_next <- matrix(b_before, m, K)
        B_next[2:m, ] <- state_shrink * B_next[2:m, ]
        b_filter <- as.vector(B_next)
        action[t] <- "carried_forward_unstable_update"
      }

      B_used <- matrix(b_report, m, K)
      A_used <- t(B_used[2:m, , drop = FALSE])
      root_used[t] <- max(Mod(eigen(A_used, only.values = TRUE)$values))
      if (!is.finite(root_used[t]) || root_used[t] >= root_max)
        stop("Internal error: the TVP state used for connectedness is not stable.")
      Psi <- array(NA_real_, dim = c(K, K, H)); Ph <- diag(K)
      for (h in 1:H) { Psi[, , h] <- Ph; Ph <- A_used %*% Ph }
      cst <- conn_summary(gfevd(Psi, Sig, H), colnames(Y))
      TCI_path[t] <- cst$TCI; NET_path[t, ] <- cst$NET
    }
    keep <- (train_n + 1):Te                             # exactly the filtered points
    if (any(!is.finite(TCI_path[keep])) || any(!is.finite(NET_path[keep, ])))
      stop("TVP filter produced a missing/non-finite reported value.")
    list(TCI = TCI_path, NET = NET_path, keep = keep,
         train_n = train_n, kappa1 = kappa1, kappa2 = kappa2,
         prior_root = prior_root,
         stability = data.frame(Filter_row = seq_len(Te), Raw_root = root_raw,
                                Used_root = root_used, Action = action),
         n_carried_forward = sum(action[keep] != "updated"))
  }

  ## TVP sensitivity engine (referee 2.1-2.3): runs a set of specifications and
  ## reports, per spec over a COMMON post-training window: TCI mean & sd,
  ## correlation of the TCI path with the baseline spec, mean NET per market,
  ## share of months with positive NET per market.
  tvp_sensitivity <- function(Y, train_set = TVP_TRAIN_SET, kappa_set = TVP_KAPPA_SET,
                              base_train = TRAIN_N, base_k = c(TVP_KAPPA1, TVP_KAPPA2)) {
    specs <- list()
    for (tr in train_set)
      specs[[sprintf("train%02d_k%.2f_%.2f", tr, base_k[1], base_k[2])]] <-
        list(train = tr, k1 = base_k[1], k2 = base_k[2])
    for (kk in kappa_set)
      specs[[sprintf("train%02d_k%.2f_%.2f", base_train, kk[1], kk[2])]] <-
        list(train = base_train, k1 = kk[1], k2 = kk[2])
    specs <- specs[!duplicated(names(specs))]
    attempts <- lapply(specs, function(s) tryCatch(
      tvp_var_filter(Y, train_n = s$train, kappa1 = s$k1, kappa2 = s$k2),
      error = function(e) structure(list(message = conditionMessage(e)), class = "tvp_error")))
    valid <- !vapply(attempts, inherits, logical(1), what = "tvp_error")
    spec_status <- data.frame(
      Spec = names(specs), Included = valid,
      Note = vapply(seq_along(attempts), function(i)
        if (valid[i]) "included" else attempts[[i]]$message, character(1)))
    fits <- attempts[valid]
    base_name <- sprintf("train%02d_k%.2f_%.2f", base_train, base_k[1], base_k[2])
    if (!base_name %in% names(fits)) stop("The baseline TVP specification is invalid.")
    ## Complete-calendar intersection after the largest INCLUDED training window.
    ## No pairwise deletion and no na.rm comparison are permitted.
    last_row <- nrow(fits[[1]]$NET)
    eligible <- (max(vapply(fits, `[[`, numeric(1), "train_n")) + 1):last_row
    is_complete <- vapply(eligible, function(tt) all(vapply(fits, function(f)
      is.finite(f$TCI[tt]) && all(is.finite(f$NET[tt, ])), logical(1))), logical(1))
    common <- eligible[is_complete]
    if (length(common) < 24 || any(diff(common) != 1))
      stop("TVP specifications do not share a sufficiently long contiguous complete window.")
    base_tci <- fits[[base_name]]$TCI[common]
    K <- ncol(Y); vn <- colnames(Y)
    stats <- do.call(rbind, lapply(names(fits), function(nm) {
      f <- fits[[nm]]; tci <- f$TCI[common]; net <- f$NET[common, , drop = FALSE]
      row <- data.frame(Spec = nm, Train_n = f$train_n, kappa1 = f$kappa1, kappa2 = f$kappa2,
                        Is_baseline = nm == base_name,
                        TCI_mean = mean(tci), TCI_sd = sd(tci),
                        Corr_with_baseline = suppressWarnings(cor(tci, base_tci)),
                        N_common_months = length(common),
                        N_carried_forward = f$n_carried_forward,
                        Pct_carried_forward = 100 * f$n_carried_forward / length(f$keep),
                        Prior_root = f$prior_root,
                        Max_raw_root = max(f$stability$Raw_root[f$keep]),
                        Max_used_root = max(f$stability$Used_root[f$keep]))
      for (v in vn) {
        row[[paste0("NETmean_", v)]] <- mean(net[, v])
        row[[paste0("PctPos_", v)]]  <- 100 * mean(net[, v] > 0)
      }
      row
    }))
    ## role-sign stability across specifications (referee 2.3)
    roles <- do.call(rbind, lapply(vn, function(v) {
      sg <- sign(stats[[paste0("NETmean_", v)]])
      data.frame(Segment = v,
                 N_specs_positive = sum(sg > 0), N_specs_negative = sum(sg < 0),
                 Sign_flips_across_specs = length(unique(sg)) > 1)
    }))
    list(specs = names(fits), fits = fits, stats = stats, roles = roles,
         common = common, base_name = base_name, spec_status = spec_status)
  }

  ## ====================== 5. Frequency decomposition (BK) ===================== ##
  ## Spectral generalized FEVD aggregated over period bands (months).   [F8]
  freq_connectedness <- function(var_model, bands = FREQ_BANDS, ngrid = FREQ_GRID) {
    A <- vars::Acoef(var_model); p <- length(A); K <- ncol(A[[1]])
    Sig <- get_sigma(var_model); sjj <- diag(Sig)
    omg <- seq(1e-4, pi, length.out = ngrid)
    num <- array(0, dim = c(K, K, ngrid)); den <- matrix(0, K, ngrid)
    for (s in seq_along(omg)) {
      Aw <- diag(K) + 0i
      for (l in 1:p) Aw <- Aw - A[[l]] * exp(-1i * omg[s] * l)
      Pw <- solve(Aw)
      PS <- Pw %*% Sig
      num[, , s] <- (Mod(PS)^2) / matrix(sjj, K, K, byrow = TRUE)
      den[, s]   <- Re(diag(Pw %*% Sig %*% Conj(t(Pw))))
    }
    den_tot <- rowSums(den)
    ## Proper BK normalization: unnormalized total-frequency table, then
    ## row-normalize so that band tables sum EXACTLY to the DY total table.
    num_tot <- apply(num, c(1, 2), sum)
    Th_all  <- num_tot / den_tot                # unnormalized theta (rows / den_i)
    row_tot <- rowSums(Th_all)
    out <- list()
    for (bn in names(bands)) {
      per <- bands[[bn]]                                  # months (period)
      lo  <- if (is.infinite(per[2])) 0 else 2 * pi / per[2]
      hi  <- 2 * pi / per[1]
      idx <- which(omg > lo & omg <= hi)
      CTb <- 100 * (apply(num[, , idx, drop = FALSE], c(1, 2), sum) / den_tot) / row_tot
      dimnames(CTb) <- list(colnames(Sig), colnames(Sig))
      FROMb <- rowSums(CTb) - diag(CTb); TOb <- colSums(CTb) - diag(CTb)
      out[[bn]] <- list(table = CTb, FROM = FROMb, TO = TOb,
                        NET = TOb - FROMb, TCI = sum(FROMb) / K)
    }
    out
  }

  ## ====================== 6. Bootstrap (static model) ======================== ##
  draw_boot_residuals <- function(E, type = BOOT_TYPE, block_len = BOOT_BLOCK_LEN) {
    Te <- nrow(E)
    if (type == "wild")
      return(E * sample(c(-1, 1), Te, replace = TRUE))
    if (type == "iid")
      return(E[sample.int(Te, replace = TRUE), , drop = FALSE])
    if (type == "wild_block") {
      if (block_len < 2 || block_len > Te) stop("Invalid BOOT_BLOCK_LEN.")
      offset <- sample.int(block_len, 1) - 1
      block_id <- ((seq_len(Te) - 1 + offset) %/% block_len) + 1
      signs <- sample(c(-1, 1), max(block_id), replace = TRUE)
      return(E * signs[block_id])
    }
    stop("Unknown BOOT_TYPE: ", type)
  }

  boot_static <- function(var_model, Y, B = BOOT_REPS, H = H_GFEVD,
                          type = BOOT_TYPE, seed = BOOT_SEED, girf_H = GIRF_H,
                          block_len = BOOT_BLOCK_LEN) {
    set.seed(seed)
    K <- ncol(Y); p <- var_model$p
    A <- vars::Acoef(var_model); cst <- vars::Bcoef(var_model)[, "const"]
    E <- stats::residuals(var_model); Te <- nrow(E)
    base_cs <- conn_summary(gfevd(get_psi(var_model, H), get_sigma(var_model), H),
                            colnames(Y))
    TCIb <- numeric(0)
    NETb <- matrix(NA_real_, B, K, dimnames = list(NULL, colnames(Y)))
    npdc_names <- apply(t(combn(colnames(Y), 2)), 1, paste, collapse = "__")
    NPDCb <- matrix(NA_real_, B, length(npdc_names), dimnames = list(NULL, npdc_names))
    GIRFb <- array(NA_real_, dim = c(B, girf_H + 1, K, K))
    for (bb in 1:B) {
      if (bb %% 100 == 0) cat("  bootstrap", bb, "of", B, "\n")
      Eb <- draw_boot_residuals(E, type = type, block_len = block_len)
      Yb <- matrix(NA_real_, Te + p, K); Yb[1:p, ] <- Y[1:p, , drop = FALSE]
      for (t in 1:Te) {
        acc <- cst
        for (l in 1:p) acc <- acc + A[[l]] %*% Yb[t + p - l, ]
        Yb[t + p, ] <- acc + Eb[t, ]
      }
      colnames(Yb) <- colnames(Y)
      fit <- tryCatch(vars::VAR(Yb, p = p, type = "const"), error = function(e) NULL)
      if (is.null(fit)) next
      if (max(vars::roots(fit)) >= 1) next
      cs <- tryCatch(conn_summary(gfevd(get_psi(fit, H), get_sigma(fit), H), colnames(Y)),
                     error = function(e) NULL)
      if (is.null(cs)) next
      TCIb <- c(TCIb, cs$TCI); NETb[bb, ] <- cs$NET
      NPDCb[bb, ] <- cs$NPDC$NPDC_ij
      GIRFb[bb, , , ] <- tryCatch(girf(fit, girf_H), error = function(e) NA)
    }
    if (length(TCIb) < 0.9 * B || sum(complete.cases(NETb)) < 0.9 * B ||
        sum(complete.cases(NPDCb)) < 0.9 * B)
      stop("Too few successful stable static-bootstrap replications.")
    q <- function(x, p) as.numeric(quantile(x, p, na.rm = TRUE))
    valid_net <- complete.cases(NETb)
    valid_npdc <- complete.cases(NPDCb)
    net_max_dev <- apply(abs(sweep(NETb[valid_net, , drop = FALSE], 2,
                                   base_cs$NET, "-")), 1, max)
    npdc_max_dev <- apply(abs(sweep(NPDCb[valid_npdc, , drop = FALSE], 2,
                                    base_cs$NPDC$NPDC_ij, "-")), 1, max)
    net_crit <- q(net_max_dev, .95)
    npdc_crit <- q(npdc_max_dev, .95)
    list(TCI = c(Lower95 = q(TCIb, .025), Upper95 = q(TCIb, .975), Mean = mean(TCIb, na.rm = TRUE)),
         NET = data.frame(Segment = colnames(Y),
                          Pointwise_Lower95 = apply(NETb, 2, q, .025),
                          Pointwise_Upper95 = apply(NETb, 2, q, .975),
                          Simultaneous_Lower95 = base_cs$NET - net_crit,
                          Simultaneous_Upper95 = base_cs$NET + net_crit,
                          BootMean = colMeans(NETb, na.rm = TRUE)),
         NPDC = data.frame(Pair = npdc_names,
                           Pointwise_Lower95 = apply(NPDCb, 2, q, .025),
                           Pointwise_Upper95 = apply(NPDCb, 2, q, .975),
                           Simultaneous_Lower95 = base_cs$NPDC$NPDC_ij - npdc_crit,
                           Simultaneous_Upper95 = base_cs$NPDC$NPDC_ij + npdc_crit,
                           BootMean = colMeans(NPDCb, na.rm = TRUE)),
         GIRF_lo = apply(GIRFb, c(2, 3, 4), q, .025),
         GIRF_hi = apply(GIRFb, c(2, 3, 4), q, .975),
         reps_used = length(TCIb))
  }

  ## Paired moving-block bootstrap for the estimand Layer 2 minus Layer 1.
  ## The same resampled months are used for nominal returns and CPI inflation,
  ## preserving the algebra real_return = nominal_return - inflation in every draw.
  circular_block_indices <- function(n, block_len) {
    idx <- integer(0)
    while (length(idx) < n) {
      s <- sample.int(n, 1)
      idx <- c(idx, ((s - 1 + 0:(block_len - 1)) %% n) + 1)
    }
    idx[seq_len(n)]
  }

  boot_layer_difference <- function(Y_nominal, inflation, cs_nominal, cs_real,
                                    B = PAIRED_BOOT_REPS,
                                    block_len = PAIRED_BOOT_BLOCK_LEN,
                                    seed = PAIRED_BOOT_SEED) {
    set.seed(seed)
    n <- nrow(Y_nominal); K <- ncol(Y_nominal)
    if (length(inflation) != n) stop("Inflation and nominal-return samples do not align.")
    d_tci <- rep(NA_real_, B)
    d_net <- matrix(NA_real_, B, K, dimnames = list(NULL, colnames(Y_nominal)))
    for (bb in seq_len(B)) {
      if (bb %% 100 == 0) cat("  paired bootstrap", bb, "of", B, "\n")
      ii <- circular_block_indices(n, block_len)
      Yn <- Y_nominal[ii, , drop = FALSE]
      Yr <- sweep(Yn, 1, inflation[ii], "-")
      cn <- tryCatch(static_connectedness(Yn), error = function(e) NULL)
      cr <- tryCatch(static_connectedness(Yr), error = function(e) NULL)
      if (is.null(cn) || is.null(cr)) next
      if (max(vars::roots(cn$model)) >= 1 || max(vars::roots(cr$model)) >= 1) next
      d_tci[bb] <- cr$TCI - cn$TCI
      d_net[bb, ] <- cr$NET - cn$NET
    }
    used <- sum(is.finite(d_tci) & complete.cases(d_net))
    if (used < 0.9 * B) stop("Too few successful paired bootstrap replications.")
    ok <- is.finite(d_tci) & complete.cases(d_net)
    q <- function(x, p) as.numeric(quantile(x, p, na.rm = TRUE))
    point_net <- cs_real$NET - cs_nominal$NET
    max_dev <- apply(abs(sweep(d_net[ok, , drop = FALSE], 2, point_net, "-")), 1, max)
    crit <- q(max_dev, .95)
    list(
      TCI = data.frame(
        Measure = "TCI", Estimate_L2_minus_L1 = cs_real$TCI - cs_nominal$TCI,
        Pointwise_Lower95 = q(d_tci[ok], .025),
        Pointwise_Upper95 = q(d_tci[ok], .975), Reps_Used = used),
      NET = data.frame(
        Measure = paste0("NET_", colnames(Y_nominal)),
        Estimate_L2_minus_L1 = point_net,
        Pointwise_Lower95 = apply(d_net[ok, , drop = FALSE], 2, q, .025),
        Pointwise_Upper95 = apply(d_net[ok, , drop = FALSE], 2, q, .975),
        Simultaneous_Lower95 = point_net - crit,
        Simultaneous_Upper95 = point_net + crit,
        Reps_Used = used))
  }

  ## ====================== 7. Diagnostics [F4] ================================ ##
  diagnostics <- function(var_model, Y_levels_log = NULL, Y_ret = NULL) {
    K <- var_model$K; Te <- var_model$obs
    out <- list()
    ## lag selection (for the record)
    out$lag_selection <- as.data.frame(t(vars::VARselect(var_model$y, lag.max = 4,
                                                         type = "const")$selection))
    ## stability
    out$stability <- data.frame(Max_Modulus = max(vars::roots(var_model)),
                                Stable = max(vars::roots(var_model)) < 1)
    ## multivariate serial correlation
    pt <- vars::serial.test(var_model, lags.pt = 12, type = "PT.adjusted")
    out$serial <- data.frame(Test = "Portmanteau (adj), lag 12",
                             Statistic = unname(pt$serial$statistic),
                             P_value = unname(pt$serial$p.value))
    ## multivariate normality: correct extraction  [F4]
    nt <- tryCatch(vars::normality.test(var_model, multivariate.only = TRUE),
                   error = function(e) NULL)
    if (!is.null(nt)) {
      out$normality <- data.frame(
        Test = c("Multivariate Jarque-Bera", "Multivariate skewness", "Multivariate kurtosis"),
        Statistic = c(unname(nt$jb.mul$JB$statistic),
                      unname(nt$jb.mul$Skewness$statistic),
                      unname(nt$jb.mul$Kurtosis$statistic)),
        P_value = c(unname(nt$jb.mul$JB$p.value),
                    unname(nt$jb.mul$Skewness$p.value),
                    unname(nt$jb.mul$Kurtosis$p.value)))
    }
    ## multivariate ARCH: NOT valid here -- documented, not reported  [F4]
    out$arch_note <- paste0(
      "Multivariate ARCH-LM omitted by design: with K=5 the test has ",
      "lags*(K(K+1)/2)^2 = lags*225 parameters vs T=", Te,
      " observations; it is saturated/invalid. Per-equation ARCH screening uses the ",
      "McLeod-Li test (Ljung-Box on squared residuals, 12 lags), reported as ",
      "ARCH_McLeodLi12_p.")
    ## per-equation diagnostics
    E <- stats::residuals(var_model)
    per_eq <- do.call(rbind, lapply(colnames(E), function(v) {
      e <- E[, v]
      lb <- Box.test(e, lag = 12, type = "Ljung-Box", fitdf = var_model$p)
      jb <- tseries::jarque.bera.test(e)
      ar <- Box.test(e^2, lag = 12, type = "Ljung-Box")      # McLeod-Li ARCH screen
      data.frame(Equation = v,
                 LjungBox12_p = lb$p.value, JB_p = jb$p.value,
                 ARCH_McLeodLi12_p = ar$p.value,
                 LjungBox_flag = lb$p.value < 0.05,
                 NonNormal_flag = jb$p.value < 0.05,
                 ARCH_flag = ar$p.value < 0.05)
    }))
    out$per_equation <- per_eq
    ## unit-root screen on returns (ADF, PP, KPSS)   [F4]
    if (!is.null(Y_ret)) {
      out$unit_roots <- do.call(rbind, lapply(colnames(Y_ret), function(v) {
        x <- Y_ret[, v]
        data.frame(Segment = v,
                   ADF_p  = suppressWarnings(tseries::adf.test(x)$p.value),
                   PP_p   = suppressWarnings(tseries::pp.test(x)$p.value),
                   KPSS_p = suppressWarnings(tseries::kpss.test(x)$p.value))
      }))
      out$unit_roots$KPSS_reject_5pct <- out$unit_roots$KPSS_p < 0.05
    }
    out$honest_summary <- paste(
      "Diagnostics summary:",
      sprintf("stable VAR (max root %.3f);", out$stability$Max_Modulus),
      sprintf("Portmanteau p=%.3f;", out$serial$P_value),
      sprintf("%d/%d equations show non-normal residuals;",
              sum(per_eq$NonNormal_flag), K),
      sprintf("%d/%d equations show ARCH effects.", sum(per_eq$ARCH_flag), K),
      "A blanket 'all diagnostics passed' claim is NOT supported; see tables.")
    out
  }

  ## ====================== 7b. Layer-2 targeted robustness ================== ##
  ## These functions are diagnostic add-ons only. They do not alter the validated
  ## Layer-1/Layer-2 baseline estimates or any resampling procedure.

  date_at <- function(idx, dates) {
    idx <- as.integer(round(idx))
    idx[!is.finite(idx)] <- NA_integer_
    idx <- pmax(1L, pmin(length(dates), idx))
    as.character(dates[idx])
  }

  jalali_at <- function(idx, jalali) {
    idx <- as.integer(round(idx))
    idx[!is.finite(idx)] <- NA_integer_
    idx <- pmax(1L, pmin(length(jalali), idx))
    as.character(jalali[idx])
  }

  ## supF plus Bai-Perron multiple-break dating for one scalar diagnostic target.
  ## For HOUSING levels the target is the conditional mean; for squared demeaned
  ## HOUSING returns it is a transparent variance proxy.
  break_scan <- function(x, dates, jalali, target, h = BREAK_TRIM) {
    stopifnot(length(x) == length(dates), length(x) == length(jalali),
              all(is.finite(x)), h > 0, h < 0.5)
    dd <- data.frame(y = as.numeric(x))

    fs <- strucchange::Fstats(y ~ 1, data = dd, from = h)
    sf <- strucchange::sctest(fs, type = "supF")

    bp_full <- strucchange::breakpoints(y ~ 1, data = dd, h = h)
    bp_summary <- summary(bp_full)
    bic_values <- as.numeric(bp_summary$RSS["BIC", , drop = TRUE])
    bic_breaks <- suppressWarnings(as.integer(colnames(bp_summary$RSS)))
    if (length(bic_breaks) != length(bic_values) || anyNA(bic_breaks)) {
      bic_breaks <- seq_along(bic_values) - 1L
    }
    expected_break_counts <- seq.int(0L, length(bic_values) - 1L)
    if (length(bic_values) < 2L || any(!is.finite(bic_values)) ||
        !identical(bic_breaks, expected_break_counts) ||
        anyDuplicated(bic_breaks) > 0L) {
      stop("Internal Bai-Perron audit failed: the complete finite BIC path ",
           "for break counts 0,...,m was not recovered.")
    }
    bic_table <- data.frame(Target = target,
                            Breaks = bic_breaks,
                            BIC = bic_values)
    selected_n <- as.integer(bic_table$Breaks[which.min(bic_table$BIC)])
    default_bp <- bp_full$breakpoints
    default_n <- if (all(is.na(default_bp))) 0L else sum(is.finite(default_bp))
    if (!identical(selected_n, as.integer(default_n))) {
      stop("Internal Bai-Perron audit failed: minimum-BIC count does not match ",
           "the package's default minimum-BIC partition.")
    }

    tests <- data.frame(
      Target = target,
      Test = "supF parameter-stability test (intercept-only)",
      Statistic = unname(sf$statistic),
      P_value = sf$p.value,
      Reject_5pct = sf$p.value < 0.05,
      BIC_selected_breaks = selected_n,
      BIC_path_rows = length(bic_values),
      BIC_internal_crosscheck = TRUE,
      Trim_fraction = h)

    breaks_table <- data.frame(
      Target = character(0), Break_number = integer(0), Break_index = integer(0),
      Break_date = character(0), Break_month_jalali = character(0),
      CI95_low_index = integer(0), CI95_low_date = character(0),
      CI95_low_month_jalali = character(0), CI95_high_index = integer(0),
      CI95_high_date = character(0), CI95_high_month_jalali = character(0))
    if (selected_n > 0L) {
      bp_selected <- strucchange::breakpoints(bp_full, breaks = selected_n)
      bp_idx <- as.integer(bp_selected$breakpoints)
      ci_obj <- tryCatch(stats::confint(bp_full, breaks = selected_n, level = 0.95),
                         error = function(e) NULL)
      ci_mat <- if (is.null(ci_obj)) NULL else as.matrix(ci_obj$confint)
      if (!is.null(ci_mat) && ncol(ci_mat) >= 3L) {
        ci_low <- as.integer(ci_mat[, 1L])
        ci_high <- as.integer(ci_mat[, 3L])
      } else {
        ci_low <- ci_high <- rep(NA_integer_, length(bp_idx))
      }
      breaks_table <- data.frame(
        Target = target,
        Break_number = seq_along(bp_idx),
        Break_index = bp_idx,
        Break_date = date_at(bp_idx, dates),
        Break_month_jalali = jalali_at(bp_idx, jalali),
        CI95_low_index = ci_low,
        CI95_low_date = date_at(ci_low, dates),
        CI95_low_month_jalali = jalali_at(ci_low, jalali),
        CI95_high_index = ci_high,
        CI95_high_date = date_at(ci_high, dates),
        CI95_high_month_jalali = jalali_at(ci_high, jalali))
    }
    list(tests = tests, bic = bic_table, breaks = breaks_table)
  }

  housing_structural_break_robustness <- function(x, dates, jalali,
                                                   za_lag = ZA_LAG,
                                                   h = BREAK_TRIM) {
    x <- as.numeric(x)
    stopifnot(length(x) == length(dates), length(x) == length(jalali),
              all(is.finite(x)))

    ## Zivot-Andrews: null = unit root; alternative = trend-stationary process
    ## with one unknown break in both intercept and trend.
    za <- urca::ur.za(x, model = "both", lag = za_lag)
    za_stat <- as.numeric(za@teststat)
    za_cv <- as.numeric(za@cval)
    za_bp <- as.integer(za@bpoint)
    za_table <- data.frame(
      Target = "Layer2_real_HOUSING",
      Test = "Zivot-Andrews unit-root test; break in intercept and trend",
      Statistic = za_stat,
      Critical_1pct = za_cv[1L],
      Critical_5pct = za_cv[2L],
      Critical_10pct = za_cv[3L],
      Reject_unit_root_5pct = za_stat < za_cv[2L],
      ZA_lag = za_lag,
      Break_index = za_bp,
      Break_date = date_at(za_bp, dates),
      Break_month_jalali = jalali_at(za_bp, jalali))

    mean_scan <- break_scan(x, dates, jalali,
                            target = "Real HOUSING return: mean", h = h)
    variance_proxy <- (x - mean(x))^2
    variance_scan <- break_scan(variance_proxy, dates, jalali,
                                target = "Real HOUSING return: squared-demeaned variance proxy",
                                h = h)

    list(
      za = za_table,
      tests = rbind(mean_scan$tests, variance_scan$tests),
      bic = rbind(mean_scan$bic, variance_scan$bic),
      breaks = rbind(mean_scan$breaks, variance_scan$breaks),
      note = c(
        "TARGETED LAYER-2 STRUCTURAL-BREAK ROBUSTNESS (baseline results unchanged)",
        sprintf("Zivot-Andrews uses model='both' and lag=%d; null hypothesis = unit root.", za_lag),
        "Reject the unit-root null at 5% only when the test statistic is more negative than the 5% critical value.",
        sprintf("supF and Bai-Perron scans trim %.0f%% at each sample end.", 100 * h),
        "supF tests parameter stability; Bai-Perron dates mean shifts selected by minimum BIC.",
        "The squared-demeaned series is a variance-break proxy motivated by the ARCH diagnostic; it is not a GARCH model.",
        "Break dates and their confidence intervals are diagnostic, not causal event identification.")
    )
  }

  break_adjusted_var_robustness <- function(Y, baseline_cs, break_results,
                                            H = H_GFEVD) {
    mean_rows <- break_results$breaks[
      break_results$breaks$Target == "Real HOUSING return: mean", , drop = FALSE]
    if (nrow(mean_rows) == 0L) {
      return(list(status = data.frame(
        Status = "No BIC-selected mean break; break-dummy VAR not estimated.")))
    }
    bp <- sort(unique(as.integer(mean_rows$Break_index)))
    ex <- sapply(seq_along(bp), function(j)
      as.numeric(seq_len(nrow(Y)) > bp[j]))
    if (is.null(dim(ex))) ex <- matrix(ex, ncol = 1L)
    colnames(ex) <- paste0("Housing_mean_break_step_", seq_along(bp))

    csb <- static_connectedness(Y, p = N_LAG, H = H, exogen = ex)
    dgb <- diagnostics(csb$model, Y_ret = NULL)
    comparison <- rbind(
      data.frame(Model = "Baseline VAR(1)", TCI = baseline_cs$TCI,
                 t(baseline_cs$NET)),
      data.frame(Model = "VAR(1) + BIC-selected HOUSING mean-break step dummy",
                 TCI = csb$TCI, t(csb$NET)))
    summary <- data.frame(
      Model = "VAR(1) + BIC-selected HOUSING mean-break step dummy",
      Break_indices = paste(bp, collapse = "/"),
      Max_Modulus = dgb$stability$Max_Modulus,
      Stable = dgb$stability$Stable,
      Portmanteau12_p = dgb$serial$P_value,
      Housing_LjungBox12_p = dgb$per_equation$LjungBox12_p[
        match("HOUSING", dgb$per_equation$Equation)],
      Housing_ARCH_McLeodLi12_p = dgb$per_equation$ARCH_McLeodLi12_p[
        match("HOUSING", dgb$per_equation$Equation)])
    list(static = csb, diagnostics = dgb, comparison = comparison,
         summary = summary, exogen = ex,
         status = data.frame(Status = "Estimated"))
  }

  var2_robustness <- function(Y, baseline_cs, H = H_GFEVD) {
    cs2 <- static_connectedness(Y, p = ROBUST_VAR_LAG, H = H)
    dg2 <- diagnostics(cs2$model, Y_ret = NULL)
    comparison <- rbind(
      data.frame(Model = "Baseline VAR(1)", Lag = 1L,
                 TCI = baseline_cs$TCI, t(baseline_cs$NET)),
      data.frame(Model = "Robustness VAR(2)", Lag = ROBUST_VAR_LAG,
                 TCI = cs2$TCI, t(cs2$NET)))
    summary <- data.frame(
      Model = "Robustness VAR(2)",
      Max_Modulus = dg2$stability$Max_Modulus,
      Stable = dg2$stability$Stable,
      Portmanteau12_p = dg2$serial$P_value,
      Housing_LjungBox12_p = dg2$per_equation$LjungBox12_p[
        match("HOUSING", dg2$per_equation$Equation)],
      Housing_ARCH_McLeodLi12_p = dg2$per_equation$ARCH_McLeodLi12_p[
        match("HOUSING", dg2$per_equation$Equation)])
    list(static = cs2, diagnostics = dg2, comparison = comparison, summary = summary)
  }

  ## ====================== 8. Event robustness [F2] =========================== ##
  event_robustness <- function(Y, dates, H = H_GFEVD) {
    d18 <- as.numeric(dates >= EVENT_2018[1] & dates <= EVENT_2018[2])
    d20 <- as.numeric(dates >= EVENT_2020[1] & dates <= EVENT_2020[2])
    res <- list()
    ## (a) exogenous event dummies, full sample -- no gaps created
    ex <- cbind(D2018 = d18, D2020 = d20)
    res$dummies <- static_connectedness(Y, exogen = ex)
    ## (b) contiguous subsamples with enough observations only
    seg_list <- list(
      post2020 = which(dates > EVENT_2020[2]),
      pre2018  = which(dates < EVENT_2018[1]),
      between  = which(dates > EVENT_2018[2] & dates < EVENT_2020[1])
    )
    res$subsamples <- list(); res$skipped <- character(0)
    for (sn in names(seg_list)) {
      idx <- seg_list[[sn]]
      if (length(idx) >= MIN_SUBSAMPLE_OBS) {
        res$subsamples[[sn]] <- static_connectedness(Y[idx, , drop = FALSE])
      } else {
        res$skipped <- c(res$skipped,
          sprintf("%s skipped: only %d obs (< %d needed for a %d-variable VAR).",
                  sn, length(idx), MIN_SUBSAMPLE_OBS, ncol(Y)))
      }
    }
    res$note <- paste(
      "Event robustness re-designed: no observation-splicing across time gaps.",
      "Primary check = exogenous event dummies (full sample);",
      "secondary check = contiguous subsamples with >=", MIN_SUBSAMPLE_OBS, "obs.",
      paste(res$skipped, collapse = " "))
    res
  }

  ## ====================== 9. Figures (all data-driven) ======================= ##
  ## Every figure closes its device via on.exit(), so an error inside one
  ## figure can never corrupt or block the figures that follow.
  fig_returns <- function(Y, dates, file, main) {
    png(file, width = 2400, height = 2800, res = 300)
    on.exit(try(dev.off(), silent = TRUE), add = TRUE)
    op <- par(mfrow = c(ncol(Y), 1), mar = c(2, 4, 2, 1))
    for (v in colnames(Y)) { plot(dates, Y[, v], type = "l", col = "steelblue4",
                                  ylab = v, xlab = "", main = if (v == colnames(Y)[1]) main else "")
                             abline(h = 0, col = "grey70") }
    par(op)
  }
  fig_heatmap <- function(CT, file, main) {
    png(file, width = 2200, height = 1900, res = 300)
    on.exit(try(dev.off(), silent = TRUE), add = TRUE)
    K <- ncol(CT)
    op <- par(mar = c(5, 6, 3, 1))
    image(1:K, 1:K, t(CT[K:1, ]), axes = FALSE, xlab = "FROM shock in (column)",
          ylab = "", col = hcl.colors(50, "YlOrRd", rev = TRUE), main = main)
    axis(1, at = 1:K, labels = colnames(CT)); axis(2, at = 1:K, labels = rev(rownames(CT)), las = 1)
    for (i in 1:K) for (j in 1:K)
      text(j, K + 1 - i, sprintf("%.1f", CT[i, j]), cex = 0.9,
           col = ifelse(CT[i, j] > max(CT) * .6, "white", "black"))
    par(op)
  }
  fig_net_ci <- function(net, bootNET, file, main) {
    png(file, width = 2200, height = 1500, res = 300)
    on.exit(try(dev.off(), silent = TRUE), add = TRUE)
    o <- order(net); nv <- net[o]; bn <- bootNET[o, ]
    cols <- ifelse(nv >= 0, "steelblue4", "firebrick")
    op <- par(mar = c(5, 6, 3, 1))
    bp <- barplot(nv, names.arg = names(nv), horiz = TRUE, col = cols, las = 1,
                  xlim = range(c(bn$Simultaneous_Lower95,
                                 bn$Simultaneous_Upper95, 0)) * 1.15, main = main,
                  xlab = "NET directional connectedness (95% simultaneous bootstrap band)")
    arrows(bn$Simultaneous_Lower95, bp, bn$Simultaneous_Upper95, bp,
           angle = 90, code = 3, length = 0.05)
    abline(v = 0, lty = 2); par(op)
  }
  fig_network <- function(cs, bootNPDC, file, main) {                    # [F6]
    png(file, width = 2300, height = 2100, res = 300)
    on.exit(try(dev.off(), silent = TRUE), add = TRUE)
    K <- length(cs$NET); vn <- names(cs$NET)
    ang <- pi / 2 - 2 * pi * (0:(K - 1)) / K
    pos <- cbind(0.5 + 0.34 * cos(ang), 0.52 + 0.32 * sin(ang)); rownames(pos) <- vn
    op <- par(mar = c(1, 1, 3, 1))
    plot(NA, xlim = c(0, 1), ylim = c(-0.02, 1), axes = FALSE, xlab = "", ylab = "", main = main)
    np <- cs$NPDC; np$Pair <- paste(np$Var_i, np$Var_j, sep = "__")
    np <- merge(np, bootNPDC, by = "Pair")
    np$Significant <- np$Simultaneous_Lower95 > 0 | np$Simultaneous_Upper95 < 0
    ## draw ALL 10 pairs: significant = solid dark with label,
    ## non-significant = light dashed (honest: estimated but not identified)
    for (r in order(np$Significant)) {
      a <- pos[np$Net_transmitter[r], ]; b <- pos[np$Net_receiver[r], ]
      sh <- 0.075 * (b - a) / sqrt(sum((b - a)^2))
      if (np$Significant[r]) {
        arrows(a[1] + sh[1], a[2] + sh[2], b[1] - sh[1], b[2] - sh[2],
               length = 0.11, lwd = 1.2 + 0.30 * np$Abs_NPDC[r], col = "grey30")
        text((a[1] + b[1]) / 2 + 0.012, (a[2] + b[2]) / 2 + 0.028,
             sprintf("%.1f", np$Abs_NPDC[r]), cex = 0.85, font = 2)
      } else {
        arrows(a[1] + sh[1], a[2] + sh[2], b[1] - sh[1], b[2] - sh[2],
               length = 0.07, lwd = 0.9, col = "grey80", lty = 2)
      }
    }
    for (v in vn) {
      cl <- if (cs$NET[v] >= 0) "steelblue4" else "firebrick"     # colour by SIGN of NET
      points(pos[v, 1], pos[v, 2], pch = 21, bg = cl, col = "white", lwd = 2,
             cex = 5.2 + 3.6 * sqrt(abs(cs$NET[v]) / max(abs(cs$NET))))
      text(pos[v, 1], pos[v, 2], substr(v, 1, 3), col = "white", font = 2, cex = 0.95)
      text(pos[v, 1], pos[v, 2] - 0.105, sprintf("%s  NET %+.1f", v, cs$NET[v]),
           col = cl, font = 2, cex = 0.85)
    }
    legend("bottomleft", bty = "n", cex = 0.8, lty = c(1, 2), lwd = c(2, 1),
           col = c("grey30", "grey80"),
           legend = c("NPDC identified (95% simultaneous band excludes 0)",
                      "NPDC estimated but not significant"))
    legend("bottomright", bty = "n", cex = 0.8, pch = 21, pt.cex = 1.6,
           pt.bg = c("steelblue4", "firebrick"),
           legend = c("NET transmitter (estimate)", "NET receiver (estimate)"))
    par(op)
  }
  fig_tvp_tci <- function(paths, dates_all, file, main, note) {
    ## paths: named list of full-length TCI vectors (NA outside each keep window)
    png(file, width = 2400, height = 1400, res = 300)
    on.exit(try(dev.off(), silent = TRUE), add = TRUE)
    op <- par(mar = c(4, 4, 3, 1))
    ok <- Reduce(`|`, lapply(paths, function(z) !is.na(z)))
    xr <- range(dates_all[ok]); yr <- range(unlist(paths), na.rm = TRUE)
    cols <- hcl.colors(length(paths), "Dark 3")
    plot(dates_all, paths[[1]], type = "n", xlim = xr, ylim = yr,
         xlab = "", ylab = "TCI (%)", main = main)      # Date x-axis preserved
    for (i in seq_along(paths))
      lines(dates_all, paths[[i]], col = cols[i],
            lwd = ifelse(i == length(paths), 2.4, 1.3))
    legend("topright", bty = "n", lwd = 2, col = cols, cex = 0.75,
           legend = names(paths))
    mtext(note, side = 3, line = -1.3, cex = 0.7, adj = 0.02)
    par(op)
  }
  fig_tvp_net <- function(tvp, ts_res, dates_all, file, main) {
    ## baseline NET path per market with a pointwise min-max band across ALL
    ## sensitivity specifications drawn behind it (referee round 3).
    png(file, width = 2400, height = 2800, res = 300)
    on.exit(try(dev.off(), silent = TRUE), add = TRUE)
    K <- ncol(tvp$NET); vn <- colnames(tvp$NET)
    Te <- nrow(tvp$NET); nspec <- length(ts_res$specs)
    band_lo <- matrix(NA_real_, Te, K, dimnames = list(NULL, vn))
    band_hi <- band_lo
    idx <- ts_res$common
    for (t in idx) for (v in vn) {
      vals <- vapply(ts_res$specs, function(nm) ts_res$fits[[nm]]$NET[t, v], numeric(1))
      if (sum(!is.na(vals)) >= 2) { band_lo[t, v] <- min(vals, na.rm = TRUE)
                                    band_hi[t, v] <- max(vals, na.rm = TRUE) }
    }
    op <- par(mfrow = c(K, 1), mar = c(2, 4, 2, 1))
    for (v in vn) {
      yl <- range(c(band_lo[, v], band_hi[, v], tvp$NET[idx, v]), na.rm = TRUE)
      plot(dates_all[idx], tvp$NET[idx, v], type = "n", ylim = yl,
           ylab = paste("NET", v), xlab = "",
           main = if (v == vn[1]) main else "")
      bb <- which(!is.na(band_lo[, v]))
      if (length(bb) > 1)
        polygon(c(dates_all[bb], rev(dates_all[bb])),
                c(band_lo[bb, v], rev(band_hi[bb, v])),
                col = adjustcolor("steelblue", 0.20), border = NA)
      abline(h = 0, col = "grey60")
      lines(dates_all[idx], tvp$NET[idx, v], col = "steelblue4", lwd = 2)
      if (v == vn[1])
        legend("topright", bty = "n", cex = 0.75,
               lty = c(1, NA), lwd = c(2, NA), pch = c(NA, 15), pt.cex = c(NA, 1.8),
               col = c("steelblue4", adjustcolor("steelblue", 0.35)),
               legend = c("Baseline specification",
                          sprintf("Min-max tuning range across %d specs (not a CI)", nspec)))
    }
    par(op)
  }
  fig_girf <- function(G, lo, hi, file, main) {
    K <- dim(G)[2]; vn <- dimnames(G)[[2]]; H <- dim(G)[1] - 1
    png(file, width = 3200, height = 3200, res = 300)
    on.exit(try(dev.off(), silent = TRUE), add = TRUE)
    op <- par(mfrow = c(K, K), mar = c(2, 2.5, 1.6, 0.6), oma = c(2, 2, 3, 1))
    for (i in 1:K) for (j in 1:K) {
      yl <- range(c(G[, i, j], lo[, i, j], hi[, i, j]), na.rm = TRUE)
      plot(0:H, G[, i, j], type = "l", lwd = 1.8, col = "steelblue4", ylim = yl,
           main = paste0(vn[i], " <- shock ", vn[j]), cex.main = 0.8, xlab = "", ylab = "")
      polygon(c(0:H, H:0), c(lo[, i, j], rev(hi[, i, j])), col = adjustcolor("steelblue", 0.25), border = NA)
      lines(0:H, G[, i, j], lwd = 1.8, col = "steelblue4"); abline(h = 0, col = "grey60")
    }
    mtext(main, outer = TRUE, cex = 1.0, font = 2)
    mtext("Generalized IRs (not structural). h=0 responses reflect contemporaneous innovation correlation.",
          outer = TRUE, side = 1, cex = 0.7)
    par(op)
  }
  fig_freq_net <- function(fq, file, main) {
    png(file, width = 2400, height = 1500, res = 300)
    on.exit(try(dev.off(), silent = TRUE), add = TRUE)
    M <- t(sapply(fq, function(b) b$NET))                # bands x markets
    op <- par(mar = c(5, 5, 3, 1))
    barplot(M, beside = TRUE, col = hcl.colors(nrow(M), "Teal"), las = 1,
            ylab = "NET connectedness", main = main,
            legend.text = paste0(rownames(M), sprintf("  (TCI %.1f)",
                                 vapply(fq, `[[`, numeric(1), "TCI"))),
            args.legend = list(bty = "n", cex = 0.8, x = "bottomright"))
    abline(h = 0)
    par(op)
  }

  ## ====================== 10. Run both layers ================================ ##
  all_results <- list()

  for (ln in names(LAYERS)) {
    cat("\n==============================\n", LAYERS[[ln]]$label, "\n==============================\n")
    Y <- LAYERS[[ln]]$Y; colnames(Y) <- varnames
    odir <- file.path(OUT_ROOT, ln); tdir <- file.path(odir, "tables_csv"); fdir <- file.path(odir, "figures")
    dir.create(tdir, recursive = TRUE, showWarnings = FALSE); dir.create(fdir, showWarnings = FALSE)
    wcsv <- function(df, name) write.csv(df, file.path(tdir, paste0(name, ".csv")), row.names = FALSE)

    ## ---- static core ----
    cs <- static_connectedness(Y)
    cat(sprintf("Static TCI = %.4f\n", cs$TCI))
    print(round(cs$NET, 2))
    static_net_df <- data.frame(Segment = varnames, FROM = round(cs$FROM, 2),
                                TO = round(cs$TO, 2), NET = round(cs$NET, 2), TCI = round(cs$TCI, 3))
    wcsv(static_net_df, "Static_NET")
    wcsv(data.frame(Segment = varnames, round(cs$table, 3)), "Static_TABLE")
    wcsv(cs$NPDC, "Static_NPDC_unique_pairs")
    wcsv(data.frame(TCI = cs$TCI), "Static_TCI")

    ## optional cross-check vs ConnectednessApproach                      [F8]
    if (HAS_CA) {
      chk <- tryCatch({
        z <- zoo::zoo(Y, order.by = dates_ret)
        o <- ConnectednessApproach::ConnectednessApproach(
          z, nlag = N_LAG, nfore = H_GFEVD, model = "VAR", connectedness = "Time",
          Connectedness_config = list(TimeConnectedness = list(generalized = TRUE)))
        max(abs(as.numeric(o$TCI) - cs$TCI))
      }, error = function(e) NA)
      cat("Cross-check |TCI(own) - TCI(ConnectednessApproach)| =", chk, "\n")
      wcsv(data.frame(max_abs_TCI_diff = chk), "CrossCheck_ConnectednessApproach")
    }

    ## ---- diagnostics ----
    dg <- diagnostics(cs$model, Y_ret = Y)
    wcsv(dg$lag_selection, "LagSelection"); wcsv(dg$stability, "VAR_Stability")
    wcsv(dg$serial, "Serial_Correlation")
    if (!is.null(dg$normality)) wcsv(dg$normality, "Multivariate_Normality")
    wcsv(dg$per_equation, "Residual_Diagnostics_By_Eq")
    wcsv(dg$unit_roots, "UnitRoot_Screen")
    writeLines(c(dg$arch_note, dg$honest_summary), file.path(tdir, "Diagnostics_Notes.txt"))
    cat(dg$honest_summary, "\n")

    ## ---- targeted final robustness: Layer 2 real HOUSING + VAR(2) only ----
    ## Layer 1 and every validated baseline output remain byte-for-byte governed
    ## by the original code paths above and below.
    break_rb <- NULL
    break_var_rb <- NULL
    var2_rb <- NULL
    if (identical(ln, "layer2_real")) {
      cat("Targeted Layer-2 robustness: structural breaks + VAR(2) diagnostics...\n")
      break_rb <- housing_structural_break_robustness(
        Y[, "HOUSING"], dates_ret, jal_ret)
      wcsv(break_rb$za, "Housing_ZivotAndrews")
      wcsv(break_rb$tests, "Housing_StructuralBreak_Tests")
      wcsv(break_rb$bic, "Housing_BaiPerron_BIC")
      wcsv(break_rb$breaks, "Housing_BaiPerron_Breaks")
      writeLines(break_rb$note, file.path(tdir, "Housing_StructuralBreak_Note.txt"))

      break_var_rb <- break_adjusted_var_robustness(Y, cs, break_rb)
      wcsv(break_var_rb$status, "Housing_BreakAdjusted_VAR_Status")
      if (!is.null(break_var_rb$static)) {
        wcsv(data.frame(Segment = varnames,
                        FROM = round(break_var_rb$static$FROM, 2),
                        TO = round(break_var_rb$static$TO, 2),
                        NET = round(break_var_rb$static$NET, 2),
                        TCI = round(break_var_rb$static$TCI, 3)),
             "Housing_BreakAdjusted_VAR1_Static_NET")
        wcsv(break_var_rb$comparison, "Housing_BreakAdjusted_VAR1_Comparison")
        wcsv(break_var_rb$summary, "Housing_BreakAdjusted_VAR1_Diagnostic_Summary")
        wcsv(break_var_rb$diagnostics$per_equation,
             "Housing_BreakAdjusted_VAR1_Residual_Diagnostics")
      }

      var2_rb <- var2_robustness(Y, cs)
      wcsv(data.frame(Segment = varnames, round(var2_rb$static$table, 3)),
           "VAR2_Static_TABLE")
      wcsv(data.frame(Segment = varnames,
                      FROM = round(var2_rb$static$FROM, 2),
                      TO = round(var2_rb$static$TO, 2),
                      NET = round(var2_rb$static$NET, 2),
                      TCI = round(var2_rb$static$TCI, 3)),
           "VAR2_Static_NET")
      wcsv(var2_rb$comparison, "VAR2_Comparison_With_Baseline")
      wcsv(var2_rb$summary, "VAR2_Diagnostic_Summary")
      wcsv(var2_rb$diagnostics$stability, "VAR2_Stability")
      wcsv(var2_rb$diagnostics$serial, "VAR2_Serial_Correlation")
      wcsv(var2_rb$diagnostics$per_equation, "VAR2_Residual_Diagnostics_By_Eq")
      writeLines(c(
        "TARGETED VAR(2) ROBUSTNESS (baseline VAR(1) unchanged)",
        "VAR(2) is a residual-diagnostic and connectedness sensitivity check, not an automatic replacement for the information-criterion-selected VAR(1).",
        var2_rb$diagnostics$arch_note,
        var2_rb$diagnostics$honest_summary),
        file.path(tdir, "VAR2_Robustness_Note.txt"))
      print(break_rb$za)
      print(break_rb$tests)
      if (!is.null(break_var_rb$summary)) print(break_var_rb$summary)
      print(var2_rb$summary)
    }

    ## ---- static bootstrap ----
    cat("Static bootstrap (", BOOT_REPS, "reps,", BOOT_TYPE,
        ", block length", BOOT_BLOCK_LEN, ")...\n")
    bt <- boot_static(cs$model, Y)
    writeLines(c(
      sprintf("Static bootstrap: %s, B=%d, seed=%d, block length=%d months.",
              BOOT_TYPE, BOOT_REPS, BOOT_SEED, BOOT_BLOCK_LEN),
      "One multiplier is shared by all five residuals in a month, preserving contemporaneous covariance.",
      "For wild_block, the multiplier is also shared within each time block to accommodate short residual dependence.",
      "NET and NPDC outputs contain pointwise intervals plus non-studentized max-deviation",
      "95% simultaneous bands. Role and network-edge identification use the simultaneous bands."),
      file.path(tdir, "Bootstrap_Method_Note.txt"))
    boot_net <- cbind(data.frame(Segment = varnames, Estimate = round(cs$NET, 2)), round(bt$NET[, -1], 3))
    boot_npdc <- data.frame(Pair = bt$NPDC$Pair,
                            Estimate = cs$NPDC$NPDC_ij[match(bt$NPDC$Pair, paste(cs$NPDC$Var_i, cs$NPDC$Var_j, sep = "__"))],
                            bt$NPDC[, -1])
    wcsv(boot_net, "Bootstrap_NET")
    wcsv(data.frame(Estimate = cs$TCI, t(bt$TCI), Reps_Used = bt$reps_used), "Bootstrap_TCI")
    wcsv(boot_npdc, "Bootstrap_NPDC")
    ## interpretation guard: is each market's transmitter/receiver role identified?
    role_df <- within(boot_net, {
      Role_identified <- Simultaneous_Lower95 > 0 | Simultaneous_Upper95 < 0
      Role <- ifelse(!Role_identified, "NOT identified (simultaneous band includes 0)",
                     ifelse(Estimate > 0, "Net transmitter", "Net receiver"))
    })
    wcsv(role_df, "NET_role_identification")
    cat("Role identification:\n")
    print(role_df[, c("Segment", "Estimate", "Simultaneous_Lower95",
                      "Simultaneous_Upper95", "Role")])

    ## ---- TVP-VAR (strict training/post-training split) + sensitivity ----
    cat("TVP-VAR strict-split filter + sensitivity (", length(TVP_TRAIN_SET),
        "training windows x", length(TVP_KAPPA_SET), "kappa combos )...\n")
    ts_res   <- tvp_sensitivity(Y)
    tvp_main <- ts_res$fits[[ts_res$base_name]]
    dates_all <- dates_ret[(N_LAG + 1):Tn]               # dates of Yt rows (t = 1..Te)
    jal_all   <- jal_ret[(N_LAG + 1):Tn]
    idx <- tvp_main$keep
    wcsv(data.frame(Date = dates_all[idx], Month_Jalali = jal_all[idx],
                    TCI = round(tvp_main$TCI[idx], 4),
                    round(tvp_main$NET[idx, ], 4)), "TVP_TCI_NET_path")


    ## ---- TVP covariance-recursion implementation sensitivity ---------------
    ## Headline nominal specification only. This is an implementation-sensitivity
    ## check, not statistical inference and not a replacement for the baseline.
    tvp_ref_path <- NULL
    tvp_ref_summary <- NULL
    if (identical(ln, "layer1_nominal")) {
      cat("TVP reference-recursion sensitivity (one-stage prediction-error covariance)...\n")

      tvp_ref <- tvp_var_filter_reference(
        Y, p = N_LAG, train_n = TRAIN_N,
        kappa1 = TVP_KAPPA1, kappa2 = TVP_KAPPA2, H = H_GFEVD
      )

      common_ref <- intersect(tvp_main$keep, tvp_ref$keep)
      if (length(common_ref) < 24L || any(diff(common_ref) != 1L))
        stop("Baseline and reference TVP recursions do not share a valid contiguous window.")

      base_ref_tci <- tvp_main$TCI[common_ref]
      alt_ref_tci <- tvp_ref$TCI[common_ref]
      ref_diff <- alt_ref_tci - base_ref_tci

      tvp_ref_path <- data.frame(
        Date = as.character(dates_all[common_ref]),
        Month_Jalali = jal_all[common_ref],
        Baseline_two_stage_TCI = base_ref_tci,
        Reference_one_stage_TCI = alt_ref_tci,
        Difference_reference_minus_baseline_pp = ref_diff,
        stringsAsFactors = FALSE
      )

      tvp_ref_summary <- data.frame(
        Check = "Nominal TVP covariance-recursion sensitivity",
        Train_n = TRAIN_N,
        kappa1 = TVP_KAPPA1,
        kappa2 = TVP_KAPPA2,
        H_GFEVD = H_GFEVD,
        N_common_months = length(common_ref),
        Start_date = as.character(dates_all[min(common_ref)]),
        End_date = as.character(dates_all[max(common_ref)]),
        Max_abs_TCI_difference_pp = max(abs(ref_diff)),
        Mean_abs_TCI_difference_pp = mean(abs(ref_diff)),
        RMSE_TCI_difference_pp = sqrt(mean(ref_diff^2)),
        Correlation = suppressWarnings(cor(base_ref_tci, alt_ref_tci)),
        Baseline_start_TCI = base_ref_tci[1L],
        Reference_start_TCI = alt_ref_tci[1L],
        Baseline_end_TCI = tail(base_ref_tci, 1L),
        Reference_end_TCI = tail(alt_ref_tci, 1L),
        Baseline_stability_adjustments = tvp_main$n_carried_forward,
        Reference_stability_adjustments = tvp_ref$n_carried_forward,
        stringsAsFactors = FALSE
      )

      wcsv(tvp_ref_path, "TVP_reference_recursion_path")
      wcsv(tvp_ref_summary, "TVP_reference_recursion_comparison")

      writeLines(c(
        "TVP REFERENCE-RECURSION SENSITIVITY",
        "Baseline: two-stage covariance recursion (prediction-error covariance in gain; filtered-error covariance carried forward).",
        "Reference: one-stage prediction-error covariance used in the gain, current connectedness calculation, and next-period covariance state.",
        "All other settings are identical: nominal returns, strict 36-month training split, kappa1=0.99, kappa2=0.99, VAR(1), H=10, and the same stability control.",
        "This is an implementation-sensitivity check, not a confidence interval or causal test.",
        sprintf("Common reported months: %d.", length(common_ref)),
        sprintf("Maximum absolute TCI difference: %.6f percentage points.", max(abs(ref_diff))),
        sprintf("Mean absolute TCI difference: %.6f percentage points.", mean(abs(ref_diff))),
        sprintf("TCI-path correlation: %.9f.", suppressWarnings(cor(base_ref_tci, alt_ref_tci)))
      ), file.path(tdir, "TVP_Reference_Recursion_Note.txt"))

      cat(sprintf(
        "Reference recursion: max |TCI diff| = %.6f pp; correlation = %.9f\n",
        max(abs(ref_diff)), suppressWarnings(cor(base_ref_tci, alt_ref_tci))
      ))
    }
    ## sensitivity stats over the common window (after the largest training set)
    wcsv(cbind(ts_res$stats[, 1:8], round(ts_res$stats[, -(1:8)], 3)),
         "TVP_sensitivity_stats")
    wcsv(ts_res$roles, "TVP_sensitivity_role_stability")
    wcsv(ts_res$spec_status, "TVP_specification_status")
    stability_rows <- do.call(rbind, lapply(ts_res$specs, function(nm) {
      f <- ts_res$fits[[nm]]; kk <- f$keep
      data.frame(Spec = nm, Date = dates_all[kk], Month_Jalali = jal_all[kk],
                 f$stability[kk, c("Raw_root", "Used_root", "Action")])
    }))
    wcsv(stability_rows, "TVP_stability_path")
    ## all sensitivity TCI paths, wide format with explicit dates
    paths_df <- data.frame(Date = dates_all, Month_Jalali = jal_all)
    for (nm in ts_res$specs) paths_df[[nm]] <- round(ts_res$fits[[nm]]$TCI, 4)
    wcsv(paths_df, "TVP_sensitivity_TCI_paths")

    ## ---- which TVP patterns are descriptively stable across checked settings?
    cw <- ts_res$common
    peak_tbl <- do.call(rbind, lapply(ts_res$specs, function(nm) {
      x <- ts_res$fits[[nm]]$TCI[cw]
      i1 <- which.max(x); j1 <- which.min(x)
      x2 <- x; x2[max(1, i1 - 6):min(length(x), i1 + 6)] <- -Inf
      i2 <- if (any(is.finite(x2))) which.max(x2) else NA_integer_
      tr <- suppressWarnings(cor(x, seq_along(x)))
      data.frame(Spec = nm,
                 TCI_mean = round(mean(x), 2),
                 TCI_min = round(min(x), 2),
                 TCI_max = round(max(x), 2),
                 Peak1_month = jal_all[cw][i1], Peak1_TCI = round(x[i1], 2),
                 Peak2_month = if (is.na(i2)) NA else jal_all[cw][i2],
                 Peak2_TCI = if (is.na(i2)) NA else round(x[i2], 2),
                 Trough_month = jal_all[cw][j1], Trough_TCI = round(x[j1], 2),
                 Trend_corr_with_time = round(tr, 3),
                 Peak1_idx = i1, Trough_idx = j1)
    }))
    wcsv(peak_tbl[, setdiff(names(peak_tbl), c("Peak1_idx", "Trough_idx"))],
         "TVP_TCI_levels_and_peaks_by_spec")
    ## Descriptive tuning-stability classification with explicit thresholds.
    ## This is not inferential evidence and is never labelled "robust".
    st <- ts_res$stats
    role_class <- do.call(rbind, lapply(varnames, function(v) {
      nm <- st[[paste0("NETmean_", v)]]; pp <- st[[paste0("PctPos_", v)]]
      verdict <- if (all(nm > 0) && all(pp >= 60))
        "TUNING-STABLE net transmitter (descriptive)"
        else if (all(nm < 0) && all(pp <= 40))
          "TUNING-STABLE net receiver (descriptive)"
        else "SENSITIVE across settings - no stable descriptive role"
      data.frame(Segment = v,
                 NETmean_min = round(min(nm), 2), NETmean_max = round(max(nm), 2),
                 PctPos_min = round(min(pp), 1), PctPos_max = round(max(pp), 1),
                 Verdict = verdict)
    }))
    wcsv(role_class, "TVP_role_tuning_stability")
    ## Stability of peak/trough timing across the checked tuning choices.
    ## all specs fall within a 3-month window on the common grid.
    peak_spread   <- diff(range(peak_tbl$Peak1_idx))
    trough_spread <- diff(range(peak_tbl$Trough_idx))
    trend_signs   <- sign(peak_tbl$Trend_corr_with_time)
    trend_signs   <- trend_signs[!is.na(trend_signs)]
    guide <- c(
      "TVP INTERPRETATION GUIDE (auto-generated; Q1.1 correction)",
      paste("Included training windows:",
            paste(sort(unique(vapply(ts_res$fits, `[[`, numeric(1), "train_n"))),
                  collapse = "/"), "months; (kappa1,kappa2) in"),
      paste(" ", paste(vapply(TVP_KAPPA_SET, function(k) sprintf("(%.2f,%.2f)", k[1], k[2]), ""),
                       collapse = ", "),
            sprintf("- compared on one complete common window of %d months.", length(cw))),
      sprintf("Common window: %s to %s; no pairwise deletion and no na.rm summaries.",
              as.character(dates_all[min(cw)]), as.character(dates_all[max(cw)])),
      "Unstable local updates are flagged and handled by carrying forward the last",
      "stable coefficient state; see TVP_stability_path.csv.",
      "",
      sprintf("1) TCI LEVEL: mean TCI ranges from %.1f to %.1f across specifications.",
              min(peak_tbl$TCI_mean), max(peak_tbl$TCI_mean)),
      "   The LEVEL is specification-dependent: report it as a range, never as a",
      "   single point estimate.",
      if (length(unique(trend_signs[trend_signs != 0])) == 1)
        sprintf("   The TIME TREND sign is TUNING-STABLE: correlation with time is %s in all specs (%.2f to %.2f).",
                ifelse(trend_signs[1] > 0, "positive", "negative"),
                min(peak_tbl$Trend_corr_with_time), max(peak_tbl$Trend_corr_with_time))
      else "   The TIME TREND of TCI is NOT consistent across specs - do not interpret.",
      "",
      if (peak_spread <= 3)
        sprintf("2) PEAK TIMING is TUNING-STABLE - the maximum-TCI month lies within a %d-month window across all specs (modal month %s).",
                peak_spread + 1, names(sort(table(peak_tbl$Peak1_month), decreasing = TRUE))[1])
      else
        sprintf("2) PEAK TIMING is SENSITIVE - argmax months span %d months across specs; interpret episodes, not a dated peak.",
                peak_spread + 1),
      if (trough_spread <= 3)
        sprintf("   TROUGH TIMING is TUNING-STABLE (modal month %s).",
                names(sort(table(peak_tbl$Trough_month), decreasing = TRUE))[1])
      else "   TROUGH TIMING is SENSITIVE across specs.",
      "",
      "3) MARKET ROLES (thresholds: sign agreement in ALL specs AND NET>0 share",
      "   >=60% for transmitters / <=40% for receivers in ALL specs):",
      paste0("   ", role_class$Segment, ": ", role_class$Verdict,
             sprintf("  [NET mean %.1f..%.1f, %%months NET>0: %.0f..%.0f]",
                     role_class$NETmean_min, role_class$NETmean_max,
                     role_class$PctPos_min, role_class$PctPos_max)),
      "",
      "TUNING-STABLE means agreement across checked settings only; it is not a",
      "confidence statement. Results labelled SENSITIVE should not carry a role claim.")
    writeLines(guide, file.path(tdir, "TVP_Interpretation_Guide.txt"))
    cat(paste(guide, collapse = "\n"), "\n")
    cat("TVP sensitivity (common window):\n")
    print(ts_res$stats[, c("Spec", "TCI_mean", "TCI_sd", "Corr_with_baseline")], digits = 4)
    writeLines(paste(
      "TVP methodology [F1, updated]: prior = OLS on the first train_n months ONLY;",
      "the Kalman filter starts at t = train_n + 1, so training observations enter",
      "through the prior once and are NEVER re-used in the update step (strict",
      "training / post-training split). The first reported point is the first",
      "post-training month. Sensitivity: training windows", paste(TVP_TRAIN_SET, collapse = "/"),
      "months and (kappa1,kappa2) combinations",
      paste(vapply(TVP_KAPPA_SET, function(k) sprintf("(%.2f,%.2f)", k[1], k[2]), ""),
            collapse = ", "),
      sprintf("- all compared over the complete common window %s to %s (%d months).",
              as.character(dates_all[min(cw)]), as.character(dates_all[max(cw)]), length(cw)),
      "Any unstable local coefficient update is reported in TVP_stability_path.csv;",
      "the last stable coefficient state is carried forward for that date, following",
      "the stability-control logic used in ConnectednessApproach::TVPVAR.",
      "No 'TVP confidence bands' are reported: bands simulated from a constant-parameter",
      "VAR do not measure path uncertainty of a time-varying DGP [F3]."),
      file.path(tdir, "TVP_Methodology_Note.txt"))

    if (RUN_TVP_DGP_SENSITIVITY) {
      cat("Optional fixed-DGP TVP sensitivity (NOT confidence bands)...\n")
      ## clearly-labelled sensitivity under a constant-parameter DGP
      set.seed(BOOT_SEED + 7)
      tcis <- replicate(TVP_DGP_REPS, {
        E <- stats::residuals(cs$model)
        Eb <- E * sample(c(-1, 1), nrow(E), replace = TRUE)
        Yb <- Y; for (t in (N_LAG + 1):nrow(Y))
          Yb[t, ] <- vars::Bcoef(cs$model)[, "const"] + vars::Acoef(cs$model)[[1]] %*% Yb[t - 1, ] + Eb[t - N_LAG, ]
        z <- tryCatch(tvp_var_filter(Yb, train_n = TRAIN_N, kappa1 = TVP_KAPPA1, kappa2 = TVP_KAPPA2),
                      error = function(e) NULL)
        if (is.null(z)) rep(NA_real_, length(idx)) else z$TCI[z$keep]
      })
      wcsv(data.frame(Date = dates_all[idx],
                      Sens_Lo = apply(tcis, 1, quantile, .025, na.rm = TRUE),
                      Sens_Hi = apply(tcis, 1, quantile, .975, na.rm = TRUE)),
           "TVP_TCI_fixedDGP_sensitivity_NOT_CI")
    }

    ## ---- event robustness (no splicing) ----
    cat("Event robustness (dummies + contiguous subsamples)...\n")
    ev <- event_robustness(Y, dates_ret)
    ev_rows <- data.frame(Spec = "Full sample + 2018/2020 event dummies (exogenous)",
                          t(round(ev$dummies$NET, 2)), TCI = round(ev$dummies$TCI, 3))
    for (sn in names(ev$subsamples))
      ev_rows <- rbind(ev_rows, data.frame(Spec = paste("Contiguous subsample:", sn),
                                           t(round(ev$subsamples[[sn]]$NET, 2)),
                                           TCI = round(ev$subsamples[[sn]]$TCI, 3)))
    ev_rows <- rbind(data.frame(Spec = "Baseline full sample", t(round(cs$NET, 2)),
                                TCI = round(cs$TCI, 3)), ev_rows)
    wcsv(ev_rows, "Robust_Events_no_splicing")
    writeLines(ev$note, file.path(tdir, "Robust_Events_Note.txt"))
    print(ev_rows)

    ## ---- lag/horizon robustness ----
    gr <- expand.grid(p = 1:2, H = c(6, 10, 12, 18))
    lh <- do.call(rbind, lapply(seq_len(nrow(gr)), function(i) {
      z <- static_connectedness(Y, p = gr$p[i], H = gr$H[i])
      data.frame(Lag = gr$p[i], Horizon = gr$H[i], TCI = round(z$TCI, 3), t(round(z$NET, 2)))
    }))
    wcsv(lh, "Robust_LagHorizon")

    ## ---- frequency decomposition ----
    fq <- NULL
    if (RUN_FREQUENCY) {
      fq <- tryCatch(freq_connectedness(cs$model), error = function(e) NULL)
      if (!is.null(fq)) {
        fq_rows <- do.call(rbind, lapply(names(fq), function(bn) data.frame(
          Band = bn, TCI = round(fq[[bn]]$TCI, 3), t(round(fq[[bn]]$NET, 2)))))
        wcsv(fq_rows, "Frequency_Band_NET_TCI")
        wcsv(data.frame(Band = names(FREQ_BANDS),
                        Period_months = vapply(FREQ_BANDS, function(b) paste(b, collapse = "-"), "")),
             "Frequency_Band_Definitions")
        ## self-test: band tables sum to the (long-horizon) static table.
        ## Benchmark is H=100 (spectral decomposition corresponds to H -> Inf).
        sum_tab <- Reduce(`+`, lapply(fq, `[[`, "table"))
        cs_long <- static_connectedness(Y, H = 100)
        wcsv(data.frame(max_abs_diff_bands_vs_staticH100 = max(abs(sum_tab - cs_long$table))),
             "Frequency_SelfTest")
      }
    }

    ## ---- GIRF ----
    G <- girf(cs$model, GIRF_H)
    gl <- data.frame()
    for (i in 1:K) for (j in 1:K)
      gl <- rbind(gl, data.frame(h = 0:GIRF_H, Response = varnames[i], Shock = varnames[j],
                                 IRF = round(G[, i, j], 4),
                                 Lower95 = round(bt$GIRF_lo[, i, j], 4),
                                 Upper95 = round(bt$GIRF_hi[, i, j], 4)))
    wcsv(gl, "GIRF_point_and_bands")
    writeLines(paste(
      "GIRFs are generalized (order-invariant) impulse responses; they are NOT structural.",
      "Significant h=0 responses reflect contemporaneous innovation correlation.",
      "Horizon-specific magnitudes and bands must be read from GIRF_point_and_bands.csv.",
      "The GIRFs do not establish causal, multi-year, or regularly-lagged transmission."),
      file.path(tdir, "GIRF_Note.txt"))

    ## ---- figures (each wrapped so one failure never blocks the rest) ----
    sf <- function(expr) tryCatch(expr, error = function(e)
      cat("FIGURE FAILED:", conditionMessage(e), "\n"))
    sf(fig_returns(Y, dates_ret, file.path(fdir, "Fig00_returns.png"), LAYERS[[ln]]$label))
    sf(fig_heatmap(cs$table, file.path(fdir, "Fig01_static_heatmap.png"),
                paste0("Static GFEVD connectedness (%), TCI = ", round(cs$TCI, 2))))
    sf(fig_net_ci(cs$NET, bt$NET, file.path(fdir, "Fig02_NET_bootstrapCI.png"),
               "NET connectedness with 95% simultaneous bootstrap bands"))
    sf(fig_network(cs, bt$NPDC, file.path(fdir, "Fig03_network_datadriven.png"),
                "Static NPDC network (all pairs; significant edges highlighted)"))
    kap_specs <- grep(sprintf("^train%02d_", TRAIN_N), ts_res$specs, value = TRUE)
    sf(fig_tvp_tci(setNames(lapply(kap_specs, function(nm) ts_res$fits[[nm]]$TCI), kap_specs),
                dates_all, file.path(fdir, "Fig04_TVP_TCI_kappa.png"),
                "TVP total connectedness: (kappa1, kappa2) sensitivity",
                sprintf("Strict split: prior on first %d months, filter starts month %d.",
                        TRAIN_N, TRAIN_N + 1)))
    trn_specs <- grep(sprintf("k%.2f_%.2f$", TVP_KAPPA1, TVP_KAPPA2), ts_res$specs, value = TRUE)
    trn_paths <- setNames(lapply(trn_specs, function(nm) {
      z <- rep(NA_real_, length(ts_res$fits[[nm]]$TCI))
      z[cw] <- ts_res$fits[[nm]]$TCI[cw]
      z
    }), trn_specs)
    sf(fig_tvp_tci(trn_paths,
                dates_all, file.path(fdir, "Fig04b_TVP_TCI_training.png"),
                "TVP total connectedness: training-window sensitivity",
                sprintf("Complete common window: %s to %s (%d months).",
                        as.character(dates_all[min(cw)]), as.character(dates_all[max(cw)]),
                        length(cw))))
    sf(fig_tvp_net(tvp_main, ts_res, dates_all, file.path(fdir, "Fig05_TVP_NET.png"),
                "TVP NET: baseline path with tuning range (not a confidence band)"))
    sf(fig_girf(G, bt$GIRF_lo, bt$GIRF_hi, file.path(fdir, "Fig06_GIRF.png"),
             "Generalized impulse responses with 95% bootstrap bands"))
    if (!is.null(fq))
      sf(fig_freq_net(fq, file.path(fdir, "Fig07_frequency_NET.png"),
                   "NET connectedness by frequency band"))

    ## ---- clean workbook [F7] ----
    ## writexl produces a minimal values-only xlsx: no drawings, no printer
    ## settings, no dangling relationships (openxlsx on some versions emits
    ## sheet rels pointing to non-existent drawingN.xml -- the exact defect
    ## flagged in the referee report, so openxlsx is deliberately avoided).
    sheet_list <- list(
      README = data.frame(Note = c(
        LAYERS[[ln]]$label,
        "Values-only workbook written with writexl: no drawings, no broken relationships.",
        "See tables_csv/ for the same content in CSV and Diagnostics_Notes.txt for caveats.")),
      Static_TABLE = data.frame(Segment = varnames, round(cs$table, 3)),
      Static_NET = static_net_df,
      Static_NPDC = cs$NPDC,
      Bootstrap_NET = boot_net,
      Bootstrap_NPDC = boot_npdc,
      NET_roles = role_df,
      Diagnostics_ByEq = dg$per_equation,
      UnitRoots = dg$unit_roots,
      Robust_Events = ev_rows,
      Robust_LagHorizon = lh,
      TVP_path = data.frame(Date = as.character(dates_all[idx]),
                            Month_Jalali = jal_all[idx],
                            TCI = round(tvp_main$TCI[idx], 4),
                            round(tvp_main$NET[idx, ], 4)),
      TVP_sensitivity = cbind(ts_res$stats[, 1:8], round(ts_res$stats[, -(1:8)], 3)),
      TVP_role_stability = ts_res$roles,
      TVP_role_tuning = role_class,
      TVP_spec_status = ts_res$spec_status,
      TVP_stability = stability_rows,
      TVP_peaks_by_spec = peak_tbl[, setdiff(names(peak_tbl), c("Peak1_idx", "Trough_idx"))],
      GIRF = gl
    )
    if (!is.null(tvp_ref_summary)) {
      sheet_list <- c(sheet_list, list(
        TVP_ref_recursion = tvp_ref_summary,
        TVP_ref_path = tvp_ref_path
      ))
    }
    if (identical(ln, "layer2_real")) {
      sheet_list <- c(sheet_list, list(
        Housing_ZA = break_rb$za,
        Housing_break_tests = break_rb$tests,
        Housing_break_BIC = break_rb$bic,
        Housing_break_dates = break_rb$breaks,
        VAR2_comparison = var2_rb$comparison,
        VAR2_summary = var2_rb$summary,
        VAR2_diagnostics = var2_rb$diagnostics$per_equation))
      if (!is.null(break_var_rb$static)) {
        sheet_list <- c(sheet_list, list(
          BreakAdj_VAR1_compare = break_var_rb$comparison,
          BreakAdj_VAR1_summary = break_var_rb$summary,
          BreakAdj_VAR1_diag = break_var_rb$diagnostics$per_equation))
      }
    }
    wb_ok <- tryCatch({
      writexl::write_xlsx(sheet_list, file.path(odir, paste0(ln, "_results_Q1.xlsx")))
      TRUE
    }, error = function(e) { cat("Workbook write failed:", conditionMessage(e),
                                 "- CSV tables remain authoritative.\n"); FALSE })

    all_results[[ln]] <- list(static = cs, boot = bt, tvp = tvp_main,
                              events = ev, diag = dg,
                              structural_break = break_rb,
                              break_adjusted_var = break_var_rb,
                              var2 = var2_rb)
  }

  ## ---- cross-layer comparison ----
  cmp <- do.call(rbind, lapply(names(all_results), function(ln) data.frame(
    Layer = ln, TCI = round(all_results[[ln]]$static$TCI, 3),
    t(round(all_results[[ln]]$static$NET, 2)))))
  write.csv(cmp, file.path(OUT_ROOT, "Layer_Comparison_TCI_NET.csv"), row.names = FALSE)
  cat("\nCross-layer comparison:\n"); print(cmp)

  ## This is the valid inferential comparison: paired resampling of the same
  ## months, with Layer 2 minus Layer 1 as the estimand.
  cat("\nPaired moving-block bootstrap for Layer 2 minus Layer 1...\n")
  colnames(ret_nominal) <- colnames(ret_real) <- varnames
  paired_diff <- boot_layer_difference(
    ret_nominal, infl,
    all_results$layer1_nominal$static,
    all_results$layer2_real$static)
  paired_diff$TCI$Simultaneous_Lower95 <- NA_real_
  paired_diff$TCI$Simultaneous_Upper95 <- NA_real_
  paired_diff$TCI$Difference_identified <- with(
    paired_diff$TCI, Pointwise_Lower95 > 0 | Pointwise_Upper95 < 0)
  paired_diff$NET$Difference_identified <- with(
    paired_diff$NET, Simultaneous_Lower95 > 0 | Simultaneous_Upper95 < 0)
  paired_tbl <- rbind(paired_diff$TCI[, names(paired_diff$NET)], paired_diff$NET)
  write.csv(paired_tbl, file.path(OUT_ROOT, "Layer_Difference_PairedBootstrap.csv"),
            row.names = FALSE)

  writexl::write_xlsx(
    list(
      Static_comparison = cmp,
      Paired_difference = paired_tbl,
      Interpretation = data.frame(Note = c(
        "Difference is defined as Layer 2 minus Layer 1.",
        "Layer 2 changes the return definition by subtracting CPI inflation.",
        "This comparison is not a causal estimate of the effect of inflation.",
        "TCI uses its pointwise paired interval; the five NET differences use simultaneous bands."))),
    file.path(OUT_ROOT, "Layer1_Layer2_Comparison_Q1.xlsx"))

  writeLines(c(
    "LAYER 1 / LAYER 2 INTERPRETATION NOTE",
    "Layer 1 uses nominal monthly log returns.",
    "Layer 2 uses CPI-adjusted returns defined as nominal return minus CPI inflation.",
    "Therefore Layer 2 is a change-of-numeraire/return-definition check; it is not",
    "an identified causal test of whether inflation drives connectedness.",
    "Use Layer_Difference_PairedBootstrap.csv for claims about differences between",
    "the two layers. Do not infer a difference from two separately reported estimates",
    "or from overlap/non-overlap of separate confidence intervals."),
    file.path(OUT_ROOT, "Layer1_Layer2_Interpretation_Note.txt"))

  ## ---- reproducibility ----
  writeLines(capture.output(sessionInfo()), file.path(OUT_ROOT, "sessionInfo.txt"))
  writeLines(c(
    "Reproduce: place the unified R script next to data(3).xlsx (or pass the workbook path as the first argument).",
    "The unified script reads the raw `series` sheet directly and performs every transformation internally.",
    paste("Static seed:", BOOT_SEED, "| Bootstrap:", BOOT_REPS, BOOT_TYPE,
          "| block:", BOOT_BLOCK_LEN, "| H:", H_GFEVD),
    paste("Paired seed:", PAIRED_BOOT_SEED, "| Reps:", PAIRED_BOOT_REPS,
          "| block:", PAIRED_BOOT_BLOCK_LEN),
    paste("TVP train:", TRAIN_N, "| training sensitivity:",
          paste(TVP_TRAIN_SET, collapse = "/"), "| baseline kappa:",
          paste(c(TVP_KAPPA1, TVP_KAPPA2), collapse = "/")),
    paste("Layer-2 targeted robustness: Zivot-Andrews lag", ZA_LAG,
          "| Bai-Perron/supF trim", BREAK_TRIM,
          "| diagnostic VAR lag", ROBUST_VAR_LAG),
    "The Layer-1 pipeline and both baseline VAR(1) models are unchanged.",
    "Nominal TVP implementation sensitivity is exported in TVP_reference_recursion_comparison.csv and TVP_reference_recursion_path.csv; the validated baseline recursion is not replaced."),
    file.path(OUT_ROOT, "README_reproducibility.txt"))
  cat("\nDone:", format(Sys.time()), "\nOutputs in:", OUT_ROOT, "\n")
  sink()

})


while (sink.number() > 0) sink()
cat("\nLayers 1 and 2 completed. Running Layer 3 from raw EOM data...\n\n")

## ========================================================================
## INTERNAL LOCKED ENGINE: LAYER 3
## ========================================================================

local({
  ###############################################################################
  ##  layer3_eom_Q1_corrected.R
  ##
  ##  Q1.1 targeted corrections:
  ##  - exact input filenames and a paired Layer-3-vs-Layer-1 comparison;
  ##  - wild-block static bootstrap with simultaneous NET/NPDC bands;
  ##  - auditable TVP stability control and a complete common calendar window;
  ##  - tuning-stability terminology instead of unsupported robustness labels;
  ##  - lag-1/2/3 diagnostic sensitivity and non-mutating package checks.
  ##  All non-TVP point-estimation formulas and source data are unchanged.
  ##  The TVP covariance recursion is aligned with the final Layer-1/2 two-stage
  ##  recursion: prediction innovation for the Kalman gain, then filtered
  ##  residual for the covariance state carried to the next date.
  ##
  ##  Layer 3 measurement-robustness analysis:
  ##  the daily traded USD, Emami gold coin, and TEDPIX series are sampled at
  ##  the LAST AVAILABLE OBSERVATION in each Jalali month. The vehicle CPI
  ##  sub-index and Tehran housing-price series remain in their original monthly
  ##  form. All five series are converted to NOMINAL monthly log returns (x100).
  ##
  ##  This script is adapted directly from the approved V10.3 Layer 1/2 code and
  ##  preserves the same static VAR-GFEVD, strict-split TVP-VAR,
  ##  sensitivity grid, diagnostics, event checks, frequency decomposition,
  ##  generalized impulse responses, figures, and values-only workbooks.
  ##
  ##  IMPORTANT:
  ##  - No CPI deflation is applied here. Inflation robustness was already tested
  ##    separately in Layer 2.
  ##  - Layer 3 isolates only the effect of monthly sampling convention:
  ##    end-of-month observations versus monthly averages.
  ##
  ##  Unified input:
  ##    data(3).xlsx
  ##    Sheet `eom`: Layer-3 levels
  ##    Sheet `series`: Layer-1 monthly-average reference
  ##    Sample: 1395/01 to 1405/02 (122 price-level observations; 121 returns)
  ##
  ##  Core settings retained from V10.3:
  ##    VAR lag = 1; GFEVD horizon = 10; wild-block bootstrap B = 1000;
  ##    baseline TVP training = 36 months; kappa1 = kappa2 = 0.99;
  ##    sensitivity across six unique TVP specifications.
  ###############################################################################

  ## --------------------------- 0. Settings ---------------------------------- ##
  rm(list = ls())
  while (sink.number() > 0) sink()          # reset any open log from a failed run
  options(stringsAsFactors = FALSE, warn = 1)

  ## Unified wrapper: both Layer-3 EOM data and the Layer-1 reference are
  ## read from data(3).xlsx. The `eom` and `series` sheets are used below.
  DATA_FILE <- MASTER_DATA_FILE
  REFERENCE_FILE <- MASTER_DATA_FILE
  cat("Selected Layer-3 input file:", DATA_FILE, "\n")
  cat("Selected Layer-1 reference file:", REFERENCE_FILE, "\n")
  N_LAG          <- 1                   # baseline VAR lag (confirmed by VARselect)
  H_GFEVD        <- 10                  # GFEVD horizon
  BOOT_REPS      <- 1000                # static bootstrap replications
  BOOT_TYPE      <- "wild_block"        # "wild_block", "wild", or "iid"
  BOOT_BLOCK_LEN <- 6                    # months; common sign within each block
  BOOT_SEED      <- 12345
  PAIRED_BOOT_REPS      <- 1000          # Layer 3 EOM minus Layer 1 average
  PAIRED_BOOT_BLOCK_LEN <- 6             # circular moving blocks, in months
  PAIRED_BOOT_SEED      <- 35791

  TRAIN_N        <- 36                  # baseline TVP prior training window     [F1]
  TVP_KAPPA1     <- 0.99                # baseline forgetting factor, coefficients
  TVP_KAPPA2     <- 0.99                # baseline decay factor, error covariance
  TVP_TRAIN_SET  <- c(24, 36, 48)       # training-window sensitivity (referee 2.1)
  TVP_KAPPA_SET  <- list(c(0.99, 0.96), # (kappa1, kappa2) sensitivity (referee 2.2)
                         c(0.99, 0.98),
                         c(0.99, 0.99),
                         c(0.98, 0.96))
  TVP_ROOT_MAX   <- 0.999                # local VAR stability threshold
  TVP_STATE_SHRINK <- 0.99               # next-state shrink after rejected update

  GIRF_H         <- 10

  RUN_FREQUENCY  <- TRUE                # Barunik-Krehlik style band decomposition
  FREQ_GRID      <- 500                 # frequency grid points on (0, pi]
  ## Band definitions in MONTHS (period). Nyquist = 2 months.
  FREQ_BANDS     <- list("short_2_6m"  = c(2, 6),
                         "medium_6_12m" = c(6, 12),
                         "long_12m_Inf" = c(12, Inf))

  RUN_TVP_DGP_SENSITIVITY <- FALSE      # [F3] optional, honest label, OFF by default
  TVP_DGP_REPS            <- 300

  ## Event windows in Gregorian reporting months, inclusive. Dates represent
  ## Jalali period ends, so the full stated Gregorian months are included.
  EVENT_2018 <- as.Date(c("2018-04-01", "2018-10-31"))
  EVENT_2020 <- as.Date(c("2020-02-01", "2020-09-30"))

  MIN_SUBSAMPLE_OBS <- 60               # minimum obs to estimate a 5-var VAR(1)

  ## --------------------------- Packages ------------------------------------- ##
  if (is.null(getOption("repos")) || is.na(getOption("repos")["CRAN"]) ||
      getOption("repos")["CRAN"] == "@CRAN@")
    options(repos = c(CRAN = "https://cloud.r-project.org"))
  need <- c("readxl", "writexl", "zoo", "vars", "tseries")
  inst <- rownames(installed.packages())
  miss <- setdiff(need, inst)
  if (length(miss) > 0)
    stop("Missing required R packages: ", paste(miss, collapse = ", "),
         ". Install once with install.packages(c(",
         paste(sprintf("'%s'", miss), collapse = ", "), ")).")
  HAS_CA <- requireNamespace("ConnectednessApproach", quietly = TRUE)  # optional cross-check

  OUT_ROOT <- file.path(MASTER_OUT_DIR, "layer_3_eom")
  dir.create(OUT_ROOT, recursive = TRUE, showWarnings = FALSE)
  cat("Outputs will be written to:", OUT_ROOT, "\n")
  LOG_FILE <- file.path(OUT_ROOT, "00_run_log.txt")
  sink(LOG_FILE, split = TRUE)
  cat("Run started:", format(Sys.time()), "\n")
  cat("Data file  :", DATA_FILE, "\n")
  cat("Reference file:", REFERENCE_FILE, "\n")
  cat("ConnectednessApproach available for cross-check:", HAS_CA, "\n\n")

  ## --------------------------- 1. Load Layer-3 data ------------------------- ##
  EXPECTED_SHEET <- "eom"
  sheets <- readxl::excel_sheets(DATA_FILE)
  if (!(EXPECTED_SHEET %in% sheets)) {
    stop("Required sheet '", EXPECTED_SHEET, "' was not found in ", DATA_FILE,
         ". Available sheets: ", paste(sheets, collapse = ", "))
  }

  raw <- suppressMessages(readxl::read_excel(DATA_FILE, sheet = EXPECTED_SHEET))
  cat("Using sheet:", EXPECTED_SHEET, " | columns:",
      paste(names(raw), collapse = ", "), "\n")

  required_cols <- c(
    "month_jalali",
    "usd_last_observation_date_jalali",
    "usd_eom_rial",
    "gold_last_observation_date_jalali",
    "gold_coin_eom_rial",
    "tedpix_last_observation_date_jalali",
    "tedpix_eom",
    "vehicle_cpi_1400_100",
    "housing_price_irr_per_sqm"
  )
  missing_cols <- setdiff(required_cols, names(raw))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in ", EXPECTED_SHEET, ": ",
         paste(missing_cols, collapse = ", "))
  }

  normalize_jalali <- function(x) gsub("-", "/", trimws(as.character(x)), fixed = TRUE)

  input_dates <- data.frame(
    month_jalali = normalize_jalali(raw$month_jalali),
    USD_date     = normalize_jalali(raw$usd_last_observation_date_jalali),
    GOLD_date    = normalize_jalali(raw$gold_last_observation_date_jalali),
    STOCK_date   = normalize_jalali(raw$tedpix_last_observation_date_jalali),
    stringsAsFactors = FALSE
  )

  levels_df <- data.frame(
    month_jalali = input_dates$month_jalali,
    USD     = as.numeric(raw$usd_eom_rial),
    GOLD    = as.numeric(raw$gold_coin_eom_rial),
    STOCK   = as.numeric(raw$tedpix_eom),
    VEHICLE = as.numeric(raw$vehicle_cpi_1400_100),
    HOUSING = as.numeric(raw$housing_price_irr_per_sqm),
    stringsAsFactors = FALSE
  )

  varnames <- c("USD", "GOLD", "STOCK", "VEHICLE", "HOUSING")
  K <- length(varnames)

  ## ---- hard input checks: fail early rather than silently alter the sample ----
  if (anyNA(levels_df) || anyNA(input_dates)) {
    stop("Missing values detected in the Layer-3 input file.")
  }
  if (any(!is.finite(as.matrix(levels_df[, varnames])))) {
    stop("Non-finite market values detected in the Layer-3 input file.")
  }
  if (any(as.matrix(levels_df[, varnames]) <= 0)) {
    stop("All five price/index level series must be strictly positive before logs.")
  }
  if (anyDuplicated(levels_df$month_jalali)) {
    stop("Duplicate Jalali months detected in the Layer-3 input file.")
  }
  if (nrow(levels_df) != 122L ||
      levels_df$month_jalali[1] != "1395/01" ||
      tail(levels_df$month_jalali, 1) != "1405/02") {
    stop("Unexpected sample. Required: 122 months from 1395/01 through 1405/02; found ",
         nrow(levels_df), " rows from ", levels_df$month_jalali[1], " through ",
         tail(levels_df$month_jalali, 1), ".")
  }

  month_parts <- strsplit(levels_df$month_jalali, "/", fixed = TRUE)
  if (any(lengths(month_parts) != 2L)) {
    stop("month_jalali must use yyyy/mm format.")
  }
  month_index <- vapply(month_parts, function(p) {
    yy <- suppressWarnings(as.integer(p[1]))
    mm <- suppressWarnings(as.integer(p[2]))
    if (is.na(yy) || is.na(mm) || mm < 1L || mm > 12L) return(NA_integer_)
    yy * 12L + mm
  }, integer(1))
  if (anyNA(month_index) || any(diff(month_index) != 1L)) {
    stop("The Jalali monthly sequence is not complete and contiguous.")
  }

  for (v in c("USD", "GOLD", "STOCK")) {
    d <- input_dates[[paste0(v, "_date")]]
    if (any(substr(d, 1, 7) != levels_df$month_jalali)) {
      stop(v, " last-observation dates do not match their month_jalali keys.")
    }
  }

  ## Jalali "yyyy/mm" -> exact Gregorian date of the END of the Jalali month.
  ## This is the reporting calendar used throughout the paper: each Jalali month
  ## is labelled by the Gregorian month containing its final day. The resulting
  ## Date is also used for calendar-based event windows and Layer-4 year alignment.
  jalali_year_start <- function(jy) {
    breaks <- c(-61L, 9L, 38L, 199L, 426L, 686L, 756L, 818L, 1111L,
                1181L, 1210L, 1635L, 2060L, 2097L, 2192L, 2262L,
                2324L, 2394L, 2456L, 3178L)
    if (jy < breaks[1L] || jy >= tail(breaks, 1L))
      stop("Jalali year is outside the supported conversion range.")

    idiv <- function(a, b) trunc(a / b)
    imod <- function(a, b) a - idiv(a, b) * b

    gy <- jy + 621L
    leap_j <- -14L
    jp <- breaks[1L]
    jump <- NA_integer_

    for (k in 2:length(breaks)) {
      jm_break <- breaks[k]
      jump <- jm_break - jp
      if (jy < jm_break) break
      leap_j <- leap_j + idiv(jump, 33L) * 8L +
        idiv(imod(jump, 33L), 4L)
      jp <- jm_break
    }

    n <- jy - jp
    leap_j <- leap_j + idiv(n, 33L) * 8L +
      idiv(imod(n, 33L) + 3L, 4L)
    if (imod(jump, 33L) == 4L && (jump - n) == 4L)
      leap_j <- leap_j + 1L

    leap_g <- idiv(gy, 4L) -
      idiv((idiv(gy, 100L) + 1L) * 3L, 4L) - 150L
    march_day <- 20L + leap_j - leap_g

    as.Date(sprintf("%04d-03-%02d", gy, march_day))
  }

  jalali_to_date <- function(s) {
    p <- strsplit(s, "/", fixed = TRUE)[[1]]
    if (length(p) != 2L) stop("Jalali month must use yyyy/mm format.")
    jy <- suppressWarnings(as.integer(p[1L]))
    jm <- suppressWarnings(as.integer(p[2L]))
    if (is.na(jy) || is.na(jm) || jm < 1L || jm > 12L)
      stop("Invalid Jalali month: ", s)

    off <- c(0L, 31L, 62L, 93L, 124L, 155L,
             186L, 216L, 246L, 276L, 306L, 336L)

    next_month_start <- if (jm < 12L) {
      jalali_year_start(jy) + off[jm + 1L]
    } else {
      jalali_year_start(jy + 1L)
    }

    next_month_start - 1L
  }
  dates_levels <- as.Date(
    vapply(levels_df$month_jalali,
           function(s) as.numeric(jalali_to_date(s)),
           numeric(1)),
    origin = "1970-01-01"
  )

  cat("Sample (levels):", levels_df$month_jalali[1], "to",
      tail(levels_df$month_jalali, 1), "| n =", nrow(levels_df), "months\n")
  cat("Gregorian period-end reporting range:", format(min(dates_levels)), "to",
      format(max(dates_levels)), "\n")

  input_audit <- data.frame(
    Check = c("Required sheet", "Required columns", "No missing values",
              "Positive finite levels", "No duplicate months",
              "Complete monthly sequence", "Expected sample",
              "Last-observation dates match month keys"),
    Status = rep("PASS", 8),
    Detail = c(
      EXPECTED_SHEET,
      paste(required_cols, collapse = "; "),
      "All required cells used by the model are non-missing",
      "USD, gold coin, TEDPIX, vehicle CPI and housing are strictly positive",
      "122 unique month keys",
      "Every adjacent Jalali month differs by exactly one month",
      "1395/01-1405/02; 122 levels; 121 returns",
      "USD, gold coin and TEDPIX dates share the same yyyy/mm key"
    ),
    stringsAsFactors = FALSE
  )

  ## Layer-1 monthly-average reference, required for a direct paired comparison.
  ref_sheets <- readxl::excel_sheets(REFERENCE_FILE)
  ref_sheet <- NULL
  for (s in ref_sheets) {
    d <- suppressMessages(readxl::read_excel(REFERENCE_FILE, sheet = s, n_max = 5))
    nm_ref_try <- tolower(names(d))
    if (any(grepl("tedpix|stock", nm_ref_try)) && any(grepl("housing", nm_ref_try))) {
      ref_sheet <- s
      break
    }
  }
  if (is.null(ref_sheet)) stop("Could not find the Layer-1 market sheet in ", REFERENCE_FILE)
  ref_raw <- suppressMessages(readxl::read_excel(REFERENCE_FILE, sheet = ref_sheet))
  nm_ref <- tolower(names(ref_raw))
  ref_col <- function(patterns) {
    hit <- which(Reduce(`|`, lapply(patterns, function(p) grepl(p, nm_ref))))
    if (length(hit) == 0L)
      stop("Layer-1 reference column not found: ", paste(patterns, collapse = "|"))
    hit[1L]
  }
  reference_levels <- data.frame(
    month_jalali = substr(as.character(ref_raw[[ref_col(c("^date$", "month", "jalali"))]]), 1L, 7L),
    USD = as.numeric(ref_raw[[ref_col(c("^usd", "dollar"))]]),
    GOLD = as.numeric(ref_raw[[ref_col(c("gold", "sekeh", "coin"))]]),
    STOCK = as.numeric(ref_raw[[ref_col(c("tedpix", "stock"))]]),
    VEHICLE = as.numeric(ref_raw[[ref_col(c("vehicle", "car"))]]),
    HOUSING = as.numeric(ref_raw[[ref_col(c("housing"))]]),
    stringsAsFactors = FALSE)
  if (nrow(reference_levels) != nrow(levels_df) ||
      !identical(reference_levels$month_jalali, levels_df$month_jalali))
    stop("Layer-1 and Layer-3 month keys are not exactly aligned.")
  if (anyNA(reference_levels) ||
      any(!is.finite(as.matrix(reference_levels[, varnames]))) ||
      any(as.matrix(reference_levels[, varnames]) <= 0))
    stop("Layer-1 reference levels must be complete, finite and strictly positive.")
  if (!isTRUE(all.equal(reference_levels$VEHICLE, levels_df$VEHICLE, tolerance = 0)) ||
      !isTRUE(all.equal(reference_levels$HOUSING, levels_df$HOUSING, tolerance = 0)))
    stop("Vehicle and housing must be identical in Layer 1 and Layer 3; only the ",
         "three traded-market sampling conventions may differ.")
  input_audit <- rbind(input_audit, data.frame(
    Check = c("Layer-1 reference month alignment",
              "Shared vehicle/housing levels unchanged"),
    Status = c("PASS", "PASS"),
    Detail = c("122 identical Jalali month keys in identical order",
               "Vehicle and housing values are exactly identical across inputs"),
    stringsAsFactors = FALSE))
  write.csv(input_audit, file.path(OUT_ROOT, "Layer3_Input_Audit.csv"),
            row.names = FALSE)

  writeLines(c(
    "LAYER 3 MEASUREMENT NOTE",
    "USD, Emami gold coin and TEDPIX use the last available observation in each Jalali month.",
    "Vehicle and housing retain their originally reported monthly observations.",
    "All five series enter as nominal monthly log returns multiplied by 100.",
    "No CPI deflation is applied: inflation robustness belongs to Layer 2.",
    "Therefore Layer 3 isolates sensitivity to the monthly sampling convention.",
    "Layer3_vs_Layer1_PairedBootstrap.csv directly estimates EOM minus monthly-average differences using paired monthly blocks."
  ), file.path(OUT_ROOT, "Layer3_Measurement_Note.txt"))

  ## --------------------------- 2. Nominal end-of-month returns -------------- ##
  logret <- function(x) 100 * diff(log(x))
  ret_eom <- sapply(levels_df[, varnames, drop = FALSE], logret)
  colnames(ret_eom) <- varnames
  ret_average <- sapply(reference_levels[, varnames, drop = FALSE], logret)
  colnames(ret_average) <- varnames

  dates_ret <- dates_levels[-1]
  jal_ret   <- levels_df$month_jalali[-1]
  Tn <- nrow(ret_eom)

  if (Tn != 121L || anyNA(ret_eom) || any(!is.finite(ret_eom)) ||
      anyNA(ret_average) || any(!is.finite(ret_average))) {
    stop("Layer-3 return construction failed: expected 121 finite return observations.")
  }
  cat("Return sample:", jal_ret[1], "to", tail(jal_ret, 1),
      "| n =", Tn, "months\n\n")

  LAYERS <- list(
    layer3_eom = list(
      Y = ret_eom,
      label = paste(
        "Layer 3: nominal end-of-month robustness",
        "(USD, gold coin and TEDPIX use last monthly observations;",
        "vehicle and housing remain monthly)"
      )
    )
  )

  ## ====================== 3. Core connectedness functions ==================== ##
  ## Transparent, auditable implementations. [F8]

  ## MA (Wold) matrices Psi_0..Psi_{H-1} from a vars::VAR object (any lag p).
  get_psi <- function(var_model, H) {
    Ph <- vars::Phi(var_model, nstep = H - 1)   # array K x K x H, Phi_0 = I
    Ph
  }
  get_sigma <- function(var_model) {
    e <- stats::residuals(var_model)
    crossprod(e) / (nrow(e) - ncol(e) * var_model$p - 1)   # df-adjusted
  }

  ## Generalized FEVD (Pesaran-Shin), row-normalized to 100.
  ## Entry [i, j] = share of variable i's H-step FEV due to shocks in j.
  gfevd <- function(Psi, Sigma, H) {
    K <- ncol(Sigma)
    num <- matrix(0, K, K); den <- numeric(K)
    sjj <- diag(Sigma)
    for (h in 1:H) {
      P  <- Psi[, , h]
      PS <- P %*% Sigma
      num <- num + (PS^2) / matrix(sjj, K, K, byrow = TRUE)
      den <- den + diag(P %*% Sigma %*% t(P))
    }
    th <- num / den
    th <- th / rowSums(th)          # row normalization
    100 * th
  }

  ## Connectedness summary from a GFEVD table.
  conn_summary <- function(CT, varnames) {
    K <- ncol(CT)
    dimnames(CT) <- list(varnames, varnames)
    FROM <- rowSums(CT) - diag(CT)
    TO   <- colSums(CT) - diag(CT)
    NET  <- TO - FROM
    TCI  <- sum(FROM) / K
    ## NPDC: unique pairs, explicit orientation [F5]
    ## npdc_ij = CT[i, j] - CT[j, i] > 0  <=>  j is a NET transmitter to i.
    pairs <- t(combn(K, 2))
    NPDC <- data.frame(
      Var_i = varnames[pairs[, 1]],
      Var_j = varnames[pairs[, 2]],
      NPDC_ij = CT[cbind(pairs[, 1], pairs[, 2])] - CT[cbind(pairs[, 2], pairs[, 1])]
    )
    NPDC$Net_transmitter <- ifelse(NPDC$NPDC_ij > 0, NPDC$Var_j, NPDC$Var_i)
    NPDC$Net_receiver    <- ifelse(NPDC$NPDC_ij > 0, NPDC$Var_i, NPDC$Var_j)
    NPDC$Abs_NPDC        <- abs(NPDC$NPDC_ij)
    list(table = CT, FROM = FROM, TO = TO, NET = NET, TCI = TCI, NPDC = NPDC)
  }

  static_connectedness <- function(Y, p = N_LAG, H = H_GFEVD, exogen = NULL) {
    vm <- vars::VAR(Y, p = p, type = "const", exogen = exogen)
    cs <- conn_summary(gfevd(get_psi(vm, H), get_sigma(vm), H), colnames(Y))
    cs$model <- vm
    cs
  }

  ## Generalized impulse responses (in return units, one s.d. shocks).
  girf <- function(var_model, H) {
    Psi <- get_psi(var_model, H + 1)
    Sig <- get_sigma(var_model)
    K <- ncol(Sig); sj <- sqrt(diag(Sig))
    out <- array(NA_real_, dim = c(H + 1, K, K),
                 dimnames = list(paste0("h", 0:H), colnames(Sig), colnames(Sig)))
    for (h in 0:H) out[h + 1, , ] <- Psi[, , h + 1] %*% Sig %*% diag(1 / sj)
    out   # [horizon, response, shock]
  }

  ## ====================== 4. TVP-VAR with a clean prior ====================== ##
  ## Koop-Korobilis forgetting-factor Kalman filter with a STRICT split:
  ##   * prior = OLS on observations 1..train_n ONLY;
  ##   * Kalman updates start at t = train_n + 1 (training data are NOT fed
  ##     through the update step a second time);
  ##   * the first reported point is exactly the first post-training month.
  ##   * unstable local updates are audited and the last stable coefficient
  ##     state is carried forward for that date; no month is silently deleted.
  ## No look-ahead and no double use of the training sample.          [F1]
  tvp_var_filter <- function(Y, p = 1, train_n = TRAIN_N,
                             kappa1 = 0.99, kappa2 = 0.99, H = H_GFEVD,
                             root_max = TVP_ROOT_MAX,
                             state_shrink = TVP_STATE_SHRINK) {
    if (p != 1) stop("The custom TVP filter is implemented for p = 1 only.")
    K <- ncol(Y); Tn <- nrow(Y)
    X <- cbind(1, Y[1:(Tn - p), , drop = FALSE])         # p = 1 regressors
    Yt <- Y[(p + 1):Tn, , drop = FALSE]
    m  <- ncol(X); d <- K * m; Te <- nrow(Yt)
    if (train_n <= m + 5) stop("train_n too small for the prior OLS.")
    if (train_n >= Te - 5) stop("train_n leaves too few post-training observations.")
    ## ---- prior from training window only ----
    Xtr <- X[1:train_n, , drop = FALSE]; Ytr <- Yt[1:train_n, , drop = FALSE]
    XtXi <- solve(crossprod(Xtr))
    Btr  <- XtXi %*% crossprod(Xtr, Ytr)                 # m x K
    Etr  <- Ytr - Xtr %*% Btr
    S0   <- crossprod(Etr) / (train_n - m)
    prior_A <- t(Btr[2:m, , drop = FALSE])
    prior_root <- max(Mod(eigen(prior_A, only.values = TRUE)$values))
    if (!is.finite(prior_root) || prior_root >= root_max)
      stop(sprintf("TVP prior is unstable for train_n=%d (root=%.4f >= %.3f).",
                   train_n, prior_root, root_max))
    b_filter <- as.vector(Btr)
    b_report_prev <- b_filter
    P    <- kronecker(S0, XtXi)                          # Var(vec(B_ols))
    Sig  <- S0
    ## ---- filter over POST-TRAINING observations only ----
    TCI_path <- rep(NA_real_, Te)
    NET_path <- matrix(NA_real_, Te, K, dimnames = list(NULL, colnames(Y)))
    root_raw <- root_used <- rep(NA_real_, Te)
    action <- rep("training", Te)
    IK <- diag(K)
    for (t in (train_n + 1):Te) {
      b_before <- b_filter
      Zt <- kronecker(IK, matrix(X[t, ], nrow = 1))      # K x d
      P  <- P / kappa1                                   # forgetting (state noise)
      Sig_prev <- Sig
      e_pred <- as.numeric(Yt[t, ]) - as.numeric(Zt %*% b_before)
      Sig_pred <- kappa2 * Sig_prev + (1 - kappa2) * tcrossprod(e_pred)
      Sig_pred <- (Sig_pred + t(Sig_pred)) / 2
      Ft <- Zt %*% P %*% t(Zt) + Sig_pred
      Kt <- P %*% t(Zt) %*% solve(Ft)
      b_candidate <- b_before + as.numeric(Kt %*% e_pred)
      P <- P - Kt %*% Zt %*% P
      P <- (P + t(P)) / 2

      ## Complete the two-stage EWMA recursion: the one-step-ahead covariance
      ## above enters the Kalman gain, while the filtered covariance carried to
      ## the next date is based on the post-update residual.
      e_filt <- as.numeric(Yt[t, ]) - as.numeric(Zt %*% b_candidate)
      Sig <- kappa2 * Sig_prev + (1 - kappa2) * tcrossprod(e_filt)
      Sig <- (Sig + t(Sig)) / 2

      B_candidate <- matrix(b_candidate, m, K)
      A_candidate <- t(B_candidate[2:m, , drop = FALSE])
      root_candidate <- tryCatch(
        max(Mod(eigen(A_candidate, only.values = TRUE)$values)),
        error = function(e) Inf)
      root_raw[t] <- root_candidate
      if (is.finite(root_candidate) && root_candidate < root_max) {
        b_report <- b_candidate
        b_report_prev <- b_candidate
        b_filter <- b_candidate
        action[t] <- "updated"
      } else {
        b_report <- b_report_prev
        B_next <- matrix(b_before, m, K)
        B_next[2:m, ] <- state_shrink * B_next[2:m, ]
        b_filter <- as.vector(B_next)
        action[t] <- "carried_forward_unstable_update"
      }

      B_used <- matrix(b_report, m, K)
      A_used <- t(B_used[2:m, , drop = FALSE])
      root_used[t] <- max(Mod(eigen(A_used, only.values = TRUE)$values))
      if (!is.finite(root_used[t]) || root_used[t] >= root_max)
        stop("Internal error: the TVP state used for connectedness is not stable.")
      Psi <- array(NA_real_, dim = c(K, K, H)); Ph <- diag(K)
      for (h in 1:H) { Psi[, , h] <- Ph; Ph <- A_used %*% Ph }
      cst <- conn_summary(gfevd(Psi, Sig, H), colnames(Y))
      TCI_path[t] <- cst$TCI; NET_path[t, ] <- cst$NET
    }
    keep <- (train_n + 1):Te                             # exactly the filtered points
    if (any(!is.finite(TCI_path[keep])) || any(!is.finite(NET_path[keep, ])))
      stop("TVP filter produced a missing/non-finite reported value.")
    list(TCI = TCI_path, NET = NET_path, keep = keep,
         train_n = train_n, kappa1 = kappa1, kappa2 = kappa2,
         prior_root = prior_root,
         stability = data.frame(Filter_row = seq_len(Te), Raw_root = root_raw,
                                Used_root = root_used, Action = action),
         n_carried_forward = sum(action[keep] != "updated"))
  }

  ## TVP sensitivity engine (referee 2.1-2.3): runs a set of specifications and
  ## reports, per spec over a COMMON post-training window: TCI mean & sd,
  ## correlation of the TCI path with the baseline spec, mean NET per market,
  ## share of months with positive NET per market.
  tvp_sensitivity <- function(Y, train_set = TVP_TRAIN_SET, kappa_set = TVP_KAPPA_SET,
                              base_train = TRAIN_N, base_k = c(TVP_KAPPA1, TVP_KAPPA2)) {
    specs <- list()
    for (tr in train_set)
      specs[[sprintf("train%02d_k%.2f_%.2f", tr, base_k[1], base_k[2])]] <-
        list(train = tr, k1 = base_k[1], k2 = base_k[2])
    for (kk in kappa_set)
      specs[[sprintf("train%02d_k%.2f_%.2f", base_train, kk[1], kk[2])]] <-
        list(train = base_train, k1 = kk[1], k2 = kk[2])
    specs <- specs[!duplicated(names(specs))]
    attempts <- lapply(specs, function(s) tryCatch(
      tvp_var_filter(Y, train_n = s$train, kappa1 = s$k1, kappa2 = s$k2),
      error = function(e) structure(list(message = conditionMessage(e)), class = "tvp_error")))
    valid <- !vapply(attempts, inherits, logical(1), what = "tvp_error")
    spec_status <- data.frame(
      Spec = names(specs), Included = valid,
      Note = vapply(seq_along(attempts), function(i)
        if (valid[i]) "included" else attempts[[i]]$message, character(1)))
    fits <- attempts[valid]
    base_name <- sprintf("train%02d_k%.2f_%.2f", base_train, base_k[1], base_k[2])
    if (!base_name %in% names(fits)) stop("The baseline TVP specification is invalid.")
    ## Complete-calendar intersection after the largest INCLUDED training window.
    ## No pairwise deletion and no na.rm comparison are permitted.
    last_row <- nrow(fits[[1]]$NET)
    eligible <- (max(vapply(fits, `[[`, numeric(1), "train_n")) + 1):last_row
    is_complete <- vapply(eligible, function(tt) all(vapply(fits, function(f)
      is.finite(f$TCI[tt]) && all(is.finite(f$NET[tt, ])), logical(1))), logical(1))
    common <- eligible[is_complete]
    if (length(common) < 24 || any(diff(common) != 1))
      stop("TVP specifications do not share a sufficiently long contiguous complete window.")
    base_tci <- fits[[base_name]]$TCI[common]
    K <- ncol(Y); vn <- colnames(Y)
    stats <- do.call(rbind, lapply(names(fits), function(nm) {
      f <- fits[[nm]]; tci <- f$TCI[common]; net <- f$NET[common, , drop = FALSE]
      row <- data.frame(Spec = nm, Train_n = f$train_n, kappa1 = f$kappa1, kappa2 = f$kappa2,
                        Is_baseline = nm == base_name,
                        TCI_mean = mean(tci), TCI_sd = sd(tci),
                        Corr_with_baseline = suppressWarnings(cor(tci, base_tci)),
                        N_common_months = length(common),
                        N_carried_forward = f$n_carried_forward,
                        Pct_carried_forward = 100 * f$n_carried_forward / length(f$keep),
                        Prior_root = f$prior_root,
                        Max_raw_root = max(f$stability$Raw_root[f$keep]),
                        Max_used_root = max(f$stability$Used_root[f$keep]))
      for (v in vn) {
        row[[paste0("NETmean_", v)]] <- mean(net[, v])
        row[[paste0("PctPos_", v)]]  <- 100 * mean(net[, v] > 0)
      }
      row
    }))
    ## role-sign stability across specifications (referee 2.3)
    roles <- do.call(rbind, lapply(vn, function(v) {
      sg <- sign(stats[[paste0("NETmean_", v)]])
      data.frame(Segment = v,
                 N_specs_positive = sum(sg > 0), N_specs_negative = sum(sg < 0),
                 Sign_flips_across_specs = length(unique(sg)) > 1)
    }))
    list(specs = names(fits), fits = fits, stats = stats, roles = roles,
         common = common, base_name = base_name, spec_status = spec_status)
  }

  ## ====================== 5. Frequency decomposition (BK) ===================== ##
  ## Spectral generalized FEVD aggregated over period bands (months).   [F8]
  freq_connectedness <- function(var_model, bands = FREQ_BANDS, ngrid = FREQ_GRID) {
    A <- vars::Acoef(var_model); p <- length(A); K <- ncol(A[[1]])
    Sig <- get_sigma(var_model); sjj <- diag(Sig)
    omg <- seq(1e-4, pi, length.out = ngrid)
    num <- array(0, dim = c(K, K, ngrid)); den <- matrix(0, K, ngrid)
    for (s in seq_along(omg)) {
      Aw <- diag(K) + 0i
      for (l in 1:p) Aw <- Aw - A[[l]] * exp(-1i * omg[s] * l)
      Pw <- solve(Aw)
      PS <- Pw %*% Sig
      num[, , s] <- (Mod(PS)^2) / matrix(sjj, K, K, byrow = TRUE)
      den[, s]   <- Re(diag(Pw %*% Sig %*% Conj(t(Pw))))
    }
    den_tot <- rowSums(den)
    ## Proper BK normalization: unnormalized total-frequency table, then
    ## row-normalize so that band tables sum EXACTLY to the DY total table.
    num_tot <- apply(num, c(1, 2), sum)
    Th_all  <- num_tot / den_tot                # unnormalized theta (rows / den_i)
    row_tot <- rowSums(Th_all)
    out <- list()
    for (bn in names(bands)) {
      per <- bands[[bn]]                                  # months (period)
      lo  <- if (is.infinite(per[2])) 0 else 2 * pi / per[2]
      hi  <- 2 * pi / per[1]
      idx <- which(omg > lo & omg <= hi)
      CTb <- 100 * (apply(num[, , idx, drop = FALSE], c(1, 2), sum) / den_tot) / row_tot
      dimnames(CTb) <- list(colnames(Sig), colnames(Sig))
      FROMb <- rowSums(CTb) - diag(CTb); TOb <- colSums(CTb) - diag(CTb)
      out[[bn]] <- list(table = CTb, FROM = FROMb, TO = TOb,
                        NET = TOb - FROMb, TCI = sum(FROMb) / K)
    }
    out
  }

  ## ====================== 6. Bootstrap (static model) ======================== ##
  draw_boot_residuals <- function(E, type = BOOT_TYPE, block_len = BOOT_BLOCK_LEN) {
    Te <- nrow(E)
    if (type == "wild")
      return(E * sample(c(-1, 1), Te, replace = TRUE))
    if (type == "iid")
      return(E[sample.int(Te, replace = TRUE), , drop = FALSE])
    if (type == "wild_block") {
      if (block_len < 2 || block_len > Te) stop("Invalid BOOT_BLOCK_LEN.")
      offset <- sample.int(block_len, 1) - 1
      block_id <- ((seq_len(Te) - 1 + offset) %/% block_len) + 1
      signs <- sample(c(-1, 1), max(block_id), replace = TRUE)
      return(E * signs[block_id])
    }
    stop("Unknown BOOT_TYPE: ", type)
  }

  boot_static <- function(var_model, Y, B = BOOT_REPS, H = H_GFEVD,
                          type = BOOT_TYPE, seed = BOOT_SEED, girf_H = GIRF_H,
                          block_len = BOOT_BLOCK_LEN) {
    set.seed(seed)
    K <- ncol(Y); p <- var_model$p
    A <- vars::Acoef(var_model); cst <- vars::Bcoef(var_model)[, "const"]
    E <- stats::residuals(var_model); Te <- nrow(E)
    base_cs <- conn_summary(gfevd(get_psi(var_model, H), get_sigma(var_model), H),
                            colnames(Y))
    TCIb <- numeric(0)
    NETb <- matrix(NA_real_, B, K, dimnames = list(NULL, colnames(Y)))
    npdc_names <- apply(t(combn(colnames(Y), 2)), 1, paste, collapse = "__")
    NPDCb <- matrix(NA_real_, B, length(npdc_names), dimnames = list(NULL, npdc_names))
    GIRFb <- array(NA_real_, dim = c(B, girf_H + 1, K, K))
    for (bb in 1:B) {
      if (bb %% 100 == 0) cat("  bootstrap", bb, "of", B, "\n")
      Eb <- draw_boot_residuals(E, type = type, block_len = block_len)
      Yb <- matrix(NA_real_, Te + p, K); Yb[1:p, ] <- Y[1:p, , drop = FALSE]
      for (t in 1:Te) {
        acc <- cst
        for (l in 1:p) acc <- acc + A[[l]] %*% Yb[t + p - l, ]
        Yb[t + p, ] <- acc + Eb[t, ]
      }
      colnames(Yb) <- colnames(Y)
      fit <- tryCatch(vars::VAR(Yb, p = p, type = "const"), error = function(e) NULL)
      if (is.null(fit)) next
      if (max(vars::roots(fit)) >= 1) next
      cs <- tryCatch(conn_summary(gfevd(get_psi(fit, H), get_sigma(fit), H), colnames(Y)),
                     error = function(e) NULL)
      if (is.null(cs)) next
      TCIb <- c(TCIb, cs$TCI); NETb[bb, ] <- cs$NET
      NPDCb[bb, ] <- cs$NPDC$NPDC_ij
      GIRFb[bb, , , ] <- tryCatch(girf(fit, girf_H), error = function(e) NA)
    }
    if (length(TCIb) < 0.9 * B || sum(complete.cases(NETb)) < 0.9 * B ||
        sum(complete.cases(NPDCb)) < 0.9 * B)
      stop("Too few successful stable static-bootstrap replications.")
    q <- function(x, p) as.numeric(quantile(x, p, na.rm = TRUE))
    valid_net <- complete.cases(NETb)
    valid_npdc <- complete.cases(NPDCb)
    net_max_dev <- apply(abs(sweep(NETb[valid_net, , drop = FALSE], 2,
                                   base_cs$NET, "-")), 1, max)
    npdc_max_dev <- apply(abs(sweep(NPDCb[valid_npdc, , drop = FALSE], 2,
                                    base_cs$NPDC$NPDC_ij, "-")), 1, max)
    net_crit <- q(net_max_dev, .95)
    npdc_crit <- q(npdc_max_dev, .95)
    list(TCI = c(Lower95 = q(TCIb, .025), Upper95 = q(TCIb, .975), Mean = mean(TCIb, na.rm = TRUE)),
         NET = data.frame(Segment = colnames(Y),
                          Pointwise_Lower95 = apply(NETb, 2, q, .025),
                          Pointwise_Upper95 = apply(NETb, 2, q, .975),
                          Simultaneous_Lower95 = base_cs$NET - net_crit,
                          Simultaneous_Upper95 = base_cs$NET + net_crit,
                          BootMean = colMeans(NETb, na.rm = TRUE)),
         NPDC = data.frame(Pair = npdc_names,
                           Pointwise_Lower95 = apply(NPDCb, 2, q, .025),
                           Pointwise_Upper95 = apply(NPDCb, 2, q, .975),
                           Simultaneous_Lower95 = base_cs$NPDC$NPDC_ij - npdc_crit,
                           Simultaneous_Upper95 = base_cs$NPDC$NPDC_ij + npdc_crit,
                           BootMean = colMeans(NPDCb, na.rm = TRUE)),
         GIRF_lo = apply(GIRFb, c(2, 3, 4), q, .025),
         GIRF_hi = apply(GIRFb, c(2, 3, 4), q, .975),
         reps_used = length(TCIb))
  }

  ## Paired circular moving-block bootstrap for the estimand Layer 3 EOM minus
  ## Layer 1 monthly average. Identical month indices are applied to both series.
  circular_block_indices <- function(n, block_len) {
    idx <- integer(0)
    while (length(idx) < n) {
      s <- sample.int(n, 1)
      idx <- c(idx, ((s - 1 + 0:(block_len - 1)) %% n) + 1)
    }
    idx[seq_len(n)]
  }

  boot_sampling_difference <- function(Y_average, Y_eom, cs_average, cs_eom,
                                       B = PAIRED_BOOT_REPS,
                                       block_len = PAIRED_BOOT_BLOCK_LEN,
                                       seed = PAIRED_BOOT_SEED) {
    set.seed(seed)
    n <- nrow(Y_eom); K <- ncol(Y_eom)
    if (!identical(dim(Y_average), dim(Y_eom)) ||
        !identical(colnames(Y_average), colnames(Y_eom)))
      stop("Layer-1 and Layer-3 return matrices are not exactly aligned.")
    d_tci <- rep(NA_real_, B)
    d_net <- matrix(NA_real_, B, K, dimnames = list(NULL, colnames(Y_eom)))
    for (bb in seq_len(B)) {
      if (bb %% 100 == 0) cat("  paired bootstrap", bb, "of", B, "\n")
      ii <- circular_block_indices(n, block_len)
      ca <- tryCatch(static_connectedness(Y_average[ii, , drop = FALSE]),
                     error = function(e) NULL)
      ce <- tryCatch(static_connectedness(Y_eom[ii, , drop = FALSE]),
                     error = function(e) NULL)
      if (is.null(ca) || is.null(ce)) next
      if (max(vars::roots(ca$model)) >= 1 || max(vars::roots(ce$model)) >= 1) next
      d_tci[bb] <- ce$TCI - ca$TCI
      d_net[bb, ] <- ce$NET - ca$NET
    }
    ok <- is.finite(d_tci) & complete.cases(d_net)
    used <- sum(ok)
    if (used < 0.9 * B) stop("Too few successful paired-bootstrap replications.")
    q <- function(x, p) as.numeric(quantile(x, p, na.rm = TRUE))
    point_net <- cs_eom$NET - cs_average$NET
    max_dev <- apply(abs(sweep(d_net[ok, , drop = FALSE], 2, point_net, "-")), 1, max)
    crit <- q(max_dev, .95)
    list(
      TCI = data.frame(
        Measure = "TCI",
        Estimate_EOM_minus_Average = cs_eom$TCI - cs_average$TCI,
        Pointwise_Lower95 = q(d_tci[ok], .025),
        Pointwise_Upper95 = q(d_tci[ok], .975),
        Simultaneous_Lower95 = NA_real_, Simultaneous_Upper95 = NA_real_,
        Reps_Used = used),
      NET = data.frame(
        Measure = paste0("NET_", colnames(Y_eom)),
        Estimate_EOM_minus_Average = point_net,
        Pointwise_Lower95 = apply(d_net[ok, , drop = FALSE], 2, q, .025),
        Pointwise_Upper95 = apply(d_net[ok, , drop = FALSE], 2, q, .975),
        Simultaneous_Lower95 = point_net - crit,
        Simultaneous_Upper95 = point_net + crit,
        Reps_Used = used))
  }

  ## ====================== 7. Diagnostics [F4] ================================ ##
  diagnostics <- function(var_model, Y_levels_log = NULL, Y_ret = NULL) {
    K <- var_model$K; Te <- var_model$obs
    out <- list()
    ## lag selection (for the record)
    out$lag_selection <- as.data.frame(t(vars::VARselect(var_model$y, lag.max = 4,
                                                         type = "const")$selection))
    ## stability
    out$stability <- data.frame(Max_Modulus = max(vars::roots(var_model)),
                                Stable = max(vars::roots(var_model)) < 1)
    ## multivariate serial correlation
    pt <- vars::serial.test(var_model, lags.pt = 12, type = "PT.adjusted")
    out$serial <- data.frame(Test = "Portmanteau (adj), lag 12",
                             Statistic = unname(pt$serial$statistic),
                             P_value = unname(pt$serial$p.value))
    ## multivariate normality: correct extraction  [F4]
    nt <- tryCatch(vars::normality.test(var_model, multivariate.only = TRUE),
                   error = function(e) NULL)
    if (!is.null(nt)) {
      out$normality <- data.frame(
        Test = c("Multivariate Jarque-Bera", "Multivariate skewness", "Multivariate kurtosis"),
        Statistic = c(unname(nt$jb.mul$JB$statistic),
                      unname(nt$jb.mul$Skewness$statistic),
                      unname(nt$jb.mul$Kurtosis$statistic)),
        P_value = c(unname(nt$jb.mul$JB$p.value),
                    unname(nt$jb.mul$Skewness$p.value),
                    unname(nt$jb.mul$Kurtosis$p.value)))
    }
    ## multivariate ARCH: NOT valid here -- documented, not reported  [F4]
    out$arch_note <- paste0(
      "Multivariate ARCH-LM omitted by design: with K=5 the test has ",
      "lags*(K(K+1)/2)^2 = lags*225 parameters vs T=", Te,
      " observations; it is saturated/invalid. Per-equation ARCH screening uses the ",
      "McLeod-Li test (Ljung-Box on squared residuals, 12 lags), reported as ",
      "ARCH_McLeodLi12_p.")
    ## per-equation diagnostics
    E <- stats::residuals(var_model)
    per_eq <- do.call(rbind, lapply(colnames(E), function(v) {
      e <- E[, v]
      lb <- Box.test(e, lag = 12, type = "Ljung-Box", fitdf = var_model$p)
      jb <- tseries::jarque.bera.test(e)
      ar <- Box.test(e^2, lag = 12, type = "Ljung-Box")      # McLeod-Li ARCH screen
      data.frame(Equation = v,
                 LjungBox12_p = lb$p.value, JB_p = jb$p.value,
                 ARCH_McLeodLi12_p = ar$p.value,
                 LjungBox_flag = lb$p.value < 0.05,
                 NonNormal_flag = jb$p.value < 0.05,
                 ARCH_flag = ar$p.value < 0.05)
    }))
    out$per_equation <- per_eq
    ## unit-root screen on returns (ADF, PP, KPSS)   [F4]
    if (!is.null(Y_ret)) {
      out$unit_roots <- do.call(rbind, lapply(colnames(Y_ret), function(v) {
        x <- Y_ret[, v]
        data.frame(Segment = v,
                   ADF_p  = suppressWarnings(tseries::adf.test(x)$p.value),
                   PP_p   = suppressWarnings(tseries::pp.test(x)$p.value),
                   KPSS_p = suppressWarnings(tseries::kpss.test(x)$p.value))
      }))
      out$unit_roots$KPSS_reject_5pct <- out$unit_roots$KPSS_p < 0.05
    }
    out$honest_summary <- paste(
      "Diagnostics summary:",
      sprintf("stable VAR (max root %.3f);", out$stability$Max_Modulus),
      sprintf("Portmanteau p=%.3f;", out$serial$P_value),
      sprintf("%d/%d equations show non-normal residuals;",
              sum(per_eq$NonNormal_flag), K),
      sprintf("%d/%d equations show ARCH effects.", sum(per_eq$ARCH_flag), K),
      "A blanket 'all diagnostics passed' claim is NOT supported; see tables.")
    out
  }

  ## Lag diagnostic sensitivity. The baseline remains p=1 (selected by HQ/SC/FPE),
  ## while p=2 and p=3 address residual persistence and the AIC disagreement.
  lag_diagnostic_sensitivity <- function(Y, lags = 1:3, H = H_GFEVD) {
    fits <- lapply(lags, function(p) {
      cs <- static_connectedness(Y, p = p, H = H)
      dg <- diagnostics(cs$model, Y_ret = NULL)
      list(p = p, cs = cs, dg = dg)
    })
    summary <- do.call(rbind, lapply(fits, function(z) data.frame(
      Lag = z$p, TCI = z$cs$TCI,
      Max_Modulus = z$dg$stability$Max_Modulus,
      Stable = z$dg$stability$Stable,
      Portmanteau12_p = z$dg$serial$P_value,
      Housing_LjungBox12_p = z$dg$per_equation$LjungBox12_p[
        match("HOUSING", z$dg$per_equation$Equation)],
      Housing_ARCH_McLeodLi12_p = z$dg$per_equation$ARCH_McLeodLi12_p[
        match("HOUSING", z$dg$per_equation$Equation)])))
    per_equation <- do.call(rbind, lapply(fits, function(z)
      cbind(Lag = z$p, z$dg$per_equation)))
    list(summary = summary, per_equation = per_equation, fits = fits)
  }

  ## ====================== 8. Event robustness [F2] =========================== ##
  event_robustness <- function(Y, dates, H = H_GFEVD) {
    d18 <- as.numeric(dates >= EVENT_2018[1] & dates <= EVENT_2018[2])
    d20 <- as.numeric(dates >= EVENT_2020[1] & dates <= EVENT_2020[2])
    res <- list()
    ## (a) exogenous event dummies, full sample -- no gaps created
    ex <- cbind(D2018 = d18, D2020 = d20)
    res$dummies <- static_connectedness(Y, exogen = ex)
    ## (b) contiguous subsamples with enough observations only
    seg_list <- list(
      post2020 = which(dates > EVENT_2020[2]),
      pre2018  = which(dates < EVENT_2018[1]),
      between  = which(dates > EVENT_2018[2] & dates < EVENT_2020[1])
    )
    res$subsamples <- list(); res$skipped <- character(0)
    for (sn in names(seg_list)) {
      idx <- seg_list[[sn]]
      if (length(idx) >= MIN_SUBSAMPLE_OBS) {
        res$subsamples[[sn]] <- static_connectedness(Y[idx, , drop = FALSE])
      } else {
        res$skipped <- c(res$skipped,
          sprintf("%s skipped: only %d obs (< %d needed for a %d-variable VAR).",
                  sn, length(idx), MIN_SUBSAMPLE_OBS, ncol(Y)))
      }
    }
    res$note <- paste(
      "Event robustness re-designed: no observation-splicing across time gaps.",
      "Primary check = exogenous event dummies (full sample);",
      "secondary check = contiguous subsamples with >=", MIN_SUBSAMPLE_OBS, "obs.",
      paste(res$skipped, collapse = " "))
    res
  }

  ## ====================== 9. Figures (all data-driven) ======================= ##
  ## Every figure closes its device via on.exit(), so an error inside one
  ## figure can never corrupt or block the figures that follow.
  fig_returns <- function(Y, dates, file, main) {
    png(file, width = 2400, height = 2800, res = 300)
    on.exit(try(dev.off(), silent = TRUE), add = TRUE)
    op <- par(mfrow = c(ncol(Y), 1), mar = c(2, 4, 2, 1))
    for (v in colnames(Y)) { plot(dates, Y[, v], type = "l", col = "steelblue4",
                                  ylab = v, xlab = "", main = if (v == colnames(Y)[1]) main else "")
                             abline(h = 0, col = "grey70") }
    par(op)
  }
  fig_heatmap <- function(CT, file, main) {
    png(file, width = 2200, height = 1900, res = 300)
    on.exit(try(dev.off(), silent = TRUE), add = TRUE)
    K <- ncol(CT)
    op <- par(mar = c(5, 6, 3, 1))
    image(1:K, 1:K, t(CT[K:1, ]), axes = FALSE, xlab = "FROM shock in (column)",
          ylab = "", col = hcl.colors(50, "YlOrRd", rev = TRUE), main = main)
    axis(1, at = 1:K, labels = colnames(CT)); axis(2, at = 1:K, labels = rev(rownames(CT)), las = 1)
    for (i in 1:K) for (j in 1:K)
      text(j, K + 1 - i, sprintf("%.1f", CT[i, j]), cex = 0.9,
           col = ifelse(CT[i, j] > max(CT) * .6, "white", "black"))
    par(op)
  }
  fig_net_ci <- function(net, bootNET, file, main) {
    png(file, width = 2200, height = 1500, res = 300)
    on.exit(try(dev.off(), silent = TRUE), add = TRUE)
    o <- order(net); nv <- net[o]; bn <- bootNET[o, ]
    cols <- ifelse(nv >= 0, "steelblue4", "firebrick")
    op <- par(mar = c(5, 6, 3, 1))
    bp <- barplot(nv, names.arg = names(nv), horiz = TRUE, col = cols, las = 1,
                  xlim = range(c(bn$Simultaneous_Lower95,
                                 bn$Simultaneous_Upper95, 0)) * 1.15, main = main,
                  xlab = "NET directional connectedness (95% simultaneous bootstrap band)")
    arrows(bn$Simultaneous_Lower95, bp, bn$Simultaneous_Upper95, bp,
           angle = 90, code = 3, length = 0.05)
    abline(v = 0, lty = 2); par(op)
  }
  fig_network <- function(cs, bootNPDC, file, main) {                    # [F6]
    png(file, width = 2300, height = 2100, res = 300)
    on.exit(try(dev.off(), silent = TRUE), add = TRUE)
    K <- length(cs$NET); vn <- names(cs$NET)
    ang <- pi / 2 - 2 * pi * (0:(K - 1)) / K
    pos <- cbind(0.5 + 0.34 * cos(ang), 0.52 + 0.32 * sin(ang)); rownames(pos) <- vn
    op <- par(mar = c(1, 1, 3, 1))
    plot(NA, xlim = c(0, 1), ylim = c(-0.02, 1), axes = FALSE, xlab = "", ylab = "", main = main)
    np <- cs$NPDC; np$Pair <- paste(np$Var_i, np$Var_j, sep = "__")
    np <- merge(np, bootNPDC, by = "Pair")
    np$Significant <- np$Simultaneous_Lower95 > 0 |
                      np$Simultaneous_Upper95 < 0
    ## draw ALL 10 pairs: significant = solid dark with label,
    ## non-significant = light dashed (honest: estimated but not identified)
    for (r in order(np$Significant)) {
      a <- pos[np$Net_transmitter[r], ]; b <- pos[np$Net_receiver[r], ]
      sh <- 0.075 * (b - a) / sqrt(sum((b - a)^2))
      if (np$Significant[r]) {
        arrows(a[1] + sh[1], a[2] + sh[2], b[1] - sh[1], b[2] - sh[2],
               length = 0.11, lwd = 1.2 + 0.30 * np$Abs_NPDC[r], col = "grey30")
        text((a[1] + b[1]) / 2 + 0.012, (a[2] + b[2]) / 2 + 0.028,
             sprintf("%.1f", np$Abs_NPDC[r]), cex = 0.85, font = 2)
      } else {
        arrows(a[1] + sh[1], a[2] + sh[2], b[1] - sh[1], b[2] - sh[2],
               length = 0.07, lwd = 0.9, col = "grey80", lty = 2)
      }
    }
    for (v in vn) {
      cl <- if (cs$NET[v] >= 0) "steelblue4" else "firebrick"     # colour by SIGN of NET
      points(pos[v, 1], pos[v, 2], pch = 21, bg = cl, col = "white", lwd = 2,
             cex = 5.2 + 3.6 * sqrt(abs(cs$NET[v]) / max(abs(cs$NET))))
      text(pos[v, 1], pos[v, 2], substr(v, 1, 3), col = "white", font = 2, cex = 0.95)
      text(pos[v, 1], pos[v, 2] - 0.105, sprintf("%s  NET %+.1f", v, cs$NET[v]),
           col = cl, font = 2, cex = 0.85)
    }
    legend("bottomleft", bty = "n", cex = 0.8, lty = c(1, 2), lwd = c(2, 1),
           col = c("grey30", "grey80"),
           legend = c("NPDC identified (95% simultaneous band excludes 0)",
                      "NPDC estimated but not significant"))
    legend("bottomright", bty = "n", cex = 0.8, pch = 21, pt.cex = 1.6,
           pt.bg = c("steelblue4", "firebrick"),
           legend = c("NET transmitter (estimate)", "NET receiver (estimate)"))
    par(op)
  }
  fig_tvp_tci <- function(paths, dates_all, file, main, note) {
    ## paths: named list of full-length TCI vectors (NA outside each keep window)
    png(file, width = 2400, height = 1400, res = 300)
    on.exit(try(dev.off(), silent = TRUE), add = TRUE)
    op <- par(mar = c(4, 4, 3, 1))
    ok <- Reduce(`|`, lapply(paths, function(z) !is.na(z)))
    xr <- range(dates_all[ok]); yr <- range(unlist(paths), na.rm = TRUE)
    cols <- hcl.colors(length(paths), "Dark 3")
    plot(dates_all, paths[[1]], type = "n", xlim = xr, ylim = yr,
         xlab = "", ylab = "TCI (%)", main = main)      # Date x-axis preserved
    for (i in seq_along(paths))
      lines(dates_all, paths[[i]], col = cols[i],
            lwd = ifelse(i == length(paths), 2.4, 1.3))
    legend("topright", bty = "n", lwd = 2, col = cols, cex = 0.75,
           legend = names(paths))
    mtext(note, side = 3, line = -1.3, cex = 0.7, adj = 0.02)
    par(op)
  }
  fig_tvp_net <- function(tvp, ts_res, dates_all, file, main) {
    ## Baseline NET path with the pointwise min-max tuning range over the complete
    ## common calendar. This range is descriptive and is not a confidence band.
    png(file, width = 2400, height = 2800, res = 300)
    on.exit(try(dev.off(), silent = TRUE), add = TRUE)
    K <- ncol(tvp$NET); vn <- colnames(tvp$NET)
    Te <- nrow(tvp$NET); nspec <- length(ts_res$specs)
    band_lo <- matrix(NA_real_, Te, K, dimnames = list(NULL, vn))
    band_hi <- band_lo
    idx <- ts_res$common
    for (t in idx) for (v in vn) {
      vals <- vapply(ts_res$specs, function(nm) ts_res$fits[[nm]]$NET[t, v], numeric(1))
      if (sum(!is.na(vals)) >= 2) { band_lo[t, v] <- min(vals, na.rm = TRUE)
                                    band_hi[t, v] <- max(vals, na.rm = TRUE) }
    }
    op <- par(mfrow = c(K, 1), mar = c(2, 4, 2, 1))
    for (v in vn) {
      yl <- range(c(band_lo[, v], band_hi[, v], tvp$NET[idx, v]), na.rm = TRUE)
      plot(dates_all[idx], tvp$NET[idx, v], type = "n", ylim = yl,
           ylab = paste("NET", v), xlab = "",
           main = if (v == vn[1]) main else "")
      bb <- which(!is.na(band_lo[, v]))
      if (length(bb) > 1)
        polygon(c(dates_all[bb], rev(dates_all[bb])),
                c(band_lo[bb, v], rev(band_hi[bb, v])),
                col = adjustcolor("steelblue", 0.20), border = NA)
      abline(h = 0, col = "grey60")
      lines(dates_all[idx], tvp$NET[idx, v], col = "steelblue4", lwd = 2)
      if (v == vn[1])
        legend("topright", bty = "n", cex = 0.75,
               lty = c(1, NA), lwd = c(2, NA), pch = c(NA, 15), pt.cex = c(NA, 1.8),
               col = c("steelblue4", adjustcolor("steelblue", 0.35)),
               legend = c("Baseline specification",
                          sprintf("Min-max tuning range across %d specs (not a CI)", nspec)))
    }
    par(op)
  }
  fig_girf <- function(G, lo, hi, file, main) {
    K <- dim(G)[2]; vn <- dimnames(G)[[2]]; H <- dim(G)[1] - 1
    png(file, width = 3200, height = 3200, res = 300)
    on.exit(try(dev.off(), silent = TRUE), add = TRUE)
    op <- par(mfrow = c(K, K), mar = c(2, 2.5, 1.6, 0.6), oma = c(2, 2, 3, 1))
    for (i in 1:K) for (j in 1:K) {
      yl <- range(c(G[, i, j], lo[, i, j], hi[, i, j]), na.rm = TRUE)
      plot(0:H, G[, i, j], type = "l", lwd = 1.8, col = "steelblue4", ylim = yl,
           main = paste0(vn[i], " <- shock ", vn[j]), cex.main = 0.8, xlab = "", ylab = "")
      polygon(c(0:H, H:0), c(lo[, i, j], rev(hi[, i, j])), col = adjustcolor("steelblue", 0.25), border = NA)
      lines(0:H, G[, i, j], lwd = 1.8, col = "steelblue4"); abline(h = 0, col = "grey60")
    }
    mtext(main, outer = TRUE, cex = 1.0, font = 2)
    mtext("Generalized IRs (not structural). h=0 responses reflect contemporaneous innovation correlation.",
          outer = TRUE, side = 1, cex = 0.7)
    par(op)
  }
  fig_freq_net <- function(fq, file, main) {
    png(file, width = 2400, height = 1500, res = 300)
    on.exit(try(dev.off(), silent = TRUE), add = TRUE)
    M <- t(sapply(fq, function(b) b$NET))                # bands x markets
    op <- par(mar = c(5, 5, 3, 1))
    barplot(M, beside = TRUE, col = hcl.colors(nrow(M), "Teal"), las = 1,
            ylab = "NET connectedness", main = main,
            legend.text = paste0(rownames(M), sprintf("  (TCI %.1f)",
                                 vapply(fq, `[[`, numeric(1), "TCI"))),
            args.legend = list(bty = "n", cex = 0.8, x = "bottomright"))
    abline(h = 0)
    par(op)
  }

  ## ====================== 10. Run Layer 3 =================================== ##
  all_results <- list()

  for (ln in names(LAYERS)) {
    cat("\n==============================\n", LAYERS[[ln]]$label, "\n==============================\n")
    Y <- LAYERS[[ln]]$Y; colnames(Y) <- varnames
    odir <- file.path(OUT_ROOT, ln); tdir <- file.path(odir, "tables_csv"); fdir <- file.path(odir, "figures")
    dir.create(tdir, recursive = TRUE, showWarnings = FALSE); dir.create(fdir, showWarnings = FALSE)
    wcsv <- function(df, name) write.csv(df, file.path(tdir, paste0(name, ".csv")), row.names = FALSE)

    ## ---- static core ----
    cs <- static_connectedness(Y)
    cat(sprintf("Static TCI = %.4f\n", cs$TCI))
    print(round(cs$NET, 2))
    static_net_df <- data.frame(Segment = varnames, FROM = round(cs$FROM, 2),
                                TO = round(cs$TO, 2), NET = round(cs$NET, 2), TCI = round(cs$TCI, 3))
    wcsv(static_net_df, "Static_NET")
    wcsv(data.frame(Segment = varnames, round(cs$table, 3)), "Static_TABLE")
    wcsv(cs$NPDC, "Static_NPDC_unique_pairs")
    wcsv(data.frame(TCI = cs$TCI), "Static_TCI")

    ## optional cross-check vs ConnectednessApproach                      [F8]
    if (HAS_CA) {
      chk <- tryCatch({
        z <- zoo::zoo(Y, order.by = dates_ret)
        o <- ConnectednessApproach::ConnectednessApproach(
          z, nlag = N_LAG, nfore = H_GFEVD, model = "VAR", connectedness = "Time",
          Connectedness_config = list(TimeConnectedness = list(generalized = TRUE)))
        max(abs(as.numeric(o$TCI) - cs$TCI))
      }, error = function(e) NA)
      cat("Cross-check |TCI(own) - TCI(ConnectednessApproach)| =", chk, "\n")
      wcsv(data.frame(max_abs_TCI_diff = chk), "CrossCheck_ConnectednessApproach")
    }

    ## ---- diagnostics ----
    dg <- diagnostics(cs$model, Y_ret = Y)
    wcsv(dg$lag_selection, "LagSelection"); wcsv(dg$stability, "VAR_Stability")
    wcsv(dg$serial, "Serial_Correlation")
    if (!is.null(dg$normality)) wcsv(dg$normality, "Multivariate_Normality")
    wcsv(dg$per_equation, "Residual_Diagnostics_By_Eq")
    wcsv(dg$unit_roots, "UnitRoot_Screen")
    writeLines(c(dg$arch_note, dg$honest_summary), file.path(tdir, "Diagnostics_Notes.txt"))
    cat(dg$honest_summary, "\n")

    ## p=1 remains the baseline; p=2/3 are diagnostic sensitivity checks.
    lag_diag <- lag_diagnostic_sensitivity(Y)
    wcsv(lag_diag$summary, "VAR_Lag_Diagnostic_Sensitivity")
    wcsv(lag_diag$per_equation, "VAR_Lag_Residual_Diagnostics_By_Eq")

    ## ---- static bootstrap ----
    cat("Static bootstrap (", BOOT_REPS, "reps,", BOOT_TYPE,
        ", block length", BOOT_BLOCK_LEN, ")...\n")
    bt <- boot_static(cs$model, Y)
    writeLines(c(
      sprintf("Static bootstrap: %s, B=%d, seed=%d, block length=%d months.",
              BOOT_TYPE, BOOT_REPS, BOOT_SEED, BOOT_BLOCK_LEN),
      "One multiplier is shared by all five residuals in a month, preserving contemporaneous covariance.",
      "For wild_block, the multiplier is also shared within each time block to accommodate short residual dependence.",
      "NET and NPDC outputs contain pointwise intervals plus non-studentized max-deviation",
      "95% simultaneous bands. Role and network-edge identification use the simultaneous bands."),
      file.path(tdir, "Bootstrap_Method_Note.txt"))
    boot_net <- cbind(data.frame(Segment = varnames, Estimate = round(cs$NET, 2)), round(bt$NET[, -1], 3))
    boot_npdc <- data.frame(Pair = bt$NPDC$Pair,
                            Estimate = cs$NPDC$NPDC_ij[match(bt$NPDC$Pair, paste(cs$NPDC$Var_i, cs$NPDC$Var_j, sep = "__"))],
                            bt$NPDC[, -1])
    wcsv(boot_net, "Bootstrap_NET")
    wcsv(data.frame(Estimate = cs$TCI, t(bt$TCI), Reps_Used = bt$reps_used), "Bootstrap_TCI")
    wcsv(boot_npdc, "Bootstrap_NPDC")
    ## interpretation guard: is each market's transmitter/receiver role identified?
    role_df <- within(boot_net, {
      Role_identified <- Simultaneous_Lower95 > 0 | Simultaneous_Upper95 < 0
      Role <- ifelse(!Role_identified, "NOT identified (simultaneous band includes 0)",
                     ifelse(Estimate > 0, "Net transmitter", "Net receiver"))
    })
    wcsv(role_df, "NET_role_identification")
    cat("Role identification:\n")
    print(role_df[, c("Segment", "Estimate", "Simultaneous_Lower95",
                      "Simultaneous_Upper95", "Role")])

    ## ---- TVP-VAR (strict training/post-training split) + sensitivity ----
    cat("TVP-VAR strict-split filter + sensitivity (", length(TVP_TRAIN_SET),
        "training windows x", length(TVP_KAPPA_SET), "kappa combos )...\n")
    ts_res   <- tvp_sensitivity(Y)
    tvp_main <- ts_res$fits[[ts_res$base_name]]
    dates_all <- dates_ret[(N_LAG + 1):Tn]               # dates of Yt rows (t = 1..Te)
    jal_all   <- jal_ret[(N_LAG + 1):Tn]
    idx <- tvp_main$keep
    wcsv(data.frame(Date = dates_all[idx], Month_Jalali = jal_all[idx],
                    TCI = round(tvp_main$TCI[idx], 4),
                    round(tvp_main$NET[idx, ], 4)), "TVP_TCI_NET_path")
    ## sensitivity stats over the common window (after the largest training set)
    meta_end <- match("Max_used_root", names(ts_res$stats))
    wcsv(cbind(ts_res$stats[, seq_len(meta_end), drop = FALSE],
               round(ts_res$stats[, -(seq_len(meta_end)), drop = FALSE], 3)),
         "TVP_sensitivity_stats")
    wcsv(ts_res$roles, "TVP_sensitivity_role_stability")
    wcsv(ts_res$spec_status, "TVP_specification_status")
    stability_rows <- do.call(rbind, lapply(ts_res$specs, function(nm) {
      f <- ts_res$fits[[nm]]; kk <- f$keep
      data.frame(Spec = nm, Date = dates_all[kk], Month_Jalali = jal_all[kk],
                 f$stability[kk, c("Raw_root", "Used_root", "Action")])
    }))
    wcsv(stability_rows, "TVP_stability_path")
    ## all sensitivity TCI paths, wide format with explicit dates
    paths_df <- data.frame(Date = dates_all, Month_Jalali = jal_all)
    for (nm in ts_res$specs) paths_df[[nm]] <- round(ts_res$fits[[nm]]$TCI, 4)
    wcsv(paths_df, "TVP_sensitivity_TCI_paths")

    ## ---- which TVP patterns are descriptively stable across checked settings?
    cw <- ts_res$common
    peak_tbl <- do.call(rbind, lapply(ts_res$specs, function(nm) {
      x <- ts_res$fits[[nm]]$TCI[cw]
      i1 <- which.max(x); j1 <- which.min(x)
      x2 <- x; x2[max(1, i1 - 6):min(length(x), i1 + 6)] <- -Inf
      i2 <- if (any(is.finite(x2))) which.max(x2) else NA_integer_
      tr <- suppressWarnings(cor(x, seq_along(x)))
      data.frame(Spec = nm,
                 TCI_mean = round(mean(x), 2),
                 TCI_min = round(min(x), 2),
                 TCI_max = round(max(x), 2),
                 Peak1_month = jal_all[cw][i1], Peak1_TCI = round(x[i1], 2),
                 Peak2_month = if (is.na(i2)) NA else jal_all[cw][i2],
                 Peak2_TCI = if (is.na(i2)) NA else round(x[i2], 2),
                 Trough_month = jal_all[cw][j1], Trough_TCI = round(x[j1], 2),
                 Trend_corr_with_time = round(tr, 3),
                 Peak1_idx = i1, Trough_idx = j1)
    }))
    wcsv(peak_tbl[, setdiff(names(peak_tbl), c("Peak1_idx", "Trough_idx"))],
         "TVP_TCI_levels_and_peaks_by_spec")
    ## Descriptive tuning-stability classification with explicit thresholds.
    ## This is not inferential evidence and is never labelled "robust".
    st <- ts_res$stats
    role_class <- do.call(rbind, lapply(varnames, function(v) {
      nm <- st[[paste0("NETmean_", v)]]; pp <- st[[paste0("PctPos_", v)]]
      verdict <- if (all(nm > 0) && all(pp >= 60))
        "TUNING-STABLE net transmitter (descriptive)"
        else if (all(nm < 0) && all(pp <= 40))
          "TUNING-STABLE net receiver (descriptive)"
        else "SENSITIVE across settings - no stable descriptive role"
      data.frame(Segment = v,
                 NETmean_min = round(min(nm), 2), NETmean_max = round(max(nm), 2),
                 PctPos_min = round(min(pp), 1), PctPos_max = round(max(pp), 1),
                 Verdict = verdict)
    }))
    wcsv(role_class, "TVP_role_tuning_stability")
    ## Stability of peak/trough timing across the checked tuning choices.
    ## all specs fall within a 3-month window on the common grid.
    peak_spread   <- diff(range(peak_tbl$Peak1_idx))
    trough_spread <- diff(range(peak_tbl$Trough_idx))
    trend_signs   <- sign(peak_tbl$Trend_corr_with_time)
    trend_signs   <- trend_signs[!is.na(trend_signs)]
    guide <- c(
      "TVP INTERPRETATION GUIDE (auto-generated; Q1.1 correction)",
      paste("Included training windows:",
            paste(sort(unique(vapply(ts_res$fits, `[[`, numeric(1), "train_n"))),
                  collapse = "/"), "months; (kappa1,kappa2) in"),
      paste(" ", paste(vapply(TVP_KAPPA_SET, function(k) sprintf("(%.2f,%.2f)", k[1], k[2]), ""),
                       collapse = ", "),
            sprintf("- compared on one complete common window of %d months.", length(cw))),
      sprintf("Common window: %s to %s; no pairwise deletion and no na.rm summaries.",
              as.character(dates_all[min(cw)]), as.character(dates_all[max(cw)])),
      "Unstable local updates are flagged and handled by carrying forward the last",
      "stable coefficient state; see TVP_stability_path.csv.",
      "",
      sprintf("1) TCI LEVEL: mean TCI ranges from %.1f to %.1f across specifications.",
              min(peak_tbl$TCI_mean), max(peak_tbl$TCI_mean)),
      "   The LEVEL is specification-dependent: report it as a range, never as a",
      "   single point estimate.",
      if (length(unique(trend_signs[trend_signs != 0])) == 1)
        sprintf("   The TIME TREND sign is TUNING-STABLE: correlation with time is %s in all specs (%.2f to %.2f).",
                ifelse(trend_signs[1] > 0, "positive", "negative"),
                min(peak_tbl$Trend_corr_with_time), max(peak_tbl$Trend_corr_with_time))
      else "   The TIME TREND of TCI is NOT consistent across specs - do not interpret.",
      "",
      if (peak_spread <= 3)
        sprintf("2) PEAK TIMING is TUNING-STABLE - the maximum-TCI month lies within a %d-month window across all specs (modal month %s).",
                peak_spread + 1, names(sort(table(peak_tbl$Peak1_month), decreasing = TRUE))[1])
      else
        sprintf("2) PEAK TIMING is SENSITIVE - argmax months span %d months across specs; interpret episodes, not a dated peak.",
                peak_spread + 1),
      if (trough_spread <= 3)
        sprintf("   TROUGH TIMING is TUNING-STABLE (modal month %s).",
                names(sort(table(peak_tbl$Trough_month), decreasing = TRUE))[1])
      else "   TROUGH TIMING is SENSITIVE across specs.",
      "",
      "3) MARKET ROLES (thresholds: sign agreement in ALL specs AND NET>0 share",
      "   >=60% for transmitters / <=40% for receivers in ALL specs):",
      paste0("   ", role_class$Segment, ": ", role_class$Verdict,
             sprintf("  [NET mean %.1f..%.1f, %%months NET>0: %.0f..%.0f]",
                     role_class$NETmean_min, role_class$NETmean_max,
                     role_class$PctPos_min, role_class$PctPos_max)),
      "",
      "TUNING-STABLE means agreement across checked settings only; it is not a",
      "confidence statement. Results labelled SENSITIVE should not carry a role claim.")
    writeLines(guide, file.path(tdir, "TVP_Interpretation_Guide.txt"))
    cat(paste(guide, collapse = "\n"), "\n")
    cat("TVP sensitivity (common window):\n")
    print(ts_res$stats[, c("Spec", "TCI_mean", "TCI_sd", "Corr_with_baseline")], digits = 4)
    writeLines(paste(
      "TVP methodology [F1, updated]: prior = OLS on the first train_n months ONLY;",
      "the Kalman filter starts at t = train_n + 1, so training observations enter",
      "through the prior once and are NEVER re-used in the update step (strict",
      "training / post-training split). The first reported point is the first",
      "post-training month. Sensitivity: training windows", paste(TVP_TRAIN_SET, collapse = "/"),
      "months and (kappa1,kappa2) combinations",
      paste(vapply(TVP_KAPPA_SET, function(k) sprintf("(%.2f,%.2f)", k[1], k[2]), ""),
            collapse = ", "),
      sprintf("- all compared over the complete common window %s to %s (%d months).",
              as.character(dates_all[min(cw)]), as.character(dates_all[max(cw)]), length(cw)),
      "Any unstable local coefficient update is reported in TVP_stability_path.csv;",
      "the last stable coefficient state is carried forward for that date.",
      "No 'TVP confidence bands' are reported: bands simulated from a constant-parameter",
      "VAR do not measure path uncertainty of a time-varying DGP [F3]."),
      file.path(tdir, "TVP_Methodology_Note.txt"))

    if (RUN_TVP_DGP_SENSITIVITY) {
      cat("Optional fixed-DGP TVP sensitivity (NOT confidence bands)...\n")
      ## clearly-labelled sensitivity under a constant-parameter DGP
      set.seed(BOOT_SEED + 7)
      tcis <- replicate(TVP_DGP_REPS, {
        E <- stats::residuals(cs$model)
        Eb <- E * sample(c(-1, 1), nrow(E), replace = TRUE)
        Yb <- Y; for (t in (N_LAG + 1):nrow(Y))
          Yb[t, ] <- vars::Bcoef(cs$model)[, "const"] + vars::Acoef(cs$model)[[1]] %*% Yb[t - 1, ] + Eb[t - N_LAG, ]
        z <- tryCatch(tvp_var_filter(Yb, train_n = TRAIN_N, kappa1 = TVP_KAPPA1, kappa2 = TVP_KAPPA2),
                      error = function(e) NULL)
        if (is.null(z)) rep(NA_real_, length(idx)) else z$TCI[z$keep]
      })
      wcsv(data.frame(Date = dates_all[idx],
                      Sens_Lo = apply(tcis, 1, quantile, .025, na.rm = TRUE),
                      Sens_Hi = apply(tcis, 1, quantile, .975, na.rm = TRUE)),
           "TVP_TCI_fixedDGP_sensitivity_NOT_CI")
    }

    ## ---- event robustness (no splicing) ----
    cat("Event robustness (dummies + contiguous subsamples)...\n")
    ev <- event_robustness(Y, dates_ret)
    ev_rows <- data.frame(Spec = "Full sample + 2018/2020 event dummies (exogenous)",
                          t(round(ev$dummies$NET, 2)), TCI = round(ev$dummies$TCI, 3))
    for (sn in names(ev$subsamples))
      ev_rows <- rbind(ev_rows, data.frame(Spec = paste("Contiguous subsample:", sn),
                                           t(round(ev$subsamples[[sn]]$NET, 2)),
                                           TCI = round(ev$subsamples[[sn]]$TCI, 3)))
    ev_rows <- rbind(data.frame(Spec = "Baseline full sample", t(round(cs$NET, 2)),
                                TCI = round(cs$TCI, 3)), ev_rows)
    wcsv(ev_rows, "Robust_Events_no_splicing")
    writeLines(ev$note, file.path(tdir, "Robust_Events_Note.txt"))
    print(ev_rows)

    ## ---- lag/horizon robustness ----
    gr <- expand.grid(p = 1:3, H = c(6, 10, 12, 18))
    lh <- do.call(rbind, lapply(seq_len(nrow(gr)), function(i) {
      z <- static_connectedness(Y, p = gr$p[i], H = gr$H[i])
      data.frame(Lag = gr$p[i], Horizon = gr$H[i], TCI = round(z$TCI, 3), t(round(z$NET, 2)))
    }))
    wcsv(lh, "Robust_LagHorizon")

    ## ---- frequency decomposition ----
    fq <- NULL
    if (RUN_FREQUENCY) {
      fq <- tryCatch(freq_connectedness(cs$model), error = function(e) NULL)
      if (!is.null(fq)) {
        fq_rows <- do.call(rbind, lapply(names(fq), function(bn) data.frame(
          Band = bn, TCI = round(fq[[bn]]$TCI, 3), t(round(fq[[bn]]$NET, 2)))))
        wcsv(fq_rows, "Frequency_Band_NET_TCI")
        wcsv(data.frame(Band = names(FREQ_BANDS),
                        Period_months = vapply(FREQ_BANDS, function(b) paste(b, collapse = "-"), "")),
             "Frequency_Band_Definitions")
        ## self-test: band tables sum to the (long-horizon) static table.
        ## Benchmark is H=100 (spectral decomposition corresponds to H -> Inf).
        sum_tab <- Reduce(`+`, lapply(fq, `[[`, "table"))
        cs_long <- static_connectedness(Y, H = 100)
        wcsv(data.frame(max_abs_diff_bands_vs_staticH100 = max(abs(sum_tab - cs_long$table))),
             "Frequency_SelfTest")
      }
    }

    ## ---- GIRF ----
    G <- girf(cs$model, GIRF_H)
    gl <- data.frame()
    for (i in 1:K) for (j in 1:K)
      gl <- rbind(gl, data.frame(h = 0:GIRF_H, Response = varnames[i], Shock = varnames[j],
                                 IRF = round(G[, i, j], 4),
                                 Lower95 = round(bt$GIRF_lo[, i, j], 4),
                                 Upper95 = round(bt$GIRF_hi[, i, j], 4)))
    wcsv(gl, "GIRF_point_and_bands")
    writeLines(paste(
      "GIRFs are generalized (order-invariant) impulse responses; they are NOT structural.",
      "Significant h=0 responses reflect contemporaneous innovation correlation.",
      "Horizon-specific magnitudes and bands must be read from GIRF_point_and_bands.csv.",
      "The GIRFs do not establish causal, multi-year, or regularly-lagged transmission."),
      file.path(tdir, "GIRF_Note.txt"))

    ## ---- figures (each wrapped so one failure never blocks the rest) ----
    sf <- function(expr) tryCatch(expr, error = function(e)
      cat("FIGURE FAILED:", conditionMessage(e), "\n"))
    sf(fig_returns(Y, dates_ret, file.path(fdir, "Fig00_returns.png"), LAYERS[[ln]]$label))
    sf(fig_heatmap(cs$table, file.path(fdir, "Fig01_static_heatmap.png"),
                paste0("Static GFEVD connectedness (%), TCI = ", round(cs$TCI, 2))))
    sf(fig_net_ci(cs$NET, bt$NET, file.path(fdir, "Fig02_NET_bootstrapCI.png"),
               "NET connectedness with 95% simultaneous bootstrap bands"))
    sf(fig_network(cs, bt$NPDC, file.path(fdir, "Fig03_network_datadriven.png"),
                "Static NPDC network (all pairs; significant edges highlighted)"))
    kap_specs <- grep(sprintf("^train%02d_", TRAIN_N), ts_res$specs, value = TRUE)
    sf(fig_tvp_tci(setNames(lapply(kap_specs, function(nm) ts_res$fits[[nm]]$TCI), kap_specs),
                dates_all, file.path(fdir, "Fig04_TVP_TCI_kappa.png"),
                "TVP total connectedness: (kappa1, kappa2) sensitivity",
                sprintf("Strict split: prior on first %d months, filter starts month %d.",
                        TRAIN_N, TRAIN_N + 1)))
    trn_specs <- grep(sprintf("k%.2f_%.2f$", TVP_KAPPA1, TVP_KAPPA2), ts_res$specs, value = TRUE)
    trn_paths <- setNames(lapply(trn_specs, function(nm) {
      z <- rep(NA_real_, length(ts_res$fits[[nm]]$TCI))
      z[cw] <- ts_res$fits[[nm]]$TCI[cw]
      z
    }), trn_specs)
    sf(fig_tvp_tci(trn_paths,
                dates_all, file.path(fdir, "Fig04b_TVP_TCI_training.png"),
                "TVP total connectedness: training-window sensitivity",
                sprintf("Complete common window: %s to %s (%d months).",
                        as.character(dates_all[min(cw)]), as.character(dates_all[max(cw)]),
                        length(cw))))
    sf(fig_tvp_net(tvp_main, ts_res, dates_all, file.path(fdir, "Fig05_TVP_NET.png"),
                "TVP NET: baseline path with tuning range (not a confidence band)"))
    sf(fig_girf(G, bt$GIRF_lo, bt$GIRF_hi, file.path(fdir, "Fig06_GIRF.png"),
             "Generalized impulse responses with 95% bootstrap bands"))
    if (!is.null(fq))
      sf(fig_freq_net(fq, file.path(fdir, "Fig07_frequency_NET.png"),
                   "NET connectedness by frequency band"))

    ## ---- clean workbook [F7] ----
    ## writexl produces a minimal values-only xlsx: no drawings, no printer
    ## settings, no dangling relationships (openxlsx on some versions emits
    ## sheet rels pointing to non-existent drawingN.xml -- the exact defect
    ## flagged in the referee report, so openxlsx is deliberately avoided).
    sheet_list <- list(
      README = data.frame(Note = c(
        LAYERS[[ln]]$label,
        "Values-only workbook written with writexl: no drawings, no broken relationships.",
        "See tables_csv/ for the same content in CSV and Diagnostics_Notes.txt for caveats.")),
      Static_TABLE = data.frame(Segment = varnames, round(cs$table, 3)),
      Static_NET = static_net_df,
      Static_NPDC = cs$NPDC,
      Bootstrap_NET = boot_net,
      Bootstrap_NPDC = boot_npdc,
      NET_roles = role_df,
      Diagnostics_ByEq = dg$per_equation,
      UnitRoots = dg$unit_roots,
      VAR_lag_diagnostics = lag_diag$summary,
      VAR_lag_by_equation = lag_diag$per_equation,
      Robust_Events = ev_rows,
      Robust_LagHorizon = lh,
      TVP_path = data.frame(Date = as.character(dates_all[idx]),
                            Month_Jalali = jal_all[idx],
                            TCI = round(tvp_main$TCI[idx], 4),
                            round(tvp_main$NET[idx, ], 4)),
      TVP_sensitivity = cbind(
        ts_res$stats[, seq_len(meta_end), drop = FALSE],
        round(ts_res$stats[, -(seq_len(meta_end)), drop = FALSE], 3)),
      TVP_role_stability = ts_res$roles,
      TVP_role_tuning = role_class,
      TVP_spec_status = ts_res$spec_status,
      TVP_stability = stability_rows,
      TVP_peaks_by_spec = peak_tbl[, setdiff(names(peak_tbl), c("Peak1_idx", "Trough_idx"))],
      GIRF = gl
    )
    wb_ok <- tryCatch({
      writexl::write_xlsx(sheet_list, file.path(odir, paste0(ln, "_results_Q1.xlsx")))
      TRUE
    }, error = function(e) { cat("Workbook write failed:", conditionMessage(e),
                                 "- CSV tables remain authoritative.\n"); FALSE })

    all_results[[ln]] <- list(static = cs, boot = bt, tvp = tvp_main,
                              events = ev, diag = dg, lag_diagnostics = lag_diag)
  }

  ## ---- Layer-3 summary ----
  summary_row <- do.call(rbind, lapply(names(all_results), function(ln) data.frame(
    Layer = ln,
    Measurement = "Nominal end-of-month for USD/GOLD/STOCK; monthly VEHICLE/HOUSING",
    TCI = round(all_results[[ln]]$static$TCI, 3),
    t(round(all_results[[ln]]$static$NET, 2)),
    row.names = NULL
  )))
  write.csv(summary_row, file.path(OUT_ROOT, "Layer3_Summary_TCI_NET.csv"),
            row.names = FALSE)
  cat("\nLayer-3 summary:\n")
  print(summary_row)

  ## ---- Direct Layer 3 EOM minus Layer 1 monthly-average comparison ----
  cat("\nPaired moving-block bootstrap: Layer 3 EOM minus Layer 1 average...\n")
  cs_average <- static_connectedness(ret_average)
  cs_eom <- all_results$layer3_eom$static
  sampling_static <- rbind(
    data.frame(Measurement = "Layer 1 monthly average", TCI = cs_average$TCI,
               t(cs_average$NET)),
    data.frame(Measurement = "Layer 3 end of month", TCI = cs_eom$TCI,
               t(cs_eom$NET)))
  write.csv(sampling_static,
            file.path(OUT_ROOT, "Layer3_vs_Layer1_Static_Comparison.csv"),
            row.names = FALSE)
  sampling_diff <- boot_sampling_difference(ret_average, ret_eom,
                                             cs_average, cs_eom)
  sampling_diff$TCI$Difference_identified <- with(
    sampling_diff$TCI, Pointwise_Lower95 > 0 | Pointwise_Upper95 < 0)
  sampling_diff$NET$Difference_identified <- with(
    sampling_diff$NET,
    Simultaneous_Lower95 > 0 | Simultaneous_Upper95 < 0)
  sampling_tbl <- rbind(sampling_diff$TCI[, names(sampling_diff$NET)],
                        sampling_diff$NET)
  write.csv(sampling_tbl,
            file.path(OUT_ROOT, "Layer3_vs_Layer1_PairedBootstrap.csv"),
            row.names = FALSE)
  writexl::write_xlsx(list(
    Static_comparison = sampling_static,
    Paired_difference = sampling_tbl,
    Interpretation = data.frame(Note = c(
      "Difference is defined as Layer 3 end-of-month minus Layer 1 monthly-average.",
      "The same resampled month blocks are applied to both measurement conventions.",
      "TCI uses a pointwise paired interval; the five NET differences use simultaneous bands.",
      "This is measurement sensitivity, not a causal effect of the sampling convention."))),
    file.path(OUT_ROOT, "Layer3_vs_Layer1_Comparison_Q1.xlsx"))

  ## ---- reproducibility ----
  writeLines(capture.output(sessionInfo()), file.path(OUT_ROOT, "sessionInfo.txt"))
  writeLines(c(
    paste0(
      "Reproduce through the unified script using data(3).xlsx. ",
      "Layer 3 reads the raw `eom` sheet and the Layer-1 reference from the raw `series` sheet."
    ),
    paste("Seed:", BOOT_SEED,
          "| Bootstrap:", BOOT_REPS, BOOT_TYPE,
          "| block:", BOOT_BLOCK_LEN,
          "| VAR lag:", N_LAG,
          "| GFEVD horizon:", H_GFEVD,
          "| TVP train:", TRAIN_N,
          "| kappa1:", TVP_KAPPA1,
          "| kappa2:", TVP_KAPPA2),
    paste("Paired seed:", PAIRED_BOOT_SEED, "| Reps:", PAIRED_BOOT_REPS,
          "| block:", PAIRED_BOOT_BLOCK_LEN),
    "Layer 3 uses nominal returns only; CPI deflation is deliberately not repeated.",
    "The three traded markets use last available monthly observations; vehicle and housing remain monthly.",
    "Use Layer3_vs_Layer1_PairedBootstrap.csv for claims about measurement differences."
  ), file.path(OUT_ROOT, "README_reproducibility.txt"))

  cat("\nDone:", format(Sys.time()),
      "\nOutputs in:", OUT_ROOT, "\n")
  sink()

})


while (sink.number() > 0) sink()
cat("\nLayer 3 completed. Constructing Layer 4 input internally...\n\n")

## ---------------- MASTER: strict-split TVP paths -> Layer 4 --------------- ##
l1_tvp_file <- file.path(
  MASTER_OUT_DIR, "layers_1_2", "layer1_nominal", "tables_csv", "TVP_TCI_NET_path.csv"
)
l2_tvp_file <- file.path(
  MASTER_OUT_DIR, "layers_1_2", "layer2_real", "tables_csv", "TVP_TCI_NET_path.csv"
)
l3_tvp_file <- file.path(
  MASTER_OUT_DIR, "layer_3_eom", "layer3_eom", "tables_csv", "TVP_TCI_NET_path.csv"
)
needed_tvp_files <- c(l1_tvp_file, l2_tvp_file, l3_tvp_file)
if (!all(file.exists(needed_tvp_files))) {
  stop(
    "One or more upstream TVP path files were not created:\n",
    paste(needed_tvp_files[!file.exists(needed_tvp_files)], collapse = "\n"),
    call. = FALSE
  )
}

l1_tvp <- utils::read.csv(l1_tvp_file, stringsAsFactors = FALSE, check.names = FALSE)
l2_tvp <- utils::read.csv(l2_tvp_file, stringsAsFactors = FALSE, check.names = FALSE)
l3_tvp <- utils::read.csv(l3_tvp_file, stringsAsFactors = FALSE, check.names = FALSE)
tvp_required <- c("Date", "Month_Jalali", "TCI", MASTER_VARS)
if (!all(tvp_required %in% names(l1_tvp)) ||
    !all(tvp_required %in% names(l2_tvp)) ||
    !all(tvp_required %in% names(l3_tvp))) {
  stop("Unexpected TVP path schema in an upstream layer.", call. = FALSE)
}

l1_key <- normalize_month(l1_tvp$Month_Jalali)
l2_key <- normalize_month(l2_tvp$Month_Jalali)
l3_key <- normalize_month(l3_tvp$Month_Jalali)
if (!identical(l1_key, l2_key) || !identical(l1_key, l3_key) ||
    nrow(l1_tvp) != 84L) {
  stop("The three strict-split TVP paths are not exactly aligned over 84 months.", call. = FALSE)
}
if (l1_key[1L] != "1398/03" || tail(l1_key, 1L) != "1405/02") {
  stop("Unexpected strict-split TVP calendar range.", call. = FALSE)
}

ctx_match <- match(l1_key, context_month)
if (anyNA(ctx_match)) {
  stop("Layer 4 could not align the TVP paths to the raw contextual sheet.", call. = FALSE)
}

## GSDB Release 4: Iran as target, including multi-target cases.
gsdb_target_text <- trimws(as.character(MASTER_GSDB$sanctioned_state))
iran_target <- grepl(
  "(^|[^[:alpha:]])Iran([^[:alpha:]]|$)",
  gsdb_target_text, ignore.case = TRUE, perl = TRUE
)
gsdb_iran <- MASTER_GSDB[iran_target, , drop = FALSE]
gsdb_iran$case_id <- as.character(gsdb_iran$case_id)
gsdb_iran$begin <- as.integer(gsdb_iran$begin)
gsdb_iran$end <- as.integer(gsdb_iran$end)
gsdb_iran$financial <- as.numeric(gsdb_iran$financial)
gsdb_iran$trade <- as.numeric(gsdb_iran$trade)

gsdb_years <- 2019:2023
MASTER_GSDB_ANNUAL <- do.call(rbind, lapply(gsdb_years, function(y) {
  active <- gsdb_iran[
    is.finite(gsdb_iran$begin) & is.finite(gsdb_iran$end) &
      gsdb_iran$begin <= y & gsdb_iran$end >= y,
    , drop = FALSE
  ]
  data.frame(
    greg_year = y,
    gsdb_active_total = length(unique(active$case_id)),
    gsdb_financial = length(unique(active$case_id[!is.na(active$financial) & active$financial == 1])),
    gsdb_trade = length(unique(active$case_id[!is.na(active$trade) & active$trade == 1])),
    stringsAsFactors = FALSE
  )
}))

expected_gsdb <- data.frame(
  greg_year = 2019:2023,
  gsdb_active_total = c(12L, 13L, 14L, 21L, 28L),
  gsdb_financial = c(6L, 7L, 9L, 16L, 22L),
  gsdb_trade = c(5L, 6L, 8L, 10L, 14L)
)
if (!isTRUE(all.equal(
  MASTER_GSDB_ANNUAL, expected_gsdb,
  check.attributes = FALSE, tolerance = 0
))) {
  print(MASTER_GSDB_ANNUAL)
  stop(
    "GSDB Iran-target extraction did not reproduce the validated 2019-2023 ",
    "annual breadth counts.", call. = FALSE
  )
}

date_gregorian <- as.Date(l1_tvp$Date)
greg_year <- as.integer(format(date_gregorian, "%Y"))
gmatch <- match(greg_year, MASTER_GSDB_ANNUAL$greg_year)

MASTER_L4_PANEL <- data.frame(
  month_jalali = l1_key,
  date_gregorian = date_gregorian,
  greg_year = greg_year,

  tci_nominal = as.numeric(l1_tvp$TCI),
  net_nominal_usd = as.numeric(l1_tvp$USD),
  net_nominal_gold = as.numeric(l1_tvp$GOLD),
  net_nominal_stock = as.numeric(l1_tvp$STOCK),
  net_nominal_vehicle = as.numeric(l1_tvp$VEHICLE),
  net_nominal_housing = as.numeric(l1_tvp$HOUSING),

  tci_real = as.numeric(l2_tvp$TCI),
  net_real_usd = as.numeric(l2_tvp$USD),
  net_real_gold = as.numeric(l2_tvp$GOLD),
  net_real_stock = as.numeric(l2_tvp$STOCK),
  net_real_vehicle = as.numeric(l2_tvp$VEHICLE),
  net_real_housing = as.numeric(l2_tvp$HOUSING),

  tci_eom = as.numeric(l3_tvp$TCI),
  net_eom_usd = as.numeric(l3_tvp$USD),
  net_eom_gold = as.numeric(l3_tvp$GOLD),
  net_eom_stock = as.numeric(l3_tvp$STOCK),
  net_eom_vehicle = as.numeric(l3_tvp$VEHICLE),
  net_eom_housing = as.numeric(l3_tvp$HOUSING),

  inflation_monthly_pct = MASTER_INFLATION_FULL[ctx_match],
  interbank_rate_pct = interbank[ctx_match],
  oil_exports_mbd = as.numeric(MASTER_CONTEXT$oil_exports_mbd)[ctx_match],
  days_in_jalali_month = as.numeric(MASTER_CONTEXT$days_in_jalali_month)[ctx_match],
  iran_heavy_usd_per_bbl = as.numeric(MASTER_CONTEXT$iran_heavy_usd_per_bbl)[ctx_match],

  gsdb_active_total = ifelse(greg_year <= 2023,
                             MASTER_GSDB_ANNUAL$gsdb_active_total[gmatch], NA_real_),
  gsdb_financial = ifelse(greg_year <= 2023,
                          MASTER_GSDB_ANNUAL$gsdb_financial[gmatch], NA_real_),
  gsdb_trade = ifelse(greg_year <= 2023,
                      MASTER_GSDB_ANNUAL$gsdb_trade[gmatch], NA_real_),
  gsdb_observed = as.numeric(greg_year <= 2023),
  stringsAsFactors = FALSE
)

## Raw-source registry used only for reproducibility and audit text.
MASTER_SOURCE_REGISTRY <- data.frame(
  component = c(
    "Five market series and CPI",
    "Interbank rate",
    "Oil export volume, earlier source",
    "Oil export volume, later source",
    "Iran Heavy monthly benchmark",
    "Sanctions case data"
  ),
  source = c(
    "TGJU / TSETMC / SCI / CBI / MRUD as documented in the manuscript",
    "Central Bank of Iran: https://www.cbi.ir/policyrates/",
    "U.S. EIA: https://www.eia.gov/",
    "UANI Tanker Tracker: https://www.unitedagainstnucleariran.com/",
    "OPEC Monthly Oil Market Report: https://www.opec.org/",
    "Global Sanctions Data Base Release 4: https://www.globalsanctionsdatabase.com/"
  ),
  note = c(
    "Raw level series are stored in the `series` and `eom` sheets.",
    "Raw monthly rate is stored in `contextual`; simple change is constructed in R.",
    "Crude plus condensate export volume is already aligned to the study month.",
    "Crude plus condensate export volume is already aligned to the study month.",
    "Raw monthly Iran Heavy benchmark used in the oil-export-revenue proxy.",
    "Raw GSDB cases are filtered for Iran as sanctioned state, including multi-target cases."
  ),
  stringsAsFactors = FALSE
)

utils::write.csv(
  MASTER_GSDB_ANNUAL,
  file.path(MASTER_DERIVED_DIR, "gsdb_annual_counts_2019_2023.csv"),
  row.names = FALSE
)
utils::write.csv(
  MASTER_L4_PANEL,
  file.path(MASTER_DERIVED_DIR, "layer4_panel_built_from_raw_and_tvp.csv"),
  row.names = FALSE, na = ""
)

writexl::write_xlsx(
  list(
    Raw_input_audit = MASTER_INPUT_AUDIT,
    Market_returns = derived_returns,
    Contextual_transforms = derived_context,
    GSDB_annual_counts = MASTER_GSDB_ANNUAL,
    Layer4_panel = MASTER_L4_PANEL
  ),
  file.path(MASTER_DERIVED_DIR, "Derived_Raw_Transformations.xlsx")
)

cat("Layer 4 panel constructed. Running final contextual analysis with Iran Heavy...\n\n")

## ========================================================================
## INTERNAL FINAL ENGINE: LAYER 4 (CURRENT RAW CONTEXT + IRAN HEAVY)
## ========================================================================

local({
  ## ============================================================================
  ## LAYER 4 — FINAL POST-ESTIMATION CONTEXTUAL-DRIVER ANALYSIS
  ## ============================================================================
  ## This script uses the final strict-split TVP connectedness paths from
  ## Layers 1–3. It does not re-estimate the network and makes no causal claim.
  ##
  ## Headline architecture:
  ##   Delta Y_t = alpha + phi Delta Y_(t-1)
  ##               + beta1 Inflation_(t-1)
  ##               + beta2 Delta Interbank_(t-1)
  ##               + beta3 Oil-revenue-proxy growth_(t-1) + error_t
  ##
  ## Headline outcomes:
  ##   (1) change in nominal TCI;
  ##   (2) change in the average USD/gold NET position;
  ##   (3) change in the receiver intensity of vehicle/housing.
  ##
  ## Inference:
  ##   Newey–West HAC (lag 4), BH-FDR across the nine headline driver tests,
  ##   and a recursive wild-block bootstrap with simultaneous intervals within
  ##   each equation. GSDB R4 is used descriptively only and never carried beyond
  ##   its 2023 coverage endpoint.
  ##
  ## Retention-gate correction:
  ##   Exclusion-sample coefficients are converted to the baseline predictor
  ##   scale before the 2020-exclusion retention ratio is evaluated (Eq. 27).
  ## ============================================================================

  options(stringsAsFactors = FALSE, scipen = 999)

  required_packages <- c(
    "readxl", "dplyr", "tidyr", "sandwich", "lmtest", "writexl",
    "ggplot2", "strucchange", "tseries"
  )
  missing_packages <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing_packages) > 0L) {
    stop(
      "Missing required package(s): ", paste(missing_packages, collapse = ", "),
      "\nInstall once with:\ninstall.packages(c(",
      paste(sprintf('"%s"', missing_packages), collapse = ", "), "))",
      call. = FALSE
    )
  }

  suppressPackageStartupMessages({
    library(dplyr)
    library(tidyr)
    library(ggplot2)
  })

  ## ---------------------------- Configuration ---------------------------------
  SEED <- 20260713L
  HAC_MAIN_LAG <- 4L
  BOOT_BLOCK <- 6L
  BOOT_B <- suppressWarnings(as.integer(Sys.getenv("LAYER4_BOOT_B", "1999")))
  if (!is.finite(BOOT_B) || BOOT_B < 499L) {
    stop("LAYER4_BOOT_B must be an integer >= 499; 1999 is recommended.", call. = FALSE)
  }

  timestamp <- function() format(Sys.time(), "%Y%m%d_%H%M%S")
  as_num <- function(x) suppressWarnings(as.numeric(as.character(x)))
  safe_sd <- function(x) {
    z <- stats::sd(x, na.rm = TRUE)
    if (!is.finite(z) || z <= sqrt(.Machine$double.eps)) {
      stop("A model regressor has zero or undefined sample variance.", call. = FALSE)
    }
    z
  }
  zscore <- function(x) as.numeric((x - mean(x, na.rm = TRUE)) / safe_sd(x))
  lag_name <- function(x, k) paste0(x, "_l", k)
  safe_p <- function(expr) tryCatch(as.numeric(expr), error = function(e) NA_real_)
  safe_write_csv <- function(x, path) utils::write.csv(x, path, row.names = FALSE, na = "")
  detect_executed_script <- function() {
    frames <- sys.frames()
    candidates <- unlist(lapply(frames, function(f) {
      if (!is.null(f$ofile)) as.character(f$ofile) else character(0)
    }), use.names = FALSE)
    candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
    if (length(candidates)) return(normalizePath(tail(candidates, 1L), mustWork = TRUE))
    file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
    if (length(file_arg)) {
      candidate <- sub("^--file=", "", tail(file_arg, 1L))
      if (file.exists(candidate)) return(normalizePath(candidate, mustWork = TRUE))
    }
    NA_character_
  }
  EXECUTED_SCRIPT_PATH <- detect_executed_script()

  ## -------------------------- Unified raw-input handoff -------------------------
  input_path <- MASTER_DATA_FILE
  out_dir <- file.path(MASTER_OUT_DIR, "layer_4_contextual")
  tables_dir <- file.path(out_dir, "tables_csv")
  figures_dir <- file.path(out_dir, "figures")
  repro_dir <- file.path(out_dir, "reproducibility")
  dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(repro_dir, recursive = TRUE, showWarnings = FALSE)

  ## ------------------------------ Read data ------------------------------------
  raw <- MASTER_L4_PANEL
  source_registry <- MASTER_SOURCE_REGISTRY
  required_columns <- c(
    "month_jalali", "date_gregorian", "greg_year",
    "tci_nominal", "net_nominal_usd", "net_nominal_gold",
    "net_nominal_stock", "net_nominal_vehicle", "net_nominal_housing",
    "tci_real", "net_real_usd", "net_real_gold", "net_real_stock",
    "net_real_vehicle", "net_real_housing",
    "tci_eom", "net_eom_usd", "net_eom_gold", "net_eom_stock",
    "net_eom_vehicle", "net_eom_housing",
    "inflation_monthly_pct", "interbank_rate_pct", "oil_exports_mbd",
    "days_in_jalali_month", "iran_heavy_usd_per_bbl",
    "gsdb_active_total", "gsdb_financial",
    "gsdb_trade", "gsdb_observed"
  )
  missing_columns <- setdiff(required_columns, names(raw))
  if (length(missing_columns) > 0L) {
    stop("Missing required column(s): ", paste(missing_columns, collapse = ", "), call. = FALSE)
  }

  panel <- raw[, required_columns]
  panel$month_jalali <- trimws(as.character(panel$month_jalali))
  panel$date_gregorian <- as.Date(as.character(panel$date_gregorian))
  numeric_columns <- setdiff(required_columns, c("month_jalali", "date_gregorian"))
  for (nm in numeric_columns) panel[[nm]] <- as_num(panel[[nm]])
  panel <- panel %>% arrange(date_gregorian)

  ## --------------------------- Input audit gates -------------------------------
  net_sum <- function(prefix) {
    rowSums(panel[, paste0("net_", prefix, "_", c("usd", "gold", "stock", "vehicle", "housing"))])
  }
  check_rows <- list()
  add_check <- function(check, observed, expected, pass, note) {
    check_rows[[length(check_rows) + 1L]] <<- data.frame(
      check = check, observed = as.character(observed), expected = as.character(expected),
      status = if (isTRUE(pass)) "PASS" else "FAIL", note = note,
      stringsAsFactors = FALSE
    )
  }
  add_check("row_count", nrow(panel), 84L, nrow(panel) == 84L,
            "Final strict-split TVP path length")
  add_check("unique_month_key", length(unique(panel$month_jalali)), nrow(panel),
            !anyDuplicated(panel$month_jalali), "month_jalali must be unique")
  add_check("strict_date_order", all(diff(panel$date_gregorian) > 0), TRUE,
            all(diff(panel$date_gregorian) > 0), "Gregorian dates strictly increase")
  add_check("monthly_date_spacing", paste(range(as.numeric(diff(panel$date_gregorian))), collapse = " to "),
            "28 to 32", all(as.numeric(diff(panel$date_gregorian)) >= 28 &
                              as.numeric(diff(panel$date_gregorian)) <= 32),
            "No missing or duplicated calendar month")
  for (layer in c("nominal", "real", "eom")) {
    err <- max(abs(net_sum(layer)), na.rm = TRUE)
    add_check(paste0("net_sum_", layer), signif(err, 6), "<= 0.0011", err <= 0.0011,
              "NET positions sum to zero up to CSV rounding")
    tci <- panel[[paste0("tci_", layer)]]
    add_check(paste0("tci_bounds_", layer), paste(range(tci), collapse = " to "),
              "0 to 100", all(is.finite(tci) & tci >= 0 & tci <= 100),
              "Connectedness index bounds")
  }
  post_2023 <- panel$greg_year > 2023
  add_check("gsdb_no_carry_forward", sum(post_2023 & is.na(panel$gsdb_active_total)),
            sum(post_2023),
            all(is.na(panel$gsdb_active_total[post_2023]) &
                  is.na(panel$gsdb_financial[post_2023]) &
                  is.na(panel$gsdb_trade[post_2023])),
            "GSDB R4 values must remain blank after 2023")
  add_check("gsdb_observed_flag", sum(panel$gsdb_observed == 1, na.rm = TRUE),
            sum(panel$greg_year <= 2023),
            all(panel$gsdb_observed == as.numeric(panel$greg_year <= 2023)),
            "Observed only inside official GSDB R4 coverage")
  add_check("positive_macro_levels", "checked", "all positive",
            all(panel$interbank_rate_pct > 0 & panel$oil_exports_mbd > 0 &
                  panel$iran_heavy_usd_per_bbl > 0, na.rm = TRUE),
            "Log changes require positive levels")
  headline_macro <- c(
    "inflation_monthly_pct", "interbank_rate_pct", "oil_exports_mbd",
    "days_in_jalali_month", "iran_heavy_usd_per_bbl"
  )
  add_check("headline_macro_complete", sum(stats::complete.cases(panel[, headline_macro])),
            nrow(panel), all(stats::complete.cases(panel[, headline_macro])),
            "Every headline oil, price, interbank, and inflation field is present")
  add_check("jalali_month_days_valid", paste(sort(unique(panel$days_in_jalali_month)), collapse = ";"),
            "29;30;31", all(panel$days_in_jalali_month %in% c(29, 30, 31)),
            "Monthly revenue conversion uses valid Jalali calendar-day counts")
  source_text <- tolower(paste(as.character(unlist(source_registry)), collapse = " "))
  required_source_tags <- c(
    "unitedagainstnucleariran.com", "eia.gov", "opec.org",
    "cbi.ir/policyrates", "globalsanctionsdatabase.com"
  )
  source_hits <- vapply(required_source_tags, grepl, logical(1), x = source_text, fixed = TRUE)
  add_check("documented_source_registry", paste(required_source_tags[source_hits], collapse = "; "),
            paste(required_source_tags, collapse = "; "), all(source_hits),
            "Unified source registry records the validated oil-volume, Iran Heavy, interbank, and sanctions source chain")
  input_integrity <- bind_rows(check_rows)
  safe_write_csv(input_integrity, file.path(tables_dir, "input_integrity_checks.csv"))
  if (any(input_integrity$status != "PASS")) {
    print(input_integrity)
    stop("Input audit failed. No econometric results were produced.", call. = FALSE)
  }

  ## ----------------------- Econometric transformations -------------------------
  panel$row_id <- seq_len(nrow(panel))
  for (layer in c("nominal", "real", "eom")) {
    panel[[paste0("core_avg_", layer)]] <-
      (panel[[paste0("net_", layer, "_usd")]] +
         panel[[paste0("net_", layer, "_gold")]]) / 2
    panel[[paste0("slow_receiver_", layer)]] <-
      -(panel[[paste0("net_", layer, "_vehicle")]] +
          panel[[paste0("net_", layer, "_housing")]]) / 2
    for (stem in c("tci", "core_avg", "slow_receiver")) {
      level_name <- paste0(stem, "_", layer)
      diff_name <- paste0("d_", stem, "_", layer)
      panel[[diff_name]] <- c(NA_real_, diff(panel[[level_name]]))
      panel[[lag_name(diff_name, 1L)]] <- dplyr::lag(panel[[diff_name]], 1L)
    }
  }
  panel$d_interbank <- c(NA_real_, diff(panel$interbank_rate_pct))
  panel$d_inflation <- c(NA_real_, diff(panel$inflation_monthly_pct))
  panel$iran_heavy_return_pct <- c(NA_real_, 100 * diff(log(panel$iran_heavy_usd_per_bbl)))
  panel$oil_volume_growth_pct <- c(NA_real_, 100 * diff(log(panel$oil_exports_mbd)))
  panel$oil_revenue_proxy_musd <- panel$oil_exports_mbd *
    panel$days_in_jalali_month * panel$iran_heavy_usd_per_bbl
  panel$oil_revenue_growth_pct <- c(
    NA_real_, 100 * diff(log(panel$oil_revenue_proxy_musd))
  )
  for (k in 1:4) {
    panel[[lag_name("inflation_monthly_pct", k)]] <- dplyr::lag(panel$inflation_monthly_pct, k)
    panel[[lag_name("d_inflation", k)]] <- dplyr::lag(panel$d_inflation, k)
    panel[[lag_name("d_interbank", k)]] <- dplyr::lag(panel$d_interbank, k)
    panel[[lag_name("iran_heavy_return_pct", k)]] <- dplyr::lag(panel$iran_heavy_return_pct, k)
    panel[[lag_name("oil_volume_growth_pct", k)]] <- dplyr::lag(panel$oil_volume_growth_pct, k)
    panel[[lag_name("oil_revenue_growth_pct", k)]] <- dplyr::lag(panel$oil_revenue_growth_pct, k)
    panel[[lag_name("greg_year", k)]] <- dplyr::lag(panel$greg_year, k)
  }

  outcome_catalog <- data.frame(
    dependent = c(
      "d_tci_nominal", "d_core_avg_nominal", "d_slow_receiver_nominal",
      "d_tci_real", "d_core_avg_real", "d_slow_receiver_real",
      "d_tci_eom", "d_core_avg_eom", "d_slow_receiver_eom"
    ),
    outcome_family = rep(c("TCI", "CORE", "RECEIVER"), 3L),
    measurement = rep(c("Nominal_monthly_average", "CPI_adjusted", "End_of_month"), each = 3L),
    label = rep(c("Change in TCI", "Change in CORE", "Change in RECEIVER"), 3L),
    stringsAsFactors = FALSE
  )

  make_model_data <- function(data, dependent, driver_lag = 1L,
                              driver_set = c("oil_revenue", "iran_heavy", "oil_decomposition"),
                              inflation_definition = c("level", "change"),
                              exclude_2020 = FALSE) {
    driver_set <- match.arg(driver_set)
    inflation_definition <- match.arg(inflation_definition)
    dep_lag_col <- lag_name(dependent, 1L)
    inflation_col <- lag_name(
      if (inflation_definition == "level") "inflation_monthly_pct" else "d_inflation",
      driver_lag
    )
    interbank_col <- lag_name("d_interbank", driver_lag)
    iran_heavy_col <- lag_name("iran_heavy_return_pct", driver_lag)
    volume_col <- lag_name("oil_volume_growth_pct", driver_lag)
    proxy_col <- lag_name("oil_revenue_growth_pct", driver_lag)
    year_cols <- unique(c("greg_year", lag_name("greg_year", 1L),
                          lag_name("greg_year", driver_lag),
                          lag_name("greg_year", driver_lag + 1L)))

    needed <- c(
      "row_id", "month_jalali", "date_gregorian", "greg_year", dependent, dep_lag_col,
      inflation_col, interbank_col, year_cols
    )
    if (driver_set == "oil_revenue") needed <- c(needed, proxy_col)
    if (driver_set == "iran_heavy") needed <- c(needed, iran_heavy_col)
    if (driver_set == "oil_decomposition") needed <- c(needed, iran_heavy_col, volume_col)
    needed <- unique(needed)
    df <- data[, needed, drop = FALSE]
    df <- df[stats::complete.cases(df), , drop = FALSE]
    ## Iran Heavy and oil-export-revenue growth are built from complete raw monthly
    ## levels in the unified contextual sheet; no extra quality-filter rows are imposed.
    if (exclude_2020) {
      no_2020 <- apply(df[, year_cols, drop = FALSE], 1L, function(z) all(z != 2020))
      df <- df[no_2020, , drop = FALSE]
    }
    if (nrow(df) < 45L) stop("Effective model sample is unexpectedly small.", call. = FALSE)

    df$dep_value <- df[[dependent]]
    df$dep_l1 <- df[[dep_lag_col]]
    df$z_inflation <- zscore(df[[inflation_col]])
    df$z_d_interbank <- zscore(df[[interbank_col]])
    if (driver_set == "oil_revenue") {
      df$z_oil_revenue_proxy <- zscore(df[[proxy_col]])
      drivers <- c("z_inflation", "z_d_interbank", "z_oil_revenue_proxy")
    } else if (driver_set == "iran_heavy") {
      df$z_iran_heavy_return <- zscore(df[[iran_heavy_col]])
      drivers <- c("z_inflation", "z_d_interbank", "z_iran_heavy_return")
    } else {
      df$z_iran_heavy_return <- zscore(df[[iran_heavy_col]])
      df$z_oil_volume <- zscore(df[[volume_col]])
      drivers <- c("z_inflation", "z_d_interbank", "z_iran_heavy_return", "z_oil_volume")
    }
    list(data = df, drivers = drivers, driver_lag = driver_lag,
         driver_set = driver_set, inflation_definition = inflation_definition,
         dependent = dependent)
  }

  term_label <- function(x) {
    labels <- c(
      "(Intercept)" = "Intercept",
      "dep_l1" = "Lagged dependent change",
      "z_inflation" = "Inflation (-L), 1 SD",
      "z_d_interbank" = "Change in interbank rate (-L), 1 SD",
      "z_iran_heavy_return" = "Iran Heavy return (-L), 1 SD",
      "z_oil_revenue_proxy" = "Oil-revenue proxy growth (-L), 1 SD",
      "z_oil_volume" = "Oil-export volume growth (-L), 1 SD"
    )
    unname(ifelse(x %in% names(labels), labels[x], x))
  }

  fit_model <- function(model_data, specification, outcome_family, measurement,
                        hac_lag = HAC_MAIN_LAG) {
    rhs <- c("dep_l1", model_data$drivers)
    fml <- stats::as.formula(paste("dep_value ~", paste(rhs, collapse = " + ")))
    fit <- stats::lm(fml, data = model_data$data)
    if (fit$rank < length(stats::coef(fit))) {
      stop("Rank-deficient Layer 4 regression: ", specification, call. = FALSE)
    }
    vc <- sandwich::NeweyWest(fit, lag = hac_lag, prewhite = FALSE, adjust = TRUE)
    ct <- lmtest::coeftest(fit, vcov. = vc)
    tab <- data.frame(
      specification = specification,
      dependent = model_data$dependent,
      outcome_family = outcome_family,
      measurement = measurement,
      driver_set = model_data$driver_set,
      inflation_definition = model_data$inflation_definition,
      driver_lag = model_data$driver_lag,
      hac_lag = hac_lag,
      term = rownames(ct),
      term_label = term_label(rownames(ct)),
      estimate = as.numeric(ct[, 1L]),
      se_hac = as.numeric(ct[, 2L]),
      t_hac = as.numeric(ct[, 3L]),
      p_hac = as.numeric(ct[, 4L]),
      ci_hac_low = as.numeric(ct[, 1L] - 1.96 * ct[, 2L]),
      ci_hac_high = as.numeric(ct[, 1L] + 1.96 * ct[, 2L]),
      n_obs = stats::nobs(fit),
      r_squared = summary(fit)$r.squared,
      adj_r_squared = summary(fit)$adj.r.squared,
      first_month = model_data$data$month_jalali[1L],
      last_month = model_data$data$month_jalali[nrow(model_data$data)],
      stringsAsFactors = FALSE
    )
    if (identical(model_data$inflation_definition, "change")) {
      tab$term_label[tab$term == "z_inflation"] <-
        "Change in inflation (-L), 1 SD"
    }
    list(fit = fit, data = model_data$data, drivers = model_data$drivers,
         table = tab, dependent = model_data$dependent,
         outcome_family = outcome_family, measurement = measurement,
         specification = specification)
  }

  catalog_row <- function(dep) outcome_catalog[outcome_catalog$dependent == dep, , drop = FALSE]

  ## ------------------------------ Main models ----------------------------------
  main_deps <- outcome_catalog$dependent[outcome_catalog$measurement == "Nominal_monthly_average"]
  main_models <- list()
  for (i in seq_along(main_deps)) {
    dep <- main_deps[i]
    catrow <- catalog_row(dep)
    md <- make_model_data(panel, dep, driver_lag = 1L, driver_set = "oil_revenue")
    main_models[[dep]] <- fit_model(
      md, "Main_nominal_lag1", catrow$outcome_family, catrow$measurement,
      hac_lag = HAC_MAIN_LAG
    )
  }
  main_coefficients <- bind_rows(lapply(main_models, `[[`, "table"))

  headline <- main_coefficients %>%
    filter(term %in% c("z_inflation", "z_d_interbank", "z_oil_revenue_proxy")) %>%
    mutate(q_bh_9tests = p.adjust(p_hac, method = "BH"))

  ## --------------------- Recursive wild-block bootstrap ------------------------
  recursive_wild_block <- function(model_object, B = BOOT_B, block_length = BOOT_BLOCK,
                                   seed = SEED) {
    fit <- model_object$fit
    df <- model_object$data
    drivers <- model_object$drivers
    beta <- stats::coef(fit)
    X <- stats::model.matrix(fit)
    lag_col <- match("dep_l1", colnames(X))
    if (is.na(lag_col)) stop("Bootstrap could not locate dep_l1.", call. = FALSE)
    residual <- stats::residuals(fit)
    residual <- residual - mean(residual)
    n <- length(residual)
    segment <- cumsum(c(TRUE, diff(df$row_id) != 1L))
    block_id <- integer(n)
    block_offset <- 0L
    for (s in unique(segment)) {
      idx <- which(segment == s)
      local_block <- ceiling(seq_along(idx) / block_length)
      block_id[idx] <- block_offset + local_block
      block_offset <- max(block_id[idx])
    }
    n_blocks <- max(block_id)
    boot <- matrix(NA_real_, nrow = B, ncol = length(beta),
                   dimnames = list(NULL, names(beta)))
    set.seed(seed)
    for (b in seq_len(B)) {
      multiplier <- sample(c(-1, 1), n_blocks, replace = TRUE)
      e_star <- residual * multiplier[block_id]
      y_star <- numeric(n)
      for (tt in seq_len(n)) {
        x_row <- X[tt, ]
        starts_segment <- tt == 1L || segment[tt] != segment[tt - 1L]
        x_row[lag_col] <- if (starts_segment) df$dep_l1[tt] else y_star[tt - 1L]
        y_star[tt] <- sum(x_row * beta) + e_star[tt]
      }
      df_star <- df
      df_star$dep_value <- y_star
      for (tt in seq_len(n)) {
        starts_segment <- tt == 1L || segment[tt] != segment[tt - 1L]
        df_star$dep_l1[tt] <- if (starts_segment) df$dep_l1[tt] else y_star[tt - 1L]
      }
      fit_star <- tryCatch(stats::lm(stats::formula(fit), data = df_star),
                           error = function(e) NULL)
      if (!is.null(fit_star)) boot[b, ] <- stats::coef(fit_star)[names(beta)]
    }
    boot <- boot[stats::complete.cases(boot), , drop = FALSE]
    if (nrow(boot) < 0.95 * B) stop("Too many failed bootstrap replications.", call. = FALSE)
    boot_drivers <- boot[, drivers, drop = FALSE]
    boot_se <- apply(boot_drivers, 2L, stats::sd)
    centered <- sweep(boot_drivers, 2L, beta[drivers], "-")
    max_t <- apply(abs(sweep(centered, 2L, boot_se, "/")), 1L, max)
    simultaneous_critical <- as.numeric(stats::quantile(max_t, 0.95, names = FALSE))
    data.frame(
      dependent = model_object$dependent,
      term = drivers,
      bootstrap = "Recursive wild-block Rademacher",
      B_requested = B,
      B_success = nrow(boot),
      block_length = block_length,
      estimate = as.numeric(beta[drivers]),
      se_boot = as.numeric(boot_se[drivers]),
      ci_boot_pct_low = apply(boot_drivers, 2L, stats::quantile, probs = 0.025, names = FALSE),
      ci_boot_pct_high = apply(boot_drivers, 2L, stats::quantile, probs = 0.975, names = FALSE),
      simultaneous_critical_95 = simultaneous_critical,
      ci_boot_sim_low = as.numeric(beta[drivers] - simultaneous_critical * boot_se[drivers]),
      ci_boot_sim_high = as.numeric(beta[drivers] + simultaneous_critical * boot_se[drivers]),
      stringsAsFactors = FALSE
    )
  }

  bootstrap_tables <- list()
  for (i in seq_along(main_models)) {
    bootstrap_tables[[i]] <- recursive_wild_block(
      main_models[[i]], B = BOOT_B, block_length = BOOT_BLOCK,
      seed = SEED + i * 1000L
    )
  }
  bootstrap_headline <- bind_rows(bootstrap_tables)
  headline <- headline %>%
    left_join(
      bootstrap_headline %>%
        select(dependent, term, B_requested, B_success, block_length,
               se_boot, ci_boot_pct_low, ci_boot_pct_high,
               simultaneous_critical_95, ci_boot_sim_low, ci_boot_sim_high),
      by = c("dependent", "term")
    ) %>%
    mutate(
      fdr_pass_5pct = q_bh_9tests < 0.05,
      simultaneous_boot_pass_5pct = ci_boot_sim_low * ci_boot_sim_high > 0
    )

  ## -------------------- Diagnostics and structural stability -------------------
  arch_lm <- function(residual, lag = 6L) {
    lag <- min(lag, floor(length(residual) / 4L))
    sq <- residual^2
    emb <- stats::embed(sq, lag + 1L)
    aux <- stats::lm(emb[, 1L] ~ emb[, -1L, drop = FALSE])
    stat <- nrow(emb) * summary(aux)$r.squared
    c(statistic = stat, df = lag, p_value = stats::pchisq(stat, df = lag, lower.tail = FALSE))
  }
  jarque_bera <- function(x) {
    n <- length(x); m <- mean(x); s <- stats::sd(x)
    skew <- mean((x - m)^3) / s^3
    kurt <- mean((x - m)^4) / s^4
    stat <- n / 6 * (skew^2 + (kurt - 3)^2 / 4)
    c(statistic = stat, df = 2, p_value = stats::pchisq(stat, df = 2, lower.tail = FALSE))
  }

  diagnose_model <- function(model_object) {
    fit <- model_object$fit
    e <- stats::residuals(fit)

    ## Ljung-Box degrees-of-freedom adjustment:
    ## Eq. (25) contains one lag of the dependent variable plus exogenous
    ## covariates. Only the dynamic AR lag is deducted here; the three
    ## exogenous covariates are not ARMA parameters.
    ar_fitdf <- 1L

    lb6 <- stats::Box.test(e, lag = min(6L, length(e) - 1L), type = "Ljung-Box",
                           fitdf = ar_fitdf)
    lb12 <- stats::Box.test(e, lag = min(12L, length(e) - 1L), type = "Ljung-Box",
                            fitdf = ar_fitdf)
    arch <- arch_lm(e, 6L)
    jb <- jarque_bera(e)
    bp <- tryCatch(lmtest::bptest(fit), error = function(e) NULL)
    reset <- tryCatch(lmtest::resettest(fit, power = 2:3, type = "fitted"),
                      error = function(e) NULL)
    data.frame(
      dependent = model_object$dependent,
      n_obs = stats::nobs(fit),
      ljung_box_6_stat = unname(lb6$statistic), ljung_box_6_p = lb6$p.value,
      ljung_box_12_stat = unname(lb12$statistic), ljung_box_12_p = lb12$p.value,
      arch_lm_6_stat = unname(arch["statistic"]), arch_lm_6_p = unname(arch["p_value"]),
      jarque_bera_stat = unname(jb["statistic"]), jarque_bera_p = unname(jb["p_value"]),
      breusch_pagan_stat = if (is.null(bp)) NA_real_ else unname(bp$statistic),
      breusch_pagan_p = if (is.null(bp)) NA_real_ else bp$p.value,
      reset_stat = if (is.null(reset)) NA_real_ else unname(reset$statistic),
      reset_p = if (is.null(reset)) NA_real_ else reset$p.value,
      inference_note = paste0(
        "HAC and wild-block inference address dependence/heteroskedasticity; ",
        "Ljung-Box(12) and RESET also enter the final strict gate"
      ),
      stringsAsFactors = FALSE
    )
  }
  residual_diagnostics <- bind_rows(lapply(main_models, diagnose_model))
  diagnostic_gate <- residual_diagnostics %>%
    transmute(
      dependent,
      ljung_box_12_p,
      reset_p,
      residual_serial_pass = is.finite(ljung_box_12_p) & ljung_box_12_p >= 0.05,
      reset_pass = is.finite(reset_p) & reset_p >= 0.05,
      diagnostic_specification_pass = residual_serial_pass & reset_pass,
      diagnostic_gate_note = paste0(
        "Requires Ljung-Box(12) p>=0.05 and RESET p>=0.05; ",
        "HAC/bootstrap address inference but do not repair a misspecified conditional mean"
      )
    )

  influence_rows <- bind_rows(lapply(main_models, function(model_object) {
    cd <- stats::cooks.distance(model_object$fit)
    threshold <- 4 / length(cd)
    data.frame(
      dependent = model_object$dependent,
      month_jalali = model_object$data$month_jalali,
      greg_year = model_object$data$greg_year,
      cooks_distance = as.numeric(cd),
      threshold_4_over_n = threshold,
      influential = as.numeric(cd) > threshold,
      stringsAsFactors = FALSE
    )
  }))
  influence_summary <- influence_rows %>%
    group_by(dependent) %>%
    summarise(
      n_obs = n(), threshold_4_over_n = first(threshold_4_over_n),
      influential_rows = sum(influential),
      max_cooks_distance = max(cooks_distance),
      max_influence_month = month_jalali[which.max(cooks_distance)],
      influential_months = paste(month_jalali[influential], collapse = ";"),
      note = "Influence diagnostic only; observations are never deleted automatically",
      .groups = "drop"
    )

  stability_model <- function(model_object) {
    fml <- stats::formula(model_object$fit)
    df <- model_object$data
    cusum <- tryCatch({
      proc <- strucchange::efp(fml, data = df, type = "Rec-CUSUM")
      strucchange::sctest(proc)
    }, error = function(e) NULL)
    bp <- tryCatch(strucchange::breakpoints(fml, data = df, h = 0.25),
                   error = function(e) NULL)
    n_breaks <- NA_integer_; break_months <- NA_character_
    if (!is.null(bp)) {
      bic <- tryCatch(stats::BIC(bp), error = function(e) NULL)
      if (!is.null(bic) && length(bic) > 0L && any(is.finite(bic))) {
        n_breaks <- which.min(bic) - 1L
        if (n_breaks > 0L) {
          selected <- tryCatch(strucchange::breakpoints(bp, breaks = n_breaks),
                               error = function(e) NULL)
          if (!is.null(selected)) {
            idx <- selected$breakpoints[is.finite(selected$breakpoints)]
            break_months <- if (length(idx)) paste(df$month_jalali[idx], collapse = ";") else NA_character_
          }
        } else break_months <- "none"
      }
    }
    data.frame(
      dependent = model_object$dependent,
      rec_cusum_stat = if (is.null(cusum)) NA_real_ else unname(cusum$statistic),
      rec_cusum_p = if (is.null(cusum)) NA_real_ else cusum$p.value,
      bic_selected_breaks = n_breaks,
      bic_break_months = break_months,
      stability_pass = !is.na(n_breaks) && n_breaks == 0L &&
        !is.null(cusum) && is.finite(cusum$p.value) && cusum$p.value >= 0.05,
      note = "Stability diagnostic, not a data-mined respecification",
      stringsAsFactors = FALSE
    )
  }
  stability_diagnostics <- bind_rows(lapply(main_models, stability_model))
  headline <- headline %>%
    left_join(stability_diagnostics %>% select(dependent, stability_pass), by = "dependent") %>%
    left_join(diagnostic_gate, by = "dependent") %>%
    mutate(
      headline_inference_pass = fdr_pass_5pct & simultaneous_boot_pass_5pct & stability_pass,
      strict_evidence_flag = FALSE,
      interpretation = "Pending the pre-specified diagnostic and robustness gates"
    )

  ## ---------------------------- Stationarity screen ----------------------------
  stationarity_one <- function(x, variable) {
    x <- x[is.finite(x)]
    adf <- tryCatch(suppressWarnings(tseries::adf.test(x,
      k = min(floor((length(x) - 1)^(1 / 3)), 6L))), error = function(e) NULL)
    kpss <- tryCatch(suppressWarnings(tseries::kpss.test(x, null = "Level", lshort = TRUE)),
                     error = function(e) NULL)
    adf_p <- if (is.null(adf)) NA_real_ else adf$p.value
    kpss_p <- if (is.null(kpss)) NA_real_ else kpss$p.value
    conclusion <- if (is.finite(adf_p) && is.finite(kpss_p) && adf_p < 0.05 && kpss_p >= 0.05) {
      "Stationarity supported by both tests"
    } else if (is.finite(adf_p) && is.finite(kpss_p) && adf_p >= 0.05 && kpss_p < 0.05) {
      "Nonstationarity warning"
    } else "Mixed/inconclusive screen"
    data.frame(
      variable = variable, n = length(x),
      adf_stat = if (is.null(adf)) NA_real_ else unname(adf$statistic), adf_p = adf_p,
      kpss_level_stat = if (is.null(kpss)) NA_real_ else unname(kpss$statistic),
      kpss_level_p = kpss_p, conclusion = conclusion, stringsAsFactors = FALSE
    )
  }
  stationarity_series <- c(
    main_deps, "inflation_monthly_pct", "d_inflation", "d_interbank", "iran_heavy_return_pct",
    "oil_volume_growth_pct", "oil_revenue_growth_pct"
  )
  stationarity_screen <- bind_rows(lapply(
    stationarity_series, function(v) stationarity_one(panel[[v]], v)
  ))

  ## ---------------------------- Robustness grid --------------------------------
  add_family_fdr <- function(tab) {
    driver_names <- c("z_inflation", "z_d_interbank", "z_iran_heavy_return",
                      "z_oil_revenue_proxy", "z_oil_volume")
    tab$p_bh_within_spec <- NA_real_
    for (s in unique(tab$specification)) {
      idx <- which(tab$specification == s & tab$term %in% driver_names)
      tab$p_bh_within_spec[idx] <- p.adjust(tab$p_hac[idx], method = "BH")
    }
    tab
  }

  ## A. Exclude every equation row whose outcome, lagged outcome, or driver source
  ## touches Gregorian 2020. This avoids artificial lags across the removed block.
  exclude_2020_models <- list()
  for (dep in main_deps) {
    catrow <- catalog_row(dep)
    md <- make_model_data(panel, dep, 1L, "oil_revenue", exclude_2020 = TRUE)
    exclude_2020_models[[dep]] <- fit_model(
      md, "Robust_exclude_2020_exposure", catrow$outcome_family, catrow$measurement, HAC_MAIN_LAG
    )
  }
  robust_exclude_2020 <- add_family_fdr(bind_rows(lapply(exclude_2020_models, `[[`, "table")))

  ## B. Pre-specified driver-lag sensitivity. The dependent lag remains one month.
  driver_lag_models <- list()
  counter <- 0L
  for (L in 2:3) for (dep in main_deps) {
    counter <- counter + 1L
    catrow <- catalog_row(dep)
    md <- make_model_data(panel, dep, L, "oil_revenue", exclude_2020 = FALSE)
    driver_lag_models[[counter]] <- fit_model(
      md, paste0("Robust_driver_lag", L), catrow$outcome_family, catrow$measurement, HAC_MAIN_LAG
    )
  }
  robust_driver_lags <- add_family_fdr(bind_rows(lapply(driver_lag_models, `[[`, "table")))

  ## C. Measurement robustness: CPI-adjusted and end-of-month TVP paths.
  measurement_models <- list()
  counter <- 0L
  alternative_deps <- outcome_catalog$dependent[outcome_catalog$measurement != "Nominal_monthly_average"]
  for (dep in alternative_deps) {
    counter <- counter + 1L
    catrow <- catalog_row(dep)
    md <- make_model_data(panel, dep, 1L, "oil_revenue", exclude_2020 = FALSE)
    measurement_models[[counter]] <- fit_model(
      md, paste0("Robust_measurement_", catrow$measurement),
      catrow$outcome_family, catrow$measurement, HAC_MAIN_LAG
    )
  }
  robust_measurement <- add_family_fdr(bind_rows(lapply(measurement_models, `[[`, "table")))

  ## D. HAC bandwidth sensitivity; coefficient estimates are intentionally fixed.
  hac_tables <- list(); counter <- 0L
  for (h in c(3L, 6L, 12L)) for (dep in main_deps) {
    counter <- counter + 1L
    catrow <- catalog_row(dep)
    md <- make_model_data(panel, dep, 1L, "oil_revenue", exclude_2020 = FALSE)
    hac_tables[[counter]] <- fit_model(
      md, paste0("Robust_HAC_lag", h), catrow$outcome_family, catrow$measurement, h
    )$table
  }
  robust_hac_lags <- add_family_fdr(bind_rows(hac_tables))

  ## E. Oil-driver construction robustness using the validated EIA/UANI volume
  ## series and the OPEC Iran Heavy benchmark separately and jointly.
  oil_models <- list(); counter <- 0L
  for (kind in c("iran_heavy", "oil_decomposition")) for (dep in main_deps) {
    counter <- counter + 1L
    catrow <- catalog_row(dep)
    md <- make_model_data(panel, dep, 1L, kind, exclude_2020 = FALSE)
    oil_models[[counter]] <- fit_model(
      md, paste0("Robust_oil_driver_", kind),
      catrow$outcome_family, catrow$measurement, HAC_MAIN_LAG
    )
  }
  oil_driver_robustness <- add_family_fdr(bind_rows(lapply(oil_models, `[[`, "table")))

  ## F. Inflation-definition robustness. The observed stationarity warning for the
  ## monthly inflation rate is handled transparently by re-estimating with lagged
  ## inflation acceleration (first difference), without replacing the headline.
  inflation_change_models <- list(); counter <- 0L
  for (dep in main_deps) {
    counter <- counter + 1L
    catrow <- catalog_row(dep)
    md <- make_model_data(
      panel, dep, driver_lag = 1L, driver_set = "oil_revenue",
      inflation_definition = "change", exclude_2020 = FALSE
    )
    inflation_change_models[[counter]] <- fit_model(
      md, "Robust_inflation_first_difference",
      catrow$outcome_family, catrow$measurement, HAC_MAIN_LAG
    )
  }
  robust_inflation_definition <- add_family_fdr(
    bind_rows(lapply(inflation_change_models, `[[`, "table"))
  )

  ## Sign concordance and 2020-retention are pre-specified robustness gates. They
  ## prevent a crisis-dominated or measurement-sensitive coefficient from being
  ## labelled "strict" merely because the full-sample p-value is small.
  driver_terms <- c("z_inflation", "z_d_interbank", "z_oil_revenue_proxy")
  main_signs <- headline %>%
    select(outcome_family, term, main_estimate = estimate) %>%
    mutate(main_sign = sign(main_estimate))
  robust_sign_pool <- bind_rows(
    robust_exclude_2020,
    robust_driver_lags,
    robust_measurement,
    robust_inflation_definition
  ) %>%
    filter(term %in% driver_terms) %>%
    select(specification, outcome_family, measurement, term, estimate)
  sign_concordance <- robust_sign_pool %>%
    left_join(main_signs, by = c("outcome_family", "term")) %>%
    group_by(outcome_family, term, main_estimate, main_sign) %>%
    summarise(
      robustness_estimates = n(),
      same_sign = sum(sign(estimate) == main_sign, na.rm = TRUE),
      sign_concordance_share = same_sign / robustness_estimates,
      .groups = "drop"
    )

  exclude_2020_terms <- robust_exclude_2020 %>%
    filter(term %in% driver_terms) %>%
    select(
      outcome_family, term,
      exclude_2020_estimate = estimate,
      exclude_2020_p_hac = p_hac,
      exclude_2020_q_bh = p_bh_within_spec
    )

  ## The headline predictors are standardized separately inside each estimation
  ## sample. Therefore, a coefficient from the 2020-exclusion sample cannot be
  ## compared directly with its baseline coefficient. Convert it to the baseline
  ## predictor scale first:
  ##   beta_excl_base = beta_excl * SD_base / SD_excl
  ## and only then compute rho = |beta_excl_base| / |beta_main|.
  retention_raw_columns <- c(
    z_inflation = "inflation_monthly_pct_l1",
    z_d_interbank = "d_interbank_l1",
    z_oil_revenue_proxy = "oil_revenue_growth_pct_l1"
  )
  retention_scaling <- bind_rows(lapply(main_deps, function(dep) {
    catrow <- catalog_row(dep)
    base_df <- main_models[[dep]]$data
    excl_df <- exclude_2020_models[[dep]]$data
    bind_rows(lapply(names(retention_raw_columns), function(term_name) {
      raw_col <- unname(retention_raw_columns[[term_name]])
      sd_base <- safe_sd(base_df[[raw_col]])
      sd_excl <- safe_sd(excl_df[[raw_col]])
      data.frame(
        outcome_family = catrow$outcome_family[[1L]],
        term = term_name,
        retention_raw_column = raw_col,
        sd_predictor_base = sd_base,
        sd_predictor_excl_2020 = sd_excl,
        predictor_scale_factor_base_over_excl = sd_base / sd_excl,
        stringsAsFactors = FALSE
      )
    }))
  }))

  if (any(!is.finite(retention_scaling$predictor_scale_factor_base_over_excl)) ||
      any(retention_scaling$predictor_scale_factor_base_over_excl <= 0)) {
    stop("Invalid predictor-scale conversion for the 2020-exclusion gate.", call. = FALSE)
  }

  robustness_gate <- sign_concordance %>%
    left_join(exclude_2020_terms, by = c("outcome_family", "term")) %>%
    left_join(retention_scaling, by = c("outcome_family", "term")) %>%
    mutate(
      sign_concordance_pass = robustness_estimates > 0 & same_sign == robustness_estimates,
      exclude_2020_estimate_base_scale = exclude_2020_estimate *
        predictor_scale_factor_base_over_excl,
      exclude_2020_same_sign = sign(exclude_2020_estimate_base_scale) == main_sign,
      exclude_2020_retention_ratio = abs(exclude_2020_estimate_base_scale) /
        pmax(abs(main_estimate), sqrt(.Machine$double.eps)),
      exclude_2020_retention_pass = exclude_2020_same_sign &
        exclude_2020_retention_ratio >= 0.50,
      robustness_pass = sign_concordance_pass & exclude_2020_retention_pass,
      robustness_gate_note = paste0(
        "Requires identical signs across exclusion/lag/measurement/inflation-definition checks ",
        "and at least 50% coefficient retention after excluding all 2020-exposed rows; ",
        "the exclusion coefficient is first converted to the baseline predictor scale (Eq. 27)"
      )
    )

  headline <- headline %>%
    left_join(
      robustness_gate %>%
        select(
          outcome_family, term, robustness_estimates, same_sign,
          sign_concordance_share, sign_concordance_pass,
          exclude_2020_estimate, exclude_2020_p_hac, exclude_2020_q_bh,
          retention_raw_column, sd_predictor_base, sd_predictor_excl_2020,
          predictor_scale_factor_base_over_excl,
          exclude_2020_estimate_base_scale,
          exclude_2020_same_sign, exclude_2020_retention_ratio,
          exclude_2020_retention_pass, robustness_pass, robustness_gate_note
        ),
      by = c("outcome_family", "term")
    ) %>%
    mutate(
      strict_evidence_flag = headline_inference_pass &
        diagnostic_specification_pass & robustness_pass,
      interpretation = case_when(
        strict_evidence_flag ~ "Strict conditional association; not causal",
        headline_inference_pass ~ paste0(
          "Headline-only association; fails diagnostic and/or robustness gate"
        ),
        TRUE ~ "Not supported by the headline inference gates"
      )
    )

  ## ---------------------------- GSDB descriptive only --------------------------
  gsdb_observed <- panel %>% filter(gsdb_observed == 1)
  gsdb_annual_observed <- gsdb_observed %>%
    group_by(greg_year) %>%
    summarise(
      first_month = first(month_jalali),
      last_month = last(month_jalali),
      months = n(),
      total_active = first(gsdb_active_total),
      financial_cases = first(gsdb_financial),
      trade_cases = first(gsdb_trade),
      mean_tci_nominal = mean(tci_nominal),
      sd_tci_nominal = sd(tci_nominal),
      .groups = "drop"
    )

  gsdb_method_note <- data.frame(
    item = c(
      "Official coverage", "Target selection", "Active-case rule",
      "Calendar alignment", "Measurement meaning", "Permitted interpretation",
      "Forbidden interpretation"
    ),
    value = c(
      "GSDB Release 4: cases through 2023; no post-2023 carry-forward",
      "Iran appears among sanctioned states, including multi-target cases",
      "A case is active in Gregorian year y when begin <= y <= end",
      "Annual case counts are aligned to TVP months in the same Gregorian year",
      "Counts capture active sanctions-case breadth, not economic severity",
      "Descriptive annual context only; five overlapping years (2019-2023)",
      "No monthly sanctions coefficient, causal effect, regime effect, or threshold claim"
    ), stringsAsFactors = FALSE
  )

  ## ------------------------------- Correlations --------------------------------
  driver_sample <- main_models[[1]]$data %>%
    select(z_inflation, z_d_interbank, z_oil_revenue_proxy)
  driver_correlations <- as.data.frame(round(stats::cor(driver_sample), 3))
  driver_correlations <- data.frame(
    variable = rownames(driver_correlations), driver_correlations,
    row.names = NULL, check.names = FALSE
  )

  ## -------------------------------- Figures ------------------------------------
  plot_headline <- headline %>%
    mutate(
      outcome_label = factor(outcome_family,
        levels = c("TCI", "CORE", "RECEIVER"),
        labels = c("TCI", "CORE", "RECEIVER")),
      driver_label = factor(term,
        levels = c("z_inflation", "z_d_interbank", "z_oil_revenue_proxy"),
        labels = c("Inflation (-1)", "Δ Interbank (-1)", "Oil-revenue growth (-1)"))
    )
  p_coef <- ggplot(plot_headline,
                   aes(x = driver_label, y = estimate, ymin = ci_boot_sim_low,
                       ymax = ci_boot_sim_high)) +
    geom_hline(yintercept = 0, linewidth = 0.35, color = "grey40") +
    geom_errorbar(width = 0.15, linewidth = 0.55) +
    geom_point(aes(shape = strict_evidence_flag), size = 2.5) +
    facet_wrap(~ outcome_label, scales = "free_y") +
    scale_shape_manual(values = c(`FALSE` = 1, `TRUE` = 16),
                       name = "All strict gates pass") +
    labs(
      x = NULL, y = "Outcome change per one-SD driver change",
      title = "Layer 4 headline associations",
      subtitle = "Recursive wild-block simultaneous 95% intervals; associational, not causal"
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 25, hjust = 1))
  ggsave(file.path(figures_dir, "FigL4_01_headline_coefficients.png"), p_coef,
         width = 10, height = 6, dpi = 300, bg = "white")

  measurement_plot <- panel %>%
    select(date_gregorian, tci_nominal, tci_real, tci_eom) %>%
    pivot_longer(-date_gregorian, names_to = "measurement", values_to = "TCI") %>%
    mutate(measurement = recode(measurement,
      tci_nominal = "Nominal monthly average",
      tci_real = "CPI-adjusted",
      tci_eom = "End-of-month"))
  p_measurement <- ggplot(measurement_plot, aes(date_gregorian, TCI, color = measurement)) +
    geom_line(linewidth = 0.75) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(x = "Gregorian year", y = "TVP-TCI", color = NULL,
         title = "Connectedness under three measurement definitions") +
    theme_minimal(base_size = 11) + theme(legend.position = "bottom")
  ggsave(file.path(figures_dir, "FigL4_02_measurement_robustness.png"), p_measurement,
         width = 9, height = 5, dpi = 300, bg = "white")

  gsdb_plot <- gsdb_annual_observed %>%
    mutate(year_label = as.character(greg_year))
  p_gsdb <- ggplot(gsdb_plot, aes(x = total_active, y = mean_tci_nominal, label = year_label)) +
    geom_path(linewidth = 0.6, color = "grey45") +
    geom_point(size = 2.6, color = "black") +
    geom_text(nudge_y = 0.25, size = 3.2) +
    labs(
      x = "Active GSDB sanctions cases involving Iran",
      y = "Annual mean nominal TVP-TCI",
      title = "GSDB sanctions-case breadth and annual mean TCI, 2019-2023",
      subtitle = "Descriptive calendar alignment only; the comparison does not identify a sanctions effect"
    ) +
    theme_minimal(base_size = 11)
  ggsave(file.path(figures_dir, "FigL4_03_GSDB_descriptive_context.png"), p_gsdb,
         width = 9, height = 5, dpi = 300, bg = "white")

  ## ------------------------------ Output tables --------------------------------
  sample_info <- bind_rows(lapply(main_models, function(x) data.frame(
    specification = x$specification,
    dependent = x$dependent,
    n_obs = nrow(x$data),
    first_month = x$data$month_jalali[1L],
    last_month = x$data$month_jalali[nrow(x$data)],
    driver_lag = 1L,
    hac_lag = HAC_MAIN_LAG,
    bootstrap_B = BOOT_B,
    bootstrap_block = BOOT_BLOCK,
    stringsAsFactors = FALSE
  )))

  tables <- list(
    input_integrity_checks = input_integrity,
    data_lineage = source_registry,
    sample_info = sample_info,
    headline_driver_terms = headline,
    main_coefficients_all = main_coefficients,
    bootstrap_headline = bootstrap_headline,
    residual_diagnostics = residual_diagnostics,
    diagnostic_gate = diagnostic_gate,
    influence_summary = influence_summary,
    influence_rows = influence_rows,
    stability_diagnostics = stability_diagnostics,
    stationarity_screen = stationarity_screen,
    robust_exclude_2020 = robust_exclude_2020,
    robust_driver_lags = robust_driver_lags,
    robust_measurement = robust_measurement,
    robust_hac_lags = robust_hac_lags,
    oil_driver_robustness = oil_driver_robustness,
    robust_inflation_definition = robust_inflation_definition,
    sign_concordance = sign_concordance,
    retention_scaling = retention_scaling,
    robustness_gate = robustness_gate,
    driver_correlations = driver_correlations,
    gsdb_annual_observed = gsdb_annual_observed,
    gsdb_tci_annual_context = gsdb_annual_observed,
    gsdb_method_note = gsdb_method_note,
    model_panel_derived = panel
  )
  for (nm in names(tables)) {
    safe_write_csv(tables[[nm]], file.path(tables_dir, paste0(nm, ".csv")))
  }
  writexl::write_xlsx(tables, file.path(out_dir, "layer4_Q1_results.xlsx"))

  headline_supported <- headline %>% filter(headline_inference_pass)
  supported <- headline %>% filter(strict_evidence_flag)
  report <- c(
    "LAYER 4 — FINAL CONTEXTUAL-DRIVER ANALYSIS",
    "===========================================",
    "Interpretation: conditional associations only; not causal or structural effects.",
    "Headline drivers: lagged monthly inflation, lagged change in interbank rate, lagged oil-export-revenue-proxy growth.",
    sprintf("Inference: Newey-West HAC lag %d; BH-FDR across 9 driver tests; recursive wild-block bootstrap B=%d, block=%d.",
            HAC_MAIN_LAG, BOOT_B, BOOT_BLOCK),
    "Headline inference gate: BH q<0.05 + simultaneous bootstrap interval excludes zero + equation stability passes.",
    "Final strict gate additionally requires Ljung-Box(12) and RESET to pass, complete sign concordance across pre-specified robustness checks, and >=50% scale-adjusted coefficient retention after excluding all 2020-exposed rows (Eq. 27).",
    "GSDB R4: descriptive only, observed through 2023, no carry-forward, no monthly sanctions coefficient.",
    "Oil-export volume: validated EIA/UANI crude-plus-condensate series; combined with the OPEC Iran Heavy benchmark in the headline oil-export-revenue proxy.",
    "Bootstrap scope: second-stage dynamic-regression uncertainty only; upstream TVP paths are treated as fixed generated outcomes.",
    "",
    paste0("Headline-inference result: ", nrow(headline_supported),
           " association(s) pass the HAC/FDR/bootstrap/stability gate."),
    if (nrow(supported) == 0L) {
      "Final strict result: no association also passes the diagnostic and full robustness gates."
    } else {
      paste0("Final strict result: ", nrow(supported),
             " association(s) pass every pre-specified gate; see headline_driver_terms.csv.")
    },
    "",
    "This script cannot by itself confer Q1 publication status. It prevents unsupported sanctions inference,",
    "documents generated-regressor limits, and supplies auditable robustness and diagnostic evidence."
  )
  writeLines(report, file.path(out_dir, "Layer4_Q1_report.txt"), useBytes = TRUE)

  repro_note <- c(
    "Reproduction instructions",
    "========================",
    "1. Run the unified script with data(3).xlsx as its only data input.",
    "2. Layer 4 is constructed internally from the strict-split TVP outputs and the raw contextual/GSDB sheets.",
    "3. Default Layer-4 bootstrap replications: 1999. For a syntax/smoke test only, set LAYER4_BOOT_B=499.",
    "4. Never replace missing post-2023 GSDB values with 2023 values.",
    "5. The bootstrap is second-stage only; it does not re-estimate the upstream TVP paths in each draw.",
    "",
    paste("Input MD5:", unname(tools::md5sum(input_path))),
    paste("Seed:", SEED),
    paste("R version:", R.version.string)
  )
  writeLines(repro_note, file.path(repro_dir, "README_reproducibility.txt"), useBytes = TRUE)
  writeLines(capture.output(sessionInfo()), file.path(repro_dir, "sessionInfo.txt"), useBytes = TRUE)

  package_versions <- data.frame(
    package = required_packages,
    version = vapply(required_packages, function(p) as.character(utils::packageVersion(p)), character(1)),
    stringsAsFactors = FALSE
  )
  safe_write_csv(package_versions, file.path(repro_dir, "package_versions.csv"))

  if (is.character(EXECUTED_SCRIPT_PATH) && length(EXECUTED_SCRIPT_PATH) == 1L &&
      !is.na(EXECUTED_SCRIPT_PATH)) {
    file.copy(
      EXECUTED_SCRIPT_PATH,
      file.path(repro_dir, basename(EXECUTED_SCRIPT_PATH)), overwrite = TRUE
    )
  }

  writeLines(c(
    paste("Input:", input_path),
    paste("Output:", out_dir),
    paste("Completed:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste("Rows in input:", nrow(panel)),
    paste("Headline equation N:", paste(unique(sample_info$n_obs), collapse = ", ")),
    paste("Headline-inference associations:", nrow(headline_supported)),
    paste("Final strict supported associations:", nrow(supported))
  ), file.path(out_dir, "00_run_log.txt"), useBytes = TRUE)

  message("Layer 4 completed. Output folder:\n", out_dir)

})


while (sink.number() > 0) sink()

## ----------------------- MASTER: unified result index ---------------------- ##
l12_cmp_file <- file.path(MASTER_OUT_DIR, "layers_1_2", "Layer_Comparison_TCI_NET.csv")
l12_pair_file <- file.path(MASTER_OUT_DIR, "layers_1_2", "Layer_Difference_PairedBootstrap.csv")
l3_sum_file <- file.path(MASTER_OUT_DIR, "layer_3_eom", "Layer3_Summary_TCI_NET.csv")
l3_pair_file <- file.path(MASTER_OUT_DIR, "layer_3_eom", "Layer3_vs_Layer1_PairedBootstrap.csv")
l4_headline_file <- file.path(
  MASTER_OUT_DIR, "layer_4_contextual", "tables_csv", "headline_driver_terms.csv"
)
l4_gsdb_file <- file.path(
  MASTER_OUT_DIR, "layer_4_contextual", "tables_csv", "gsdb_tci_annual_context.csv"
)

read_if_exists <- function(path) {
  if (file.exists(path)) {
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    data.frame(Note = paste("Missing expected output:", path), stringsAsFactors = FALSE)
  }
}

key_l12 <- read_if_exists(l12_cmp_file)
key_l3 <- read_if_exists(l3_sum_file)
key_l2diff <- read_if_exists(l12_pair_file)
key_l3diff <- read_if_exists(l3_pair_file)
key_l4 <- read_if_exists(l4_headline_file)
key_gsdb <- read_if_exists(l4_gsdb_file)

writexl::write_xlsx(
  list(
    Static_L1_L2 = key_l12,
    Static_L3 = key_l3,
    CPIadj_minus_nominal = key_l2diff,
    EOM_minus_nominal = key_l3diff,
    Layer4_headline = key_l4,
    GSDB_context = key_gsdb,
    Raw_input_audit = MASTER_INPUT_AUDIT
  ),
  file.path(MASTER_OUT_DIR, "Unified_Key_Results.xlsx")
)

output_index <- c(
  "UNIFIED FOUR-LAYER RAW-DATA PIPELINE — OUTPUT INDEX",
  "===================================================",
  paste("Raw input:", MASTER_DATA_FILE),
  paste("Input MD5:", unname(tools::md5sum(MASTER_DATA_FILE))),
  "",
  "1) layers_1_2/",
  "   - layer1_nominal/: baseline nominal monthly-average specification",
  "   - layer2_real/: legacy folder name for the CPI-adjusted specification",
  "   - all static GFEVD/TCI/FROM/TO/NET/NPDC results",
  "   - wild-block bootstrap and simultaneous NET/NPDC bands",
  "   - strict-split TVP-VAR paths and tuning sensitivity",
  "   - diagnostics, event checks, frequency decomposition, GIRFs",
  "   - Layer-2 targeted HOUSING break/VAR(2) robustness",
  "   - direct paired CPI-adjusted-minus-nominal inference",
  "",
  "2) layer_3_eom/",
  "   - end-of-month specification for USD/GOLD/STOCK",
  "   - same static/bootstrap/TVP/diagnostic/frequency/GIRF pipeline",
  "   - direct paired EOM-minus-monthly-average inference",
  "",
  "3) layer_4_contextual/",
  "   - contextual regressions from strict-split TVP paths",
  "   - lagged CPI inflation, change in interbank rate, and oil-export-revenue-proxy growth",
  "   - oil proxy constructed from crude+condensate volume x Jalali days x Iran Heavy price",
  "   - HAC, BH-FDR, recursive wild-block simultaneous intervals, diagnostics and robustness",
  "   - GSDB used only as annual descriptive case-breadth context for 2019-2023",
  "",
  "4) derived_from_raw/",
  "   - transparent return/contextual transformations built directly from the raw workbook",
  "   - GSDB annual Iran-target counts",
  "   - internally constructed Layer-4 panel",
  "",
  "5) Unified_Key_Results.xlsx",
  "   - compact index of the principal outputs; authoritative detailed outputs remain in layer folders.",
  "",
  "Intentional final-design difference from the older Layer-4 package:",
  "   - no GSDB High/Low classification or gsdb_regime_summary is produced;",
  "   - no Brent price is used; the price input is Iran Heavy.",
  "",
  paste("Completed:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
)
writeLines(output_index, file.path(MASTER_OUT_DIR, "README_UNIFIED_OUTPUTS.txt"), useBytes = TRUE)

## Save the exact code used when the script path can be detected.
detect_master_script <- function() {
  frames <- sys.frames()
  candidates <- unlist(lapply(frames, function(f) {
    if (!is.null(f$ofile)) as.character(f$ofile) else character(0)
  }), use.names = FALSE)
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(candidates)) return(normalizePath(tail(candidates, 1L), mustWork = TRUE))
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg)) {
    candidate <- sub("^--file=", "", tail(file_arg, 1L))
    if (file.exists(candidate)) return(normalizePath(candidate, mustWork = TRUE))
  }
  NA_character_
}
master_script_path <- detect_master_script()
if (is.character(master_script_path) && length(master_script_path) == 1L &&
    !is.na(master_script_path)) {
  file.copy(
    master_script_path,
    file.path(MASTER_OUT_DIR, "Unified_Four_Layer_Raw_Pipeline_USED.R"),
    overwrite = TRUE
  )
}

writeLines(
  c(
    paste("Input:", MASTER_DATA_FILE),
    paste("Input MD5:", unname(tools::md5sum(MASTER_DATA_FILE))),
    paste("Output:", MASTER_OUT_DIR),
    paste("Completed:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    "All raw transformations are performed inside this unified script.",
    "Layer 1/2 and Layer 3 analytical engines preserve the locked Q1 code logic.",
    "Layer 4 preserves the final contextual inference architecture but uses the current raw interbank data and Iran Heavy oil benchmark.",
    "GSDB is descriptive only and is not carried beyond 2023."
  ),
  file.path(MASTER_OUT_DIR, "00_MASTER_run_log.txt"),
  useBytes = TRUE
)

cat("\n============================================================\n")
cat("UNIFIED PIPELINE COMPLETED\n")
cat("All outputs are in:\n", MASTER_OUT_DIR, "\n")
cat("============================================================\n")
