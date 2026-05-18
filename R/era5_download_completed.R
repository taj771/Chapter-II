# Download all completed CDS jobs programmatically
# Lists successful jobs from CDS API and downloads any missing monthly files
# Run this instead of manually clicking 50 download buttons in the UI

library(ecmwfr)
library(httr)
library(jsonlite)
library(ncdf4)

OUT_DIR <- "./Data/ERA5/monthly"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

CDS_BASE <- "https://cds.climate.copernicus.eu/api"
TOKEN    <- wf_get_key()

# CDS Beta API v2: Personal Access Token requires "Bearer" prefix
auth_header <- add_headers(Authorization = paste("Bearer", TOKEN))

# ── 1. List all successful jobs ───────────────────────────────────────────────

message("Fetching completed jobs from CDS...")
resp <- GET(
  paste0(CDS_BASE, "/retrieve/v1/jobs"),
  query = list(status = "successful", limit = 200),
  auth_header,
  timeout(30)
)

if (status_code(resp) != 200) {
  stop("Failed to list jobs: HTTP ", status_code(resp), "\n", content(resp, "text"))
}

jobs_data <- fromJSON(content(resp, "text", encoding = "UTF-8"))
jobs <- jobs_data$jobs

if (is.null(jobs) || nrow(jobs) == 0) {
  message("No completed jobs found in CDS queue.")
  quit(save = "no")
}

message(sprintf("Found %d completed jobs", nrow(jobs)))

# ── 2. Download each job's result ─────────────────────────────────────────────

for (i in seq_len(nrow(jobs))) {
  job_id <- jobs$jobID[i]

  # Get download URL from results endpoint
  res_resp <- GET(
    paste0(CDS_BASE, "/retrieve/v1/jobs/", job_id, "/results"),
    auth_header,
    timeout(30)
  )

  if (status_code(res_resp) != 200) {
    message(sprintf("  Job %s: results endpoint failed (%d)", job_id, status_code(res_resp)))
    next
  }

  res <- tryCatch(fromJSON(content(res_resp, "text", encoding = "UTF-8")),
                  error = function(e) NULL)
  if (is.null(res) || is.null(res$asset$value$href)) {
    message(sprintf("  Job %s: no download URL", job_id))
    next
  }

  dl_url <- res$asset$value$href
  tmp_file <- file.path(OUT_DIR, paste0("tmp_", job_id, ".nc"))

  message(sprintf("  Downloading job %s ...", job_id))
  dl_resp <- GET(dl_url, auth_header, write_disk(tmp_file, overwrite = TRUE), timeout(600))

  if (status_code(dl_resp) != 200 || !file.exists(tmp_file) || file.size(tmp_file) < 10000) {
    message(sprintf("  FAILED download for job %s", job_id))
    if (file.exists(tmp_file)) file.remove(tmp_file)
    next
  }

  # Detect year-month from NC time metadata
  nc <- tryCatch(nc_open(tmp_file), error = function(e) NULL)
  if (is.null(nc)) {
    message("  Cannot read NC: ", basename(tmp_file))
    next
  }
  tv <- tryCatch(ncvar_get(nc, "valid_time"), error = function(e) NULL)
  nc_close(nc)

  if (is.null(tv)) {
    message("  No valid_time in: ", basename(tmp_file))
    next
  }

  t0       <- as.POSIXct(min(tv), origin = "1970-01-01", tz = "UTC")
  yr       <- format(t0, "%Y")
  mo       <- format(t0, "%m")
  new_name <- file.path(OUT_DIR, sprintf("era5_%s_%s.nc", yr, mo))

  if (file.exists(new_name)) {
    message(sprintf("  Already have era5_%s_%s.nc — removing duplicate", yr, mo))
    file.remove(tmp_file)
  } else {
    file.rename(tmp_file, new_name)
    message(sprintf("  Saved: era5_%s_%s.nc (%.1f MB)",
                    yr, mo, file.size(new_name) / 1e6))
  }

  Sys.sleep(1)
}

# ── 3. Report coverage ────────────────────────────────────────────────────────

have <- list.files(OUT_DIR, pattern = "era5_[0-9]{4}_[0-9]{2}[.]nc")
have_ym <- sub("era5_(.*)[.]nc", "\\1", have)
all_ym <- apply(expand.grid(yr = 2018:2023, mo = sprintf("%02d", 1:12))[, c(1,2)], 1,
                paste, collapse = "_")
missing <- sort(setdiff(all_ym, have_ym))

message(sprintf("\nHave: %d / 72 months", length(have_ym)))
if (length(missing) > 0) {
  message("Still missing:")
  message(paste(missing, collapse = ", "))
  message("\nRun era5_download.R to fetch these via API")
} else {
  message("All 72 months complete! Run era5_et0.R next.")
}
