# Rename CDS UI downloads (hash-named .nc) to era5_YYYY_MM.nc
# Reads valid_time from each file, detects year-month, renames or removes duplicates
# Run from WEP_Submission/ directory

library(ncdf4)

OUT_DIR <- "./Data/ERA5/monthly"

all_files <- list.files(OUT_DIR, pattern = "\\.nc$", full.names = TRUE)
hash_files <- all_files[!grepl("era5_[0-9]{4}_[0-9]{2}\\.nc$", all_files)]

message(sprintf("Hash-named files to process: %d", length(hash_files)))

renamed  <- 0
dupes    <- 0
failures <- 0

for (f in hash_files) {
  nc <- tryCatch(nc_open(f), error = function(e) NULL)
  if (is.null(nc)) {
    message("  CANNOT OPEN: ", basename(f))
    failures <- failures + 1
    next
  }

  tv <- tryCatch(ncvar_get(nc, "valid_time"), error = function(e) NULL)
  nc_close(nc)

  if (is.null(tv) || length(tv) == 0) {
    message("  NO valid_time: ", basename(f))
    failures <- failures + 1
    next
  }

  t0  <- as.POSIXct(min(tv), origin = "1970-01-01", tz = "UTC")
  yr  <- format(t0, "%Y")
  mo  <- format(t0, "%m")
  new <- file.path(OUT_DIR, sprintf("era5_%s_%s.nc", yr, mo))

  if (file.exists(new)) {
    message(sprintf("  DUPE era5_%s_%s.nc — removing %s", yr, mo, basename(f)))
    file.remove(f)
    dupes <- dupes + 1
  } else {
    file.rename(f, new)
    message(sprintf("  RENAMED -> era5_%s_%s.nc", yr, mo))
    renamed <- renamed + 1
  }
}

# Report coverage
have    <- list.files(OUT_DIR, pattern = "era5_[0-9]{4}_[0-9]{2}\\.nc$")
have_ym <- sub("era5_([0-9]{4}_[0-9]{2})\\.nc", "\\1", have)
all_ym  <- apply(expand.grid(yr = 2018:2023, mo = sprintf("%02d", 1:12))[, c(1,2)], 1,
                 paste, collapse = "_")
missing <- sort(setdiff(all_ym, have_ym))

message(sprintf("\n=== Summary ==="))
message(sprintf("Renamed:  %d", renamed))
message(sprintf("Dupes removed: %d", dupes))
message(sprintf("Failures: %d", failures))
message(sprintf("Have: %d / 72 months", length(have_ym)))
if (length(missing) > 0) {
  message("Still missing:")
  message(paste(missing, collapse = ", "))
  message("\nRun era5_download.R to fetch these")
} else {
  message("All 72 months complete! Run era5_et0.R next.")
}
