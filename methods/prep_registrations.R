#!/usr/bin/env Rscript
# Convert a MS Forms xlsx export into the registrations.csv schema
# consumed by app.R: level,name,email,availability,comments
#
# Usage:
#   Rscript methods/prep_registrations.R <input> [output]
#
# <input> may be:
#   - a local path to the xlsx
#   - a Google Drive share URL (file must be shared "Anyone with the link")
#   - a bare Drive file ID
#
# Default output: data/registrations.csv

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
})

extract_drive_id <- function(s) {
  m <- str_match(s, "/d/([a-zA-Z0-9_-]+)")
  if (!is.na(m[1, 2])) return(m[1, 2])
  m <- str_match(s, "[?&]id=([a-zA-Z0-9_-]+)")
  if (!is.na(m[1, 2])) return(m[1, 2])
  if (str_detect(s, "^[a-zA-Z0-9_-]{20,}$")) return(s)
  NA_character_
}

resolve_input <- function(input) {
  if (file.exists(input)) return(input)
  id <- extract_drive_id(input)
  if (is.na(id)) stop("Input not found as file or recognizable Drive URL/ID: ", input)
  tmp <- tempfile(fileext = ".xlsx")

  if (requireNamespace("googledrive", quietly = TRUE)) {
    message("Downloading from Drive via googledrive (authenticated): ", id)
    googledrive::drive_auth(
      scopes = "https://www.googleapis.com/auth/drive.readonly",
      cache = gargle::gargle_oauth_cache()
    )
    googledrive::drive_download(googledrive::as_id(id), path = tmp, overwrite = TRUE)
    return(tmp)
  }

  message("googledrive not installed; falling back to public download URL.")
  url <- sprintf("https://drive.google.com/uc?export=download&id=%s", id)
  utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
  tmp
}

transform <- function(xlsx_path) {
  raw <- read_excel(xlsx_path)
  pick <- function(pattern) {
    hit <- grep(pattern, colnames(raw), value = TRUE, ignore.case = TRUE)
    if (!length(hit)) stop("Column not found matching: ", pattern)
    hit[1]
  }

  level_col <- pick("^Select your learning level")
  name_col  <- pick("first and last name")
  email_col <- pick("^Please enter your email")
  avail_col <- pick("^In general, which time of day")
  cmt_col   <- pick("comments or questions")

  tibble(
    level        = str_trim(str_remove(raw[[level_col]], regex("\\s*Level\\s*$", ignore_case = TRUE))),
    name         = str_trim(raw[[name_col]]),
    email        = str_trim(raw[[email_col]]),
    availability = str_remove(str_trim(raw[[avail_col]]), ";\\s*$"),
    comments     = ifelse(is.na(raw[[cmt_col]]), "", raw[[cmt_col]])
  )
}

main <- function(args) {
  if (!length(args)) stop("Usage: Rscript methods/prep_registrations.R <input> [output]")
  input  <- args[[1]]
  output <- if (length(args) >= 2) args[[2]] else "data/registrations.csv"

  xlsx <- resolve_input(input)
  out  <- transform(xlsx)
  dir.create(dirname(output), showWarnings = FALSE, recursive = TRUE)
  write.csv(out, output, row.names = FALSE)
  message(sprintf("Wrote %d rows to %s", nrow(out), output))
}

if (sys.nframe() == 0) main(commandArgs(trailingOnly = TRUE))
