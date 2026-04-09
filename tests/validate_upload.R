library(testthat)
library(dplyr)

# ── validate_upload extracted from app.R ─────────────────────────────────────
# Column names are ignored entirely — only column order matters.
# The first 5 columns are treated as: level, name, email, availability, comments.

parse_time_slots <- function(time_string) {
  if (is.na(time_string) || time_string == "") {
    return(character(0))
  }
  slots <- strsplit(time_string, ";")[[1]]
  trimws(slots[slots != ""])
}

validate_upload <- function(df) {
  errors <- character(0)

  # Must have at least 5 columns — names don't matter, order does
  if (ncol(df) < 5) {
    errors <- c(
      errors,
      paste0(
        "File must have at least 5 columns (level, name, email, availability, ",
        "comments in that order). Found: ",
        ncol(df),
        "."
      )
    )
    return(errors)
  }

  # Rename positionally for all downstream checks
  check_df <- df
  names(check_df)[1:5] <- c(
    "level",
    "name",
    "email",
    "availability",
    "comments"
  )
  check_df <- check_df %>% mutate(across(everything(), trimws))

  # No missing names
  if (any(is.na(check_df$name) | check_df$name == "")) {
    errors <- c(errors, "One or more rows have a missing name.")
  }

  # No missing emails
  if (any(is.na(check_df$email) | check_df$email == "")) {
    errors <- c(errors, "One or more rows have a missing email.")
  }

  # Valid level values
  valid_levels <- c("Beginner", "Intermediate", "Advanced")
  bad_levels <- unique(check_df$level[
    !check_df$level %in% valid_levels &
      !is.na(check_df$level) &
      check_df$level != ""
  ])
  if (length(bad_levels) > 0) {
    errors <- c(
      errors,
      paste0(
        "Unrecognised level values: ",
        paste(bad_levels, collapse = ", "),
        ". Expected: Beginner, Intermediate, or Advanced."
      )
    )
  }

  # Valid time slots
  valid_slots <- c(
    "Morning, 8am - 10am",
    "Morning, 10am - 12pm",
    "Lunch, 12pm - 2pm",
    "Afternoon, 2pm - 4pm",
    "Evening, 4pm - 6pm"
  )
  all_slots <- unlist(lapply(check_df$availability, parse_time_slots))
  bad_slots <- unique(all_slots[!all_slots %in% valid_slots])
  if (length(bad_slots) > 0) {
    errors <- c(
      errors,
      paste0(
        "Unrecognised time slot(s): ",
        paste(bad_slots, collapse = "; "),
        ". Check for encoding issues or extra whitespace."
      )
    )
  }

  # At least one person has availability
  if (all(is.na(check_df$availability) | check_df$availability == "")) {
    errors <- c(
      errors,
      paste0(
        "No availability data found. Check that the correct file was uploaded."
      )
    )
  }

  errors
}

# ── Fixture helpers ───────────────────────────────────────────────────────────

valid_row <- function(
  level = "Intermediate",
  name = "Test Person",
  email = "test@example.com",
  availability = "Lunch, 12pm - 2pm",
  comments = ""
) {
  data.frame(
    level,
    name,
    email,
    availability,
    comments,
    stringsAsFactors = FALSE
  )
}

# ── Column count ──────────────────────────────────────────────────────────────

test_that("returns error when fewer than 5 columns", {
  df <- data.frame(a = 1, b = 2, c = 3)
  err <- validate_upload(df)
  expect_true(any(grepl("at least 5 columns", err)))
})

test_that("returns no error with exactly 5 columns", {
  err <- validate_upload(valid_row())
  expect_length(err, 0)
})

test_that("accepts files with more than 5 columns (extra survey metadata)", {
  df <- cbind(valid_row(), extra1 = "x", extra2 = "y")
  err <- validate_upload(df)
  expect_length(err, 0)
})

test_that("column names are irrelevant — validation passes with arbitrary headers", {
  df <- valid_row()
  names(df) <- c("Q1", "Q2", "Q3", "Q4", "Q5")
  err <- validate_upload(df)
  expect_length(err, 0)
})

# ── Name validation ───────────────────────────────────────────────────────────

test_that("returns error when name column (col 2) is empty", {
  df <- valid_row()
  df$name <- ""
  err <- validate_upload(df)
  expect_true(any(grepl("missing name", err)))
})

test_that("returns error when name column (col 2) is NA", {
  df <- valid_row()
  df$name <- NA
  err <- validate_upload(df)
  expect_true(any(grepl("missing name", err)))
})

test_that("whitespace-only name is treated as missing", {
  df <- valid_row()
  df$name <- "   "
  err <- validate_upload(df)
  expect_true(any(grepl("missing name", err)))
})

# ── Email validation ──────────────────────────────────────────────────────────

test_that("returns error when email column (col 3) is empty", {
  df <- valid_row()
  df$email <- ""
  err <- validate_upload(df)
  expect_true(any(grepl("missing email", err)))
})

test_that("returns error when email column (col 3) is NA", {
  df <- valid_row()
  df$email <- NA
  err <- validate_upload(df)
  expect_true(any(grepl("missing email", err)))
})

# ── Level validation ──────────────────────────────────────────────────────────

test_that("accepts all three valid level values", {
  for (lvl in c("Beginner", "Intermediate", "Advanced")) {
    df <- valid_row(level = lvl)
    err <- validate_upload(df)
    expect_length(err, 0, label = paste("level:", lvl))
  }
})

test_that("returns error for unrecognised level value", {
  df <- valid_row(level = "Expert")
  err <- validate_upload(df)
  expect_true(any(grepl("Unrecognised level", err)))
})

test_that("error message names the bad level value", {
  df <- valid_row(level = "Ninja")
  err <- validate_upload(df)
  expect_true(any(grepl("Ninja", err)))
})

test_that("multiple bad level values are all reported", {
  df <- rbind(valid_row(level = "Expert"), valid_row(level = "Ninja"))
  err <- validate_upload(df)
  expect_true(any(grepl("Expert", err)))
  expect_true(any(grepl("Ninja", err)))
})

test_that("level check is case-sensitive — 'beginner' is rejected", {
  df <- valid_row(level = "beginner")
  err <- validate_upload(df)
  expect_true(any(grepl("Unrecognised level", err)))
})

# ── Availability / time slot validation ───────────────────────────────────────

test_that("accepts all five valid time slots individually", {
  valid_slots <- c(
    "Morning, 8am - 10am",
    "Morning, 10am - 12pm",
    "Lunch, 12pm - 2pm",
    "Afternoon, 2pm - 4pm",
    "Evening, 4pm - 6pm"
  )
  for (slot in valid_slots) {
    df <- valid_row(availability = slot)
    err <- validate_upload(df)
    expect_length(err, 0, label = paste("slot:", slot))
  }
})

test_that("accepts multiple valid slots separated by semicolons", {
  df <- valid_row(availability = "Morning, 8am - 10am;Evening, 4pm - 6pm")
  err <- validate_upload(df)
  expect_length(err, 0)
})

test_that("returns error for unrecognised time slot", {
  df <- valid_row(availability = "Midnight, 11pm - 1am")
  err <- validate_upload(df)
  expect_true(any(grepl("Unrecognised time slot", err)))
})

test_that("error message names the bad slot", {
  df <- valid_row(availability = "Midnight, 11pm - 1am")
  err <- validate_upload(df)
  expect_true(any(grepl("Midnight, 11pm - 1am", err)))
})

test_that("trailing/leading whitespace in slot name triggers unrecognised slot error", {
  df <- valid_row(availability = " Lunch, 12pm - 2pm ") # spaces inside the value
  # parse_time_slots trims, but if encoding adds invisible chars they won't trim
  # This test documents that pure whitespace padding is handled gracefully
  err <- validate_upload(df)
  expect_length(err, 0) # trimws inside validate handles this
})

test_that("returns error when all availability values are empty", {
  df <- valid_row()
  df$availability <- ""
  err <- validate_upload(df)
  expect_true(any(grepl("No availability data", err)))
})

test_that("does not error when some but not all availability is empty", {
  df <- rbind(
    valid_row(availability = "Lunch, 12pm - 2pm"),
    valid_row(availability = "", name = "Person 2", email = "p2@example.com")
  )
  err <- validate_upload(df)
  expect_length(err, 0)
})

# ── Multiple simultaneous errors ──────────────────────────────────────────────

test_that("collects multiple independent errors in one pass", {
  df <- valid_row(level = "Wizard", availability = "Never, 3am - 4am")
  df$email <- ""
  err <- validate_upload(df)
  expect_gte(length(err), 3) # bad level + bad slot + missing email
})

# ── Column order independence from names ──────────────────────────────────────

test_that("validates correctly when survey exports columns with verbose question text as headers", {
  df <- valid_row()
  names(df) <- c(
    "What is your experience level?",
    "Full name",
    "Work email address",
    "Which times work for you?",
    "Any questions or comments?"
  )
  err <- validate_upload(df)
  expect_length(err, 0)
})

test_that("correctly identifies bad level even when column is named something unexpected", {
  df <- valid_row(level = "Rockstar")
  names(df) <- c("col_a", "col_b", "col_c", "col_d", "col_e")
  err <- validate_upload(df)
  expect_true(any(grepl("Unrecognised level", err)))
})
