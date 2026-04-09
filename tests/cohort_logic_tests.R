library(testthat)
library(dplyr)
library(tidyr)

# ── Helpers extracted from app.R ─────────────────────────────────────────────
# These mirror the logic in app.R so tests run without launching Shiny.

parse_time_slots <- function(time_string) {
  if (is.na(time_string) || time_string == "") {
    return(character(0))
  }
  slots <- strsplit(time_string, ";")[[1]]
  trimws(slots[slots != ""])
}

expand_availability <- function(data) {
  data %>%
    rowwise() %>%
    mutate(slots = list(parse_time_slots(availability))) %>%
    unnest(slots) %>%
    filter(slots != "")
}

find_cohorts <- function(survey_data, max_size = 25, min_size = 10) {
  expanded <- expand_availability(survey_data)
  all_people <- unique(survey_data$email)
  assigned <- character(0)
  cohort_list <- list()
  almost_list <- list()
  cohort_num <- 1

  while (length(assigned) < length(all_people)) {
    unassigned <- setdiff(all_people, assigned)
    unassigned_avail <- expanded %>% filter(email %in% unassigned)

    if (nrow(unassigned_avail) == 0) {
      break
    }

    best <- unassigned_avail %>%
      group_by(slots) %>%
      summarise(count = n(), .groups = "drop") %>%
      arrange(desc(count)) %>%
      slice(1)

    best_slot <- best$slots
    best_count <- best$count

    participants <- unassigned_avail %>%
      filter(slots == best_slot) %>%
      select(name, email, level) %>%
      distinct()

    if (best_count >= min_size) {
      participants <- participants %>% slice(1:min(n(), max_size))

      cohort_list[[cohort_num]] <- list(
        cohort_id = cohort_num,
        time_slot = best_slot,
        count = nrow(participants),
        participants = participants,
        status = "viable"
      )

      assigned <- c(assigned, participants$email)
      cohort_num <- cohort_num + 1
    } else {
      almost_list[[best_slot]] <- list(
        time_slot = best_slot,
        count = best_count,
        needed = min_size - best_count,
        participants = participants
      )

      assigned <- c(assigned, participants$email)
    }
  }

  list(cohorts = cohort_list, almost_cohorts = almost_list)
}

# ── Fixtures ──────────────────────────────────────────────────────────────────

make_registrants <- function(n, slot) {
  data.frame(
    level = rep("Intermediate", n),
    name = paste0("Person", seq_len(n)),
    email = paste0("person", seq_len(n), "@example.com"),
    availability = slot,
    comments = "",
    stringsAsFactors = FALSE
  )
}

sample_data <- read.csv(
  here::here("data", "sample_registrations.csv"),
  stringsAsFactors = FALSE
)

# ── parse_time_slots ──────────────────────────────────────────────────────────

test_that("parse_time_slots handles a single slot", {
  result <- parse_time_slots("Morning, 8am - 10am")
  expect_equal(result, "Morning, 8am - 10am")
})

test_that("parse_time_slots splits multiple semicolon-separated slots", {
  result <- parse_time_slots("Morning, 8am - 10am;Lunch, 12pm - 2pm")
  expect_equal(result, c("Morning, 8am - 10am", "Lunch, 12pm - 2pm"))
})

test_that("parse_time_slots returns empty character for NA", {
  expect_equal(parse_time_slots(NA), character(0))
})

test_that("parse_time_slots returns empty character for empty string", {
  expect_equal(parse_time_slots(""), character(0))
})

test_that("parse_time_slots trims whitespace around slot names", {
  result <- parse_time_slots("  Morning, 8am - 10am ; Evening, 4pm - 6pm  ")
  expect_equal(result, c("Morning, 8am - 10am", "Evening, 4pm - 6pm"))
})

test_that("parse_time_slots handles all five valid time slots", {
  all_slots <- paste(
    "Morning, 8am - 10am",
    "Morning, 10am - 12pm",
    "Lunch, 12pm - 2pm",
    "Afternoon, 2pm - 4pm",
    "Evening, 4pm - 6pm",
    sep = ";"
  )
  result <- parse_time_slots(all_slots)
  expect_length(result, 5)
})

# ── expand_availability ───────────────────────────────────────────────────────

test_that("expand_availability produces one row per slot per person", {
  df <- data.frame(
    name = c("A", "B"),
    email = c("a@x.com", "b@x.com"),
    level = c("Beginner", "Advanced"),
    availability = c(
      "Morning, 8am - 10am;Lunch, 12pm - 2pm",
      "Evening, 4pm - 6pm"
    ),
    comments = c("", ""),
    stringsAsFactors = FALSE
  )
  expanded <- expand_availability(df)
  expect_equal(nrow(expanded), 3)
})

test_that("expand_availability drops rows with empty availability", {
  df <- data.frame(
    name = "Ghost",
    email = "ghost@x.com",
    level = "Beginner",
    availability = "",
    comments = "",
    stringsAsFactors = FALSE
  )
  expanded <- expand_availability(df)
  expect_equal(nrow(expanded), 0)
})

# ── find_cohorts: assignment completeness ─────────────────────────────────────

test_that("every registrant is assigned when cohorts are viable", {
  df <- make_registrants(15, "Lunch, 12pm - 2pm")
  result <- find_cohorts(df, max_size = 25, min_size = 10)
  assigned <- unlist(lapply(result$cohorts, function(x) x$participants$email))
  expect_setequal(assigned, df$email)
})

test_that("registrants without a viable cohort land in almost_cohorts", {
  df <- make_registrants(5, "Evening, 4pm - 6pm")
  result <- find_cohorts(df, max_size = 25, min_size = 10)
  almost_assigned <- unlist(lapply(result$almost_cohorts, function(x) {
    x$participants$email
  }))
  expect_setequal(almost_assigned, df$email)
  expect_length(result$cohorts, 0)
})

test_that("no person appears in more than one cohort", {
  df <- make_registrants(30, "Morning, 10am - 12pm")
  result <- find_cohorts(df, max_size = 25, min_size = 10)
  all_assigned <- unlist(lapply(result$cohorts, function(x) {
    x$participants$email
  }))
  expect_equal(length(all_assigned), length(unique(all_assigned)))
})

# ── find_cohorts: size constraints ───────────────────────────────────────────

test_that("no viable cohort exceeds max_size", {
  df <- make_registrants(40, "Afternoon, 2pm - 4pm")
  result <- find_cohorts(df, max_size = 15, min_size = 5)
  sizes <- sapply(result$cohorts, function(x) x$count)
  expect_true(all(sizes <= 15))
})

test_that("almost_cohorts record correct 'needed' count", {
  df <- make_registrants(6, "Morning, 8am - 10am")
  result <- find_cohorts(df, max_size = 25, min_size = 10)
  expect_length(result$almost_cohorts, 1)
  expect_equal(result$almost_cohorts[[1]]$needed, 4)
})

test_that("a cohort is viable at exactly min_size", {
  df <- make_registrants(10, "Lunch, 12pm - 2pm")
  result <- find_cohorts(df, max_size = 25, min_size = 10)
  expect_length(result$cohorts, 1)
  expect_equal(result$cohorts[[1]]$count, 10)
})

# ── find_cohorts: multi-slot behaviour ───────────────────────────────────────

test_that("algorithm selects the slot with the most unassigned people first", {
  big_group <- make_registrants(12, "Lunch, 12pm - 2pm")
  small_group <- make_registrants(3, "Evening, 4pm - 6pm")
  df <- bind_rows(big_group, small_group)
  result <- find_cohorts(df, max_size = 25, min_size = 10)

  first_slot <- result$cohorts[[1]]$time_slot
  expect_equal(first_slot, "Lunch, 12pm - 2pm")
})

test_that("people available in multiple slots are not double-counted", {
  # Person available in two slots — should appear in exactly one cohort
  overlap <- data.frame(
    level = "Intermediate",
    name = "Overlap Person",
    email = "overlap@example.com",
    availability = "Lunch, 12pm - 2pm;Evening, 4pm - 6pm",
    comments = "",
    stringsAsFactors = FALSE
  )
  others <- make_registrants(12, "Lunch, 12pm - 2pm")
  df <- bind_rows(overlap, others)
  result <- find_cohorts(df, max_size = 25, min_size = 10)

  all_emails <- unlist(lapply(result$cohorts, function(x) x$participants$email))
  overlap_count <- sum(all_emails == "overlap@example.com")
  expect_equal(overlap_count, 1)
})

# ── Sample data integration test ──────────────────────────────────────────────

test_that("sample CSV loads with correct columns", {
  expect_true(all(
    c("level", "name", "email", "availability", "comments") %in%
      names(sample_data)
  ))
})

test_that("sample CSV level values are valid", {
  valid_levels <- c("Beginner", "Intermediate", "Advanced")
  expect_true(all(sample_data$level %in% valid_levels))
})

test_that("sample CSV has no missing names or emails", {
  expect_false(any(is.na(sample_data$name) | sample_data$name == ""))
  expect_false(any(is.na(sample_data$email) | sample_data$email == ""))
})

test_that("find_cohorts runs without error on sample data", {
  expect_no_error(find_cohorts(sample_data, max_size = 25, min_size = 5))
})

test_that("all sample registrants are assigned on sample data", {
  result <- find_cohorts(sample_data, max_size = 25, min_size = 5)
  assigned <- c(
    unlist(lapply(result$cohorts, function(x) x$participants$email)),
    unlist(lapply(result$almost_cohorts, function(x) x$participants$email))
  )
  expect_setequal(assigned, sample_data$email)
})
