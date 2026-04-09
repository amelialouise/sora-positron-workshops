library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(DT)
library(ggplot2)

ui <- page_sidebar(
  title = "Workshop Cohort Builder",
  sidebar = sidebar(
    fileInput("file", "Upload Survey CSV", accept = ".csv"),
    hr(),
    checkboxInput(
      "exclude_completed",
      "Exclude completed attendees",
      value = FALSE
    ),
    conditionalPanel(
      condition = "input.exclude_completed == true",
      textAreaInput(
        "completed_emails",
        "Completed attendee emails (one per line):",
        rows = 5,
        placeholder = "email1@example.com\nemail2@example.com"
      )
    ),
    hr(),
    numericInput(
      "cohort_size",
      "Maximum cohort size:",
      value = 20,
      min = 1,
      max = 25
    ),
    numericInput(
      "min_cohort_size",
      "Minimum cohort size:",
      value = 10,
      min = 5,
      max = 10
    ),
    actionButton(
      "find_cohorts",
      "Find Optimal Cohorts",
      class = "btn-primary w-100"
    ),
    hr(),
    downloadButton(
      "download_cohorts",
      "Download Cohort Assignments",
      class = "btn-success w-100"
    )
  ),

  navset_card_tab(
    nav_panel(
      "Data Overview",
      card(
        card_header("Uploaded Registrants"),
        DTOutput("registrants_table")
      )
    ),

    nav_panel(
      "Availability Heatmap",
      card(
        card_header("Time Slot Availability"),
        plotOutput("availability_heatmap", height = "500px")
      )
    ),

    nav_panel(
      "Suggested Cohorts",
      layout_columns(
        col_widths = c(8, 4),
        div(
          style = "max-width: 1200px;",
          uiOutput("cohort_cards")
        ),
        card(
          card_header("Optimal Cohorts = Everyone Assigned"),
          uiOutput("cohort_summary")
        )
      )
    ),

    nav_panel(
      "Questions & Comments",
      card(
        card_header("Participant Questions and Comments"),
        p("Copy and paste these questions/comments to share with presenters:"),
        verbatimTextOutput("comments_output")
      )
    ),

    nav_panel(
      "Instructions",
      card(
        card_header("How to Use This App"),
        markdown(
          "
### Step 1: Upload Data
- Upload your survey CSV file using the file input in the sidebar
- The app will automatically parse the relevant columns

### Step 2: Exclude Completed Attendees (Optional)
- Check the 'Exclude completed attendees' box
- Paste email addresses of people who have already completed the training (one per line)

### Step 3: Find Cohorts
- Set your maximum cohort size (default: 25)
- Click 'Find Optimal Cohorts' to generate groupings
- The app ensures everyone is assigned to a cohort

### Step 4: Review and Download
- Review suggested cohorts in the 'Suggested Cohorts' tab
- Download the cohort assignments as a CSV for your records

### Step 5: Review Questions
- Check the 'Questions & Comments' tab to see participant questions
- Copy and paste these to share with presenters

### Note on Privacy
- No data is stored on the server
- All processing happens in your browser session
- Download results and save them to SORA's private Google Drive
        "
        )
      )
    )
  )
)

server <- function(input, output, session) {
  parse_time_slots <- function(time_string) {
    if (is.na(time_string) || time_string == "") {
      return(character(0))
    }
    slots <- strsplit(time_string, ";")[[1]]
    trimws(slots[slots != ""])
  }

  survey_data <- reactive({
    req(input$file)

    df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    names(df) <- trimws(names(df))

    col_names <- names(df)

    data <- df %>%
      select(
        level = tail(col_names, 5)[1],
        name = tail(col_names, 5)[2],
        email = tail(col_names, 5)[3],
        availability = tail(col_names, 5)[4],
        comments = tail(col_names, 5)[5]
      ) %>%
      mutate(across(everything(), trimws)) %>%
      filter(!is.na(name) & name != "" & !is.na(email) & email != "")

    if (
      input$exclude_completed &&
        !is.null(input$completed_emails) &&
        input$completed_emails != ""
    ) {
      completed <- trimws(strsplit(input$completed_emails, "\n")[[1]])
      completed <- completed[completed != ""]
      data <- data %>% filter(!(email %in% completed))
    }

    data
  })

  expanded_data <- reactive({
    req(survey_data())

    survey_data() %>%
      rowwise() %>%
      mutate(slots = list(parse_time_slots(availability))) %>%
      unnest(slots) %>%
      filter(slots != "")
  })

  output$registrants_table <- renderDT({
    req(survey_data())

    survey_data() %>%
      select(
        Name = name,
        Email = email,
        Level = level,
        Availability = availability
      ) %>%
      datatable(
        options = list(pageLength = 20, scrollX = TRUE),
        rownames = FALSE
      )
  })

  output$availability_heatmap <- renderPlot({
    req(expanded_data())

    time_order <- c(
      "Morning, 8am - 10am",
      "Morning, 10am - 12pm",
      "Lunch, 12pm - 2pm",
      "Afternoon, 2pm - 4pm",
      "Evening, 4pm - 6pm"
    )

    slot_counts <- expanded_data() %>%
      group_by(slots) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(slots = factor(slots, levels = rev(time_order)))

    ggplot(slot_counts, aes(x = count, y = slots, fill = count)) +
      geom_col() +
      geom_text(aes(label = count), hjust = -0.2, size = 5) +
      scale_fill_gradient(low = "#e3f2fd", high = "#1976d2") +
      labs(
        title = "Number of Registrants Available per Time Slot",
        x = "Number of Available Participants",
        y = "Time Slot"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none") +
      xlim(0, max(slot_counts$count) * 1.15)
  })

  output$comments_output <- renderText({
    req(survey_data())

    comments_data <- survey_data() %>%
      filter(!is.na(comments) & comments != "") %>%
      select(name, comments)

    if (nrow(comments_data) == 0) {
      return("No questions or comments submitted.")
    }

    paste0(
      seq_len(nrow(comments_data)),
      ". ",
      comments_data$name,
      ":\n   ",
      comments_data$comments,
      collapse = "\n\n"
    )
  })

  cohorts <- eventReactive(input$find_cohorts, {
    req(survey_data(), expanded_data())

    max_size <- input$cohort_size
    min_size <- input$min_cohort_size
    all_people <- unique(survey_data()$email)
    assigned_people <- character(0)
    cohort_list <- list()
    almost_cohort_list <- list()
    cohort_num <- 1

    while (length(assigned_people) < length(all_people)) {
      unassigned <- setdiff(all_people, assigned_people)
      unassigned_avail <- expanded_data() %>% filter(email %in% unassigned)

      if (nrow(unassigned_avail) == 0) {
        break
      }

      best_slot_info <- unassigned_avail %>%
        group_by(slots) %>%
        summarise(count = n(), .groups = "drop") %>%
        arrange(desc(count)) %>%
        slice(1)

      best_slot <- best_slot_info$slots
      best_count <- best_slot_info$count

      cohort_participants <- unassigned_avail %>%
        filter(slots == best_slot) %>%
        select(name, email, level) %>%
        distinct()

      if (best_count >= min_size) {
        cohort_participants <- cohort_participants %>%
          slice(1:min(n(), max_size))

        cohort_list[[cohort_num]] <- list(
          cohort_id = cohort_num,
          time_slot = best_slot,
          count = nrow(cohort_participants),
          participants = cohort_participants,
          status = "viable"
        )

        assigned_people <- c(assigned_people, cohort_participants$email)
        cohort_num <- cohort_num + 1
      } else {
        almost_cohort_list[[best_slot]] <- list(
          time_slot = best_slot,
          count = best_count,
          needed = min_size - best_count,
          participants = cohort_participants
        )

        assigned_people <- c(assigned_people, cohort_participants$email)
      }
    }

    list(cohorts = cohort_list, almost_cohorts = almost_cohort_list)
  })

  output$cohort_summary <- renderUI({
    req(cohorts(), survey_data())

    total_people <- nrow(survey_data())
    total_cohorts <- length(cohorts()$cohorts)
    assigned_people <- sum(sapply(cohorts()$cohorts, function(x) x$count))
    almost_assigned <- sum(sapply(cohorts()$almost_cohorts, function(x) {
      x$count
    }))

    card(
      card_header("Summary", class = "bg-primary text-white"),
      h4(
        paste0(
          total_people,
          " registrants distributed across ",
          total_cohorts,
          " viable cohort",
          if (total_cohorts != 1) "s" else ""
        ),
        class = "text-center my-3"
      ),
      if (length(cohorts()$almost_cohorts) > 0) {
        p(
          paste0(
            almost_assigned,
            " registrant",
            if (almost_assigned != 1) "s" else "",
            " in time slots that need more sign-ups (scroll for details)"
          ),
          class = "text-center text-warning"
        )
      }
    )
  })

  output$cohort_cards <- renderUI({
    req(cohorts())

    viable_uis <- lapply(seq_along(cohorts()$cohorts), function(i) {
      cohort <- cohorts()$cohorts[[i]]
      card(
        card_header(
          class = "bg-success-subtle",
          strong(paste0(
            "Cohort ",
            cohort$cohort_id,
            ": ",
            cohort$time_slot,
            ". Total Participants: ",
            cohort$count
          ))
        ),
        DTOutput(paste0("cohort_table_", i))
      )
    })

    almost_uis <- lapply(seq_along(cohorts()$almost_cohorts), function(i) {
      cohort <- cohorts()$almost_cohorts[[i]]
      card(
        card_header(
          class = "bg-warning-subtle",
          strong(paste0("\u26a0\ufe0f Almost-Viable: ", cohort$time_slot))
        ),
        p(
          paste0(
            "Current: ",
            cohort$count,
            " registrants | Need: ",
            cohort$needed,
            " more to reach minimum of ",
            input$min_cohort_size
          ),
          class = "text-warning fw-bold"
        ),
        DTOutput(paste0("almost_cohort_table_", i))
      )
    })

    do.call(tagList, c(viable_uis, almost_uis))
  })

  observe({
    req(cohorts())

    lapply(seq_along(cohorts()$cohorts), function(i) {
      output[[paste0("cohort_table_", i)]] <- renderDT({
        cohorts()$cohorts[[i]]$participants %>%
          select(Name = name, Email = email, Level = level) %>%
          datatable(
            options = list(
              pageLength = 15,
              dom = "tip",
              paging = TRUE,
              scrollY = "800px",
              scrollCollapse = FALSE
            ),
            rownames = FALSE
          )
      })
    })

    lapply(seq_along(cohorts()$almost_cohorts), function(i) {
      output[[paste0("almost_cohort_table_", i)]] <- renderDT({
        cohorts()$almost_cohorts[[i]]$participants %>%
          select(Name = name, Email = email, Level = level) %>%
          datatable(
            options = list(pageLength = 5, dom = "ti", paging = FALSE),
            rownames = FALSE
          )
      })
    })
  })

  output$download_cohorts <- downloadHandler(
    filename = function() paste0("workshop_cohorts_", Sys.Date(), ".csv"),
    content = function(file) {
      req(cohorts())

      bind_rows(lapply(seq_along(cohorts()$cohorts), function(i) {
        cohort <- cohorts()$cohorts[[i]]
        cohort$participants %>%
          mutate(
            cohort_number = cohort$cohort_id,
            time_slot = cohort$time_slot,
            cohort_size = cohort$count
          )
      })) %>%
        select(cohort_number, time_slot, cohort_size, name, email, level) %>%
        write.csv(file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
