#' @name app.R
#' @title NYTimes Bestseller AI Reporter — Shiny Dashboard
#' @description
#' Complete AI-powered reporter combining:
#'   1. API Integration  — Queries the NYT Books API for live bestseller data
#'   2. Web Interface    — Interactive Shiny dashboard with filtering, sorting,
#'                         recommendations table, and top-10 bar chart
#'   3. AI Reporting     — Generates trend reports via Ollama (local) or OpenAI
#'
#' Data can be pre-fetched with fetch_bestsellers.py to data/bestsellers.json
#' for faster loading and full list coverage.

# 0. SETUP ##############################################################

## 0.1 Load packages ####################################################

library(shiny)
library(httr2)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(bslib)

## 0.2 Load environment ##################################################

env_path <- c(".env", file.path("..", ".env"))
env_file <- env_path[file.exists(env_path)][1]
if (!is.na(env_file)) {
  readRenviron(env_file)
} else {
  warning("No .env found. Set NYT_API_KEY in .env for API access.")
}

API_KEY            <- Sys.getenv("NYT_API_KEY")
OPENAI_KEY         <- Sys.getenv("OPENAI_API_KEY", "")
OLLAMA_API_KEY     <- Sys.getenv("OLLAMA_API_KEY", "")
OLLAMA_PORT        <- as.integer(Sys.getenv("OLLAMA_PORT", "11434"))
OLLAMA_MODEL       <- Sys.getenv("OLLAMA_MODEL", "smollm2:1.7b")
OLLAMA_CLOUD_MODEL <- Sys.getenv("OLLAMA_CLOUD_MODEL", "gemma3:4b")
BASE_URL           <- "https://api.nytimes.com/svc/books/v3"

# Priority: OpenAI > Ollama Cloud > Ollama Local
AI_BACKEND <- if (nzchar(OPENAI_KEY)) {
  "openai"
} else if (nzchar(OLLAMA_API_KEY)) {
  "ollama_cloud"
} else {
  "ollama_local"
}

## 0.3 API helpers ######################################################

`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x

nyt_get <- function(path, query = list()) {
  query <- c(list("api-key" = API_KEY), query)
  url <- paste0(BASE_URL, "/", path)
  req <- request(url) |>
    req_url_query(!!!query) |>
    req_method("GET")
  resp <- tryCatch(req_perform(req), error = function(e) NULL)
  if (is.null(resp) || resp_status(resp) != 200) return(NULL)
  out <- resp_body_json(resp)
  if (identical(out$status, "OK")) out else NULL
}

get_list_names <- function() {
  out <- nyt_get("lists/names.json")
  if (is.null(out) || length(out$results) == 0) return(NULL)
  results <- out$results
  data.frame(
    list_name_encoded = vapply(results, function(x) x$list_name_encoded %||% "", ""),
    list_name         = vapply(results, function(x) x$list_name %||% x$list_name_encoded %||% "", ""),
    stringsAsFactors  = FALSE
  ) |> filter(nzchar(list_name_encoded), nzchar(list_name))
}

get_current_list <- function(list_name_encoded) {
  if (!nzchar(list_name_encoded)) return(NULL)
  path <- paste0("lists/current/", list_name_encoded, ".json")
  out  <- nyt_get(path)
  if (is.null(out)) return(NULL)
  books <- out$results$books %||% list()
  if (length(books) == 0) return(NULL)
  rows <- lapply(books, book_to_row)
  bind_rows(rows)
}

book_to_row <- function(b) {
  data.frame(
    rank          = as.integer(b$rank %||% NA),
    title         = as.character(b$title %||% ""),
    author        = as.character(b$author %||% ""),
    weeks_on_list = as.integer(b$weeks_on_list %||% NA),
    description   = as.character(b$description %||% ""),
    publisher     = as.character(b$publisher %||% ""),
    stringsAsFactors = FALSE
  )
}

## 0.4 Load cached JSON if available ####################################

bestsellers_json_path <- NULL
for (p in c("data/bestsellers.json")) {
  if (file.exists(p)) { bestsellers_json_path <- p; break }
}

load_bestsellers_file <- function() {
  if (is.null(bestsellers_json_path)) return(NULL)
  out <- tryCatch(jsonlite::read_json(bestsellers_json_path), error = function(e) NULL)
  if (is.null(out) || !is.list(out$lists)) return(NULL)
  out
}

## 0.5 AI helpers ########################################################

build_ai_summary <- function(all_books_df) {
  total   <- nrow(all_books_df)
  n_lists <- length(unique(all_books_df$list_name))
  top1    <- all_books_df |> filter(rank == 1)
  top_wks <- all_books_df |> arrange(desc(weeks_on_list)) |> head(10)

  fmt <- function(df, cols) {
    hdr <- paste(formatC(cols, width = 22, flag = "-"), collapse = " | ")
    sep <- paste(rep("-", nchar(hdr)), collapse = "")
    body <- apply(df[, cols, drop = FALSE], 1, function(r) {
      paste(formatC(as.character(r), width = 22, flag = "-"), collapse = " | ")
    })
    paste(c(hdr, sep, body), collapse = "\n")
  }

  cols_rank <- intersect(c("list_name", "title", "author", "weeks_on_list"), names(top1))
  cols_long <- intersect(c("list_name", "rank", "title", "author", "weeks_on_list"), names(top_wks))

  paste0(
    "NYT Bestseller Data (current week):\n",
    "- Total lists: ", n_lists, "\n",
    "- Total books: ", total, "\n\n",
    "#1 Ranked Books by List:\n",
    fmt(top1, cols_rank), "\n\n",
    "Top 10 Books by Weeks on List (longest-running bestsellers):\n",
    fmt(top_wks, cols_long), "\n"
  )
}

build_ai_prompt <- function() {
  today <- format(Sys.Date(), "%B %d, %Y")
  paste0(
    "You are a book industry analyst writing a weekly NYT Bestseller briefing. ",
    "Start the report with this exact title line:\n",
    "Weekly NYT Bestseller Briefing \u2013 ", today, "\n\n",
    "Then write 6-8 bullet points covering:\n",
    "1. \U0001F4CA Key trends across the bestseller lists this week\n",
    "2. \U0001F31F Which books have the longest staying power and why that matters\n",
    "3. \U0001F4DA Notable patterns in genres, publishers, or authors\n",
    "4. \U0001F4A1 One actionable recommendation for a reader looking for their next book\n\n",
    "Use emojis at the start of each bullet point to make the report lively and scannable. ",
    "Use **bold** (double asterisks) for emphasis — never use *italics* (single asterisks). ",
    "Use clear, professional but friendly language. ",
    "Do NOT end with questions, offers for follow-up, or suggestions to the reader. ",
    "End the report cleanly after the last bullet point."
  )
}

call_ollama_local <- function(data_summary) {
  url  <- paste0("http://localhost:", OLLAMA_PORT, "/api/generate")
  body <- list(
    model  = OLLAMA_MODEL,
    prompt = paste0(build_ai_prompt(), "\n\nDATA:\n", data_summary),
    stream = FALSE
  )
  resp <- request(url) |>
    req_method("POST") |>
    req_body_json(body) |>
    req_timeout(180) |>
    req_perform()
  if (resp_status(resp) != 200) stop("Ollama Local returned status ", resp_status(resp))
  resp_body_json(resp)$response
}

call_ollama_cloud <- function(data_summary) {
  url <- "https://ollama.com/api/chat"
  body <- list(
    model = OLLAMA_CLOUD_MODEL,
    messages = list(
      list(role = "system", content = build_ai_prompt()),
      list(role = "user",   content = paste0("Here is the current data:\n\n", data_summary))
    ),
    stream = FALSE
  )
  resp <- request(url) |>
    req_method("POST") |>
    req_headers(
      Authorization  = paste("Bearer", OLLAMA_API_KEY),
      `Content-Type` = "application/json"
    ) |>
    req_body_json(body) |>
    req_timeout(120) |>
    req_perform()
  if (resp_status(resp) != 200) stop("Ollama Cloud returned status ", resp_status(resp))
  resp_body_json(resp)$message$content
}

call_openai <- function(data_summary) {
  url <- "https://api.openai.com/v1/chat/completions"
  body <- list(
    model = "gpt-4o-mini",
    messages = list(
      list(role = "system", content = build_ai_prompt()),
      list(role = "user",   content = paste0("Here is the current data:\n\n", data_summary))
    ),
    temperature = 0.7,
    max_tokens  = 1024
  )
  resp <- request(url) |>
    req_method("POST") |>
    req_headers(
      Authorization  = paste("Bearer", OPENAI_KEY),
      `Content-Type` = "application/json"
    ) |>
    req_body_json(body) |>
    req_timeout(60) |>
    req_perform()
  if (resp_status(resp) != 200) stop("OpenAI returned status ", resp_status(resp))
  resp_body_json(resp)$choices[[1]]$message$content
}

call_ai <- function(data_summary) {
  switch(AI_BACKEND,
    openai       = call_openai(data_summary),
    ollama_cloud = call_ollama_cloud(data_summary),
    ollama_local = call_ollama_local(data_summary)
  )
}


# 1. UI #################################################################

ui <- fluidPage(
  title = "NYTimes Bestseller AI Reporter",
  theme = bs_theme(
    bootswatch   = "flatly",
    base_font    = font_google("Quicksand"),
    heading_font = font_google("Dancing Script"),
    bg        = "#FFF0F3",
    fg        = "#4A2040",
    primary   = "#D1729B",
    secondary = "#E8A0BF"
  ),
  tags$head(tags$style(HTML("
    @import url('https://fonts.googleapis.com/css2?family=Lora:ital,wght@0,400;0,600;1,400&family=Quicksand:wght@400;600;700&family=Dancing+Script:wght@600;700&display=swap');

    body { font-family: 'Quicksand', sans-serif; background: #FFF0F3; color: #4A2040; }

    /* --- layout --- */
    .screen { min-height: 100vh; display: flex; flex-direction: column; box-sizing: border-box; background: #FFF0F3; }
    .screen * { box-sizing: border-box; }
    .screen-top { flex: 2 1 0; min-height: 0; display: flex; flex-direction: row; overflow: hidden; padding: 0.5rem 0; }
    .screen-bottom { flex: 1 1 0; min-height: 0; overflow: hidden; padding: 0.5rem 0; }
    .left-section  { flex: 0 0 36%; display: flex; flex-direction: column; gap: 0.5rem; padding-right: 0.5rem; min-height: 0; overflow: hidden; }
    .right-section { flex: 1 1 0; display: flex; flex-direction: column; min-height: 0; padding-left: 0.5rem; overflow: hidden; }

    /* --- cards --- */
    .app-card { background: #FFFFFF; border-radius: 14px; box-shadow: 0 3px 12px rgba(209,114,155,0.15); border: 1px solid #F2D0E0; display: flex; flex-direction: column; }
    .card-title { flex: 0 0 auto; justify-content: center; align-items: center; text-align: center; padding: 1.2rem 1rem !important; background: linear-gradient(135deg, #FADADD 0%, #F9E4EC 100%); border-radius: 14px 14px 0 0; }
    .card-title h2 { margin: 0; color: #9B3D6E; font-weight: 700; font-size: 1.5rem; font-family: 'Dancing Script', cursive; }
    .card-filter { flex: 1 1 0; min-height: 0; padding: 1.25rem !important; display: flex; flex-direction: column; justify-content: center; align-items: center; text-align: center; overflow: hidden; }
    .card-filter .filters-stack { width: 100%; max-width: 280px; }
    .card-filter .filters-stack .form-group { margin-bottom: 1rem; text-align: left; }
    .card-filter .filters-stack label { font-weight: 600; font-size: 0.95rem; color: #4A2040; }
    .selectize-dropdown, .selectize-dropdown-content { max-height: 280px !important; overflow-y: auto !important; }
    .selectize-input { border-color: #F2D0E0 !important; }

    /* --- table card --- */
    .card-table { flex: 1 1 0; min-height: 0; display: flex; flex-direction: column; overflow: hidden; padding: 0.75rem 0.5rem; }
    .card-table h5 { color: #9B3D6E; font-family: 'Dancing Script', cursive; font-weight: 700; }
    .table-scroll-wrapper { flex: 1; min-height: 100px; overflow-y: auto; overflow-x: auto; -webkit-overflow-scrolling: touch; }
    .card-table table { width: 100%; margin: 0; table-layout: fixed; }
    .card-table table thead th { background: #D1729B !important; color: #FFFFFF !important; font-weight: 700; padding: 0.6rem 0.75rem; border-color: #C4608A; }
    .card-table table thead th:nth-child(1), .card-table table tbody td:nth-child(1) { width: 8%; }
    .card-table table thead th:nth-child(2), .card-table table tbody td:nth-child(2) { width: 30%; }
    .card-table table thead th:nth-child(3), .card-table table tbody td:nth-child(3) { width: 25%; }
    .card-table table thead th:nth-child(4), .card-table table tbody td:nth-child(4) { width: 10%; }
    .card-table table thead th:nth-child(5), .card-table table tbody td:nth-child(5) { width: 27%; }
    .card-table table tbody td { border-color: #F9E4EC; color: #4A2040; }
    .card-table table tbody tr:nth-child(even) { background: #FFF7FA; }

    /* --- graph card --- */
    .card-graph { height: 100%; min-height: 0; padding: 1rem; overflow: hidden; display: flex; flex-direction: column; background: linear-gradient(135deg, #F9D5E5 0%, #F4B8D0 50%, #EDAFCA 100%) !important; border: 1px solid #E8A0BF !important; }
    .card-graph h5 { color: #7A2D50; font-family: 'Dancing Script', cursive; font-weight: 700; }
    .card-graph > div { flex: 1; min-height: 0; }

    /* --- AI report tab --- */
    .ai-section { padding: 1rem 0; }
    .ai-controls-card { background: #FFFFFF; border-radius: 14px; box-shadow: 0 3px 12px rgba(209,114,155,0.15); border: 1px solid #F2D0E0; padding: 1.5rem; margin-bottom: 1.2rem; }
    .ai-controls-card h4 { color: #9B3D6E; font-weight: 700; margin-top: 0; font-family: 'Dancing Script', cursive; font-size: 1.4rem; }
    .ai-controls-card p { color: #6B3A5A; font-size: 0.95rem; }

    .ai-report-card { background: #FFFFFF; border-radius: 14px; box-shadow: 0 3px 12px rgba(209,114,155,0.15); border: 1px solid #F2D0E0; padding: 1.5rem; margin-bottom: 1rem; }

    .paper-page {
      background: #FFFDF9;
      border: 1px solid #E8D8C4;
      border-radius: 4px;
      padding: 2.5rem 3rem;
      margin: 0.8rem 0;
      box-shadow: 2px 3px 12px rgba(0,0,0,0.06), 0 0 0 1px rgba(0,0,0,0.02);
      font-family: 'Lora', 'Georgia', serif;
      position: relative;
      min-height: 200px;
    }
    .paper-page::before {
      content: '';
      position: absolute;
      top: 0; left: 45px; bottom: 0;
      width: 1px;
      background: #F4CCCC;
      opacity: 0.5;
    }
    .paper-page h4 {
      font-family: 'Lora', 'Georgia', serif;
      color: #7A2D50;
      font-weight: 600;
      font-size: 1.35rem;
      margin-top: 0;
      margin-bottom: 1rem;
      padding-bottom: 0.6rem;
      border-bottom: 1px solid #F2D0E0;
    }
    .paper-page .ai-report-text {
      font-family: 'Lora', 'Georgia', serif;
      font-size: 1.02rem;
      line-height: 1.85;
      color: #3D1F33;
      text-align: justify;
      hyphens: auto;
      -webkit-hyphens: auto;
    }

    .btn-generate { background: linear-gradient(135deg, #D1729B 0%, #E8A0BF 100%); color: #FFFFFF; border: none; font-weight: 700; padding: 0.65rem 1.8rem; border-radius: 25px; font-size: 1rem; cursor: pointer; letter-spacing: 0.5px; transition: all 0.2s; }
    .btn-generate:hover { background: linear-gradient(135deg, #B85D85 0%, #D1729B 100%); color: #FFFFFF; transform: translateY(-1px); box-shadow: 0 4px 12px rgba(209,114,155,0.35); }
    .btn-export { background: linear-gradient(135deg, #E8A0BF 0%, #F2C4D6 100%); color: #7A2D50; border: none; font-weight: 700; padding: 0.6rem 1.6rem; border-radius: 25px; font-size: 0.95rem; cursor: pointer; transition: all 0.2s; }
    .btn-export:hover { background: linear-gradient(135deg, #D1729B 0%, #E8A0BF 100%); color: #FFFFFF; transform: translateY(-1px); }

    /* --- tabs --- */
    .nav-tabs { border-bottom: 2px solid #F2D0E0; }
    .nav-tabs .nav-link { color: #9B3D6E; font-weight: 600; border: none; padding: 0.6rem 1.2rem; border-radius: 10px 10px 0 0; }
    .nav-tabs .nav-link:hover { background: #FFF7FA; color: #7A2D50; }
    .nav-tabs .nav-link.active { background: #FFFFFF; color: #9B3D6E; border: 1px solid #F2D0E0; border-bottom: 2px solid #FFFFFF; margin-bottom: -2px; }
  "))),

  # Top-level tab navigation
  tags$div(
    style = "padding: 0.5rem 1rem;",
    tabsetPanel(
      id = "main_tabs",

      # ---- TAB 1: DASHBOARD ----
      tabPanel(
        "Bestseller Dashboard",
        tags$div(
          class = "screen",
          tags$div(
            class = "screen-top",
            tags$div(
              class = "left-section",
              tags$div(class = "app-card card-title",
                tags$h2("\U0001F338 NYTimes Bestseller Recommendations \U0001F338")
              ),
              tags$div(
                class = "app-card card-filter",
                tags$h5("\U0001F3AF Filters", class = "mt-0 mb-3 text-muted"),
                tags$div(
                  class = "filters-stack",
                  selectInput("genre", "Bestseller list",
                    choices = c("Choose a list..." = ""), selected = "", width = "100%"),
                  selectInput("filterby", "Sort by",
                    choices = c(
                      "Rank (1-15)"               = "rank_asc",
                      "Weeks on list (most first)" = "weeks_desc",
                      "Weeks on list (least first)" = "weeks_asc",
                      "Title A-Z"                  = "title_asc",
                      "Author A-Z"                 = "author_asc"
                    ), selected = "rank_asc", width = "100%")
                )
              )
            ),
            tags$div(
              class = "right-section",
              tags$div(
                class = "app-card card-table",
                tags$h5("\U0001F4D6 Recommendations", class = "mt-0 mb-3"),
                tags$div(class = "table-scroll-wrapper", tableOutput("table_out"))
              )
            )
          ),
          tags$div(
            class = "screen-bottom",
            tags$div(
              class = "app-card card-graph",
              tags$h5("\U0001F4C8 Top 10 \u2014 Weeks on list (this list)", class = "mt-0 mb-3"),
              plotOutput("plot_out", height = "280px")
            )
          )
        )
      ),

      # ---- TAB 2: AI REPORT ----
      tabPanel(
        "AI Report",
        tags$div(
          class = "ai-section",
          tags$div(
            class = "ai-controls-card",
            tags$h4("Generate Bestseller Books Report"),
            tags$p("Get a curated weekly briefing on what's trending across the NYT Bestseller lists ",
                   "\u2014 top-ranked titles, longest-running fan favorites, standout authors, and a ",
                   "personalized reading pick just for you."),
            tags$div(
              style = "display: flex; align-items: center; gap: 1rem; margin-bottom: 0.5rem;",
              actionButton("btn_ai", "GENERATE REPORT", class = "btn-generate"),
              conditionalPanel(
                condition = "output.ai_loading",
                tags$span(style = "color: #D1729B; font-weight: 600;", "\u2728 Brewing your report, hang tight...")
              )
            )
          ),
          tags$div(
            class = "ai-report-card",
            tags$div(class = "paper-page", htmlOutput("ai_report_out")),
            tags$div(style = "text-align: right; margin-top: 0.8rem;",
              downloadButton("download_report", "Export Report as PDF", class = "btn-export")
            )
          ),
          tags$div(
            class = "ai-report-card",
            tags$div(style = "text-align: center; padding: 0.5rem 0;",
              tags$h4("\U0001F370 Books by Bestseller Category",
                      style = "color: #9B3D6E; font-family: 'Dancing Script', cursive; font-weight: 700;"),
              plotOutput("pie_chart", height = "420px", width = "100%")
            )
          )
        )
      )
    )
  )
)


# 2. SERVER #############################################################

server <- function(input, output, session) {

  # ---- Shared data ----

  bestsellers_cached <- reactiveVal(load_bestsellers_file())
  list_options       <- reactiveVal(NULL)

  # All books across all lists (for AI report)
  all_books <- reactiveVal(data.frame())

  load_lists <- function() {
    cached <- bestsellers_cached()
    if (!is.null(cached) && length(cached$lists) > 0) {
      opts <- do.call(rbind, lapply(cached$lists, function(l) {
        data.frame(
          list_name_encoded = as.character(l$list_name_encoded %||% ""),
          list_name         = as.character(l$list_name %||% ""),
          stringsAsFactors  = FALSE
        )
      }))
      opts <- opts |> filter(nzchar(list_name_encoded), nzchar(list_name))
      list_options(opts)

      all_rows <- do.call(rbind, lapply(cached$lists, function(l) {
        books <- l$books %||% list()
        if (length(books) == 0) return(NULL)
        rows <- lapply(books, function(b) {
          r <- book_to_row(b)
          r$list_name <- as.character(l$list_name %||% "")
          r
        })
        do.call(rbind, rows)
      }))
      if (!is.null(all_rows)) all_books(all_rows)
    } else {
      list_options(get_list_names())
    }
  }

  observe({ load_lists() })

  observeEvent(list_options(), {
    opts <- list_options()
    if (is.null(opts) || nrow(opts) == 0) {
      updateSelectInput(session, "genre",
        choices = c("(No lists found - run fetch_bestsellers.py or check NYT_API_KEY)" = ""))
      return(invisible())
    }
    ch <- setNames(opts$list_name_encoded, opts$list_name)
    updateSelectInput(session, "genre", choices = ch, selected = ch[1])
  }, ignoreNULL = TRUE)

  # ---- Dashboard: book data for selected genre ----

  rec_raw <- reactive({
    enc <- input$genre
    if (is.null(enc) || !nzchar(enc)) return(NULL)
    cached <- bestsellers_cached()
    if (!is.null(cached) && length(cached$lists) > 0) {
      lst <- Find(function(l) identical(l$list_name_encoded, enc), cached$lists)
      if (is.null(lst)) return(NULL)
      books <- lst$books %||% list()
      if (length(books) == 0) {
        return(data.frame(rank = integer(), title = character(), author = character(),
                          weeks_on_list = integer(), description = character(),
                          publisher = character(), stringsAsFactors = FALSE))
      }
      do.call(rbind, lapply(books, book_to_row))
    } else {
      get_current_list(enc)
    }
  })

  rec_sorted <- reactive({
    data <- rec_raw()
    if (is.null(data)) return(NULL)
    fb <- input$filterby
    if (is.null(fb) || !nzchar(fb)) fb <- "rank_asc"
    switch(fb,
      rank_asc   = data |> arrange(rank),
      weeks_desc = data |> arrange(desc(weeks_on_list), rank),
      weeks_asc  = data |> arrange(weeks_on_list, rank),
      title_asc  = data |> arrange(title),
      author_asc = data |> arrange(author),
      data |> arrange(rank)
    )
  })

  output$table_out <- renderTable({
    data <- rec_sorted()
    if (is.null(data)) {
      return(data.frame(Message = "Select a bestseller list. If the dropdown is empty, run fetch_bestsellers.py or set NYT_API_KEY."))
    }
    if (nrow(data) == 0) {
      return(data.frame(Message = "No bestsellers in this category this week."))
    }
    data |>
      select(rank, title, author, weeks_on_list, publisher) |>
      rename(Rank = rank, Title = title, Author = author,
             `Weeks on list` = weeks_on_list, Publisher = publisher)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  output$plot_out <- renderPlot({
    data <- rec_sorted()
    if (is.null(data)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Select a bestseller list above.", cex = 1.1)
      return(invisible())
    }
    if (nrow(data) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No bestsellers in this category this week.", cex = 1.1)
      return(invisible())
    }
    top10 <- data |> slice_head(n = 10) |>
      mutate(title_short = substr(title, 1, 40))
    pink_tints <- colorRampPalette(c("#FADADD", "#F4B8D0", "#E88DBF", "#D1729B", "#B8508A", "#9B3D6E"))(nrow(top10))
    top10$bar_color <- pink_tints[rank(top10$weeks_on_list, ties.method = "first")]

    ggplot(top10, aes(x = reorder(title_short, weeks_on_list), y = weeks_on_list, fill = bar_color)) +
      geom_col(width = 0.7, color = "#FFFFFF", linewidth = 0.3) +
      scale_fill_identity() +
      coord_flip() +
      labs(x = NULL, y = "Weeks on list", title = NULL) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position      = "none",
        panel.grid.major.y   = element_blank(),
        panel.grid.minor     = element_blank(),
        text                 = element_text(color = "#4A2040", family = "sans"),
        axis.text            = element_text(color = "#7A2D50", face = "bold"),
        plot.background      = element_rect(fill = "transparent", color = NA),
        panel.background     = element_rect(fill = "transparent", color = NA)
      )
  })

  # ---- Pie chart (AI Report tab) ----

  output$pie_chart <- renderPlot({
    ab <- all_books()
    if (is.null(ab) || nrow(ab) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No data loaded yet.", cex = 1.1, col = "#9B3D6E")
      return(invisible())
    }

    counts <- ab |>
      group_by(list_name) |>
      summarise(n = n(), .groups = "drop") |>
      filter(n > 0) |>
      arrange(desc(n))

    pink_pal <- colorRampPalette(c(
      "#FADADD", "#F9C6D9", "#F4B8D0", "#EDA4C4",
      "#E88DBF", "#D1729B", "#C05A8A", "#9B3D6E"
    ))(nrow(counts))

    counts$pct <- counts$n / sum(counts$n) * 100
    counts$label <- paste0(counts$list_name, "\n(", counts$n, " books)")

    ggplot(counts, aes(x = "", y = n, fill = reorder(list_name, -n))) +
      geom_col(width = 1, color = "#FFFFFF", linewidth = 0.6) +
      coord_polar(theta = "y") +
      scale_fill_manual(values = pink_pal) +
      labs(fill = NULL) +
      theme_void(base_size = 13) +
      theme(
        legend.text      = element_text(color = "#4A2040", size = 10),
        legend.position  = "right",
        legend.key.size  = unit(0.45, "cm"),
        plot.background  = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA)
      )
  }, bg = "transparent")

  # ---- AI Report tab ----

  ai_loading  <- reactiveVal(FALSE)
  ai_report   <- reactiveVal("Press 'Generate Report' to create an AI-powered bestseller analysis.")

  output$ai_loading <- reactive({ ai_loading() })
  outputOptions(output, "ai_loading", suspendWhenHidden = FALSE)

  observeEvent(input$btn_ai, {
    ab <- all_books()
    if (is.null(ab) || nrow(ab) == 0) {
      ai_report("No data available. Make sure data/bestsellers.json exists (run fetch_bestsellers.py).")
      return()
    }

    ai_loading(TRUE)
    ai_report("Generating report...")

    tryCatch({
      summary_text <- build_ai_summary(ab)
      report_text  <- call_ai(summary_text)
      ai_report(report_text)

      out_path <- file.path("data", "ai_report.txt")
      dir.create("data", showWarnings = FALSE)
      backend_label <- switch(AI_BACKEND,
        openai       = "OpenAI (gpt-4o-mini)",
        ollama_cloud = paste0("Ollama Cloud (", OLLAMA_CLOUD_MODEL, ")"),
        ollama_local = paste0("Ollama Local (", OLLAMA_MODEL, ")")
      )
      writeLines(c(
        "AI-Generated NYT Bestseller Report",
        paste("Backend:", backend_label),
        paste(rep("=", 40), collapse = ""),
        "", report_text
      ), out_path)
    },
    error = function(e) {
      msg <- conditionMessage(e)
      hint <- switch(AI_BACKEND,
        ollama_local = paste0("\n\nMake sure Ollama is running (ollama serve) with model '", OLLAMA_MODEL,
                              "', or set OLLAMA_API_KEY / OPENAI_API_KEY in .env."),
        ollama_cloud = "\n\nCheck that your OLLAMA_API_KEY in .env is valid.",
        openai       = "\n\nCheck that your OPENAI_API_KEY in .env is valid."
      )
      ai_report(paste0("Error generating report: ", msg, hint))
    })

    ai_loading(FALSE)
  })

  output$ai_report_out <- renderUI({
    report <- ai_report()
    if (is.null(report) || !nzchar(report)) return(NULL)

    lines <- strsplit(report, "\n")[[1]]
    first_non_empty <- which(nzchar(trimws(lines)))[1]

    if (!is.na(first_non_empty) && first_non_empty <= length(lines)) {
      title_line <- trimws(lines[first_non_empty])
      title_line <- sub("^#+ *", "", title_line)
      body_lines <- lines[-first_non_empty]
      body_text  <- paste(body_lines, collapse = "\n")
    } else {
      title_line <- ""
      body_text  <- report
    }

    has_real_report <- !grepl("^Press 'Generate", report) && !grepl("^Generating report", report)

    md_to_html <- function(txt) {
      txt <- gsub("\\*\\*(.+?)\\*\\*", "<strong>\\1</strong>", txt)
      txt <- gsub("__(.+?)__", "<strong>\\1</strong>", txt)
      # Convert any remaining italics to bold for consistency
      txt <- gsub("\\*(.+?)\\*", "<strong>\\1</strong>", txt)
      txt <- gsub("_(.+?)_", "<strong>\\1</strong>", txt)
      txt <- gsub("\n", "<br/>", txt)
      txt
    }

    tagList(
      if (has_real_report && nzchar(title_line)) {
        tags$h4(HTML(md_to_html(title_line)), style = "color: #7A2D50; font-weight: 700; margin-top: 0;")
      },
      tags$div(class = "ai-report-text",
        HTML(md_to_html(if (has_real_report) body_text else report))
      )
    )
  })

  # ---- PDF export ----

  output$download_report <- downloadHandler(
    filename = function() {
      paste0("NYT_Bestseller_Report_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      report <- ai_report()
      if (is.null(report) || !nzchar(report) || grepl("^Press 'Generate", report)) {
        report <- "No report generated yet. Click 'Generate Report' first."
      }

      pdf(file, width = 8.5, height = 11, family = "Helvetica")
      on.exit(dev.off(), add = TRUE)

      raw_lines <- strsplit(report, "\n")[[1]]
      all_lines <- unlist(lapply(raw_lines, function(l) {
        if (nzchar(trimws(l))) strwrap(l, width = 90) else ""
      }))

      lines_per_page <- 40
      line_h <- 0.021

      new_page <- function(is_first = FALSE) {
        plot.new()
        par(mar = c(1, 1, 1, 1))
        plot.window(xlim = c(0, 1), ylim = c(0, 1))
        if (is_first) {
          text(0.5, 0.97, "NYT Bestseller AI Report", cex = 1.8, font = 2, family = "Helvetica")
          text(0.5, 0.935, format(Sys.Date(), "%B %d, %Y"), cex = 1.0, col = "gray40")
          segments(0.08, 0.915, 0.92, 0.915, col = "gray50", lwd = 0.8)
          return(0.89)
        }
        return(0.96)
      }

      y_top <- new_page(is_first = TRUE)
      line_count <- 0

      for (l in all_lines) {
        if (line_count >= lines_per_page) {
          y_top <- new_page()
          line_count <- 0
        }
        y <- y_top - line_count * line_h
        is_bullet <- grepl("^\\s*[-*]\\s", l) || grepl("^\\s*\\d+[\\.)]\\s", l)
        text(0.08, y, l, adj = c(0, 1), cex = 0.72,
             family = "Helvetica", font = if (is_bullet) 1 else 1)
        line_count <- line_count + 1
      }

      text(0.5, 0.02, "Generated by NYTimes Bestseller AI Reporter",
           cex = 0.6, col = "gray50", family = "Helvetica")
    }
  )
}


# 3. RUN ################################################################

shinyApp(ui = ui, server = server)
