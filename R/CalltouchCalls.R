CalltouchCalls <- function(
    dateFrom = Sys.Date() - 31,
    dateTo   = Sys.Date() - 1,
    id       = NULL,
    token    = NULL,
    sleep    = 0.2
) {
  proc_start <- Sys.time()

  if (is.null(id) || is.null(token)) stop("❌ Token or ID is not defined")

  # Формат дат
  dateFrom <- format(as.Date(dateFrom), "%d/%m/%Y")
  dateTo   <- format(as.Date(dateTo),   "%d/%m/%Y")

  base_url <- paste0("https://api.calltouch.ru/calls-service/RestAPI/", id, "/calls-diary/calls")

  # Первичный запрос для определения количества записей
  resp0 <- httr2::request(base_url) |>
    httr2::req_url_query(
      clientApiId = token,
      dateFrom = dateFrom,
      dateTo   = dateTo,
      page = 1,
      limit = 1
    ) |>
    httr2::req_perform()

  if (httr2::resp_status(resp0) != 200) stop("❌ Ошибка: HTTP ", httr2::resp_status(resp0))

  total <- httr2::resp_body_json(resp0, simplifyVector = TRUE)$recordsTotal
  limit <- 1000
  pages <- ceiling(total / limit)

  message("🔎 Найдено записей: ", total, " (страниц: ", pages, ")")

  # Прогресс-бар
  pb <- cli::cli_progress_bar("Загрузка", total = pages, format = "{cli::pb_bar} {cli::pb_percent} {n}/{total}")

  all_data <- list()

  for (page in 1:pages) {
    resp <- httr2::request(base_url) |>
      httr2::req_url_query(
        clientApiId = token,
        dateFrom = dateFrom,
        dateTo   = dateTo,
        page     = page,
        limit    = limit,
        withCallTags = "true"
      ) |>
      httr2::req_perform()

    if (httr2::resp_status(resp) == 200) {
      dat <- httr2::resp_body_json(resp, simplifyVector = TRUE)
      if (!is.null(dat$records) && length(dat$records) > 0) {
        all_data[[page]] <- tibble::as_tibble(dat$records)
      }
    }

    cli::cli_progress_update(pb, set = page)
    if (sleep > 0) Sys.sleep(sleep)
  }

  cli::cli_progress_done(pb)

  callall <- dplyr::bind_rows(all_data)

  # Безопасная распаковка callTags
  if ("callTags" %in% names(callall)) {
    callall <- callall |>
      tidyr::unnest_longer(callTags, keep_empty = TRUE) |>
      tidyr::unnest_wider(callTags, names_sep = "_")

    drop_cols <- intersect(c("category", "type"), names(callall))
    if (length(drop_cols) > 0) callall <- dplyr::select(callall, -dplyr::all_of(drop_cols))

    name_cols <- grep("^callTags_names", names(callall), value = TRUE)
    if (length(name_cols) > 0) callall <- dplyr::rename(callall, callTags = !!rlang::sym(name_cols[1]))
  }

  # Оставляем только нужные колонки
  keep_cols <- c(
    "date", "city", "uniqueCall", "source", "medium", "duration", "ref",
    "waitingConnect", "ctCallerId", "callbackCall", "keyword", "successful",
    "timestamp", "callId", "utmSource", "callerNumber", "utmTerm",
    "utmCampaign", "url", "phoneNumber", "uniqTargetCall", "targetCall",
    "utmMedium", "hostname"
  )

  callall <- dplyr::select(callall, dplyr::any_of(keep_cols))

  message("✅ Загружено за ", round(difftime(Sys.time(), proc_start, units = "secs"), 1), " сек.")

  return(callall)
}
