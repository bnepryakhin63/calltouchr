CalltouchCalls <- function (
    dateFrom = Sys.Date() - 31 ,
    dateTo   = Sys.Date() - 1,
    id       = NULL,
    token    = NULL,
    sleep    = 0.2
) {
  proc_start <- Sys.time()

  if (is.null(id) | is.null(token)) {
    stop("❌ Token or ID is not defined")
  }

  # Формат дат
  dateFrom <- format.Date(as.Date(dateFrom), "%d/%m/%Y")
  dateTo   <- format.Date(as.Date(dateTo),   "%d/%m/%Y")

  base_url <- paste0("https://api.calltouch.ru/calls-service/RestAPI/", id, "/calls-diary/calls")

  # Первичный запрос
  resp0 <- request(base_url) |>
    req_url_query(
      clientApiId = token,
      dateFrom = dateFrom,
      dateTo = dateTo,
      page = 1,
      limit = 1
    ) |>
    req_perform()

  if (resp_status(resp0) != 200) {
    stop("❌ Ошибка: HTTP ", resp_status(resp0))
  }

  total <- resp_body_json(resp0, simplifyVector = TRUE)$recordsTotal
  limit <- 1000
  pages <- ceiling(total / limit)

  message("🔎 Найдено записей: ", total, " (страниц: ", pages, ")")

  # Прогресс-бар
  pb <- cli_progress_bar("Загрузка", total = pages, format = "{cli::pb_bar} {cli::pb_percent} {n}/{total}")

  all_data <- list()

  for (page in 1:pages) {
    resp <- request(base_url) |>
      req_url_query(
        clientApiId = token,
        dateFrom = dateFrom,
        dateTo   = dateTo,
        page     = page,
        limit    = limit,
        withCallTags = "true"
      ) |>
      req_perform()

    if (resp_status(resp) == 200) {
      dat <- resp_body_json(resp, simplifyVector = TRUE)
      if (!is.null(dat$records) && length(dat$records) > 0) {
        all_data[[page]] <- as_tibble(dat$records)
      }
    }

    cli_progress_update(pb, set = page)

    if (sleep > 0) Sys.sleep(sleep)
  }

  cli_progress_done(pb)

  callall <- bind_rows(all_data)

  # Распаковка тегов (безопасная версия)
  if ("callTags" %in% names(callall)) {
    callall <- callall %>%
      unnest_longer(callTags, keep_empty = TRUE) %>%
      unnest_wider(callTags, names_sep = "_")

    # Удаляем лишние колонки, если они есть
    drop_cols <- intersect(c("category", "type"), names(callall))
    if (length(drop_cols) > 0) {
      callall <- select(callall, -all_of(drop_cols))
    }

    # Переименовываем, если есть поле с именами
    name_cols <- grep("^callTags_names", names(callall), value = TRUE)
    if (length(name_cols) > 0) {
      callall <- rename(callall, callTags = !!sym(name_cols[1]))
    }
  }

  ## Оставляем только нужные поля
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
