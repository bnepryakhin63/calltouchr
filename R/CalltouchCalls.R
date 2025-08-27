CalltouchCalls <- function(
    dateFrom = Sys.Date() - 31,
    dateTo   = Sys.Date() - 1,
    id       = NULL,
    token    = NULL,
    sleep    = 0.2
) {
  if (is.null(id) || is.null(token)) stop("❌ Token or ID is not defined")

  # Разбиваем период на месяцы, чтобы не перегружать API
  seq_months <- seq(as.Date(dateFrom), as.Date(dateTo), by = "month")
  if (tail(seq_months, 1) != as.Date(dateTo)) seq_months <- c(seq_months, as.Date(dateTo))

  all_data <- list()
  proc_start <- Sys.time()

  for (i in 1:(length(seq_months) - 1)) {
    start <- seq_months[i]
    end   <- seq_months[i + 1] - 1
    if (end > as.Date(dateTo)) end <- as.Date(dateTo)

    dateFrom_fmt <- format(start, "%d/%m/%Y")
    dateTo_fmt   <- format(end, "%d/%m/%Y")

    base_url <- paste0("https://api.calltouch.ru/calls-service/RestAPI/", id, "/calls-diary/calls")

    # Первый запрос, чтобы узнать total
    resp0 <- request(base_url) |>
      req_url_query(
        clientApiId = token,
        dateFrom = dateFrom_fmt,
        dateTo   = dateTo_fmt,
        page     = 1,
        limit    = 1
      ) |>
      req_perform()

    if (resp_status(resp0) != 200) stop("❌ Ошибка HTTP ", resp_status(resp0))

    total <- resp_body_json(resp0, simplifyVector = TRUE)$recordsTotal
    limit <- 1000
    pages <- ceiling(total / limit)
    message("🔎 Загружено период: ", start, " - ", end, " | записей: ", total, " (страниц: ", pages, ")")

    # Прогресс-бар
    pb <- cli_progress_bar("Загрузка", total = pages, format = "{cli::pb_bar} {cli::pb_percent} {n}/{total}")

    for (page in 1:pages) {
      resp <- request(base_url) |>
        req_url_query(
          clientApiId = token,
          dateFrom = dateFrom_fmt,
          dateTo   = dateTo_fmt,
          page     = page,
          limit    = limit,
          withCallTags = "true"
        ) |>
        req_perform()

      if (resp_status(resp) == 200) {
        dat <- resp_body_json(resp, simplifyVector = TRUE)
        if (!is.null(dat$records) && length(dat$records) > 0) {
          tmp <- as_tibble(dat$records)

          # Безопасная распаковка callTags
          if ("callTags" %in% names(tmp)) {
            tmp$callTags <- lapply(tmp$callTags, function(x) {
              if (is.list(x)) paste(unlist(x), collapse = ";")
              else if (is.atomic(x)) as.character(x)
              else ""
            })
          }

          all_data <- c(all_data, list(tmp))
        }
      }

      cli_progress_update(pb, set = page)
      if (sleep > 0) Sys.sleep(sleep)
    }

    cli_progress_done(pb)
  }

  # Объединяем все данные
  callall <- bind_rows(all_data)

  # Оставляем только нужные колонки
  keep_cols <- c(
    "date", "city", "uniqueCall", "source", "medium", "duration", "ref",
    "waitingConnect", "ctCallerId", "callbackCall", "keyword", "successful",
    "timestamp", "callId", "utmSource", "callerNumber", "utmTerm",
    "utmCampaign", "url", "phoneNumber", "uniqTargetCall", "targetCall",
    "utmMedium", "hostname", "callTags"
  )

  callall <- select(callall, any_of(keep_cols))

  message("✅ Общие записи загружены за ", round(difftime(Sys.time(), proc_start, units = "secs"), 1), " сек.")

  return(callall)
}
