CalltouchCalls <- function(
    dateFrom = Sys.Date() - 31,
    dateTo   = Sys.Date() - 1,
    id       = NULL,
    token    = NULL,
    sleep    = 0.2
) {
  proc_start <- Sys.time()

  if (is.null(id) | is.null(token)) stop("Token or ID is not defined")

  dateFrom <- format.Date(as.Date(dateFrom), "%d/%m/%Y")
  dateTo   <- format.Date(as.Date(dateTo), "%d/%m/%Y")

  base_url <- paste0("https://api.calltouch.ru/calls-service/RestAPI/", id, "/calls-diary/calls")

  # первичный запрос для получения общего числа записей
  resp0 <- request(base_url) |>
    req_url_query(
      clientApiId = token,
      dateFrom = dateFrom,
      dateTo = dateTo,
      page = 1,
      limit = 1
    ) |>
    req_perform()

  if (resp_status(resp0) != 200) stop("HTTP error: ", resp_status(resp0))

  total <- resp_body_json(resp0, simplifyVector = TRUE)$recordsTotal
  limit <- 1000
  pages <- ceiling(total / limit)
  message("🔎 Найдено записей: ", total, " (страниц: ", pages, ")")

  callall <- list()

  for (i in 1:pages) {
    resp <- request(base_url) |>
      req_url_query(
        clientApiId = token,
        dateFrom = dateFrom,
        dateTo   = dateTo,
        page     = i,
        limit    = limit,
        withCallTags = "true"
      ) |>
      req_perform()

    dat <- resp_body_json(resp, simplifyVector = TRUE)

    if (!is.null(dat$records) && length(dat$records) > 0) {
      # Преобразуем в tibble и добавляем в список
      callall[[i]] <- as_tibble(dat$records, .name_repair = "unique")
    }

    if (sleep > 0) Sys.sleep(sleep)
  }

  # объединяем все страницы
  callall <- bind_rows(callall)

  # Обрабатываем callTags безопасно
  if ("callTags" %in% names(callall)) {
    callall <- callall %>%
      mutate(callTags = lapply(callTags, function(x) {
        if (is.null(x)) return(list())
        if (is.list(x) && length(x) > 0) return(x)
        list(x)
      }))
  }

  # Оставляем только нужные столбцы
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
