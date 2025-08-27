CalltouchCalls <- function (
    dateFrom = Sys.Date() - 31,
    dateTo   = Sys.Date() - 1,
    id       = NULL,
    token    = NULL,
    sleep    = 0.2
) {
  proc_start <- Sys.time()

  if (is.null(id) | is.null(token)) stop("❌ Token or ID is not defined")

  dateFrom <- format.Date(as.Date(dateFrom), "%d/%m/%Y")
  dateTo   <- format.Date(as.Date(dateTo), "%d/%m/%Y")

  base_url <- paste0("https://api.calltouch.ru/calls-service/RestAPI/", id, "/calls-diary/calls")

  # Первый запрос для определения total записей
  resp0 <- request(base_url) |>
    req_url_query(
      clientApiId = token,
      dateFrom = dateFrom,
      dateTo = dateTo,
      page = 1,
      limit = 1
    ) |>
    req_perform()

  if (resp_status(resp0) != 200) stop("❌ Ошибка: HTTP ", resp_status(resp0))

  total <- resp_body_json(resp0, simplifyVector = TRUE)$recordsTotal
  limit <- 1000
  pages <- ceiling(total / limit)

  message("🔎 Найдено записей: ", total, " (страниц: ", pages, ")")

  all_data <- list()

  for (pg in seq_len(pages)) {
    resp <- request(base_url) |>
      req_url_query(
        clientApiId = token,
        dateFrom = dateFrom,
        dateTo   = dateTo,
        page     = pg,
        limit    = limit,
        withCallTags = "true"
      ) |>
      req_perform()

    if (resp_status(resp) == 200) {
      dat <- resp_body_json(resp, simplifyVector = TRUE)
      if (!is.null(dat$records) && length(dat$records) > 0) {
        all_data[[pg]] <- as_tibble(dat$records)
      }
    }

    if (sleep > 0) Sys.sleep(sleep)
  }

  callall <- bind_rows(all_data)

  # Распаковка callTags безопасно
  if ("callTags" %in% names(callall)) {
    callall <- callall %>%
      mutate(callTags = lapply(callTags, function(x) if(is.null(x)) NA else x)) %>%
      unnest_longer(callTags, keep_empty = TRUE) %>%
      unnest_wider(callTags, names_sep = "_")

    # Переименование только если колонка реально существует
    name_cols <- grep("^callTags_names", names(callall), value = TRUE)
    if (length(name_cols) > 0) {
      callall <- rename(callall, callTags = !!sym(name_cols[1]))
    }

    # Удаляем лишние колонки
    drop_cols <- intersect(c("category", "type"), names(callall))
    if (length(drop_cols) > 0) callall <- select(callall, -all_of(drop_cols))
  }

  message("✅ Загружено за ", round(difftime(Sys.time(), proc_start, units = "secs"), 1), " сек.")

  return(callall)
}
