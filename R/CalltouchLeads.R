CalltouchLeads2 <- function(
    dateFrom = NULL,
    dateTo = NULL,
    id = NULL,
    token = NULL
) {
  proc_start <- Sys.time()  # Сохраняем время начала выполнения

  # Проверка обязательных параметров
  if (is.null(id) || is.null(token)) {
    stop("Token или ID не определены")
  }

  # Преобразуем даты в нужный формат
  dateFrom <- format(as.Date(dateFrom), "%m/%d/%Y")
  dateTo <- format(as.Date(dateTo), "%m/%d/%Y")

  # Формируем URL для запроса
  url <- paste0(
    "https://api.calltouch.ru/calls-service/RestAPI/requests/?clientApiId=", token,
    "&siteId=", id,
    "&dateFrom=", dateFrom,
    "&dateTo=", dateTo,
    "&withMapVisits=true"
  )

  message("Запрос к Calltouch: ", url)

  # Выполняем GET-запрос
  response <- GET(url)
  stop_for_status(response)  # Прерываем выполнение, если ошибка

  # Получаем ответ в виде списка
  dataRaw <- content(response, as = "parsed", simplifyVector = TRUE)

  # Преобразуем в tibble
  leads_df <- as_tibble(dataRaw)

  # Извлекаем данные из вложенного объекта client
  if ("client" %in% names(leads_df)) {
    leads_df <- leads_df %>%
      mutate(
        client_id = tryCatch(client$clientId, error = function(e) NA),
        client_fio = tryCatch(client$fio, error = function(e) NA),
        client_phone = sapply(client$phones, function(x) {
          if (is.data.frame(x) && ncol(x) > 0) x[1, 1] else NA
        })
      )
  }

  # Извлекаем данные из вложенного объекта mapVisits
  if ("mapVisits" %in% names(leads_df)) {
    leads_df <- leads_df %>%
      mutate(
        visit_source = sapply(mapVisits, function(x) {
          if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) NA else x$source[1]
        }),
        visit_medium = sapply(mapVisits, function(x) {
          if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) NA else x$medium[1]
        }),
        visit_utm_campaign = sapply(mapVisits, function(x) {
          if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) NA else x$utmCampaign[1]
        }),
        date_only = tryCatch(as.Date(substr(dateStr, 1, 10), format = "%d/%m/%Y"), error = function(e) NA),
        time_only = tryCatch(substr(dateStr, 12, 19), error = function(e) NA)
      )
  }

  # Удаляем ненужные вложенные колонки
  leads_df <- leads_df %>% select(-any_of(c("client", "mapVisits")))

  # Сообщаем о времени выполнения
  message("Получение данных завершено за ", round(difftime(Sys.time(), proc_start, units = "secs"), 2), " сек.")

  return(leads_df)
}
