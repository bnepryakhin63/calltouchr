CalltouchCalls <- function (
  dateFrom = Sys.Date() - 31 ,
  dateTo = Sys.Date() - 1,
  id = NULL,
  server = NULL,
  token = NULL)
{
  proc_start <- Sys.time()
  ## Проверка на наличие необходимых параметров
  if (is.null(id) | is.null(token)) {
    stop(
      "Token or ID is not defined (Токен или ID не указаны)"
    )
  }
  ## Настройка опций среды обработки чтобы строки имели класс фактора
  if (getOption("stringsAsFactors") == TRUE) {
    string_as_factor <- "change"
    options(stringsAsFactors = F)
  } else {
    string_as_factor <- "no change"
  }
  ## Задаем единый формат дат для стартовой и конечной даты
  dateFrom <- format.Date(dateFrom, "%d/%m/%Y")
  dateTo <- format.Date(dateTo, "%d/%m/%Y")

  ## Создает первичный GET запрос к API Колтача для того чтобы понять количество страниц с данными
  url <- paste0(server,"calls-service/RestAPI/",id,"/calls-diary/calls?clientApiId=",
                token,"&dateFrom=",dateFrom,"&dateTo=",dateTo,"&page=1&limit=10")
  answer <- GET(url)
  dataRaw <- content(answer)

  ## Парсим полученный ответ
  total <- dataRaw$recordsTotal

  limit <- 12
  page <- total %/% limit
  offset <- total %% limit
  if (offset == 0) page <- page - 1
  result <- data.frame(stringsAsFactors = F)

  ## Создает основной GET запрос к API Колтача для сбора данных
  for (i in 0:page)
  {
    url <- paste0(server,"calls-service/RestAPI/",id,"/calls-diary/calls?clientApiId=",
                  token,"&dateFrom=",dateFrom,"&dateTo=",dateTo,"&page=",i+1,"&limit=",limit)
    answer <- GET(url)
    dataRaw <- content(answer)
    dataRaw <- dataRaw$records
    if (length(dataRaw) > 0)
    {
      column_names <- unlist(lapply(c(names(dataRaw[[1]])), function(x) return(x)))
      rows <- lapply(dataRaw, function(x) return(x))
      rows <- setNames(object = data.frame(do.call(rbind, lapply(rows, as.character, unlist))), nm = names(rows[[1]]))
      rows <- replace(rows, rows=="NULL", NA)
      result <- rbind(result,rows)
    }
    packageStartupMessage(".", appendLF = F)
  }
  return(result)
}
