CalltouchCalls <- function (
  dateFrom = Sys.Date() - 31 ,
  dateTo = Sys.Date() - 1,
  id = NULL,
  token = NULL)
{
  proc_start <- Sys.time()
  ## Проверка на наличие необходимых параметров
  if (is.null(id) | is.null(token)) {
    stop(
      "Token or ID is not defined"
    )
  }
  ## Задаем единый формат дат для стартовой и конечной даты
  dateFrom <- format.Date(dateFrom, "%d/%m/%Y")
  dateTo <- format.Date(dateTo, "%d/%m/%Y")

  ## Создает первичный GET запрос к API Колтача для того чтобы понять количество страниц с данными
  url <- paste0("https://api.calltouch.ru/calls-service/RestAPI/",id,"/calls-diary/calls?clientApiId=",
                token,"&dateFrom=",dateFrom,"&dateTo=",dateTo,"&page=1&limit=10")
  answer <- GET(url)
  dataRaw <- content(answer)

  ## Парсим полученный ответ
  total <- dataRaw$recordsTotal

  limit <- 1000
  page <- total %/% limit
  offset <- total %% limit
  if (offset == 0) page <- page - 1
  result <- data.frame(stringsAsFactors = F)
  callall <- list()
  ## Создает основной GET запрос к API Колтача для сбора данных
  for (i in 0:page)
  {
    url2 <-paste0("https://api.calltouch.ru/calls-service/RestAPI/",id,"/calls-diary/calls?clientApiId=",
                  token,"&dateFrom=",dateFrom,"&dateTo=",dateTo,"&page=",i+1,"&limit=",limit,"&withCallTags=true")
    answer <- GET(url2)
    dataRaw <- content(answer)
    staff_dict <- tibble(firstdatac = dataRaw)

    callall <- c(callall, list(staff_dict[5,] %>%
                                 unnest_longer(firstdatac) %>%
                                 unnest_wider(firstdatac)
    ))

  }
  callall <- bind_rows(callall)

  try(callall <- callall %>%
        unnest_longer(callTags)%>%
        unnest_wider(callTags)%>%
        select(-category, -type)%>%
        unnest_longer(names)%>%
        rename(callTags=names), silent = T)

  return(callall)
}
