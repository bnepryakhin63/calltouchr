
<!-- README.md is generated from README.Rmd. Please edit that file -->

# calltouchr - пакет для работы с API Calltouch

<!-- badges: start -->

<!-- badges: end -->

### Бейджи

[![](https://cranlogs.r-pkg.org/badges/calltouchr)](https://cran.r-project.org/package=calltouchr)

## Краткое описание.

Пакет calltouchr предназначен для загрузки данных из сервиса
колтрекинга Calltouch в R, с помощью функций данного пакета вы
можете работать с перечисленными ниже сервисами и службами API
Calltouch, не углубляясь при этом в документацию по работе с этими API
сервисами.

  - [CalltouchCalls](https://support.calltouch.ru/hc/ru/articles/209231269-Управление-звонками-через-API)
    - Предназначен для получения запросов данных о звонках и их сбора
    статистики по всему ID аккаунту пользователя calltouch. В данной
    функции собираются по умолчанию проставляемые Тэги-разметки из
    журнала звонков. withCallTags=true - по умолчанию

Главная задача пакета - это выгрузка всех данных из ID аккаунта.
Передача данных в calltouch пока не реализована в рамках данного
пакета.

## Установка

Установить dev версию можно из репозитория GitHub, для этого сначала
требуется установить и подключить пакет devtools.

``` r
install.packages("devtools")
devtools::install_github('bnepryakhin63/calltouchr')
```

### Пример использования

В пакете пока реализована только 1 функция, но она полноценно закрывает
все потребности по выгрузке данных по звонкам из calltouch

``` r
Calls <- CalltouchCalls(
                    dateFrom = Sys.Date() - 31 ,
                    dateTo = Sys.Date() - 1,
                    id <- "3****",
                    server <- "https://api-**********.ru/",
                    token <- "e***************aej")
```

dateFrom - Дата начиная с которой должны собираться данные dateTo - Дата
до которой должны собираться данные id - ID из кабинета calltouch server
\<- имя сервера из кабинета calltouch token \<- Токен из кабинета
calltouch

### Ссылки

1.  Баг репорты, предложения по доработке и улучшению функционала
    calltouchr оставлять
    [тут\>\>](https://github.com/bnepryakhin63/calltouchr/issues).
2.  [Список
    релизов](https://github.com/bnepryakhin63/calltouchr/releases).
3.  [Если есть вопросы пишите в telegram](https://t.me/Bogdann63).

### Автор пакета

Непряхин Богдан
