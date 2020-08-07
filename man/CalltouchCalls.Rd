\name{CalltouchCalls}
\alias{CalltouchCalls}
\title{Downloads data on calls from calltouch.ru}
\description{all available fields with data on calls for the selected date are unloaded. Using API calltouch from official site https://support.calltouch.ru/hc/ru/categories/360000847232}
\usage{
CalltouchCalls(
dateFrom = Sys.Date() - 31 ,
dateTo = Sys.Date() - 1,
id = NULL,
server = NULL,
token = NULL)
}

\arguments{
  \item{dateFrom}{Start date of data collection}
  \item{dateTo}{End date of data collection}
  \item{id}{account ID API calltouch}
  \item{server}{server name in account API calltouch}
  \item{token}{secret token in account API calltouch}
}
\details{}
\value{Returns data frame with many columns
}
\references{}
\author{Bogdan Nepryakhin}
\note{}

\seealso{}
\examples{
Calls <- CalltouchCalls(
                    dateFrom = Sys.Date() - 31 ,
                    dateTo = Sys.Date() - 1,
                    id <- "3****",
                    server <- "https://api-**********.ru/",
                    token <- "e***************aej")
}
