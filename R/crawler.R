#' getAirlineCode
#'
#' Get Airline Codes
#'
#' @param origin the code of your original airport
#' @param destination the code of your destination airport
#' @examples
#' getAirlineCode("TPE","SFO")
#' @import httr
#' @import rvest
#' @import XML
#' @export
getAirlineCode <- function(origin="TPE", destination="SEA") {
  url = paste0("http://www.colatour.com.tw/C10C_AirTicket/C10C_00_Query.aspx?TxType=Airline&Destination=",destination,"&Origin=",origin,"&AirlineCode=")
  url %>% GET %>% content(as = "text",encoding = "utf8") %>% `Encoding<-`("UTF-8") %>% xmlToDataFrame
}



#' getFlightsData
#'
#' Get Flights Data
#'
#' @param fareType
#' @param startDate
#' @param endDate
#' @param journeyType
#' @param originCode
#' @param destinationCode
#' @param bankName
#' @examples
#' getFlightsData(fareType="Y") %>% View
#' @import httr
#' @import rvest
#' @import XML
#' @import testit
#' @export
getFlightsData <- function(fareType="C",  # C=business class &Ｙ=econemic class
                           startDate="2016/09/03",
                           endDate="2016/09/03",
                           journeyType="來回",
                           originCode="TN",
                           destinationCode="SEA",
                           bankName="全部") {
  url = paste0("http://www.colatour.com.tw/C10C_AirTicket/C10C_01_FareChoose.aspx?",
               "FareType=",fareType,
               "&JourneyType=",URLdecode(URLdecode(journeyType)),
               "&OriginCode=",originCode,"&DestinationCode=",destinationCode,
               "&ReturnCode=&ReturnName=&RouteSummary=",paste0(originCode,"-",destinationCode,"-",originCode),
               "&AirlineCode=&StartDate=",startDate,"&EndDate=",endDate,
               "&BankName=",URLencode(bankName),"&DirectFlightMark=False")
  url

  tables = url %>%
    GET %>%
    content(as = "text",encoding = "utf8") %>%
    `Encoding<-`("UTF-8") %>%
    read_html %>%
    html_table(trim = F,fill = T)

  # tables[[8]] %>% View

  data_table_filter = sapply(tables, function(tab){
    tab[1,1] == "序號"
  })

  tables[data_table_filter][[1]]
}
