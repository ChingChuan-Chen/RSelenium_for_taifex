# check that installr is installed
if (!"installr" %in% rownames(installed.packages()))
  install.packages("installr")

# check that required packages are installed
installr::require2(xml2)
installr::require2(plyr)
installr::require2(pipeR)
installr::require2(lubridate)
installr::require2(data.table)
installr::require2(stringr)
installr::require2(RSelenium)

# function to parse arguments
if (interactive()) {
  workingDir <- normalizePath(".", "/")
} else {
  workingDir <- commandArgs(FALSE) %>>% `[`(str_detect(., "^--file=")) %>>%
    str_split("=") %>>% `[[`(1L) %>>% `[`(2L) %>>% normalizePath("/") %>>% dirname
  print(workingDir)
}
setwd(workingDir)

# firefox profile
Sys.setenv(R_ZIPCMD = "C:/Rtools/bin/zip.exe") # RTools
fprof <- makeFirefoxProfile(list(browser.download.folderList = 2L,
                                 browser.download.manager.showWhenStarting = FALSE,
                                 browser.download.useDownloadDir = TRUE,
                                 browser.download.dir = str_replace_all(normalizePath(str_c(workingDir, '/data')),
                                                                        "\\\\", "\\\\\\\\"),
                                 browser.helperApps.neverAsk.saveToDisk = "application/octet-stream",
                                 browser.helperApps.neverAsk.openFile = "application/octet-stream"))

# start Selenium server
rD <- rsDriver(browser = "firefox", extraCapabilities = fprof)
remDr <- rD$client
remDr$navigate("http://www.taifex.com.tw/chinese/3/dl_3_2_3.asp")

downloadFileFunc <- function(remDr, year = year(Sys.time()), month = month(Sys.time())) {
  # get starting date and end date
  startDate <- as_date(sprintf("%04i/%02i/01", year, month))
  numDays <- days_in_month(startDate)
  endDate <- min(startDate + numDays - 1, Sys.Date())
  # if the startDate is in the future, then stop
  if (startDate > Sys.Date()) return(FALSE)

  # input startDate/endDate at http://www.taifex.com.tw/chinese/3/dl_3_2_3.asp
  mapply(function(id, date){
    taifex <- remDr$findElement(using = "xpath", sprintf("//*/input[@id = '%s']", id))
    taifex$clearElement()
    taifex$sendKeysToElement(list(date,  key = "enter"))
  }, c("datestart", "dateend"), format(c(startDate, endDate), "%Y/%m/%d")) %>>% invisible()

  # select commodity id
  contract <- remDr$findElements(value = "//*[@id = 'COMMODITY_ID']/option")
  selectLoc <- sapply(contract, function(x){x$getElementAttribute('value')}) %>>%
    sapply(`==`, "TXO") %>>% which
  contract[[selectLoc]]$clickElement()
  # start to download
  submit <- remDr$findElement(using = "xpath", "//*/input[@id = 'button4']")
  submit$clickElement()
  return(TRUE)
}

downloadFileFunc(remDr, 2018, 4)

st <- proc.time()
CJ(year = 2015:2018, month = 1:12) %>>%
  with(mapply(function(y, m) downloadFileFunc(remDr, y, m), .$year, .$month)) %>>%
  invisible()
elapsedTime <- proc.time() - st
print(elapsedTime)
rD$server$stop()
