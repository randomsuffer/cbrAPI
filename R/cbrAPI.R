#' Allows to download and read dbf-files with different forms of accounting reports (101, 102, 123, 134, 135) for a certain time period, as well as find information about the banks on Central Bank of Russia site.
#'
#' The goal of this project is to make available to users all over the world the unique data of the accounting statements of Russian banks, which is collected by the Central Bank of Russian Federation. Data is represented as archived dbf files (database files). The package provides the opportunity to download these archives from the Central Bank's website, read them and load into R. You can download any of the five forms of the accounting reports (101, 102, 123, 134, 135) for any given time period or over the entire available. You can also get detailed information on any Bank, knowing his registration number to find out the name, address, is it registered it in the Deposit Insurance Agency, is it went bankrupt or not.
#'
#' @docType package
#' @name cbrAPI
#' @importFrom plyr rbind.fill
#' @importFrom foreign read.dbf
#' @importFrom XML readHTMLTable
#' @export download.dbf download.dbf.all getBankInfo
NULL

# get number of days in a month
numberOfDays <- function(date) {
  m <- format(date, format="%m")
  while (format(date, format="%m") == m) {
    date <- date + 1
  }
  return(as.integer(format(date - 1, format="%d")))
}

# inarchive rar files
unrar <- function(filename, exdir) {
  #for Windows
  if(Sys.info()[1] == "Windows") {
#     # need installr package
#     if(length(grep('installr', rownames(installed.packages()))) < 1) {
#       install.packages('installr')
#     }
#     require(installr)
#     # install unrar once, create file not to do it again
#     if(file.exists('data/unrar.txt') == FALSE) {
#       install.URL('http://www.rarlab.com/rar/unrarw32.exe')
#       file.create('data/unrar.txt')
#       cat("Do not delete me!", file="data/unrar.txt", append=TRUE)
#     }
    warning('Make sure that you have firstly installed unrar tools from ', shQuote('http://www.rarlab.com/rar/unrarw32.exe'), ' and extracted them to C:\\Windows.')
    dir.create(substr(exdir, 1, 16), showWarnings = FALSE)
    #exdir <- paste(getwd(), '/dbf/101-20100101', sep = '')
    cmd <- paste('unrar e', shQuote(filename), '-y -o', shQuote(exdir))
    system(cmd, ignore.stderr = TRUE, ignore.stdout = TRUE)
  }
  #for OS X
  if(Sys.info()[1] == "Darwin") {
    system('ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"', ignore.stderr = TRUE)
    # system('brew update', ignore.stdout = TRUE)
    system('brew install unrar', ignore.stderr = TRUE)
    dir.create(substr(exdir, 1, 16), showWarnings = FALSE)
    system(paste('unrar e', filename, exdir), ignore.stdout = TRUE)
  }
  #for Unix
  if(Sys.info()[1] == "Unix") {
    warning('Make sure that you have firstly installed 7zip with ', shQuote('sudo apt-get install p7zip-rar'))
    #     system('sudo apt-get install p7zip-rar')
    system(paste("7z x ", as.character(filename)))
  }
  #if(.Platform$OS.type == 'unix')
}

#' @title Download financial statements of Russian banks
#' @description This function allows you to download and read dbf-files, which contain all available accounting reports and financial statements from site of Central Bank of Russia (www.cbr.ru). \cr
#' See \code{vignette(package = 'cbrAPI')} for proper use.
#' @param startdate Start date, contains from when to start downloading data. Format \code{'yyyy-mm'}.
#' @param enddate End date, contains from when to finish downloading data. Format \code{'yyyy-mm'}. Defaults to \code{startdate}.
#' @param form Report form, contains what type of form to download. Can be \code{101}, \code{102}, \code{123}, \code{134} and \code{135}. Defaults to \code{101}.
#' @param tofactor Convert resulted data frame to factor or not? Defaults to \code{FALSE}.
#' @param savefiles Save downloaded dbf-files or not? Defaults to \code{FALSE}.
#' @return Returns data frame with row-binded content of dbf-files
#' @seealso \code{\link{download.file}} for simple downloading, \code{\link{read.dbf}} for reading dbf-files
#' @examples
#' data101 <- download.dbf("2006-07")
#' data123 <- download.dbf("2014-07", "2015-01", form=123)
#' @keywords dbf cbr download read accounting reports russia banks
download.dbf <- function(startdate, enddate=startdate, form=101, tofactor=FALSE, savefiles=FALSE) {
  path = 'dbf'
  unlink(path, recursive = TRUE)
  dir.create(path, showWarnings = FALSE)
  
  startdate <- paste(startdate, "-01", sep="")
  startdate <- as.Date(startdate, "%Y-%m-%d")
  enddate <- paste(enddate, "-01", sep="")
  enddate <- as.Date(enddate, "%Y-%m-%d")
  
  lastdate <- Sys.Date() - as.numeric(substr(Sys.Date(), 9, 10))
  lastdate <- lastdate - numberOfDays(lastdate) + 1
  if(enddate > lastdate) {
    stop('No data for that date. Check enddate arg.')
  }
  
  # from 01.02.2004, monthly
  if(form == 101) {
    if(startdate >= as.Date("2004-02-01", "%Y-%m-%d")) {
      date <- startdate
      while(date <= enddate) {
        folder <- paste(substr(date, 1, 4), substr(date, 6, 7), substr(date, 9, 10), sep="")
        if(date < "2009-01-01") {
          #2004 - 2008 zip
          filetype <- '.zip'
          url <- paste("http://www.cbr.ru/credit/forms/101-", folder, filetype, sep="")
          filename <- paste(path, "/101-", folder, filetype, sep="")
          download.file(url, filename, mode = 'wb')
          unzip(filename, exdir=paste(path, "/101-", folder, sep=""))
        } else {
          #2009 - 2014 rar
          filetype <- '.rar'
          url <- paste("http://www.cbr.ru/credit/forms/101-", folder, filetype, sep="")
          filename <- paste(path, "/101-", folder, filetype, sep="")
          download.file(url, filename, mode = 'wb')
          unrar(filename, exdir=paste(path, "/101-", folder, sep=""))
        }
        date <- date+numberOfDays(date)
      }
    }
    else {
      stop("Start date must be later than 2004.02.01")
    }
    # read dbf
    # start from 2-nd, 1-st is root dir
    data <- NULL
    for(i in 2:length(list.dirs(path))) {
      # take mmyyyyB1.DBF, if not exists, take mmyyyy_B.DBF
      index <- grep("B1", list.files(list.dirs(path)[i]))
      if(length(index) < 1) {
        index <- grep("_B", list.files(list.dirs(path)[i]))
      }
      data <- rbind.fill(data,
                         read.dbf(paste(list.dirs(path)[i], "/",
                                        list.files(list.dirs(path)[i])[index], sep=""), as.is = FALSE))
      message(i-1, " from ", length(list.dirs(path))-1, " done.")
    }
  }
  # from 01.01.2004, quarterly
  if(form == 102) {
    if(startdate >= as.Date("2004-01-01", "%Y-%m-%d")) {
      date <- startdate
      if(as.numeric(substr(date, 6,7)) %in% c(1,4,7,10)) {
        while(date <= enddate) {
          folder <- paste(substr(date, 1, 4), substr(date, 6, 7), substr(date, 9, 10), sep="")
          if(date < "2009-01-01") {
            #2004 - 2008 zip
            filetype <- '.zip'
            url <- paste("http://www.cbr.ru/credit/forms/102-", folder, filetype, sep="")
            filename <- paste(path, "/102-", folder, filetype, sep="")
            download.file(url, filename, mode = 'wb')
            unzip(filename, exdir=paste(path, "/102-", folder, sep=""))
          } else {
            #2009 - 2014 rar
            filetype <- '.rar'
            url <- paste("http://www.cbr.ru/credit/forms/102-", folder, filetype, sep="")
            filename <- paste(path, "/102-", folder, filetype, sep="")
            download.file(url, filename, mode = 'wb')
            unrar(filename, exdir=paste(path, "/102-", folder, sep=""))
          }
          # go to the next quarter
          for(i in 1:3) { date <- date+numberOfDays(date) }
        }
      }
      else {
        stop("No dbf found on this date")
      }
    }
    else {
      stop("Start date must be later than 2004.01.01")
    }
    # read dbf
    # start from 2-nd, 1-st is root dir
    data <- NULL
    date <- startdate
    for(i in 2:length(list.dirs(path))) {
      # take mmyyyyP1.DBF, if not exists, take mmyyyy_P.DBF
      index <- grep("P1", list.files(list.dirs(path)[i]))
      if(length(index) < 1) {
        index <- grep("_P", list.files(list.dirs(path)[i]))
      }
      data <- rbind.fill(data,
                         cbind(read.dbf(paste(list.dirs(path)[i], "/",
                                              list.files(list.dirs(path)[i])[index], sep=""), as.is = FALSE), date))
      # go to the next quarter
      for(j in 1:3) { date <- date+numberOfDays(date) }
      message(i-1, " from ", length(list.dirs(path))-1, " done.")
    }
    data <- data[ , c('REGN','CODE','date','ITOGO','SIM_R','SIM_V','SIM_ITOGO')]
    colnames(data)[3] <- 'DT'
  }
  # from 01.02.2014, monthly
  if(form == 123) {
    if(startdate >= as.Date("2014-02-01", "%Y-%m-%d")) {
      date <- startdate
      while(date <= enddate) {
        folder <- paste(substr(date, 1, 4), substr(date, 6, 7), substr(date, 9, 10), sep="")
        filetype <- '.rar'
        url <- paste("http://www.cbr.ru/credit/forms/123-", folder, filetype, sep="")
        filename <- paste(path, "/123-", folder, filetype, sep="")
        download.file(url, filename, mode = 'wb')
        unrar(filename, exdir=paste(path, "/123-", folder, sep=""))
        date <- date+numberOfDays(date)
      }
    }
    else {
      stop("Start date must be later than 2014.02.01")
    }
    # read dbf
    # start from 2-nd, 1-st is root dir
    data <- NULL
    for(i in 2:length(list.dirs(path))) {
      # take mmyyyy_123D.DBF
      index <- grep("_123D", list.files(list.dirs(path)[i]))
      data <- rbind.fill(data,
                         read.dbf(paste(list.dirs(path)[i], "/",
                                        list.files(list.dirs(path)[i])[index], sep=""), as.is = FALSE))
      message(i-1, " from ", length(list.dirs(path))-1, " done.")
    }
  }
  # from 01.06.2010, monthly
  if(form == 134) {
    if(startdate >= as.Date("2010-06-01", "%Y-%m-%d")) {
      date <- startdate
      while(date <= enddate) {
        folder <- paste(substr(date, 1, 4), substr(date, 6, 7), substr(date, 9, 10), sep="")
        filetype <- '.rar'
        url <- paste("http://www.cbr.ru/credit/forms/134-", folder, filetype, sep="")
        filename <- paste(path, "/134-", folder, filetype, sep="")
        download.file(url, filename, mode = 'wb')
        unrar(filename, exdir=paste(path, "/134-", folder, sep=""))
        date <- date+numberOfDays(date)
      }
    }
    else {
      stop("Start date must be later than 2010.06.01")
    }
    # read dbf
    # start from 2-nd, 1-st is root dir
    data <- NULL
    for(i in 2:length(list.dirs(path))) {
      # take mmyyyy_134D.DBF
      index <- grep("_134D", list.files(list.dirs(path)[i]))
      data <- rbind.fill(data,
                         read.dbf(paste(list.dirs(path)[i], "/",
                                        list.files(list.dirs(path)[i])[index], sep=""), as.is = FALSE))
      message(i-1, " from ", length(list.dirs(path))-1, " done.")
    }
  }
  # from 01.06.2010, monthly
  if(form == 135) {
    if(startdate >= as.Date("2010-06-01", "%Y-%m-%d")) {
      date <- startdate
      while(date <= enddate) {
        folder <- paste(substr(date, 1, 4), substr(date, 6, 7), substr(date, 9, 10), sep="")
        filetype <- '.rar'
        url <- paste("http://www.cbr.ru/credit/forms/135-", folder, filetype, sep="")
        filename <- paste(path, "/135-", folder, filetype, sep="")
        download.file(url, filename, mode = 'wb')
        unrar(filename, exdir=paste(path, "/135-", folder, sep=""))
        date <- date+numberOfDays(date)
      }
      # read dbf
      # start from 2-nd, 1-st is root dir
      data <- NULL
      for(i in 2:length(list.dirs(path))) {
        # take mmyyyy_135_3.DBF
        index <- grep("_135_3", list.files(list.dirs(path)[i]))
        data <- rbind.fill(data,
                           read.dbf(paste(list.dirs(path)[i], "/",
                                          list.files(list.dirs(path)[i])[index], sep=""), as.is = FALSE))
        message(i-1, " from ", length(list.dirs(path))-1, " done.")
      }
    }
    else {
      stop("Start date must be later than 2010.06.01")
    }
  }
  if(form %in% c(101, 102, 123, 134, 135) == FALSE) {
    stop('No such form on www.cbr.ru')
  }
  
  if(tofactor == TRUE) {
    message("\nConverting to factor...")
    data <- data.frame(lapply(data, factor))
  }
  if(savefiles == FALSE) {
    unlink(path, recursive = TRUE)
  }
  return(data)
}

#' @title Download financial statements of Russian banks
#' @description This function allows you to download and read dbf-files FOR ALL AVAILABLE DATES, which contain all available accounting reports and financial statements from site of Central Bank of Russia (www.cbr.ru) \cr
#' See \code{vignette(package = 'cbrAPI')} for proper use.
#' @param form Report form, contains what type of form to download. Can be \code{101}, \code{102}, \code{123}, \code{134} and \code{135}. Defaults to \code{101}.
#' @param tofactor Convert resulted data frame to factor or not? Defaults to \code{FALSE}.
#' @param savefiles Save downloaded dbf-files or not? Defaults to \code{FALSE}.
#' @return Returns data frame with row-binded content of dbf-files
#' @seealso \code{\link{download.file}} for simple downloading, \code{\link{read.dbf}} for reading dbf-files
#' @examples
#' data135 <- download.dbf.all(form=135)
#' @keywords dbf cbr download read accounting reports russia banks
download.dbf.all <- function(form=101, tofactor=FALSE, savefiles=FALSE) {
  if(form == 101) {startdate <- '2004-02'}
  if(form == 102) {startdate <- '2004-01'}
  if(form == 123) {startdate <- '2014-02'}
  if(form == 134) {startdate <- '2010-06'}
  if(form == 135) {startdate <- '2010-06'}
  enddate <- Sys.Date() - as.numeric(substr(Sys.Date(), 9, 10))
  enddate <- enddate - numberOfDays(enddate) + 1
  enddate <- substr(enddate, 1, 7)
  data <- download.dbf(startdate, enddate, form=form, tofactor=tofactor, savefiles=savefiles)
  return(data)
}

#' @title Get information about Russian banks
#' @description This function allows you to parse information about Russian banks by registration number from site of Central Bank of Russia (www.cbr.ru). \cr
#' See \code{vignette(package = 'cbrAPI')} for proper use.
#' @param regnum Registration number of some bank. Can be integer or string.
#' @return Returns data frame with information about bank.\cr\cr
#' \code{name} - Bank's name\cr
#' \code{regnum} - Bank's registration number\cr
#' \code{address} - Bank's address\cr
#' \code{balance} - Presence of balance reports (monthly)\cr
#' \code{profitandlosses} - Presence of profit and loss statement (quarterly)\cr
#' \code{dia} - Is this bank registered in the Deposit Insurance Agency (DIA)?\cr
#' \code{intheprocess} - Is this bank in the process of registration (no license yet)?\cr
#' \code{withdrawed} - Is licence withdrawn?\cr
#' \code{revoked} - Is licence revoked?\cr
#' \code{eliminated} - Is licence eliminated?
#' @seealso \code{\link{readHTMLTable}} for parsing html tables, \code{\link{download.dbf}} for downloading financial statements of Russian banks
#' @examples
#' info <- getBankInfo(2856)
#' info <- getBankInfo("30")$name
#' @keywords dbf cbr download read accounting reports russia banks
getBankInfo <- function(regnum) {
  codes <- c('\u0411', '\u041F', '\u0421', "\u043E\u0444\u0440", "\u043E\u0442\u0437", "\u0430\u043D\u043D", "\u043B\u0438\u043A")
  info <- matrix(nrow=0,ncol=10)
  url <- paste("http://www.cbr.ru/credit/colist.asp?find=", regnum, "&how=rnum", sep="")
  table <- readHTMLTable(url)[[2]]
  name <- as.character(table[1,4])
  address <- as.character(table[1,5])
  if(length(grep(codes[1], as.character(table[1,2]))) > 0) { balance <- TRUE } else { balance <- FALSE }
  if(length(grep(codes[2], as.character(table[1,2]))) > 0) { profitandlosses <- TRUE } else { profitandlosses <- FALSE }
  if(length(grep(codes[3], as.character(table[1,2]))) > 0) { dia <- TRUE } else { dia <- FALSE }
  if(length(grep(codes[4], as.character(table[1,2]))) > 0) { intheprocess <- TRUE } else { intheprocess <- FALSE }
  if(length(grep(codes[5], as.character(table[1,2]))) > 0) { withdrawed <- TRUE } else { withdrawed <- FALSE }
  if(length(grep(codes[6], as.character(table[1,2]))) > 0) { revoked <- TRUE } else { revoked <- FALSE }
  if(length(grep(codes[7], as.character(table[1,2]))) > 0) { eliminated <- TRUE } else { eliminated <- FALSE }
  info <- rbind(c(name, regnum, address, balance, profitandlosses, dia, intheprocess, withdrawed, revoked, eliminated))
  colnames(info) <- c('name', 'regnum', 'address', 'balance', 'profitandlosses', 'dia', 'intheprocess', 'withdrawed', 'revoked', 'eliminated')
  info <- as.data.frame(info)
  return(info)
}