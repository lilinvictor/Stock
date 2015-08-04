library(quantmod)

loadAStock <- function(code = 600150)
{
	if(code >= 600000)
	{
		symbol <- sprintf("%06d.ss", code)
	}
	else
	{
		symbol <- sprintf("%06d.sz", code)
	}
	
	loadStock(symbol)
}

loadStock <- function(symbol)
{
  data <- getSymbols(symbol, auto.assign = F, src = "yahoo")
  names(data) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
  
  return(data)
}

getStockDayFile <- function(code = 600150, dir = "C:\\new_hyzq_v6")
{
	if(code >= 600000)
	{
		sprintf("%s\\vipdoc\\sh\\lday\\sh%06d.day", dir, code)
	}
	else
	{
		sprintf("%s\\vipdoc\\sz\\lday\\sz%06d.day", dir, code)
	}
}

loadStockFromHyzqFile <- function(code = 600150, dir = "C:\\new_hyzq_v6")
{
	if(code >= 600000)
	{
		dataFile <- sprintf("%s\\vipdoc\\sh\\lday\\sh%06d.day", dir, code)
	}
	else
	{
		dataFile <- sprintf("%s\\vipdoc\\sz\\lday\\sz%06d.day", dir, code)
	}

	f <- file(dataFile, "rb")
	history <- NULL
	repeat {
		dayData <- readBin(f, integer(), n = 8)
		if(length(dayData) < 1) break
		data <- data.frame(
			Date	 = dayData[1],
			Open	 = dayData[2] / 100.0,
			High	 = dayData[3] / 100.0,
			Low		= dayData[4] / 100.0,
			Close	= dayData[5] / 100.0,
			Amount = dayData[6],
			Volume = dayData[7],
			Prev	 = dayData[8] / 100.0)
		history <- rbind(history, data)
	}
	close(f)
	
	history
}

plotStock <- function(data)
{
  plot(x = index(data), y = data$Adjusted)
}