source("./common.R")

rebalance <- function(code = 600150, cashRate = 50, cashRateChange = 10, upRate = 20, downRate = 20, tradeCost = 0.15)
{
	# Always maintain balance with fixed distribution: cash x%, stock (100-x)%
	# Rebalance when stock price raise up upRate%, or drop down downRate%
	# Adjust cash rate: decrease cashRateChange% when price raise up, increase otherwise
	tradeHistory <- NULL

	# Load stock data
	data = loadStock(code)
	
	index <- 1
	balance <- 1000000
	stock <- (100 - cashRate) / 100.0 * balance
	cash <- cashRate / 100.0 * balance
	date <- data$Date[index]
	price <- data$Close[index]
	change <- NA
	
	# Save into trade history
	tradeRecord <- data.frame(
		Date = date,
		Price = price,
		ChangeRate = change,
		Balance = balance,
		StockBalance = stock,
		CashBalance = cash)
	tradeHistory <- rbind(tradeHistory, tradeRecord)
	
	upPrice <- (1 + upRate / 100.0) * price
	downPrice <- (1 - downRate / 100.0) * price
	
	index <- index + 1
	while(index <= nrow(data))
	{
		if(data$Close[index] >= upPrice || data$Close[index] <= downPrice)
		{
			if(data$Close[index] <= downPrice)
			{
				cashRate <- cashRate - cashRateChange
				if(cashRate <= 0)
				{
					cashRate <- 0
				}
			}
			else
			{
				cashRate <- cashRate + cashRateChange
				if(cashRate >= 100)
				{
					cashRate <- 100
				}
			}
			
			# Rebalance account
			newPrice <- data$Close[index]
			change <- (newPrice - price) / price * 100.0
			newStockBalance <- (newPrice * (1 - tradeCost / 100.0)) / (price * (1 + tradeCost / 100.0)) * stock
			
			balance <- newStockBalance + cash
			stock <- (100 - cashRate) / 100.0 * balance
			cash <- cashRate / 100.0 * balance
			date <- data$Date[index]
			price <- data$Close[index]		
			
			upPrice <- (1 + upRate / 100.0) * price
			downPrice <- (1 - downRate / 100.0) * price
			
			# Save into trade history
			tradeRecord <- data.frame(
				Date = date,
				Price = price,
				ChangeRate = change,
				Balance = balance,
				StockBalance = stock,
				CashBalance = cash)
			tradeHistory <- rbind(tradeHistory, tradeRecord)
		}
		
		index <- index + 1
	}
			
	# Open position
	index <- index - 1
	if(date != data$Date[index])
	{
		# Update current stock balance
		newPrice <- data$Close[index]
		change <- (newPrice - price) / price * 100.0
		stock <- (newPrice * (1 - tradeCost / 100.0)) / (price * (1 + tradeCost / 100.0)) * stock
		balance <- stock + cash
		date <- data$Date[index]
		price <- data$Close[index]
		
		# Save into trade history
		tradeRecord <- data.frame(
			Date = date,
			Price = price,
			ChangeRate = change,
			Balance = balance,
			StockBalance = stock,
			CashBalance = cash)
		tradeHistory <- rbind(tradeHistory, tradeRecord)
	}

	# Return trade history
	tradeHistory
}
