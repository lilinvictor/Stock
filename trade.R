source("./common.R")

tradeDown <- function(stock = loadStock(600150), downRate1 = 10, downRate2 = 10, profitRate = 5, lossRate = 10, tradeCost = 0.25)
{
	# Buy first half when stock price is down by downRate1% from previous max
	# Sell first half when profit is higher than profitRate%
	# Buy second half when stock price is down by downRate2% from first buy price
	# Sell second+first together to cover combined cost
	# Sell second+first with previous close price when price is down by lossRate% from second buy price
	tradeHistory <- NULL

	index <- 1
	balance <- 100000
	buyDate1 <- NA
	buyDate2 <- NA
	max <- stock$High[index]
	
	index <- index + 1
	while(index <= nrow(stock))
	{
		if(is.na(buyDate1))
		{
			# No position yet, track low price to decide if to buy
			lowPrice <- round(max * (1 - downRate1/100), digits = 2)
			
			if(stock$Low[index] < lowPrice)
			{
				# Buy first half
				buyDate1 <- stock$Date[index]
				buyDate2 <- NA
				buyPrice1 <- if(stock$Open[index] < lowPrice) stock$Open[index] else lowPrice
				vol <- balance / 2 / buyPrice1 / (1 + tradeCost / 100);
				buyVol1 <- vol - (vol %% 100);
				buyCost1 <- buyPrice1 * buyVol1 * (1 + tradeCost/100)
				balance <- balance - buyCost1
			}
			else
			{
				# Keep tracking previous high
				if(stock$High[index] > max) max <- stock$High[index]
			}
		}
		else
		{
			# We have position
			if(is.na(buyDate2))
			{
				# Already have half position, decide if it's time to sell or buy second half
				targetPrice <- round(buyPrice1 * (1 + profitRate/100), digits = 2)
				appendPrice <- round(buyPrice1 * (1 - downRate2/100), digits = 2)
				
				if(stock$High[index] > targetPrice)
				{
					# Sell all position
					sellPrice <- if(stock$Open[index] > targetPrice) stock$Open[index] else targetPrice
					sellValue <- sellPrice * buyVol1 * (1 - tradeCost/100)
					profit <- sellValue - buyCost1
					balance <- balance + sellValue
					
					# Save into trade history
					tradeRecord <- data.frame(
						BuyDate1 = buyDate1,
						BuyPrice1 = buyPrice1,
						BuyVol1 = buyVol1,
						BuyCost1 = buyCost1,
						BuyDate2 = NA,
						BuyPrice2 = NA,
						BuyVol2 = NA,
						BuyCost2 = NA,
						SellDate = stock$Date[index],
						SellPrice = sellPrice,
						SellValue = sellValue,
						Profit = profit,
						Balance = balance)
					tradeHistory <- rbind(tradeHistory, tradeRecord)
					
					# Reset trade
					buyDate1 <- NA
					buyDate2 <- NA
					max <- stock$High[index]
				}
				else if(stock$Low[index] < appendPrice)
				{
					# Buy second half
					buyDate2 <- stock$Date[index]
					buyPrice2 <- if(stock$Open[index] < appendPrice) stock$Open[index] else appendPrice
					vol <- balance / buyPrice2 / (1 + tradeCost / 100);
					buyVol2 <- vol - (vol %% 100);
					buyCost2 <- buyPrice2 * buyVol2 * (1 + tradeCost/100)
					balance <- balance - buyCost2
				}
			}
			else
			{
				# Already full position, either sell to cover base cost, or stop loss
				costBase <- (buyCost1 + buyCost2) / (buyVol1 + buyVol2) / (1 - tradeCost/100)
				#targetPrice <- round(costBase, digits = 2)
				#targetPrice <- round(costBase * (1 + profitRate/100), digits = 2)
				targetPrice <- round(buyPrice2 * (1 + profitRate/100), digits = 2)
				stopLossPrice <- round(buyPrice2 * (1 - lossRate/100), digits = 2)
				
				sellPrice <- NA
				if(stock$High[index] > targetPrice)
				{
					# Sell all position to cover base cost
					sellPrice <- if(stock$Open[index] > targetPrice) stock$Open[index] else targetPrice
				}
				else if(stock$Low[index] < stopLossPrice)
				{
					# Sell with market price, i.e. Open price as next day
					sellPrice <- stock$Open[index + 1]
				}
				
				if(!is.na(sellPrice))
				{
					sellValue <- sellPrice * (buyVol1 + buyVol2) * (1 - tradeCost/100)
					profit <- sellValue - (buyCost1 + buyCost2)
					balance <- balance + sellValue
					
					tradeRecord <- data.frame(
						BuyDate1 = buyDate1,
						BuyPrice1 = buyPrice1,
						BuyVol1 = buyVol1,
						BuyCost1 = buyCost1,
						BuyDate2 = buyDate2,
						BuyPrice2 = buyPrice2,
						BuyVol2 = buyVol2,
						BuyCost2 = buyCost2,
						SellDate = stock$Date[index],
						SellPrice = sellPrice,
						SellValue = sellValue,
						Profit = profit,
						Balance = balance)
					
					# Save into trade history
					tradeHistory <- rbind(tradeHistory, tradeRecord)
					
					# Reset trade
					buyDate1 <- NA
					buyDate2 <- NA
					max <- stock$High[index]
				}
			}
		}
		
		index <- index + 1
	}
	
	# Open position
	if(!is.na(buyDate1))
	{
		index <- index - 1
		sellPrice <- stock$Close[index]
		sellDate <- stock$Date[index]
		
		if(is.na(buyDate2))
		{
			sellValue <- sellPrice * buyVol1 * (1 - tradeCost/100)
			profit <- sellValue - buyCost1
			balance <- balance + sellValue
			
			tradeRecord <- data.frame(
				BuyDate1 = buyDate1,
				BuyPrice1 = buyPrice1,
				BuyVol1 = buyVol1,
				BuyCost1 = buyCost1,
				BuyDate2 = NA,
				BuyPrice2 = NA,
				BuyVol2 = NA,
				BuyCost2 = NA,
				SellDate = sellDate,
				SellPrice = sellPrice,
				SellValue = sellValue,
				Profit = profit,
				Balance = balance)
		}
		else
		{
			sellValue <- sellPrice * (buyVol1 + buyVol2) * (1 - tradeCost/100)
			profit <- sellValue - buyCost1 - buyCost2
			balance <- balance + sellValue
			
			tradeRecord <- data.frame(
				BuyDate1 = buyDate1,
				BuyPrice1 = buyPrice1,
				BuyVol1 = buyVol1,
				BuyCost1 = buyCost1,
				BuyDate2 = buyDate2,
				BuyPrice2 = buyPrice2,
				BuyVol2 = buyVol2,
				BuyCost2 = buyCost2,
				SellDate = sellDate,
				SellPrice = sellPrice,
				SellValue = sellValue,
				Profit = profit,
				Balance = balance)
		}
		
		# Save into trade history
		tradeHistory <- rbind(tradeHistory, tradeRecord)
	}

	# Return trade history
	tradeHistory
}

tradeUp <- function(stock = loadStock(600150), append = FALSE, upRate = 10, downRate = 10, profitRate = 5, lossRate = 10, tradeCost = 0.25)
{
	# Buy first half when stock price is up by upRate% from previous low
	# Sell first half when profit is higher than profitRate%
	# Buy second half when stock price is down by downRate% from first buy price
	# Sell second+first together to cover combined cost
	# Sell second+first with previous close price when price is down by lossRate% from second buy price
	tradeHistory <- NULL

	index <- 1
	balance <- 100000
	buyDate1 <- NA
	buyDate2 <- NA
	min <- stock$Low[index]
	
	index <- index + 1
	while(index <= nrow(stock))
	{
		if(is.na(buyDate1))
		{
			# No position yet, track stock price to decide if to buy
			highPrice <- round(min * (1 + upRate/100), digits = 2)
			
			if(stock$High[index] > highPrice)
			{
				# Buy first half with market price, aka open price of next day
				index <- index + 1
				buyDate1 <- stock$Date[index]
				buyDate2 <- NA
				buyPrice1 <- stock$Open[index]
				vol <- balance / 2 / buyPrice1 / (1 + tradeCost / 100);
				buyVol1 <- vol - (vol %% 100);
				buyCost1 <- buyPrice1 * buyVol1 * (1 + tradeCost/100)
				balance <- balance - buyCost1
			}
			else
			{
				# Keep tracking previous low
				if(stock$Low[index] < min) min <- stock$Low[index]
			}
		}
		else
		{
			# We have position
			if(is.na(buyDate2))
			{
				# Already have half position, decide if it's time to sell or buy second half
				targetPrice <- round(buyPrice1 * (1 + profitRate/100), digits = 2)
				appendPrice <- round(buyPrice1 * (1 - downRate/100), digits = 2)
				
				if(stock$High[index] > targetPrice)
				{
					# Sell all position
					sellPrice <- if(stock$Open[index] > targetPrice) stock$Open[index] else targetPrice
					sellValue <- sellPrice * buyVol1 * (1 - tradeCost/100)
					profit <- sellValue - buyCost1
					balance <- balance + sellValue
					
					# Save into trade history
					tradeRecord <- data.frame(
						BuyDate1 = buyDate1,
						BuyPrice1 = buyPrice1,
						BuyVol1 = buyVol1,
						BuyCost1 = buyCost1,
						BuyDate2 = NA,
						BuyPrice2 = NA,
						BuyVol2 = NA,
						BuyCost2 = NA,
						SellDate = stock$Date[index],
						SellPrice = sellPrice,
						SellValue = sellValue,
						Profit = profit,
						Balance = balance)
					tradeHistory <- rbind(tradeHistory, tradeRecord)
					
					# Reset trade
					buyDate1 <- NA
					buyDate2 <- NA
					max <- stock$High[index]
				}
				else if(append)
				{
					if(stock$Low[index] < appendPrice)
					{
						# Buy second half
						buyDate2 <- stock$Date[index]
						buyPrice2 <- if(stock$Open[index] < appendPrice) stock$Open[index] else appendPrice
						vol <- balance / buyPrice2 / (1 + tradeCost / 100);
						buyVol2 <- vol - (vol %% 100);
						buyCost2 <- buyPrice2 * buyVol2 * (1 + tradeCost/100)
						balance <- balance - buyCost2
					}
				}
				else
				{
					# Don't buy more stock when loss. Instead, stop loss when necessary
					stopLossPrice <- round(buyPrice1 * (1 - lossRate/100), digits = 2)
					if(stock$Low[index] < stopLossPrice)
					{
						# Sell with market price, i.e. Open price as next day
						index <- index + 1
						sellPrice <- stock$Open[index]
						sellValue <- sellPrice * buyVol1 * (1 - tradeCost/100)
						profit <- sellValue - buyCost1
						balance <- balance + sellValue
						
						tradeRecord <- data.frame(
							BuyDate1 = buyDate1,
							BuyPrice1 = buyPrice1,
							BuyVol1 = buyVol1,
							BuyCost1 = buyCost1,
							BuyDate2 = NA,
							BuyPrice2 = NA,
							BuyVol2 = NA,
							BuyCost2 = NA,
							SellDate = stock$Date[index],
							SellPrice = sellPrice,
							SellValue = sellValue,
							Profit = profit,
							Balance = balance)
						
						# Save into trade history
						tradeHistory <- rbind(tradeHistory, tradeRecord)
						
						# Reset trade
						buyDate1 <- NA
						buyDate2 <- NA
						max <- stock$High[index]
					}
				}
			}
			else
			{
				# Already full position, either sell to cover base cost, or stop loss
				costBase <- round((buyCost1 + buyCost2) / (buyVol1 + buyVol2) / (1 - tradeCost/100), digits = 2)
				targetPrice <- costBase
				stopLossPrice <- round(buyPrice2 * (1 - lossRate/100), digits = 2)
				
				sellPrice <- NA
				if(stock$High[index] > targetPrice)
				{
					# Sell all position to cover base cost
					sellPrice <- if(stock$Open[index] > targetPrice) stock$Open[index] else targetPrice
				}
				else if(stock$Low[index] < stopLossPrice)
				{
					# Sell with market price, i.e. Open price as next day
					sellPrice <- stock$Open[index + 1]
				}
				
				if(!is.na(sellPrice))
				{
					sellValue <- sellPrice * (buyVol1 + buyVol2) * (1 - tradeCost/100)
					profit <- sellValue - (buyCost1 + buyCost2)
					balance <- balance + sellValue
					
					tradeRecord <- data.frame(
						BuyDate1 = buyDate1,
						BuyPrice1 = buyPrice1,
						BuyVol1 = buyVol1,
						BuyCost1 = buyCost1,
						BuyDate2 = buyDate2,
						BuyPrice2 = buyPrice2,
						BuyVol2 = buyVol2,
						BuyCost2 = buyCost2,
						SellDate = stock$Date[index],
						SellPrice = sellPrice,
						SellValue = sellValue,
						Profit = profit,
						Balance = balance)
					
					# Save into trade history
					tradeHistory <- rbind(tradeHistory, tradeRecord)
					
					# Reset trade
					buyDate1 <- NA
					buyDate2 <- NA
					max <- stock$High[index]
				}
			}
		}
		
		index <- index + 1
	}
	
	# Open position
	if(!is.na(buyDate1))
	{
		index <- index - 1
		sellPrice <- stock$Close[index]
		sellDate <- stock$Date[index]
		
		if(is.na(buyDate2))
		{
			sellValue <- sellPrice * buyVol1 * (1 - tradeCost/100)
			profit <- sellValue - buyCost1
			balance <- balance + sellValue

			tradeRecord <- data.frame(
				BuyDate1 = buyDate1,
				BuyPrice1 = buyPrice1,
				BuyVol1 = buyVol1,
				BuyCost1 = buyCost1,
				BuyDate2 = NA,
				BuyPrice2 = NA,
				BuyVol2 = NA,
				BuyCost2 = NA,
				SellDate = sellDate,
				SellPrice = sellPrice,
				SellValue = sellValue,
				Profit = profit,
				Balance = balance)
		}
		else
		{
			sellValue <- sellPrice * (buyVol1 + buyVol2) * (1 - tradeCost/100)
			profit <- sellValue - buyCost1 - buyCost2
			balance <- balance + sellValue
			
			tradeRecord <- data.frame(
				BuyDate1 = buyDate1,
				BuyPrice1 = buyPrice1,
				BuyVol1 = buyVol1,
				BuyCost1 = buyCost1,
				BuyDate2 = buyDate2,
				BuyPrice2 = buyPrice2,
				BuyVol2 = buyVol2,
				BuyCost2 = buyCost2,
				SellDate = sellDate,
				SellPrice = sellPrice,
				SellValue = sellValue,
				Profit = profit,
				Balance = balance)
		}
		
		# Save into trade history
		tradeHistory <- rbind(tradeHistory, tradeRecord)
	}

	# Return trade history
	tradeHistory
}
