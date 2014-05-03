tradeStats <- function (Portfolios, Symbols, use = c("txns", "trades")) 
{
  ret <- NULL
  use <- use[1]
  for (Portfolio in Portfolios) {
    pname <- Portfolio
    Portfolio <- .getPortfolio(pname)
    if (missing(Symbols)) 
      symbols <- ls(Portfolio$symbols)
    else symbols <- Symbols
    for (symbol in symbols) {
      txn <- Portfolio$symbols[[symbol]]$txn
      posPL <- Portfolio$symbols[[symbol]]$posPL
      posPL <- posPL[-1, ]
      PL.gt0 <- txn$Net.Txn.Realized.PL[txn$Net.Txn.Realized.PL > 0]
      PL.lt0 <- txn$Net.Txn.Realized.PL[txn$Net.Txn.Realized.PL < 0]
      PL.ne0 <- txn$Net.Txn.Realized.PL[txn$Net.Txn.Realized.PL !=0]
      if (length(PL.ne0) == 0) {next}
      DailyPL <- apply.daily(PL.ne0, sum)
      AvgDailyPL <- mean(DailyPL)
      MedDailyPL <- median(DailyPL)
      StdDailyPL <- sd(as.numeric(as.vector(DailyPL)))
      switch(use, txns = {
      }, trades = {
        trades <- perTradeStats(pname, symbol)
        PL.gt0 <- trades$Net.Trading.PL[trades$Net.Trading.PL > 0]
        PL.lt0 <- trades$Net.Trading.PL[trades$Net.Trading.PL < 0]
        PL.ne0 <- trades$Net.Trading.PL[trades$Net.Trading.PL !=0]
      })
      if (!length(PL.ne0) > 0) 
        (next)()
      GrossProfits <- sum(PL.gt0)
      GrossLosses <- sum(PL.lt0)
      ProfitFactor <- abs(GrossProfits/GrossLosses)
      AvgTradePL <- mean(PL.ne0)
      MedTradePL <- median(PL.ne0)
      StdTradePL <- sd(as.numeric(as.vector(PL.ne0)))
      NumberOfTxns <- nrow(txn) - 1
      NumberOfTrades <- length(PL.ne0)
      PercentPositive <- (length(PL.gt0)/length(PL.ne0)) * 
        100
      PercentNegative <- (length(PL.lt0)/length(PL.ne0)) * 
        100
      MaxWin <- max(txn$Net.Txn.Realized.PL)
      MaxLoss <- min(txn$Net.Txn.Realized.PL)
      AvgWinTrade <- mean(PL.gt0)
      MedWinTrade <- median(PL.gt0)
      AvgLossTrade <- mean(PL.lt0)
      MedLossTrade <- median(PL.lt0)
      AvgWinLoss <- AvgWinTrade/-AvgLossTrade
      MedWinLoss <- MedWinTrade/-MedLossTrade
      Equity <- cumsum(posPL$Net.Trading.PL)
      if (!nrow(Equity)) {
        warning("No Equity rows for", symbol)
        (next)()
      }
      TotalNetProfit <- last(Equity)
      if (is.na(TotalNetProfit)) {
        warning("TotalNetProfit NA for", symbol)
        (next)()
      }
      Equity.max <- cummax(Equity)
      MaxEquity <- max(Equity)
      MinEquity <- min(Equity)
      EndEquity <- last(Equity)
      names(EndEquity) <- "End.Equity"
      if (EndEquity != TotalNetProfit && last(txn$Pos.Qty) == 
            0) {
        warning("Total Net Profit for", symbol, "from transactions", 
                TotalNetProfit, "and cumulative P&L from the Equity Curve", 
                EndEquity, "do not match. This can happen in long/short portfolios.")
        message("Total Net Profit for", symbol, "from transactions", 
                TotalNetProfit, "and cumulative P&L from the Equity Curve", 
                EndEquity, "do not match. This can happen in long/short portfolios.")
      }
      MaxDrawdown <- -max(Equity.max - Equity)
      retOnMDD <- -TotalNetProfit/MaxDrawdown
      names(retOnMDD) <- "retOnMDD"
      #######
      #trading.pl <- getPortfolio(pname)$summary$Net.Trading.PL
      trading.pl <- getPortfolio(pname)$symbol[[symbol]]$posPL$Net.Trading.PL
      rets <- trading.pl / initEq
      omega <- as.numeric(Omega(rets))
      #######
      tmpret <- data.frame(Portfolio = pname, Symbol = symbol, 
                           Num.Txns = NumberOfTxns, Num.Trades = NumberOfTrades, 
                           Total.Net.Profit = TotalNetProfit, Avg.Trade.PL = AvgTradePL, 
                           Med.Trade.PL = MedTradePL, Largest.Winner = MaxWin, 
                           Largest.Loser = MaxLoss, Gross.Profits = GrossProfits, 
                           Gross.Losses = GrossLosses, Std.Dev.Trade.PL = StdTradePL, 
                           Percent.Positive = PercentPositive, Percent.Negative = PercentNegative, 
                           Profit.Factor = ProfitFactor, Avg.Win.Trade = AvgWinTrade, 
                           Med.Win.Trade = MedWinTrade, Avg.Losing.Trade = AvgLossTrade, 
                           Med.Losing.Trade = MedLossTrade, Avg.Daily.PL = AvgDailyPL, 
                           Med.Daily.PL = MedDailyPL, Std.Dev.Daily.PL = StdDailyPL, 
                           Max.Drawdown = MaxDrawdown, retOnMDD = retOnMDD, 
                           Avg.WinLoss.Ratio = AvgWinLoss, Med.WinLoss.Ratio = MedWinLoss, 
                           Max.Equity = MaxEquity, Min.Equity = MinEquity, 
      #                     End.Equity = EndEquity)
                          End.Equity = EndEquity, omega=omega)
      rownames(tmpret) <- symbol
      ret <- rbind(ret, tmpret)
    }
  }
  return(ret)
}