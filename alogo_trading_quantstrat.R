install.packages("remotes")

remotes::install_github("braverock/quantstrat")

install.packages(quantstrat)

library(quantstrat)
library(quantmod)

install.packages("devtools")
require(devtools)
install_github("braverock/blotter") # dependency
install_github("braverock/quantstrat")


# Get SPY from yahoo
getSymbols("SPY", 
           from = "2000-01-01", 
           to = "2016-06-30", 
           src =  "yahoo", 
           adjust =  TRUE)

# Plot the closing price of SPY
plot(Cl(SPY))

# Add a 200-day SMA using lines()
lines(SMA(Cl(SPY), n = 200), col = "red")

# Create initdate, from, and to strings
initdate <- "1999-01-01"
from <- "2003-01-01"
to <- "2015-12-31"

# Set the timezone to UTC
Sys.setenv(TZ="UTC")

# Set the currency to USD 
currency("USD")

# Retrieve SPY from yahoo
getSymbols("SPY", from=from, to=to, src="yahoo", adjust=TRUE)

# Use stock() to initialize SPY and set currency to USD
stock("SPY", currency="USD" )

# Define   trade size and initial equity
tradesize <- 100000
initeq <- 100000

# Define the names of   strategy, portfolio and account
strategy.st <- "firststrat"
portfolio.st <- "firststrat"
account.st <- "firststrat"

# Remove the existing strategy if it exists
rm.strat(strategy.st)

# Initialize the portfolio
initPortf(portfolio.st, symbols = "SPY", initDate = initdate, currency = "USD")

# Initialize the account
initAcct(account.st, portfolios = portfolio.st, initDate = initdate, currency = "USD", initEq = initeq)

# Initialize the orders
initOrders(portfolio.st, initDate = initdate)

# Store the strategy
strategy(strategy.st, store = TRUE)

# Create a 200-day SMA
spy_sma <- SMA(Cl(SPY), n=200)

# Create an RSI with a 3-day lookback period
spy_rsi <- RSI(Cl(SPY), n=3)

# Plot the closing prices of SPY
plot(Cl(SPY))

# Overlay a 200-day SMA
lines(SMA(Cl(SPY), n = 200), col = "red")

# Plot the closing price of SPY
plot(Cl(SPY))

# Plot the RSI 2
plot(RSI(Cl(SPY), n = 2))

# Add a 200-day SMA indicator to strategy.st
add.indicator(strategy = strategy.st, 
              name = "SMA", 
              arguments = list(x=quote(Cl(mktdata)), n=200), 
              label = "SMA200")

# Add a 50-day SMA indicator to strategy.st
add.indicator(strategy = strategy.st, 
              name = "SMA", 
              arguments = list(x= quote(Cl(mktdata)), n=50) ,
              label = "SMA50")

# Add an RSI 3 indicator to strategy.st
add.indicator(strategy = strategy.st, 
              name = "RSI", 
              arguments = list(x=quote(Cl(mktdata)), n=3), 
              label = "RSI_3")

# Write the calc_RSI_avg function
calc_RSI_avg <- function(price, n1, n2) {
  RSI_1 <- RSI(price = price, n = 3)
  RSI_2 <- RSI(price = price, n = 4)
  RSI_avg <- (RSI_1 + RSI_2)/2
  colnames(RSI_avg) <- "RSI_avg"
  return(RSI_avg)
}

# Add this function as RSI_3_4 to   strategy with n1 = 3 and n2 = 4
add.indicator(strategy.st, name = "calc_RSI_avg", arguments = list(price = quote(Cl(mktdata)), n1 = 3, n2 = 4), label = "RSI_3_4")

# Declare the DVO function
DVO <- function(HLC, navg = 2, percentlookback = 126) {
  ratio <- Cl(HLC)/((Hi(HLC) + Lo(HLC))/2)
  avgratio <- SMA(ratio, n = navg)
  out <- runPercentRank(avgratio, n = percentlookback, exact.multiplier = 1) * 100
  colnames(out) <- "DVO"
  return(out)
}

# Add the DVO indicator to   strategy
add.indicator(strategy = strategy.st, name = "DVO", 
              arguments = list(HLC = quote(HLC(mktdata)), navg = 2, percentlookback = 126),
              label = "DVO_2_126")

# Use applyIndicators to test out  indicators
test <- applyIndicators(strategy = strategy.st, mktdata = OHLC(SPY))

# Subset  data between Sep. 1 and Sep. 5 of 2013
test_subset <- test["2013-09-01/2013-09-05"]

# Add a sigComparison which specifies that SMA50 must be greater than SMA200
add.signal(strategy.st, name = "sigComparison",
           arguments = list(columns = c("SMA50", "SMA200"), 
                            relationship = "gt"),
           label = "longfilter")

# Add a sigCrossover which specifies that the SMA50 is less than the SMA200 
add.signal(strategy.st, name = "sigCrossover",
           arguments = list(columns = c("SMA50", "SMA200"),
                            relationship = "lt"),
           label = "filterexit")


# Implement a sigThreshold which specifies that DVO_2_126 must be less than 20
add.signal(strategy.st, name = "sigThreshold", 
           arguments = list(column = "DVO_2_126", threshold = 20, 
                            relationship = "lt", cross = FALSE), 
           label = "longthreshold")

# Add a sigThreshold signal to   strategy that specifies that DVO_2_126 must cross above 80
add.signal(strategy.st, name = "sigThreshold", 
            arguments = list(column = "DVO_2_126",threshold = 80, 
                            relationship = "gt",  cross = TRUE), 
           label = "thresholdexit")

# Create   dataset: test
test_init <- applyIndicators(strategy.st, mktdata = OHLC(SPY))
test <- applySignals(strategy = strategy.st, mktdata = test_init)

add.signal(strategy.st, name = "sigFormula",
           
           # Specify that longfilter and longthreshold must be TRUE
           arguments = list(formula = "longfilter & longthreshold", 
                            
                            # Specify that cross must be TRUE
                            cross = TRUE),
           
           # Label it longentry
           label = "longentry")


add.rule(strategy.st, name = "ruleSignal", 
         arguments = list(sigcol = "filterexit", sigval = TRUE, orderqty = "all", 
                          ordertype = "market", orderside = "long", 
                          replace = FALSE, prefer = "Open"), 
         type = "exit")



add.rule(strategy.st, name = "ruleSignal", 
         arguments = list(sigcol = "thresholdexit", sigval = TRUE, orderqty = "all", 
                          ordertype = "market", orderside = "long", 
                          replace = FALSE, prefer = "Open"), 
         type = "exit")


#an entry rule of 1 share when all conditions line up to enter into a position
add.rule(strategy.st, name = "ruleSignal", 
              arguments=list(sigcol = "longentry", sigval = TRUE,
                        orderqty = 1, ordertype = "market",
                        orderside = "long", replace = FALSE, 
                        prefer = "Open"), type = "enter")

#a rule that uses an osFUN to size an entry position
add.rule(strategy = strategy.st, name = "ruleSignal",
         arguments = list(sigcol = "longentry", sigval = TRUE, ordertype = "market",
                          orderside = "long", replace = FALSE, prefer = "Open",
                          osFUN = osMaxDollar, tradeSize = tradesize,
                          maxSize = tradesize), type = "enter")

# Use applyStrategy() to apply   strategy. Save this to out
out <- applyStrategy(strategy = strategy.st, portfolios = portfolio.st)

# Update   portfolio (portfolio.st)
updatePortf(portfolio.st)
daterange <- time(getPortfolio(portfolio.st)$summary)[-1]

# Update   account (account.st)
updateAcct(account.st, daterange)
updateEndEq(account.st)

# Get the tradeStats for   portfolio
tstats <- tradeStats(Portfolios = portfolio.st)

# Print the profit factor
tstats$Profit.Factor

# Use chart.Posn to view   system's performance on SPY
chart.Posn(Portfolio = portfolio.st, Symbol = "SPY")

# Compute the SMA50
sma50 <- SMA(x = Cl(SPY), n = 50)

# Compute the SMA200
sma200 <- SMA(x = Cl(SPY), n = 200)

# Compute the DVO_2_126 with an navg of 2 and a percentlookback of 126
dvo <- DVO(HLC = HLC(SPY), navg = 2, percentlookback = 126)

# Recreate the chart.Posn of the strategy from the previous exercise
chart.Posn(Portfolio = portfolio.st, Symbol = "SPY")

# Overlay the SMA50 on   plot as a blue line
add_TA(sma50, on = 1, col = "blue")

# Overlay the SMA200 on   plot as a red line
add_TA(sma200, on = 1, col = "red")

# Add the DVO_2_126 to the plot in a new window
add_TA(dvo)

portpl <- .blotter$portfolio.firststrat$summary$Net.Trading.PL
SharpeRatio.annualized(portpl, geometric=FALSE)

# Get instrument returns
instrets <- PortfReturns(portfolio.st)

# Compute Sharpe ratio from returns
SharpeRatio.annualized(instrets, geometric = FALSE)

