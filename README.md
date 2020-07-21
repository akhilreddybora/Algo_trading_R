# Algo_trading_R

quantstrat is currently only available on GitHub. If you want to install it on your own machine, you first need to remotes package.

install.packages("remotes")

Then you can install quantstrat using

remotes::install_github("braverock/quantstrat")


you will obtain data for SPY, an exchange traded fund (ETF) that tracks the top 500 companies in the United States by market cap. This data is from Yahoo! Finance, which is a sufficient source of data for strategies that do not require instantaneous "see the close, buy the close" execution.

you will need to fill in three dates:

    An initialization date for your backtest.
    The start of your data.
    The end of your data.

The initialization date must always come before the start of the data, otherwise there will be serious errors in the output of your backtest.

You should also specify what time zone and currency you will be working with with the functions Sys.setenv() and currency().

a trade size of 100,000 USD in an object called tradesize which determines the amount you wager on each trade.
Second, you will set your initial equity to 100,000 USD in an object called initeq.

Quantstrat requires three different objects to work: an account, a portfolio, and a strategy. 
An account is comprised of portfolios, and a portfolio is comprised of strategies
