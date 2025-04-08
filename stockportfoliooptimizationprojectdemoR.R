# === Step 0: Install packages if needed ===
packages <- c("PortfolioAnalytics", "PerformanceAnalytics", "quantmod", "xts",
              "ROI", "ROI.plugin.quadprog", "ROI.plugin.glpk")

new_pkgs <- packages[!(packages %in% installed.packages()[, "Package"])]
if(length(new_pkgs)) install.packages(new_pkgs)

# === Step 1: Load libraries ===
library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(quantmod)
library(xts)
library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)

# === Step 2: Get historical stock data ===
tickers <- c("AAPL", "GOOG", "MSFT", "AMZN")
getSymbols(tickers, from = "2018-01-01", to = "2023-01-01")

# Combine adjusted close prices into one xts object
prices <- merge(Ad(AAPL), Ad(GOOG), Ad(MSFT), Ad(AMZN))
colnames(prices) <- tickers

# Calculate daily returns
returns <- na.omit(Return.calculate(prices))
head(returns)

# === Step 3: Create Mean-Variance Optimized (MVO) Portfolio ===
# Create initial portfolio specification
port_spec <- portfolio.spec(assets = colnames(returns))

# Add constraints
port_spec <- add.constraint(portfolio = port_spec, type = "full_investment")  # weights sum to 1
port_spec <- add.constraint(portfolio = port_spec, type = "box", min = 0, max = 1)  # no shorting

# Add objective to minimize portfolio standard deviation (risk)
port_spec <- add.objective(portfolio = port_spec, type = "risk", name = "StdDev")

# Optimize the portfolio using ROI plugin
opt_result <- optimize.portfolio(R = returns, portfolio = port_spec, optimize_method = "ROI")

# View results
print(extractWeights(opt_result))
chart.Weights(opt_result)

# Equal-weight portfolio (25% in each asset)
equal_weights <- rep(0.25, length(tickers))
equal_returns <- Return.portfolio(R = returns, weights = equal_weights, rebalance_on = "months")

# Optimized portfolio return
opt_returns <- Return.portfolio(R = returns, weights = extractWeights(opt_result), rebalance_on = "months")

# Combine into one object for comparison
combined_returns <- merge(equal_returns, opt_returns)
colnames(combined_returns) <- c("Equal Weight", "Optimized")

# Plot performance
charts.PerformanceSummary(combined_returns, main = "Equal Weight vs Optimized Portfolio")

# Print basic stats
table.AnnualizedReturns(combined_returns)

# Load the CVXR package
install.packages("CVXR")
library(CVXR)

# Define variables
n_assets <- ncol(returns)
w <- Variable(n_assets)  # decision variable: portfolio weights

# Estimate mean and covariance
mu_hat <- colMeans(returns)
cov_mat <- cov(returns)

# Define the objective: minimize risk (portfolio variance)
objective <- Minimize(quad_form(w, cov_mat))

# Add constraints: weights sum to 1 and no shorting
constraints <- list(
  sum(w) == 1,
  w >= 0
)

# Define and solve the problem
problem <- Problem(objective, constraints)
result <- solve(problem)

# Extract optimal weights
w_opt <- result$getValue(w)
names(w_opt) <- colnames(returns)
print(w_opt)
