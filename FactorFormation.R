# === EASY: Build packages from source using GitHub ===
# Purpose: Check if there are any build issues and confirm the package installs cleanly

install.packages("remotes")  # Needed to install packages from GitHub
library(remotes)

# Install ExpectedReturns (quant finance factor tools)
install_github("JustinMShea/ExpectedReturns")  # No build errors on my machine

# Install FactorAnalytics (asset pricing models, regressions)
install_github("braverock/FactorAnalytics")  # Also installed without issues

# === MEDIUM: Check for available vignettes ===
# Purpose: See if the packages include long-form documentation or tutorials

# Attempt to list any registered vignettes
vignette(package = "ExpectedReturns")       # → No vignettes found
vignette(package = "FactorAnalytics")       # → No vignettes found

# Manually check if any vignette files exist in the installed package path
list.files(system.file("doc", package = "ExpectedReturns"), recursive = TRUE)  # → character(0)
list.files(system.file("doc", package = "FactorAnalytics"), recursive = TRUE)  # → character(0)

# Conclusion: No vignettes were bundled with the GitHub development versions.
# In a real-world setting, I would message the authors offering to help set up vignette infra.

# === HARDER: Reflect and refactor ===
# Purpose: Show understanding of statistical summaries and package structure

# Based on my prior work (e.g., factor analysis / portfolio return diagnostics),
# I noticed that performance stats like mean return, standard deviation, and Sharpe ratio
# are calculated repeatedly. Here's a reusable helper function to reduce repetition:

summarize_returns <- function(R, scale = 252) {
  data.frame(
    Mean = mean(R, na.rm = TRUE) * scale,
    StdDev = sd(R, na.rm = TRUE) * sqrt(scale),
    Sharpe = mean(R, na.rm = TRUE) / sd(R, na.rm = TRUE) * sqrt(scale)
  )
}

# Example usage (assuming R is a vector of daily returns):
# summarize_returns(Return.calculate(Cl(AAPL)))

# This function could be included in the final package or vignette
# to simplify diagnostics and promote DRY (Don’t Repeat Yourself) coding practices.


