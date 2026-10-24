###00 install necessary package###
# CRAN (recommended)
install.packages("missForest")

# Development version (from GitHub)
setwd("C:/Users/nilsv/OneDrive/Bureaublad/UniversiteitUtrecht/datawrangling/assignment2")



###01 perform the missForest ###
#Load parkinson data
park <- read.csv("parkinsonsdata/parkinsons.data", header = TRUE)
park <- park[, !(names(park) %in% c("name", "status"))]   # drop ID + response, as in the paper
dim(park)   # should be 195 x 22

library(missForest)

rates  <- c(0.10, 0.20, 0.30)
n_sims <- 50          # set to 2 first to test, then 50 (~15 min on Parkinson's)
set.seed(2026)        # once, outside the loops: the whole run is reproducible

results <- list()

for (p in rates) {
  for (s in seq_len(n_sims)) {
    park_mis <- prodNA(park, noNA = p)                      # new random NA pattern each sim
    imp      <- missForest(park_mis, ntree = 100, backend = "randomForest")
    
    results[[length(results) + 1]] <- data.frame(
      rate = p,
      sim  = s,
      true = mixError(imp$ximp, park_mis, park),            # true NRMSE on deleted cells
      oob  = imp$OOBerror                                    # missForest's own estimate
    )
  }
}

results <- do.call(rbind, results)   # one long data frame: 150 rows

aggregate(cbind(true, oob) ~ rate, data = results, FUN = mean)

###02 create a table with the main results ###

library(dplyr)

summary_tab <- results %>%
  group_by(rate) %>%
  summarise(
    nrmse_true = mean(true),                      # compare to Figure 1 (black bar)
    se_true    = sd(true) / sqrt(n()),            # paper reports standard errors
    nrmse_oob  = mean(oob),                       # compare to Figure 4 (plus sign)
    abs_diff   = mean(abs(true - oob)),           # how well OOB tracks the truth
    rel_diff   = mean(abs(true - oob) / true),    # paper claims ~10–15% on average
    .groups    = "drop"
  ) %>%
  mutate(paper_nrmse = c(NA, NA, NA))             # fill in by hand from Figure 1

knitr::kable(
  summary_tab, digits = 3,
  col.names = c("Missing rate", "NRMSE (true)", "SE", "NRMSE (OOB)",
                "|true − OOB|", "Rel. diff", "Paper NRMSE (≈)")
)

###03  recreate figure 4 ###

library(tidyr)
library(ggplot2)

# per-run difference: positive = OOB underestimates the true error
results$diff <- results$true - results$oob

# mean true error and mean OOB estimate per rate, in long format for the overlay
means <- results %>%
  group_by(rate) %>%
  summarise(true = mean(true), oob = mean(oob), .groups = "drop") %>%
  pivot_longer(c(true, oob), names_to = "type", values_to = "value")

fig4 <- ggplot(results, aes(x = factor(rate), y = diff)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_boxplot(width = 0.5, outlier.shape = 1) +
  geom_point(data = means, aes(y = value, shape = type), size = 3) +
  scale_shape_manual(
    values = c(true = 1, oob = 3),                     # 1 = open circle, 3 = plus, as in the paper
    labels = c(true = "mean true error", oob = "mean OOB estimate")
  ) +
  labs(
    x = "Proportion of missing values",
    y = expression(err[true] - widehat(err)[OOB]),
    shape = NULL,
    title = "Parkinson's data - my replication"
  ) +
  theme_classic() +
  theme(legend.position = "bottom")

fig4
ggsave("fig4_parkinsons.pdf", fig4, width = 5, height = 4)