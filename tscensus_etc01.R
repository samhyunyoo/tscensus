################################################################################
#
# Saving all working datasets at once 
#
# Pop Census 2% Sample 1960-2020 
#
################################################################################


library(purrr)

years <- c(1960, 1966, seq(1970, 2000, by = 5))
datasets <- paste0("ts", years)

if (!dir.exists("data")) dir.create("data")

walk(datasets, ~ saveRDS(get(.x), file = file.path("data", paste0(.x, ".rds"))))

datasets2 <- paste0("pop", years)
walk(datasets2, ~ saveRDS(get(.x), file = file.path("data", paste0(.x, ".rds"))))
