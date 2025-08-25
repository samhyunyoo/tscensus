################################################################################
#
# Saving all working datasets at once 
#
# Pop Census 2% Sample 1960-2020 
#
################################################################################

library(tidyverse)
library(purrr)

years <- c(1960, 1966, seq(1970, 2020, by = 5))


# ts 계열 불러오기
datasets <- paste0("ts", years)
walk(datasets, ~ assign(.x, readRDS(file.path("data", paste0(.x, ".rds"))), envir = .GlobalEnv))

# pop 계열 불러오기
datasets2 <- paste0("pop", years)
walk(datasets2, ~ assign(.x, readRDS(file.path("data", paste0(.x, ".rds"))), envir = .GlobalEnv))

