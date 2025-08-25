

library(purrr)
library(dplyr)
library(stringr)
library(tibble)

# 1) 불러올 대상 이름 정의
years     <- c(1960, 1966, seq(1970, 2020, by = 5))
datasets  <- paste0("ts",  years)
datasets2 <- paste0("pop", years)

targets   <- c(datasets)
# targets   <- c(datasets, datasets2)

# 실제 존재하는 .rds만 대상으로
files      <- file.path("data", paste0(targets, ".rds"))
have_files <- targets[file.exists(files)]

# 1. 각 데이터셋을 불러오며, year 변수를 (데이터셋명 끝 4자리)로 채워서 같은 이름으로 assign
load_and_tag <- function(nm) {
  path <- file.path("data", paste0(nm, ".rds"))
  df   <- readRDS(path)
  yr   <- as.integer(str_extract(nm, "\\d{4}$"))
  
  # year 변수 덮어쓰기(있든 없든 해당 연도로 통일)
  df[["year"]] <- yr
  
  assign(nm, df, envir = .GlobalEnv)
  invisible(NULL)
}

walk(have_files, load_and_tag)

# 2. 각 데이터셋의 colnames를 추출하고,
#    org_admin, org_group5, agefm, ceb 존재 여부를 통합표로 정리
vars_to_check <- c("org_admin", "org_region5", "agefm", "ceb")

colcheck_summary <-
  map_dfr(have_files, function(nm) {
    df   <- get(nm, envir = .GlobalEnv)
    cols <- names(df)
    
    tibble(
      dataset = nm,
      year    = as.integer(str_extract(nm, "\\d{4}$")),
      n_rows  = nrow(df),
      n_cols  = ncol(df),
      has_org_admin  = "org_admin"  %in% cols,
      has_org_region5 = "org_region5" %in% cols,
      has_agefm      = "agefm"      %in% cols,
      has_ceb        = "ceb"        %in% cols,
      colnames       = list(cols)   # 필요 시 unnest_longer(colnames)로 펼칠 수 있음
    )
  })

# 확인
colcheck_summary
colcheck_summary[, c(2, 3, 5, 6, 7, 8)]

as.matrix(colcheck_summary[, c(2, 3, 5, 6, 7, 8)])

