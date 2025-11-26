################################################################################
#
# Decomposition Analysis: Missing on CEB = childless women 
#
# 1990, 2000, 2010, 2015, 2020  
#
################################################################################

################################################################################
# Poisson Decomposition with Variable-Level & Category-Level Effects
# 1990, 2010, 2015, 2020
################################################################################

library(tidyverse)
library(broom)
library(gt)

################################################################################
# 1. 데이터 준비
################################################################################

df_use <- readRDS("data/df_use.rds")

df <- df_use %>%
  select(
    year,
    ceb,
    agegrp = agegr,
    educ,
    org_region5,
    res_region5,
    afm_cat = agefmgr,
    w = w_norm
  ) %>%
  mutate(
    afm_cat = ifelse(is.na(afm_cat), "None", afm_cat)
  ) %>%
  filter(year %in% c(1990, 2010, 2015, 2020)) %>%
  filter(!is.na(ceb), !is.na(w))

vars <- c("agegrp", "educ", "org_region5", "res_region5", "afm_cat")

years <- sort(unique(df$year))


################################################################################
# 2. (함수 1) 단일 변수 모형 기반의 Level-by-Level 분해 함수
################################################################################

poisson_decomp_byvar_level <- function(vars, dfA, dfB, wA, wB) {
  results <- list()
  
  for (v in vars) {
    message("  Variable: ", v)
    
    # factor level 통일
    dfA[[v]] <- factor(dfA[[v]])
    dfB[[v]] <- factor(dfB[[v]], levels = levels(dfA[[v]]))
    levs <- levels(dfA[[v]])
    
    # 단일 변수 포아송 모형
    form <- as.formula(paste("ceb ~", v))
    mA <- glm(form, family = poisson(link="log"), data = dfA, weights = wA)
    mB <- glm(form, family = poisson(link="log"), data = dfB, weights = wB)
    
    # level별 비중
    pA <- sapply(levs, function(l) weighted.mean(dfA[[v]] == l, wA))
    pB <- sapply(levs, function(l) weighted.mean(dfB[[v]] == l, wB))
    
    # level별 예측값
    newDat <- data.frame(tmp = levs)
    colnames(newDat) <- v
    
    muA <- predict(mA, newdata = newDat, type = "response")
    muB <- predict(mB, newdata = newDat, type = "response")
    
    # 4) level별 구성효과 / 계수효과 / 합 계산
    comp_l <- (pB - pA) * muA
    coef_l <- pB * (muB - muA)
    total_l <- comp_l + coef_l
    
    res_v <- tibble(
      variable  = v,
      level     = rep(levs, times = 3),     # 각 level을 3번 반복
      component = rep(c("composition", "coefficient", "total"), each = length(levs)),
      value     = c(comp_l, coef_l, total_l)
    )

    results[[v]] <- res_v
  }
  
  bind_rows(results)
}


################################################################################
# 3. 시기별 반복 수행
################################################################################

byvar_level_results <- list()

for (i in 1:(length(years) - 1)) {
  y0 <- years[i]
  y1 <- years[i + 1]
  
  dA <- df %>% filter(year == y0)
  dB <- df %>% filter(year == y1)
  
  message("Decomposing ", y0, " → ", y1)
  
  tmp <- poisson_decomp_byvar_level(vars, dA, dB, dA$w, dB$w)
  tmp <- tmp %>% mutate(period = paste0(y0, "-", y1))
  byvar_level_results[[i]] <- tmp
}

final_byvar_level <- bind_rows(byvar_level_results)


################################################################################
# 4. 출력 테이블 정리
################################################################################

table_out <- final_byvar_level %>%
  mutate(value = round(value, 3)) %>%
  pivot_wider(names_from = period, values_from = value) %>%
  arrange(variable, level, component)

# GT 테이블 출력
gt(table_out)

