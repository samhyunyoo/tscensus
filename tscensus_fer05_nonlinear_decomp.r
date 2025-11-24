################################################################################
#
# Decomposition Analysis: Missing on CEB = childless women 
#
# 1990, 2000, 2010, 2015, 2020  
#
################################################################################

library(tidyverse)


# 1. Check the data again ---------------------------------------------------
library(ggplot2)
library(dplyr)
library(scales)

pal_comp <- c("#e31a1c", "#ff7f00", "#1f78b4", "#33a02c", "#ba39a0", "#bebebe")
pal_point <- c("#F18C8E", "#FFBF80", "#8FBCDA", "#99D096", "#DC9CD0", "#DEDEDE")

# region별 base color 지정
region_cols <- setNames(pal_comp, unique(asfr_res$res_region5))



df_use <- readRDS("data/df_use.rds")

df_use
colnames(df_use)
table(df_use$agegr, df_use$ceb, useNA = "ifany")
table(df_use$agegr, df_use$mar, useNA = "ifany")



# ------------------------------------------------------------
# (1) 패키지 로드
# ------------------------------------------------------------
library(dplyr)
library(broom)
library(tibble)

# ------------------------------------------------------------
# (2) 사용자 정의 함수: Poisson 분해
# ------------------------------------------------------------
poisson_decomp_weighted <- function(mA, mB, dfA, dfB, wA, wB) {
  
  # 예측값 계산
  muA_A <- predict(mA, newdata = dfA, type = "response")
  muA_B <- predict(mA, newdata = dfB, type = "response")
  muB_B <- predict(mB, newdata = dfB, type = "response")
  
  # 가중 평균
  meanA <- weighted.mean(muA_A, wA)
  meanB <- weighted.mean(muB_B, wB)
  
  # 총 변화량
  total_change <- meanB - meanA
  
  # 구성효과: X 변화만 반영 (계수는 mA 고정)
  comp_effect <- weighted.mean(muA_B, wB) - weighted.mean(muA_A, wA)
  
  # 계수효과: 베타 변화만 반영 (X는 B 시점 고정)
  coef_effect <- weighted.mean(muB_B, wB) - weighted.mean(muA_B, wB)
  
  tibble(
    component = c("composition", "coefficient", "total_change"),
    value = c(comp_effect, coef_effect, total_change)
  )
}

# ------------------------------------------------------------
# (1) 변수별 구성·계수 효과 분해 함수
# ------------------------------------------------------------
poisson_decomp_byvar <- function(vars, dfA, dfB, wA, wB) {
  results <- list()
  
  for (v in vars) {
    # 단일 변수 모델
    mA <- glm(ceb ~ get(v), family = poisson(link="log"), data = dfA, weights = wA)
    mB <- glm(ceb ~ get(v), family = poisson(link="log"), data = dfB, weights = wB)
    
    # 분해 계산
    res <- poisson_decomp_weighted(mA, mB, dfA, dfB, wA, wB)
    res <- res %>% mutate(variable = v)
    results[[v]] <- res
  }
  
  bind_rows(results)
}

# ------------------------------------------------------------
# (3) 데이터 준비
# ------------------------------------------------------------
# 예시 데이터 구조 (사용자 데이터에 맞게 수정)
# df <- read_csv("your_microdata.csv")
# 변수: year, ceb, agegrp, educ, org_region5, res_region5, afm_cat, w

# 필요한 변수만 선택

colnames(df_use)
df <- df_use %>%
  select(year, ceb, agegrp = agegr, educ, org_region5, res_region5, afm_cat= agefmgr, w = w_norm) %>%
  mutate(afm_cat = ifelse(is.na(afm_cat), "None", afm_cat)) |> 
  filter(year %in% c(1990, 2010, 2015, 2020)) |> 
  filter(!is.na(ceb), !is.na(w))




# ------------------------------------------------------------
# (2) 주요 변수 목록 정의
# ------------------------------------------------------------
vars <- c("agegrp", "educ", "org_region5", "res_region5", "afm_cat")

# ------------------------------------------------------------
# (3) 시기별 반복
# ------------------------------------------------------------
years <- sort(unique(df$year))
byvar_results <- list()

for (i in 1:(length(years) - 1)) {
  y0 <- years[i]
  y1 <- years[i + 1]
  
  dA <- df %>% filter(year == y0)
  dB <- df %>% filter(year == y1)
  
  message("Decomposing ", y0, " → ", y1)
  
  tmp <- poisson_decomp_byvar(vars, dA, dB, dA$w, dB$w)
  tmp <- tmp %>% mutate(period = paste0(y0, "-", y1))
  byvar_results[[i]] <- tmp
}

final_byvar <- bind_rows(byvar_results)


summary_total <- final_byvar %>%
  group_by(period, component) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(variable = "전체 합계")

# --- 전체 포함 결과 결합 ---
final_byvar_all <- bind_rows(final_byvar, summary_total)


# ------------------------------------------------------------
# (4) 결과 보기
# ------------------------------------------------------------


library(gt)
final_byvar_all %>%
  select(period, variable, component, value) %>%
  mutate(value = round(value, 3)) |> 
  tidyr::pivot_wider(names_from = period, values_from = value) %>%
  arrange(variable, component) %>%
  gt() %>%
  gt::fmt_number(columns = c("1990-2010", "2010-2015", "2015-2020"), decimals = 3)


df |> filter(year ==2020)
m2020 <- glm(ceb ~ agegrp + educ + org_region5 + res_region5 +afm_cat, 
             family = poisson(link="log"), data = df |> filter(year ==2020), weights = w)
summary(m2020)



