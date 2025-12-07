################################################################################
#
# Computing native retention rate with four areas
#
################################################################################

library(dplyr)
library(ggplot2)
library(patchwork)
library(scales)
library(forcats)
library(tidyverse)
library(RColorBrewer)
library(colorspace)

# 0) 시도 레벨(주신 case_when 순서 그대로) --------------------------
region_levels17 <- c(
  "Seoul","Busan","Daegu","Incheon","Gwangju","Daejeon","Ulsan","Sejong",
  "Gyeonggi","Gangwon","Chungbuk","Chungnam","Jeonbuk","Jeonnam","Gyeongbuk","Gyeongnam","Jeju"
)
# 범례/팔레트용 전체 레벨(Abroad, NA 포함)
region_levels_all <- c(region_levels17, "Abroad")

# 1) 팔레트에 이름 부여 (팔레트 길이는 18개 이상이라고 가정; 마지막은 NA 전용 회색 추가)
pal_admin <- colorRampPalette(brewer.pal(12, "Paired"))(19)

pal_admin[8]  <- desaturate(pal_admin[8], amount = 1)
pal_admin[3]  <- darken(pal_admin[3], amount = 0.2)
pal_admin[17] <- darken(pal_admin[17], amount = 0.3)

pal_comp <- c("#e31a1c", "#ff7f00", "#1f78b4", "#33a02c", "#ba39a0", "#bebebe")

stopifnot(length(pal_admin) >= length(region_levels17) + 1)
pal_named <- setNames(
  c(pal_admin[seq_len(length(region_levels17) + 1)], "#9E9E9E"),
  region_levels_all
)
#  이때 pal_admin의 앞 18개는: 17개 시도 + Abroad 에 매칭됩니다. "NA"는 회색으로 고정.

wighted <- readRDS("data/pop2020.rds")

# 2) 연도별 자료 로드 --------------------------------------------------------
pop2000 <- readRDS("data/pop2000.rds")
pop2005 <- readRDS("data/pop2005.rds")
pop2010 <- readRDS("data/pop2010.rds")
pop2015 <- readRDS("data/pop2015.rds")


################################################################################
# 함수: calc_retention_5area_youth
# - 출생지 기준 4개 권역 + Total에 대해
# - 0세 기준 정규화된 누적 잔류함수 l(x,i) 계산
# - 개방연령구간을 35+로 두고, 35세까지의 누적 잔류확률로 PPNYR 산출
################################################################################

calc_retention_5area_youth <- function(year, youth_upper = 35) {
  file_path <- paste0("data/pop", year, ".rds")
  
  pop <- readRDS(file_path) %>%
    mutate(
      org_region5 = as.character(org_region5),
      res_region5 = as.character(res_region5)
    )
  
  # --- (1) 성별 Total 추가 ---
  pop_total_sex <- pop %>%
    group_by(org_region5, res_region5, agegr) %>%
    summarise(pop_weighted = sum(pop_weighted, na.rm = TRUE), .groups = "drop") %>%
    mutate(sex = "Total")
  
  pop_all <- bind_rows(pop, pop_total_sex)
  
  # --- (2) retention 계산 ---
  # (2-1) 지역별 retention
  retention_region <- pop_all %>%
    mutate(
      native = if_else(org_region5 == res_region5, "native", "none")
    ) %>%
    group_by(org_region5, sex, agegr, native) %>%
    summarise(pop = sum(pop_weighted, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(
      names_from  = native,
      values_from = pop,
      values_fill = 0
    ) %>%
    mutate(
      retention = native / (native + none),
      year      = year
    )
  
  # (2-2) 전지역 Total retention (org_region5를 그룹에서 제거)
  retention_total <- pop_all %>%
    mutate(
      native = if_else(org_region5 == res_region5, "native", "none")
    ) %>%
    group_by(sex, agegr, native) %>%
    summarise(pop = sum(pop_weighted, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(
      names_from  = native,
      values_from = pop,
      values_fill = 0
    ) %>%
    mutate(
      retention  = native / (native + none),
      year       = year,
      org_region5 = "Total"
    )
  
  # (2-3) 합치기
  retention <- bind_rows(retention_region, retention_total)
  
  # --- (3) 생명표 계산 (개방연령구간 35+ 설정: 35세까지의 l(x,i) 사용) ---
  table <- retention %>%
    mutate(
      age_lower = as.numeric(stringr::str_extract(agegr, "^[0-9]+"))
    ) %>%
    group_by(org_region5, sex) %>%
    arrange(age_lower, .by_group = TRUE) %>%
    # 35세까지의 연령구간만 사용 (35+는 개방구간으로 처리)
    filter(age_lower <= youth_upper) %>%
    mutate(
      # 단면 잔류비율 r(x,i)
      rx = retention,
      # 누적 잔류함수 l(x,i) = r(x,i)/r(0,i)
      lx = rx / dplyr::first(rx),
      Sx = lx,  # 논문에서의 l(x,i)와 동일, 기록용
      # 조건부 잔류확률 p_x(i) = l(x+n,i) / l(x,i)
      px = dplyr::lead(lx) / lx,
      qx = 1 - px,
      # 마지막 구간(개방연령구간) 처리
      qx = dplyr::if_else(is.na(qx), 0, qx),
      px = dplyr::if_else(is.na(px), 1, px),
      # 이론적 범위 밖의 값 보정
      qx = pmin(pmax(qx, 1e-6), 1 - 1e-6),
      px = 1 - qx,
      dx = lx * qx,
      Lx = 5 * (lx - 0.5 * dx),
      Tx = rev(cumsum(rev(Lx))),
      ex = Tx / lx
    ) %>%
    ungroup()
  
  # --- (4) summary 계산 ---
  summary <- table %>%
    group_by(org_region5, sex) %>%
    summarise(
      # 35세(개방연령구간 하한)까지의 누적 잔류확률: PPNYR
      PPNYR = lx[which.max(age_lower)],
      # e0: 최소 연령(대개 0세)의 기대 '잔류기간' (참고용)
      e0    = ex[which.min(age_lower)],
      .groups = "drop"
    ) %>%
    mutate(year = year)
  
  list(
    year      = year,
    retention = retention,
    table     = table,
    summary   = summary
  )
}


# --- (6) 반복 실행 예시 -----------------------------------------------------
years <- c(2000, 2010, 2015, 2020)
results_5area_youth <- purrr::map(years, calc_retention_5area_youth)

retention_5area_youth_all <- purrr::map_dfr(results_5area_youth, "retention", .id = "index")
table_5area_youth_all     <- purrr::map_dfr(results_5area_youth, "table",     .id = "index")
summary_5area_youth_all   <- purrr::map_dfr(results_5area_youth, "summary",   .id = "index")

# --- (1) Wide 형태 요약표 ---------------------------------------------------
table_all_wide_5area_youth <- summary_5area_youth_all %>%
  select(year, org_region5, sex, PPNYR) %>%
  tidyr::pivot_wider(names_from = sex, values_from = PPNYR)

table_all_wide_5area_youth


PPNYR5 <- summary_5area_youth_all %>% 
  filter(org_region5 != "NA") %>% 
  select(year, org_region5, sex, PPNYR) %>% 
  mutate(
    org_region5 = factor(
      org_region5,
      levels = c("Total", "Seoul", "Rest Capital", "Metros", "Provinces", "Abroad")
    ), 
    sex = factor(sex, levels = c("Male", "Female", "Total"))
  ) %>%
  arrange(org_region5, sex) %>% 
  mutate(PPNYR = round(PPNYR, 3)) %>% 
  tidyr::pivot_wider(names_from = c(sex, year), values_from = PPNYR) %>% 
  filter(!is.na(org_region5))

write.csv(PPNYR5, "data/PPNR5.csv", row.names = FALSE)


# --- (2) 요약 진단용 표 만들기 ---------------------------------------------
diagnosis_table_5area_youth <- summary_5area_youth_all %>%
  # PPNYR 테이블과 원자료의 인구수 결합
  left_join(
    retention_5area_youth_all %>%
      group_by(year, org_region5, sex) %>%
      summarise(total_pop = sum(native + none, na.rm = TRUE), .groups = "drop"),
    by = c("year", "org_region5", "sex")
  ) %>%
  # wide 형태로 전환
  select(year, org_region5, sex, PPNYR, total_pop) %>%
  tidyr::pivot_wider(
    names_from = sex,
    values_from = c(PPNYR, total_pop),
    names_sep = "_"
  ) %>%
  mutate(
    # 인구비 및 단순 평균 계산
    pop_ratio_MF   = total_pop_Male / total_pop_Female,
    PPNYR_mean     = (PPNYR_Male + PPNYR_Female) / 2,
    # 인구가중평균
    PPNYR_weighted = (PPNYR_Male * total_pop_Male + PPNYR_Female * total_pop_Female) /
      (total_pop_Male + total_pop_Female),
    # Total이 남녀보다 높은지 여부
    higher_than_both = PPNYR_Total > pmax(PPNYR_Male, PPNYR_Female)
  ) %>%
  arrange(year, desc(PPNYR_Total))

diagnosis_table_5area_youth


# --- (3) qx 진단용 통계표 ---------------------------------------------------
table_5area_youth_all %>%
  group_by(year, org_region5, sex) %>%
  summarise(
    mean_qx = mean(qx, na.rm = TRUE),
    max_qx  = max(qx, na.rm = TRUE),
    min_qx  = min(qx, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year, org_region5) %>%
  view()


# --- (4) 잔류곡선 시각화 ----------------------------------------------------
table_5area_youth_all$org_region5 <- factor(
  table_5area_youth_all$org_region5, 
  levels = c("Seoul", "Rest Capital", "Metros", "Provinces")
)

## 2020 four-areas 
table_5area_youth_all %>%
  filter(
    year == 2020,
    sex == "Total",
    !is.na(org_region5),
    !org_region5 %in% c("Abroad", "NA")
  ) %>%
  ggplot(aes(
    x     = age_lower,
    y     = lx,
    group = org_region5, 
    shape = org_region5,
    color = org_region5,
    lty   = org_region5
  )) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = seq(0, 35, 5),
    labels = c("0-4", "5-9", "10-14", "15-19",
               "20-24", "25-29", "30-34", "35+")
  ) +
  scale_y_continuous(limits = c(0.4, 1)) + 
  labs(
    shape = "Place of birth",
    color = "Place of birth",
    lty   = "Place of birth",
    x     = "Age group",
    y     = "Proportion of Native Youths Retained"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave("graphs/survival_PPNYR_2020_4areas.png", width = 10, height = 6, dpi = 300)


# --- (5) 디버깅/점검용 CSV 출력 ---------------------------------------------
a <- table_5area_youth_all %>%
  filter(
    year == 2020,
    sex == "Total",
    !is.na(org_region5),
    !org_region5 %in% c("Abroad", "NA")
  ) %>%
  select(org_region5, sex, agegr, retention, Sx, lx, qx, age_lower)

write.csv(a, "data/a.csv", row.names = FALSE)
write.csv(table_5area_youth_all, "data/table_5area_youth_all.csv", row.names = FALSE)

table_5area_youth_all
## 35세까지(개방연령구간 35+)를 사용하여 PPNYR 산출
