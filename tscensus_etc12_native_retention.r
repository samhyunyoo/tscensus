
################################################################################
#
# Computing native retention rate
#
# 
#
################################################################################


library(dplyr)
library(ggplot2)
library(patchwork)
library(scales)
library(forcats)

# 0) 시도 레벨(주신 case_when 순서 그대로) --------------------------
region_levels17 <- c(
  "Seoul","Busan","Daegu","Incheon","Gwangju","Daejeon","Ulsan","Sejong",
  "Gyeonggi","Gangwon","Chungbuk","Chungnam","Jeonbuk","Jeonnam","Gyeongbuk","Gyeongnam","Jeju"
)
# 범례/팔레트용 전체 레벨(Abroad, NA 포함)
region_levels_all <- c(region_levels17, "Abroad")

# 1) 팔레트에 이름 부여 (팔레트 길이는 18개 이상이라고 가정; 마지막은 NA 전용 회색 추가)
stopifnot(length(pal_admin) >= length(region_levels17) + 1)
pal_named <- setNames(c(pal_admin[seq_len(length(region_levels17) + 1)], "#9E9E9E"),
                      region_levels_all)
#  이때 pal_admin의 앞 18개는: 17개 시도 + Abroad 에 매칭됩니다. "NA"는 회색으로 고정.

wighted<- readRDS("data/pop2020.rds")

# 2) 변수 팩터화(순서 고정) ------------------------------------------

colnames(pop2020)

pop2000 <- readRDS("data/pop2000.rds")
pop2005 <- readRDS("data/pop2005.rds")
pop2010 <- readRDS("data/pop2010.rds")
pop2015 <- readRDS("data/pop2015.rds")

library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

calc_retention <- function(year) {
  file_path <- paste0("data/pop", year, ".rds")
  pop <- readRDS(file_path)
  
  # --- (1) Total 범주 추가 ---
  pop_total <- pop %>%
    group_by(org_admin, res_admin, agegr) %>%
    summarise(pop_weighted = sum(pop_weighted, na.rm = TRUE), .groups = "drop") %>%
    mutate(sex = "Total")
  
  pop_all <- bind_rows(pop, pop_total)
  
  # --- (2) retention 계산 ---
  retention <- pop_all |>
    mutate(
      org_admin = as.character(org_admin),
      res_admin = as.character(res_admin),
      native = ifelse(org_admin == res_admin, "native", "none")
    ) |>
    group_by(org_admin, sex, agegr, native) |>
    summarise(pop = sum(pop_weighted, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = native, values_from = pop, values_fill = 0) |>
    mutate(retention = native / (native + none))
  
  # --- (3) table 계산 (평활화 없음, qx 클리핑 포함) ---
  table <- retention %>%
    group_by(org_admin, sex) %>%
    arrange(as.numeric(str_extract(agegr, "^[0-9]+")), .by_group = TRUE) %>%
    mutate(
      Sx = retention,
      Sx = Sx / first(Sx),
      px = lead(Sx) / Sx,
      qx = 1 - px,
      qx = if_else(is.na(qx), 0, qx),
      px = if_else(is.na(px), 1, px),
      # --- qx를 0~1 사이로 제한 ---
      qx = pmin(pmax(qx, 1e-6), 1 - 1e-6),
      px = 1 - qx,
      # --- 생명표 계산 ---
      lx = cumprod(c(1, head(px, -1))),
      dx = lx * qx,
      Lx = 5 * (lx - 0.5 * dx),
      Tx = rev(cumsum(rev(Lx))),
      ex = Tx / lx
    ) %>%
    ungroup()
  
  # --- (4) summary 계산 ---
  summary <- table %>%
    group_by(org_admin, sex) %>%
    summarise(
      PPRR = tail(lx, 1),
      e0 = ex[which.min(as.numeric(str_extract(as.character(agegr), "^[0-9]+")))],
      .groups = "drop"
    ) %>%
    mutate(year = year)
  
  return(list(
    year = year,
    retention = retention,
    table = table,
    summary = summary
  ))
}

# --- (5) 반복 수행 ---
years <- c(2000, 2010, 2015, 2020)
results <- map(years, calc_retention)

# --- (6) 결과 병합 ---
# 각 결과 병합 + 연도 부여
retention_all <- map_dfr(results, "retention", .id = "index") |>
  mutate(year = years[as.integer(index)]) |>
  select(-index) |>
  filter(org_admin != "Abroad")

table_all <- map_dfr(results, "table", .id = "index") |>
  mutate(year = years[as.integer(index)]) |>
  select(-index) |>
  filter(org_admin != "Abroad")

summary_all <- map_dfr(results, "summary", .id = "index") |>
  mutate(year = years[as.integer(index)]) |>
  select(-index) |>
  filter(org_admin != "Abroad")

# wide 형태 요약표
table_all_wide <- summary_all %>%
  select(year, org_admin, sex, PPRR) %>%
  pivot_wider(names_from = sex, values_from = PPRR)

table_all_wide



# 요약 진단용 표 만들기

diagnosis_table <- summary_all %>%
  # PPRR 테이블과 원자료의 인구수 결합
  left_join(
    retention_all %>%
      group_by(year, org_admin, sex) %>%
      summarise(total_pop = sum(native + none, na.rm = TRUE), .groups = "drop"),
    by = c("year", "org_admin", "sex")
  ) %>%
  # wide 형태로 전환
  select(year, org_admin, sex, PPRR, total_pop) %>%
  pivot_wider(
    names_from = sex,
    values_from = c(PPRR, total_pop),
    names_sep = "_"
  ) %>%
  mutate(
    # 인구비와 단순 평균 계산
    pop_ratio_MF = total_pop_Male / total_pop_Female,
    PPRR_mean = (PPRR_Male + PPRR_Female) / 2,
    # 인구가중평균
    PPRR_weighted = (PPRR_Male * total_pop_Male + PPRR_Female * total_pop_Female) /
      (total_pop_Male + total_pop_Female),
    # Total이 남녀보다 높은지 여부
    higher_than_both = PPRR_Total > pmax(PPRR_Male, PPRR_Female)
  ) %>%
  arrange(year, desc(PPRR_Total))

diagnosis_table
view(diagnosis_table)

table_all |> 
  group_by(year, org_admin, sex) |> 
  summarise(mean = mean(qx), 
            max = max(qx), 
            min = min(qx)) |> 
  view()
