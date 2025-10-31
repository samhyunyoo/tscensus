
################################################################################
#
# Computing native retention rate with four areas
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

pop2020
calc_retention_5area <- function(year) {
  file_path <- paste0("data/pop", year, ".rds")
  pop <- readRDS(file_path)
  
  # --- (1) Total 범주 추가 ---
  pop_total <- pop %>%
    group_by(org_region5, res_region5, agegr) %>%
    summarise(pop_weighted = sum(pop_weighted, na.rm = TRUE), .groups = "drop") %>%
    mutate(sex = "Total")
  
  pop_all <- bind_rows(pop, pop_total)
  
  # --- (2) retention 계산 ---
  retention <- pop_all |>
    mutate(
      org_region5 = as.character(org_region5),
      res_region5 = as.character(res_region5),
      native = ifelse(org_region5 == res_region5, "native", "none")
    ) |>
    group_by(org_region5, sex, agegr, native) |>
    summarise(pop = sum(pop_weighted, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = native, values_from = pop, values_fill = 0) |>
    mutate(retention = native / (native + none))
  
  # --- (3) table 계산 (평활화 없음, qx 클리핑 포함) ---
  table <- retention %>%
    group_by(org_region5, sex) %>%
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
    group_by(org_region5, sex) %>%
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
results_5area <- map(years, calc_retention_5area)

# --- (6) 결과 병합 ---
# 각 결과 병합 + 연도 부여
retention_all_5area <- map_dfr(results_5area, "retention", .id = "index") |>
  mutate(year = years[as.integer(index)]) |>
  select(-index) |>
  filter(org_region5 != "Abroad")

table_all_5area <- map_dfr(results_5area, "table", .id = "index") |>
  mutate(year = years[as.integer(index)]) |>
  select(-index) |>
  filter(org_region5 != "Abroad")

summary_all_5area <- map_dfr(results_5area, "summary", .id = "index") |>
  mutate(year = years[as.integer(index)]) |>
  select(-index) |>
  filter(org_region5 != "Abroad")

# wide 형태 요약표
table_all_wide_5area <- summary_all_5area %>%
  select(year, org_region5, sex, PPRR) %>%
  pivot_wider(names_from = sex, values_from = PPRR)

table_all_wide_5area



# 요약 진단용 표 만들기

diagnosis_table_5area <- summary_all_5area %>%
  # PPRR 테이블과 원자료의 인구수 결합
  left_join(
    retention_all_5area %>%
      group_by(year, org_region5, sex) %>%
      summarise(total_pop = sum(native + none, na.rm = TRUE), .groups = "drop"),
    by = c("year", "org_region5", "sex")
  ) %>%
  # wide 형태로 전환
  select(year, org_region5, sex, PPRR, total_pop) %>%
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

diagnosis_table_5area

table_all_5area |> 
  group_by(year, org_region5, sex) |> 
  summarise(mean = mean(qx), 
            max = max(qx), 
            min = min(qx)) |> 
  view()



# Test some graphs 
table_all_5area %>%
  filter(year == 2000, sex == "Total", !is.na(org_region5), !(org_region5 =="Abroad")) %>%
  mutate(age_lower = as.numeric(sub("^(\\d+).*", "\\1", agegr))) %>%  # 연령 하한 추출
  ggplot(aes(x = age_lower, y = lx, group = org_region5, color = org_region5)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 80, 5)) +
  labs(x = "Age group (lower bound)", y = "lx (retained population ratio)") +
  theme_minimal()


write.csv(table_all_5area, "data/table_all_5area.csv", row.names = FALSE)



table_all_5area
## 35세까지 줄여서 사용하기로 함


