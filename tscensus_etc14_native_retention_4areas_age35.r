
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
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

calc_retention_5area_youth <- function(year) {
  file_path <- paste0("data/pop", year, ".rds")
  pop <- readRDS(file_path)
  
  # --- (1) Total 범주 추가 ---
  pop_total <- pop %>%
    group_by(org_region5, res_region5, agegr) %>%
    summarise(pop_weighted = sum(pop_weighted, na.rm = TRUE), .groups = "drop") %>%
    mutate(sex = "Total")
  
  pop_all <- bind_rows(pop, pop_total)
  
  # --- (2) 연령 상한 35+으로 제한 (35세 초과 구간은 35+로 통합) ---
  pop_all <- pop_all %>%
    mutate(
      # agegr이 "35+" 이상이면 모두 "35+"로 처리
      age_lower = as.numeric(str_extract(agegr, "^[0-9]+")),
      agegr = if_else(age_lower >= 35, "35+", agegr)
    ) %>%
    group_by(org_region5, res_region5, sex, agegr) %>%
    summarise(pop_weighted = sum(pop_weighted, na.rm = TRUE), .groups = "drop")
  
  # --- (3) retention 계산 ---
  retention <- pop_all %>%
    mutate(
      org_region5 = as.character(org_region5),
      res_region5 = as.character(res_region5),
      native = ifelse(org_region5 == res_region5, "native", "none")
    ) %>%
    group_by(org_region5, sex, agegr, native) %>%
    summarise(pop = sum(pop_weighted, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = native, values_from = pop, values_fill = 0) %>%
    mutate(retention = native / (native + none), 
           year = year)
  
  # --- (4) 생명표 계산 (35+까지만 포함) ---
  table <- retention %>%
    group_by(org_region5, sex) %>%
    arrange(as.numeric(str_extract(agegr, "^[0-9]+")), .by_group = TRUE) %>%
    # 35+까지만 포함
    filter(as.numeric(str_extract(agegr, "^[0-9]+")) <= 35 | agegr == "35+") %>%
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
  
  # --- (5) summary 계산 ---
  summary <- table %>%
    group_by(org_region5, sex) %>%
    summarise(
      PPNYR = tail(lx, 1),  # 35세까지 잔류 확률
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

# --- (6) 반복 실행 예시 ---
years <- c(2000, 2010, 2015, 2020)
results_5area_youth<- map(years, calc_retention_5area_youth)

retention_5area_youth_all <- map_dfr(results_5area_youth, "retention", .id = "index")
table_5area_youth_all <- map_dfr(results_5area_youth, "table", .id = "index")
summary_5area_youth_all <- map_dfr(results_5area_youth, "summary", .id = "index")

# --- (1) Wide 형태 요약표 ---
table_all_wide_5area_youth <- summary_5area_youth_all %>%
  select(year, org_region5, sex, PPNYR) %>%
  pivot_wider(names_from = sex, values_from = PPNYR)

table_all_wide_5area_youth


# --- (2) 요약 진단용 표 만들기 ---
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
  pivot_wider(
    names_from = sex,
    values_from = c(PPNYR, total_pop),
    names_sep = "_"
  ) %>%
  mutate(
    # 인구비 및 단순 평균 계산
    pop_ratio_MF = total_pop_Male / total_pop_Female,
    PPNYR_mean = (PPNYR_Male + PPNYR_Female) / 2,
    # 인구가중평균
    PPNYR_weighted = (PPNYR_Male * total_pop_Male + PPNYR_Female * total_pop_Female) /
      (total_pop_Male + total_pop_Female),
    # Total이 남녀보다 높은지 여부
    higher_than_both = PPNYR_Total > pmax(PPNYR_Male, PPNYR_Female)
  ) %>%
  arrange(year, desc(PPNYR_Total))

diagnosis_table_5area_youth


# --- (3) qx 진단용 통계표 ---
table_5area_youth_all %>%
  group_by(year, org_region5, sex) %>%
  summarise(
    mean_qx = mean(qx, na.rm = TRUE),
    max_qx  = max(qx, na.rm = TRUE),
    min_qx  = min(qx, na.rm = TRUE)
  ) %>%
  arrange(year, org_region5) %>%
  view()


# --- (4) 잔류곡선 시각화 ---
library(ggplot2)
unique(
  table_5area_youth_all$org_region5)
table_5area_youth_all$org_region5 <- factor(table_5area_youth_all$org_region5, 
                                            levels = c("Seoul", "Rest Capital", "Metros", "Provinces"))
table_5area_youth_all %>%
  filter(year == 2000, sex == "Total", !is.na(org_region5), !org_region5 %in% c("Abroad", "NA")) %>%
  mutate(age_lower = as.numeric(sub("^(\\d+).*", "\\1", agegr))) %>%
  ggplot(aes(x = age_lower, y = lx, group = org_region5, color = org_region5)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(0, 35, 5), limits = c(0, 35)) +
  labs(
    x = "Age group (lower bound)",
    y = "lx (retained population ratio)",
    title = "Youth Retention Curves by Region (PPNYR, ≤35)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

write.csv(table_all_5area, "data/table_all_5area.csv", row.names = FALSE)



table_all_5area
## 35세까지 줄여서 사용하기로 함


