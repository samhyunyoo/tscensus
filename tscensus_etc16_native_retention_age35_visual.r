
################################################################################
#
# Computing native retention rate
#
# Visualization
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


# --- (12) 결과 저장 ---
table_youth_all <- read.csv("../data/table_youth_all.csv")




# 2) 변수 팩터화(순서 고정) ------------------------------------------

colnames(pop2020)

pop2000 <- readRDS("data/pop2000.rds")
pop2005 <- readRDS("data/pop2005.rds")
pop2010 <- readRDS("data/pop2010.rds")
pop2015 <- readRDS("data/pop2015.rds")




library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

calc_retention_youth <- function(year) {
  file_path <- paste0("data/pop", year, ".rds")
  pop <- readRDS(file_path) %>%
    mutate(
      org_admin = as.character(org_admin),
      res_admin = as.character(res_admin)
    )
  
  # --- (1) 성별 Total 범주 추가 ---
  pop_total <- pop %>%
    group_by(org_admin, res_admin, agegr) %>%
    summarise(pop_weighted = sum(pop_weighted, na.rm = TRUE), .groups = "drop") %>%
    mutate(sex = "Total")
  
  pop_all <- bind_rows(pop, pop_total)
  
  # --- (2) 연령 상한 35+으로 제한 (35세 초과는 모두 35+로 통합) ---
  pop_all <- pop_all %>%
    mutate(
      age_lower = as.numeric(str_extract(agegr, "^[0-9]+")),
      agegr = if_else(age_lower >= 35, "35+", agegr)
    ) %>%
    group_by(org_admin, res_admin, sex, agegr) %>%
    summarise(pop_weighted = sum(pop_weighted, na.rm = TRUE), .groups = "drop")
  
  # --- (3) retention 계산 ---
  # (3-1) 행정구역별 retention
  retention_region <- pop_all %>%
    mutate(
      org_admin = as.character(org_admin),
      res_admin = as.character(res_admin),
      native = if_else(org_admin == res_admin, "native", "none")
    ) %>%
    group_by(org_admin, sex, agegr, native) %>%
    summarise(pop = sum(pop_weighted, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = native, values_from = pop, values_fill = 0) %>%
    mutate(
      retention = native / (native + none),
      year = year
    )
  
  # (3-2) 전체 Total (모든 org_admin 합)
  retention_total <- pop_all %>%
    mutate(
      org_admin = as.character(org_admin),
      res_admin = as.character(res_admin),
      native = if_else(org_admin == res_admin, "native", "none")
    ) %>%
    group_by(sex, agegr, native) %>%  # org_admin 제외
    summarise(pop = sum(pop_weighted, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = native, values_from = pop, values_fill = 0) %>%
    mutate(
      retention = native / (native + none),
      org_admin = "Total",  # 전체 행정구역 묶음
      year = year
    )
  
  # (3-3) 합치기
  retention <- bind_rows(retention_region, retention_total)
  
  # --- (4) 생명표 계산 (35+까지만 포함) ---
  table <- retention %>%
    group_by(org_admin, sex) %>%
    arrange(as.numeric(str_extract(agegr, "^[0-9]+")), .by_group = TRUE) %>%
    filter(as.numeric(str_extract(agegr, "^[0-9]+")) <= 35 | agegr == "35+") %>%
    mutate(
      Sx = retention,
      Sx = Sx / first(Sx),
      px = lead(Sx) / Sx,
      qx = 1 - px,
      qx = if_else(is.na(qx), 0, qx),
      px = if_else(is.na(px), 1, px),
      qx = pmin(pmax(qx, 1e-6), 1 - 1e-6),
      px = 1 - qx,
      lx = cumprod(c(1, head(px, -1))),
      dx = lx * qx,
      Lx = 5 * (lx - 0.5 * dx),
      Tx = rev(cumsum(rev(Lx))),
      ex = Tx / lx
    ) %>%
    ungroup() %>%
    mutate(year = year)
  
  # --- (5) summary 계산 ---
  summary <- table %>%
    group_by(org_admin, sex) %>%
    summarise(
      PPNYR = tail(lx, 1),
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


# --- (6) 반복 수행 ---
years <- c(2000, 2010, 2015, 2020)
results_youth <- map(years, calc_retention_youth)

# --- (7) 결과 병합 ---
retention_youth_all <- map_dfr(results_youth, "retention", .id = "index") %>%
  mutate(year = years[as.integer(index)]) %>%
  select(-index) %>%
  filter(org_admin != "Abroad")

table_youth_all <- map_dfr(results_youth, "table", .id = "index") %>%
  mutate(year = years[as.integer(index)]) %>%
  select(-index) %>%
  filter(org_admin != "Abroad")

summary_youth_all <- map_dfr(results_youth, "summary", .id = "index") %>%
  mutate(year = years[as.integer(index)]) %>%
  select(-index) %>%
  filter(org_admin != "Abroad")


# --- (8) Wide 형태 요약표 ---
table_all_wide_youth <- summary_youth_all %>%
  select(year, org_admin, sex, PPNYR) %>%
  pivot_wider(names_from = sex, values_from = PPNYR)

table_all_wide_youth 


PPNYR17 <- summary_youth_all %>%
  select(year, org_admin, sex, PPNYR) |> 
  mutate(org_admin = factor(org_admin, levels = c("Total", region_levels17)), 
         sex = factor(sex, levels = c("Male", "Female", "Total"))) |>
  arrange(org_admin, sex) |> 
  pivot_wider(names_from = c(sex, year), values_from = PPNYR) |> 
  filter(!is.na(org_admin))

write.csv(PPNYR17, "data/PPNR17.csv", row.names = FALSE)

# --- (9) 요약 진단용 표 만들기 ---
diagnosis_table_youth <- summary_youth_all %>%
  left_join(
    retention_youth_all %>%
      group_by(year, org_admin, sex) %>%
      summarise(total_pop = sum(native + none, na.rm = TRUE), .groups = "drop"),
    by = c("year", "org_admin", "sex")
  ) %>%
  select(year, org_admin, sex, PPNYR, total_pop) %>%
  pivot_wider(
    names_from = sex,
    values_from = c(PPNYR, total_pop),
    names_sep = "_"
  ) %>%
  mutate(
    pop_ratio_MF = total_pop_Male / total_pop_Female,
    PPNYR_mean = (PPNYR_Male + PPNYR_Female) / 2,
    PPNYR_weighted = (PPNYR_Male * total_pop_Male + PPNYR_Female * total_pop_Female) /
      (total_pop_Male + total_pop_Female),
    higher_than_both = PPNYR_Total > pmax(PPNYR_Male, PPNYR_Female)
  ) %>%
  arrange(year, desc(PPNYR_Total))

diagnosis_table_youth


# --- (10) qx 진단용 표 ---
table_youth_all %>%
  group_by(year, org_admin, sex) %>%
  summarise(
    mean_qx = mean(qx, na.rm = TRUE),
    max_qx = max(qx, na.rm = TRUE),
    min_qx = min(qx, na.rm = TRUE)
  ) %>%
  view()


# --- (11) 그래프 예시 ---
table_youth_all %>%
  filter(year == 2000, sex == "Total", !is.na(org_admin), !org_admin %in% c("Abroad", "NA")) %>%
  mutate(age_lower = as.numeric(sub("^(\\d+).*", "\\1", agegr))) %>%
  ggplot(aes(x = age_lower, y = lx, group = org_admin, color = org_admin)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(0, 35, 5), limits = c(0, 35)) +
  labs(
    x = "Age group (lower bound)",
    y = "lx (retained population ratio)",
    title = "Youth Retention Curves by Region (PPNYR, ≤35)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# --- (12) 결과 저장 ---
write.csv(table_youth_all, "data/table_youth_all.csv", row.names = FALSE)





## retention 비율의 단조 증가 가정 위배 


