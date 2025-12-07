
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
library(tidyverse)
library(scales)
library(RColorBrewer)
library(colorspace)


pal_admin <- colorRampPalette(brewer.pal(12, "Paired"))(19)

pal_admin[8]  <- desaturate(pal_admin[8], amount = 1)
pal_admin[3]  <- darken(pal_admin[3], amount = 0.2)
pal_admin[17] <- darken(pal_admin[17], amount = 0.3)

pal_comp <- c("#e31a1c", "#ff7f00", "#1f78b4", "#33a02c", "#ba39a0", "#bebebe")


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

calc_retention <- function(year, youth_upper = 35) {
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
    tidyr::pivot_wider(names_from = native, values_from = pop, values_fill = 0) |>
    mutate(retention = native / (native + none))
  
  # --- (3) life-table용 table 계산 (본문의 수식에 맞춤) ---
  table <- retention %>%
    # age 하한 추출
    mutate(age_lower = as.numeric(stringr::str_extract(agegr, "^[0-9]+"))) %>%
    group_by(org_admin, sex) %>%
    arrange(age_lower, .by_group = TRUE) %>%
    mutate(
      # 누적 잔류비율 l(x)에 해당하는 값 (정규화 전)
      Sx = retention,
      # x = 0에서 1이 되도록 정규화 (본문: "연령 x가 0세일 때의 잔류비율로 정규화")
      Sx = Sx / dplyr::first(Sx),
      # 인접 연령구간 [x, x+n)에서의 조건부 잔류확률 px = S(x+n)/S(x)
      px = dplyr::lead(Sx) / Sx,
      qx = 1 - px,
      # NA 처리
      qx = dplyr::if_else(is.na(qx), 0, qx),
      px = dplyr::if_else(is.na(px), 1, px),
      # 귀환이동 미식별 및 기술적 안정성 확보를 위한 clipping
      # (본문에서 언급한 "이론적 범위 밖 값" 보정)
      qx = pmin(pmax(qx, 1e-6), 1 - 1e-6),
      px = 1 - qx,
      # 생명표식 누적 잔류확률 l(x) = ∏ px (Sx와 이론상 동일하지만, clipping 반영 버전)
      lx = cumprod(c(1, head(px, -1))),
      dx = lx * qx,
      Lx = 5 * (lx - 0.5 * dx),
      Tx = rev(cumsum(rev(Lx))),
      ex = Tx / lx
    ) %>%
    ungroup()
  
  # --- (4) summary 계산: PPNYR = 35세까지 누적 잔류확률 ---
  summary <- table %>%
    group_by(org_admin, sex) %>%
    summarise(
      # 청년 상한연령(예: 35세)에 최초로 도달하는 구간의 lx를 PPNYR로 사용
      PPNYR = {
        idx <- which(age_lower >= youth_upper)[1]
        if (is.na(idx)) NA_real_ else lx[idx]
      },
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
  select(year, org_admin, sex, PPNYR) %>%
  tidyr::pivot_wider(names_from = sex, values_from = PPNYR)




# 요약 진단용 표 만들기
diagnosis_table <- summary_all %>%
  # PPNYR 테이블과 원자료의 인구수 결합
  left_join(
    retention_all %>%
      group_by(year, org_admin, sex) %>%
      summarise(total_pop = sum(native + none, na.rm = TRUE), .groups = "drop"),
    by = c("year", "org_admin", "sex")
  ) %>%
  select(year, org_admin, sex, PPNYR, total_pop) %>%
  tidyr::pivot_wider(
    names_from = sex,
    values_from = c(PPNYR, total_pop),
    names_sep = "_"
  ) %>%
  mutate(
    pop_ratio_MF = total_pop_Male / total_pop_Female,
    PPNYR_mean = (PPNYR_Male + PPNYR_Female) / 2,
    PPNYR_weighted =
      (PPNYR_Male * total_pop_Male + PPNYR_Female * total_pop_Female) /
      (total_pop_Male + total_pop_Female),
    higher_than_both = PPNYR_Total > pmax(PPNYR_Male, PPNYR_Female)
  ) %>%
  arrange(year, desc(PPNYR_Total))


diagnosis_table



# Test some graphs 
table_all %>%
  filter(year == 2000, sex == "Total", !is.na(org_admin), !(org_admin =="Abroad")) %>%
  mutate(age_lower = as.numeric(sub("^(\\d+).*", "\\1", agegr))) %>%  # 연령 하한 추출
  ggplot(aes(x = age_lower, y = lx, group = org_admin, color = org_admin)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 80, 5)) +
  labs(x = "Age group (lower bound)", y = "lx (retained population ratio)") +
  theme_minimal()


write.csv(table_all, "data/table_all.csv", row.names = FALSE)




## retention 비율의 단조 증가 가정 위배 


