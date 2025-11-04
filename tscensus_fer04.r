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

# 2. Chck asfr  ---------------------------------------------------


allasfr_res <- df_use |> 
  mutate(year = as.factor(year)) |> 
  group_by(year, res_region5, age) |> 
  summarise(asfr = sum(ceb * w_norm, na.rm = TRUE)/sum(w_norm, na.rm = TRUE), .groups = "drop")

allasfr_res |> filter(year ==2010)

allasfr_org <- df_use  |> 
  mutate(year = as.factor(year)) |> 
  group_by(year, org_region5, age) |> 
  summarise(asfr = sum(ceb * w_norm, na.rm = TRUE)/sum(w_norm, na.rm = TRUE), .groups = "drop")


allasfr_res_org <- df_use  |> 
  mutate(year = as.factor(year)) |> 
  group_by(year, res_region5, org_region5, age) |> 
  summarise(asfr = sum(ceb * w_norm, na.rm = TRUE)/sum(w_norm, na.rm = TRUE), .groups = "drop")


allasfr_res |> filter(age == 25)
asfr_res |> filter(age == 25)


allasfr_res |> 
  ggplot(aes(x = age, y = asfr, 
             group = res_region5, color = res_region5, linetype = res_region5)) +
  geom_line(size = 1) +
  geom_vline(xintercept = c(30, 35), color = "gray", linetype = "dashed")+
  
  scale_color_manual(values = pal_comp) +
  scale_y_continuous(limits = c(0, 4.0)) +
  labs(x = "Age", y = "Mean number of children-ever-born", 
       color = "Place of residence", 
       linetype = "Place of residence") +
  theme_minimal() +
  theme(
    legend.position        = "inside",              # ← 새 방식
    legend.position.inside = c(0.98, 0.02),         # 패널 내부 좌표 (오른쪽-아래)
    legend.justification   = c(1, 0),               # 범례 기준점을 오른쪽-아래로
    legend.background      = element_rect(fill = scales::alpha("white", 0.7), color = NA),
    legend.key             = element_blank()
  ) +
  facet_wrap(.~year)

ggsave("graphs/trend_all_ceb_res_2010-2020.png", width = 10, height = 6, scale = 0.7)




allasfr_org |> 
  mutate(year = as.factor(year)) |> 
  ggplot(aes(x = age, y = asfr, 
             group = org_region5, color = org_region5, linetype = org_region5)) +
  geom_line(size = 1) +
  geom_vline(xintercept = c(30, 35), color = "gray", linetype = "dashed")+
  
  scale_color_manual(values = pal_comp) +
  scale_y_continuous(limits = c(0, 4.3)) +
  labs(x = "Age", y = "Mean number of children-ever-born", 
       color = "Place of birth", 
       linetype = "Place of birth") +
  theme_minimal() +
  theme(
    legend.position        = "inside",              # ← 새 방식
    legend.position.inside = c(0.98, 0.02),         # 패널 내부 좌표 (오른쪽-아래)
    legend.justification   = c(1, 0),               # 범례 기준점을 오른쪽-아래로
    legend.background      = element_rect(fill = scales::alpha("white", 0.7), color = NA),
    legend.key             = element_blank()
  ) +
  facet_wrap(.~year)

ggsave("graphs/trend_all_ceb_org_1990-2020.png", width = 10, height = 6, scale = 0.7)


# 3. Chck asfr by place ---------------------------------------------------


allasfr_res |> 
  ggplot(aes(x = age, y = asfr, 
             color = res_region5,
             linetype = year)) +
  geom_line(size = 1) +
  geom_vline(xintercept = c(30, 35), color = "gray", linetype = "dashed")+
  scale_color_manual(values = pal_comp, guide = "none") +
  scale_linetype_manual(values = c(5:1)) +
  scale_y_continuous(limits = c(0, 4.3)) +
  labs(x = "Age", y = "Mean number of children-ever-born", 
       color = "Place of residence", 
       linetype = "Year") +
  theme_minimal() +
  theme(
    legend.position      = "bottom",              # ← 새 방식
    legend.direction     = "horizontal",    # 가로 배치
    legend.justification = "center",
    #    legend.position.inside = c(0.98, 0.3),         # 패널 내부 좌표 (오른쪽-위)
    #    legend.justification   = c(1, 1),               # 범례 기준점을 오른쪽-위로
    legend.background      = element_rect(fill = scales::alpha("white", 0.7), color = NA),
    legend.key             = element_blank()
  ) +
  facet_wrap(.~res_region5)

ggsave("graphs/trend_all_ceb_by_res_period_1990-2020.png", width = 10, height = 6, scale = 0.7)



allasfr_org |> 
  ggplot(aes(x = age, y = asfr, 
             color = org_region5,
             linetype = year)) +
  geom_line(size = 1) +
  geom_vline(xintercept = c(30, 35), color = "gray", linetype = "dashed")+
  scale_color_manual(values = pal_comp, guide = "none") +
  scale_linetype_manual(values = c(5:1)) +
  scale_y_continuous(limits = c(0, 4.3)) +
  labs(x = "Age", y = "Mean number of children-ever-born", 
       color = "Place of birth", 
       linetype = "Year") +
  theme_minimal() +
  theme(
    legend.position      = "bottom",              # ← 새 방식
    legend.direction     = "horizontal",    # 가로 배치
    legend.justification = "center",
    #    legend.position.inside = c(0.98, 0.3),         # 패널 내부 좌표 (오른쪽-위)
    #    legend.justification   = c(1, 1),               # 범례 기준점을 오른쪽-위로
    legend.background      = element_rect(fill = scales::alpha("white", 0.7), color = NA),
    legend.key             = element_blank()
  ) +
  facet_wrap(.~org_region5)

ggsave("graphs/trend_all_ceb_by_org_period_1990-2020.png", width = 10, height = 6, scale = 0.7)


allasfr_res_org |> 
  ggplot(aes(x = age, y = asfr, 
             color = res_region5,
             linetype = year)) +
  geom_line(size = 0.7) +
  geom_vline(xintercept = c(30, 35), color = "gray", linetype = "dashed")+
  scale_color_manual(values = pal_comp, guide = "none") +
  scale_linetype_manual(values = c(5:1)) +
  scale_y_continuous(limits = c(0, 4.3)) +
  labs(x = "Age", y = "Mean number of children-ever-born", 
       color = "Place of birth", 
       linetype = "Year") +
  theme_minimal() +
  theme(
    legend.position      = "bottom",              # ← 새 방식
    legend.direction     = "horizontal",    # 가로 배치
    legend.justification = "center",
    #    legend.position.inside = c(0.98, 0.3),         # 패널 내부 좌표 (오른쪽-위)
    #    legend.justification   = c(1, 1),               # 범례 기준점을 오른쪽-위로
    legend.background      = element_rect(fill = scales::alpha("white", 0.7), color = NA),
    legend.key             = element_blank()
  ) +
  facet_grid(org_region5~res_region5)

ggsave("graphs/trend_all_ceb_by_res_org_period_1990-2020.png", width = 10, height = 6, scale = 0.7)



# 4. General figures  ---------------------------------------------------


allasfr_res <- df_use |> 
  mutate(year = as.factor(year)) |> 
  group_by(year, res_region5, age) |> 
  summarise(asfr = sum(ceb * w_norm, na.rm = TRUE)/sum(w_norm, na.rm = TRUE), .groups = "drop")



## based on res_region5
allfer_res <- df_use |> 
  filter(age >= 45) |>  
  mutate(year = as.factor(year)) |> 
  group_by(cohort, res_region5) |> 
  summarise(cohortfer = sum(ceb * w_norm, na.rm = TRUE)/sum(w_norm, na.rm = TRUE), 
            wgtN= sum(n()*w_norm, na.rm = TRUE),
            .groups = "drop") |> 
  ungroup()|> 
  mutate(wgtTotal = sum(wgtN, na.rm = TRUE), 
         wgtProp = wgtN / wgtTotal) |> 
  select(cohort, res_region5, cohortfer, wgtProp)


allfer_res |> 
  group_by(cohort) |> 
  summarise(CFR = sum(cohortfer * wgtProp, na.rm = TRUE)/sum(wgtProp, na.rm = TRUE), 
            Prop = sum(wgtProp, na.rm = TRUE))

df_use |> 
  filter(age >= 45) |>  
  mutate(year = as.factor(year)) |> 
  group_by(cohort) |> 
  summarise(cohortfer = sum(ceb * w_norm, na.rm = TRUE)/sum(w_norm, na.rm = TRUE), 
            wgtN= sum(n()*w_norm, na.rm = TRUE),
            .groups = "drop") |> 
  group_by(cohort) |> 
  mutate(wgtTotal = sum(wgtN, na.rm = TRUE), 
         Prop_gb_cohort = wgtN / wgtTotal) |> 
  ungroup() |> 
  mutate(Prop_gb_none = wgtN / sum(wgtTotal, na.rm = TRUE))







allfer_org <- df_use |> 
  filter(age >= 45) |>  
  
  mutate(year = as.factor(year)) |> 
  group_by(cohort, org_region5) |> 
  summarise(cohortfer = sum(ceb * w_norm, na.rm = TRUE)/sum(w_norm, na.rm = TRUE), .groups = "drop")

CFR_res_org <- df_use |> 
  filter(age >= 45) |>  
  
  mutate(year = as.factor(year)) |> 
  group_by(cohort, res_region5, org_region5) |> 
  summarise(cohortfer = sum(ceb * w_norm, na.rm = TRUE)/sum(w_norm, na.rm = TRUE), 
            wgtN= sum(n()*w_norm, na.rm = TRUE),
            .groups = "drop") |> 
  ungroup()|> 
  group_by(cohort) |> 
  mutate(wgtProp = wgtN / sum(wgtN, na.rm = TRUE)) |>
  ungroup() |> 
  select(cohort, org_region5, res_region5, cohortfer, wgtProp)

CFR_res_org |> 
  group_by(cohort) |> 
  summarise(all = sum(wgtProp))

write.csv(CFR_res_org, "data/CFR_res_org.csv", row.names = FALSE)




# 1. 여기서 부터 
df <- CFR_res_org |> 
  mutate(year = 49 + as.numeric(str_sub(cohort, 1, 4))) |> 
  rename(prop = wgtProp, 
         ceb_mean = cohortfer) |> 
  select(-cohort) |> 
  filter(year %in% c(1990, 2000, 2010, 2020))

  
df %>%
  group_by(year) %>%
  summarise(
    ceb_mean = weighted.mean(ceb_mean, prop),
    prop = sum(prop),
    .groups = "drop"
  ) %>%
  arrange(year)


# 1. 출생기 기준 기술통계 ----------------------------------------------------------

# --- (A) 출생지 기준 요약표 ---
birth_summary <- df %>%
  group_by(year, org_region5) %>%
  summarise(
    ceb_mean = weighted.mean(ceb_mean, prop),
    prop = sum(prop),
    .groups = "drop"
  ) %>%
  arrange(org_region5, year)

# --- (B) 표 형태 출력 ---
print(birth_summary)

# --- (C) 시각화: 출생지별 완결출산율 변화 ---
ggplot(birth_summary, aes(x = year, y = ceb_mean, color = org_region5)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(title = "출생지별 완결출산율 변화 (45세 이상 여성)",
       x = "연도", y = "평균 출생아수") +
  theme_minimal(base_family = "AppleGothic")

# --- (D) 시각화: 출생지별 구성비 변화 ---
ggplot(birth_summary, aes(x = year, y = prop, color = org_region5)) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "출생지별 인구 구성비 변화",
       x = "연도", y = "비중(%)") +
  theme_minimal(base_family = "AppleGothic")




# 2. 거주지 기준 기술통계 ----------------------------------------------------------

res_summary <- df %>%
  group_by(year, res_region5) %>%
  summarise(
    ceb_mean = weighted.mean(ceb_mean, prop),
    prop = sum(prop),
    .groups = "drop"
  ) %>%
  arrange(res_region5, year)

# 거주지별 완결출산율 추이
ggplot(res_summary, aes(x = year, y = ceb_mean, color = res_region5)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(title = "거주지별 완결출산율 변화 (45세 이상 여성)",
       x = "연도", y = "평균 출생아수") +
  theme_minimal(base_family = "AppleGothic")

# 거주지별 구성비 추이
ggplot(res_summary, aes(x = year, y = prop, color = res_region5)) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "거주지별 인구 구성비 변화",
       x = "연도", y = "비중(%)") +
  theme_minimal(base_family = "AppleGothic")


# 3. 출생지와 거주지 교차 기술통계 ----------------------------------------------------------


cross_summary <- df %>%
  mutate(group = paste(org_region5, res_region5, sep = "-")) %>%
  group_by(year, group) %>%
  summarise(
    ceb_mean = weighted.mean(ceb_mean, prop),
    prop = sum(prop),
    .groups = "drop"
  ) %>%
  arrange(group, year)

# 상위 주요 조합만 시각화 (예: 5개)
top_groups <- cross_summary %>%
  group_by(group) %>%
  summarise(mean_prop = mean(prop)) %>%
  top_n(5, mean_prop) %>%
  pull(group)

ggplot(cross_summary %>% filter(group %in% top_groups),
       aes(x = year, y = ceb_mean, color = group)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(title = "주요 출생지×거주지 조합별 완결출산율 변화",
       x = "연도", y = "평균 출생아수") +
  theme_minimal(base_family = "AppleGothic")


# 4. Kitagawa method  ----------------------------------------------------------

library(dplyr)
library(readr)
library(ggplot2)

# --- Kitagawa 함수 ---
kitagawa <- function(df0, df1, group_vars) {
  df_merge <- inner_join(df0, df1, by = group_vars, suffix = c("0", "1"))
  comp <- sum((df_merge$prop1 - df_merge$prop0) * (df_merge$ceb_mean0 + df_merge$ceb_mean1) / 2)
  rate <- sum((df_merge$ceb_mean1 - df_merge$ceb_mean0) * (df_merge$prop0 + df_merge$prop1) / 2)
  tibble(component = c("composition", "rate", "total"),
         value = c(comp, rate, comp + rate))
}

# --- 연도별 쌍 정의 ---
years <- sort(unique(df$year))

results_stage1 <- list()
results_stage2 <- list()
results_stage3 <- list()

for (i in 1:(length(years)-1)) {
  y0 <- years[i]; y1 <- years[i+1]
  
  df0 <- df %>% filter(year == y0)
  df1 <- df %>% filter(year == y1)
  
  # --- (1단계) 출생지역 기준 ---
  df0_org <- df0 %>%
    group_by(org_region5) %>%
    summarise(ceb_mean = weighted.mean(ceb_mean, prop),
              prop = sum(prop), .groups="drop")
  
  df1_org <- df1 %>%
    group_by(org_region5) %>%
    summarise(ceb_mean = weighted.mean(ceb_mean, prop),
              prop = sum(prop), .groups="drop")
  
  res1 <- kitagawa(df0_org, df1_org, "org_region5") %>%
    mutate(period = paste0(y0, "-", y1),
           stage = "1단계: 출생지 기준")
  
  # --- (2단계) 거주지역 기준 ---
  df0_res <- df0 %>%
    group_by(res_region5) %>%
    summarise(ceb_mean = weighted.mean(ceb_mean, prop),
              prop = sum(prop), .groups="drop")
  
  df1_res <- df1 %>%
    group_by(res_region5) %>%
    summarise(ceb_mean = weighted.mean(ceb_mean, prop),
              prop = sum(prop), .groups="drop")
  
  res2 <- kitagawa(df0_res, df1_res, "res_region5") %>%
    mutate(period = paste0(y0, "-", y1),
           stage = "2단계: 거주지 기준")
  
  # --- (3단계) 출생지×거주지 기준 ---
  res3 <- kitagawa(df0, df1, c("org_region5", "res_region5")) %>%
    mutate(period = paste0(y0, "-", y1),
           stage = "3단계: 출생지×거주지 기준")
  
  results_stage1[[i]] <- res1
  results_stage2[[i]] <- res2
  results_stage3[[i]] <- res3
}

# --- 결과 합치기 ---
final_results <- bind_rows(results_stage1, results_stage2, results_stage3)
print(final_results)
view(final_results)

final_outcome <- final_results |> 
  mutate(value = round(value, 3)) |> 
  group_by(period) |> 
  mutate(percentage = scales:: percent(value / value[component == "total"], accuracy = 0.1), 
         value = as.character(value)) |> 
  ungroup() |> 
  pivot_longer(c(2, 5), names_to = "type", values_to = "value") |> 

  select(stage, period, component, type, value) |> 
  pivot_wider(names_from = period, values_from = value)


library(kableExtra)
write.csv(final_outcome, "data/final_outcome.csv", 
          row.names = FALSE, fileEncoding = "UTF-8")

# --- 시각화 ---
ggplot(final_results, aes(x = period, y = value, fill = component)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~stage, scales = "free_y") +
  labs(title = "Kitagawa 분해 결과: 출생지 → 거주지 → 출생지×거주지 기준",
       x = "기간", y = "완결출산율 변화 기여도") +
  theme_minimal(base_family = "AppleGothic")

