################################################################################
#
# Exploratory Data Analysis II: Missing on CEB = childless women 
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
  summarise(cohortfer = sum(ceb * w_norm, na.rm = TRUE)/sum(w_norm, na.rm = TRUE), .groups = "drop")

allfer_org <- df_use |> 
  filter(age >= 45) |>  
  
  mutate(year = as.factor(year)) |> 
  group_by(cohort, org_region5) |> 
  summarise(cohortfer = sum(ceb * w_norm, na.rm = TRUE)/sum(w_norm, na.rm = TRUE), .groups = "drop")

allfer_res_org <- df_use |> 
  filter(age >= 45) |>  
  
  mutate(year = as.factor(year)) |> 
  group_by(cohort, res_region5, org_region5) |> 
  summarise(cohortfer = sum(ceb * w_norm, na.rm = TRUE)/sum(w_norm, na.rm = TRUE), .groups = "drop")


allfer_res |> 
  ggplot(aes(x = cohort, y = cohortfer, 
             group = res_region5, color = res_region5, 
             fill = res_region5, shape = res_region5)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_shape_manual(values = c(21:25)) +
  scale_fill_manual(values = pal_point) +
  scale_color_manual(values = pal_comp) +
  scale_y_continuous(limits = c(0, 4.3)) +
  labs(x = "Birth cohort", y = "Fertility", 
       color = "Place of residence", 
       fill = "Place of residence", 
       shape = "Place of residence") +
  theme_minimal() +
  theme(
    legend.position        = "inside",              # ← 새 방식
    legend.position.inside = c(0.98, 0.98),         # 패널 내부 좌표 (오른쪽-위)
    legend.justification   = c(1, 1),               # 범례 기준점을 오른쪽-위로
    legend.background      = element_rect(fill = scales::alpha("white", 0.7), color = NA),
    legend.key             = element_blank()
  )

ggsave("graphs/trend_all_fer_res_1931-1975.png", width = 10, height = 6, scale = 0.7)

allfer_org |> #  filter(org_region5 != "Abroad") |> 
  ggplot(aes(x = cohort, y = cohortfer, 
             group = org_region5, color = org_region5, 
             fill = org_region5, shape = org_region5)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_shape_manual(values = c(21:25, 19)) +
  scale_fill_manual(values = pal_point) +
  scale_color_manual(values = pal_comp) +
  scale_y_continuous(limits = c(0, 4.3)) +
  labs( x = "Birth cohort", y = "Fertility", 
        color = "Place of birth", 
        fill = "Place of birth", 
        shape = "Place of birth") +
  theme_minimal() +
  theme(
    legend.position        = "inside",              # ← 새 방식
    legend.position.inside = c(0.98, 0.98),         # 패널 내부 좌표 (오른쪽-위)
    legend.justification   = c(1, 1),               # 범례 기준점을 오른쪽-위로
    legend.background      = element_rect(fill = scales::alpha("white", 0.7), color = NA),
    legend.key             = element_blank()
  )

ggsave("graphs/trend_all_fer_org_1931-1975.png", width = 10, height = 6, scale = 0.7)


## trends in completed fertility by origin for each residence area
allfer_res_org |> # filter(org_region5 != "Abroad") |> 
  ggplot(aes(x = cohort, y = cohortfer, 
             group = org_region5, color = org_region5, 
             fill = org_region5, shape = org_region5)) +
  geom_line(size = 0.7) +
  geom_point(size = 1.5) +
  scale_shape_manual(values = c(21:25, 19)) +
  scale_fill_manual(values = pal_point) +
  scale_color_manual(values = pal_comp) +
  scale_y_continuous(limits = c(0, 4.3)) +
  scale_x_discrete(labels = \(x) ifelse(seq_along(x) %% 2 == 1, x, "")) +
  
  theme_minimal() +
  labs( x = "Birth cohort", y = "Fertility", 
        color = "Place of birth", 
        fill = "Place of birth", 
        shape = "Place of birth") +  facet_wrap(.~res_region5) +
  theme(
    legend.position      = "bottom",        # 하단
    legend.direction     = "horizontal",    # 가로 배치
    legend.justification = "center",
    legend.box.margin    = margin(t = 4),
    legend.key.height    = unit(10, "pt"),
    legend.key.width     = unit(14, "pt"),
    plot.margin          = margin(6, 8, 6, 6)
  ) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE))

ggsave("graphs/trend_all_fer_res_x_org_1931-1975.png", width = 10, height = 6, scale = 0.8)


## trends in completed fertility by residence for each origin 
allfer_res_org |> # filter(org_region5 != "Abroad") |> 
  ggplot(aes(x = cohort, y = cohortfer, 
             group = res_region5, color = res_region5, 
             fill = res_region5, shape = res_region5)) +
  geom_line(size = 0.7) +
  geom_point(size = 1.5) +
  scale_shape_manual(values = c(21:25)) +
  scale_fill_manual(values = pal_point) +
  scale_color_manual(values = pal_comp) +
  scale_y_continuous(limits = c(0, 4.3)) +
  scale_x_discrete(labels = \(x) ifelse(seq_along(x) %% 2 == 1, x, "")) +
  theme_minimal() +
  labs( x = "Birth cohort", y = "Fertility", 
        color = "Place of birth", 
        fill = "Place of birth", 
        shape = "Place of birth") +
  facet_wrap(.~org_region5) +
  theme(
    legend.position        = "inside",              # ← 새 방식
    legend.position.inside = c(0.98, 0.02),         # 패널 내부 좌표 (오른쪽-아래)
    legend.justification   = c(1, 0),               # 범례 기준점을 오른쪽-아래로
    legend.background      = element_rect(fill = scales::alpha("white", 0.7), color = NA),
    legend.key             = element_blank()
  ) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE))

ggsave("graphs/trend_all_fer_org_x_res_1931-1975.png", width = 10, height = 6, scale = 0.8)




