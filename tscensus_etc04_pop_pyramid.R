
################################################################################
#
# Pop pyramid 
#
#  
#
################################################################################




# 1. data work ------------------------------------------------------------


library(tidyverse)
library(scales)
library(RColorBrewer)
library(colorspace)


pal_admin <- colorRampPalette(brewer.pal(12, "Paired"))(19)

pal_admin[8]  <- desaturate(pal_admin[8], amount = 1)
pal_admin[3]  <- darken(pal_admin[3], amount = 0.2)
pal_admin[17] <- darken(pal_admin[17], amount = 0.3)

pal_comp <- c("#e31a1c", "#ff7f00", "#1f78b4", "#33a02c", "#ba39a0", "#bebebe")

table(pop2020$org_admin)

ggplot(pop2020, aes(x = agegr, y = pop_signed, fill = res_admin)) +
  geom_col(width = 0.9) +
  coord_flip() +
  scale_y_continuous(labels = ~ scales::comma(abs(.x))) +
  # check your color combinations
  scale_fill_manual(values = pal_admin, drop = FALSE) +
  labs(x = NULL, y = NULL, fill = "place of residence") +
  theme_minimal() +
  theme(
    plot.title    = element_text(face = "bold", size = 16),
    axis.text.y   = element_text(size = 10),
    legend.position = "bottom",
    panel.grid.major.y = element_blank()
  )

ggsave("graphs/prd2000_res_admin.png", width = 10, height = 5)




library(dplyr)
library(ggplot2)
library(patchwork)
library(scales)

# 2. For all population in 2020 --------------------------------------------


# 1) 거주지 기준 피라미드 (범례 숨김)
g_res <- ggplot(pop2020, aes(x = agegr, y = pop_signed, fill = res_admin)) +
  geom_col(width = 0.9) +
  coord_flip() +
  scale_y_continuous(labels = ~ scales::comma(abs(.x))) +
  # check your color combinations
  scale_fill_manual(values = pal_admin, drop = FALSE, guide = "none") +
  labs(title = "Place of residence", 
       x = NULL, y = NULL, fill = "place of residence") +
  theme_minimal() 

# 2) 출생지 기준 피라미드 (이 플롯의 범례만 사용)
g_org <- ggplot(pop2020, aes(x = agegr, y = pop_signed, fill = org_admin)) +
  geom_col(width = 0.9) +
  coord_flip() +
  scale_y_continuous(labels = ~ scales::comma(abs(.x))) +
  # check your color combinations
  scale_fill_manual(values = pal_admin, drop = TRUE) +
  labs(title = "Place of birth", 
       x = NULL, y = NULL, fill = "Region") +
  theme_minimal() 

# 3) 나란히 배치 + 하단 중앙에 단일 범례 (org_admin)
combined <- (g_res + g_org) +
  plot_layout(guides = "collect") &                                       # 범례 수집
  theme(legend.position = "bottom", legend.justification = "center")

combined
## 4) 저장 (선택)
ggsave("graphs/pyramids_res_vs_origin_2020.png", combined, width = 10, height = 6, dpi = 300)





# 2. For Seoul population in 2020 --------------------------------------------
# (권장) 양쪽 동일 스케일
## 0) 스택 높이 기준 최대값(여유 5%)
agg <- pop2020 %>%
  filter(org_admin == "Seoul") |> 
  group_by(agegr) %>%
  summarise(
    stack_pos = sum(pmax(pop_signed, 0), na.rm = TRUE),   # 양수 스택 합
    stack_neg = sum(pmax(-pop_signed, 0), na.rm = TRUE),  # 음수 스택 합(절대값)
    .groups = "drop"
  )

max_abs <- max(agg$stack_pos, agg$stack_neg, na.rm = TRUE) * 1.05
y_scale <- scale_y_continuous(limits = c(-max_abs, max_abs),
                              labels = ~ comma(abs(.x)))

# 1) 거주지 기준 피라미드 (범례 숨김)
seoul_org <- pop2020 |> 
  filter(org_admin == "Seoul") |> 
  ggplot(aes(x = agegr, y = pop_signed, fill = res_admin)) +
  geom_col(width = 0.9) +
  coord_flip() +
  scale_y_continuous(limits = c(-max_abs, max_abs), labels = ~ scales::comma(abs(.x))) +
  # check your color combinations
  scale_fill_manual(values = pal_admin, drop = FALSE, guide = "none") +
  labs(title = "Seoul origin", 
       x = NULL, y = NULL, fill = "place of residence") +
  theme_minimal() 

# 2) 출생지 기준 피라미드 (이 플롯의 범례만 사용)
seoul_res <- pop2020 |> 
  filter(res_admin == "Seoul") |> 
  ggplot(aes(x = agegr, y = pop_signed, fill = org_admin)) +
  geom_col(width = 0.9) +
  coord_flip() +
  scale_y_continuous(limits = c(-max_abs, max_abs), labels = ~ scales::comma(abs(.x))) +
  # check your color combinations
  scale_fill_manual(values = pal_admin, drop = TRUE) +
  labs(title = "Seoul residents", 
       x = NULL, y = NULL, fill = "Region") +
  theme_minimal() 

# 3) 나란히 배치 + 하단 중앙에 단일 범례 (org_admin)
combined2 <- (seoul_org + seoul_res) +
  plot_layout(guides = "collect") &                                       # 범례 수집
  theme(legend.position = "bottom", legend.justification = "center")

combined2
## 4) 저장 (선택)
ggsave("graphs/pmd_Seoul_origin_res_2020.png", combined, width = 10, height = 6, dpi = 300)





# 3. For Busan population in 2020 --------------------------------------------
# (권장) 양쪽 동일 스케일
## 0) 스택 높이 기준 최대값(여유 5%)
agg <- pop2020 %>%
  filter(org_admin == "Busan") |> 
  group_by(agegr) %>%
  summarise(
    stack_pos = sum(pmax(pop_signed, 0), na.rm = TRUE),   # 양수 스택 합
    stack_neg = sum(pmax(-pop_signed, 0), na.rm = TRUE),  # 음수 스택 합(절대값)
    .groups = "drop"
  )

max_abs <- max(agg$stack_pos, agg$stack_neg, na.rm = TRUE) * 1.05
y_scale <- scale_y_continuous(limits = c(-max_abs, max_abs),
                              labels = ~ comma(abs(.x)))

# 1) 거주지 기준 피라미드 (범례 숨김)
busan_org <- pop2020 |> 
  filter(org_admin == "Busan") |> 
  ggplot(aes(x = agegr, y = pop_signed, fill = res_admin)) +
  geom_col(width = 0.9) +
  coord_flip() +
  scale_y_continuous(limits = c(-max_abs, max_abs), labels = ~ scales::comma(abs(.x))) +
  # check your color combinations
  scale_fill_manual(values = pal_admin, drop = FALSE, guide = "none") +
  labs(title = "Busan origin", 
       x = NULL, y = NULL, fill = "place of residence") +
  theme_minimal() 

# 2) 출생지 기준 피라미드 (이 플롯의 범례만 사용)
busan_res <- pop2020 |> 
  filter(res_admin == "Busan") |> 
  ggplot(aes(x = agegr, y = pop_signed, fill = org_admin)) +
  geom_col(width = 0.9) +
  coord_flip() +
  scale_y_continuous(limits = c(-max_abs, max_abs), labels = ~ scales::comma(abs(.x))) +
  # check your color combinations
  scale_fill_manual(values = pal_admin, drop = TRUE) +
  labs(title = "Busan residents", 
       x = NULL, y = NULL, fill = "Region") +
  theme_minimal() 

# 3) 나란히 배치 + 하단 중앙에 단일 범례 (org_admin)
combined3 <- (busan_org + busan_res) +
  plot_layout(guides = "collect") &                                       # 범례 수집
  theme(legend.position = "bottom", legend.justification = "center")

combined3
## 4) 저장 (선택)
ggsave("graphs/pmd_Seoul_origin_res_2020.png", combined, width = 10, height = 6, dpi = 300)
