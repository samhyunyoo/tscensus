################################################################################
#
# Exploratory Data Analysis
#
# 1990, 2000, 2010, 2015, 2020  
#
################################################################################

library(tidyverse)


# 1. Check the data again ---------------------------------------------------

myfer <- readRDS("data/myfer.rds") |> 
  filter(org_region5 !="NA")
ceball <- readRDS("data/ceball.rds") |> 
  filter(org_region5 !="NA")


str(ceball)
myceb <- ceball |> 
  filter(age < 50)

table(myceb$ceb, useNA = "ifany")
table(df_use$ceb, useNA = "ifany")

test <- myceb |> filter(is.na(ceb))

table(test$mar, test$ceb, useNA="ifany")

# 2. Chck asfr  ---------------------------------------------------


asfr_res <- myceb |> 
  mutate(year = as.factor(year)) |> 
  group_by(year, res_region5, age) |> 
  summarise(asfr = sum(ceb * wgt, na.rm = TRUE)/sum(wgt, na.rm = TRUE), .groups = "drop")

asfr_res |> filter(year ==2010)

asfr_org <- myceb |> 
  mutate(year = as.factor(year)) |> 
  group_by(year, org_region5, age) |> 
  summarise(asfr = sum(ceb * wgt, na.rm = TRUE)/sum(wgt, na.rm = TRUE), .groups = "drop")


asfr_res_org <- myceb |> 
  mutate(year = as.factor(year)) |> 
  group_by(year, res_region5, org_region5, age) |> 
  summarise(asfr = sum(ceb * wgt, na.rm = TRUE)/sum(wgt, na.rm = TRUE), .groups = "drop")

asfr_res |> filter(year ==1990, age ==49)
asfr_res |> 
  ggplot(aes(x = age, y = asfr, 
             group = res_region5, color = res_region5, linetype = res_region5)) +
  geom_line(size = 1) +
  geom_vline(xintercept = c(30, 35), color = "gray", linetype = "dashed")+
  
  scale_color_manual(values = pal_comp) +
  scale_y_continuous(limits = c(0, 4.3)) +
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

ggsave("graphs/trend_ceb_res_1990-2020.png", width = 10, height = 6, scale = 0.7)




asfr_org |> 
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

ggsave("graphs/trend_ceb_org_1990-2020.png", width = 10, height = 6, scale = 0.7)


# 3. Chck asfr by place ---------------------------------------------------

library(ggplot2)
library(dplyr)
library(scales)

pal_comp <- c("#e31a1c", "#ff7f00", "#1f78b4", "#33a02c", "#ba39a0", "#bebebe")

# region별 base color 지정
region_cols <- setNames(pal_comp, unique(asfr_res$res_region5))

# region + year 조합에 대해 gradient 색 만들기
asfr_res <- asfr_res %>%
  group_by(res_region5) %>%
  mutate(year_rank = rank(year)) %>%
  ungroup()

asfr_res |> 
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

ggsave("graphs/trend_ceb_by_res_period_1990-2020.png", width = 10, height = 6, scale = 0.7)



asfr_org |> 
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

ggsave("graphs/trend_ceb_by_org_period_1990-2020.png", width = 10, height = 6, scale = 0.7)


asfr_res_org |> 
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

ggsave("graphs/trend_ceb_by_res_org_period_1990-2020.png", width = 10, height = 6, scale = 0.7)


# 4. Further exploration --------------------------------------------------

## this is only for ever-married women only 
asfr_res_org |> 
  filter(year ==2000, age == 49) |> 
  pivot_wider(names_from = res_region5, values_from = asfr)
  
# 5. Further exploration --------------------------------------------------

colnames(ceball)

table(myceb$mar)
df_reg <- myceb |> 
  mutate(ceb = ifelse(is.na(ceb), 0, ceb),
    fct_mar = case_when(mar == 2 ~ "Married", 
                             mar == 1 ~ "Single", 
                             mar %in% c(3, 4) ~ "Divorced/widowed", 
                             TRUE ~ NA_character_),
  fct_mar = factor(fct_mar, levels = c("Married", "Single", "Divorced/widowed")), 
  fct_educ = factor(educ), 
  fct_agefmgr = ifelse(is.na(agefmgr), "Single", agefmgr), 
  fct_agefmgr = factor(fct_agefmgr), 
  fct_year = factor(year)
  )
table(df_reg$mar)


# 가중치 포함 요약
library(Hmisc)
w <- df_reg$wgt/1000
w_mean <- weighted.mean(df_reg$ceb, w)
w_var  <- Hmisc::wtd.var(df_reg$ceb, weights = w)   # install.packages("Hmisc")
c(mean = w_mean, var = w_var)

# 0의 과다 여부(관측 vs 포아송 기대)
mu_hat <- w_mean
p0_obs <- weighted.mean(df_reg$ceb == 0, w)
p0_exp <- exp(-mu_hat)
c(p0_obs = p0_obs, p0_exp = p0_exp)

# var >> mean → 과산포(Overdispersion) 의심 → 포아송 단독은 위험.
# var << mean → 과소산포(Underdispersion) 의심.
# 가능하면 **집단별(예: agegr, fct_mar)**로도 확인해봐. 특정 집단에서 과산포가 심할 수 있어.
df_reg |>
  group_by(agegr, fct_mar) |>
  summarise(
    n = sum(w),
    mean = weighted.mean(ceb, w, na.rm = FALSE),
    var  = Hmisc::wtd.var(ceb, weights = w, na.rm = FALSE),
    p0   = weighted.mean(ceb == 0, w, na.rm = FALSE),
    .groups = "drop"
  )


df_reg |> filter(agefmgr == "NA")
table(df_reg$agefmgr, useNA = "ifany")
model1 <- glm(ceb ~ agegr + fct_mar + fct_educ + org_region5 + res_region5 + fct_year, 
    data = df_reg, family = "poisson", weights = wgt/1000)
summary(model1)


model2 <- glm(ceb ~ agegr + fct_agefmgr + fct_educ + org_region5 + res_region5 + fct_year, 
              data = df_reg, family = "poisson", weights = wgt/1000)
summary(model2)


model3 <- glm(ceb ~ agegr + fct_agefmgr + fct_educ + org_region5 + res_region5 + org_region5*res_region5 +fct_year, 
              data = df_reg, family = "poisson", weights = wgt/1000)
summary(model3)


## 포아송 확인 

# 이미 적합한 model1~3 있음
over_phi <- function(m){
  # 가중치 있는 glm에서 근사치로 사용
  pr <- residuals(m, type = "pearson")
  df <- df.residual(m)
  sum(pr^2) / df
}
c(model1 = over_phi(model1),
  model2 = over_phi(model2),
  model3 = over_phi(model3))
# 1.0 근처 → OK
# 1.5~2 이상 → 과산포 강함(포아송 표준오차 과소추정)
# 0.7 이하 → 과소산포 의심


# install.packages(c("AER","performance"))
AER::dispersiontest(model1)   # 양수면 과산포 경향
performance::check_overdispersion(model1)
performance::check_overdispersion(model1)






# 6. Modelling --------------------------------------------------

library(sandwich)
library(lmtest)
# library(modelsummary)  # 표 만들 때 유용(선택)
table(df_reg$year, df_reg$educ)
table(df_reg$year, df_reg$mar)
table(df_reg$year, df_reg$ceb)
table(df_reg$year, df_reg$org_region5)
table(df_reg$year, df_reg$res_region5)
table(df_reg$year, df_reg$agefmgr)


df_use  <- df_reg |> 
  mutate(cohort1 = floor((year - age)/5)*5, 
                           cohort2 = cohort1 - 4, 
                           cohort = paste(cohort2, cohort1, sep = "-")) |> 
  select(-cohort1, -cohort2)

# 수치 안정화를 위해 각 연도별 가중치를 평균 1로
df_use <- df_use |> 
  group_by(year) |> 
  mutate(w_norm = wgt / mean(wgt, na.rm = TRUE)) |> 
  ungroup()
# w_norm  <- df_use$wgt / mean(df_use$wgt, na.rm = TRUE)  # 수치 안정화를 위해 평균 1로
# df_use$w_norm <- w_norm

table(df_reg$year, df_reg$mar)
table(df_use$year, df_use$mar)


saveRDS(df_use, "data/df_use.rds")


form <- ceb ~ agegr + fct_mar + fct_educ + org_region5 + res_region5 + org_region5*res_region5+ fct_year

# 1) OLS (보조)
m_ols <- lm(form, data = df_use, weights = w_norm)
coeftest(m_ols, vcov. = vcovHC(m_ols, type = "HC3"))

# 2) PPML (포아송 + 강건SE) — 메인 후보
m_ppml <- glm(form, family = poisson(link = "log"), data = df_use, weights = w_norm)
coeftest(m_ppml, vcov. = vcovHC(m_ppml, type = "HC3"))
irr_ppml <- exp(coef(m_ppml))  # IRR

# 3) quasi-Poisson — 메인 후보(대체)
m_qp <- glm(form, family = quasipoisson(link = "log"), data = df_use, weights = w_norm)
coeftest(m_qp, vcov. = vcovHC(m_qp, type = "HC3"))
disp_qp <- summary(m_qp)$dispersion  # φ < 1 이면 과소산포
irr_qp  <- exp(coef(m_qp))

# (선택) 한 표로 정리
modelsummary::msummary(
  list("OLS (robust SE)" = m_ols,
       "PPML (robust SE)" = m_ppml,
       "Quasi-Poisson (robust SE)" = m_qp),
  vcov = list(vcovHC, vcovHC, vcovHC),
  vcov_args = list(list(type="HC3"), list(type="HC3"), list(type="HC3")),
  exponentiate = c(FALSE, TRUE, TRUE),  # PPML/Quasi-Poisson을 IRR로
  gof_omit = 'DF|Deviance'
)

