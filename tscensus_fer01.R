################################################################################
#
# Checking Fertility Data
#
# 1990, 2000, 2010, 2015, 2020  
#
################################################################################

library(tidyverse)

colnames(ts2020)

fer2020 <- ts2020 |>
  select("res_code1",   "res_admin",   "res_region5", "org_check",   "org_code0",   "org_code1",  
         "org_admin",   "org_region5", "age",         "agegr",       "sex",         "educ",       
         "mar",         "agefm",       "agefmgr",     "ceb",         "expnkids",   
         "wgt") |> 
  mutate(year = 2020) |> 
  filter(sex == "Female", age >= 45, age <= 49)

fer2015 <- ts2015 |>
  select("res_code1",   "res_admin",   "res_region5", "org_check",   "org_code0",   "org_code1",  
         "org_admin",   "org_region5", "age",         "agegr",       "sex",         "educ",       
         "mar",         "agefm",       "agefmgr",     "ceb",         "expnkids",   
         "wgt") |> 
  mutate(year = 2015) |> 
  filter(sex == "Female", age >= 45, age <= 49)

fer2010 <- ts2010 |>
  select("res_code1",   "res_admin",   "res_region5", "org_check",   "org_code0",   "org_code1",  
         "org_admin",   "org_region5", "age",         "agegr",       "sex",         "educ",       
         "mar",         "agefm",       "agefmgr",     "ceb",         "expnkids",   
         "wgt") |> 
  mutate(year = 2010) |> 
  filter(sex == "Female", age >= 45, age <= 49)

fer2000 <- ts2000 |>
  select("res_code1",   "res_admin",   "res_region5", "org_check",   "org_code0",   "org_code1",  
         "org_admin",   "org_region5", "age",         "agegr",       "sex",         "educ",       
         "mar",         "ceb",            
         "wgt") |> 
  mutate(year = 2000) |> 
  filter(sex == "Female", age >= 45, age <= 54)

fer1990 <- ts1990 |>
  select("res_code1",   "res_admin",   "res_region5", "org_check",   "org_code0",   "org_code1",  
         "org_admin",   "org_region5", "age",         "agegr",       "sex",         "educ",       
         "mar",         "agefm",       "agefmgr",     "ceb",            
         "wgt") |> 
  mutate(year = 1990) |> 
  filter(sex == "Female", age >= 45, age <= 54)


ferall <- bind_rows(fer2020, fer2015, fer2010, fer2000, fer1990) |> 
  filter(sex == "Female")




# 2. Compute indicators ---------------------------------------------------

library(tidyverse)
myfer <- ferall |> 
  mutate(cohort1 = floor((year - age)/5)*5, 
         cohort2 = cohort1 - 4, 
         cohort = paste(cohort2, cohort1, sep = "-")) 


a <- myfer |> 
  filter(cohort == "1956-1960")

table(fer2000$ceb)
table(fer2000$res_region5)

is.numeric(fer2000$ceb)

fer2000 |> 
  group_by(res_region5) |> 
  summarise(cohortfer = sum(ceb * wgt, na.rm = TRUE)/sum(wgt, na.rm = TRUE), .groups = "drop")

table(fer2020$ceb)
table(fer2015$ceb)
table(fer2010$ceb)
table(fer2000$ceb)
table(fer1990$ceb)
table(fer1990$wgt)
table(fer2000$res_region5)

## based on res_region5
fer_res <- myfer |> 
  group_by(cohort, res_region5) |> 
  summarise(cohortfer = sum(ceb * wgt, na.rm = TRUE)/sum(wgt, na.rm = TRUE), .groups = "drop")

fer_org <- myfer |> 
  group_by(cohort, org_region5) |> 
  summarise(cohortfer = sum(ceb * wgt, na.rm = TRUE)/sum(wgt, na.rm = TRUE), .groups = "drop")

fer_org
View(fer_org)

fer_res_org <- myfer |> 
  group_by(cohort, res_region5, org_region5) |> 
  summarise(cohortfer = sum(ceb * wgt, na.rm = TRUE)/sum(wgt, na.rm = TRUE), .groups = "drop")


fer_res |>
  ggplot(aes(x = cohort, y = cohortfer, group = res_region5, color = res_region5)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = pal_comp) +
  scale_y_continuous(limits = c(1, 5)) +
  labs(x = "Birth cohort", y = "Fertility", color = "Place of residence") +
  theme_minimal() +
  theme(
    legend.position        = "inside",              # ← 새 방식
    legend.position.inside = c(0.98, 0.98),         # 패널 내부 좌표 (오른쪽-위)
    legend.justification   = c(1, 1),               # 범례 기준점을 오른쪽-위로
    legend.background      = element_rect(fill = scales::alpha("white", 0.7), color = NA),
    legend.key             = element_blank()
  )

ggsave("graphs/trend_fer_res_1931-1975.png", width = 10, height = 6, scale = 0.7)

fer_org |> filter(org_region5 != "Abroad") |> 
  ggplot(aes(x = cohort, y = cohortfer, group = org_region5, color = org_region5)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = pal_comp) +
  scale_y_continuous(limits = c(1, 5)) +
  labs( x = "Birth cohort", y = "Fertility", color = "Place of birth") +
  theme_minimal() +
  theme(
    legend.position        = "inside",              # ← 새 방식
    legend.position.inside = c(0.98, 0.98),         # 패널 내부 좌표 (오른쪽-위)
    legend.justification   = c(1, 1),               # 범례 기준점을 오른쪽-위로
    legend.background      = element_rect(fill = scales::alpha("white", 0.7), color = NA),
    legend.key             = element_blank()
  )

ggsave("graphs/trend_fer_org_1931-1975.png", width = 10, height = 6, scale = 0.7)


## trends in completed fertility by origin for each residence area
fer_res_org |> filter(org_region5 != "Abroad") |> 
  ggplot(aes(x = cohort, y = cohortfer, group = org_region5, color = org_region5)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = pal_comp) +
  scale_y_continuous(limits = c(1, 5)) +
  scale_x_discrete(labels = \(x) ifelse(seq_along(x) %% 2 == 1, x, "")) +
  
  theme_minimal() +
  labs( x = "Birth cohort", y = "Fertility", group = "Place of birth", color = "Place of birth") +
  facet_wrap(.~res_region5) +
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

ggsave("graphs/trend_fer_res_x_org_1931-1975.png", width = 10, height = 6, scale = 0.8)


## trends in completed fertility by residence for each origin 
fer_res_org |> filter(org_region5 != "Abroad") |> 
  ggplot(aes(x = cohort, y = cohortfer, group = res_region5, color = res_region5)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = pal_comp) +
  scale_y_continuous(limits = c(1, 5)) +
  scale_x_discrete(labels = \(x) ifelse(seq_along(x) %% 2 == 1, x, "")) +
  theme_minimal() +
  labs( x = "Birth cohort", y = "Fertility", group = "Place of residence", color = "Place of residence")+
  facet_wrap(.~org_region5) +
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

ggsave("graphs/trend_fer_org_x_res_1931-1975.png", width = 10, height = 6, scale = 0.8)
