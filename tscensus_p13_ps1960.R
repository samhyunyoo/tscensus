################################################################################
#
# Data Cleaning
#
# Pop Census 2% Sample 1960 
#
################################################################################

library(tidyverse)
library(scales)
library(RColorBrewer)
library(colorspace)


# 0. Defining categories ----------------------------------------------------

## sidomap for the place of residence
sidomap_res <- function(code){
  sido <- case_when(
    code == 11 ~ 'Seoul',
    code == 21 ~ 'Busan',
    code == 22 ~ 'Daegu',
    code == 23 ~ 'Incheon',
    code == 24 ~ 'Gwangju',
    code == 25 ~ 'Deajeon',
    code == 26 ~ 'Ulsan',
    code == 29 ~ 'Sejong',
    code == 31 ~ 'Gyeonggi',
    code == 32 ~ 'Gangwon',
    code == 33 ~ 'Chungbuk',
    code == 34 ~ 'Chungnam',
    code == 35 ~ 'Jeonbuk',
    code == 36 ~ 'Jeonnam',
    code == 37 ~ 'Gyeongbuk',
    code == 38 ~ 'Gyeongnam',
    code == 39 ~ 'Jeju', 
    TRUE ~"NA"
  ) 
  factor(sido, 
         levels = c('Seoul', 	'Busan', 	'Daegu', 	'Incheon', 	'Gwangju', 	'Deajeon', 	'Ulsan', 	'Sejong', 	'Gyeonggi', 	'Gangwon', 	'Chungbuk', 	'Chungnam', 	'Jeonbuk', 	'Jeonnam', 	'Gyeongbuk', 	'Gyeongnam', 	'Jeju' 
         ))
}

## sidomap for the place of origin
sidomap_org <- function(code){
  sido <- case_when(
    code == 11 ~ 'Seoul',
    code == 21 ~ 'Busan',
    code == 22 ~ 'Daegu',
    code == 23 ~ 'Incheon',
    code == 24 ~ 'Gwangju',
    code == 25 ~ 'Deajeon',
    code == 26 ~ 'Ulsan',
    code == 29 ~ 'Sejong',
    code == 31 ~ 'Gyeonggi',
    code == 32 ~ 'Gangwon',
    code == 33 ~ 'Chungbuk',
    code == 34 ~ 'Chungnam',
    code == 35 ~ 'Jeonbuk',
    code == 36 ~ 'Jeonnam',
    code == 37 ~ 'Gyeongbuk',
    code == 38 ~ 'Gyeongnam',
    code == 39 ~ 'Jeju',
    code >= 40 ~ 'Aborad', 
    TRUE ~"NA"
  ) 
  factor(sido, 
         levels = c('Seoul', 	'Busan', 	'Daegu', 	'Incheon', 	'Gwangju', 	'Deajeon', 	'Ulsan', 	'Sejong', 	'Gyeonggi', 	'Gangwon', 	'Chungbuk', 	'Chungnam', 	'Jeonbuk', 	'Jeonnam', 	'Gyeongbuk', 	'Gyeongnam', 	'Jeju', 	'Aborad', 'NA' 
))
}

## regionmap for the place of residence
regionmap_res <- function(code){
  region <- case_when(
    code == 11 ~ "Seoul", 
    code %in% c(23, 31) ~ "Rest Capital", 
    code %in% c(21, 22, 24, 25, 26, 29) ~ "Metro", 
    code %in% c(32, 33, 34, 35, 36, 37, 38, 39) ~ "Provinces", 
    TRUE ~ "NA") 
  factor(
    region, 
    levels = c("Seoul", "Rest Capital", "Metro","Provinces")
  )
}

## regionmap for the place of origin
regionmap_org <- function(code){
  region <- case_when(
  code == 11 ~ "Seoul", 
  code %in% c(23, 31) ~ "Rest Capital", 
  code %in% c(21, 22, 24, 25, 26, 29) ~ "Metro", 
  code %in% c(32, 33, 34, 35, 36, 37, 38, 39) ~ "Provinces", 
  code >= 40 ~ "Abroad", 
  TRUE ~ "NA") 
  factor(
    region, 
    levels = c("Seoul", "Rest Capital", "Metro","Provinces", "Abroad", "NA")
  )
}


# 1. tscensus 1960 --------------------------------------------------------


tscensus1960 <- readRDS("data/tscensus1960.rds")
ts1960 <- tscensus1960 |> 
  mutate(
  
    # region codes 
    res_code0 = as.numeric(V1), 
    res_code1 = case_when(
      res_code0 == 0 ~ 11, 
      res_code0 == 1 ~ 31, 
      res_code0 == 2 ~ 33, 
      res_code0 == 3 ~ 34, 
      res_code0 == 4 ~ 35, 
      res_code0 == 5 ~ 36, 
      res_code0 == 6 ~ 37, 
      res_code0 == 7 ~ 38, 
      res_code0 == 8 ~ 32, 
      res_code0 == 9 ~ 39, 
      TRUE ~ NA_integer_
    ),
    res_admin = sidomap_res(res_code1), 
    res_region5 = regionmap_res(res_code1), 

    # org_check = as.numeric(V22),
    org_code0 = as.numeric(V9),
    org_code1 = case_when(
      org_code0 == 0 ~ 11, 
      org_code0 == 1 ~ 31, 
      org_code0 == 2 ~ 33, 
      org_code0 == 3 ~ 34, 
      org_code0 == 4 ~ 35, 
      org_code0 == 5 ~ 36, 
      org_code0 == 6 ~ 37, 
      org_code0 == 7 ~ 38, 
      org_code0 == 8 ~ 32, 
      org_code0 == 9 ~ 39, 
      TRUE ~ NA_integer_
      ), 
    # org_code1 = ifelse(org_check %in% c(1, 2), res_code1,
    #                    ifelse(org_check == 3, org_code0, NA)),
    # 
    org_admin = sidomap_org(org_code1),
    org_region5 = regionmap_org(org_code1),
    
    # demographics
    age = as.numeric(V6), 
    agegr = cut(
      age, 
      breaks = c(seq(0, 85, by = 5), Inf), 
      right = FALSE, 
      labels = c(
        paste(seq(0, 84, by = 5), seq(4, 84, by = 5), sep = "-"), "85+")
      ), 
    
    sex = case_when(
      V7 %in% c(1, "1") ~ "Male", 
      V7 %in% c(2, "2") ~ "Female", 
      TRUE ~ as.character(V7)
    ), 
    
    eduyr = as.numeric(V13), 

    educ = case_when(
      eduyr < 12 | eduyr ==99  ~ "<HS", 
      eduyr == 12 ~ "HS", 
      eduyr > 12 & eduyr < 16  ~ "SomeCol", 
      eduyr >= 16  ~ "BA+"
    ), 
    educ = factor(educ, levels = c("<HS", "HS", "SomeCol", "BA+")), 
  
    mar1960 = as.numeric(V8), 
    mar = ifelse(mar1960 == 4, 1, 
                 ifelse(is.na(mar1960), NA, mar1960+1)),
    # agefm = as.numeric(V35), 

    # ceb_boy = as.numeric(V31), 
    # ceb_girl = as.numeric(V32), 
    # ceb = rowSums(across(c(ceb_boy, ceb_girl)), na.rm = TRUE), 
    ceb = ifelse(V11 == "99", NA, as.numeric(V11)),
    
    # expkid = as.numeric(V78), 
    # expnkids = as.numeric(V80), 
    
    wgt = as.numeric(V49), 
    
  )



# 3. pop pyramid ----------------------------------------------------------

pop1960 <- ts1960 |> 
  group_by(agegr, sex, res_admin, res_region5, org_admin, org_region5) |> 
  summarise(pop_wighted = sum(wgt, na.rm = TRUE), .groups = "drop") |> 
  mutate(pop_signed = ifelse(sex == "Male", -pop_wighted,  pop_wighted))

# color palette
library(colorspace)
library(RColorBrewer)

pal_admin <- colorRampPalette(brewer.pal(12, "Paired"))(19)

pal_admin[8]  <- desaturate(pal_admin[8], amount = 1)
pal_admin[3]  <- darken(pal_admin[3], amount = 0.2)
pal_admin[17] <- darken(pal_admin[17], amount = 0.3)

pal_comp <- c("#e31a1c", "#ff7f00", "#1f78b4", "#33a02c", "#ba39a0", "#bebebe")

ggplot(pop1960, aes(x = agegr, y = pop_signed, fill = res_admin)) +
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

ggsave("graphs/prd1960_res_admin.png", width = 10, height = 5)




