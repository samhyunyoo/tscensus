################################################################################
#
# Data Cleaning
#
# Pop Census 2% Sample 2015 
#
################################################################################

library(tidyverse)
library(scales)
library(RColorBrewer)
# library(ColorSpace)


# 0. Defining categories ----------------------------------------------------

## sidomap for the place of residence
sidomap_res <- function(code){
  sido <- case_when(
    code == 11 ~ 'Seoul',
    code == 21 ~ 'Busan',
    code == 22 ~ 'Daegu',
    code == 23 ~ 'Incheon',
    code == 24 ~ 'Gwangju',
    code == 25 ~ 'Daejeon',
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
         levels = c('Seoul', 	'Busan', 	'Daegu', 	'Incheon', 	'Gwangju', 	'Daejeon', 	'Ulsan', 	'Sejong', 	'Gyeonggi', 	'Gangwon', 	'Chungbuk', 	'Chungnam', 	'Jeonbuk', 	'Jeonnam', 	'Gyeongbuk', 	'Gyeongnam', 	'Jeju' 
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
    code == 25 ~ 'Daejeon',
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
    code >= 40 ~ 'Abroad', 
    TRUE ~"NA"
  ) 
  factor(sido, 
         levels = c('Seoul', 	'Busan', 	'Daegu', 	'Incheon', 	'Gwangju', 	'Daejeon', 	'Ulsan', 	'Sejong', 	'Gyeonggi', 	'Gangwon', 	'Chungbuk', 	'Chungnam', 	'Jeonbuk', 	'Jeonnam', 	'Gyeongbuk', 	'Gyeongnam', 	'Jeju', 	'Abroad', 'NA' 
))
}

## regionmap for the place of residence
regionmap_res <- function(code){
  region <- case_when(
    code == 11 ~ "Seoul", 
    code %in% c(23, 31) ~ "Rest Capital", 
    code %in% c(21, 22, 24, 25, 26, 29) ~ "Metros", 
    code %in% c(32, 33, 34, 35, 36, 37, 38, 39) ~ "Provinces", 
    TRUE ~ "NA") 
  factor(
    region, 
    levels = c("Seoul", "Rest Capital", "Metros","Provinces")
  )
}

## regionmap for the place of origin
regionmap_org <- function(code){
  region <- case_when(
  code == 11 ~ "Seoul", 
  code %in% c(23, 31) ~ "Rest Capital", 
  code %in% c(21, 22, 24, 25, 26, 29) ~ "Metros", 
  code %in% c(32, 33, 34, 35, 36, 37, 38, 39) ~ "Provinces", 
  code >= 40 ~ "Abroad", 
  TRUE ~ "NA") 
  factor(
    region, 
    levels = c("Seoul", "Rest Capital", "Metros","Provinces", "Abroad", "NA")
  )
}


# 1. tscensus 2015 --------------------------------------------------------


tscensus2015 <- readRDS("data/tscensus2015.rds")
table(tscensus2015$V8, useNA = "ifany")
ts2015 <- tscensus2015 |> 
  mutate(
  
    # region codes 
    res_code1 = as.numeric(V1), 
    res_admin = sidomap_res(res_code1), 
    res_region5 = regionmap_res(res_code1), 
    
    org_check = as.numeric(V17), 
    org_code0 = as.numeric(V20), 
    org_code1 = ifelse(org_check %in% c(1, 2), res_code1, 
                       ifelse(org_check == 3, org_code0, 
                              ifelse(org_check == 4, 40, NA))), 
    
    org_admin = sidomap_org(org_code1), 
    org_region5 = regionmap_org(org_code1),
    
    # demographics
    age = as.numeric(V8), 
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
    
    edua = as.numeric(V10), 
    edub = as.numeric(V11),
    
    educ = case_when(
      edua < 4 | (edua == 4 & edub != 1) ~ "<HS", 
      edua == 4 & edub == 1 ~ "HS", 
      edua == 5 | (edua == 6 & edub !=1) ~ "SomeCol", 
      (edua == 6 & edub ==1) | edua %in% c(7, 8) ~ "BA+"
    ), 
    educ = factor(educ, levels = c("<HS", "HS", "SomeCol", "BA+")), 
  
    mar = as.numeric(V48), 
    agefm = as.numeric(V76), 
    agefmgr = case_when(agefm %in% c(1, 2) ~ "<20", 
                        agefm == 3 ~ "20-24", 
                        agefm == 4 ~ "25-29", 
                        agefm == 5 ~ "30-34", 
                        agefm == 6 ~ "35-39", 
                        agefm == 7  ~ "40+", 
                        TRUE ~ NA_character_
    ),
    
    ceb_boy = as.numeric(V78), 
    ceb_girl = as.numeric(V80), 
    ceb = rowSums(across(c(ceb_boy, ceb_girl)), na.rm = TRUE), 
    
    expkid = as.numeric(V84), 
    expnkids = as.numeric(V62), 
    
    wgt = as.numeric(V97), 
    
  )



# 3. pop pyramid ----------------------------------------------------------


pop2015 <- ts2015 |> 
  group_by(agegr, sex, res_admin, org_admin, res_region5, org_region5) |> 
  summarise(pop_weighted = sum(wgt, na.rm = TRUE), .groups = "drop") |> 
  mutate(pop_signed = ifelse(sex == "Male", -pop_weighted,  pop_weighted))

# color palette
library(colorspace)
library(RColorBrewer)

pal_admin <- colorRampPalette(brewer.pal(12, "Paired"))(19)

pal_admin[8]  <- desaturate(pal_admin[8], amount = 1)
pal_admin[3]  <- darken(pal_admin[3], amount = 0.2)
pal_admin[17] <- darken(pal_admin[17], amount = 0.3)

pal_comp <- c("#e31a1c", "#ff7f00", "#1f78b4", "#33a02c", "#ba39a0", "#bebebe")

ggplot(pop2015, aes(x = agegr, y = pop_signed, fill = res_admin)) +
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

ggsave("graphs/prd2015_res_admin.png", width = 10, height = 5)



