################################################################################
#
# Data Cleaning
#
# Pop Census 2% Sample 1980 
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


# 1. tscensus 1980 --------------------------------------------------------


tscensus1980 <- readRDS("data/tscensus1980.rds")
ts1980 <- tscensus1980 |> 
  mutate(
  
    # region codes 
    res_code1 = as.numeric(V1), 
    res_admin = sidomap_res(res_code1), 
    res_region5 = regionmap_res(res_code1), 
    
    # org_check = as.numeric(V14), 
    # org_code0 = as.numeric(V13), 
    # org_code1 = ifelse(org_check %in% c(1, 2), res_code1, 
    #                    ifelse(org_check == 3, org_code0, NA)), 
    # 
    # org_admin = sidomap_org(org_code1), 
    # org_region5 = regionmap_org(org_code1),
    
    # demographics
    age = as.numeric(V12), 
    agegr = cut(
      age, 
      breaks = c(seq(0, 85, by = 5), Inf), 
      right = FALSE, 
      labels = c(
        paste(seq(0, 84, by = 5), seq(4, 84, by = 5), sep = "-"), "85+")
      ), 
    
    sex = case_when(
      V11 %in% c(1, "1") ~ "Male", 
      V11 %in% c(2, "2") ~ "Female", 
      TRUE ~ as.character(V11)
    ), 
    
    edua = as.numeric(V13), 
    edub = as.numeric(V14),
    
    educ = case_when(
      edua < 4 | (edua %in% c(4, 5) & edub != 1) ~ "<HS", 
      edua %in% c(4, 5) & edub == 1 ~ "HS", 
      edua == 6 | (edua == 7 & edub !=1) ~ "SomeCol", 
      (edua == 7 & edub ==1)  ~ "BA+", 
      TRUE ~ NA_character_
    ), 
    educ = factor(educ, levels = c("<HS", "HS", "SomeCol", "BA+")), 
    
    mar1980 = as.numeric(V15), 
    mar = ifelse(mar1980 == 4, 1, 
                 ifelse(is.na(mar1980), NA, mar1980+1)), 
    # agefm = as.numeric(V35), 

    # ceb_boy = as.numeric(V31), 
    # ceb_girl = as.numeric(V32), 
    # ceb = rowSums(across(c(ceb_boy, ceb_girl)), na.rm = TRUE), 
    # ceb = as.numeric(V33),
    
    # expkid = as.numeric(V78), 
    # expnkids = as.numeric(V80), 
    
    wgt = as.numeric(V17), 
    
  )



# 3. pop pyramid ----------------------------------------------------------

pop1980 <- ts1980 |> 
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

ggplot(pop1980, aes(x = agegr, y = pop_signed, fill = res_admin)) +
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

ggsave("graphs/prd1980_res_admin.png", width = 10, height = 5)




