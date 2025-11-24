
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


library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

table_youth_all <- read.csv("data/table_youth_all.csv")


order_prov <- table_youth_all |>
  filter(agegr == "35+", year %in% c(2015, 2020)) |> 
  arrange(sex, retention) |> 
  group_by(year, sex) |> 
  mutate(order = row_number()) |> 
  select(org_admin, sex, retention, year, order)

hline_df <- order_prov |> filter(year == 2020, sex != "Total", org_admin == "Total") |> 
  select(sex, retention)
  
b <- order_prov |> filter(year == 2020, sex != "Total", org_admin != "Total")
write.csv(b, "data/b.csv", row.names = FALSE)

order_prov |> filter(year == 2020, sex != "Total", org_admin != "Total") |> 
  ggplot(aes(x = reorder(org_admin, order), y = retention, fill = sex)) +
  geom_col() + 
  geom_hline(data = hline_df, aes(yintercept = retention), color = "red", lty = "dashed") +
  facet_wrap(.~sex) +
  coord_flip() +  # 가독성을 위해 가로막대
  labs(x = "", y = "Period Proportion of Native Youths Retention", 
       fill = "Sex") +
  theme_bw()

ggsave("graphs/colum_graph_PPNYR.png", width = 10, height = 6, dpi = 300)


## retention 비율의 단조 증가 가정 위배 


