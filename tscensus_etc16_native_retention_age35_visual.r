################################################################################
#
# Computing native retention rate
# Visualization
#
################################################################################

library(dplyr)
library(ggplot2)
library(patchwork)
library(scales)
library(forcats)
library(tidyr)
library(stringr)
library(purrr)

# ------------------------------------------------------------
# 데이터 불러오기
# ------------------------------------------------------------
table_youth_all <- read.csv("data/table_youth_all.csv")

# ------------------------------------------------------------
# 35+에서의 lx(=Sx) 사용 → 이것이 PPNYR
# retention 대신 반드시 lx 사용해야 함
# ------------------------------------------------------------

order_prov <- table_youth_all |>
  filter(agegr == "35+", year %in% c(2015, 2020)) |> 
  mutate(PPNYR = lx, 
         gender = ifelse(sex =="Male", "남성", 
                         ifelse(sex =="Female", "여성", "전체"))) |>        # 생명표 누적 잔류 확률
  arrange(desc(sex), PPNYR) |> 
  group_by(year, sex) |> 
  mutate(order = row_number()) |> 
  ungroup() |> 
  select(org_admin, sex, PPNYR, year, order)

order_prov |> select(org_admin, order, sex) |> filter(org_admin == "Gyeonggi")

order <- order_prov |> filter(sex == "Total", year == 2020) |> select(org_admin, order)
order_prov <- order_prov |> select(-order) |> left_join(order, by = c("org_admin" = "org_admin"))


# ------------------------------------------------------------
# 2020년 남/녀 지역 순위
# ------------------------------------------------------------
b <- order_prov |> 
  filter(year == 2020, sex != "Total", org_admin != "Total")

write.csv(b, "data/b.csv", row.names = FALSE)

# ------------------------------------------------------------
# 시각화: 지역별 PPNYR (lx 기반)
# ------------------------------------------------------------

# 원래 지역 레벨
region_levels <- c("Total", 
                   "Seoul","Busan","Daegu","Incheon","Gwangju","Daejeon","Ulsan","Sejong",
                   "Gyeonggi","Gangwon","Chungbuk","Chungnam","Jeonbuk","Jeonnam","Gyeongbuk","Gyeongnam","Jeju"
)

# "Total"을 제외한 17개 시도명
region_levels17 <- region_levels[region_levels != "Total"]

# 영어–한국어 매핑 벡터 생성
region_kor <- c(
  Seoul      = "서울특별시",
  Busan      = "부산광역시",
  Daegu      = "대구광역시",
  Incheon    = "인천광역시",
  Gwangju    = "광주광역시",
  Daejeon    = "대전광역시",
  Ulsan      = "울산광역시",
  Sejong     = "세종특별자치시",
  Gyeonggi   = "경기도",
  Gangwon    = "강원도",
  Chungbuk   = "충청북도",
  Chungnam   = "충청남도",
  Jeonbuk    = "전라북도",
  Jeonnam    = "전라남도",
  Gyeongbuk  = "경상북도",
  Gyeongnam  = "경상남도",
  Jeju       = "제주특별자치도"
)

order_prov <- order_prov |> 
  mutate(
  org_admin_kor = region_kor[org_admin], 
  gender = ifelse(sex =="Male", "남성", ifelse(sex =="Female", "여성", "전체")))


# ------------------------------------------------------------
# 전체 대비 수평선 (Total)
# ------------------------------------------------------------
hline_df <- order_prov |> 
  filter(year == 2020, sex != "Total", org_admin == "Total") |> 
  select(gender, PPNYR)

sex_colors <- c(
  "남성"   = "#1B9E77",  # Teal / 시원하고 중립적
  "여성" = "#D95F02"   # Rose / 따뜻하지만 빨강 고정관념에서 탈피
)

order_prov |> filter(order ==18)

p <- order_prov |> 
  filter(year == 2020, sex != "Total", org_admin != "Total") |> 
  ggplot(aes(x = reorder(org_admin_kor, order), y = PPNYR, fill = gender)) +
  geom_col() + 
  geom_hline(data = hline_df, aes(yintercept = PPNYR), 
             color = "black", lty = "dotdash", linewidth = 0.7) +
  scale_fill_manual(values = sex_colors) +
  facet_wrap(. ~ gender) +
  coord_flip() +
  labs(
    x = "",
    y = "청년인구 잔류비율",
    fill = "성별"
  ) +
  theme_bw(base_size = 70) +
  theme(
    legend.position = "none",
    text = element_text(family = "NanumGothic"),
    axis.text.x = element_text(size = 40, margin = margin(t = 15)),
    axis.text.y = element_text(size = 40, margin = margin(r = 10)),
    strip.text  = element_text(size = 55, margin = margin(b = 15)),
    axis.title  = element_text(size = 50),
    panel.spacing = unit(1.5, "lines")
  )
p

ggsave("graphs/PPNYR_barplot.png",
       plot = p,
       width = 40, height = 30, units = "cm",
       dpi = 450, scale = 1.8)


# ------------------------------------------------------------
# retention 단조 증가 가정 위배 여부 확인
# (각 org_admin, sex 조합별로 retention이 나이 증가와 함께 감소하는가)
# ------------------------------------------------------------
monotonic_check <- table_youth_all |>
  group_by(year, org_admin, sex) |>
  arrange(as.numeric(str_extract(agegr, "^[0-9]+")), .by_group = TRUE) |>
  mutate(non_monotonic = retention > lag(retention)) |>
  filter(non_monotonic == TRUE)

write.csv(monotonic_check, "data/monotonic_violation.csv", row.names = FALSE)

colnames(table_youth_all)


region_levels <- c("Total", 
  "Seoul","Busan","Daegu","Incheon","Gwangju","Daejeon","Ulsan","Sejong",
  "Gyeonggi","Gangwon","Chungbuk","Chungnam","Jeonbuk","Jeonnam","Gyeongbuk","Gyeongnam","Jeju"
)


table_youth_all |> 
  mutate(org_admin = factor(org_admin, levels = region_levels)) |> 
  filter(year == 2020, sex == "Male") |> 
  select(year, org_admin, agegr, lx) |> 
  pivot_wider(names_from = agegr, values_from = lx ) |> 
  arrange(org_admin)
  

table_youth_all |> 
  mutate(org_admin = factor(org_admin, levels = region_levels)) |> 
  filter(year == 2020, sex == "Female") |> 
  select(year, org_admin, agegr, lx) |> 
  pivot_wider(names_from = agegr, values_from = lx )


PNYR <- table_youth_all |> 
  mutate(org_admin = factor(org_admin, levels = region_levels), 
         sex = factor(sex, levels = c("Male", "Female", "Total"))) |> 
  filter(year == 2020, agegr == "35+") |> 
  arrange(org_admin, sex) |> 
  select(year, org_admin, sex, agegr, lx) |> 
  pivot_wider(names_from = sex, values_from = lx ) 


write.csv(PNYR, "data/PNYR.csv", row.names = FALSE)
