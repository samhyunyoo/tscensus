
################################################################################
#
# Computing native retention rate
#
# 
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

pop2020 <- readRDS("data/pop2020.rds")

# 2) 변수 팩터화(순서 고정) ------------------------------------------

colnames(pop2020)
retention2020 <- pop2020 |> 
  mutate(native = ifelse(org_admin == res_admin, "native", "none")) |> 
  group_by(org_admin, sex, agegr, native) |> 
  summarise(pop = sum(pop_weighted, na.rm = TRUE), 
            .groups = "drop") |> 
  pivot_wider(names_from = native, values_from = pop) |> 
  mutate(retention = native / (native + none))

# retention rate is something like never-married proportion 
# S = retion_x / retention_x-n


table2020 <- retention2020 %>%
  group_by(org_admin, sex) %>%
  mutate(
    # 1 연령별 유지율 (Sx)
    Sx = retention,
    Sx = Sx / first(Sx),
    
    # 2 구간 잔류확률(px), 이탈확률(qx)
    px = lead(Sx) / Sx,
    qx = 1 - px,
    
    qx = if_else(is.na(qx), 0, qx),   # 마지막 구간: 이탈 없음
    px = if_else(is.na(px), 1, px),   # 마지막 구간: 잔류 100%
    
    # 3 lx : 잔류코호트(생존자 수) — 시작 구간을 1로 정규화
    lx = Sx / first(Sx),
    
    # 4 dx : 구간 이탈자수
    dx = lx * qx,
    
    # 5 Lx : 구간 평균 잔류자수 (이탈이 균등 발생한다고 가정, n=5)
    Lx = 5 * (lx - 0.5 * dx),
    
    # 6 Tx : 잔류기간의 총합 (상위 구간부터 누적)
    Tx = rev(cumsum(rev(Lx))),
    
    # 7 ex : 기대 잔류기간 (평균 남은 5년 단위 잔류기간)
    ex = Tx / lx
  ) %>%
  ungroup()


table2020


library(dplyr)


table2020_summary <- table2020 %>%
  group_by(org_admin, sex) %>%
  summarise(
    # PPRR: 마지막 연령의 잔류확률 (단조화 적용해 계산)
    PPRR = tail(Sx, 1),
    # e0: 최저 연령구간의 기대 잔류기간
    e0 = ex[which.min(as.numeric(sub("^([0-9]+).*", "\\1", as.character(agegr))))],
    .groups = "drop"
  )

table2020_summary |> select(org_admin, sex, PPRR) |> pivot_wider(names_from = sex, values_from = PPRR)


