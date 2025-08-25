
################################################################################
#
# Creating Pop pyramid at once 
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
region_levels_all <- c(region_levels17, "Abroad", "NA")

# 1) 팔레트에 이름 부여 (팔레트 길이는 18개 이상이라고 가정; 마지막은 NA 전용 회색 추가)
stopifnot(length(pal_admin) >= length(region_levels17) + 1)
pal_named <- setNames(c(pal_admin[seq_len(length(region_levels17) + 1)], "#9E9E9E"),
                      region_levels_all)
#  이때 pal_admin의 앞 18개는: 17개 시도 + Abroad 에 매칭됩니다. "NA"는 회색으로 고정.

# 2) 변수 팩터화(순서 고정) ------------------------------------------
pop2020 <- pop2020 %>%
  mutate(
    res_admin = factor(res_admin, levels = region_levels_all),
    # org_admin: 결측을 "NA" 텍스트로 노출(범례에 필요 시만 나타남)
    org_admin = fct_explicit_na(factor(org_admin, levels = region_levels_all), na_level = "NA")
  )

# 3) 출력 폴더 ---------------------------------------------------------
dir.create("graphs", showWarnings = FALSE)

# 4) 공용 테마 --------------------------------------------------------
base_theme <- theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_blank())

# 5) 시도별 그림 함수 --------------------------------------------------
make_pyramids_for <- function(REG) {
  
  d_org <- pop2020 %>% filter(org_admin == REG)   # 해당 시도 출생자 (fill = res_admin)
  d_res <- pop2020 %>% filter(res_admin == REG)   # 해당 시도 거주자 (fill = org_admin)
  
  if (nrow(d_org) == 0 & nrow(d_res) == 0) {
    message("Skip (no data): ", REG); return(invisible(NULL))
  }
  
  # 스택 합 기준 최대값 계산(양/음 별도) + 5% 여유
  agg1 <- d_org %>% group_by(agegr) %>%
    summarise(pos = sum(pmax(pop_signed, 0), na.rm = TRUE),
              neg = sum(pmax(-pop_signed, 0), na.rm = TRUE), .groups = "drop")
  agg2 <- d_res %>% group_by(agegr) %>%
    summarise(pos = sum(pmax(pop_signed, 0), na.rm = TRUE),
              neg = sum(pmax(-pop_signed, 0), na.rm = TRUE), .groups = "drop")
  
  max_abs <- max(c(agg1$pos, agg1$neg, agg2$pos, agg2$neg), na.rm = TRUE) * 1.05
  y_scale <- scale_y_continuous(
    limits = c(-max_abs, max_abs),
    labels = ~ comma(abs(.x)),
    expand = expansion(mult = 0.01)
  )
  
  # (1) 해당 시도 출생자 기준(범례 숨김)
  g_origin <- ggplot(d_org, aes(x = agegr, y = pop_signed, fill = res_admin)) +
    geom_col(width = 0.9) +
    coord_flip() +
    y_scale +
    scale_fill_manual(values = pal_named, drop = FALSE, guide = "none") +
    labs(title = paste0(REG, " origin"), x = NULL, y = NULL) +
    base_theme
  
  # (2) 해당 시도 거주자 기준(이 플롯의 범례만 사용, 제목 없음)
  g_resident <- ggplot(d_res, aes(x = agegr, y = pop_signed, fill = org_admin)) +
    geom_col(width = 0.9) +
    coord_flip() +
    y_scale +
    scale_fill_manual(values = pal_named, drop = TRUE, name = NULL) +  # 필요 시만 범례 항목 노출
    labs(title = paste0(REG, " residents"), x = NULL, y = NULL) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +              # 범례 행 수 조정
    base_theme
  
  # 합치기(하단 중앙 단일 범례)
  p <- (g_origin + g_resident) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom", legend.justification = "center")
  
  # 저장
  slug <- gsub("[^A-Za-z0-9]+", "_", REG)
  fn <- file.path("graphs", paste0("pmd_", slug, "_origin_res_2020.png"))
  ggsave(fn, p, width = 10, height = 6, dpi = 300)
  message("Saved: ", fn)
  
  invisible(p)
}

# 6) 실행: 17개 시도 전체 --------------------------------------------
invisible(lapply(region_levels17, make_pyramids_for))
