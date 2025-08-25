################################################################################
#
# Excuting R scripts at once
#
# 
#
################################################################################


files <- c(
  "C:/MySam/tscensus/tscensus_p01_ps2020.R",
  "C:/MySam/tscensus/tscensus_p02_ps2015.R",
  "C:/MySam/tscensus/tscensus_p03_ps2010.R",
  "C:/MySam/tscensus/tscensus_p04_ps2005.R",
  "C:/MySam/tscensus/tscensus_p05_ps2000.R",
  "C:/MySam/tscensus/tscensus_p06_ps1995.R",
  "C:/MySam/tscensus/tscensus_p07_ps1990.R",
  "C:/MySam/tscensus/tscensus_p08_ps1985.R",
  "C:/MySam/tscensus/tscensus_p09_ps1980.R",
  "C:/MySam/tscensus/tscensus_p10_ps1975.R",
  "C:/MySam/tscensus/tscensus_p11_ps1970.R",
  "C:/MySam/tscensus/tscensus_p12_ps1966.R",
  "C:/MySam/tscensus/tscensus_p13_ps1960.R"
)

# pXX 번호로 정렬(혹시 순서가 섞였을 경우 대비)
ord <- order(as.integer(sub("^.*p(\\d+)_.*$", "\\1", basename(files))))
files <- files[ord]

run_one <- function(f) {
  cat("\n--- Running:", f, "---\n")
  logf <- paste0(sub("\\.R$", "", f), ".log")
  zz <- file(logf, open = "wt", encoding = "native.enc")
  sink(zz, split = TRUE); sink(zz, type = "message")
  t0 <- Sys.time()
  ok <- tryCatch({
    source(f, local = .GlobalEnv, chdir = TRUE)
    TRUE
  }, error = function(e) {
    message("ERROR: ", conditionMessage(e)); FALSE
  })
  t1 <- Sys.time()
  sink(type = "message"); sink(); close(zz)
  cat(sprintf("Finished: %s in %.1f sec. Log: %s\n",
              basename(f), as.numeric(difftime(t1, t0, units = "secs")), logf))
  ok
}
res <- vapply(files, run_one, logical(1))
cat(sprintf("\nDone. Succeeded: %d / %d\n", sum(res), length(res)))