################################################################################
#
# Correcting typo at one 
#
#  
#
################################################################################









# 모든 R 스크립트/문서에서 "Aborad"→"Abroad", "Deajeon"→"Daejeon" 일괄 수정
ext <- c("\\.R$", "\\.r$")
files <- list.files(getwd(), pattern = paste(ext, collapse="|"),
                    recursive = TRUE, full.names = TRUE)

drop_dirs <- c("^\\.git/", "^renv/", "^packrat/", "^\\.Rproj\\.user/", "^node_modules/")
files <- files[!grepl(paste(drop_dirs, collapse="|"), gsub("\\\\","/", files))]

repl <- list("\\bpop_wighted\\b"="pop_weighted", "\\bMetro\\b"="Metros")

fix_file <- function(path, backup=TRUE){
  txt <- readLines(path, warn=FALSE)            # 인코딩 지정 안 함 (원본 그대로)
  out <- txt
  for(pat in names(repl)){
    out <- gsub(pat, repl[[pat]], out, perl=TRUE, useBytes=TRUE)  # <- 핵심: useBytes=TRUE
  }
  if(!identical(txt, out)){
    if(backup && !file.exists(paste0(path,".bak"))) file.copy(path, paste0(path,".bak"))
    writeLines(out, path, useBytes=TRUE)        # 원본 인코딩 유지
    message("Fixed: ", path); return(TRUE)
  }
  FALSE
}

changed <- vapply(files, fix_file, logical(1))
cat(sprintf("Files changed: %d / %d\n", sum(changed), length(files)))
