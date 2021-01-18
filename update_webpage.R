require(pkgdown)
setwd("~/Work/research/LGPR/lgpr-usage")
orig_dir <- getwd()

# Rename current docs dir
file.rename("docs", "docs_old")

# Get path to lgpr (and new docs and tutorials)
lgpr_dir <- "../lgpr/"
doc_dir <- file.path(lgpr_dir, "docs")
tut_dir <- file.path(lgpr_dir, "tutorials")

# Copy docs from lgpr dir
file.copy(doc_dir, ".", recursive = TRUE)

# Copy front page figure
file.copy("docs_old/fig1.png", "docs")

# Create tutorials dir
dir.create("docs/tutorials")

# Copy html there
setwd(tut_dir)
fns <- list.files(pattern = "\\.html$", recursive = TRUE)
setwd(orig_dir)
L <- length(fns)
for (j in seq_len(L)) {
  fn <- file.path(lgpr_dir, "tutorials", fns[j])
  cat("copying from", fn, "\n")
  file.copy(fn, "docs/tutorials")
}

# Edit index.html
create_new_html <- function() {
  html_add <- readLines("html_insert.txt")
  html_add <- paste(html_add, collapse = "\n")
  
  html_old <- readLines("docs/index.html")
  num_lines <- length(html_old)
  s1 <- "<p>R package for <strong>L</strong>ongitudinal <strong>G</strong>aussian <strong>P</strong>rocess <strong>R</strong>egression.</p>"
  s2 <- "<div id=\"requirements\" class=\"section level2\">"
  line1 <- which(html_old == s1)
  line2 <- which(html_old == s2)
  
  str1 <- paste(html_old[1:line1], collapse = "\n")
  str2 <- paste(html_old[line2:num_lines], collapse = "\n")
  html_new <- paste(str1, html_add, str2, sep = "\n")
  return(html_new)
}
new_html <- create_new_html()
writeLines(new_html, con = "docs/index.html")

# Remove docs_old after running this



