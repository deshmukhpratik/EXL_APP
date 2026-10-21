# tests/testthat/fixtures/build_fixtures.R
# Regenerate the binary PDF/DOCX fixtures used by test-policy-extractor.R.
# Run from credit_policy_app_portable/ as:
#   Rscript tests/testthat/fixtures/build_fixtures.R

library(grDevices)

dir.create("tests/testthat/fixtures", showWarnings = FALSE, recursive = TRUE)

# ---- sample_policy.pdf: contains all the demo levers ----
sample_text <- c(
  "ACME Personal Loan Credit Policy",
  "",
  "Eligibility:",
  "  FICO >= 700",
  "  DTI <= 35%",
  "  Min credit history: 24 months",
  "  No bankruptcies in last 7 years",
  "",
  "Risk Rules:",
  "  Hard inquiries (6M) <= 3",
  "  Utilization <= 80%",
  "",
  "Pricing:",
  "  Base APR: 11%",
  "",
  "Decision:",
  "  Auto-approve: score > 740",
  "  Manual review: 700-740",
  "  Decline: < 700"
)

pdf("tests/testthat/fixtures/sample_policy.pdf", width = 8, height = 11)
plot.new()
text(0.05, seq(0.95, 0.05, length.out = length(sample_text)),
     sample_text, adj = 0, family = "mono", cex = 0.9)
dev.off()

# ---- no_levers.pdf: readable, but no policy levers ----
no_levers_text <- c(
  "ACME Internal Memo",
  "Subject: Q3 results review",
  "",
  "The team performed well this quarter.",
  "All goals were met. Congratulations to everyone."
)
pdf("tests/testthat/fixtures/no_levers.pdf", width = 8, height = 11)
plot.new()
text(0.05, seq(0.95, 0.05, length.out = length(no_levers_text)),
     no_levers_text, adj = 0, family = "mono", cex = 0.9)
dev.off()

# ---- sample_policy.docx: minimal valid DOCX with same content ----
# DOCX is a ZIP containing [Content_Types].xml + word/document.xml + _rels/.rels
build_docx <- function(out_path, body_lines) {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  writeLines(c(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">',
    '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>',
    '<Default Extension="xml" ContentType="application/xml"/>',
    '<Override PartName="/word/document.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml"/>',
    '</Types>'
  ), file.path(tmp, "[Content_Types].xml"))

  dir.create(file.path(tmp, "_rels"))
  writeLines(c(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
    '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="word/document.xml"/>',
    '</Relationships>'
  ), file.path(tmp, "_rels", ".rels"))

  dir.create(file.path(tmp, "word"))
  # Escape & first (so &-escapes we introduce aren't re-escaped), then < and >.
  xml_escape <- function(s) {
    s <- gsub("&", "&amp;", s, fixed = TRUE)
    s <- gsub("<", "&lt;", s, fixed = TRUE)
    s <- gsub(">", "&gt;", s, fixed = TRUE)
    s
  }
  paragraphs <- vapply(body_lines,
    function(l) paste0('<w:p><w:r><w:t xml:space="preserve">',
                       xml_escape(l),
                       '</w:t></w:r></w:p>'),
    character(1), USE.NAMES = FALSE)
  writeLines(c(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">',
    '<w:body>',
    paragraphs,
    '</w:body></w:document>'
  ), file.path(tmp, "word", "document.xml"))

  old <- setwd(tmp); on.exit(setwd(old), add = TRUE, after = FALSE)
  zip_files <- c("[Content_Types].xml", "_rels/.rels", "word/document.xml")
  abs_out <- normalizePath(file.path(old, out_path), mustWork = FALSE)
  if (file.exists(abs_out)) file.remove(abs_out)
  utils::zip(abs_out, zip_files, flags = "-r9X")
}

build_docx("tests/testthat/fixtures/sample_policy.docx", sample_text)

cat("Fixtures regenerated.\n")
