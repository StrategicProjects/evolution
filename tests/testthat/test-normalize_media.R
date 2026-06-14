test_that(".normalize_media() returns HTTP(S) URLs unchanged", {
  expect_equal(.normalize_media("https://example.com/a.png"),
               "https://example.com/a.png")
  expect_equal(.normalize_media("HTTP://example.com/a.png"),
               "HTTP://example.com/a.png")
})

test_that(".normalize_media() encodes existing local files to base64", {
  tmp <- tempfile(fileext = ".bin")
  writeBin(as.raw(c(1, 2, 3, 4)), tmp)
  on.exit(unlink(tmp), add = TRUE)

  out <- suppressMessages(.normalize_media(tmp))
  expect_equal(out, base64enc::base64encode(tmp))
})

test_that(".normalize_media() strips data-URI prefixes", {
  raw <- base64enc::base64encode(charToRaw("hello"))
  expect_equal(.normalize_media(paste0("data:text/plain;base64,", raw)), raw)
})

test_that(".normalize_media() passes through clean base64", {
  raw <- base64enc::base64encode(charToRaw("hello world"))
  expect_equal(.normalize_media(raw), raw)
})

test_that(".normalize_media() rejects clearly invalid input", {
  # No path-like characters and not valid base64 -> generic error.
  expect_error(.normalize_media("!!!!"), "does not appear to be a valid")
  expect_error(.normalize_media(c("a", "b")), "single string")
})

test_that(".normalize_media() gives a path-aware error for missing files", {
  expect_error(.normalize_media("does-not-exist.pdf"),
               "looks like a file path")
  expect_error(.normalize_media("~/no/such/file.png"),
               "looks like a file path")
})

test_that(".normalize_media() does not treat a mistyped path as base64", {
  # "report.pdf" matches no base64 (has a dot) and the file is absent.
  expect_error(.normalize_media("report.pdf"), "looks like a file path")
})
