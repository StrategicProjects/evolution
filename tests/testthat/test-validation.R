# Argument validation happens before any network call, so these tests run
# fully offline. A fake client is enough to exercise the guard clauses.

fake_client <- function() {
  evo_client(base_url = "https://example.com", api_key = "k", instance = "inst")
}

test_that("evo_client() validates its arguments", {
  expect_error(evo_client("", "k", "i"), "base_url")
  expect_error(evo_client("https://x", "", "i"), "api_key")
  expect_error(evo_client("https://x", "k", ""), "instance")
})

test_that("evo_client() builds a request and prints cleanly", {
  cl <- fake_client()
  expect_s3_class(cl, "evo_client")
  expect_equal(cl$instance, "inst")
  expect_invisible(print(cl))
  out <- cli::cli_fmt(print(cl))
  expect_true(any(grepl("Evolution API Client", out)))
  expect_true(any(grepl("inst", out)))
})

test_that("evo_client() strips trailing slashes from base_url", {
  cl <- evo_client("https://example.com///", "k", "inst")
  expect_equal(cl$req$url, "https://example.com")
})

test_that(".evo_post() rejects a non-client object", {
  expect_error(.evo_post(list(), "p", list()), "evo_client")
})

test_that("send_text() validates number and text before any request", {
  cl <- fake_client()
  expect_error(send_text(cl, "", "hi"), "number")
  expect_error(send_text(cl, "5581", ""), "text")
})

test_that("send_media() validates mediatype", {
  cl <- fake_client()
  expect_error(
    send_media(cl, "5581", "audio", "audio/ogg", media = "https://x/a.ogg",
               file_name = "a.ogg"),
    "mediatype"
  )
})

test_that("send_location() requires numeric coordinates", {
  cl <- fake_client()
  expect_error(send_location(cl, "5581", latitude = "x", longitude = 1),
               "must be numeric")
})

test_that("send_poll() requires at least two options", {
  cl <- fake_client()
  expect_error(send_poll(cl, "5581", name = "Q?", values = "only one"),
               "at least 2 options")
})

test_that("send_reaction() validates key and reaction", {
  cl <- fake_client()
  expect_error(send_reaction(cl, key = list(), reaction = "x"), "id")
  expect_error(
    send_reaction(cl, key = list(id = "1"), reaction = c("a", "b")),
    "single character"
  )
})

test_that("check_is_whatsapp() requires a non-empty character vector", {
  cl <- fake_client()
  expect_error(check_is_whatsapp(cl, numbers = character(0)), "non-empty")
})
