test_that(".evo_path() joins segments with a single slash", {
  expect_equal(.evo_path("message", "sendText", "inst"), "message/sendText/inst")
  expect_equal(.evo_path("chat", "whatsappNumbers", "inst"), "chat/whatsappNumbers/inst")
})

test_that(".compact() removes only NULL elements", {
  expect_equal(.compact(list(a = 1, b = NULL, c = "x")), list(a = 1, c = "x"))
  # NA, FALSE and empty strings must be preserved (only NULL is dropped)
  expect_equal(.compact(list(a = NA, b = FALSE, c = "")), list(a = NA, b = FALSE, c = ""))
  expect_equal(.compact(list()), list())
})

test_that(".assert_scalar_string() accepts a single non-empty string", {
  expect_silent(.assert_scalar_string("ok", "x"))
})

test_that(".assert_scalar_string() rejects invalid values", {
  expect_error(.assert_scalar_string("", "x"), "non-empty character")
  expect_error(.assert_scalar_string(c("a", "b"), "x"), "single non-empty")
  expect_error(.assert_scalar_string(123, "x"), "single non-empty")
  expect_error(.assert_scalar_string(NULL, "x"), "single non-empty")
})

test_that(".normalize_number() strips formatting from plain numbers", {
  expect_equal(.normalize_number("+55 81 99999-0000"), "5581999990000")
  expect_equal(.normalize_number("(81) 99999 0000"), "81999990000")
  expect_equal(.normalize_number("5581999990000"), "5581999990000")
})

test_that(".normalize_number() passes JIDs through unchanged", {
  expect_equal(.normalize_number("120363000000000000@g.us"),
               "120363000000000000@g.us")
  expect_equal(.normalize_number("5581999990000@s.whatsapp.net"),
               "5581999990000@s.whatsapp.net")
})

test_that(".normalize_number() errors when no digits remain", {
  expect_error(.normalize_number("+"), "does not contain any digits")
})

test_that("%||% returns the left side unless it is NULL", {
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% 2, 2)
  expect_equal("" %||% "fallback", "")
})
