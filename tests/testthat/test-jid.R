test_that("jid() strips non-digits and appends the WhatsApp suffix", {
  expect_equal(jid("5581999990000"), "5581999990000@s.whatsapp.net")
  expect_equal(jid("+55 81 99999-0000"), "5581999990000@s.whatsapp.net")
  expect_equal(jid("(81) 99999 0000"), "81999990000@s.whatsapp.net")
})

test_that("jid() is vectorised over the input", {
  expect_equal(
    jid(c("+5581999990000", "5511988887777")),
    c("5581999990000@s.whatsapp.net", "5511988887777@s.whatsapp.net")
  )
})

test_that("jid() rejects non-character input", {
  expect_error(jid(5581999990000), "must be a character")
  expect_error(jid(character(0)), "must be a character")
})
