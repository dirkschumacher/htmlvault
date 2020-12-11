test_that("function call without error", {
  expect_silent(
    htmlvault_encrypt_file(
      path = system.file(
        "html-template.html",
        package = "htmlvault"
      ),
      output_path = tempfile()
    )
  )
})
