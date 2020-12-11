#' Encrypt files and embed them in an self decrypting html file
#'
#' This function takes a file, encrypts the complete file using \code{\link[sodium:data_encrypt]{sodium::data_encrypt}}
#' and a given key. It then injects the encrypted content into an html template that
#' contains the \code{sodium} decryption code compiled to javascript.
#' The resulting file is fully self contained as it can decrypt itself.
#' When the user enters the correct key, the user gets asked to download
#' the decrypted file.
#'
#' @param path the file you want to encrypt
#' @param mime_type optional, the MIME type of your input file.
#' @param extension optional, the extension of your input file. E.g. docx or pdf.
#' @param output_path optional, the output path
#' @param key optional, the encryption key
#' @param output_template_path a path to the output template.
#' The output template needs have the same html form elements (same ids) and the same placeholders as the default template. Everything else can be customized.
#'
#' @details
#' Warning: You are using this at your own risk. Make sure your encryption key is
#' strong enough. For serious use cases, please also review the code of the functions.
#' Any feedback is appreciated. This is an early package version.
#'
#' @return
#' The html code of the resulting file as an invisible character vector.
#'
#' @references
#' The package follows the same approach as the node module \href{https://github.com/derhuerst/self-decrypting-html-page}{self-decrypting-html-page}.
#' The decryption code is based on a number of great node modules.
#' All licenses are also bundled with each html file.
#'
#' @export
htmlvault_encrypt_file <- function(path,
                                   mime_type = mime::guess_type(path),
                                   extension = tools::file_ext(path),
                                   output_path = paste0(path, ".enc.html"),
                                   key = sodium::random(32L),
                                   output_template_path = system.file(
                                     "html-template.html",
                                     package = "htmlvault"
                                   )) {
  stopifnot(
    file.exists(path),
    is.raw(key),
    length(key) >= 32L,
    is.character(extension) && length(extension) == 1 && !is.na(extension),
    is.character(mime_type) && length(mime_type) == 1 && !is.na(mime_type),
    file.exists(output_template_path)
  )
  content <- readBin(path, "raw", n = file.size(path))
  nonce <- sodium::random(24L)
  encrypted_content <- sodium::data_encrypt(content, key, nonce)
  js <- read_pkg_file("html-template.js")
  tpl <- readr::read_file(output_template_path)
  tpl <- inject_raw_data(tpl, "encrypted", encrypted_content)
  tpl <- inject_raw_data(tpl, "nonce", nonce)
  tpl <- inject_raw_data(tpl, "mime_type", mime_type, to_hex = FALSE)
  tpl <- inject_raw_data(tpl, "extension", extension, to_hex = FALSE)
  tpl <- inject_raw_data(tpl, "js", js, to_hex = FALSE)
  readr::write_file(tpl, output_path)
  invisible(tpl)
}

inject_raw_data <- function(template, key, content, to_hex = TRUE) {
  gsub(
    x = template,
    pattern = paste0("{{", key, "}}"),
    replacement = if (to_hex) sodium::bin2hex(content) else content,
    fixed = TRUE
  )
}

read_pkg_file <- function(path) {
  readr::read_file(system.file(path, package = "htmlvault"))
}
