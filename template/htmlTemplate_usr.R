template_dfa = function (x) 
{
  .Call("htmltools_template_dfa", PACKAGE = "htmltools", x)
}

htmlTemplate.usr = function (filename = NULL, vars, text_ = NULL, document_ = "auto") 
{
  if (!xor(is.null(filename), is.null(text_))) {
    stop("htmlTemplate requires either `filename` or `text_`.")
  }
  if (!is.null(filename)) {
    html <- readChar(filename, file.info(filename)$size, 
                     useBytes = TRUE)
    Encoding(html) <- "UTF-8"
  }
  else if (!is.null(text_)) {
    text_ <- paste8(text_, collapse = "\\n")
    html <- enc2utf8(text_)
  }
  pieces <- template_dfa(html)
  Encoding(pieces) <- "UTF-8"
  if ("headContent" %in% names(vars)) {
    stop("Can't use reserved argument name 'headContent'.")
  }
  vars$headContent <- function() HTML("<!-- HEAD_CONTENT -->")
  env <- list2env(vars, parent = globalenv())
  pieces <- mapply(pieces, rep_len(c(FALSE, TRUE), length.out = length(pieces)), 
                   FUN = function(piece, isCode) {
                     if (isCode) {
                       eval(parse(text = piece), env)
                     }
                     else if (piece == "") {
                       NULL
                     }
                     else {
                       HTML(piece)
                     }
                   }, SIMPLIFY = FALSE)
  result <- tagList(pieces)
  if (document_ == "auto") {
    document_ = grepl("<HTML(\\\\s[^<]*)?>", html, ignore.case = TRUE)
  }
  if (document_) {
    class(result) <- c("html_document", class(result))
  }
  result
}