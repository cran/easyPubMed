custom_grep <-
function(xml_data, tag, format = "list"){
  #
  x <- xml_data
  tag.op <- paste("\\<", tag, "((\\>)|([[:space:]](.*)\\>))", sep = "")
  tag.cl <- paste("(<\\/)", tag, "(\\>)", sep = "")
  #
  out.result <- list()
  i = 1
  while (nchar(x) > 0 &
         regexpr(tag.op, x) > 0 &
         regexpr(tag.cl, x) > 0){
    tag.op.pos <- regexpr(tag.op, x)
    nu.x <- substr(x, (tag.op.pos - 1), nchar(x))
    inner.trim <- regexpr(">", nu.x, fixed = TRUE)
    nu.x <- substr(nu.x, (inner.trim + 1), nchar(nu.x))
    #
    tag.cl.pos <- regexpr(tag.cl, nu.x)
    tag.cl.full <- tag.cl.pos + attributes(tag.cl.pos)$match.length + 1
    x <- substr(nu.x, tag.cl.full, nchar(x))
    nu.x <- substr(nu.x, 1, (tag.cl.pos - 1))
    #
    out.result[[i]] <- nu.x
    i <- i + 1
  }
  if (format != "list") {
    out.result <- do.call(c, out.result)
  }
  return(out.result)
}
