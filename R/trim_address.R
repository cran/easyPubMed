trim_address <-
function(addr) {
  out.addr <- gsub("(^([[:space:]]{0,5}[[:digit:]]{1,2}[[:space:]]{0,2}))", "", addr)
  out.addr <- gsub("(\\.).{0,5}$", "", out.addr)
  out.addr <- gsub("(\\;).*$", "", out.addr)
  return (out.addr)
}
