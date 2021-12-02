rc <- function(addr, port=55555, password=NULL, timer=FALSE) {
  check.is.flag(timer)
  validate_address(addr)
  addr <- scrub_addr(addr)
  validate_port(port, warn=FALSE)
  test_connection(addr, port)
  reset_state()
  set(whoami, "local")
  set(timer, timer)
  set(port, port)
  set(remote_addr, addr)
  set(clientpw, password)
  set(isbatch, FALSE)
  if (isWindows()) grDevices::windows.options(rescale="fit")

  test <- remoter_init_client()
  if (!test) return(FALSE)
  timer <- getval(timer)
  EVALFUN <- timerfun(timer)
  remoter_client_sendrecv(input="remoter_env_sync", env=globalenv())
  set.status(continuation, FALSE)
  set.status(visible, FALSE)
  remoter_client_objcleanup(globalenv())

  invisible(TRUE)
}


swd <- function() {
  input <- sprintf('setwd("%s")', getwd())
  remoter:::remoter_client_sendrecv(input=input, env=globalenv())
  remoter:::remoter_client_objcleanup(globalenv())
}


s <- function(EXPR, input, prnt=TRUE) {
  if (missing(input)) input <- deparse(substitute(EXPR))
  if (grepl("(^|\n|\\{|\\(|\\[|<-) *(s|swd|dn|dnl|rg|rgl|up|rc|rd)\\(", paste(input, collapse="\n"))) stop("It seems that the input expression should not be used within s(), please double check.")
  remoter_client_sendrecv(input=input, env=globalenv())
  if (prnt) remoter_repl_printer()
  remoter_client_objcleanup(globalenv())
}


rg <- function(EXPR, input) {
  if (missing(input)) input <- deparse(substitute(EXPR))
  s(input=c(".tmp <- ", input))
  s(s2c(.tmp, "res", parent.frame(4))) # parent.frame(4) gets `res` into the current running environment of rg()
  s(rm(.tmp))
  res
}


dn <- function(object) {
  object <- deparse(substitute(object))
  res <- rg(input=object)
  assign(object, value=res, envir=parent.frame())
}


rgl <- function(EXPR, input) {
  if (missing(input)) input <- deparse(substitute(EXPR))
  s(input=c(".tmp1 <- ", input))
  ns <- rg(input="{if (is.null(names(.tmp1))) 1:length(.tmp1) else names(.tmp1)}")
  res <- list()
  if (is.numeric(ns)) {
    for (i in ns) {
      res[[i]] <- rg(input=sprintf(".tmp1[[%d]]", i))
    }
  } else {
    for (i in ns) {
      res[[i]] <- rg(input=sprintf('.tmp1[["%s"]]', i))
    }
  }
  s(rm(.tmp1))
  res
}


dnl <- function(object) {
  object <- deparse(substitute(object))
  res <- rgl(input=object)
  assign(object, value=res, envir=parent.frame())
}


up <- function(object, newname, env=.GlobalEnv) {
  object <- deparse(substitute(object))
  if (missing(newname)) newname <- object else newname <- deparse(substitute(newname))
  env <- deparse(substitute(env))
  s(input=sprintf('c2s(%s, "%s", %s)', object, newname, env), prnt=FALSE)
}


rd <- function() {
  remoter_client_sendrecv(input="exit()", env=globalenv())
  remoter_client_objcleanup(globalenv())
}


