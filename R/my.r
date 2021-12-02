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
  if (grepl("((^|\n|\\{) *(s|swd|dn|dnl|rg|up|rc|rd)\\()|(<- *rg\\()", paste(input, collapse="\n"))) stop("It seems that the input expression should not be used within s(), please double check.")
  remoter_client_sendrecv(input=input, env=globalenv())
  if (prnt) remoter_repl_printer()
  remoter_client_objcleanup(globalenv())
}


dn <- function(object, newname, env=.GlobalEnv) {
  object <- deparse(substitute(object))
  if (missing(newname)) newname <- object else newname <- deparse(substitute(newname))
  env <- deparse(substitute(env))
  s(input=sprintf('s2c(%s, "%s", %s)', object, newname, env), prnt=FALSE)
}


rg <- function(EXPR, input) {
  if (missing(input)) input <- deparse(substitute(EXPR))
  if (grepl("((^|\n|\\{) *(s|swd|dn|dnl|rg|up|rc|rd)\\()|(<- *rg\\()", paste(input, collapse="\n"))) stop("It seems that the input expression should not be used within rg(), please double check.")
  s(input=c(".tmp <- ", input))
  dn(.tmp, res, parent.frame(5)) # parent.frame(5) finally get `res` to appear here in the running env
  s(rm(.tmp))
  res
}


dnl <- function(x, newname, env=.GlobalEnv) {
  x <- deparse(substitute(x))
  if (missing(newname)) newname <- x else newname <- deparse(substitute(newname))
  ns <- rg(input=sprintf("{if (is.null(names(%s))) 1:length(%s) else names(%s)}", x, x, x))
  res <- list()
  if (is.numeric(ns)) {
    for (i in ns) {
      res[[i]] <- rg(input=sprintf('%s[[%s]]', x, i))
    }
  } else {
    for (i in ns) {
      res[[i]] <- rg(input=sprintf('%s[["%s"]]', x, i))
    }
  }
  assign(newname, value=res, envir=env)
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


