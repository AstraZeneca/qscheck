# Shorter version to extract a parameter from the call. Makes the
# probing more compact, and if the value is not specified, returns
# the default.
callget <- function(call, env, param, default) {
  if (is.null(call[[param]])) {
    return(default)
  }

  return(eval(call[[param]], env))

}
