control_ops <- list(
  "if" = function(cond, body, orelse = NULL) {
    attr(cond, "expr") <- rlang::enexpr(cond)
    attr(cond, "env") <- rlang::caller_env()
    UseMethod("if", cond)
  },

  "if.default" =  function(cond, body, orelse = NULL) {

    cond_ <- attr(cond, "expr")
    env_ <- attr(cond, "env")

    body_ <- rlang::enexpr(body)
    orelse_ <- rlang::enexpr(orelse)

    e_ <- rlang::expr(base::`if`(!!cond_, !!body_, !!orelse_))

    val <- rlang::eval_bare(e_, env = env_)
    invisible(val)
  },

  "if.tensorflow.tensor" = function(cond, body, orelse = NULL) {

    cond_ <- attr(cond, "expr")
    env_ <- attr(cond, "env")

    body_ <- rlang::enexpr(body)
    orelse_ <- rlang::enexpr(orelse)

    true_fn <- rlang::new_function(
      args = NULL,
      body = rlang::expr({`__res__`<- eval(!!body_, envir = environment()); as.list(environment())}),
      env = env_
    )

    false_fn <- rlang::new_function(
      args = NULL,
      body = rlang::expr({`__res__`<- eval(!!orelse_, envir = environment()); as.list(environment())}),
      env = env_
    )

    env_[["__true_fn__"]] <- true_fn
    env_[["__false_fn__"]] <- false_fn

    on.exit({
      env_[["__true_fn__"]] <- NULL
      env_[["__false_fn__"]] <- NULL
    })

    e_ <- rlang::expr(
      tf$cond(
        !!cond_,
        true_fn = `__true_fn__` ,
        false_fn = `__false_fn__`
      )
    )

    val <- rlang::eval_bare(e_, env = env_)
    res <- val$`__res__`
    val$`__res__` <- NULL

    list2env(val, envir = env_)

    invisible(res)
  }
)

tf_function <- function(fun){
  list2env(control_ops, environment(fun))
  tf$`function`(fun)
}
