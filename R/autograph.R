get_and_clean_env <- rlang::expr({
  env <- as.list(environment())
  env[["__true_fn__"]] <- NULL
  env[["__false_fn__"]] <- NULL
  env
})

control_ops <- list(
  "if" = function(cond, body, orelse = NULL) {

    `if` <- .Primitive("if")

    env_ <- rlang::caller_env()
    cond_ <- rlang::enexpr(cond)
    body_ <- rlang::enexpr(body)
    orelse_ <- rlang::enexpr(orelse)

    if (inherits(cond, "tensorflow.tensor")) {
      control_ops$`if.tensorflow.tensor`(cond_, body_, orelse_, env_)
    } else {
      control_ops$`if.default`(cond_, body_, orelse_, env_)
    }

  },

  "if.default" =  function(cond_, body_, orelse_ = NULL, env_) {
    e_ <- rlang::expr(base::`if`(!!cond_, !!body_, !!orelse_))

    val <- rlang::eval_bare(e_, env = env_)
    invisible(val)
  },

  "if.tensorflow.tensor" = function(cond_, body_, orelse_ = NULL, env_) {

    true_fn <- rlang::new_function(
      args = NULL,
      body = rlang::expr({
        `__res__`<- eval(!!body_, envir = environment())
        !!get_and_clean_env
      }),
      env = env_
    )

    false_fn <- rlang::new_function(
      args = NULL,
      body = rlang::expr({
        `__res__`<- eval(!!orelse_, envir = environment())
        !!get_and_clean_env
      }),
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
