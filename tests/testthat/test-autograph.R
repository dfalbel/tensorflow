source("utils.R")

test_that("simple function", {
  skip_if_no_tensorflow()

  fun <- tf_function(function(x) {
    tf$add(x, 1)
  })

  res <- as.numeric(fun(tf$constant(5)))

  expect_equal(res, 6)
})

test_that("works with if and tf.bool inside function", {
  skip_if_no_tensorflow()

  fun <- tf_function(function(x) {
    if (tf$reduce_sum(x) > 5)
      x * 100
    else
      x * 10
  })

  res <- as.numeric(fun(tf$constant(10)))
  expect_equal(res, 1000)

  res <- as.numeric(fun(tf$constant(4), FALSE))
  expect_equal(res, 40)


  fun <- tf_function(function(x) {
    if (tf$reduce_sum(x) > 5)
      y <- x * 100
    else
      y <- x * 10

    y + 1
  })

  res <- as.numeric(fun(tf$constant(10)))
  expect_equal(res, 1001)

  res <- as.numeric(fun(tf$constant(4), FALSE))
  expect_equal(res, 41)
})




