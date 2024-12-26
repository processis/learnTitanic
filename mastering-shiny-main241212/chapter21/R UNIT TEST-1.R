install.packages("testthat")
library(testthat)

# 编写单元测试


# 定义一个简单的函数
add <- function(x, y) {
  return(x + y)
}

# 编写单元测试
test_that("add函数测试", {
  expect_equal(add(3, 4), 7)
})

#运行单元测试
test_dir("tests/")