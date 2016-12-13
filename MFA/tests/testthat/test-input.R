context("Input test")

test_that("check_center with ok argument", {

  expect_true(check_center(TRUE))
})


test_that("check_center with incorrect argument", {

  expect_error(check_center(3))
})

test_that("check_scale with ok argument", {

  expect_true(check_center(TRUE))
})


test_that("check_scale with incorrect argument", {

  expect_error(check_center("Z"))
})

test_that("check_sets with ok list", {

  expect_true(check_sets(matrix(1:9,nrow=3),list(1:2,2:3)))
})


test_that("check_sets fails with invalid lengths", {

  expect_error(check_sets(matrix(1:9,nrow=3),list(1:8)))
  expect_error(check_sets(matrix(1:9,nrow=3),list(1:2,3:5)))
})

test_that("check_ncomps with ok argument", {

  expect_true(check_ncomps(matrix(1:9,nrow=3,ncol=3),2))
})


test_that("check_ncomps fails with invalid lengths", {

  expect_error(check_ncomps(matrix(1:9,nrow=3,ncol=3),0))
  expect_error(check_ncomps(matrix(1:9,nrow=3,ncol=3),6))
})

test_that("check_tables with ok argument", {

  expect_true(check_tables(matrix(1:9,nrow=3,ncol=3),matrix(1:6,nrow=3,ncol=2)))
})


test_that("check_tables fails with invalid dimensions", {

  expect_error(check_tables(matrix(1:9,nrow=3,ncol=3),matrix(1:6,nrow=2,ncol=3)))
})

test_that("check_orders with ok argument", {

  expect_true(check_orders(1,2,mfa(mtcars,sets=list(1:3,4:6,7:8,9:11))))
})


test_that("check_orders fails with invalid input", {
  expect_error(check_orders(6,2,mfa(mtcars,sets=list(1:3,4:6,7:8,9:11))))
  expect_error(check_orders(4,5,mfa(mtcars,sets=list(1:3,4:6,7:8,9:11))))
})

test_that("check_types with ok argument", {

  expect_true(check_types(1))
})


test_that("check_types fails with invalid input", {
  expect_error(check_orders(4))
})


test_that("check_label with ok argument", {

  expect_true(check_label(mfa(mtcars[c(1:2,4:5,8:9,20:21),],sets=list(1:3,4:6,7:8,9:11)),labels = c("Maz1","Maz2","Hor1","Hor2","Merc1","Merc2","Toyota1","Toyota2")))
})

test_that("check_label fails with incorrect length", {
  expect_error(check_label(mfa(mtcars[c(1:2,4:5,8:9,20:21),],sets=list(1:3,4:6,7:8,9:11)),labels = c(1:13)))

})


test_that("check_pos_integer with ok number", {

  expect_true(check_pos_integer(3))
})


test_that("check_pos_integer fails with invalid number", {

  expect_error(check_pos_integer(-10))
  expect_error(check_pos_integer(2.1))
})

test_that("check_dim with ok number", {

  expect_true(check_dim(mfa(data=mtcars,sets=list(1:3,4:6,7:8,9:11)),dim=1))
})


test_that("check_dim with invalid number", {

  expect_error(check_dim(mfa(data=mtcars,sets=list(1:3,4:6,7:8,9:11)),dim=20))
  expect_error(check_dim(mfa(data=mtcars,sets=list(1:3,4:6,7:8,9:11)),dim=-1))
})
