context("Output test")
wines<-read.csv("https://raw.githubusercontent.com/ucb-stat243/stat243-fall-2016/master/problem-sets/final-project/data/wines.csv")
X<-as.matrix(wines[1:12,2:54])
sets<-list(1:6,7:12,13:18,19:23,24:29,30:34,35:38,39:44,45:49,50:53)
MFA<-mfa(X,sets)
singular.value<-c(0.878,0.351,0.301,0.276,0.244,0.198,0.176,0.158,0.137,0.116,0.106)
eigen.value<-c(0.770,0.123,0.091,0.076,0.060,0.039,0.031,0.025,0.019,0.013,0.011)
cumulative<-c(0.770,0.893,0.984,1.060,1.120,1.159,1.190,1.215,1.233,1.247,1.258)
test_that("check correct singular value from MatrixA", {
  expect_true(identical(round(MFA$MatrixA[[1]],3),singular.value))
})

test_that("check correct eigen value from MatrixA", {
  expect_true(identical(round(MFA$MatrixA[[2]],3),eigen.value))
})

test_that("check correct cumulative from MatrixA", {
  expect_true(identical(round(MFA$MatrixA[[3]],3),cumulative))
})

factor.score<-matrix(c(-0.980,-0.809,-0.761,-1.115,1.373,1.264,0.808,0.925,-0.669,0.073,-0.476,0.367,0.163,0.033,-0.454,-0.166,-0.128,-0.108,0.205,0.408,0.369,-0.757,0.513,-0.076),ncol=2)

test_that("check correct factor score from MatrixF with 2 dimensions", {
  expect_true(identical(matrix(round(MFA$MatrixF[,1:2],3),ncol=2),factor.score))
})

MFA1<-mfa(X,sets,ncomps=2)
partial.fs <- matrix(c(-1.037,-1.179,-0.213,-0.946,1.546,1.176,0.698,1.006,-0.922,0.189,-0.643,0.323,0.155,0.596,-0.104,0.446,-0.676,-0.747,0.166,-0.063,0.486,-0.936,0.640,0.036),ncol=2)

test_that("check correct partial factor score from MatrixEFG with 2 dimensions", {
  expect_true(identical(matrix(round(MFA1$MatrixEFG[[1]],3),ncol=2),partial.fs))
})

contr<-matrix(c(0.101,0.101,0.089,0.085,0.077,0.080,0.059,0.085,0.092,0.082,0.095,0.069,0.133,0.043,0.050,0.083,0.130,0.119,0.048,0.044),ncol=2)

test_that("check for correct contribution", {
  expect_true(identical(matrix(round(contributions(X,sets,type=3)[,1:2],3),ncol=2),contr))
})



partial.inertia <- matrix(c(0.0779,0.0771,0.0778,0.0743,0.0751,0.0776,0.0787,0.0736,0.0771,0.081,0.0117,0.0084,0.0186,0.006,0.0078,0.0128,0.0275,0.0165,0.0065,0.007),ncol=2)
test_that("check for correct partial inertia", {
  expect_true(identical(matrix(round(mfa(X,sets,ncomps=2)$MatrixEscVar,4),ncol=2),partial.inertia))
})
