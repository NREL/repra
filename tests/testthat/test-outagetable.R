context("Outage table objects")

t1 <- data.frame(Capacity = 10, EFOR = 0.02)
t2 <- data.frame(Capacity = c(10, 20), EFOR = c(0.02, 0.01))

ot1 <- outage_table(t1)
ot2 <- outage_table(t2)

test_that("Outage table format", {
  expect_that(outage_table(t1), is_a("outagetable"))
  expect_that(outage_table(t1), is_a("data.table"))
  expect_that(outage_table(t1), has_names(c("Level", "Area", "Capacity", "Capacity2", "Prob", "LOLP", "BaseEUE")))
})

test_that("Simple outage table", {
  expect_that(ot1$Capacity, equals(c(0, 10)))
  expect_that(ot1$Prob, equals(c(0.02, 0.98)))
  expect_that(ot1$Capacity, equals(ot1$Capacity2))
  expect_that(ot2$Capacity, equals(c(0, 10, 20, 30)))
  expect_that(ot2$Prob, equals(c(0.0002, 0.0098, 0.0198, 0.9702)))
  expect_that(ot2$Capacity, equals(ot2$Capacity2))
})
