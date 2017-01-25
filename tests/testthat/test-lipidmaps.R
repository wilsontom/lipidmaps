context("lipidmaps")

test_that("lipidmaps",
          {

          expect_true(is.data.frame(LMsearch("ZKRPGPZHULJLKJ-JHRQRACZSA-N")))
          expect_true(is.data.frame(LMsearch("C18H32O2")))
          expect_error(LMsearch("Oleic acid"))
          }
)
