library(testthat)
library(NUSS)

ndict  <- data.frame(
  to_search = c("is", "science", "scienceis", "this", "thisis", "a", "approach", "ascientific", "ascientificapproach", "beauty", "beautyof", "beautyofscience", "everywhere", "isa", "isascientific", "isascientificapproach", "iseverywhere", "isscience", "of", "ofscience", "scienceiseverywhere", "scientific", "scientificapproach", "the", "thebeauty", "thebeautyof", "thebeautyofscience", "thisisa", "thisisascientific", "thisisascientificapproach", "thisisscience"),
  to_replace = c("is", "science", "science is", "this", "this is", "a", "approach", "a scientific", "a scientific approach", "beauty", "beauty of", "beauty of science", "everywhere", "is a", "is a scientific", "is a scientific approach", "is everywhere", "is science", "of", "of science", "science is everywhere", "scientific", "scientific approach", "the", "the beauty", "the beauty of", "the beauty of science", "this is a", "this is a scientific", "this is a scientific approach", "this is science"),
  id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31),
  points = c(4, 4, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
)

test_that("ngrams_segmentation works correctly", {
  # Test case 1: Basic usage
  expect_equal(
    NUSS::ngrams_segmentation("thisisscience", ndict)[,1:2],
    data.frame(
      sequence = c("thisisscience"),
      segmented = c("this is science")
      )
    )

  # Test case 2: Multiple sequences
  sequences <- c("scienceiseverywhere", "thisisscience")
  result <- NUSS::ngrams_segmentation(sequences, ndict)
  expect_equal(
    result[, 1:2],
    data.frame(
      sequence = c("scienceiseverywhere", "thisisscience"),
      segmented = c("science is everywhere", "this is science")
    )
  )
})
