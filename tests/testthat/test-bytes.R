describe("as_bench_bytes", {
  it("accepts numeric input unchanged", {
    expect_equal(unclass(as_bench_bytes(123L)), 123L)
    expect_equal(unclass(as_bench_bytes(123)), 123)
  })
  it("accepts bench_byte input unchanged", {
    x <- as_bench_bytes(123)
    expect_equal(as_bench_bytes(x), x)
  })
  it("coerces character input", {
    expect_equal(unclass(as_bench_bytes("1")), 1)
    expect_equal(unclass(as_bench_bytes("1K")), 1024)
    expect_equal(unclass(as_bench_bytes("1M")), 1024 * 1024)
    expect_equal(unclass(as_bench_bytes("10M")), 10 * 1024 * 1024)
    expect_equal(unclass(as_bench_bytes("1G")), 1024 * 1024 * 1024)
  })
})

describe("format.bench_bytes", {
  it("formats bytes under 1024 as whole numbers", {
    expect_equal(format(bench_bytes(0)), "0B")
    expect_equal(format(bench_bytes(1)), "1B")
    expect_equal(format(bench_bytes(1023)), "1023B")
  })
  it("formats bytes 1024 and up as abbreviated numbers", {
    expect_equal(format(bench_bytes(1024)), "1KB")
    expect_equal(format(bench_bytes(1025)), "1KB")
    expect_equal(format(bench_bytes(2^16)), "64KB")
    expect_equal(format(bench_bytes(2^24)), "16MB")
    expect_equal(format(bench_bytes(2^24 + 555555)), "16.5MB")
    expect_equal(format(bench_bytes(2^32)), "4GB")
    expect_equal(format(bench_bytes(2^48)), "256TB")
    expect_equal(format(bench_bytes(2^64)), "16EB")
  })
  it("handles NA and NaN", {
    expect_equal(format(bench_bytes(NA)), "NA")
    expect_equal(format(bench_bytes(NaN)), "NaN")
  })
  it("works with vectors", {
    v <- c(NA, 1, 2^13, 2^20, NaN, 2^15)
    expect_equal(
      format(bench_bytes(v), trim = TRUE),
      c("NA", "1B", "8KB", "1MB", "NaN", "32KB"))

    expect_equal(format(bench_bytes(numeric())), character())
  })
})

describe("sum.bench_bytes", {
  it("sums its input and returns a bench_byte", {
    expect_equal(sum(bench_bytes(0)), new_bench_bytes(0))
    expect_equal(sum(bench_bytes(c(1, 2))), new_bench_bytes(3))
    expect_equal(sum(bench_bytes(c(1, NA))), new_bench_bytes(NA_real_))
  })
})

describe("min.bench_bytes", {
  it("finds minimum input and returns a bench_byte", {
    expect_equal(min(bench_bytes(0)), new_bench_bytes(0))
    expect_equal(min(bench_bytes(c(1, 2))), new_bench_bytes(1))
    expect_equal(min(bench_bytes(c(1, NA))), new_bench_bytes(NA_real_))
  })
})

describe("max.bench_bytes", {
  it("finds maximum input and returns a bench_byte", {
    expect_equal(max(bench_bytes(0)), new_bench_bytes(0))
    expect_equal(max(bench_bytes(c(1, 2))), new_bench_bytes(2))
    expect_equal(max(bench_bytes(c(1, NA))), new_bench_bytes(NA_real_))
  })
})

describe("[.bench_bytes", {
  it("retains the bench_bytes class", {
    x <- bench_bytes(c(100, 200, 300))
    expect_equal(x[], x)
    expect_equal(x[1], new_bench_bytes(100))
    expect_equal(x[1:2], new_bench_bytes(c(100, 200)))
  })
})

describe("Ops.bench_bytes", {
  it("errors for unary operators", {
    x <- bench_bytes(c(100, 200, 300))
    expect_error(!x, "unary '!' not defined for \"bench_bytes\" objects")
    expect_error(+x, "unary '\\+' not defined for \"bench_bytes\" objects")
    expect_error(-x, "unary '-' not defined for \"bench_bytes\" objects")
  })

  it("works with boolean comparison operators", {
    x <- bench_bytes(c(100, 200, 300))

    expect_equal(x == 100, c(TRUE, FALSE, FALSE))
    expect_equal(x != 100, c(FALSE, TRUE, TRUE))
    expect_equal(x > 100, c(FALSE, TRUE, TRUE))
    expect_equal(x >= 100, c(TRUE, TRUE, TRUE))
    expect_equal(x < 200, c(TRUE, FALSE, FALSE))
    expect_equal(x <= 200, c(TRUE, TRUE, FALSE))
  })

  it("works with arithmetic operators", {
    x <- bench_bytes(c(100, 200, 300))

    expect_equal(x + 100, bench_bytes(c(200, 300, 400)))
    expect_equal(x - 100, bench_bytes(c(0, 100, 200)))
    expect_equal(x * 100, bench_bytes(c(10000, 20000, 30000)))
    expect_equal(x / 2, bench_bytes(c(50, 100, 150)))
    expect_equal(x ^ 2, bench_bytes(c(10000, 40000, 90000)))
  })

  it("errors for other binary operators", {
    x <- bench_bytes(c(100, 200, 300))
    expect_error(x %% 2, "'%%' not defined for \"bench_bytes\" objects")
    expect_error(x %/% 2, "'%/%' not defined for \"bench_bytes\" objects")
    expect_error(x & TRUE, "'&' not defined for \"bench_bytes\" objects")
    expect_error(x | TRUE, "'|' not defined for \"bench_bytes\" objects")
  })
})
