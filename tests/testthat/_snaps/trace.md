# tree printing only changes deliberately

    Code
      print(trace, dir = dir)
    Output
          x
       1. \-rlang i() at test-trace.R:25:2
       2.   \-rlang j(...) at test-trace.R:18:7
       3.     \-rlang k(...) at test-trace.R:19:21
       4.       \-rlang l(...) at test-trace.R:22:4
    Code
      cat("\n")
    Output
      
    Code
      print(trace_slice(trace, 0L), dir = dir)
    Output
      x

# can print tree with collapsed branches

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
           x
        1. \-rlang f()
        2.   \-rlang g() at test-trace.R:49:20
        3.     +-base::tryCatch(...) at test-trace.R:50:20
        4.     | \-base tryCatchList(...)
        5.     |   +-base tryCatchOne(...)
        6.     |   | \-base doTryCatch(...)
        7.     |   \-base tryCatchList(...)
        8.     |     \-base tryCatchOne(...)
        9.     |       \-base doTryCatch(...)
       10.     \-rlang h()
       11.       +-base::tryCatch(...) at test-trace.R:51:20
       12.       | \-base tryCatchList(...)
       13.       |   \-base tryCatchOne(...)
       14.       |     \-base doTryCatch(...)
       15.       \-rlang i()
       16.         +-base::tryCatch(...) at test-trace.R:52:20
       17.         | \-base tryCatchList(...)
       18.         \-rlang::trace_back(...)
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
           x
        1. \-rlang f()
        2.   \-rlang g() at test-trace.R:49:20
        3.     +-[ base::tryCatch(...) ] with 6 more calls at test-trace.R:50:20
       10.     \-rlang h()
       11.       +-[ base::tryCatch(...) ] with 3 more calls at test-trace.R:51:20
       15.       \-rlang i()
       16.         +-[ base::tryCatch(...) ] with 1 more call at test-trace.R:52:20
       18.         \-rlang::trace_back(...)
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
        1. rlang f()
        2. rlang g()
           at test-trace.R:49:20
       10. rlang h()
       15. rlang i()
       18. rlang::trace_back(...)

---

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
           x
        1. \-rlang f()
        2.   +-base::eval(...) at test-trace.R:61:7
        3.   | \-base::eval(...)
        4.   +-base::eval(...)
        5.   | \-base::eval(...)
        6.   \-rlang g()
        7.     +-base::tryCatch(...) at test-trace.R:62:7
        8.     | \-base tryCatchList(...)
        9.     |   +-base tryCatchOne(...)
       10.     |   | \-base doTryCatch(...)
       11.     |   \-base tryCatchList(...)
       12.     |     \-base tryCatchOne(...)
       13.     |       \-base doTryCatch(...)
       14.     +-base::eval(...)
       15.     | \-base::eval(...)
       16.     \-rlang h()
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
           x
        1. \-rlang f()
        2.   +-[ base::eval(...) ] with 1 more call at test-trace.R:61:7
        4.   +-[ base::eval(...) ] with 1 more call
        6.   \-rlang g()
        7.     +-[ base::tryCatch(...) ] with 6 more calls at test-trace.R:62:7
       14.     +-[ base::eval(...) ] with 1 more call
       16.     \-rlang h()
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
        1. rlang f()
        6. rlang g()
       16. rlang h()

# cli_branch() handles edge case

    Code
      cli_branch(tree[-1, ])
    Output
      [1] " 1. rlang f()"

# collapsed formatting doesn't collapse single frame siblings

    Code
      print(trace, simplify = "none", srcrefs = FALSE)
    Output
          x
       1. \-rlang f()
       2.   +-rlang::eval_bare(...)
       3.   \-rlang g()
    Code
      print(trace, simplify = "collapse", srcrefs = FALSE)
    Output
          x
       1. \-rlang f()
       2.   +-rlang::eval_bare(...)
       3.   \-rlang g()

# recursive frames are rewired to the global env

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-rlang::eval_tidy(...)
       2. \-rlang f()
       3.   \-rlang g()
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-rlang::eval_tidy(...)
       2. \-rlang f()
       3.   \-rlang g()
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. rlang::eval_tidy(...)
       2. rlang f()
       3. rlang g()

# long backtrace branches are truncated

    Code
      cat("Full:\n")
    Output
      Full:
    Code
      print(trace, simplify = "branch", srcrefs = FALSE)
    Output
        1. rlang f(...)
        2. rlang f(...)
        3. rlang f(...)
        4. rlang f(...)
        5. rlang f(...)
        6. rlang f(...)
        7. rlang f(...)
        8. rlang f(...)
        9. rlang f(...)
       10. rlang f(...)
       11. rlang f(...)
    Code
      cat("\n5 frames:\n")
    Output
      
      5 frames:
    Code
      print(trace, simplify = "branch", max_frames = 5, srcrefs = FALSE)
    Output
        1. rlang f(...)
        2. rlang f(...)
        3. rlang f(...)
           ...
       10. rlang f(...)
       11. rlang f(...)
    Code
      cat("\n2 frames:\n")
    Output
      
      2 frames:
    Code
      print(trace, simplify = "branch", max_frames = 2, srcrefs = FALSE)
    Output
        1. rlang f(...)
           ...
       11. rlang f(...)
    Code
      cat("\n1 frame:\n")
    Output
      
      1 frame:
    Code
      print(trace, simplify = "branch", max_frames = 1, srcrefs = FALSE)
    Output
        1. rlang f(...)
           ...

# eval() frames are collapsed

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-rlang f()
       2.   +-base::eval(...)
       3.   | \-base::eval(...)
       4.   \-rlang g()
       5.     +-base::eval(...)
       6.     | \-base::eval(...)
       7.     \-rlang::trace_back(...)
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-rlang f()
       2.   +-[ base::eval(...) ] with 1 more call
       4.   \-rlang g()
       5.     +-[ base::eval(...) ] with 1 more call
       7.     \-rlang::trace_back(...)
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. rlang f()
       4. rlang g()
       7. rlang::trace_back(...)

---

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-rlang f()
       2.   +-base::evalq(...)
       3.   | \-base::evalq(...)
       4.   \-rlang g()
       5.     +-base::evalq(...)
       6.     | \-base::evalq(...)
       7.     \-rlang::trace_back(...)
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-rlang f()
       2.   +-[ base::evalq(...) ] with 1 more call
       4.   \-rlang g()
       5.     +-[ base::evalq(...) ] with 1 more call
       7.     \-rlang::trace_back(...)
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. rlang f()
       4. rlang g()
       7. rlang::trace_back(...)

# %>% frames are collapsed

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-NULL %>% f() %>% g(1, 2) %>% h(3, ., 4)
       2. \-rlang h(...)
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-NULL %>% f() %>% g(1, 2) %>% h(3, ., 4)
       2. \-rlang h(...)
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. NULL %>% f() %>% g(1, 2) %>% h(3, ., 4)
       2. rlang h(...)

---

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-f(NULL) %>% g(list(.)) %>% h(3, ., list(.))
       2. \-rlang h(...)
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-f(NULL) %>% g(list(.)) %>% h(3, ., list(.))
       2. \-rlang h(...)
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. f(NULL) %>% g(list(.)) %>% h(3, ., list(.))
       2. rlang h(...)

---

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-rlang f(...)
       2. +-g(NULL %>% f()) %>% h()
       3. \-rlang h(...)
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-rlang f(...)
       2. +-g(NULL %>% f()) %>% h()
       3. \-rlang h(...)
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. rlang f(...)
       3. rlang h(...)

# children of collapsed %>% frames have correct parent

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-NA %>% F() %>% G() %>% H()
       2. \-rlang H(...)
       3.   \-rlang f()
       4.     \-rlang h()
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-NA %>% F() %>% G() %>% H()
       2. \-rlang H(...)
       3.   \-rlang f()
       4.     \-rlang h()
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. NA %>% F() %>% G() %>% H()
       2. rlang H(...)
       3. rlang f()
       4. rlang h()

# children of collapsed frames are rechained to correct parent

    Code
      cat("Full:\n")
    Output
      Full:
    Code
      print(trace, simplify = "none", srcrefs = FALSE)
    Output
          x
       1. \-rlang f()
       2.   \-base::eval(...)
       3.     \-base::eval(...)
       4.       \-rlang g()
    Code
      cat("\nCollapsed:\n")
    Output
      
      Collapsed:
    Code
      print(trace, simplify = "collapse", srcrefs = FALSE)
    Output
          x
       1. \-rlang f()
       2.   \-[ base::eval(...) ] with 1 more call
       4.     \-rlang g()
    Code
      cat("\nBranch:\n")
    Output
      
      Branch:
    Code
      print(trace, simplify = "branch", srcrefs = FALSE)
    Output
       1. rlang f()
       2. [ base::eval(...) ] with 1 more call
       4. rlang g()

# combinations of incomplete and leading pipes collapse properly

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-NA %>% F() %>% T() %>% F() %>% F()
       2. +-rlang F(...)
       3. +-rlang F(...)
       4. \-rlang T(...)
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-NA %>% F() %>% T() %>% F() %>% F()
       2. +-rlang F(...)
       3. +-rlang F(...)
       4. \-rlang T(...)
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. NA %>% F() %>% T() %>% F() %>% F()
       4. rlang T(...)

---

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-T(NA) %>% F()
       2. +-rlang F(...)
       3. \-rlang T(...)
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-T(NA) %>% F()
       2. +-rlang F(...)
       3. \-rlang T(...)
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. T(NA) %>% F()
       3. rlang T(...)

---

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-F(NA) %>% F() %>% T() %>% F() %>% F()
       2. +-rlang F(...)
       3. +-rlang F(...)
       4. \-rlang T(...)
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-F(NA) %>% F() %>% T() %>% F() %>% F()
       2. +-rlang F(...)
       3. +-rlang F(...)
       4. \-rlang T(...)
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. F(NA) %>% F() %>% T() %>% F() %>% F()
       4. rlang T(...)

---

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-NA %>% T()
       2. \-rlang T(...)
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-NA %>% T()
       2. \-rlang T(...)
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. NA %>% T()
       2. rlang T(...)

---

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-NA %>% F() %>% T()
       2. \-rlang T(...)
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-NA %>% F() %>% T()
       2. \-rlang T(...)
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. NA %>% F() %>% T()
       2. rlang T(...)

---

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-F(NA) %>% T()
       2. \-rlang T(...)
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-F(NA) %>% T()
       2. \-rlang T(...)
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. F(NA) %>% T()
       2. rlang T(...)

---

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-F(NA) %>% F() %>% T()
       2. \-rlang T(...)
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-F(NA) %>% F() %>% T()
       2. \-rlang T(...)
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. F(NA) %>% F() %>% T()
       2. rlang T(...)

# calls before and after pipe are preserved

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-rlang F(...)
       2. +-NA %>% T()
       3. \-rlang T(...)
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-rlang F(...)
       2. +-NA %>% T()
       3. \-rlang T(...)
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. rlang F(...)
       3. rlang T(...)

---

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-NA %>% C()
       2. \-rlang C(...)
       3.   \-rlang f()
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-NA %>% C()
       2. \-rlang C(...)
       3.   \-rlang f()
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. NA %>% C()
       2. rlang C(...)
       3. rlang f()

---

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-rlang F(...)
       2. +-NA %>% C()
       3. \-rlang C(...)
       4.   \-rlang f()
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-rlang F(...)
       2. +-NA %>% C()
       3. \-rlang C(...)
       4.   \-rlang f()
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. rlang F(...)
       3. rlang C(...)
       4. rlang f()

# always keep very first frame as part of backtrace branch

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-rlang gen()
       2. \-rlang gen.default()
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-rlang gen()
       2. \-rlang gen.default()
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. rlang gen()
       2. rlang gen.default()

# anonymous calls are stripped from backtraces

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-(function() {...
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-(function() {...
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
      

# collapsing of eval() frames detects when error occurs within eval()

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-base::eval()
       2. \-base::.handleSimpleError(...)
       3.   \-rlang h(...)
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-base::eval()
       2. \-base::.handleSimpleError(...)
       3.   \-rlang h(...)
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. base::eval()
       2. base::.handleSimpleError(...)
       3. rlang h(...)

# can print degenerate backtraces

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-foo
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-foo
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. foo

---

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-NULL
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-NULL
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. NULL

---

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-1L
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-1L
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. 1L

# check for dangling promise in call CAR (#492)

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-base::print(...)
       2. \-rlang print.foo(...)
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-base::print(...)
       2. \-rlang print.foo(...)
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. base::print(...)
       2. rlang print.foo(...)

# dangling srcrefs are not printed

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-rlang f(...)
       2.   \-rlang g(...) at fixtures/trace-srcref2.R:2:2
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-rlang f(...)
       2.   \-rlang g(...) at fixtures/trace-srcref2.R:2:2
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. rlang f(...)
       2. rlang g(...)
          at fixtures/trace-srcref2.R:2:2

# summary.rlang_trace() prints the full tree

    Code
      summary(trace, srcrefs = FALSE)
    Output
          x
       1. \-rlang f()
       2.   \-rlang g()
       3.     \-rlang h()

# global functions have `global::` prefix

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-rlang g(...)
       2.   \-global f(...)
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-rlang g(...)
       2.   \-global f(...)
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. rlang g(...)
       2. global f(...)

# local functions inheriting from global do not have `global::` prefix

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-rlang g(...)
       2.   \-f(...)
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-rlang g(...)
       2.   \-f(...)
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. rlang g(...)
       2. f(...)

# can trim layers of backtraces

    Code
      local_options(rlang_trace_format_srcrefs = FALSE)
      cat_line("No trimming:")
    Output
      No trimming:
    Code
      summary(trace0)
    Output
           x
        1. \-rlang f(...) at test-trace.R:456:2
        2.   +-base::identity(...) at test-trace.R:452:7
        3.   +-base::identity(...)
        4.   \-rlang g(...)
        5.     +-base::identity(...) at test-trace.R:453:7
        6.     +-base::identity(...)
        7.     \-rlang h(...)
        8.       +-base::identity(...) at test-trace.R:454:7
        9.       +-base::identity(...)
       10.       \-rlang::trace_back(...)
    Code
      cat_line("", "", "One layer (the default):")
    Output
      
      
      One layer (the default):
    Code
      summary(trace1)
    Output
          x
       1. \-rlang f(...) at test-trace.R:457:2
       2.   +-base::identity(...) at test-trace.R:452:7
       3.   +-base::identity(...)
       4.   \-rlang g(...)
       5.     +-base::identity(...) at test-trace.R:453:7
       6.     +-base::identity(...)
       7.     \-rlang h(...)
    Code
      cat_line("", "", "Two layers:")
    Output
      
      
      Two layers:
    Code
      summary(trace2)
    Output
          x
       1. \-rlang f(...) at test-trace.R:458:2
       2.   +-base::identity(...) at test-trace.R:452:7
       3.   +-base::identity(...)
       4.   \-rlang g(...)
    Code
      cat_line("", "", "Three layers:")
    Output
      
      
      Three layers:
    Code
      summary(trace3)
    Output
          x
       1. \-rlang f(...) at test-trace.R:459:2

# caught error does not display backtrace in knitted files

    Code
      cat_line(render_md("test-trace-full.Rmd"))
    Output
          library(rlang)
      
          f <- function() g()
          g <- function() h()
          h <- function() rlang::abort("foo")
      
          f()
      
          ## Error: foo
      
      Currently needs to be in a different chunk:
      
          last_error()
      
          ## <error/rlang_error>
          ## Error in `h()`: foo
          ## Backtrace:
          ##  1. global f()
          ##  2. global g()
          ##  3. global h()
          ## Run `rlang::last_trace()` to see the full context.
      
          last_trace()
      
          ## <error/rlang_error>
          ## Error in `h()`: foo
          ## Backtrace:
          ##     x
          ##  1. \-global f()
          ##  2.   \-global g()
          ##  3.     \-global h()
      
          options(rlang_backtrace_on_error_report = "reminder")
          f()
      
          ## Error: foo
          ## Run `rlang::last_error()` to see where the error occurred.
      
          options(rlang_backtrace_on_error_report = "full")
          f()
      
          ## Error: foo
          ## Backtrace:
          ##     x
          ##  1. \-global f()
          ##  2.   \-global g()
          ##  3.     \-global h()

# backtraces don't contain inlined objects (#1069, r-lib/testthat#1223)

    Code
      summary(trace)
    Output
          x
       1. +-rlang::inject(...)
       2. \-rlang f(...)
       3.   +-base::do.call(...)
       4.   \-rlang g(...)
       5.     \-rlang h()

