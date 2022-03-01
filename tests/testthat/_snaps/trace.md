# tree printing only changes deliberately

    Code
      print(trace, dir = dir)
    Output
          x
       1. \-rlang i() at test-trace.R:25:2
       2.   \-rlang j(i) at test-trace.R:18:7
       3.     \-rlang k(i) at test-trace.R:19:21
       4.       \-rlang l(i) at test-trace.R:22:4
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
        3.     +-base::tryCatch(h(), foo = identity, bar = identity) at test-trace.R:50:20
        4.     | \-base tryCatchList(expr, classes, parentenv, handlers)
        5.     |   +-base tryCatchOne(...)
        6.     |   | \-base doTryCatch(return(expr), name, parentenv, handler)
        7.     |   \-base tryCatchList(expr, names[-nh], parentenv, handlers[-nh])
        8.     |     \-base tryCatchOne(expr, names, parentenv, handlers[[1L]])
        9.     |       \-base doTryCatch(return(expr), name, parentenv, handler)
       10.     \-rlang h()
       11.       +-base::tryCatch(i(), baz = identity) at test-trace.R:51:20
       12.       | \-base tryCatchList(expr, classes, parentenv, handlers)
       13.       |   \-base tryCatchOne(expr, names, parentenv, handlers[[1L]])
       14.       |     \-base doTryCatch(return(expr), name, parentenv, handler)
       15.       \-rlang i()
       16.         +-base::tryCatch(trace_back(e, bottom = 0)) at test-trace.R:52:20
       17.         | \-base tryCatchList(expr, classes, parentenv, handlers)
       18.         \-rlang::trace_back(e, bottom = 0)
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
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
        1. rlang f()
        2. rlang g()
             at test-trace.R:49:20
       10. rlang h()
       15. rlang i()

---

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
           x
        1. \-rlang f()
        2.   +-base::eval(quote(eval(quote(g())))) at test-trace.R:61:7
        3.   | \-base::eval(quote(eval(quote(g()))))
        4.   +-base::eval(quote(g()))
        5.   | \-base::eval(quote(g()))
        6.   \-rlang g()
        7.     +-base::tryCatch(eval(quote(h())), foo = identity, bar = identity) at test-trace.R:62:7
        8.     | \-base tryCatchList(expr, classes, parentenv, handlers)
        9.     |   +-base tryCatchOne(...)
       10.     |   | \-base doTryCatch(return(expr), name, parentenv, handler)
       11.     |   \-base tryCatchList(expr, names[-nh], parentenv, handlers[-nh])
       12.     |     \-base tryCatchOne(expr, names, parentenv, handlers[[1L]])
       13.     |       \-base doTryCatch(return(expr), name, parentenv, handler)
       14.     +-base::eval(quote(h()))
       15.     | \-base::eval(quote(h()))
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
       2.   +-rlang::eval_bare(quote(g()))
       3.   \-rlang g()
    Code
      print(trace, simplify = "collapse", srcrefs = FALSE)
    Output
          x
       1. \-rlang f()
       2.   +-rlang::eval_bare(quote(g()))
       3.   \-rlang g()

# recursive frames are rewired to the global env

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-rlang::eval_tidy(quo(f()))
       2. \-rlang f()
       3.   \-rlang g()
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-rlang::eval_tidy(quo(f()))
       2. \-rlang f()
       3.   \-rlang g()
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. rlang::eval_tidy(quo(f()))
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
        1. rlang f(10)
        2. rlang f(n - 1)
        3. rlang f(n - 1)
        4. rlang f(n - 1)
        5. rlang f(n - 1)
        6. rlang f(n - 1)
        7. rlang f(n - 1)
        8. rlang f(n - 1)
        9. rlang f(n - 1)
       10. rlang f(n - 1)
       11. rlang f(n - 1)
    Code
      cat("\n5 frames:\n")
    Output
      
      5 frames:
    Code
      print(trace, simplify = "branch", max_frames = 5, srcrefs = FALSE)
    Output
        1. rlang f(10)
        2. rlang f(n - 1)
        3. rlang f(n - 1)
           ...
       10. rlang f(n - 1)
       11. rlang f(n - 1)
    Code
      cat("\n2 frames:\n")
    Output
      
      2 frames:
    Code
      print(trace, simplify = "branch", max_frames = 2, srcrefs = FALSE)
    Output
        1. rlang f(10)
           ...
       11. rlang f(n - 1)
    Code
      cat("\n1 frame:\n")
    Output
      
      1 frame:
    Code
      print(trace, simplify = "branch", max_frames = 1, srcrefs = FALSE)
    Output
        1. rlang f(10)
           ...

# eval() frames are collapsed

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-rlang f()
       2.   +-base::eval(quote(g()))
       3.   | \-base::eval(quote(g()))
       4.   \-rlang g()
       5.     +-base::eval(quote(trace_back(e, bottom = 0)))
       6.     | \-base::eval(quote(trace_back(e, bottom = 0)))
       7.     \-rlang::trace_back(e, bottom = 0)
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-rlang f()
       2.   +-[ base::eval(...) ] with 1 more call
       4.   \-rlang g()
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. rlang f()
       4. rlang g()

---

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-rlang f()
       2.   +-base::evalq(g())
       3.   | \-base::evalq(g())
       4.   \-rlang g()
       5.     +-base::evalq(trace_back(e, bottom = 0))
       6.     | \-base::evalq(trace_back(e, bottom = 0))
       7.     \-rlang::trace_back(e, bottom = 0)
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-rlang f()
       2.   +-[ base::evalq(...) ] with 1 more call
       4.   \-rlang g()
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. rlang f()
       4. rlang g()

# %>% frames are collapsed

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-NULL %>% f() %>% g(1, 2) %>% h(3, ., 4)
       2. \-rlang h(3, ., 4)
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-NULL %>% f() %>% g(1, 2) %>% h(3, ., 4)
       2. \-rlang h(3, ., 4)
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. NULL %>% f() %>% g(1, 2) %>% h(3, ., 4)
       2. rlang h(3, ., 4)

---

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-f(NULL) %>% g(list(.)) %>% h(3, ., list(.))
       2. \-rlang h(3, ., list(.))
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-f(NULL) %>% g(list(.)) %>% h(3, ., list(.))
       2. \-rlang h(3, ., list(.))
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. f(NULL) %>% g(list(.)) %>% h(3, ., list(.))
       2. rlang h(3, ., list(.))

---

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-rlang f(g(NULL %>% f()) %>% h())
       2. +-g(NULL %>% f()) %>% h()
       3. \-rlang h(.)
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-rlang f(g(NULL %>% f()) %>% h())
       2. +-g(NULL %>% f()) %>% h()
       3. \-rlang h(.)
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. rlang f(g(NULL %>% f()) %>% h())
       3. rlang h(.)

# children of collapsed %>% frames have correct parent

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-NA %>% F() %>% G() %>% H()
       2. \-rlang H(.)
       3.   \-rlang f()
       4.     \-rlang h()
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-NA %>% F() %>% G() %>% H()
       2. \-rlang H(.)
       3.   \-rlang f()
       4.     \-rlang h()
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. NA %>% F() %>% G() %>% H()
       2. rlang H(.)
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
       2.   \-base::eval(quote(g()), env())
       3.     \-base::eval(quote(g()), env())
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
       2. +-rlang F(.)
       3. +-rlang F(.)
       4. \-rlang T(.)
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-NA %>% F() %>% T() %>% F() %>% F()
       2. +-rlang F(.)
       3. +-rlang F(.)
       4. \-rlang T(.)
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. NA %>% F() %>% T() %>% F() %>% F()
       4. rlang T(.)

---

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-T(NA) %>% F()
       2. +-rlang F(.)
       3. \-rlang T(NA)
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-T(NA) %>% F()
       2. +-rlang F(.)
       3. \-rlang T(NA)
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. T(NA) %>% F()
       3. rlang T(NA)

---

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-F(NA) %>% F() %>% T() %>% F() %>% F()
       2. +-rlang F(.)
       3. +-rlang F(.)
       4. \-rlang T(.)
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-F(NA) %>% F() %>% T() %>% F() %>% F()
       2. +-rlang F(.)
       3. +-rlang F(.)
       4. \-rlang T(.)
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. F(NA) %>% F() %>% T() %>% F() %>% F()
       4. rlang T(.)

---

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-NA %>% T()
       2. \-rlang T(.)
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-NA %>% T()
       2. \-rlang T(.)
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. NA %>% T()
       2. rlang T(.)

---

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-NA %>% F() %>% T()
       2. \-rlang T(.)
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-NA %>% F() %>% T()
       2. \-rlang T(.)
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. NA %>% F() %>% T()
       2. rlang T(.)

---

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-F(NA) %>% T()
       2. \-rlang T(.)
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-F(NA) %>% T()
       2. \-rlang T(.)
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. F(NA) %>% T()
       2. rlang T(.)

---

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-F(NA) %>% F() %>% T()
       2. \-rlang T(.)
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-F(NA) %>% F() %>% T()
       2. \-rlang T(.)
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. F(NA) %>% F() %>% T()
       2. rlang T(.)

# calls before and after pipe are preserved

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-rlang F(NA %>% T())
       2. +-NA %>% T()
       3. \-rlang T(.)
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-rlang F(NA %>% T())
       2. +-NA %>% T()
       3. \-rlang T(.)
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. rlang F(NA %>% T())
       3. rlang T(.)

---

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-NA %>% C()
       2. \-rlang C(.)
       3.   \-rlang f()
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-NA %>% C()
       2. \-rlang C(.)
       3.   \-rlang f()
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. NA %>% C()
       2. rlang C(.)
       3. rlang f()

---

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-rlang F(NA %>% C())
       2. +-NA %>% C()
       3. \-rlang C(.)
       4.   \-rlang f()
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-rlang F(NA %>% C())
       2. +-NA %>% C()
       3. \-rlang C(.)
       4.   \-rlang f()
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. rlang F(NA %>% C())
       3. rlang C(.)
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
       3.   \-rlang h(simpleError(msg, call))
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-base::eval()
       2. \-base::.handleSimpleError(...)
       3.   \-rlang h(simpleError(msg, call))
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. base::eval()
       2. base::.handleSimpleError(...)
       3. rlang h(simpleError(msg, call))

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
       1. +-base::print(foo)
       2. \-rlang print.foo(foo)
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-base::print(foo)
       2. \-rlang print.foo(foo)
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. base::print(foo)
       2. rlang print.foo(foo)

# dangling srcrefs are not printed

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-rlang f(current_env())
       2.   \-rlang g(e) at fixtures/trace-srcref2.R:2:2
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-rlang f(current_env())
       2.   \-rlang g(e) at fixtures/trace-srcref2.R:2:2
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. rlang f(current_env())
       2. rlang g(e)
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
       1. \-rlang g(current_env())
       2.   \-global f(e)
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-rlang g(current_env())
       2.   \-global f(e)
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. rlang g(current_env())
       2. global f(e)

# local functions inheriting from global do not have `global::` prefix

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-rlang g(current_env())
       2.   \-f(e)
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-rlang g(current_env())
       2.   \-f(e)
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. rlang g(current_env())
       2. f(e)

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
        1. \-rlang f(0) at test-trace.R:456:2
        2.   +-base::identity(identity(g(n))) at test-trace.R:452:7
        3.   +-base::identity(g(n))
        4.   \-rlang g(n)
        5.     +-base::identity(identity(h(n))) at test-trace.R:453:7
        6.     +-base::identity(h(n))
        7.     \-rlang h(n)
        8.       +-base::identity(identity(trace_back(e, bottom = n))) at test-trace.R:454:7
        9.       +-base::identity(trace_back(e, bottom = n))
       10.       \-rlang::trace_back(e, bottom = n)
    Code
      cat_line("", "", "One layer (the default):")
    Output
      
      
      One layer (the default):
    Code
      summary(trace1)
    Output
          x
       1. \-rlang f(1) at test-trace.R:457:2
       2.   +-base::identity(identity(g(n))) at test-trace.R:452:7
       3.   +-base::identity(g(n))
       4.   \-rlang g(n)
       5.     +-base::identity(identity(h(n))) at test-trace.R:453:7
       6.     +-base::identity(h(n))
       7.     \-rlang h(n)
    Code
      cat_line("", "", "Two layers:")
    Output
      
      
      Two layers:
    Code
      summary(trace2)
    Output
          x
       1. \-rlang f(2) at test-trace.R:458:2
       2.   +-base::identity(identity(g(n))) at test-trace.R:452:7
       3.   +-base::identity(g(n))
       4.   \-rlang g(n)
    Code
      cat_line("", "", "Three layers:")
    Output
      
      
      Three layers:
    Code
      summary(trace3)
    Output
          x
       1. \-rlang f(3) at test-trace.R:459:2

# caught error does not display backtrace in knitted files

    Code
      cat_line(render_md("test-trace-full.Rmd"))
    Output
          library(rlang)
      
          f <- function() g()
          g <- function() h()
          h <- function() rlang::abort("foo")
      
          f()
      
          ## Error in `h()`:
          ## ! foo
      
      Currently needs to be in a different chunk:
      
          last_error()
      
          ## <error/rlang_error>
          ## Error in `h()`:
          ## ! foo
          ## ---
          ## Backtrace:
          ##  1. global f()
          ##  2. global g()
          ##  3. global h()
          ## Run `rlang::last_trace()` to see the full context.
      
          last_trace()
      
          ## <error/rlang_error>
          ## Error in `h()`:
          ## ! foo
          ## ---
          ## Backtrace:
          ##     x
          ##  1. \-global f()
          ##  2.   \-global g()
          ##  3.     \-global h()
          ##  4.       \-rlang::abort("foo")
      
          options(rlang_backtrace_on_error_report = "reminder")
          f()
      
          ## Error in `h()`:
          ## ! foo
      
          ## Run `rlang::last_error()` to see where the error occurred.
      
          options(rlang_backtrace_on_error_report = "full")
          f()
      
          ## Error in `h()`:
          ## ! foo
      
          ## Backtrace:
          ##     x
          ##  1. \-global f()
          ##  2.   \-global g()
          ##  3.     \-global h()
          ##  4.       \-rlang::abort("foo")

# backtraces don't contain inlined objects (#1069, r-lib/testthat#1223)

    Code
      summary(trace)
    Output
          x
       1. +-rlang::inject(f(!!list()))
       2. \-rlang f(`<list>`)
       3.   +-base::do.call("g", list(runif(1e+06) + 0))
       4.   \-rlang g(`<dbl>`)
       5.     \-rlang h()

# runs of namespaces are embolden (#946)

    Code
      print(err)
    Output
      [1m[1m[1m[34m<error/rlang_error>[39m[22m
      [1m[33mError[39m in [1m[1m`1 + ""`:[22m
      [33m![39m non-numeric argument to binary operator
      ---
      [1mBacktrace:[22m
      [90m  1. [39m[1mrlang[22m::catch_cnd(withCallingHandlers(f(), error = entrace), "error")
      [90m  9. [39mrlang f()
      [90m 10. [39mrlang g()
      [90m 11. [39mrlang h()
      [90m 12. [39m[1mbase[22m::identity(1 + "")
    Code
      summary(err)
    Output
      [1m[1m[1m[34m<error/rlang_error>[39m[22m
      [1m[33mError[39m in [1m[1m`1 + ""`:[22m
      [33m![39m non-numeric argument to binary operator
      ---
      [1mBacktrace:[22m
      [90m     [39mx
      [90m  1. [39m+-[1mrlang[22m::catch_cnd(withCallingHandlers(f(), error = entrace), "error")
      [90m  2. [39m| +-rlang::eval_bare(...)
      [90m  3. [39m| +-[1mbase[22m::tryCatch(...)
      [90m  4. [39m| | \-base tryCatchList(expr, classes, parentenv, handlers)
      [90m  5. [39m| |   \-base tryCatchOne(expr, names, parentenv, handlers[[1L]])
      [90m  6. [39m| |     \-base doTryCatch(return(expr), name, parentenv, handler)
      [90m  7. [39m| \-base::force(expr)
      [90m  8. [39m+-base::withCallingHandlers(f(), error = entrace)
      [90m  9. [39m\-[1mrlang[22m f()
      [90m 10. [39m  \-rlang g()
      [90m 11. [39m    \-rlang h()
      [90m 12. [39m      \-[1mbase[22m::identity(1 + "")

# `bottom` must be a positive integer

    Code
      (expect_error(trace_back(bottom = -1)))
    Output
      <error/rlang_error>
      Error in `trace_back()`:
      ! `bottom` must be a positive integer.

# collapsed case in branch formatting

     1. f()
     2. g()
     3. h()
     4. [ evalq() ] with 1 more call

# trailing `FALSE` visibility is handled

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-f()
       2.   \-g()
       3.     \-h()
       4.       \-foo()
       5.         \-bar()
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-f()
       2.   \-g()
       3.     \-h()
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. f()
       2. g()
       3. h()

# can format empty traces

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
      x
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
      x
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
      

