# tree printing only changes deliberately

    Code
      print(trace, dir = dir)
    Output
          x
       1. \-rlang:::i() test-trace.R:26:2
       2.   \-rlang:::j(i) test-trace.R:19:7
       3.     \-rlang:::k(i) test-trace.R:20:21
       4.       \-rlang:::l(i) test-trace.R:23:4
    Code
      cat("\n")
    Output
      
    Code
      print(trace_subset(trace, 0L), dir = dir)
    Output
      x

# can print tree with collapsed branches

    Code
      # Full
    Code
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
           x
        1. \-rlang:::f()
        2.   \-rlang:::g() test-trace.R:51:20
        3.     +-base::tryCatch(h(), foo = identity, bar = identity) test-trace.R:52:20
        4.     | \-base:::tryCatchList(expr, classes, parentenv, handlers)
        5.     |   +-base:::tryCatchOne(...)
        6.     |   | \-base:::doTryCatch(return(expr), name, parentenv, handler)
        7.     |   \-base:::tryCatchList(expr, names[-nh], parentenv, handlers[-nh])
        8.     |     \-base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
        9.     |       \-base:::doTryCatch(return(expr), name, parentenv, handler)
       10.     \-rlang:::h()
       11.       +-base::tryCatch(i(), baz = identity) test-trace.R:53:20
       12.       | \-base:::tryCatchList(expr, classes, parentenv, handlers)
       13.       |   \-base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
       14.       |     \-base:::doTryCatch(return(expr), name, parentenv, handler)
       15.       \-rlang:::i()
       16.         +-base::tryCatch(trace_back(e, bottom = 0)) test-trace.R:54:20
       17.         | \-base:::tryCatchList(expr, classes, parentenv, handlers)
       18.         \-rlang::trace_back(e, bottom = 0)
    Code
      # Collapsed
    Code
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
           x
        1. \-rlang:::f()
        2.   \-rlang:::g() test-trace.R:51:20
        3.     +-[ base::tryCatch(...) ] with 6 more calls test-trace.R:52:20
       10.     \-rlang:::h()
       11.       +-[ base::tryCatch(...) ] with 3 more calls test-trace.R:53:20
       15.       \-rlang:::i()
       16.         +-[ base::tryCatch(...) ] with 1 more call test-trace.R:54:20
       18.         \-rlang::trace_back(e, bottom = 0)
    Code
      # Branch
    Code
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
        1. rlang:::f()
        2. rlang:::g() test-trace.R:51:20
       10. rlang:::h()
       15. rlang:::i()
       18. rlang::trace_back(e, bottom = 0)

---

    Code
      # Full
    Code
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
           x
        1. \-rlang:::f()
        2.   +-base::eval(quote(eval(quote(g())))) test-trace.R:64:7
        3.   | \-base::eval(quote(eval(quote(g()))))
        4.   +-base::eval(quote(g()))
        5.   | \-base::eval(quote(g()))
        6.   \-rlang:::g()
        7.     +-base::tryCatch(eval(quote(h())), foo = identity, bar = identity) test-trace.R:65:7
        8.     | \-base:::tryCatchList(expr, classes, parentenv, handlers)
        9.     |   +-base:::tryCatchOne(...)
       10.     |   | \-base:::doTryCatch(return(expr), name, parentenv, handler)
       11.     |   \-base:::tryCatchList(expr, names[-nh], parentenv, handlers[-nh])
       12.     |     \-base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
       13.     |       \-base:::doTryCatch(return(expr), name, parentenv, handler)
       14.     +-base::eval(quote(h()))
       15.     | \-base::eval(quote(h()))
       16.     \-rlang:::h()
    Code
      # Collapsed
    Code
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
           x
        1. \-rlang:::f()
        2.   +-[ base::eval(...) ] with 1 more call test-trace.R:64:7
        4.   +-[ base::eval(...) ] with 1 more call
        6.   \-rlang:::g()
        7.     +-[ base::tryCatch(...) ] with 6 more calls test-trace.R:65:7
       14.     +-[ base::eval(...) ] with 1 more call
       16.     \-rlang:::h()
    Code
      # Branch
    Code
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
        1. rlang:::f()
        6. rlang:::g()
       16. rlang:::h()

# recursive frames are rewired to the global env

    Code
      # Full
    Code
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-rlang::eval_tidy(quo(f()))
       2. \-rlang:::f()
       3.   \-rlang:::g()
    Code
      # Collapsed
    Code
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-[ rlang::eval_tidy(...) ]
       2. \-rlang:::f()
       3.   \-rlang:::g()
    Code
      # Branch
    Code
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. rlang::eval_tidy(quo(f()))
       2. rlang:::f()
       3. rlang:::g()

# long backtrace branches are truncated

    Code
      cat("Full:\n")
    Output
      Full:
    Code
      print(trace, simplify = "branch", srcrefs = FALSE)
    Output
        1. rlang:::f(10)
        2. rlang:::f(n - 1)
        3. rlang:::f(n - 1)
        4. rlang:::f(n - 1)
        5. rlang:::f(n - 1)
        6. rlang:::f(n - 1)
        7. rlang:::f(n - 1)
        8. rlang:::f(n - 1)
        9. rlang:::f(n - 1)
       10. rlang:::f(n - 1)
       11. rlang:::f(n - 1)
    Code
      cat("\n5 frames:\n")
    Output
      
      5 frames:
    Code
      print(trace, simplify = "branch", max_frames = 5, srcrefs = FALSE)
    Output
        1. rlang:::f(10)
        2. rlang:::f(n - 1)
        3. rlang:::f(n - 1)
           ...
       10. rlang:::f(n - 1)
       11. rlang:::f(n - 1)
    Code
      cat("\n2 frames:\n")
    Output
      
      2 frames:
    Code
      print(trace, simplify = "branch", max_frames = 2, srcrefs = FALSE)
    Output
        1. rlang:::f(10)
           ...
       11. rlang:::f(n - 1)
    Code
      cat("\n1 frame:\n")
    Output
      
      1 frame:
    Code
      print(trace, simplify = "branch", max_frames = 1, srcrefs = FALSE)
    Output
        1. rlang:::f(10)
           ...

# eval() frames are collapsed

    Code
      # Full
    Code
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-rlang:::f()
       2.   +-base::eval(quote(g()))
       3.   | \-base::eval(quote(g()))
       4.   \-rlang:::g()
       5.     +-base::eval(quote(trace_back(e, bottom = 0)))
       6.     | \-base::eval(quote(trace_back(e, bottom = 0)))
       7.     \-rlang::trace_back(e, bottom = 0)
    Code
      # Collapsed
    Code
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-rlang:::f()
       2.   +-[ base::eval(...) ] with 1 more call
       4.   \-rlang:::g()
       5.     +-[ base::eval(...) ] with 1 more call
       7.     \-rlang::trace_back(e, bottom = 0)
    Code
      # Branch
    Code
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. rlang:::f()
       4. rlang:::g()
       7. rlang::trace_back(e, bottom = 0)

---

    Code
      # Full
    Code
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-rlang:::f()
       2.   +-base::evalq(g())
       3.   | \-base::evalq(g())
       4.   \-rlang:::g()
       5.     +-base::evalq(trace_back(e, bottom = 0))
       6.     | \-base::evalq(trace_back(e, bottom = 0))
       7.     \-rlang::trace_back(e, bottom = 0)
    Code
      # Collapsed
    Code
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-rlang:::f()
       2.   +-[ base::evalq(...) ] with 1 more call
       4.   \-rlang:::g()
       5.     +-[ base::evalq(...) ] with 1 more call
       7.     \-rlang::trace_back(e, bottom = 0)
    Code
      # Branch
    Code
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. rlang:::f()
       4. rlang:::g()
       7. rlang::trace_back(e, bottom = 0)

# %>% frames are collapsed

    Code
      # Full
    Code
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-NULL %>% f() %>% g(1, 2) %>% h(3, ., 4)
       2. \-rlang:::h(3, ., 4)
    Code
      # Collapsed
    Code
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-[ NULL %>% f() %>% g(1, 2) %>% h(3, ., 4) ]
       2. \-rlang:::h(3, ., 4)
    Code
      # Branch
    Code
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. NULL %>% f() %>% g(1, 2) %>% h(3, ., 4)
       2. rlang:::h(3, ., 4)

---

    Code
      # Full
    Code
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-f(NULL) %>% g(list(.)) %>% h(3, ., list(.))
       2. \-rlang:::h(3, ., list(.))
    Code
      # Collapsed
    Code
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-[ f(NULL) %>% g(list(.)) %>% h(3, ., list(.)) ]
       2. \-rlang:::h(3, ., list(.))
    Code
      # Branch
    Code
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. f(NULL) %>% g(list(.)) %>% h(3, ., list(.))
       2. rlang:::h(3, ., list(.))

---

    Code
      # Full
    Code
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-rlang:::f(g(NULL %>% f()) %>% h())
       2. +-g(NULL %>% f()) %>% h()
       3. \-rlang:::h(.)
    Code
      # Collapsed
    Code
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-[ rlang:::f(...) ]
       2. +-[ g(NULL %>% f()) %>% h() ]
       3. \-rlang:::h(.)
    Code
      # Branch
    Code
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. rlang:::f(g(NULL %>% f()) %>% h())
       3. rlang:::h(.)

# children of collapsed %>% frames have correct parent

    Code
      # Full
    Code
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-NA %>% F() %>% G() %>% H()
       2. \-rlang:::H(.)
       3.   \-rlang:::f()
       4.     \-rlang:::h()
    Code
      # Collapsed
    Code
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-[ NA %>% F() %>% G() %>% H() ]
       2. \-rlang:::H(.)
       3.   \-rlang:::f()
       4.     \-rlang:::h()
    Code
      # Branch
    Code
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. NA %>% F() %>% G() %>% H()
       2. rlang:::H(.)
       3. rlang:::f()
       4. rlang:::h()

# children of collapsed frames are rechained to correct parent

    Code
      cat("Full:\n")
    Output
      Full:
    Code
      print(trace, simplify = "none", srcrefs = FALSE)
    Output
          x
       1. \-rlang:::f()
       2.   \-base::eval(quote(g()), env())
       3.     \-base::eval(quote(g()), env())
       4.       \-rlang:::g()
    Code
      cat("\nCollapsed:\n")
    Output
      
      Collapsed:
    Code
      print(trace, simplify = "collapse", srcrefs = FALSE)
    Output
          x
       1. \-rlang:::f()
       2.   \-[ base::eval(...) ] with 1 more call
       4.     \-rlang:::g()
    Code
      cat("\nBranch:\n")
    Output
      
      Branch:
    Code
      print(trace, simplify = "branch", srcrefs = FALSE)
    Output
       1. rlang:::f()
       2. [ base::eval(...) ] with 1 more call
       4. rlang:::g()

# combinations of incomplete and leading pipes collapse properly

    Code
      # Full
    Code
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-NA %>% F() %>% T() %>% F() %>% F()
       2. +-rlang:::F(.)
       3. +-rlang:::F(.)
       4. \-rlang:::T(.)
    Code
      # Collapsed
    Code
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-[ NA %>% F() %>% T() %>% F() %>% F() ]
       2. +-[ rlang:::F(...) ]
       3. +-[ rlang:::F(...) ]
       4. \-rlang:::T(.)
    Code
      # Branch
    Code
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. NA %>% F() %>% T() %>% F() %>% F()
       4. rlang:::T(.)

---

    Code
      # Full
    Code
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-T(NA) %>% F()
       2. +-rlang:::F(.)
       3. \-rlang:::T(NA)
    Code
      # Collapsed
    Code
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-[ T(NA) %>% F() ]
       2. +-[ rlang:::F(...) ]
       3. \-rlang:::T(NA)
    Code
      # Branch
    Code
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. T(NA) %>% F()
       3. rlang:::T(NA)

---

    Code
      # Full
    Code
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-F(NA) %>% F() %>% T() %>% F() %>% F()
       2. +-rlang:::F(.)
       3. +-rlang:::F(.)
       4. \-rlang:::T(.)
    Code
      # Collapsed
    Code
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-[ F(NA) %>% F() %>% T() %>% F() %>% F() ]
       2. +-[ rlang:::F(...) ]
       3. +-[ rlang:::F(...) ]
       4. \-rlang:::T(.)
    Code
      # Branch
    Code
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. F(NA) %>% F() %>% T() %>% F() %>% F()
       4. rlang:::T(.)

---

    Code
      # Full
    Code
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-NA %>% T()
       2. \-rlang:::T(.)
    Code
      # Collapsed
    Code
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-[ NA %>% T() ]
       2. \-rlang:::T(.)
    Code
      # Branch
    Code
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. NA %>% T()
       2. rlang:::T(.)

---

    Code
      # Full
    Code
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-NA %>% F() %>% T()
       2. \-rlang:::T(.)
    Code
      # Collapsed
    Code
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-[ NA %>% F() %>% T() ]
       2. \-rlang:::T(.)
    Code
      # Branch
    Code
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. NA %>% F() %>% T()
       2. rlang:::T(.)

---

    Code
      # Full
    Code
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-F(NA) %>% T()
       2. \-rlang:::T(.)
    Code
      # Collapsed
    Code
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-[ F(NA) %>% T() ]
       2. \-rlang:::T(.)
    Code
      # Branch
    Code
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. F(NA) %>% T()
       2. rlang:::T(.)

---

    Code
      # Full
    Code
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-F(NA) %>% F() %>% T()
       2. \-rlang:::T(.)
    Code
      # Collapsed
    Code
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-[ F(NA) %>% F() %>% T() ]
       2. \-rlang:::T(.)
    Code
      # Branch
    Code
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. F(NA) %>% F() %>% T()
       2. rlang:::T(.)

# calls before and after pipe are preserved

    Code
      # Full
    Code
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-rlang:::F(NA %>% T())
       2. +-NA %>% T()
       3. \-rlang:::T(.)
    Code
      # Collapsed
    Code
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-[ rlang:::F(...) ]
       2. +-[ NA %>% T() ]
       3. \-rlang:::T(.)
    Code
      # Branch
    Code
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. rlang:::F(NA %>% T())
       3. rlang:::T(.)

---

    Code
      # Full
    Code
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-NA %>% C()
       2. \-rlang:::C(.)
       3.   \-rlang:::f()
    Code
      # Collapsed
    Code
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-[ NA %>% C() ]
       2. \-rlang:::C(.)
       3.   \-rlang:::f()
    Code
      # Branch
    Code
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. NA %>% C()
       2. rlang:::C(.)
       3. rlang:::f()

---

    Code
      # Full
    Code
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-rlang:::F(NA %>% C())
       2. +-NA %>% C()
       3. \-rlang:::C(.)
       4.   \-rlang:::f()
    Code
      # Collapsed
    Code
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-[ rlang:::F(...) ]
       2. +-[ NA %>% C() ]
       3. \-rlang:::C(.)
       4.   \-rlang:::f()
    Code
      # Branch
    Code
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. rlang:::F(NA %>% C())
       3. rlang:::C(.)
       4. rlang:::f()

# always keep very first frame as part of backtrace branch

    Code
      # Full
    Code
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-rlang:::gen()
       2. \-rlang:::gen.default()
    Code
      # Collapsed
    Code
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-[ rlang:::gen() ]
       2. \-rlang:::gen.default()
    Code
      # Branch
    Code
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. rlang:::gen()
       2. rlang:::gen.default()

# anonymous calls are stripped from backtraces

    Code
      # Full
    Code
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-(function() {...
    Code
      # Collapsed
    Code
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-(function() {...
    Code
      # Branch
    Code
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
      

# collapsing of eval() frames detects when error occurs within eval()

    Code
      # Full
    Code
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-base::eval()
       2. \-base::.handleSimpleError(...)
       3.   \-rlang:::h(simpleError(msg, call))
    Code
      # Collapsed
    Code
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-[ base::eval() ]
       2. \-base::.handleSimpleError(...)
       3.   \-rlang:::h(simpleError(msg, call))
    Code
      # Branch
    Code
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. base::eval()
       2. base::.handleSimpleError(...)
       3. rlang:::h(simpleError(msg, call))

# can print degenerate backtraces

    Code
      # Full
    Code
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-foo
    Code
      # Collapsed
    Code
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-foo
    Code
      # Branch
    Code
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. foo

---

    Code
      # Full
    Code
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-NULL
    Code
      # Collapsed
    Code
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-NULL
    Code
      # Branch
    Code
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. NULL

---

    Code
      # Full
    Code
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-1L
    Code
      # Collapsed
    Code
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-1L
    Code
      # Branch
    Code
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. 1L

# check for dangling promise in call CAR (#492)

    Code
      # Full
    Code
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-base::print(foo)
       2. \-rlang:::print.foo(foo)
    Code
      # Collapsed
    Code
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-[ base::print(...) ]
       2. \-rlang:::print.foo(foo)
    Code
      # Branch
    Code
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. base::print(foo)
       2. rlang:::print.foo(foo)

# dangling srcrefs are not printed

    Code
      # Full
    Code
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-rlang:::f(current_env())
       2.   \-rlang:::g(e)
    Code
      # Collapsed
    Code
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-rlang:::f(current_env())
       2.   \-rlang:::g(e)
    Code
      # Branch
    Code
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. rlang:::f(current_env())
       2. rlang:::g(e)

# summary.rlang_trace() prints the full tree

    Code
      summary(trace, srcrefs = FALSE)
    Output
          x
       1. \-rlang:::f()
       2.   \-rlang:::g()
       3.     \-rlang:::h()

# unexported functions have `:::` prefix

    Code
      # Full
    Code
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-rlang:::f()
       2.   \-rlanglibtest:::test_trace_unexported_child(e)
       3.     \-rlanglibtest:::test_trace_unexported(e)
    Code
      # Collapsed
    Code
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-rlang:::f()
       2.   \-rlanglibtest:::test_trace_unexported_child(e)
       3.     \-rlanglibtest:::test_trace_unexported(e)
    Code
      # Branch
    Code
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. rlang:::f()
       2. rlanglibtest:::test_trace_unexported_child(e)
       3. rlanglibtest:::test_trace_unexported(e)

# global functions have `global::` prefix

    Code
      # Full
    Code
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-rlang:::g(current_env())
       2.   \-global::f(e)
    Code
      # Collapsed
    Code
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-rlang:::g(current_env())
       2.   \-global::f(e)
    Code
      # Branch
    Code
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. rlang:::g(current_env())
       2. global::f(e)

# local functions inheriting from global do not have `global::` prefix

    Code
      # Full
    Code
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-rlang:::g(current_env())
       2.   \-f(e)
    Code
      # Collapsed
    Code
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-rlang:::g(current_env())
       2.   \-f(e)
    Code
      # Branch
    Code
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. rlang:::g(current_env())
       2. f(e)

# can trim layers of backtraces

    Code
      local_options(rlang_trace_format_srcrefs = FALSE)
    Code
      cat_line("No trimming:")
    Output
      No trimming:
    Code
      summary(trace0)
    Output
           x
        1. \-rlang:::f(0) test-trace.R:482:2
        2.   +-base::identity(identity(g(n))) test-trace.R:478:7
        3.   +-base::identity(g(n))
        4.   \-rlang:::g(n)
        5.     +-base::identity(identity(h(n))) test-trace.R:479:7
        6.     +-base::identity(h(n))
        7.     \-rlang:::h(n)
        8.       +-base::identity(identity(trace_back(e, bottom = n))) test-trace.R:480:7
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
       1. \-rlang:::f(1) test-trace.R:483:2
       2.   +-base::identity(identity(g(n))) test-trace.R:478:7
       3.   +-base::identity(g(n))
       4.   \-rlang:::g(n)
       5.     +-base::identity(identity(h(n))) test-trace.R:479:7
       6.     +-base::identity(h(n))
       7.     \-rlang:::h(n)
    Code
      cat_line("", "", "Two layers:")
    Output
      
      
      Two layers:
    Code
      summary(trace2)
    Output
          x
       1. \-rlang:::f(2) test-trace.R:484:2
       2.   +-base::identity(identity(g(n))) test-trace.R:478:7
       3.   +-base::identity(g(n))
       4.   \-rlang:::g(n)
    Code
      cat_line("", "", "Three layers:")
    Output
      
      
      Three layers:
    Code
      summary(trace3)
    Output
          x
       1. \-rlang:::f(3) test-trace.R:485:2

