# tree printing only changes deliberately

    Code
      print(trace, dir = dir)
    Output
          x
       1. \-rlang (local) i() at test-trace.R:25:2
       2.   \-rlang (local) j(i) at test-trace.R:18:7
       3.     \-rlang (local) k(i) at test-trace.R:19:21
       4.       \-rlang (local) l(i) at test-trace.R:22:4
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
        1. \-rlang (local) f()
        2.   \-rlang (local) g() at test-trace.R:49:20
        3.     +-base::tryCatch(h(), foo = identity, bar = identity) at test-trace.R:50:20
        4.     | \-base (local) tryCatchList(expr, classes, parentenv, handlers)
        5.     |   +-base (local) tryCatchOne(...)
        6.     |   | \-base (local) doTryCatch(return(expr), name, parentenv, handler)
        7.     |   \-base (local) tryCatchList(expr, names[-nh], parentenv, handlers[-nh])
        8.     |     \-base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
        9.     |       \-base (local) doTryCatch(return(expr), name, parentenv, handler)
       10.     \-rlang (local) h()
       11.       +-base::tryCatch(i(), baz = identity) at test-trace.R:51:20
       12.       | \-base (local) tryCatchList(expr, classes, parentenv, handlers)
       13.       |   \-base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
       14.       |     \-base (local) doTryCatch(return(expr), name, parentenv, handler)
       15.       \-rlang (local) i()
       16.         +-base::tryCatch(trace_back(e, bottom = 0)) at test-trace.R:52:20
       17.         | \-base (local) tryCatchList(expr, classes, parentenv, handlers)
       18.         \-rlang::trace_back(e, bottom = 0)
    Code
      # Focused
      print_focused_trace(trace, dir = dir, srcrefs = srcrefs)
    Output
           x
        1. \-rlang (local) f()
        2.   \-rlang (local) g() at test-trace.R:49:20
        3.     +<<-base::tryCatch(h(), foo = identity, bar = identity) at test-trace.R:50:20>>
        4.     | <<\-base (local) tryCatchList(expr, classes, parentenv, handlers)>>
        5.     |   <<+-base (local) tryCatchOne(...)>>
        6.     |   <<| \-base (local) doTryCatch(return(expr), name, parentenv, handler)>>
        7.     |   <<\-base (local) tryCatchList(expr, names[-nh], parentenv, handlers[-nh])>>
        8.     |     <<\-base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])>>
        9.     |       <<\-base (local) doTryCatch(return(expr), name, parentenv, handler)>>
       10.     \-rlang (local) h()
       11.       +<<-base::tryCatch(i(), baz = identity) at test-trace.R:51:20>>
       12.       | <<\-base (local) tryCatchList(expr, classes, parentenv, handlers)>>
       13.       |   <<\-base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])>>
       14.       |     <<\-base (local) doTryCatch(return(expr), name, parentenv, handler)>>
       15.       \-rlang (local) i()
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
        1. rlang (local) f()
        2. rlang (local) g()
             at test-trace.R:49:20
       10. rlang (local) h()
       15. rlang (local) i()

---

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
           x
        1. \-rlang (local) f()
        2.   +-base::eval(quote(eval(quote(g())))) at test-trace.R:61:7
        3.   | \-base::eval(quote(eval(quote(g()))))
        4.   +-base::eval(quote(g()))
        5.   | \-base::eval(quote(g()))
        6.   \-rlang (local) g()
        7.     +-base::tryCatch(eval(quote(h())), foo = identity, bar = identity) at test-trace.R:62:7
        8.     | \-base (local) tryCatchList(expr, classes, parentenv, handlers)
        9.     |   +-base (local) tryCatchOne(...)
       10.     |   | \-base (local) doTryCatch(return(expr), name, parentenv, handler)
       11.     |   \-base (local) tryCatchList(expr, names[-nh], parentenv, handlers[-nh])
       12.     |     \-base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
       13.     |       \-base (local) doTryCatch(return(expr), name, parentenv, handler)
       14.     +-base::eval(quote(h()))
       15.     | \-base::eval(quote(h()))
       16.     \-rlang (local) h()
    Code
      # Focused
      print_focused_trace(trace, dir = dir, srcrefs = srcrefs)
    Output
           x
        1. \-rlang (local) f()
        2.   +<<-base::eval(quote(eval(quote(g())))) at test-trace.R:61:7>>
        3.   | <<\-base::eval(quote(eval(quote(g()))))>>
        4.   +<<-base::eval(quote(g()))>>
        5.   | <<\-base::eval(quote(g()))>>
        6.   \-rlang (local) g()
        7.     +<<-base::tryCatch(eval(quote(h())), foo = identity, bar = identity) at test-trace.R:62:7>>
        8.     | <<\-base (local) tryCatchList(expr, classes, parentenv, handlers)>>
        9.     |   <<+-base (local) tryCatchOne(...)>>
       10.     |   <<| \-base (local) doTryCatch(return(expr), name, parentenv, handler)>>
       11.     |   <<\-base (local) tryCatchList(expr, names[-nh], parentenv, handlers[-nh])>>
       12.     |     <<\-base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])>>
       13.     |       <<\-base (local) doTryCatch(return(expr), name, parentenv, handler)>>
       14.     +<<-base::eval(quote(h()))>>
       15.     | <<\-base::eval(quote(h()))>>
       16.     \-rlang (local) h()
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
        1. rlang (local) f()
        6. rlang (local) g()
       16. rlang (local) h()

# cli_branch() handles edge case

    Code
      cli_branch(tree[-1, ])
    Output
      [1] " 1. rlang (local) f()"

# collapsed formatting doesn't collapse single frame siblings

    Code
      print(trace, simplify = "none", drop = TRUE, srcrefs = FALSE)
    Output
          x
       1. \-rlang (local) f()
       2.   +-rlang::eval_bare(quote(g()))
       3.   \-rlang (local) g()
    Code
      print(trace, simplify = "none", drop = FALSE, srcrefs = FALSE)
    Output
          x
       1. \-rlang (local) f()
       2.   +-rlang::eval_bare(quote(g()))
       3.   \-rlang (local) g()

# recursive frames are rewired to the global env

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-rlang::eval_tidy(quo(f()))
       2. \-rlang (local) f()
       3.   \-rlang (local) g()
    Code
      # Focused
      print_focused_trace(trace, dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-rlang::eval_tidy(quo(f()))
       2. \-rlang (local) f()
       3.   \-rlang (local) g()
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. rlang::eval_tidy(quo(f()))
       2. rlang (local) f()
       3. rlang (local) g()

# long backtrace branches are truncated

    Code
      cat("Full:\n")
    Output
      Full:
    Code
      print(trace, simplify = "branch", srcrefs = FALSE)
    Output
        1. rlang (local) f(10)
        2. rlang (local) f(n - 1)
        3. rlang (local) f(n - 1)
        4. rlang (local) f(n - 1)
        5. rlang (local) f(n - 1)
        6. rlang (local) f(n - 1)
        7. rlang (local) f(n - 1)
        8. rlang (local) f(n - 1)
        9. rlang (local) f(n - 1)
       10. rlang (local) f(n - 1)
       11. rlang (local) f(n - 1)
    Code
      cat("\n5 frames:\n")
    Output
      
      5 frames:
    Code
      print(trace, simplify = "branch", max_frames = 5, srcrefs = FALSE)
    Output
        1. rlang (local) f(10)
        2. rlang (local) f(n - 1)
        3. rlang (local) f(n - 1)
           ...
       10. rlang (local) f(n - 1)
       11. rlang (local) f(n - 1)
    Code
      cat("\n2 frames:\n")
    Output
      
      2 frames:
    Code
      print(trace, simplify = "branch", max_frames = 2, srcrefs = FALSE)
    Output
        1. rlang (local) f(10)
           ...
       11. rlang (local) f(n - 1)
    Code
      cat("\n1 frame:\n")
    Output
      
      1 frame:
    Code
      print(trace, simplify = "branch", max_frames = 1, srcrefs = FALSE)
    Output
        1. rlang (local) f(10)
           ...

# eval() frames are collapsed

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-rlang (local) f()
       2.   +-base::eval(quote(g()))
       3.   | \-base::eval(quote(g()))
       4.   \-rlang (local) g()
       5.     +-base::eval(quote(trace_back(e, bottom = 0)))
       6.     | \-base::eval(quote(trace_back(e, bottom = 0)))
       7.     \-rlang::trace_back(e, bottom = 0)
    Code
      # Focused
      print_focused_trace(trace, dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-rlang (local) f()
       2.   +<<-base::eval(quote(g()))>>
       3.   | <<\-base::eval(quote(g()))>>
       4.   \-rlang (local) g()
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. rlang (local) f()
       4. rlang (local) g()

---

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-rlang (local) f()
       2.   +-base::evalq(g())
       3.   | \-base::evalq(g())
       4.   \-rlang (local) g()
       5.     +-base::evalq(trace_back(e, bottom = 0))
       6.     | \-base::evalq(trace_back(e, bottom = 0))
       7.     \-rlang::trace_back(e, bottom = 0)
    Code
      # Focused
      print_focused_trace(trace, dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-rlang (local) f()
       2.   +<<-base::evalq(g())>>
       3.   | <<\-base::evalq(g())>>
       4.   \-rlang (local) g()
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. rlang (local) f()
       4. rlang (local) g()

# children of collapsed frames are rechained to correct parent

    Code
      cat("Full + drop:\n")
    Output
      Full + drop:
    Code
      print(trace, simplify = "none", drop = TRUE, srcrefs = FALSE)
    Output
          x
       1. \-rlang (local) f()
       2.   \-base::eval(quote(g()), env())
       3.     \-base::eval(quote(g()), env())
       4.       \-rlang (local) g()
    Code
      cat("Full - drop:\n")
    Output
      Full - drop:
    Code
      print(trace, simplify = "none", drop = FALSE, srcrefs = FALSE)
    Output
          x
       1. \-rlang (local) f()
       2.   \-base::eval(quote(g()), env())
       3.     \-base::eval(quote(g()), env())
       4.       \-rlang (local) g()
    Code
      cat("\nBranch:\n")
    Output
      
      Branch:
    Code
      print(trace, simplify = "branch", srcrefs = FALSE)
    Output
       1. rlang (local) f()
       2. base::eval(quote(g()), env())
       3. base::eval(quote(g()), env())
       4. rlang (local) g()

# combinations of incomplete and leading pipes collapse properly

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-NA %>% F() %>% T() %>% F() %>% F()
       2. +-rlang (local) F(.)
       3. +-rlang (local) F(.)
       4. \-rlang (local) T(.)
    Code
      # Focused
      print_focused_trace(trace, dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-NA %>% F() %>% T() %>% F() %>% F()
       2. +-rlang (local) F(.)
       3. +-rlang (local) F(.)
       4. \-rlang (local) T(.)
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. NA %>% F() %>% T() %>% F() %>% F()
       4. rlang (local) T(.)

---

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-T(NA) %>% F()
       2. +-rlang (local) F(.)
       3. \-rlang (local) T(NA)
    Code
      # Focused
      print_focused_trace(trace, dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-T(NA) %>% F()
       2. +-rlang (local) F(.)
       3. \-rlang (local) T(NA)
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. T(NA) %>% F()
       3. rlang (local) T(NA)

---

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-F(NA) %>% F() %>% T() %>% F() %>% F()
       2. +-rlang (local) F(.)
       3. +-rlang (local) F(.)
       4. \-rlang (local) T(.)
    Code
      # Focused
      print_focused_trace(trace, dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-F(NA) %>% F() %>% T() %>% F() %>% F()
       2. +-rlang (local) F(.)
       3. +-rlang (local) F(.)
       4. \-rlang (local) T(.)
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. F(NA) %>% F() %>% T() %>% F() %>% F()
       4. rlang (local) T(.)

---

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-NA %>% T()
       2. \-rlang (local) T(.)
    Code
      # Focused
      print_focused_trace(trace, dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-NA %>% T()
       2. \-rlang (local) T(.)
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. NA %>% T()
       2. rlang (local) T(.)

---

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-NA %>% F() %>% T()
       2. \-rlang (local) T(.)
    Code
      # Focused
      print_focused_trace(trace, dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-NA %>% F() %>% T()
       2. \-rlang (local) T(.)
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. NA %>% F() %>% T()
       2. rlang (local) T(.)

---

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-F(NA) %>% T()
       2. \-rlang (local) T(.)
    Code
      # Focused
      print_focused_trace(trace, dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-F(NA) %>% T()
       2. \-rlang (local) T(.)
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. F(NA) %>% T()
       2. rlang (local) T(.)

---

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-F(NA) %>% F() %>% T()
       2. \-rlang (local) T(.)
    Code
      # Focused
      print_focused_trace(trace, dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-F(NA) %>% F() %>% T()
       2. \-rlang (local) T(.)
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. F(NA) %>% F() %>% T()
       2. rlang (local) T(.)

# calls before and after pipe are preserved

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-rlang (local) F(NA %>% T())
       2. +-NA %>% T()
       3. \-rlang (local) T(.)
    Code
      # Focused
      print_focused_trace(trace, dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-rlang (local) F(NA %>% T())
       2. +-NA %>% T()
       3. \-rlang (local) T(.)
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. rlang (local) F(NA %>% T())
       3. rlang (local) T(.)

---

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-NA %>% C()
       2. \-rlang (local) C(.)
       3.   \-rlang (local) f()
    Code
      # Focused
      print_focused_trace(trace, dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-NA %>% C()
       2. \-rlang (local) C(.)
       3.   \-rlang (local) f()
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. NA %>% C()
       2. rlang (local) C(.)
       3. rlang (local) f()

---

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-rlang (local) F(NA %>% C())
       2. +-NA %>% C()
       3. \-rlang (local) C(.)
       4.   \-rlang (local) f()
    Code
      # Focused
      print_focused_trace(trace, dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-rlang (local) F(NA %>% C())
       2. +-NA %>% C()
       3. \-rlang (local) C(.)
       4.   \-rlang (local) f()
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. rlang (local) F(NA %>% C())
       3. rlang (local) C(.)
       4. rlang (local) f()

# always keep very first frame as part of backtrace branch

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-rlang (local) gen()
       2. \-rlang (local) gen.default()
    Code
      # Focused
      print_focused_trace(trace, dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-rlang (local) gen()
       2. \-rlang (local) gen.default()
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. rlang (local) gen()
       2. rlang (local) gen.default()

# anonymous calls are stripped from backtraces

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-(function() {...
    Code
      # Focused
      print_focused_trace(trace, dir = dir, srcrefs = srcrefs)
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
       3.   \-rlang (local) h(simpleError(msg, call))
    Code
      # Focused
      print_focused_trace(trace, dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-base::eval()
       2. \-base::.handleSimpleError(...)
       3.   \-rlang (local) h(simpleError(msg, call))
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. base::eval()
       2. base::.handleSimpleError(...)
       3. rlang (local) h(simpleError(msg, call))

# can print degenerate backtraces

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-foo
    Code
      # Focused
      print_focused_trace(trace, dir = dir, srcrefs = srcrefs)
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
      # Focused
      print_focused_trace(trace, dir = dir, srcrefs = srcrefs)
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
      # Focused
      print_focused_trace(trace, dir = dir, srcrefs = srcrefs)
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
       2. \-rlang (local) print.foo(foo)
    Code
      # Focused
      print_focused_trace(trace, dir = dir, srcrefs = srcrefs)
    Output
          x
       1. +-base::print(foo)
       2. \-rlang (local) print.foo(foo)
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. base::print(foo)
       2. rlang (local) print.foo(foo)

# dangling srcrefs are not printed

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-rlang (local) f(current_env())
       2.   \-rlang (local) g(e) at fixtures/trace-srcref2.R:2:2
    Code
      # Focused
      print_focused_trace(trace, dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-rlang (local) f(current_env())
       2.   \-rlang (local) g(e) at fixtures/trace-srcref2.R:2:2
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. rlang (local) f(current_env())
       2. rlang (local) g(e)
            at fixtures/trace-srcref2.R:2:2

# summary.rlang_trace() prints the full tree

    Code
      summary(trace, srcrefs = FALSE)
    Output
          x
       1. \-rlang (local) f()
       2.   \-rlang (local) g()
       3.     \-rlang (local) h()

# global functions have `global::` prefix

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-rlang (local) g(current_env())
       2.   \-global f(e)
    Code
      # Focused
      print_focused_trace(trace, dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-rlang (local) g(current_env())
       2.   \-global f(e)
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. rlang (local) g(current_env())
       2. global f(e)

# local functions inheriting from global do not have `global::` prefix

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-rlang (local) g(current_env())
       2.   \-f(e)
    Code
      # Focused
      print_focused_trace(trace, dir = dir, srcrefs = srcrefs)
    Output
          x
       1. \-rlang (local) g(current_env())
       2.   \-f(e)
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
       1. rlang (local) g(current_env())
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
        1. \-rlang (local) f(0) at test-trace.R:412:2
        2.   +-base::identity(identity(g(n))) at test-trace.R:408:7
        3.   +-base::identity(g(n))
        4.   \-rlang (local) g(n)
        5.     +-base::identity(identity(h(n))) at test-trace.R:409:7
        6.     +-base::identity(h(n))
        7.     \-rlang (local) h(n)
        8.       +-base::identity(identity(trace_back(e, bottom = n))) at test-trace.R:410:7
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
       1. \-rlang (local) f(1) at test-trace.R:413:2
       2.   +-base::identity(identity(g(n))) at test-trace.R:408:7
       3.   +-base::identity(g(n))
       4.   \-rlang (local) g(n)
       5.     +-base::identity(identity(h(n))) at test-trace.R:409:7
       6.     +-base::identity(h(n))
       7.     \-rlang (local) h(n)
    Code
      cat_line("", "", "Two layers:")
    Output
      
      
      Two layers:
    Code
      summary(trace2)
    Output
          x
       1. \-rlang (local) f(2) at test-trace.R:414:2
       2.   +-base::identity(identity(g(n))) at test-trace.R:408:7
       3.   +-base::identity(g(n))
       4.   \-rlang (local) g(n)
    Code
      cat_line("", "", "Three layers:")
    Output
      
      
      Three layers:
    Code
      summary(trace3)
    Output
          x
       1. \-rlang (local) f(3) at test-trace.R:415:2

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
       2. \-rlang (local) f(`<list>`)
       3.   +-base::do.call("g", list(runif(1e+06) + 0))
       4.   \-rlang (local) g(`<dbl>`)
       5.     \-rlang (local) h()

# runs of namespaces are embolden (#946)

    Code
      print(err)
    Output
      [1m[1m[1m[34m<error/rlang_error>[39m[22m
      [1m[33mError[39m in [1m[1m[1m[94m`1 + ""`[39m[1m:[22m
      [33m![39m non-numeric argument to binary operator
      ---
      [1mBacktrace:[22m
      [90m  1. [39m[1mrlang[22m::catch_cnd(withCallingHandlers(f(), error = entrace), "error")
      [90m  9. [39mrlang (local) f()
      [90m 10. [39mrlang (local) g()
      [90m 11. [39mrlang (local) h()
      [90m 12. [39m[1mbase[22m::identity(1 + "")
    Code
      summary(err)
    Output
      [1m[1m[1m[34m<error/rlang_error>[39m[22m
      [1m[33mError[39m in [1m[1m[1m[94m`1 + ""`[39m[1m:[22m
      [33m![39m non-numeric argument to binary operator
      ---
      [1mBacktrace:[22m
      [90m     [39mx
      [90m  1. [39m+-[1mrlang[22m::catch_cnd(withCallingHandlers(f(), error = entrace), "error")
      [90m  2. [39m| +-rlang::eval_bare(...)
      [90m  3. [39m| +-[1mbase[22m::tryCatch(...)
      [90m  4. [39m| | \-base (local) tryCatchList(expr, classes, parentenv, handlers)
      [90m  5. [39m| |   \-base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
      [90m  6. [39m| |     \-base (local) doTryCatch(return(expr), name, parentenv, handler)
      [90m  7. [39m| \-base::force(expr)
      [90m  8. [39m+-base::withCallingHandlers(f(), error = entrace)
      [90m  9. [39m\-[1mrlang[22m (local) f()
      [90m 10. [39m  \-rlang (local) g()
      [90m 11. [39m    \-rlang (local) h()
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
     4. evalq()
     5. evalq()

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
      # Focused
      print_focused_trace(trace, dir = dir, srcrefs = srcrefs)
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
      # Focused
      print_focused_trace(trace, dir = dir, srcrefs = srcrefs)
    Output
      x
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
      

# sibling streaks in tree backtraces

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
      <error/rlang_error>
      Error in `h()`:
      ! foo
      ---
      Backtrace:
           x
        1. +-rlang::catch_cnd(f(g()), "error")
        2. | +-rlang::eval_bare(...)
        3. | +-base::tryCatch(...)
        4. | | \-base (local) tryCatchList(expr, classes, parentenv, handlers)
        5. | |   \-base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
        6. | |     \-base (local) doTryCatch(return(expr), name, parentenv, handler)
        7. | \-base::force(expr)
        8. +-rlang (local) f(g())
        9. | +-base::identity(identity(x))
       10. | \-base::identity(x)
       11. \-rlang (local) g()
       12.   +-rlang (local) f(f(h()))
       13.   | +-base::identity(identity(x))
       14.   | \-base::identity(x)
       15.   +-rlang (local) f(h())
       16.   | +-base::identity(identity(x))
       17.   | \-base::identity(x)
       18.   \-rlang (local) h()
       19.     \-rlang::abort("foo")
    Code
      # Focused
      print_focused_trace(trace, dir = dir, srcrefs = srcrefs)
    Output
      <error/rlang_error>
      Error in `h()`:
      ! foo
      ---
      Backtrace:
           x
        1. +-rlang::catch_cnd(f(g()), "error")
        2. | <<+-rlang::eval_bare(...)>>
        3. | <<+-base::tryCatch(...)>>
        4. | <<| \-base (local) tryCatchList(expr, classes, parentenv, handlers)>>
        5. | <<|   \-base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])>>
        6. | <<|     \-base (local) doTryCatch(return(expr), name, parentenv, handler)>>
        7. | <<\-base::force(expr)>>
        8. +-rlang (local) f(g())
        9. | <<+-base::identity(identity(x))>>
       10. | <<\-base::identity(x)>>
       11. \-rlang (local) g()
       12.   +<<-rlang (local) f(f(h()))>>
       13.   | <<+-base::identity(identity(x))>>
       14.   | <<\-base::identity(x)>>
       15.   +<<-rlang (local) f(h())>>
       16.   | <<+-base::identity(identity(x))>>
       17.   | <<\-base::identity(x)>>
       18.   \-rlang (local) h()
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
      <error/rlang_error>
      Error in `h()`:
      ! foo
      ---
      Backtrace:
        1. rlang::catch_cnd(f(g()), "error")
       11. rlang (local) g()
       18. rlang (local) h()

# parallel '|' branches are correctly emphasised

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
      <error/rlang_error>
      Error in `h()`:
      ! foo
      ---
      Backtrace:
           x
        1. +-testthat::expect_error(parallel(f(0)))
        2. | \-testthat:::expect_condition_matching(...)
        3. |   \-testthat:::quasi_capture(...)
        4. |     +-testthat (local) .capture(...)
        5. |     | \-base::withCallingHandlers(...)
        6. |     \-rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        7. +-rlang (local) parallel(f(0))
        8. | +-rlang (local) p1(identity(x))
        9. | | \-rlang (local) p2(x)
       10. | |   \-rlang (local) p3(x)
       11. | \-base::identity(x)
       12. \-rlang (local) f(0)
       13.   \-rlang (local) g(n)
       14.     \-rlang (local) h(n)
       15.       \-rlang::abort("foo")
    Code
      # Focused
      print_focused_trace(trace, dir = dir, srcrefs = srcrefs)
    Output
      <error/rlang_error>
      Error in `h()`:
      ! foo
      ---
      Backtrace:
           x
        1. +-testthat::expect_error(parallel(f(0)))
        2. | <<\-testthat:::expect_condition_matching(...)>>
        3. |   <<\-testthat:::quasi_capture(...)>>
        4. |     <<+-testthat (local) .capture(...)>>
        5. |     <<| \-base::withCallingHandlers(...)>>
        6. |     <<\-rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))>>
        7. +-rlang (local) parallel(f(0))
        8. | <<+-rlang (local) p1(identity(x))>>
        9. | <<| \-rlang (local) p2(x)>>
       10. | <<|   \-rlang (local) p3(x)>>
       11. | <<\-base::identity(x)>>
       12. \-rlang (local) f(0)
       13.   \-rlang (local) g(n)
       14.     \-rlang (local) h(n)
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
      <error/rlang_error>
      Error in `h()`:
      ! foo
      ---
      Backtrace:
        1. testthat::expect_error(parallel(f(0)))
       12. rlang (local) f(0)
       13. rlang (local) g(n)
       14. rlang (local) h(n)

---

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
      <error/rlang_error>
      Error in `h()`:
      ! foo
      ---
      Backtrace:
           x
        1. +-testthat::expect_error(deep(1))
        2. | \-testthat:::expect_condition_matching(...)
        3. |   \-testthat:::quasi_capture(...)
        4. |     +-testthat (local) .capture(...)
        5. |     | \-base::withCallingHandlers(...)
        6. |     \-rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        7. \-rlang (local) deep(1)
        8.   +-rlang (local) parallel(f(n))
        9.   | +-rlang (local) p1(identity(x))
       10.   | | \-rlang (local) p2(x)
       11.   | |   \-rlang (local) p3(x)
       12.   | \-base::identity(x)
       13.   \-rlang (local) f(n)
       14.     \-rlang (local) g(n)
       15.       \-rlang (local) h(n)
       16.         +-rlang (local) parallel(f(n - 1))
       17.         | +-rlang (local) p1(identity(x))
       18.         | | \-rlang (local) p2(x)
       19.         | |   \-rlang (local) p3(x)
       20.         | \-base::identity(x)
       21.         \-rlang (local) f(n - 1)
       22.           \-rlang (local) g(n)
       23.             \-rlang (local) h(n)
       24.               \-rlang::abort("foo")
    Code
      # Focused
      print_focused_trace(trace, dir = dir, srcrefs = srcrefs)
    Output
      <error/rlang_error>
      Error in `h()`:
      ! foo
      ---
      Backtrace:
           x
        1. +-testthat::expect_error(deep(1))
        2. | <<\-testthat:::expect_condition_matching(...)>>
        3. |   <<\-testthat:::quasi_capture(...)>>
        4. |     <<+-testthat (local) .capture(...)>>
        5. |     <<| \-base::withCallingHandlers(...)>>
        6. |     <<\-rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))>>
        7. \-rlang (local) deep(1)
        8.   +<<-rlang (local) parallel(f(n))>>
        9.   | <<+-rlang (local) p1(identity(x))>>
       10.   | <<| \-rlang (local) p2(x)>>
       11.   | <<|   \-rlang (local) p3(x)>>
       12.   | <<\-base::identity(x)>>
       13.   \-rlang (local) f(n)
       14.     \-rlang (local) g(n)
       15.       \-rlang (local) h(n)
       16.         +<<-rlang (local) parallel(f(n - 1))>>
       17.         | <<+-rlang (local) p1(identity(x))>>
       18.         | <<| \-rlang (local) p2(x)>>
       19.         | <<|   \-rlang (local) p3(x)>>
       20.         | <<\-base::identity(x)>>
       21.         \-rlang (local) f(n - 1)
       22.           \-rlang (local) g(n)
       23.             \-rlang (local) h(n)
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
      <error/rlang_error>
      Error in `h()`:
      ! foo
      ---
      Backtrace:
        1. testthat::expect_error(deep(1))
        7. rlang (local) deep(1)
       13. rlang (local) f(n)
       14. rlang (local) g(n)
       15. rlang (local) h(n)
       21. rlang (local) f(n - 1)
       22. rlang (local) g(n)
       23. rlang (local) h(n)

# error calls and args are highlighted

    Code
      print_highlighted_trace(parent)
    Output
      <error/rlang_error>
      Error in <<CALL `h()`>>:
      ! `x` must be a single string, not a number.
      ---
      Backtrace:
           x
        1. +-rlang:::catch_error(f(1))
        2. | \-rlang::catch_cnd(expr, "error")
        3. |   +-rlang::eval_bare(...)
        4. |   +-base::tryCatch(...)
        5. |   | \-base (local) tryCatchList(expr, classes, parentenv, handlers)
        6. |   |   \-base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
        7. |   |     \-base (local) doTryCatch(return(expr), name, parentenv, handler)
        8. |   \-base::force(expr)
        9. \-rlang (local) f(1)
       10.   \-rlang (local) g(x)
       11.     \-rlang (local) <<CALL h(>><<ARG x = x>><<CALL )>>
    Code
      print_highlighted_trace(child)
    Output
      <error/rlang_error>
      Error in <<CALL `wrapper()`>>:
      ! Tilt.
      Caused by error in <<CALL `h()`>>:
      ! `x` must be a single string, not a number.
      ---
      Backtrace:
           x
        1. +-rlang:::catch_error(wrapper())
        2. | \-rlang::catch_cnd(expr, "error")
        3. |   +-rlang::eval_bare(...)
        4. |   +-base::tryCatch(...)
        5. |   | \-base (local) tryCatchList(expr, classes, parentenv, handlers)
        6. |   |   \-base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
        7. |   |     \-base (local) doTryCatch(return(expr), name, parentenv, handler)
        8. |   \-base::force(expr)
        9. \-rlang (local) wrapper()
       10.   +-rlang::try_fetch(f(1), error = function(cnd) abort("Tilt.", parent = cnd))
       11.   | +-base::tryCatch(...)
       12.   | | \-base (local) tryCatchList(expr, classes, parentenv, handlers)
       13.   | |   \-base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
       14.   | |     \-base (local) doTryCatch(return(expr), name, parentenv, handler)
       15.   | \-base::withCallingHandlers(...)
       16.   \-rlang (local) f(1)
       17.     \-rlang (local) g(x)
       18.       \-rlang (local) <<CALL h(>><<ARG x = x>><<CALL )>>

# error calls and args are highlighted (no highlighted arg)

    Code
      print_highlighted_trace(argless)
    Output
      <error/rlang_error>
      Error in <<CALL `h()`>>:
      ! foo
      ---
      Backtrace:
           x
        1. +-rlang:::catch_error(f())
        2. | \-rlang::catch_cnd(expr, "error")
        3. |   +-rlang::eval_bare(...)
        4. |   +-base::tryCatch(...)
        5. |   | \-base (local) tryCatchList(expr, classes, parentenv, handlers)
        6. |   |   \-base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
        7. |   |     \-base (local) doTryCatch(return(expr), name, parentenv, handler)
        8. |   \-base::force(expr)
        9. \-rlang (local) f()
       10.   \-rlang (local) g()
       11.     \-rlang (local) <<CALL h(>><<CALL )>>

# frame is detected from the left

    Code
      # If detected from the right, `evalq()`is highlighted instead of `h()`
      print_highlighted_trace(err)
    Output
      <error/rlang_error>
      Error in <<CALL `h()`>>:
      ! foo
      ---
      Backtrace:
           x
        1. +-rlang:::catch_error(f())
        2. | \-rlang::catch_cnd(expr, "error")
        3. |   +-rlang::eval_bare(...)
        4. |   +-base::tryCatch(...)
        5. |   | \-base (local) tryCatchList(expr, classes, parentenv, handlers)
        6. |   |   \-base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
        7. |   |     \-base (local) doTryCatch(return(expr), name, parentenv, handler)
        8. |   \-base::force(expr)
        9. \-rlang (local) f()
       10.   \-rlang (local) g()
       11.     \-rlang (local) <<CALL h(>><<CALL )>>

# arg is defensively checked

    Code
      print_highlighted_trace(err)
    Output
      <error/rlang_error>
      Error in <<CALL `h()`>>:
      ! foo
      ---
      Backtrace:
           x
        1. +-rlang:::catch_error(f())
        2. | \-rlang::catch_cnd(expr, "error")
        3. |   +-rlang::eval_bare(...)
        4. |   +-base::tryCatch(...)
        5. |   | \-base (local) tryCatchList(expr, classes, parentenv, handlers)
        6. |   |   \-base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
        7. |   |     \-base (local) doTryCatch(return(expr), name, parentenv, handler)
        8. |   \-base::force(expr)
        9. \-rlang (local) f()
       10.   \-rlang (local) g()
       11.     \-rlang (local) <<CALL h(>><<CALL )>>

# namespaced calls are highlighted

    Code
      print_highlighted_trace(err)
    Output
      <error/rlang_error>
      Error in <<CALL `rlang:::as_string()`>>:
      ! Can't convert a number to a string.
      ---
      Backtrace:
           x
        1. +-rlang:::catch_error(f())
        2. | \-rlang::catch_cnd(expr, "error")
        3. |   +-rlang::eval_bare(...)
        4. |   +-base::tryCatch(...)
        5. |   | \-base (local) tryCatchList(expr, classes, parentenv, handlers)
        6. |   |   \-base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
        7. |   |     \-base (local) doTryCatch(return(expr), name, parentenv, handler)
        8. |   \-base::force(expr)
        9. \-rlang (local) f()
       10.   \-rlang (local) g()
       11.     \-rlang (local) h()
       12.       \-<<CALL rlang:::as_string(>>1<<CALL )>>

# can highlight long lists of arguments in backtrace (#1456)

    Code
      print_highlighted_trace(err)
    Output
      <error/rlang_error>
      Error in <<CALL `g()`>>:
      ! foo
      ---
      Backtrace:
           x
        1. +-rlang:::catch_error(f())
        2. | \-rlang::catch_cnd(expr, "error")
        3. |   +-rlang::eval_bare(...)
        4. |   +-base::tryCatch(...)
        5. |   | \-base (local) tryCatchList(expr, classes, parentenv, handlers)
        6. |   |   \-base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
        7. |   |     \-base (local) doTryCatch(return(expr), name, parentenv, handler)
        8. |   \-base::force(expr)
        9. \-rlang (local) f()
       10.   \-rlang (local) <<CALL g(>>aaaaaaaaaaaa = aaaaaaaaaaaa, bbbbbbbbbbbb = bbbbbbbbbbbb, cccccccccccc = cccccccccccc,
        dddddddddddd = dddddddddddd, eeeeeeeeeeee = eeeeeeeeeeee, ...<<CALL )>>

---

    Code
      print_highlighted_trace(err)
    Output
      <error/rlang_error>
      Error in <<CALL `g()`>>:
      ! foo
      ---
      Backtrace:
           x
        1. +-rlang:::catch_error(f(arg = "bbbbbbbbbbbb"))
        2. | \-rlang::catch_cnd(expr, "error")
        3. |   +-rlang::eval_bare(...)
        4. |   +-base::tryCatch(...)
        5. |   | \-base (local) tryCatchList(expr, classes, parentenv, handlers)
        6. |   |   \-base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
        7. |   |     \-base (local) doTryCatch(return(expr), name, parentenv, handler)
        8. |   \-base::force(expr)
        9. \-rlang (local) f(arg = "bbbbbbbbbbbb")
       10.   \-rlang (local) <<CALL g(>><<ARG bbbbbbbbbbbb = bbbbbbbbbbbb>><<CALL )>>

# can highlight multi-line arguments in backtrace (#1456)

    Code
      print_highlighted_trace(err)
    Output
      <error/rlang_error>
      Error in <<CALL `g()`>>:
      ! foo
      ---
      Backtrace:
           x
        1. +-rlang:::catch_error(f())
        2. | \-rlang::catch_cnd(expr, "error")
        3. |   +-rlang::eval_bare(...)
        4. |   +-base::tryCatch(...)
        5. |   | \-base (local) tryCatchList(expr, classes, parentenv, handlers)
        6. |   |   \-base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
        7. |   |     \-base (local) doTryCatch(return(expr), name, parentenv, handler)
        8. |   \-base::force(expr)
        9. \-rlang (local) f()
       10.   \-rlang (local) <<CALL g(>>x = {
        a
        b
      }, ...<<CALL )>>

---

    Code
      print_highlighted_trace(err)
    Output
      <error/rlang_error>
      Error in <<CALL `g()`>>:
      ! foo
      ---
      Backtrace:
           x
        1. +-rlang:::catch_error(f(arg = "x"))
        2. | \-rlang::catch_cnd(expr, "error")
        3. |   +-rlang::eval_bare(...)
        4. |   +-base::tryCatch(...)
        5. |   | \-base (local) tryCatchList(expr, classes, parentenv, handlers)
        6. |   |   \-base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
        7. |   |     \-base (local) doTryCatch(return(expr), name, parentenv, handler)
        8. |   \-base::force(expr)
        9. \-rlang (local) f(arg = "x")
       10.   \-rlang (local) <<CALL g(>><<ARG x = { ... }>><<CALL )>>

