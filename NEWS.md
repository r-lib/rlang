
# rlang 0.1.9000

* `env_child()`'s first argument is now `.parent` instead of `parent`.

* `env()` and `env_child()` can now get arguments starting with `.`.
  Prior to this fix, these arguments were partial-matching on
  `env_bind()`'s `.env` argument.


# rlang 0.1

Initial release.
