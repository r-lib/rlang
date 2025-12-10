# Package index

## Tidy evaluation

The programmable data-masking framework developed for the tidyverse.

### Tools

- [`embrace-operator`](https://rlang.r-lib.org/reference/embrace-operator.md)
  [`curly-curly`](https://rlang.r-lib.org/reference/embrace-operator.md)
  :

  Embrace operator `{{`

- [`glue-operators`](https://rlang.r-lib.org/reference/glue-operators.md)
  :

  Name injection with `"{"` and `"{{"`

- [`dot-data`](https://rlang.r-lib.org/reference/dot-data.md)
  [`.data`](https://rlang.r-lib.org/reference/dot-data.md)
  [`tidyeval-data`](https://rlang.r-lib.org/reference/dot-data.md)
  [`.env`](https://rlang.r-lib.org/reference/dot-data.md) :

  `.data` and `.env` pronouns

### Metaprogramming tools

- [`injection-operator`](https://rlang.r-lib.org/reference/injection-operator.md)
  [`bang-bang`](https://rlang.r-lib.org/reference/injection-operator.md)
  [`!!`](https://rlang.r-lib.org/reference/injection-operator.md) :

  Injection operator `!!`

- [`splice-operator`](https://rlang.r-lib.org/reference/splice-operator.md)
  [`!!!`](https://rlang.r-lib.org/reference/splice-operator.md) :

  Splice operator `!!!`

- [`qq_show`](https://rlang.r-lib.org/reference/qq_show.md) : Show
  injected expression

- [`englue()`](https://rlang.r-lib.org/reference/englue.md) : Defuse
  function arguments with glue

- [`expr`](https://rlang.r-lib.org/reference/expr.md) : Defuse an R
  expression

- [`enquo()`](https://rlang.r-lib.org/reference/enquo.md)
  [`enquos()`](https://rlang.r-lib.org/reference/enquo.md) : Defuse
  function arguments

- [`sym()`](https://rlang.r-lib.org/reference/sym.md)
  [`syms()`](https://rlang.r-lib.org/reference/sym.md)
  [`data_sym()`](https://rlang.r-lib.org/reference/sym.md)
  [`data_syms()`](https://rlang.r-lib.org/reference/sym.md) : Create a
  symbol or list of symbols

- [`as_label()`](https://rlang.r-lib.org/reference/as_label.md) : Create
  a default name for an R object

- [`as_name()`](https://rlang.r-lib.org/reference/as_name.md) : Extract
  names from symbols

### Advanced tools

- [`enexpr()`](https://rlang.r-lib.org/reference/defusing-advanced.md)
  [`exprs()`](https://rlang.r-lib.org/reference/defusing-advanced.md)
  [`enexprs()`](https://rlang.r-lib.org/reference/defusing-advanced.md)
  [`ensym()`](https://rlang.r-lib.org/reference/defusing-advanced.md)
  [`ensyms()`](https://rlang.r-lib.org/reference/defusing-advanced.md)
  [`quo()`](https://rlang.r-lib.org/reference/defusing-advanced.md)
  [`quos()`](https://rlang.r-lib.org/reference/defusing-advanced.md)
  [`enquo0()`](https://rlang.r-lib.org/reference/defusing-advanced.md)
  [`enquos0()`](https://rlang.r-lib.org/reference/defusing-advanced.md)
  : Advanced defusal operators
- [`eval_tidy()`](https://rlang.r-lib.org/reference/eval_tidy.md) :
  Evaluate an expression with quosures and pronoun support
- [`as_data_mask()`](https://rlang.r-lib.org/reference/as_data_mask.md)
  [`as_data_pronoun()`](https://rlang.r-lib.org/reference/as_data_mask.md)
  [`new_data_mask()`](https://rlang.r-lib.org/reference/as_data_mask.md)
  : Create a data mask

## Function arguments

### Check arguments

- [`arg_match()`](https://rlang.r-lib.org/reference/arg_match.md)
  [`arg_match0()`](https://rlang.r-lib.org/reference/arg_match.md) :
  Match an argument to a character vector
- [`check_exclusive()`](https://rlang.r-lib.org/reference/check_exclusive.md)
  : Check that arguments are mutually exclusive
- [`check_required()`](https://rlang.r-lib.org/reference/check_required.md)
  : Check that argument is supplied
- [`missing_arg()`](https://rlang.r-lib.org/reference/missing_arg.md)
  [`is_missing()`](https://rlang.r-lib.org/reference/missing_arg.md)
  [`maybe_missing()`](https://rlang.r-lib.org/reference/missing_arg.md)
  : Generate or handle a missing argument

### Check dots

- [`check_dots_empty()`](https://rlang.r-lib.org/reference/check_dots_empty.md)
  : Check that dots are empty
- [`check_dots_used()`](https://rlang.r-lib.org/reference/check_dots_used.md)
  : Check that all dots have been used
- [`check_dots_unnamed()`](https://rlang.r-lib.org/reference/check_dots_unnamed.md)
  : Check that all dots are unnamed

### Collect dynamic dots

Collect arguments contained in `...` with `!!!` and name-injection
support.

- [`dyn-dots`](https://rlang.r-lib.org/reference/dyn-dots.md)
  [`tidy-dots`](https://rlang.r-lib.org/reference/dyn-dots.md)
  [`doc_dots_dynamic`](https://rlang.r-lib.org/reference/dyn-dots.md)
  [`:=`](https://rlang.r-lib.org/reference/dyn-dots.md) : Dynamic dots
  features
- [`list2()`](https://rlang.r-lib.org/reference/list2.md)
  [`dots_list()`](https://rlang.r-lib.org/reference/list2.md) : Collect
  dynamic dots in a list
- [`pairlist2()`](https://rlang.r-lib.org/reference/pairlist2.md) :
  Collect dynamic dots in a pairlist
- [`splice()`](https://rlang.r-lib.org/reference/splice.md)
  [`is_spliced()`](https://rlang.r-lib.org/reference/splice.md)
  [`is_spliced_bare()`](https://rlang.r-lib.org/reference/splice.md) :
  Splice values at dots collection time

## Error handling

### Signal errors and other conditions

- [`abort()`](https://rlang.r-lib.org/reference/abort.md)
  [`warn()`](https://rlang.r-lib.org/reference/abort.md)
  [`inform()`](https://rlang.r-lib.org/reference/abort.md)
  [`signal()`](https://rlang.r-lib.org/reference/abort.md)
  [`reset_warning_verbosity()`](https://rlang.r-lib.org/reference/abort.md)
  [`reset_message_verbosity()`](https://rlang.r-lib.org/reference/abort.md)
  : Signal an error, warning, or message
- [`cnd_signal()`](https://rlang.r-lib.org/reference/cnd_signal.md) :
  Signal a condition object
- [`local_use_cli()`](https://rlang.r-lib.org/reference/local_use_cli.md)
  **\[experimental\]** : Use cli to format error messages

### Handle errors

- [`global_handle()`](https://rlang.r-lib.org/reference/global_handle.md)
  : Register default global handlers
- [`global_entrace()`](https://rlang.r-lib.org/reference/global_entrace.md)
  : Entrace unexpected errors
- [`global_prompt_install()`](https://rlang.r-lib.org/reference/global_prompt_install.md)
  : Prompt user to install missing packages
- [`try_fetch()`](https://rlang.r-lib.org/reference/try_fetch.md)
  **\[experimental\]** : Try an expression with condition handlers
- [`caller_arg`](https://rlang.r-lib.org/reference/caller_arg.md) : Find
  the caller argument for error messages
- [`local_error_call()`](https://rlang.r-lib.org/reference/local_error_call.md)
  : Set local error call in an execution environment
- [`args_error_context`](https://rlang.r-lib.org/reference/args_error_context.md)
  : Documentation anchor for error arguments
- [`catch_cnd()`](https://rlang.r-lib.org/reference/catch_cnd.md) :
  Catch a condition

### Backtraces

- [`last_error()`](https://rlang.r-lib.org/reference/last_error.md)
  [`last_trace()`](https://rlang.r-lib.org/reference/last_error.md) :

  Last [`abort()`](https://rlang.r-lib.org/reference/abort.md) error

- [`last_warnings()`](https://rlang.r-lib.org/reference/last_warnings.md)
  [`last_messages()`](https://rlang.r-lib.org/reference/last_warnings.md)
  : Display last messages and warnings

- [`global_entrace()`](https://rlang.r-lib.org/reference/global_entrace.md)
  : Entrace unexpected errors

- [`rlang_backtrace_on_error`](https://rlang.r-lib.org/reference/rlang_backtrace_on_error.md)
  [`add_backtrace`](https://rlang.r-lib.org/reference/rlang_backtrace_on_error.md)
  [`rlang_backtrace_on_error_report`](https://rlang.r-lib.org/reference/rlang_backtrace_on_error.md)
  [`rlang_backtrace_on_warning_report`](https://rlang.r-lib.org/reference/rlang_backtrace_on_error.md)
  : Display backtrace on error

- [`trace_back()`](https://rlang.r-lib.org/reference/trace_back.md)
  [`trace_length()`](https://rlang.r-lib.org/reference/trace_back.md) :
  Capture a backtrace

### Conditions

- [`rlang_error`](https://rlang.r-lib.org/reference/rlang_error.md)
  **\[experimental\]** :

  Errors of class `rlang_error`

- [`cnd_message()`](https://rlang.r-lib.org/reference/cnd_message.md)
  [`cnd_header()`](https://rlang.r-lib.org/reference/cnd_message.md)
  [`cnd_body()`](https://rlang.r-lib.org/reference/cnd_message.md)
  [`cnd_footer()`](https://rlang.r-lib.org/reference/cnd_message.md) :
  Build an error message from parts

- [`format_error_bullets()`](https://rlang.r-lib.org/reference/format_error_bullets.md)
  : Format bullets for error messages

- [`cnd_inherits()`](https://rlang.r-lib.org/reference/cnd_inherits.md)
  : Does a condition or its ancestors inherit from a class?

## Session

### State

- [`is_installed()`](https://rlang.r-lib.org/reference/is_installed.md)
  [`check_installed()`](https://rlang.r-lib.org/reference/is_installed.md)
  : Are packages installed in any of the libraries?
- [`is_interactive()`](https://rlang.r-lib.org/reference/is_interactive.md)
  [`local_interactive()`](https://rlang.r-lib.org/reference/is_interactive.md)
  [`with_interactive()`](https://rlang.r-lib.org/reference/is_interactive.md)
  : Is R running interactively?
- [`local_options()`](https://rlang.r-lib.org/reference/local_options.md)
  [`with_options()`](https://rlang.r-lib.org/reference/local_options.md)
  [`push_options()`](https://rlang.r-lib.org/reference/local_options.md)
  [`peek_options()`](https://rlang.r-lib.org/reference/local_options.md)
  [`peek_option()`](https://rlang.r-lib.org/reference/local_options.md)
  : Change global options
- [`on_load()`](https://rlang.r-lib.org/reference/on_load.md)
  [`run_on_load()`](https://rlang.r-lib.org/reference/on_load.md)
  [`on_package_load()`](https://rlang.r-lib.org/reference/on_load.md) :
  Run expressions on load
- [`faq-options`](https://rlang.r-lib.org/reference/faq-options.md) :
  Global options for rlang

### Search path and namespaces

- [`search_envs()`](https://rlang.r-lib.org/reference/search_envs.md)
  [`search_env()`](https://rlang.r-lib.org/reference/search_envs.md)
  [`pkg_env()`](https://rlang.r-lib.org/reference/search_envs.md)
  [`pkg_env_name()`](https://rlang.r-lib.org/reference/search_envs.md)
  [`is_attached()`](https://rlang.r-lib.org/reference/search_envs.md)
  [`base_env()`](https://rlang.r-lib.org/reference/search_envs.md)
  [`global_env()`](https://rlang.r-lib.org/reference/search_envs.md) :
  Search path environments
- [`empty_env()`](https://rlang.r-lib.org/reference/empty_env.md) : Get
  the empty environment
- [`is_namespace()`](https://rlang.r-lib.org/reference/is_namespace.md)
  : Is an object a namespace environment?
- [`ns_env()`](https://rlang.r-lib.org/reference/ns_env.md)
  [`ns_imports_env()`](https://rlang.r-lib.org/reference/ns_env.md)
  [`ns_env_name()`](https://rlang.r-lib.org/reference/ns_env.md) : Get
  the namespace of a package
- [`env_name()`](https://rlang.r-lib.org/reference/env_name.md)
  [`env_label()`](https://rlang.r-lib.org/reference/env_name.md) : Label
  of an environment

## Defused expressions

- [`parse_expr()`](https://rlang.r-lib.org/reference/parse_expr.md)
  [`parse_exprs()`](https://rlang.r-lib.org/reference/parse_expr.md)
  [`parse_quo()`](https://rlang.r-lib.org/reference/parse_expr.md)
  [`parse_quos()`](https://rlang.r-lib.org/reference/parse_expr.md) :
  Parse R code
- [`expr_print()`](https://rlang.r-lib.org/reference/expr_print.md)
  [`expr_deparse()`](https://rlang.r-lib.org/reference/expr_print.md) :
  Print an expression
- [`is_expression()`](https://rlang.r-lib.org/reference/is_expression.md)
  [`is_syntactic_literal()`](https://rlang.r-lib.org/reference/is_expression.md)
  [`is_symbolic()`](https://rlang.r-lib.org/reference/is_expression.md)
  : Is an object an expression?
- [`exprs_auto_name()`](https://rlang.r-lib.org/reference/exprs_auto_name.md)
  [`quos_auto_name()`](https://rlang.r-lib.org/reference/exprs_auto_name.md)
  : Ensure that all elements of a list of expressions are named

### Evaluate

- [`eval_tidy()`](https://rlang.r-lib.org/reference/eval_tidy.md) :
  Evaluate an expression with quosures and pronoun support
- [`eval_bare()`](https://rlang.r-lib.org/reference/eval_bare.md) :
  Evaluate an expression in an environment
- [`exec()`](https://rlang.r-lib.org/reference/exec.md) : Execute a
  function
- [`inject()`](https://rlang.r-lib.org/reference/inject.md) : Inject
  objects in an R expression

### Symbols

- [`sym()`](https://rlang.r-lib.org/reference/sym.md)
  [`syms()`](https://rlang.r-lib.org/reference/sym.md)
  [`data_sym()`](https://rlang.r-lib.org/reference/sym.md)
  [`data_syms()`](https://rlang.r-lib.org/reference/sym.md) : Create a
  symbol or list of symbols
- [`is_symbol()`](https://rlang.r-lib.org/reference/is_symbol.md) : Is
  object a symbol?
- [`as_string()`](https://rlang.r-lib.org/reference/as_string.md) : Cast
  symbol to string

### Calls

- [`call2()`](https://rlang.r-lib.org/reference/call2.md) : Create a
  call
- [`is_call()`](https://rlang.r-lib.org/reference/is_call.md) : Is
  object a call?
- [`call_args()`](https://rlang.r-lib.org/reference/call_args.md)
  [`call_args_names()`](https://rlang.r-lib.org/reference/call_args.md)
  : Extract arguments from a call
- [`call_inspect()`](https://rlang.r-lib.org/reference/call_inspect.md)
  : Inspect a call
- [`call_match()`](https://rlang.r-lib.org/reference/call_match.md) :
  Match supplied arguments to function definition
- [`call_modify()`](https://rlang.r-lib.org/reference/call_modify.md) :
  Modify the arguments of a call
- [`call_name()`](https://rlang.r-lib.org/reference/call_name.md)
  [`call_ns()`](https://rlang.r-lib.org/reference/call_name.md)
  [`is_call_simple()`](https://rlang.r-lib.org/reference/call_name.md) :
  Extract function name or namespace of a call

### Quosures

- [`new_quosure()`](https://rlang.r-lib.org/reference/new_quosure.md)
  [`as_quosure()`](https://rlang.r-lib.org/reference/new_quosure.md)
  [`is_quosure()`](https://rlang.r-lib.org/reference/new_quosure.md) :
  Create a quosure from components
- [`new_quosures()`](https://rlang.r-lib.org/reference/new_quosures.md)
  [`as_quosures()`](https://rlang.r-lib.org/reference/new_quosures.md)
  [`is_quosures()`](https://rlang.r-lib.org/reference/new_quosures.md) :
  Create a list of quosures
- [`quo_is_missing()`](https://rlang.r-lib.org/reference/quosure-tools.md)
  [`quo_is_symbol()`](https://rlang.r-lib.org/reference/quosure-tools.md)
  [`quo_is_call()`](https://rlang.r-lib.org/reference/quosure-tools.md)
  [`quo_is_symbolic()`](https://rlang.r-lib.org/reference/quosure-tools.md)
  [`quo_is_null()`](https://rlang.r-lib.org/reference/quosure-tools.md)
  [`quo_get_expr()`](https://rlang.r-lib.org/reference/quosure-tools.md)
  [`quo_get_env()`](https://rlang.r-lib.org/reference/quosure-tools.md)
  [`quo_set_expr()`](https://rlang.r-lib.org/reference/quosure-tools.md)
  [`quo_set_env()`](https://rlang.r-lib.org/reference/quosure-tools.md)
  : Quosure getters, setters and predicates
- [`quo_squash()`](https://rlang.r-lib.org/reference/quo_squash.md) :
  Squash a quosure

### Formulas

- [`f_rhs()`](https://rlang.r-lib.org/reference/f_rhs.md)
  [`` `f_rhs<-`() ``](https://rlang.r-lib.org/reference/f_rhs.md)
  [`f_lhs()`](https://rlang.r-lib.org/reference/f_rhs.md)
  [`` `f_lhs<-`() ``](https://rlang.r-lib.org/reference/f_rhs.md)
  [`f_env()`](https://rlang.r-lib.org/reference/f_rhs.md)
  [`` `f_env<-`() ``](https://rlang.r-lib.org/reference/f_rhs.md) : Get
  or set formula components
- [`f_text()`](https://rlang.r-lib.org/reference/f_text.md)
  [`f_name()`](https://rlang.r-lib.org/reference/f_text.md)
  [`f_label()`](https://rlang.r-lib.org/reference/f_text.md) : Turn RHS
  of formula into a string or label
- [`new_formula()`](https://rlang.r-lib.org/reference/new_formula.md) :
  Create a formula
- [`is_formula()`](https://rlang.r-lib.org/reference/is_formula.md)
  [`is_bare_formula()`](https://rlang.r-lib.org/reference/is_formula.md)
  : Is object a formula?

## Objects

- [`hash()`](https://rlang.r-lib.org/reference/hash.md)
  [`hash_file()`](https://rlang.r-lib.org/reference/hash.md) : Hashing

### Environments

- [`env()`](https://rlang.r-lib.org/reference/env.md)
  [`new_environment()`](https://rlang.r-lib.org/reference/env.md) :
  Create a new environment
- [`env_print()`](https://rlang.r-lib.org/reference/env_print.md) :
  Pretty-print an environment
- [`env_parent()`](https://rlang.r-lib.org/reference/env_parent.md)
  [`env_tail()`](https://rlang.r-lib.org/reference/env_parent.md)
  [`env_parents()`](https://rlang.r-lib.org/reference/env_parent.md) :
  Get parent environments
- [`env_depth()`](https://rlang.r-lib.org/reference/env_depth.md) :
  Depth of an environment chain
- [`get_env()`](https://rlang.r-lib.org/reference/get_env.md)
  [`set_env()`](https://rlang.r-lib.org/reference/get_env.md)
  [`env_poke_parent()`](https://rlang.r-lib.org/reference/get_env.md) :
  Get or set the environment of an object
- [`env_clone()`](https://rlang.r-lib.org/reference/env_clone.md)
  [`env_coalesce()`](https://rlang.r-lib.org/reference/env_clone.md) :
  Clone or coalesce an environment
- [`env_inherits()`](https://rlang.r-lib.org/reference/env_inherits.md)
  : Does environment inherit from another environment?
- [`is_environment()`](https://rlang.r-lib.org/reference/is_environment.md)
  [`is_bare_environment()`](https://rlang.r-lib.org/reference/is_environment.md)
  : Is object an environment?
- [`as_environment()`](https://rlang.r-lib.org/reference/as_environment.md)
  : Coerce to an environment
- [`current_call()`](https://rlang.r-lib.org/reference/stack.md)
  [`current_fn()`](https://rlang.r-lib.org/reference/stack.md)
  [`current_env()`](https://rlang.r-lib.org/reference/stack.md)
  [`caller_call()`](https://rlang.r-lib.org/reference/stack.md)
  [`caller_fn()`](https://rlang.r-lib.org/reference/stack.md)
  [`caller_env()`](https://rlang.r-lib.org/reference/stack.md)
  [`frame_call()`](https://rlang.r-lib.org/reference/stack.md)
  [`frame_fn()`](https://rlang.r-lib.org/reference/stack.md) : Get
  properties of the current or caller frame
- [`env_browse()`](https://rlang.r-lib.org/reference/env_browse.md)
  [`env_is_browsed()`](https://rlang.r-lib.org/reference/env_browse.md)
  **\[defunct\]** : Browse environments
- [`env_is_user_facing()`](https://rlang.r-lib.org/reference/env_is_user_facing.md)
  : Is frame environment user facing?

### Stack

- [`current_call()`](https://rlang.r-lib.org/reference/stack.md)
  [`current_fn()`](https://rlang.r-lib.org/reference/stack.md)
  [`current_env()`](https://rlang.r-lib.org/reference/stack.md)
  [`caller_call()`](https://rlang.r-lib.org/reference/stack.md)
  [`caller_fn()`](https://rlang.r-lib.org/reference/stack.md)
  [`caller_env()`](https://rlang.r-lib.org/reference/stack.md)
  [`frame_call()`](https://rlang.r-lib.org/reference/stack.md)
  [`frame_fn()`](https://rlang.r-lib.org/reference/stack.md) : Get
  properties of the current or caller frame

### Environment bindings

- [`env_bind()`](https://rlang.r-lib.org/reference/env_bind.md)
  [`env_bind_lazy()`](https://rlang.r-lib.org/reference/env_bind.md)
  [`env_bind_active()`](https://rlang.r-lib.org/reference/env_bind.md)
  [`` `%<~%` ``](https://rlang.r-lib.org/reference/env_bind.md) : Bind
  symbols to objects in an environment
- [`env_unbind()`](https://rlang.r-lib.org/reference/env_unbind.md) :
  Remove bindings from an environment
- [`env_poke()`](https://rlang.r-lib.org/reference/env_poke.md) : Poke
  an object in an environment
- [`env_cache()`](https://rlang.r-lib.org/reference/env_cache.md) :
  Cache a value in an environment
- [`local_bindings()`](https://rlang.r-lib.org/reference/local_bindings.md)
  [`with_bindings()`](https://rlang.r-lib.org/reference/local_bindings.md)
  : Temporarily change bindings of an environment
- [`env_has()`](https://rlang.r-lib.org/reference/env_has.md) : Does an
  environment have or see bindings?
- [`env_get()`](https://rlang.r-lib.org/reference/env_get.md)
  [`env_get_list()`](https://rlang.r-lib.org/reference/env_get.md) : Get
  an object in an environment
- [`env_names()`](https://rlang.r-lib.org/reference/env_names.md)
  [`env_length()`](https://rlang.r-lib.org/reference/env_names.md) :
  Names and numbers of symbols bound in an environment

### Functions

- [`new_function()`](https://rlang.r-lib.org/reference/new_function.md)
  : Create a function
- [`as_function()`](https://rlang.r-lib.org/reference/as_function.md)
  [`is_lambda()`](https://rlang.r-lib.org/reference/as_function.md) :
  Convert to function
- [`is_function()`](https://rlang.r-lib.org/reference/is_function.md)
  [`is_closure()`](https://rlang.r-lib.org/reference/is_function.md)
  [`is_primitive()`](https://rlang.r-lib.org/reference/is_function.md)
  [`is_primitive_eager()`](https://rlang.r-lib.org/reference/is_function.md)
  [`is_primitive_lazy()`](https://rlang.r-lib.org/reference/is_function.md)
  : Is object a function?
- [`fn_fmls()`](https://rlang.r-lib.org/reference/fn_fmls.md)
  [`fn_fmls_names()`](https://rlang.r-lib.org/reference/fn_fmls.md)
  [`fn_fmls_syms()`](https://rlang.r-lib.org/reference/fn_fmls.md)
  [`` `fn_fmls<-`() ``](https://rlang.r-lib.org/reference/fn_fmls.md)
  [`` `fn_fmls_names<-`() ``](https://rlang.r-lib.org/reference/fn_fmls.md)
  : Extract arguments from a function
- [`fn_body()`](https://rlang.r-lib.org/reference/fn_body.md)
  [`` `fn_body<-`() ``](https://rlang.r-lib.org/reference/fn_body.md) :
  Get or set function body
- [`fn_env()`](https://rlang.r-lib.org/reference/fn_env.md)
  [`` `fn_env<-`() ``](https://rlang.r-lib.org/reference/fn_env.md) :
  Return the closure environment of a function
- [`as_closure()`](https://rlang.r-lib.org/reference/as_closure.md) :
  Transform to a closure

### S3

- [`inherits_any()`](https://rlang.r-lib.org/reference/inherits_any.md)
  [`inherits_all()`](https://rlang.r-lib.org/reference/inherits_any.md)
  [`inherits_only()`](https://rlang.r-lib.org/reference/inherits_any.md)
  : Does an object inherit from a set of classes?
- [`zap()`](https://rlang.r-lib.org/reference/zap.md)
  [`is_zap()`](https://rlang.r-lib.org/reference/zap.md) : Create zap
  objects
- [`as_bytes()`](https://rlang.r-lib.org/reference/bytes-class.md)
  [`parse_bytes()`](https://rlang.r-lib.org/reference/bytes-class.md) :
  Human readable memory sizes
- [`new_box()`](https://rlang.r-lib.org/reference/box.md)
  [`is_box()`](https://rlang.r-lib.org/reference/box.md)
  [`unbox()`](https://rlang.r-lib.org/reference/box.md) : Box a value
- [`as_box()`](https://rlang.r-lib.org/reference/as_box.md)
  [`as_box_if()`](https://rlang.r-lib.org/reference/as_box.md) : Convert
  object to a box
- [`done()`](https://rlang.r-lib.org/reference/done.md)
  [`is_done_box()`](https://rlang.r-lib.org/reference/done.md) : Box a
  final value for early termination

### Attributes

- [`set_names()`](https://rlang.r-lib.org/reference/set_names.md) : Set
  names of a vector
- [`names2()`](https://rlang.r-lib.org/reference/names2.md)
  [`` `names2<-`() ``](https://rlang.r-lib.org/reference/names2.md) :
  Get names of a vector
- [`has_name()`](https://rlang.r-lib.org/reference/has_name.md) : Does
  an object have an element with this name?
- [`is_named()`](https://rlang.r-lib.org/reference/is_named.md)
  [`is_named2()`](https://rlang.r-lib.org/reference/is_named.md)
  [`have_name()`](https://rlang.r-lib.org/reference/is_named.md) : Is
  object named?
- [`zap_srcref()`](https://rlang.r-lib.org/reference/zap_srcref.md) :
  Zap source references

### Vectors

- [`lgl()`](https://rlang.r-lib.org/reference/vector-construction.md)
  [`int()`](https://rlang.r-lib.org/reference/vector-construction.md)
  [`dbl()`](https://rlang.r-lib.org/reference/vector-construction.md)
  [`cpl()`](https://rlang.r-lib.org/reference/vector-construction.md)
  [`chr()`](https://rlang.r-lib.org/reference/vector-construction.md)
  [`bytes()`](https://rlang.r-lib.org/reference/vector-construction.md)
  **\[questioning\]** : Create vectors
- [`list2()`](https://rlang.r-lib.org/reference/list2.md)
  [`dots_list()`](https://rlang.r-lib.org/reference/list2.md) : Collect
  dynamic dots in a list
- [`rep_along()`](https://rlang.r-lib.org/reference/rep_along.md)
  [`rep_named()`](https://rlang.r-lib.org/reference/rep_along.md) :
  Create vectors matching the length of a given vector
- [`seq2()`](https://rlang.r-lib.org/reference/seq2.md)
  [`seq2_along()`](https://rlang.r-lib.org/reference/seq2.md) :
  Increasing sequence of integers in an interval

### Type predicates

- [`is_list()`](https://rlang.r-lib.org/reference/type-predicates.md)
  [`is_atomic()`](https://rlang.r-lib.org/reference/type-predicates.md)
  [`is_vector()`](https://rlang.r-lib.org/reference/type-predicates.md)
  [`is_integer()`](https://rlang.r-lib.org/reference/type-predicates.md)
  [`is_double()`](https://rlang.r-lib.org/reference/type-predicates.md)
  [`is_complex()`](https://rlang.r-lib.org/reference/type-predicates.md)
  [`is_character()`](https://rlang.r-lib.org/reference/type-predicates.md)
  [`is_logical()`](https://rlang.r-lib.org/reference/type-predicates.md)
  [`is_raw()`](https://rlang.r-lib.org/reference/type-predicates.md)
  [`is_bytes()`](https://rlang.r-lib.org/reference/type-predicates.md)
  [`is_null()`](https://rlang.r-lib.org/reference/type-predicates.md) :
  Type predicates
- [`is_scalar_list()`](https://rlang.r-lib.org/reference/scalar-type-predicates.md)
  [`is_scalar_atomic()`](https://rlang.r-lib.org/reference/scalar-type-predicates.md)
  [`is_scalar_vector()`](https://rlang.r-lib.org/reference/scalar-type-predicates.md)
  [`is_scalar_integer()`](https://rlang.r-lib.org/reference/scalar-type-predicates.md)
  [`is_scalar_double()`](https://rlang.r-lib.org/reference/scalar-type-predicates.md)
  [`is_scalar_complex()`](https://rlang.r-lib.org/reference/scalar-type-predicates.md)
  [`is_scalar_character()`](https://rlang.r-lib.org/reference/scalar-type-predicates.md)
  [`is_scalar_logical()`](https://rlang.r-lib.org/reference/scalar-type-predicates.md)
  [`is_scalar_raw()`](https://rlang.r-lib.org/reference/scalar-type-predicates.md)
  [`is_string()`](https://rlang.r-lib.org/reference/scalar-type-predicates.md)
  [`is_scalar_bytes()`](https://rlang.r-lib.org/reference/scalar-type-predicates.md)
  [`is_bool()`](https://rlang.r-lib.org/reference/scalar-type-predicates.md)
  : Scalar type predicates
- [`is_bare_list()`](https://rlang.r-lib.org/reference/bare-type-predicates.md)
  [`is_bare_atomic()`](https://rlang.r-lib.org/reference/bare-type-predicates.md)
  [`is_bare_vector()`](https://rlang.r-lib.org/reference/bare-type-predicates.md)
  [`is_bare_double()`](https://rlang.r-lib.org/reference/bare-type-predicates.md)
  [`is_bare_complex()`](https://rlang.r-lib.org/reference/bare-type-predicates.md)
  [`is_bare_integer()`](https://rlang.r-lib.org/reference/bare-type-predicates.md)
  [`is_bare_numeric()`](https://rlang.r-lib.org/reference/bare-type-predicates.md)
  [`is_bare_character()`](https://rlang.r-lib.org/reference/bare-type-predicates.md)
  [`is_bare_logical()`](https://rlang.r-lib.org/reference/bare-type-predicates.md)
  [`is_bare_raw()`](https://rlang.r-lib.org/reference/bare-type-predicates.md)
  [`is_bare_string()`](https://rlang.r-lib.org/reference/bare-type-predicates.md)
  [`is_bare_bytes()`](https://rlang.r-lib.org/reference/bare-type-predicates.md)
  : Bare type predicates
- [`is_empty()`](https://rlang.r-lib.org/reference/is_empty.md) : Is
  object an empty vector or NULL?
- [`is_integerish()`](https://rlang.r-lib.org/reference/is_integerish.md)
  [`is_bare_integerish()`](https://rlang.r-lib.org/reference/is_integerish.md)
  [`is_scalar_integerish()`](https://rlang.r-lib.org/reference/is_integerish.md)
  : Is a vector integer-like?
- [`is_true()`](https://rlang.r-lib.org/reference/is_true.md)
  [`is_false()`](https://rlang.r-lib.org/reference/is_true.md) : Is
  object identical to TRUE or FALSE?

### Weak references

- [`is_weakref()`](https://rlang.r-lib.org/reference/is_weakref.md) : Is
  object a weak reference?
- [`new_weakref()`](https://rlang.r-lib.org/reference/new_weakref.md) :
  Create a weak reference
- [`wref_key()`](https://rlang.r-lib.org/reference/wref_key.md)
  [`wref_value()`](https://rlang.r-lib.org/reference/wref_key.md) : Get
  key/value from a weak reference object

### Operators

- [`` `%||%` ``](https://rlang.r-lib.org/reference/op-null-default.md) :

  Default value for `NULL`

- [`` `%&&%` ``](https://rlang.r-lib.org/reference/op-null-continuation.md)
  :

  Default value for non-`NULL`

- [`` `%|%` ``](https://rlang.r-lib.org/reference/op-na-default.md) :
  Replace missing values

- [`` `%@%` ``](https://rlang.r-lib.org/reference/op-get-attr.md)
  [`` `%@%<-`() ``](https://rlang.r-lib.org/reference/op-get-attr.md) :
  Infix attribute accessor and setter
