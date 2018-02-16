Hi,

This is an automated email to let you know about that {{{ my_package }}}
has been submitted to CRAN today ({{{ date }}}). To check for
potential problems, I ran `R CMD check` on your package
{{{your_package}}} ({{{your_version}}}).

I found: {{{your_summary}}}.

{{#you_have_problems}}
{{{your_results}}}

If I got an ERROR because I couldn't install your package (or one of
it's dependencies), my apologies. You'll have to run the checks
yourself.

Otherwise, please carefully look at the results, and let me know if
I've introduced a bug in {{{ my_package }}}. If I have, I'd really
appreciate a minimal reproducible example that uses only {{{
my_package }}} functions. That way I can find and fix the bug as
quickly as possible.

If it doesn't look like a bug in {{{ my_package }}}, please prepare an
update for CRAN.

Until it gets to CRAN, you can get the development version of {{{
my_package }}} to run the checks yourself with:

    # install.packages("devtools")
    devtools::install_github("{{my_github}}")

To see what's changed visit <https://github.com/{{{my_github}}}/blob/master/NEWS.md>.

{{/you_have_problems}}
{{^you_have_problems}}
It looks like everything is ok, so you don't need to take any action,
but you might want to read the
NEWS, <https://github.com/{{{my_github}}}/blob/master/NEWS.md>, to see
what's changed.
{{/you_have_problems}}

If you have any questions about this email, please feel free to respond directly.

Regards,

{{{ me }}}
