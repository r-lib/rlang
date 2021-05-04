#ifndef RLANG_INTERNAL_ENCODING_H
#define RLANG_INTERNAL_ENCODING_H


/*
 * Recursively normalise encodings of character vectors.
 *
 * A CHARSXP is considered normalised if:
 * - It is the NA_STRING
 * - It is ASCII, which means the encoding will be unmarked
 * - It is marked as UTF-8
 *
 * Attributes are normalised as well.
 *
 * ASCII strings will never get marked with an encoding when they go
 * through `Rf_mkCharLenCE()`, but they will get marked as ASCII. Since
 * UTF-8 is fully compatible with ASCII, they are treated like UTF-8.
 *
 * This converts vectors that are completely marked as Latin-1 to UTF-8, rather
 * than leaving them as Latin-1. This ensures that two vectors can be compared
 * consistently if they have both been normalised.
 *
 * Bytes-encoded vectors are not supported, as they cannot be
 * converted to UTF-8 by `Rf_translateCharUTF8()`.
 *
 * If `x` is not shared (i.e. `r_is_shared(x) == false`), this function will
 * modify `x` in place. Otherwise, a copy is made.
 */
r_obj* r_normalise_encoding(r_obj* x);


#endif
