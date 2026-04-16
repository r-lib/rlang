# hashes are stable across R versions

    Code
      hash(NULL)
    Output
      [1] "2a33816ed7e0c373dbe563c737220b65"
    Code
      hash(1)
    Output
      [1] "f7aceb844cccc35d4158f668ad10ca09"
    Code
      hash(1L)
    Output
      [1] "7e17de31f208f2c9a9bb7861c170adbd"
    Code
      hash(TRUE)
    Output
      [1] "92241473727f71264e1b2bc0ab92e12c"
    Code
      hash("a")
    Output
      [1] "a3c018b93e69786ebae85a55fc4a2029"
    Code
      hash(NA_real_)
    Output
      [1] "47dfb4b8f75ef3fae1c5d5abc333e8df"
    Code
      hash(NaN)
    Output
      [1] "aa9722e9cdbd0b3e80cc7c01f5e26f14"
    Code
      hash(NA_integer_)
    Output
      [1] "074c1ddffff8471b2d17d9166b5e5ee6"
    Code
      hash(NA)
    Output
      [1] "a7fa84d42c566e6a00eeec1d71f2e824"
    Code
      hash(NA_character_)
    Output
      [1] "ef335708a51c7e684cbbd3ff074bad09"
    Code
      hash(1:5 + 0L)
    Output
      [1] "79fd186742862fd67996efa5fb38ef6d"
    Code
      hash(raw(0))
    Output
      [1] "668003fc25bc87179a51419ed50a426f"
    Code
      hash(list())
    Output
      [1] "a98f8596453b1ea9e402d7cd427934d6"
    Code
      hash(quote(x))
    Output
      [1] "4f8b944f5b90e955b08ad6d5f8d5ae56"
    Code
      hash(quote(foo))
    Output
      [1] "6f5acbb9b2bcf91b3ce9a8b831135cad"

# different objects produce different hashes

    Code
      hash(1L)
    Output
      [1] "7e17de31f208f2c9a9bb7861c170adbd"
    Code
      hash(2L)
    Output
      [1] "ccc4347a94c126a7bddabe4882be4e3d"
    Code
      hash("a")
    Output
      [1] "a3c018b93e69786ebae85a55fc4a2029"
    Code
      hash("b")
    Output
      [1] "2bc4eeb6a72324a1cbee0bc7f78f88df"
    Code
      hash(1:3)
    Output
      [1] "bc412f636d3a636f009042be66a6b563"
    Code
      hash(4:6)
    Output
      [1] "69924214931d354e125c6fba2a4dc400"
    Code
      hash(TRUE)
    Output
      [1] "92241473727f71264e1b2bc0ab92e12c"
    Code
      hash(FALSE)
    Output
      [1] "e47c1f4209c62bb2f21d4037e060a220"
    Code
      hash(list(1))
    Output
      [1] "81213cc1212f9e180ddbb2961aca4580"
    Code
      hash(list(2))
    Output
      [1] "5615e667a689cae649ae1b7709b1f374"
    Code
      hash(quote(x))
    Output
      [1] "4f8b944f5b90e955b08ad6d5f8d5ae56"
    Code
      hash(quote(y))
    Output
      [1] "524be0a807e6bb2cbdff221c3fa48fc0"

