# hashes are stable across R versions

    Code
      hash(NULL)
    Output
      [1] "2c0a8a99dc147d5445c3b49d035665b2"
    Code
      hash(1)
    Output
      [1] "67fcd16b8893780e1dec0283befa759c"
    Code
      hash(1L)
    Output
      [1] "d87e277f141617e763489e62a5df3130"
    Code
      hash(TRUE)
    Output
      [1] "7c07a664e1048fe6cdeaac2d0f71aa78"
    Code
      hash("a")
    Output
      [1] "345dd027121de1e3ec3a99ef06844d61"
    Code
      hash(NA_real_)
    Output
      [1] "f058e14525c560c2d6425d07aea445c1"
    Code
      hash(NaN)
    Output
      [1] "9f8d823845e7696305e0e5e467938261"
    Code
      hash(NA_integer_)
    Output
      [1] "3b6f5296d2515c3663e2a9f769b64d64"
    Code
      hash(NA)
    Output
      [1] "9a28348da68377d7951482bd5343677e"
    Code
      hash(NA_character_)
    Output
      [1] "2dd6099f76a4ac4aacb0d4010f365eaf"
    Code
      hash(1:5 + 0L)
    Output
      [1] "8cda18256e98aaaac2fa333f5cb9fcaa"
    Code
      hash(raw(0))
    Output
      [1] "cf7153ecb1afcebfda56fcab5045ff5a"
    Code
      hash(list())
    Output
      [1] "b8610210c8823cd6748cd246b51f315d"
    Code
      hash(quote(x))
    Output
      [1] "a51d4d22068e93dbfcec8b32494faf07"
    Code
      hash(quote(foo))
    Output
      [1] "2bc7a92e46dcd34fbe09c3cb26ab31e8"

# hashes are stable across R versions with ALTREP objects

    Code
      hash(1:5)
    Output
      [1] "8cda18256e98aaaac2fa333f5cb9fcaa"

# different objects produce different hashes

    Code
      hash(1L)
    Output
      [1] "d87e277f141617e763489e62a5df3130"
    Code
      hash(2L)
    Output
      [1] "4f8b1048a979981e9f7de154f44e4214"
    Code
      hash("a")
    Output
      [1] "345dd027121de1e3ec3a99ef06844d61"
    Code
      hash("b")
    Output
      [1] "3e2c8d9e9fddb37f910c65d79eebec37"
    Code
      hash(1:3)
    Output
      [1] "0086983c3ad7643f058f25a9db6727ca"
    Code
      hash(4:6)
    Output
      [1] "66824ff6fcfe38e04308b685a5264382"
    Code
      hash(TRUE)
    Output
      [1] "7c07a664e1048fe6cdeaac2d0f71aa78"
    Code
      hash(FALSE)
    Output
      [1] "86c52b301deb0fab6b1457520d22b54d"
    Code
      hash(list(1))
    Output
      [1] "065a33d840b42793b55db7dcfeda2d71"
    Code
      hash(list(2))
    Output
      [1] "bab7a86c7158632d5a9726ab24619138"
    Code
      hash(quote(x))
    Output
      [1] "a51d4d22068e93dbfcec8b32494faf07"
    Code
      hash(quote(y))
    Output
      [1] "1e7d14e3617cf9ceeb43df8e9459b6d5"

