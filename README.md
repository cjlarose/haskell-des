haskell-des
===========

This is an implementation of DES-like cipher described in *Introduction to
Cryptography* (Trappe, Washington) Second Edition, §4.2.

Compilation
-----------

To compile, use `cabal`.

    cabal configure
    cabal build

Usage
-----

The key is passed in on the command line as an integer. The least-significant
9 bits of the integer are used. `des` accepts a message from `stdin` and
outputs the result to `stdout`.

### Encryption

    ./des encrypt 119 < plaintext.in > ciphertext.out

### Decryption

    ./des decrypt 119 < ciphertext.in > plaintext.out
