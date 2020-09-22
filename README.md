# Main top-level repository

Pre-requisists: *Install GNU's Multiple Precision Arithmetic Library (GMP)*

```
$ git clone git@github.com:obscrete/main.git
$ git clone git@github.com:obscrete/mpa.git
$ git clone git@github.com:obscrete/elgamal.git
$ cd main
$ make
$ export ERL_LIBS=..
$ erl -pa ../elgamal/test
Erlang/OTP 22 [erts-10.7] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [hipe]

Eshell V10.7  (abort with ^G)
1> unit_test_elgamal:start().
Randomized ciphertext size: 1325
Randomized ciphertext size: 1837
Randomized ciphertext size: 2348
Randomized ciphertext size: 2859
Randomized ciphertext size: 3370
Randomized ciphertext size: 3881
Randomized ciphertext size: 4392
Randomized ciphertext size: 4904
Randomized ciphertext size: 5416
Randomized ciphertext size: 5928
ok
2> 
```
