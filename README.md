# bits-and-bobs

A Haskell library for working with binary data, inspired by Erlang's bit syntax.

### Motivation

Erlang has a feature called bit syntax which allows the user to encode and
decode data at the bit-level. Here's an example, where we encode three integers
into two bytes:

```erlang
1>​ Red = 2.
2
2>​ Green = 61.
61
3>​ Blue = 20.
20
4>​ Mem = <<Red:5, Green:6, Blue:5>>.
<<23,180>>
```

Normally, even the smallest integer takes up one byte (e.g. `char` in C or
`Int8` in Haskell) but Erlang's bit syntax lets us encode, e.g., `Red` using
only 5 bits (rather than the default 8 bits) and thus we can fit all three
integers in $5 + 6 + 5 = 16$ bits or two bytes.

We can also pattern match at the bit-level using sizes to get our integers back:

```erlang
5>​ <<R1:5, G1:6, B1:5>> = Mem.
<<23,180>>
6>​ R1.
2
7>​ G1.
61
8>​ B1.
20
```

For larger integer types, e.g. `0x12345678 :: Int32`, we can also specify the
byte order or [endianness](https://en.wikipedia.org/wiki/Endianness):

```erlang
1>​ {<<16#12345678:32/big>>,<<16#12345678:32/little>>,
​<<16#12345678:32/native>>,<<16#12345678:32>>}.
{<<18,52,86,120>>,<<120,86,52,18>>,
<<120,86,52,18>>,<<18,52,86,120>>}
```

For a slightly larger example, here's pattern-matching on an IP datagram of IP
protocol version 4:

```erlang
-define​(IP_VERSION, 4).
​-define​(IP_MIN_HDR_LEN, 5).

...
DgramSize = ​byte_size​(Dgram),
​case​ Dgram ​of
  <<?IP_VERSION:4, HLen:4, SrvcType:8, TotLen:16,
    ID:16, Flags:3, FragOff:13,
    TTL:8, Proto:8, HdrChkSum:16,
    SrcIP:32,
    DestIP:32, RestDgram/binary>> ​when​ HLen >= 5, 4*HLen =< DgramSize ->
        OptsLen = 4*(HLen - ?IP_MIN_HDR_LEN),
        <<Opts:OptsLen/binary,Data/binary>> = RestDgram,
        ...
```

Note how we can match on the header length, `HLen`, and later use the value of
that match as the size when pattern matching on later values.

### Usage

This library lets you do similar things to Erlang's bit syntax, but in a more
clunky way.

```haskell
import BitsAndBobs

let
  pattern0    = sized word32 5 ::: sized word32 6 ::: sized word32 5 ::: Nil
  bytestring0 = byteString [2 :. sized word32 5, 61 :. sized word32 6, 20 :. sized word32 5]
bitMatch pattern0 bytestring0
  -- => (2,(61,(20,())))

let
  pattern1    = word8 :>>= \sz -> sized bytes sz ::: bytes ::: Nil
  bytestring1 = byteString [5 :. word8, "hello, rest" :. bytes]
bitMatch pattern1 bytestring1
  -- => (5,("hello",(", rest",())))
```

### How it works

The high-level idea when encoding a bunch of, possibly sized, values into a
`ByteString` is as follows:

  1. For each value convert the value into a list of booleans (or bits);
  2. If the value is sized then only take that many bits, otherwise if it isn't
     sized use the default value, e.g. `Int8` = 8 bits, `Float` = 32 bits, etc;
  3. Concatenate the lists of booleans for each value into a single list of
     booleans;
  4. Split the list in groups of 8 bits;
  5. Convert each 8 bits into a byte (`Word8`);
  6. Create a `ByteString` from list of `Word8`s.

For decoding or pattern-matching a, possibly sized, pattern against a
`ByteString` the idea is:

  1. Convert `ByteString` into list of booleans (or bits);
  2. For each pattern take its size many bits from the list;
  3. Convert the bits into the value type of the pattern;
  4. Continue matching the remaining patterns against the remaining bits.

`Float` and `Double`s get converted into `Word32` and `Word64` respectively
before converted into bits, and `Int`egers are encoding using
[zigzag](https://en.wikipedia.org/wiki/Variable-length_quantity#Zigzag_encoding)
encoding.

### Contributing

There's a bunch of small things, that I know, are missing from Erlang:

  * Support for more types: `Word16`, `Word64`, `Int16`, `Int32`, `Int64`,
    `Double`, `BitString`, `Text`;
  * Error checking, e.g. user provided size cannot be bigger than the size of
    the type, or not enough bits to make a match;
  * Underscore patterns;
  * Unit type specifier.

A couple of things that could make it more convenient to use:

  * Return ADT instead of tuples when matching, perhaps using `Monad` or `Arrow`
    interface?
  * Generic encode/decode?
  * Quasi quoting?

There's a couple of things that can be optimised as well:

  * Apparently `Data.Vector.Unboxed Bool` isn't as tightly packed as they could
    be, see `bitvec` package;
  * Must be better way to go from bits to, say, `Word8` by e.g. by casting?
    Probably relies on, the above point that, the bits being packed right first.

And a couple of chores:

  * `haddock` perhaps with `doctest`?
  * CI
  * Test suite
      - Move existing tests;
      - Add property `forall seg. segToMatch seg == bitMatch (segToPattern seg)
        (byteString seg)`?

But the thing that really interests me is the question: how can we go beyond
what Erlang can do and make it even easier to work with binary data?

For example, the current interface makes it possible to decode binary data into
Haskell types, which can then be manipulated, and finally encoded back to
binary. But what if I merely want to update some binary in-place without reading
it all in and writing it all back out?

For example consider some on disk data structure, e.g. a B-tree. Or imagine a
concurrent lock-free queue implemented on top of an byte array, where we want to
atomically increment and fetch some position counter and then based on that
write the data at the right offset into the array.

How can we do zero-copy/zero-parse stuff? E.g. lets say we have a pre-allocated
byte array/pointer to `Word8` buffer, we could then use
[`recvBufFrom`](https://hackage.haskell.org/package/network-3.1.2.7/docs/Network-Socket.html#v:recvBufFrom)`
:: Socket -> Ptr a -> Int -> IO (Int, SockAddr)` to receive network traffic from
a socket into that buffer and thus avoiding copying and therefore allocating
memory. Imagine we use some protocol where we can inspect the first few bits to
find out which type of message it is, and based on the message type we know at
what offsets its fields are, could we then project those fields by merely
casting (i.e. avoid parsing)?

Another question would be if we can encode and pattern-match modulo (column)
compression?

I'm not sure if this repo is a good starting point for trying to answer these
questions, perhaps a completely different approach is required but it helped me
understand Erlang's approach and limitations a bit better at least.

If any of the above interests you, feel free to get in touch.

### See also

  * https://www.erlang.org/doc/reference_manual/expressions.html#bit_syntax;
  * https://www.erlang.org/doc/programming_examples/bit_syntax.html;
  * Joe Armstrong's PhD
    [thesis](http://kth.diva-portal.org/smash/record.jsf?pid=diva2%3A9492&dswid=-1166) (2003),
    p. 60;
  * https://hackage.haskell.org/package/binary-bits;
  * https://github.com/axman6/BitParser;
  * https://github.com/squaremo/bitsyntax-js;
  * https://capnproto.org/.

### License

See the [LICENSE](./LICENSE) file.
