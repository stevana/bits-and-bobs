# bits-and-bobs

A library for working with binary data, inspired by Erlang's bit syntax.

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

### Extending Erlang's bit syntax

Erlang's bit syntax makes it possible to decode binary data into the host
languague's types, which can then be manipulated, and finally encoded back to
binary.

While already useful, it doesn't cover some interesting use cases. Let me try to
explain the use cases and at the same time sketch possible ways we can extend
Erlang's bit syntax to cover those.

#### In-place updates

What if we merely want to update some binary in-place without reading it all in
and writing it all back out?

For example, the de facto standard for metadata format for mp3 files is called
[ID3](https://en.wikipedia.org/wiki/ID3). This was never part of the mp3
specification, but rather added afterwards and so in order to not break
backwards-compatibility with old media players they added it at the end of the
file.

Lets imagine we wanted to write a metadata editor for mp3 files using Erlang's
bit syntax. I think no matter how smart the Erlang run-time is about bit syntax,
it's hard to imagine that it wouldn't need to deserialse and serialise more data
than necessary. Worst case it would deserialise all of the audio that leads up
to where the metadata starts, but even if it's somehow clever and starts from
the back then we'd still probably need to at least deserialise all fields
preceding the field we want to update.

Inspired by this problem and how tools like [`poke`](https://jemarch.net/poke)
work, I've started another experiment based on `Schema`s with this use case in
mind, here's an example session of editing the metadata of an mp3 file:

```
$ cabal run mp3 -- /tmp/test.mp3

mp3> help
schema | read <field> | write <field> <value> | list | q(uit)

mp3> schema
audio   : Binary
header  : Magic "TAG"
title   : ByteString (Fixed 30)
artist  : ByteString (Fixed 30)
album   : ByteString (Fixed 30)
year    : ByteString (Fixed 4)
comment : ByteString (Fixed 30)
genre   : UInt8

mp3> read title
Unknown

mp3> write title "Bits and Bobs"

mp3> read title
Bits and Bobs

mp3> list
Right (Id3V1 {title = "Bits and Bobs", artist = "", album = "", year = "2023", comment = ""})
mp3> quit
```

The user needs to specify the `Schema`, which is closely mapped to the ID3v1
specficiation and the rest is provided by the library. In particular all the
offsets to the different fields are calculated from the schema, which allow us
to jump straight to the field of interest without parsing. The above interactive
[editor](app/Main.hs) is completely [generic](src/BitsAndBobs/Editor.hs) and
works for any `Schema`!

#### On-disk data structures

Now that we can edit files in-place on the disk it would be nice to use this in
order to implement on-disk data structures. For example imagine we'd like to do
some kind of logging. If our schemas could express arrays and records we could
define our log to be an a struct with a length field and an array of records
field that of size length. In addition to extending the schema with arrays and
records, we'd also need atomic increments of the length field so that we can in
a thread-safe manner allocate space in our array. B-trees would be another
interesting on-disk data structure to implement.

The generic editor would be useful for debugging and manipulating such data
structures, but we'd probably want more tooling. For logging we probably want
something like `cat` and `grep` but generic in `Schema`.

#### Zero-copy

When we `read` a `ByteString` field in the mp3 metadata example above, we copied
the bytes from the underlying file. Sometimes we might want to avoid doing that.

For example imagine we are implementing some network protocol. We can use a
pre-allocated buffer and [`recv`](https://linux.die.net/man/2/recv) bytes from a
socket into this buffer (avoiding allocating memory while handling requests),
once the request is inside our buffer we can decode individual fields (without
parsing) and from that we can determine what kind of request it is. Let's
imagine it's some kind of write request where we want to save the payload of
some field to disk. It would be a waste to copy the bytestring of the payload
only to write it disk immediately after, since the network request consists of
raw bytes and that's what we want to write to the disk anyway. Instead we'd like
to be able to decode the payload field as a pointer/slice of the buffer which we
pass to [`write`](https://linux.die.net/man/2/write) (thus avoiding copying aka
"zero-copy").

#### Backward- and forward-compatiability and migrations

Another big topic is schema evolution. How can we maintain backward- and
forward-compatibility as our software evolves? We probably want to be able to
migrate old formats into newer ones somehow also.

#### Compression

Currently our schemas cannot express how to compress fields on disk, or how to
avoid sending unnecessary data in consecutive network messages.

An example of the former might be to compress a bytestring field, using say
[deflate](https://en.wikipedia.org/wiki/Deflate), before writing it to disk.
While an example of the former might be to only send the difference or change of
some integer field, instead of sending the whole integer again. To make things
more concrete, lets say the integer represents epoch time and we send messages
several times per second, then by only sending the difference or
[delta](https://en.wikipedia.org/wiki/Delta_encoding) in time since the last
message we can save space. Other examples of compression include
[dictionary](https://en.wikipedia.org/wiki/Dictionary_coder) compression,
[run-length encoding](https://en.wikipedia.org/wiki/Run-length_encoding), [bit
packing](https://en.wikipedia.org/wiki/Apache_Parquet#Bit_packing) and [Huffman
coding](https://en.wikipedia.org/wiki/Huffman_coding).

It would be neat if encoding and decoding fields could be done modulo
compression! Likewise the schema-based `cat` and `grep` could also work modulo
compression.

A related topic is storing our data in a row-based or columnar fashion. Take the
example of a logging library we discussed earlier with a schema that's an array
of records, i.e. each log call adds a new record to the array. This is nice in
terms of writing efficiency, but if we wanted to do a lot of grepping or some
aggregation on some field in the record then we'd have to jump around a lot in
the file (jumping over the other fields that we are not interested in). It could
be more efficient to restructure our data into a record of arrays stead, where
each array only has data from one field, that way searching or aggregating over
that field would be much more efficient (no jumping around). Some compression is
also a lot easier to apply on columnar data, i.e. delta and run-length encoding.
Perhaps it would make sense if the schema-based tools could do such data
transformations in order to optimise for reads or archiving?

#### Pandoc for binary formats?

* Humanly readable

* `curl | bnb` and `bnb | curl`?

* Convert from and to protobufs or avro?

* Not yet another interface description language
  ([IDL](https://en.wikipedia.org/wiki/Interface_description_language)), but
  rather a DSL for IDLs?

### Contributing

The current implementation is in Haskell, but I'd really like to encourage a
discussion beyond specific languages. For something like this to succeed I'd
imagine we'd need libraries implemented in many languages.

There's a bunch of small things that are missing from the original port of the
Erlang's bit syntax:

  * Support for more types: `Word16`, `Word64`, `Int16`, `Int32`, `Int64`,
    `Double`, `BitString`, `Text`;
  * Error checking, e.g. user provided size cannot be bigger than the size of
    the type, or not enough bits to make a match;
  * Underscore patterns;
  * Unit type specifier;
  * Testing the property `forall seg. segToMatch seg == bitMatch (segToPattern seg)
    (byteString seg)`?

There's a couple of things that can be optimised as well:

  * Apparently `Data.Vector.Unboxed Bool` isn't as tightly packed as they could
    be, see `bitvec` package;
  * Must be better way to go from bits to, say, `Word8` by e.g. casting?
    Probably relies on, the above point that, the bits being packed right first.

Regarding the newer `Schema`-based approach the first thing we'd probably want
to do is to merge it with the old approach, in particular allow bit-level sizes
in `Schema`s while also fixing all the above mentioned things. Then, or in
parallel, it would be nice to work out how to neatly solve all the mentioned use
cases.

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
  * https://capnproto.org/;
  * GNU [poke](https://jemarch.net/poke), extensible editor for structured
    binary data;
  * https://github.com/wader/fq;
  * *Designing Data-Intensive Applications* by Martin Kleppmann (chapter 3-4,
    2017);
  * *Development and Deployment of Multiplayer Online Games, Vol. I* by Sergey
     Ignatchenko (pp. 200-216 and 259-285, 2017).

### License

See the [LICENSE](./LICENSE) file.
