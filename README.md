# pkcs7

[![Build Status](https://travis-ci.org/kisom/pkcs7.svg?branch=master)](https://travis-ci.org/kisom/pkcs7)

This implements the PKCS #7 padding scheme. This scheme is
defined in [RFC 5652, section 6.3](http://tools.ietf.org/html/rfc5652#section-6.3).

Strings will be padded out to multiples of the block size; for example,
a 5-byte string with an 8-byte block size will have three bytes of padding
added. If the length is already a multiple of the block size, an entire
block size worth of padding is added.

The padding bytes are all set to the number of padding bytes. Returning
to the previous example, the padding string would be three bytes of
the byte 0x03.

Unpadding checks the length and padding, and if this is valid, strips off
the padding.

The following functions are defined for `String`s:

+ `pad`: apply PKCS #7 padding assuming an AES block size
+ `padN`: apply PKCS #7 padding, specifying the block size
+ `unpad`: remove PKCS #7 padding assuming an AES block size
+ `unpadN`: remove PKCS #7 padding, specifying the block size

The following functions are defined for `ByteString`s:

+ `padBytes`: apply PKCS #7 padding assuming an AES block size
+ `padBytesN`: apply PKCS #7 padding, specifying the block size
+ `unpadBytes`: remove PKCS #7 padding assuming an AES block size
+ `unpadBytesN`: remove PKCS #7 padding, specifying the block size

The unpadding functions return a `Maybe a` (where a is either a `String`
or `ByteString`, depending on the function). If there was a padding error,
`Nothing` is returned. Otherwise, `Just` the string is returned.

