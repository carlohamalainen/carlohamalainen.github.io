---
author: Carlo Hamalainen
date: "2016-12-31T00:00:00Z"
guid: http://carlo-hamalainen.net/2016/12/31/subkeys-in-gpg-for-a-yubikey/
title: Subkeys in GPG for a YubiKey
url: /2016/12/31/subkeys-in-gpg-for-a-yubikey/
---

I recently got two [YubiKeys](https://www.yubico.com/) to try out another form of 2FA and to see how they work with my PGP setup (Enigmail and Thunderbird). I followed ankitrasto's guide ([part 1](https://ankitrasto.wordpress.com/2015/08/16/part-12-email-encryption-with-the-yubikey-neo-gpg-and-linux/) and [part 2](https://ankitrasto.wordpress.com/2015/10/20/part-22-email-encryption-with-the-yubikey-neo-gpg-and-linux/)) to move a key to the YubiKey.

I then exported my public key with ``gpg2 -a --export carlo@carlo-hamalainen.net`` and sent it to a friend. He replied with the reasonable question: why didn't the fingerprint ``E3E4A5B8`` change? The exported data changed (was longer) yet the fingerprint, which looks like a hash, was the same.

What's going on here is that originally I had a main key ``E3E4A5B8`` for signing (the "S" next to usage) and certification (the "C"). Meanwhile, encryption was done using a subkey ``81E07A3C`` (the "E").

    $ gpg2 --edit-key carlo@carlo-hamalainen.net
    gpg (GnuPG) 2.1.11; Copyright (C) 2016 Free Software Foundation, Inc.
    This is free software: you are free to change and redistribute it.
    There is NO WARRANTY, to the extent permitted by law.

    Secret key is available.

    sec  rsa4096/E3E4A5B8
         created: 2013-07-10  expires: never       usage: SC
         trust: ultimate      validity: ultimate
    ssb  rsa4096/81E07A3C
         created: 2013-07-10  expires: never       usage: E
    [ultimate] (1). Carlo Hamalainen

From my friend's perspective, I only had one "public key", the one with fingerprint ``E3E4A5B8``.

When I added two subkeys (one for each YubiKey), I got ``BE8897FA`` and ``766D56F8``. These entries have a ``card-no`` which refers to the serial number of the YubiKey where the subkey lives.

    $ gpg2 --edit-key carlo@carlo-hamalainen.net
    gpg (GnuPG) 2.1.11; Copyright (C) 2016 Free Software Foundation, Inc.
    This is free software: you are free to change and redistribute it.
    There is NO WARRANTY, to the extent permitted by law.

    Secret key is available.

    sec  rsa4096/E3E4A5B8
         created: 2013-07-10  expires: never       usage: SC
         trust: ultimate      validity: ultimate
    ssb  rsa4096/81E07A3C
         created: 2013-07-10  expires: never       usage: E
    ssb  rsa2048/BE8897FA
         created: 2016-11-20  expires: never       usage: E
         card-no: 0006 XXXXXXXX
    ssb  rsa2048/766D56F8
         created: 2016-12-13  expires: never       usage: E
         card-no: 0006 YYYYYYYY
    [ultimate] (1). Carlo Hamalainen

To see more detail about the keys, we have to inspect the packets in the export.

## Packet dump

We can use ``gpg2 --list-packets --verbose`` to see what is in the output of ``gpg2 -a --export``.

Here is the original packet dump when I had just the main key and the encryption subkey:

{{< mypre >}} <span style="color:green;">
$ gpg2 -a --export | gpg2 --list-packets --verbose
# off=0 ctb=99 tag=6 hlen=3 plen=525
:public key packet:
    version 4, algo 1, created 1373439395, expires 0
    pkey[0]: C35E579D722847E70515382572B28200 (very long line, snipped)
    pkey[1]: 010001
    keyid: 269E0DC4E3E4A5B8
# off=528 ctb=b4 tag=13 hlen=2 plen=45
:user ID packet: "Carlo Hamalainen "
# off=575 ctb=89 tag=2 hlen=3 plen=568
:signature packet: algo 1, keyid 269E0DC4E3E4A5B8
    version 4, created 1373439395, md5len 0, sigclass 0x13
    digest algo 2, begin of digest 05 44
    hashed subpkt 2 len 4 (sig created 2013-07-10)
    hashed subpkt 27 len 1 (key flags: 03)
    hashed subpkt 11 len 5 (pref-sym-algos: 9 8 7 3 2)
    hashed subpkt 21 len 5 (pref-hash-algos: 8 2 9 10 11)
    hashed subpkt 22 len 3 (pref-zip-algos: 2 3 1)
    hashed subpkt 30 len 1 (features: 01)
    hashed subpkt 23 len 1 (key server preferences: 80)
    subpkt 16 len 8 (issuer key ID 269E0DC4E3E4A5B8)
    data: REDACTED001</span><span style="color:purple;">
# off=1146 ctb=b9 tag=14 hlen=3 plen=525
:public sub key packet:
    version 4, algo 1, created 1373439395, expires 0
    pkey[0]: REDACTED002
    pkey[1]: 010001
    keyid: EF86D47281E07A3C
# off=1674 ctb=89 tag=2 hlen=3 plen=543
:signature packet: algo 1, keyid 269E0DC4E3E4A5B8
    version 4, created 1373439395, md5len 0, sigclass 0x18
    digest algo 2, begin of digest b9 63
    hashed subpkt 2 len 4 (sig created 2013-07-10)
    hashed subpkt 27 len 1 (key flags: 0C)
    subpkt 16 len 8 (issuer key ID 269E0DC4E3E4A5B8)
    data: REDACTED003 </span>
{{< /mypre >}}

With the two subkeys added for the YubiKeys, the dump now looks like this:

{{< mypre >}}
<span style="color:green;">
# off=0 ctb=99 tag=6 hlen=3 plen=525
:public key packet:
    version 4, algo 1, created 1373439395, expires 0
    pkey[0]: C35E579D722847E70515382572B28200 (very long line, snipped)
    pkey[1]: 010001
    keyid: 269E0DC4E3E4A5B8
# off=528 ctb=b4 tag=13 hlen=2 plen=45
:user ID packet: "Carlo Hamalainen "
# off=575 ctb=89 tag=2 hlen=3 plen=568
:signature packet: algo 1, keyid 269E0DC4E3E4A5B8
    version 4, created 1373439395, md5len 0, sigclass 0x13
    digest algo 2, begin of digest 05 44
    hashed subpkt 2 len 4 (sig created 2013-07-10)
    hashed subpkt 27 len 1 (key flags: 03)
    hashed subpkt 11 len 5 (pref-sym-algos: 9 8 7 3 2)
    hashed subpkt 21 len 5 (pref-hash-algos: 8 2 9 10 11)
    hashed subpkt 22 len 3 (pref-zip-algos: 2 3 1)
    hashed subpkt 30 len 1 (features: 01)
    hashed subpkt 23 len 1 (key server preferences: 80)
    subpkt 16 len 8 (issuer key ID 269E0DC4E3E4A5B8)
    data: REDACTED001<span style="color:purple;">
# off=1146 ctb=b9 tag=14 hlen=3 plen=525
:public sub key packet:
    version 4, algo 1, created 1373439395, expires 0
    pkey[0]: REDACTED002
    pkey[1]: 010001
    keyid: EF86D47281E07A3C
# off=1674 ctb=89 tag=2 hlen=3 plen=543
:signature packet: algo 1, keyid 269E0DC4E3E4A5B8
    version 4, created 1373439395, md5len 0, sigclass 0x18
    digest algo 2, begin of digest b9 63
    hashed subpkt 2 len 4 (sig created 2013-07-10)
    hashed subpkt 27 len 1 (key flags: 0C)
    subpkt 16 len 8 (issuer key ID 269E0DC4E3E4A5B8)
    data: REDACTED003<span style="color:blue;">
# off=2220 ctb=b9 tag=14 hlen=3 plen=269
:public sub key packet:
    version 4, algo 1, created 1479618635, expires 0
    pkey[0]: REDACTED004
    pkey[1]: 010001
    keyid: 6D682AD2BE8897FA
# off=2492 ctb=89 tag=2 hlen=3 plen=543
:signature packet: algo 1, keyid 269E0DC4E3E4A5B8
    version 4, created 1479618635, md5len 0, sigclass 0x18
    digest algo 8, begin of digest bc 2c
    hashed subpkt 2 len 4 (sig created 2016-11-20)
    hashed subpkt 27 len 1 (key flags: 0C)
    subpkt 16 len 8 (issuer key ID 269E0DC4E3E4A5B8)
    data: REDACTED005<span style="color:orange;">
# off=3038 ctb=b9 tag=14 hlen=3 plen=269
:public sub key packet:
    version 4, algo 1, created 1481611279, expires 0
    pkey[0]: REDACTED006
    pkey[1]: 010001
    keyid: 17118623766D56F8
# off=3310 ctb=89 tag=2 hlen=3 plen=543
:signature packet: algo 1, keyid 269E0DC4E3E4A5B8
    version 4, created 1481611279, md5len 0, sigclass 0x18
    digest algo 8, begin of digest a2 63
    hashed subpkt 2 len 4 (sig created 2016-12-13)
    hashed subpkt 27 len 1 (key flags: 0C)
    subpkt 16 len 8 (issuer key ID 269E0DC4E3E4A5B8)
    data: REDACTED007
{{< /mypre >}}

1. In both dumps, my main key is the same
(the {{< myspan color="green" >}} green block {{< /myspan >}}).
The fingerprint is ``269E0DC4E3E4A5B8`` in both dumps.

2. The {{< myspan color="purple" >}}next block{{< /myspan >}}
is the same in both dumps -- it is the encryption subkey. The signature packet is our way of proving that the subkey is attached to our main key. Note the line ``issuer key ID 269E0DC4E3E4A5B8``.

3.  {{< myspan color="blue" >}}This part{{< /myspan >}}
is for my first YubiKey subkey, ``BE8897FA``.  The signature packet claims that the subkey is issued by ``269E0DC4E3E4A5B8``.

4.  {{< myspan color="orange" >}}This part{{< /myspan >}}
is for my second YubiKey subkey, ``766D56F8``. Its signature packet also claims that the subkey is issued by ``269E0DC4E3E4A5B8``.

##  pgpdump

An alternative to ``gpg2 --list-packets --verbose`` is ``pgpdump``. This formats the packet dump in a nicer way, for example rendering the timestamp ``created 1373439395`` as ``Public key creation time - Wed Jul 10 14:56:35 SGT 2013``.

{{< mypre >}}
$ gpg2 --armor --export carlo@carlo-hamalainen.net | pgpdump
<span style="color:green;">Old: Public Key Packet(tag 6)(525 bytes)
    Ver 4 - new
    Public key creation time - Wed Jul 10 14:56:35 SGT 2013
    Pub alg - RSA Encrypt or Sign(pub 1)
    RSA n(4096 bits) - ...
    RSA e(17 bits) - ...
Old: User ID Packet(tag 13)(45 bytes)
    User ID - Carlo Hamalainen
Old: Signature Packet(tag 2)(568 bytes)
    Ver 4 - new
    Sig type - Positive certification of a User ID and Public Key packet(0x13).
    Pub alg - RSA Encrypt or Sign(pub 1)
    Hash alg - SHA1(hash 2)
    Hashed Sub: signature creation time(sub 2)(4 bytes)
        Time - Wed Jul 10 14:56:35 SGT 2013
    Hashed Sub: key flags(sub 27)(1 bytes)
        Flag - This key may be used to certify other keys
        Flag - This key may be used to sign data
    Hashed Sub: preferred symmetric algorithms(sub 11)(5 bytes)
        Sym alg - AES with 256-bit key(sym 9)
        Sym alg - AES with 192-bit key(sym 8)
        Sym alg - AES with 128-bit key(sym 7)
        Sym alg - CAST5(sym 3)
        Sym alg - Triple-DES(sym 2)
    Hashed Sub: preferred hash algorithms(sub 21)(5 bytes)
        Hash alg - SHA256(hash 8)
        Hash alg - SHA1(hash 2)
        Hash alg - SHA384(hash 9)
        Hash alg - SHA512(hash 10)
        Hash alg - SHA224(hash 11)
    Hashed Sub: preferred compression algorithms(sub 22)(3 bytes)
        Comp alg - ZLIB (comp 2)
        Comp alg - BZip2(comp 3)
        Comp alg - ZIP (comp 1)
    Hashed Sub: features(sub 30)(1 bytes)
        Flag - Modification detection (packets 18 and 19)
    Hashed Sub: key server preferences(sub 23)(1 bytes)
        Flag - No-modify
    Sub: issuer key ID(sub 16)(8 bytes)
        Key ID - 0x269E0DC4E3E4A5B8
    Hash left 2 bytes - 05 44
    RSA m^d mod n(4095 bits) - ...
        -> PKCS-1</span>
<span style="color:purple;">Old: Public Subkey Packet(tag 14)(525 bytes)
    Ver 4 - new
    Public key creation time - Wed Jul 10 14:56:35 SGT 2013
    Pub alg - RSA Encrypt or Sign(pub 1)
    RSA n(4096 bits) - ...
    RSA e(17 bits) - ...
Old: Signature Packet(tag 2)(543 bytes)
    Ver 4 - new
    Sig type - Subkey Binding Signature(0x18).
    Pub alg - RSA Encrypt or Sign(pub 1)
    Hash alg - SHA1(hash 2)
    Hashed Sub: signature creation time(sub 2)(4 bytes)
        Time - Wed Jul 10 14:56:35 SGT 2013
    Hashed Sub: key flags(sub 27)(1 bytes)
        Flag - This key may be used to encrypt communications
        Flag - This key may be used to encrypt storage
    Sub: issuer key ID(sub 16)(8 bytes)
        Key ID - 0x269E0DC4E3E4A5B8
    Hash left 2 bytes - b9 63
    RSA m^d mod n(4095 bits) - ...
        -> PKCS-1</span>
<span style="color:blue;">Old: Public Subkey Packet(tag 14)(269 bytes)
    Ver 4 - new
    Public key creation time - Sun Nov 20 13:10:35 SGT 2016
    Pub alg - RSA Encrypt or Sign(pub 1)
    RSA n(2048 bits) - ...
    RSA e(17 bits) - ...
Old: Signature Packet(tag 2)(543 bytes)
    Ver 4 - new
    Sig type - Subkey Binding Signature(0x18).
    Pub alg - RSA Encrypt or Sign(pub 1)
    Hash alg - SHA256(hash 8)
    Hashed Sub: signature creation time(sub 2)(4 bytes)
        Time - Sun Nov 20 13:10:35 SGT 2016
    Hashed Sub: key flags(sub 27)(1 bytes)
        Flag - This key may be used to encrypt communications
        Flag - This key may be used to encrypt storage
    Sub: issuer key ID(sub 16)(8 bytes)
        Key ID - 0x269E0DC4E3E4A5B8
    Hash left 2 bytes - bc 2c
    RSA m^d mod n(4096 bits) - ...
        -> PKCS-1</span>
<span style="color:orange;">Old: Public Subkey Packet(tag 14)(269 bytes)
    Ver 4 - new
    Public key creation time - Tue Dec 13 14:41:19 SGT 2016
    Pub alg - RSA Encrypt or Sign(pub 1)
    RSA n(2048 bits) - ...
    RSA e(17 bits) - ...
Old: Signature Packet(tag 2)(543 bytes)
    Ver 4 - new
    Sig type - Subkey Binding Signature(0x18).
    Pub alg - RSA Encrypt or Sign(pub 1)
    Hash alg - SHA256(hash 8)
    Hashed Sub: signature creation time(sub 2)(4 bytes)
        Time - Tue Dec 13 14:41:19 SGT 2016
    Hashed Sub: key flags(sub 27)(1 bytes)
        Flag - This key may be used to encrypt communications
        Flag - This key may be used to encrypt storage
    Sub: issuer key ID(sub 16)(8 bytes)
        Key ID - 0x269E0DC4E3E4A5B8
    Hash left 2 bytes - a2 63
    RSA m^d mod n(4093 bits) - ...
        -> PKCS-1</span>
{{< /mypre >}}

##  Manually checking the packets

I noticed that the gpg2 dump for the packet at offset ``3038`` refers to the key id ``17118623766D56F8`` but the equivalent block from pgpdump has no key id:


```
# off=3038 ctb=b9 tag=14 hlen=3 plen=269
:public sub key packet:
    version 4, algo 1, created 1481611279, expires 0
    pkey[0]: REDACTED006
    pkey[1]: 010001
    keyid: 17118623766D56F8
```

```
Old: Public Subkey Packet(tag 14)(269 bytes)
    Ver 4 - new
    Public key creation time - Tue Dec 13 14:41:19 SGT 2016
    Pub alg - RSA Encrypt or Sign(pub 1)
    RSA n(2048 bits) - ...
    RSA e(17 bits) - ...
```

The RFC tells us how to calculate the [fingerprint of a V4 packet](https://tools.ietf.org/html/rfc4880#section-12.2):

> A V4 fingerprint is the 160-bit SHA-1 hash of the octet 0x99, followed by the two-octet packet length, followed by the entire Public-Key packet starting with the version field.  The Key ID is the low-order 64 bits of the fingerprint.

My Python snippet to do this is [here](https://github.com/carlohamalainen/playground/blob/master/pgp/key_id/calculate_key_id.py#L91-L103).  A snippet is below:

```python
xs = read_binary_public_key()

SUBKEY_OFFSET = 3038
SUBKEY_PLEN   = 269

subkey_packet = xs[SUBKEY_OFFSET:SUBKEY_OFFSET+SUBKEY_PLEN+2] # +2 because the PLEN is short by one?

assert subkey_packet[2] == 'x04'

for_fingerprint = ['x99'] + subkey_packet

h = hashlib.sha1(to_str(for_fingerprint))

assert h.digest_size == 20
key_id = to_hexdump(h.digest()[12:], s='')

assert key_id == '17118623766D56F8'
```

## Subkey binding signature

The subkey packet at ``off=3038`` defines the subkey ``17118623766D56F8``. The next packet at ``off=3310`` provides proof that the key ``17118623766D56F8`` is attached to our main public key ``269E0DC4E3E4A5B8``.

The signature packet doesn't refer to the offset ``3038`` or the key id ``17118623766D56F8`` of the subkey packet, so let's check the contents of the signature packet to see if it really does match the subkey data.

{{< mypre >}}
# off=3038 ctb=b9 tag=14 hlen=3 plen=269
:public sub key packet:
    version 4, algo 1, created 1481611279, expires 0
    pkey[0]: REDACTED006
    pkey[1]: 010001
    keyid: 17118623766D56F8
# off=3310 ctb=89 tag=2 hlen=3 plen=543
:signature packet: algo 1, keyid 269E0DC4E3E4A5B8
    version 4, created 1481611279, md5len 0, sigclass 0x18
    <span style="color:red;">digest algo 8, begin of digest a2 63</span>
    hashed subpkt 2 len 4 (sig created 2016-12-13)
    hashed subpkt 27 len 1 (key flags: 0C)
    subpkt 16 len 8 (issuer key ID 269E0DC4E3E4A5B8)
    data: REDACTED007
{{< /mypre >}}

The first thing that we can check is that the left 16 bits of the hash matches ``A2 63`` (red line above). Checking this hash wasn't completely straightforward, just reading from [RFC](https://tools.ietf.org/html/rfc4880).  (Other people ran into [similar issues](http://crypto.stackexchange.com/questions/2734/openpgp-signature-packet-hashed-data).) The full block of code is [here](https://github.com/carlohamalainen/playground/blob/master/pgp/key_id/calculate_key_id.py#L129-L277). Sample is below:

```python
signature_block = xs[3310:3310+543+2]

# Starts off with two bytes for the length.
assert 543 == (ord(signature_block[0]) << 8) + ord(signature_block[1])

hash_subpacket_length = (ord(signature_block[6]) << 8) + ord(signature_block[7])
assert hash_subpacket_length == 9

start_of_hashed_part   = 8
start_of_unhashed_part = 7 + hash_subpacket_length + 1

unhash_subpacket_length = ( (ord(signature_block[start_of_unhashed_part]) << 8)
                          +  ord(signature_block[start_of_unhashed_part+1])
                          )

start_of_left_16 = start_of_unhashed_part + unhash_subpacket_length + 2

left_16 = signature_block[start_of_left_16:start_of_left_16+2]

assert left_16 == ['xA2', 'x63']

hashed_part_of_sig = signature_block[start_of_hashed_part:start_of_hashed_part+hash_subpacket_length]
assert len(hashed_part_of_sig) == hash_subpacket_length

public_key_block = xs[0:0+525+2] # +2 for what?
assert public_key_block[2] == 'x04'

header1     = ['x99'] + public_key_block[:8]
pubkey_body = public_key_block[8:]

header2     = ['x99'] + subkey_packet[:8]
subsig_body = subkey_packet[8:]

# Version, class, pub key algo, digest algo.
version_class_algos = [ 'x04', 'x18',
                        signature_block[4],
                        signature_block[5]
                      ]

m = hash_subpacket_length
assert m == 9
hash_chunk_length = [chr(m >> 8), chr(m)]

"""
According to https://tools.ietf.org/html/rfc4880#section-5.2.4
the final bit of data is a trailer of six octets:

   V4 signatures also hash in a final trailer of six octets: the
   version of the Signature packet, i.e., 0x04; 0xFF; and a four-octet,
   big-endian number that is the length of the hashed data from the
   Signature packet (note that this number does not include these final
   six octets).

But in gnupg-2.1.11, we see the following in g10/sig-check.c:

410     else {
411     byte buf[6];
412     size_t n;
413     gcry_md_putc( digest, sig->pubkey_algo );
414     gcry_md_putc( digest, sig->digest_algo );
415     if( sig->hashed ) {
416         n = sig->hashed->len;
417             gcry_md_putc (digest, (n >> 8) );
418             gcry_md_putc (digest,  n       );
419         gcry_md_write (digest, sig->hashed->data, n);
420         n += 6;
421     }
422     else {
423       /* Two octets for the (empty) length of the hashed
424              section. */
425           gcry_md_putc (digest, 0);
426       gcry_md_putc (digest, 0);
427       n = 6;
428     }
429     /* add some magic per Section 5.2.4 of RFC 4880.  */
430     buf[0] = sig->version;
431     buf[1] = 0xff;
432     buf[2] = n >> 24;
433     buf[3] = n >> 16;
434     buf[4] = n >>  8;
435     buf[5] = n;
436     gcry_md_write( digest, buf, 6 );
437     }
438     gcry_md_final( digest );

Line 420 adds 6, so we'll do the same even though it
seems to disagree with the RFC.

"""

n = m + 6
assert n == 15

magic = ['x04', 'xff',
         chr(n >> 24),
         chr(n >> 16),
         chr(n >>  8),
         chr(n)]

for_digest = []

for_digest += header1
for_digest += pubkey_body

for_digest += header2
for_digest += subsig_body

for_digest += version_class_algos

for_digest += hash_chunk_length
for_digest += hashed_part_of_sig

for_digest += magic

# According to https://tools.ietf.org/html/rfc4880#section-9.4
# the hash algo 8 is SHA256.
assert 'x08' == signature_block[5]
digest = hashlib.sha256(to_str(for_digest)).digest()

assert 'A2 63' == to_hexdump(digest[:2])
```

We find that the first 16 bits of the digest is ``A2 63``, matching the data in the signature packet, so the binding signature passes the first test.

The second test is to convert the digest to an [MPI](https://gnupg.org/documentation/manuals/gcrypt/MPI-formats.html#MPI-formats) and verify it using the specified public key algorithm (an exercise for the reader since [pygcrypt](https://pypi.python.org/pypi/pygcrypt) fails to install on my laptop 🙂

The RFC alone wasn't enough for me to reconstruct the subkey hash check, for example the ``+6`` quirk. I had to poke around in ``gpg 2.1.11`` to see what was being used in the hash. For efficiency reasons, libgcrypt lets you push single characters or blocks of bytes to the hash buffer (``gcry_md_putc`` and ``gcry_md_write``; see [the libgcrypt docs](https://gnupg.org/documentation/manuals/gcrypt/Working-with-hash-algorithms.html)) so you can't dump a contiguous block of memory to compare against ``for_digest``).  My hacky debugging (print statements) is in [this patch](https://github.com/carlohamalainen/playground/blob/master/pgp/key_id/debugging-against-gpg-2.1.11.patch). For some reason ``gpg2 --check-sigs 766D56F8!`` wasn't exercising the signature checking code (cached somewhere!?) so on [line 108](https://github.com/carlohamalainen/playground/blob/master/pgp/key_id/debugging-against-gpg-2.1.11.patch#L108) of my patch I had to force ``opt.no_sig_cache = 1;``.

## Enigmail and key fingerprints

So why doesn't Enigmail let you choose which subkey is being used for encryption? As far as I can tell this is by design:

[https://sourceforge.net/p/enigmail/forum/support/thread/37b7a5c8](https://sourceforge.net/p/enigmail/forum/support/thread/37b7a5c8)

> Enigmail does not try to expose all possible features to end users. The goal of Enigmail is to be usable for everyone, including beginners. I think that the concept of subkeys is way too complex even for many average users. Most of them are already confused by public and secret keys.
>
> You can use gpg.conf to configure the way you want to use subkeys. I will not implement specific features for this in Enigmail.

And then:

> Unfortunately, this doesn't work in this case. gpg is invoked by enigmail with the -u / --local-user argument, completely overriding my settings in gpg.conf. If you / enigmail invoked it with the --default-key argument, it would be a different story. But it does not.
> ...
> If you would change the next enigmail update to use the --default-key argument instead of the -u argument, it would really help.
>
> EDIT:
>
> Ok, patched enigmail myself. It works as expected with --default-key instead of -u.

And then:

> I have decided that I will not replace "-u" (or the equivalent "--local-user") by "--default-key" in Enigmail. Here is why:
>
> If a user specified local-user in gpg.conf, then use of "-u" in Enigmail will lead to the key being signed by both keys. This is what some users (especially companies) want, expect from Enigmail, and know that it's been supported for the last 10 years. Using --default-key will break this; in other words, gpg.conf will "win" over Enigmail.
>
> The requirement to a specific subkey for signing is by far less common, and average users don't need to do this.

We can see what's going on by using an old Unix trick: make a gpg2 "binary" that is a shell script and put it before the real gpg2 binary in the ``$PATH``:

```
$ cat bin/gpg2
#!/bin/bash

echo $@ >> /tmp/gpg2.txt

/usr/bin/gpg2 `echo $@ | sed 's/-u 0x7679121C22964C12888893D1269E0DC4E3E4A5B8/-u 766D56F8!/g'`
```

The ``-u 766D56F8!`` parameter forcibly chooses that subkey (the exclamation mark is needed).

{{< myspan color="red">}}
This trick is stupid, and potentially dangerous, since someone could convince you to
[sign a document with the encryption key instead of the signing key](http://security.stackexchange.com/questions/1806/why-should-one-not-use-the-same-asymmetric-key-for-encryption-as-they-do-for-sig). So don't do it! By default gpg2 uses the last encryption subkey for encryption.  {{< /myspan >}}

## Links

Source code for the snippets in this blog post: [https://github.com/carlohamalainen/playground/tree/master/pgp/key_id](https://github.com/carlohamalainen/playground/tree/master/pgp/key_id)

[Anatomy of a gpg key](https://davesteele.github.io/gpg/2014/09/20/anatomy-of-a-gpg-key)

[RFC 4880](https://tools.ietf.org/html/rfc4880)

[Part 1/2: Email Encryption with the Yubikey-NEO, GPG and Linux](https://ankitrasto.wordpress.com/2015/08/16/part-12-email-encryption-with-the-yubikey-neo-gpg-and-linux/)

[Part 2/2: Email Encryption with the Yubikey-NEO, GPG and Linux](https://ankitrasto.wordpress.com/2015/10/20/part-22-email-encryption-with-the-yubikey-neo-gpg-and-linux/)

[Libgcrypt Reference Manual](https://gnupg.org/documentation/manuals/gcrypt/)

[GNU Privacy Guard](https://gnupg.org/download/index.html)

[gnupg-2.1.11.tar.bz2](https://gnupg.org/ftp/gcrypt/gnupg/gnupg-2.1.11.tar.bz2) and [signature](https://gnupg.org/ftp/gcrypt/gnupg/gnupg-2.1.11.tar.bz2.sig)
