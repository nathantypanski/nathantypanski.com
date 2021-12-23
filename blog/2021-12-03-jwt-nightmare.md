----
title: JWSs & JWTs are a nightmare
tags: JWT, JWS, evil
----

I had the pleasure of reviewing some code last month that implements the JWS signature algorithms. JWS is the kind of thing where you could be forgiven for thinking "hey, everyone's using this, so it must be good!" JWTs are built on JWS, and lots of people use those. There's an [IETF draft RFC](https://datatracker.ietf.org/doc/html/draft-ietf-jose-json-web-signature) for JWS in the works that seems to be gaining traction.

Here's an example JWS from that RFC:

```json
{
 "payload":
  "eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGF
   tcGxlLmNvbS9pc19yb290Ijp0cnVlfQ",
 "protected":"eyJhbGciOiJFUzI1NiJ9",
 "header":
  {"kid":"e9bc097a-ce51-4036-9562-d2ade882db0d"},
 "signature":
  "DtEhU3ljbEg8L38VWAfUAqOyKAM6-Xx-F4GawxaepmXFCgfTjDxw5djxLa8IS
   lSApmWQxfKTUJqPP3-Kg6NU1Q"
}
```

The payload and `protected` fields are base64-encoded without padding. The `payload` looks like this:

```json
{"iss":"joe",
 "exp":1300819380,
 "http://example.com/is_root":true}
```

And here's the `protected` field:

```json
{"alg":"ES256"}
```

ES256 is the ECDSA P-256 SHA-256 digital signature algorithm.

## Cryptographic doom

Moxie Marlinspike coined the _cryptographic doom principle_[^cryptographic-doom] in 2011 to refer to a pattern he had seen in implementations of message authentication codes (MACs).

[^cryptographic-doom]: [The Cryptographic Doom Principle](https://moxie.org/2011/12/13/the-cryptographic-doom-principle.html) - Moxie Marlinspike (2011)
[^signature-formats]: [Envelopes and Wrappers and Formats, Oh My!](https://dlorenc.medium.com/signature-formats-9b7b2a127473) - Dan Lorenc (2021)
[^critical-jwt-vulnerabilities]: [Critical vulnerabilities in JSON Web Token libraries](https://auth0.com/blog/critical-vulnerabilities-in-json-web-token-libraries/) - Tim McLean (2020)

