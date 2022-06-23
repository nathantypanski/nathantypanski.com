----
title: JWS is a nightmare
tags: JWT, JWS, evil
math: true
----

JWS is the kind of thing where you could be forgiven for thinking "hey, everyone's using this, so it must be good!" JWTs are built on JWS, and lots of people use those. JWS is standardized by the IETF in [RFC7515](https://datatracker.ietf.org/doc/html/rfc7515). I'm hoping that by the time you're done with this post, you'll do something else. If you just want to know what that "something else" is, skip to [the bottom][A better way to sign JSON].

In this post, I will show how the JWS standard encourages implementations to structure their validation logic in an insecure manner. Thus JWTs, being built upon a rotten foundation, are themselves insecure---or at least very difficult to implement securely. Then I will provide suggestions on alternatives to JWS.

Here's an example JWS from that RFC:

```json
{
 "payload":
  "eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ",
 "protected":"eyJhbGciOiJFUzI1NiJ9",
 "header": {"kid":"e9bc097a-ce51-4036-9562-d2ade882db0d"},
 "signature": "DtEhU3ljbEg8L38VWAfUAqOyKAM6-Xx-F4GawxaepmXFCgfTjDxw5djxLa8ISlSApmWQxfKTUJqPP3-Kg6NU1Q"
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

Moxie Marlinspike coined the [_cryptographic doom principle_](https://moxie.org/2011/12/13/the-cryptographic-doom-principle.html) in 2011 to refer to a pattern he had seen in implementations of message authentication codes (MACs). It generalizes the kinds of flaws found in [Vaudenay's famous 2002 paper](https://www.iacr.org/cryptodb/archive/2002/EUROCRYPT/2850/2850.pdf) and [SSH plaintext recovery](https://www.isg.rhul.ac.uk/~kp/SandPfinal.pdf) into a principle:

> If you have to perform any cryptographic operation before verifying the MAC on a message you’ve received, it will somehow inevitably lead to doom.

An extraordinarily common example of cryptographic doom is to calculate MACs based on plaintext in an encrypted message payload. To check the authenticity of a message, recipients must necessarily decrypt the message. In Vaudenay's attack, we exploit this construction to recover plaintext from encrypted messages, using only a single bit of information in servers' responses (padding error or MAC error).

In other words, when designing cryptographic protocols, we should **strive to authenticate data as early as possible.** Keep that in mind as you read the rest of this post.

## Verifying a JWS

Let's take another look at the example JWS object from earlier.

```json
{
 "payload":
  "eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ",
 "protected":"eyJhbGciOiJFUzI1NiJ9",
 "header": {"kid":"e9bc097a-ce51-4036-9562-d2ade882db0d"},
 "signature": "DtEhU3ljbEg8L38VWAfUAqOyKAM6-Xx-F4GawxaepmXFCgfTjDxw5djxLa8ISlSApmWQxfKTUJqPP3-Kg6NU1Q"
}
```

In order to verify this, we need to do the following:

1. `base64urldecode()` the `protected` attribute.
1. Deserialize the `protected` attribute into JSON.
1. Calculate the JOSE header as the union of the `protected` object and the JWS ("unprotected") object.
1. Verify that the implementation understands and can process the algorithm and any fields in the `crit` header.
1. `base64urldecode()` the `payload` attribute.
1. `base64urldecode()` the `signature` attribute.
1. Determine the verification algorithm to use by extracting it from the decoded `protected` header.
1. Construct the JWS signing input by concatenating encoded `protected` and `payload` headers together with a "." character.

   $$ \begin{aligned} & \texttt{ascii}(\texttt{base64urlencode}(\texttt{utf-8}(\texttt{protected}))) \\
    & \qquad || \quad ``." \\
    & \qquad || \quad \texttt{base64urldecode}(\texttt{payload}) \end{aligned} $$

   [comment]: $$ \texttt{ascii}(\texttt{base64urlencode}(\texttt{utf-8}(\texttt{protected}))) \quad || \quad ``." \quad || \quad \texttt{base64urldecode}(\texttt{payload}) $$
1. Using the algorithm from (8), verify the JWS.

Every step in this list prior to (9) is an opportunity for attackers to modify objects that are deserialized into JSON, control the algorithm in use by the system, and generally mess around where we expect contents to be integrity-protected.

Moreover, the fact that we are required to perform so many deserialization, decoding, and conditional algorithm selection operations prior to JWS verification invites implementations to make bad decisions about error handling. It might be tempting, to an implementor of JWS verification, to return "helpful" error messages about the validity of the JSON objects inside at step (5) before moving on to verification.


We should strive to introduce minimal transformations prior to signature verification. By requiring deserialization into JSON objects, the design invites implementations to perform early deserialization into application objects, potentially exposing themselves to [major deserialization bugs](https://github.com/advisories?query=cwe%3A502) with untrusted input data.

### Attacker-controlled algorithms

As [Auth0 discovered in late 2020](https://auth0.com/blog/critical-vulnerabilities-in-json-web-token-libraries/), allowing the attacker to control the algorithm used for signing affords them a number of options to bypass signature checks.

In the simplest case, setting the `algorithm` field to `none` and using an empty signature value (`""`) results in successful verification for any key.

The more complex attack requires the victim to use public key encryption algorithms and publish the public key. If the server supports `RS256` RSA signatures, attackers can encode the public key into PEM and use it as an HMAC key to sign a message with the `HS256` algorithm. Unless the library (or server) strictly enforces the algorithm to use for each key ID, it will pass validation.[^verification]

The solution to this is to distrust the client's provided algorithm. If we're verifying a client's JWS, then we probably already have their public key. From this key, we can derive the algorithm in use for the client (or store it, in our database, along with the key and `kid`). When we are presented with a new JWS to verify, we should use the algorithm specified for this key.

The final version of the JWS RFC now includes a section on [algorithm protection](https://datatracker.ietf.org/doc/html/rfc7515#section-10.7) addressing this issue, but it fails to be presecriptive about mitigations. According to the RFC, implementations can choose to

- only support algorithms that are not vulnerable to substitution attacks,
- require the `alg` header be carried in the JWS protected header,
- or include a field containing the algorithm in the application payload, and match it with the `alg` header.

Unfortunately, none of these mitigations include deriving the algorithm from the `kid` parameter, by far the most obviously correct mitigation.

#### PASETO

[PASETO](https://github.com/paseto-standard/paseto-spec) is an alternative JWT-like design that attempts to fix the algorithm selection bug. It uses protocol versions to specify algorithms, which is a generally-accepted good practice, and supports sound cryptographic algorithms like Ed25519. Unlike JWS, it does not support extensions by means of a `crit` parameter. In fact, it only allows two options:

1. `version`, indicating the ciphersuite to use.
2. `purpose`, where a value of `local` means symmetric-key encryption and `public `means public-key signatures.

Unfortunately, the PASETO design invites [remarkably similar bugs to JWS](https://mailarchive.ietf.org/arch/msg/cfrg/Yd85GHaPfkUYvCsWAdikByQsiLQ/), since attackers may control the protocol version instead of the algorithm directly. Now the PASETO spec includes [warnings to strongly type the algorithm in use](https://github.com/paseto-standard/paseto-spec/blob/master/docs/02-Implementation-Guide/03-Algorithm-Lucidity.md), just like JWS.

## Protocols should be defensive against implementations

I'm not a cryptographer, so when I'm forced to grapple with something involving cryptography, I like it to be so stupid simple that it's obviously correct. If I'm reviewing code that includes cryptography parts, I want it to be so obviously flawless that there's no debate---even among relative amateurs---about its correctness.

Primitives should be outsourced to sound, well-regarded libraries where possible. In fact, as much as we can should build upon a battle-tested, fuzzed, expertly-developed implementation.

When designing security protocols, we should be making this kind of dead-simple implementation as easy and obvious as possible for software. Now, I'm not talking about _primitives_ here, although certain modern primitives are [remarkably simple](https://ed25519.cr.yp.to/python/ed25519.py). I mean that the composition of primitives into a functioning security protocol should, to the extent possible, by extremely obvious.

A major mistake we see again and again in the first generation of internet security protocols is kitchen-sink design. SSL/TLS with its configurable ciphersuites, the Eldritch horror of PGP email signatures, and other early security standards are victims of this. SSL/TLS assumes that implementations can correctly handle things like ciphersuite negotiation. PGP is a [graveyard](https://blog.cryptographyengineering.com/2014/08/13/whats-matter-with-pgp/) of this type of [complexity](https://latacora.micro.blog/2019/07/16/the-pgp-problem.html). JWS, despite its marketing as "just signed JSON objects," is the polar opposite of obvious protocol. It exposes algorithm selection to the client, verification requires multiple steps of deserialization, encoding, and serialization, and it is far too extensible for its own good.

Modern tools are going the other way. [Minisign](https://jedisct1.github.io/minisign/), [OpenBSD Signify](https://www.openbsd.org/papers/bsdcan-signify.html) and [age](https://age-encryption.org/v1) are relentlessly simple, electing to do one thing well and no more.[^simple] Facebook [crypto auth tokens](https://eprint.iacr.org/2018/413.pdf) use a clever construction of nested MACs to support per-service auth tokens without explicit key sharing, but it's still simple enough that you can implement it by nesting library calls.

## A better way to sign JSON

Most mistakes in request signing protocols stem from serializing, parsing, and decoding the object [prior to verification](https://www.daemonology.net/blog/2008-12-18-AWS-signature-version-1-is-insecure.html). Don't do those steps. Most of the time it doesn't matter.

If you need unforgeability of messages by the server, then use Ed25519. This is unnecessary for most web APIs, but it is important in some cases, like [AWS multi-region access points](https://shufflesharding.com/posts/aws-sigv4-and-sigv4a). If you trust the server, then use HMAC-SHA256, a symmetric authentication algorithm. **Do not support both of these at once**. Think long and hard about what is appropriate for your application.

Have clients serialize the JSON object they wish to sign into a bytestring `json_bytes`. Now your signature algorithm becomes just:

$$ \texttt{sign}(\texttt{json_bytes},\ \texttt{key}).$$

And verification?

$$ \texttt{verify}(\texttt{json_bytes},\ \texttt{key}).$$

This is the method recommended by Latacora Security in [How (not) to sign a JSON object](https://latacora.micro.blog/2019/07/24/how-not-to.html).

Notice that verification does not require deserialization, nor character decoding! If the payload is a JSON object, we can safely deserialize it after verification. If you want, you can use [OpenBSD Signify](https://www.openbsd.org/papers/bsdcan-signify.html) so that clients have readily available CLI and library tooling support.

Now, this has the downside that two identical JSON objects may not result in identical signatures. Fortunately, this does not generally matter, and in any case it probably matters less than the correctness of your cryptographic system. If you need to check the equality of two requests, you can generally verify, deserialize, canonicalize and compare them inside your application server logic as necessary. Plus, this way when you find a bug in your canonicalization logic, you can fix it without forcing your clients to update.

[^cryptographic-doom]: [The Cryptographic Doom Principle](https://moxie.org/2011/12/13/the-cryptographic-doom-principle.html) - Moxie Marlinspike (2011)
[^vaudenay]: [Security Flaws Induced by CBC Padding
Applications to SSL, IPSEC, WTLS...](https://www.iacr.org/cryptodb/archive/2002/EUROCRYPT/2850/2850.pdf) - Serge Vaudenay (2002)
[^signature-formats]: [Signature Formats - _Envelopes and Wrappers and Formats, Oh My!_](https://dlorenc.medium.com/signature-formats-9b7b2a127473) - Dan Lorenc (2021)
[^critical-jwt-vulnerabilities]: [Critical vulnerabilities in JSON Web Token libraries](https://auth0.com/blog/critical-vulnerabilities-in-json-web-token-libraries/) - Tim McLean (2020)
[^ssh-plaintext]: [Plaintext Recovery Attacks Against SSH](https://www.isg.rhul.ac.uk/~kp/SandPfinal.pdf) - Martin R. Albrecht, Kenneth G. Paterson, Gaven J. Watson (2009)
[^signify]: [signify: Securing OpenBSD From Us To You](https://www.openbsd.org/papers/bsdcan-signify.html) - Ted Unangst (2015)
[^age]: [A simple file encryption tool & format](https://age-encryption.org/v1) Filippo Valsorda & Ben Carwright-Cox (2019)
[^pgp-green]: [What’s the matter with PGP?](https://blog.cryptographyengineering.com/2014/08/13/whats-matter-with-pgp/) - Matthew Green (2014)
[^pgp-latacora]: [The PGP Problem](https://latacora.micro.blog/2019/07/16/the-pgp-problem.html) - Latacora (2019)
[^verification]: The following code has two major problems. The first is that we are returning "helpful" error messages related to the application [prior to header verification][Cryptographic Doom], potentially leaking bits of information an attacker can use to exploit our application. The second is that we allow the "algorithm" field to be attacker-controlled, leaving us open to [algorithm selection attacks][Attacker-controlled algorithms].
    ```python
    def verify_jws(jws_string):
        jws = json.loads(string)

        # Decode protected.
        protected_decoded = base64urldecode(jws['protected'])

        # Deserialize protected into JSON.
        protected = json.loads(protected_decoded)

        # Calculate the JOSE header.
        jose = jws | protected

        # Verify that the implementation understands and can process any fields in
        # the "crit" header.
        for header in protected['crit']:
            if not header in supported_critical_headers:
                raise JWSError('Unsupported Header')

        # Verify expected headers are present
        if not 'exp' in protected['crit']:
            raise JWSError('expected "exp" header')
        if not 'nbf' in protected['crit']:
            raise JWSError('expected "exp" header')

        # Decode the "payload" attribute
        payload_decoded = base64urldecode(jws['payload'])
        payload = json.loads(payload_decoded)

        # Decode the "signature" attribute
        signature = base64urldecode(jws['signature'])

        # Reject invalid timestamps
        expiration = datetime.utcfromtimestamp(int(payload['exp']))
        not_before = datetime.utcfromtimestamp(int(payload['nbf']))
        now = datetime.now()
        if now > expiration:
            raise JWSError("JWS is expired")
        if not_before > now:
            raise JWSError("JWS is not valid yet")
        # Verify the application can handle the supplied request.
        # Raises UnsupportedAttributesError if invalid.
        validate_attributes(jose)

        algorithm = protected['algorithm']
        if not algorithm in supported_algorithms:
            msg = "{0} is unsupported".format(algorithm)
            raise JWSUnsupportedAlgorithmError(msg)

        # Calculate the signature payload.
        signature_payload = (base64url(protected_decoded.encode('utf-8'))
                            + '.' + base64url(payload_decoded)).encode('ascii')

        # Verify with the provided algorithm.
        return verify_payload(algorithm, signature_payload)
    ```
[^sigv1]: [AWS signature version 1 is insecure](https://www.daemonology.net/blog/2008-12-18-AWS-signature-version-1-is-insecure.html) - Colin Percival (2008)
[^sigv4a]: [AWS SIGv4 and SIGv4A](https://shufflesharding.com/posts/aws-sigv4-and-sigv4a) - Colm MacCárthaigh (2021)
[^simple]: Unfortunately, this simplicity can be to a fault. The new `age` encryption tool does not support public key authentication, meaning constructs such as
    ```bash
    $ curl "${ENCRYPTED_TARBALL_URL}" age --decrypt | tar xf
    ```
    are [dangerous](https://www.imperialviolet.org/2014/06/27/streamingencryption.html). But this problem can be resolved by using `signify` to sign the encrypted blobs, and always verifying data before decrypting it.
