# Algorand (`haskell-algorand-sdk`)

A Haskell SDK for the Algorand blockchain.

Tools for working with transactions on the Algorand blockchain
and interacting with algod through its REST API.


## Use

### As a library

* Add `algorand-sdk` as a dependency to your packge.
* Follow the documentation and have a look at the example CLI app.

### As a CLI tool

* Install `algorand-sdk` using your favourite Haskell package manager.
* The binary will be called `halgo`.

#### Creating an account

Use `halgo acc new <file>` to create a new account and save its key to the file:

```text
$ halgo acc new ./example.acc
HNVCPPGOW2SC2YVDVDICU3YNONSTEFLXDXREHJR2YBEKDC2Z3IUZSC6YGI
```

It prints the address of your account. You can view the address anytime
using `halgo acc show <file>`:

```text
$ halgo acc show ./example.acc
HNVCPPGOW2SC2YVDVDICU3YNONSTEFLXDXREHJR2YBEKDC2Z3IUZSC6YGI
```

You can use the address to top-up your account
(for example, using the [faucet] on Testnet).

[faucet]: https://bank.testnet.algorand.network/

#### Talking to a node

`halgo` can talk to `algod` using its REST API. The commands for talking
to a node all start with `halgo node`.

* Use `--network` to choose what network (genesis ID) to connect to.
* Use `--url` to give the URL of the API of the node.
* If network is one of the well-known public ones (`mainnet-v1.0`,
  `testnet-v1.0`, `betanet-v1.0`), giving URL is optional as AlgoExplorer
  nodes will be used by default.
*  `--network` is `testnet-v1.0` by default.

Use `algo node url` to see the URL of the chosen node:

```text
$ halgo node url
https://api.testnet.algoexplorer.io/

$ halgo --network mainnet-v1.0 node url
https://api.algoexplorer.io/

$ halgo --network mainnet-v1.0 node --url https://example.com/ url
https://example.com/
```

Use `halgo node version` to get basic information about the node:

```text
$ halgo node version
{
    "genesis_hash_b64": "SGO1GKSzyE7IEPItTxCByw9x8FmnrCDexi9/cOUJOiI=",
    "build": {
        "minor": 4,
        "channel": "stable",
        "major": 2,
        "commit_hash": "573a34c4",
        "branch": "rel/stable",
        "build_number": 1
    },
    "versions": [
        "v1",
        "v2"
    ],
    "genesis_id": "testnet-v1.0"
}
```

#### Check your balance

`halgo node fetch` queries node for information about an account. The output
is formatted as JSON. Here is an easy way to check your balance:

```text
$ halgo node fetch acc HNVCPPGOW2SC2YVDVDICU3YNONSTEFLXDXREHJR2YBEKDC2Z3IUZSC6YGI | jq '.amount'
100000000
```

#### Transactions

Let’s perform a simple payment transaction. Assuming you followed the steps above,
you have an account in `./example.acc` with some algos on it.

Let‘s create a second account:

```text
$ halgo acc new ./another.acc
OFQHFQX6FMW4ELREN57QSH5U5LN63EGLZIFF7TEKIF5YWMNTPMXZ6BUSNE
```

Now let’s create a payment transaction sending 100k microalgos to this new account:

```text
$ halgo txn new pay $(halgo acc show ./another.acc) 100000 > /tmp/transaction
```

We saved the transaction to a file so we can look into it and see that it is
simply a bunch of JSON:

```text
$ cat /tmp/transaction | halgo txn show-unsigned
{
    "gh": "SGO1GKSzyE7IEPItTxCByw9x8FmnrCDexi9/cOUJOiI=",
    "lv": 12718072,
    "amt": 100000,
    "fv": 12717072,
    "fee": 1000,
    "gen": "testnet-v1.0",
    "rcv": "OFQHFQX6FMW4ELREN57QSH5U5LN63EGLZIFF7TEKIF5YWMNTPMXZ6BUSNE",
    "type": "pay",
    "snd": "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAY5HFKQ"
}
```

You might wonder what’s up with the sender address. Well, when creating the
transaction we only specified the receiver’s address. The sender will get
filled in once we sign it. Let’s do just that:

```text
$ cat /tmp/transaction | halgo txn sign ./example.acc
gqNzaWfEQB8XAgzMdY/bUFBhaInAm9ZezXKpx6VsG94ZUwEoKCawGbB326hGKj/FmMgGqiF+/aNWwwfpmmCfsX+TxVAkiQSjdHhuiaNhbXTOAAGGoKNmZWXNA+iiZnbOAMIMEKNnZW6sdGVzdG5ldC12MS4womdoxCBIY7UYpLPITsgQ8i1PEIHLD3HwWaesIN7GL39w5Qk6IqJsds4Awg/4o3JjdsQgcWBywv4rLcIuJG9/CR+06tvtkMvKCl/MikF7izGzey+jc25kxCA7aie8zrakLWKjqNAqbw1zZTIVdx3iQ6Y6wEihi1naKaR0eXBlo3BheQ==
```

A lot of base64! However, we can ask `halgo` to decode this madness for us
(and it will even verify the signature):

```text
$ cat /tmp/transaction | halgo txn sign ./example.acc | halgo txn show
{
    "txn": {
        "gh": "SGO1GKSzyE7IEPItTxCByw9x8FmnrCDexi9/cOUJOiI=",
        "lv": 12718072,
        "amt": 100000,
        "fv": 12717072,
        "fee": 1000,
        "gen": "testnet-v1.0",
        "rcv": "OFQHFQX6FMW4ELREN57QSH5U5LN63EGLZIFF7TEKIF5YWMNTPMXZ6BUSNE",
        "type": "pay",
        "snd": "HNVCPPGOW2SC2YVDVDICU3YNONSTEFLXDXREHJR2YBEKDC2Z3IUZSC6YGI"
    },
    "sig": "HxcCDMx1j9tQUGFoicCb1l7NcqnHpWwb3hlTASgoJrAZsHfbqEYqP8WYyAaqIX79o1bDB+maYJ+xf5PFUCSJBA=="
}
```

Enough of looking at intermediate results, let’s submit it to the node:

```text
$ cat /tmp/transaction | halgo txn sign ./example.acc | halgo node send
7MJLZW6SHUZOXVJOY2TEGHE4RQNOKDX5JB3HYWSDSKM75EUQD3SQ
```

The node returns the transaction ID, which we can now use to query the status:

```text
$ halgo node txn-status 7MJLZW6SHUZOXVJOY2TEGHE4RQNOKDX5JB3HYWSDSKM75EUQD3SQ
Confirmed in round 12717084
```

and even fetch the transaction from the node (as long as it remains in its pool):

```text
$ halgo node fetch txn 7MJLZW6SHUZOXVJOY2TEGHE4RQNOKDX5JB3HYWSDSKM75EUQD3SQ
{
    "txn": {
        "gh": "SGO1GKSzyE7IEPItTxCByw9x8FmnrCDexi9/cOUJOiI=",
        "lv": 12718072,
        "amt": 100000,
        "fv": 12717072,
        "fee": 1000,
        "gen": "testnet-v1.0",
        "rcv": "OFQHFQX6FMW4ELREN57QSH5U5LN63EGLZIFF7TEKIF5YWMNTPMXZ6BUSNE",
        "type": "pay",
        "snd": "HNVCPPGOW2SC2YVDVDICU3YNONSTEFLXDXREHJR2YBEKDC2Z3IUZSC6YGI"
    },
    "sig": "HxcCDMx1j9tQUGFoicCb1l7NcqnHpWwb3hlTASgoJrAZsHfbqEYqP8WYyAaqIX79o1bDB+maYJ+xf5PFUCSJBA=="
}
```


## Contributing

If you encounter any issues when using this library or have improvement ideas,
please open report in issue on GitHub. You are also very welcome to submit
pull request, if you feel like doing so.

### Development

This SDK currently only supports a limited set of transaction types.
Adding new transaction types should be easy as defining all the datatypes
according to the specification and implementing serialisation. Serialisation
is just a bunch of boilerplate code that follows the same pattern for
all data types (it is absolutely possible to generate it using Generics
and, hopefully, this will happen in the future). See `Data.Algorand.Transaction`.

The definition of the node API is in `Network.Algorand.Node.Api` and adding
missing endpoints should be pretty straightforward.


## License

[MPL-2.0] © [Serokell]

[MPL-2.0]: https://spdx.org/licenses/MPL-2.0.html
[Serokell]: https://serokell.io/
