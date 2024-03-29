# Algorand (`haskell-algorand-sdk`)

[![Build status](https://badge.buildkite.com/fee949a1cd2d80a51deb17fe28c2d8b367255344e870dcf87a.svg)](https://buildkite.com/serokell/haskell-algorand-sdk)

A Haskell SDK for the Algorand blockchain.

Tools for working with transactions on the Algorand blockchain and interacting
with algo's node and indexer through its REST API.


## Use

### As a library

* Add `algorand-sdk` as a dependency to your package.
* Follow the documentation and have a look at the example CLI app.

### As a CLI tool

* Install `algorand-sdk` using your favorite Haskell package manager.
* The binary will be called `halgo`; type `halgo --help` to see available commands.

#### Creating an account

Use `halgo account new <file>` to create a new account and save its key to the file:

```text
$ halgo account new ./example.acc
HNVCPPGOW2SC2YVDVDICU3YNONSTEFLXDXREHJR2YBEKDC2Z3IUZSC6YGI
```

It prints the address of your account. You can view the address anytime
using `halgo account show <file>`:

```text
$ halgo account show ./example.acc
HNVCPPGOW2SC2YVDVDICU3YNONSTEFLXDXREHJR2YBEKDC2Z3IUZSC6YGI
```

You can use the address to top-up your account (for example, using the [faucet]
on Testnet).

[faucet]: https://bank.testnet.algorand.network/

#### Talking to a node and indexer

`halgo` can talk to algo node and indexer using its REST API:
* The commands for talking to a node all start with `halgo node`.
* The commands for talking to an indexer all start with `halgo indexer`.

* Use `--help` to see all available commands.
* Use `--network` to choose what network (genesis ID) to connect to.
* If network is one of the well-known public ones (`mainnet-v1.0`,
  `testnet-v1.0`, `betanet-v1.0`), giving URL is optional as AlgoExplorer
  nodes will be used by default.
* `--network` is `testnet-v1.0` by default.

* Use `-n` or `--node-host` to give the URL of the API of the node.
* Use `-i` or `--indexer-host` to give the URL of the API of the indexer.

Use `halgo node host` to see the URL of the chosen node:

```text
$ halgo node host
https://api.testnet.algoexplorer.io/

$ halgo --network mainnet-v1.0 node host
https://api.algoexplorer.io/

$ halgo --network mainnet-v1.0 node --node-host https://example.com/ host
https://example.com/
```

Same for indexer (`halgo indexer host`)!


Use `halgo node version` and `halgo node status` to get basic information about
the node:

```text
$ halgo node version
"testnet-v1.0"

$ halgo node status
{
    "time-since-last-round": 2848631166,
    "catchpoint-verified-accounts": 0,
    "last-round": 14004250,
    "next-version": "https://github.com/algorandfoundation/specs/tree/d050b3cade6d5c664df8bd729bf219f179812595",
    "catchup-time": 0,
    "next-version-supported": true,
    "last-catchpoint": "14000000#T2ETVH3DU4OMNFGYBNHOB26SAPGK477VVHG4XXXA62YTFNLS7BHQ",
    "catchpoint-total-blocks": 0,
    "catchpoint": "",
    "catchpoint-total-accounts": 0,
    "catchpoint-processed-accounts": 0,
    "next-version-round": 14004251,
    "catchpoint-acquired-blocks": 0,
    "last-version": "https://github.com/algorandfoundation/specs/tree/d050b3cade6d5c664df8bd729bf219f179812595",
    "stopped-at-unsupported-round": false
}
```

Use `halgo indexer version` and `halgo indexer status` to get basic information
about the indexer:

```text
$ halgo indexer version
"testnet-v1.0"

$ halgo indexer status
{
    "round": 16937938,
    "db-available": true,
    "genesis-id": "testnet-v1.0",
    "is-migrating": false,
    "genesis-hash": "SGO1GKSzyE7IEPItTxCByw9x8FmnrCDexi9/cOUJOiI=",
    "version": "1.1.2",
    "message": "16937938"
}
```

#### Fetch block information

Use `halgo indexer fetch block --round N` to get information about block:

```text
$ halgo indexer fetch block --round 1
{
    "transactions-root": "LN9CHN+zm1OHdoF2SPpftJW9YI2J/F0Zv/FJsNdAheA=",
    "transactions": [
        {
            "signature": {
                "sig": "tlAccbsxpTazfxt3Yu69Li98QvH6nDAPNHdU8LUkpBLxu1umNePq0NwfHbrtv3hW4dxKQw32SbszeMrv5X64Cg=="
            },
            "confirmed-round": 1,
            "created-application-index": null,
            "intra-round-offset": 0,
            "group": null,
            "sender-rewards": 0,
            "genesis-id": null,
            "note": null,
            "receiver-rewards": 0,
            "sender": "GD64YIY3TWGDMCNPP553DZPPR6LDUSFQOIJVFDPPXWEG3FVOJCCDBBHU5A",
            "payment-transaction": {
                "amount": 100000000,
                "close-remainder-to": null,
                "receiver": "3NVE2MK2QYZQFOZ5XIRQTM7JRHNPUBV7QKLYLT7OO6QXFHXMRIAUXXNCBM"
            },
            "fee": 10000,
            "lease": null,
            "last-valid": 1000,
            "genesis-hash": null,
            "close-rewards": 0,
            "closing-amount": 0,
            "id": "QOOBRVQMX4HW5QZ2EGLQDQCQTKRF3UP3JKDGKYPCXMI6AVV35KQA",
            "created-asset-index": null,
            "rekey-to": null,
            "auth-addr": null,
            "tx-type": "pay",
            "first-valid": 0
        }
    ],
    "seed": "sBoIA83cUpa8jNOdqmGClw85y0bYcFhfmKzrnxmhgdc=",
    "round": 1,
    "genesis-id": "testnet-v1.0",
    "genesis-hash": "SGO1GKSzyE7IEPItTxCByw9x8FmnrCDexi9/cOUJOiI=",
    "previous-block-hash": "G8RmwKHb8iQahvMCvwJVZuI97pdN/dvSIkr7ZCI0AlM=",
    "txn-counter": 0,
    "timestamp": 1560210480
}
```

#### Check your balance

`halgo indexer fetch account` queries indexer for information about an account.
The output is formatted as JSON. Here is an easy way to check your balance:

```text
$ halgo indexer fetch account $(halgo account show ./example.acc) | jq '.account.amount'
100000000
```

#### Transactions

Let’s perform a simple payment transaction. Assuming you followed the steps above,
you have an account in `./example.acc` with some algos on it.

Let‘s create a second account:

```text
$ halgo account new ./another.acc
OFQHFQX6FMW4ELREN57QSH5U5LN63EGLZIFF7TEKIF5YWMNTPMXZ6BUSNE
```

Now let’s create a payment transaction sending 100k microalgos to this new account:

```text
$ halgo txn new pay $(halgo account show ./another.acc) 100000 > /tmp/transaction
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
    "type": "pay"
}
```

You might wonder where is a sender address. Well, when creating the transaction
we only specified the receiver’s address. The sender will get filled in once we
sign it. Let’s do just that:

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
$ TXN_ID=$(cat /tmp/transaction | halgo txn sign ./example.acc | halgo node send)
$ echo $TXN_ID
7MJLZW6SHUZOXVJOY2TEGHE4RQNOKDX5JB3HYWSDSKM75EUQD3SQ
```

The node returns the transaction ID, which we can now use to query the status:

```text
$ halgo node txn-status $TXN_ID
Confirmed in round 12717084
```

and even fetch the transaction from the node (as long as it remains in its pool):

```text
$ halgo node fetch $TXN_ID
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

#### Transaction groups

Let’s create two payment transactions:

```text
$ halgo txn new pay $(halgo account show ./another.acc) 100000 > /tmp/txs
$ halgo txn new pay $(halgo account show ./another.acc) 200000 >> /tmp/txs
```

We simply wrote two transactions into the same file.

Unfortunately, there is a small problem: `halgo txn new pay` does not fill in
the sender field of the transaction, since it is normally set later, when
signing. However, when making transactions into a group, they must be already
complete, so we will have to do a bit of manual surgery on these transactions
and set the sender field ourselves.

Luckily, it can be done relatively easily, since many `halgo` commands support JSON:

```text
$ cat /tmp/txs | halgo txn show-unsigned | jq '.[].snd = "'$(halgo account show ./example.acc)'"' > /tmp/txs.fixed
```

(Alternatively, you can just save the transactions as JSON into a file and edit
using your text editor.)

Now we call `halgo txn group` to calculate and set the group ID on each of
our transactions in the file (note the `--json` flag as by default it
expects input transactions encoded as base64):

```text
$ cat /tmp/txs.fixed | halgo txn group --json > /tmp/group
```

We can use `halgo txn show-unsigned` to make sure that the transactions now have
their `grp` field set to the same value:

```text
$ cat /tmp/group | halgo txn show-unsigned | jq '.[].grp'
"nTYG+OjVGy6xXHea1l59aSWE0PwkrziW8kgB88tcdQU="
"nTYG+OjVGy6xXHea1l59aSWE0PwkrziW8kgB88tcdQU="
```

`halgo txn group` also has the `--check` flag that makes it verify that the
transactions form a valid group (it will output the same group again, so we
silence it by redirecting output to `/dev/null`):

```text
$ cat /tmp/group | halgo txn group --check >/dev/null && echo OK
OK
```

Lastly, we can sign and send this group:

```text
$ cat /tmp/group | halgo txn sign ./example.acc | halgo node send
FWX2THG4UQHUB36M4WRSH6Z4YQ3C7W4DWR7VYEXKBAVYWK6XKCXQ
```

The `send` command prints the ID of the first transaction in the group,
but we can query the status of individual transactions:

```text
$ cat /tmp/group | halgo txn id | xargs -L1 halgo node txn-status
Confirmed in round 12899884
Confirmed in round 12899884
```

#### Contract accounts

`halgo contract compile` allows you to compile TEAL code to binary:

```text
$ echo "int 1" > /tmp/true.teal
$ halgo contract compile /tmp/true.teal
Writing compiled program to `/tmp/true.teal.tok`
6Z3C3LDVWGMX23BMSYMANACQOSINPFIRF77H7N3AWJZYV6OH6GWTJKVMXY
```

It prepends the `.tok` suffix to the source code file name and puts raw binary
data into that new file. It also prints the address of the account corresponding
to the contract with this code.

Once you have the compiled code, you can get its address again any time:

```text
$ halgo contract address /tmp/true.teal.tok
6Z3C3LDVWGMX23BMSYMANACQOSINPFIRF77H7N3AWJZYV6OH6GWTJKVMXY
```

If you are lucky, this account will have some spare algos on the testnet.
Let’s make it send some of its algos to you.

You can use `halgo txn lsign` to sign a transaction with a Logic Signature:

```text
$ halgo txn new pay $(halgo account show ./example.acc) 100 | halgo txn lsign /tmp/true.teal.tok | halgo node send
BVFGYILJKW2J5SWSVMZFZDEXWOLUUENDRW67WOQDDXUR7CUGQBBA
```

Ta-da!


## Contributing

If you encounter any issues when using this library or have improvement ideas,
please open report in issue on GitHub. You are also very welcome to submit
pull request, if you feel like doing so.

### Development

This SDK currently only supports a limited set of transaction types.
Adding new transaction types should be easy as defining all the data types
according to the specification and implementing serialization. Serialization
is just a bunch of boilerplate code that follows the same pattern for
all data types (it is absolutely possible to generate it using Generics
and, hopefully, this will happen in the future). See `Data.Algorand.Transaction`.

The definitions of the APIs are in `Network.Algorand.Api.*` modules and adding
missing endpoints should be pretty straightforward.


## License

[MPL-2.0] © [Serokell]

[MPL-2.0]: https://spdx.org/licenses/MPL-2.0.html
[Serokell]: https://serokell.io/
