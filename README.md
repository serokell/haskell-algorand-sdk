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
$ halgo node fetch HNVCPPGOW2SC2YVDVDICU3YNONSTEFLXDXREHJR2YBEKDC2Z3IUZSC6YGI | jq '.amount'
100000000
```


## Contributing

If you encounter any issues when using this library or have improvement ideas,
please open report in issue on GitHub. You are also very welcome to submit
pull request, if you feel like doing so.


## License

[MPL-2.0] © [Serokell]

[MPL-2.0]: https://spdx.org/licenses/MPL-2.0.html
[Serokell]: https://serokell.io/
