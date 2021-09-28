<!--
SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>

SPDX-License-Identifier: MPL-2.0
-->

# Changelog

## Unreleased

### Added

* Basic data types:
    * Microalgo amounts
    * Signing keys
    * Addresses
    * Block
    * Round
* Transaction types:
    * Payment transactions
    * Application call transactions
    * Asset transfer transactions
* Canonical MessagePack serialisation
* Signing and signature verification (only for simple single signatures)
* Transaction groups
* Node API client:
    * `/versions`
    * `/status`
    * `/v2/blocks/:round` (DEPRECATED: for compatibility purposes only)
    * `/v2/accounts/:address` (DEPRECATED: for compatibility purposes only)
    * `/v2/transactions` (POST)
    * `/v2/transactions` (POST) (raw)
    * `/v2/transactions/params`
    * `/v2/transactions/pending/:txid`
    * `/v2/teal/compile` (POST)
* Indexer API client:
    * `/health`
    * `/v2/accounts/:address?:round`
    * `/v2/blocks/:round`
* CLI:
    * `halgo account`
        * `new`
        * `show`
        * `export`
    * `halgo contract`
        * `compile`
        * `address`
    * `halgo node`
        * `host`
        * `version`
        * `status`
        * `fetch`
        * `send`
        * `txn-status`
    * `halgo indexer`
        * `host`
        * `version`
        * `status`
        * `fetch account`
        * `fetch block`
    * `halgo txn`
        * `show`
        * `show-unsigned`
        * `sign`
        * `lsign`
        * `id`
        * `new pay`
        * `new axfr`
        * `group`
