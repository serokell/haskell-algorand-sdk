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
* Transaction types:
    * Payment transactions
    * Application call transactions
    * Asset transfer transactions
* Canonical MessagePack serialisation
* Signing and signature verification (only for simple single signatures)
* Transaction groups
* REST API client:
    * `/health`
    * `/versions`
    * `/v2/accounts/:address`
    * `/v2/transactions/params`
    * `/v2/transactions` (POST)
    * `/v2/transactions` (POST) (raw)
    * `/v2/transactions/pending/:txid`
* CLI:
    * `halgo acc`
        * `new`
        * `show`
        * `export`
    * `halgo node`
        * `url`
        * `version`
        * `fetch acc`
        * `fetch txn
        * `send`
        * `txn-status`
    * `halgo txn`
        * `show`
        * `show-unsigned`
        * `sign`
        * `id`
        * `new pay`
        * `new axfr`
        * `group`
