#!/bin/bash

cardano-cli transaction build \
    --babbage-era \ 
    --change-address addr_test1vzq35kzf5ta90m90p5p6vatfxjmvyeguyrnjy64hefucp3clw3tz7 \
    --tx-in ddddd#0 \
    --tx-in-collateral ssadsad#1 \
    --tx-in-script-file "assets/gift.plutus" \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-file "assets/unit-datum.json" \
    --out-file "txs/collect-gift.unsigned"