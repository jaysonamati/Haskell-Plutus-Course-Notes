#!/bin/bash

cardano-cli transaction build \
    --change-address addr_test1vzq35kzf5ta90m90p5p6vatfxjmvyeguyrnjy64hefucp3clw3tz7 \
    --tx-in a7cba4737536f4eec8375dced735863cd5b3f9a2e19f7fed7fe16d06921d362d#0 \
    --tx-out "<script-addr> + 100000000 lovelace" \ 
    --tx-out-inline-datum-file "assets/unit-datum.json" \
    --out-file "txs/make-gift.unsigned"