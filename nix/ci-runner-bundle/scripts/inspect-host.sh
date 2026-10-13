#!/usr/bin/env bash
# Read-only diagnostics. Run as root on the deployed NixOS host.
set -euo pipefail
if [[ $EUID -ne 0 ]]; then
  echo 'Run this script with sudo on the NixOS host.' >&2
  exit 1
fi
systemctl --no-pager status nftables ci-mitm-ca ci-mitm-netns ci-mitmproxy || true
printf '\n=== dedicated guard table ===\n'
nft list table inet ci_egress
printf '\n=== proxy namespace ===\n'
ip -n ci-mitm -4 address show
ip -n ci-mitm -4 route show
printf '\n=== actual upstream forwarding route ===\n'
ip -4 route get 1.1.1.1 from 10.234.0.2 iif ci-mitm-host
printf '\n=== public CA ===\n'
openssl x509 -in /var/lib/ci-mitmproxy-public/ca.crt -noout -subject -dates -fingerprint -sha256
printf '\n=== audit files ===\n'
ls -lah /var/log/ci-mitmproxy/
printf '\n=== recent audit records ===\n'
tail -n 5 /var/log/ci-mitmproxy/audit.jsonl | jq .
