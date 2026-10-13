#!/usr/bin/env bash
# Run INSIDE a disposable runner. A refused connection alone does not prove the
# firewall enforced it; also inspect host counters and test known listening LAN services.
set -euo pipefail
proxy=${CI_PROXY_URL:-http://10.234.0.2:8080}
ca=${CI_CA_BUNDLE:-/run/ci-trust/ca-bundle.crt}
failures=0

blocked() {
  local description=$1
  shift
  if "$@" >/dev/null 2>&1; then
    printf 'FAIL: %s was reachable\n' "$description" >&2
    failures=$((failures + 1))
  else
    printf 'Blocked/unreachable: %s (confirm host counters)\n' "$description"
  fi
}

# --disable is the first curl option: ignore any ~/.curlrc configuration.
# Explicit --proxy/--noproxy overrides inherited proxy environment variables.
printf 'Checking proxied HTTPS with CA verification...\n'
if curl --disable --fail --silent --show-error --max-time 30 \
    --proxy "$proxy" --noproxy '' --cacert "$ca" https://api.github.com/ >/dev/null; then
  printf 'PASS: verified proxied HTTPS\n'
else
  printf 'FAIL: verified proxied HTTPS\n' >&2
  failures=$((failures + 1))
fi

# IP literals avoid mistaking disabled DNS for a direct-egress block.
blocked 'direct public TCP/80' python3 -c 'import socket; socket.create_connection(("1.1.1.1", 80), timeout=5).close()'
blocked 'direct public TCP/443' python3 -c 'import socket; socket.create_connection(("1.1.1.1", 443), timeout=5).close()'
blocked 'direct router TCP/80' python3 -c 'import socket; socket.create_connection(("172.16.1.1", 80), timeout=5).close()'
blocked 'router through proxy' curl --disable --fail --max-time 5 --proxy "$proxy" --noproxy '' http://172.16.1.1/
blocked 'metadata through proxy' curl --disable --fail --max-time 5 --proxy "$proxy" --noproxy '' http://169.254.169.254/
blocked 'host veth through proxy' curl --disable --fail --max-time 5 --proxy "$proxy" --noproxy '' http://10.233.0.1/
blocked 'non-web CONNECT port' curl --disable --fail --max-time 5 --proxy "$proxy" --noproxy '' --cacert "$ca" https://github.com:22/

if [[ -S /nix/var/nix/daemon-socket/socket ]]; then
  printf 'FAIL: host Nix daemon socket is visible\n' >&2
  failures=$((failures + 1))
else
  printf 'PASS: no host Nix daemon socket visible\n'
fi
if grep -q 'PRIVATE KEY' /run/ci-trust/proxy-ca.crt; then
  printf 'FAIL: distributed CA contains a private key\n' >&2
  failures=$((failures + 1))
else
  printf 'PASS: distributed CA has no PEM private-key block\n'
fi
printf 'Smoke checks complete: %s failures. Run the full deployment acceptance checklist.\n' "$failures"
[[ $failures -eq 0 ]]
