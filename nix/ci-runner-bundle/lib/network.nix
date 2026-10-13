{lib}: let
  octet = "(25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9]?[0-9])";
  ipv4Pattern = "${octet}(\\.${octet}){3}";
in {
  ipv4 = lib.types.strMatching ipv4Pattern;
  ipv4Network = lib.types.strMatching "${ipv4Pattern}/(3[0-2]|[12][0-9]|[0-9])";
  interface = lib.types.strMatching "[a-zA-Z0-9_.:-]{1,15}";
  # These strings are used by nspawn's flag parser: no whitespace or colons.
  runtimePath = lib.types.strMatching "/[a-zA-Z0-9_./-]+";
  elements = values: lib.concatStringsSep ", " values;
  quotedElements = values: lib.concatStringsSep ", " (map builtins.toJSON values);
  # Conservative IPv4 policy, not a dynamically maintained IANA registry.
  blockedNetworks = [
    "0.0.0.0/8"
    "10.0.0.0/8"
    "100.64.0.0/10"
    "127.0.0.0/8"
    "169.254.0.0/16"
    "172.16.0.0/12"
    "192.0.0.0/24"
    "192.0.2.0/24"
    "192.88.99.0/24"
    "192.168.0.0/16"
    "198.18.0.0/15"
    "198.51.100.0/24"
    "203.0.113.0/24"
    "224.0.0.0/4"
    "240.0.0.0/4"
  ];
}
