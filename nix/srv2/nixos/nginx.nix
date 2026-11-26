{pkgs-unstable, ...}: let
  lib = pkgs-unstable.lib;
  add_request_id = builtins.mapAttrs (_: v:
    v
    // {
      extraConfig = lib.concatLines [
        "proxy_set_header X-Request-ID $request_id;"
        "add_header X-Request-ID $request_id always;"
        (lib.defaultTo "" (v.extraConfig or null))
      ];
    });
in {
  services.nginx = {
    enable = true;
    package = pkgs-unstable.nginx.override {
      modules = [
        pkgs-unstable.nginxModules.fancyindex
      ];
    };
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
    appendHttpConfig = ''
      log_format structured_fwoar escape=json
      			    '{"remote_addr": "$remote_addr", "remote_user": "$remote_user", "local_time": "$time_local", '
                                  '"request": "$request", "status": "$status", "sent": "$body_bytes_sent", '
       '"http_host": "$http_host", "hostname": "$hostname", "server_name": "$server_name", '
      			    '"referer": "$http_referer", "user_agent": "$http_user_agent", "CF-Connecting-IP": "$http_CF_Connecting_IP", "X-Forwarded-For": "$http_x_forwarded_for", "request_id": "$request_id"}';
      access_log /var/log/nginx/access.log structured_fwoar;
    '';

    virtualHosts = {
      "book.fwoar.co" = {
        enableACME = true;
        forceSSL = true;
        root = "/home/edwlan/oldhome/Documents/ebooks";
        locations = add_request_id {
          "/" = {
            extraConfig = ''
              fancyindex on;
            '';
          };
          "/Philosophy_Theology/Journals" = {
            extraConfig = ''
              allow 172.16.0.0/16; deny all; '';
          };
        };
      };
      "fwoar.co" = {
        enableACME = true;
        forceSSL = true;
        root = "/home/edwlan/public_html";
        locations = add_request_id {
          "/" = {};
          "/hub/" = {
            proxyPass = "http://127.0.0.1:5000/";

            extraConfig = ''
              proxy_set_header X-Script-Name /hub/;
              proxy_pass_header Authorization;
            '';
          };
          "/radicale/" = {
            proxyPass = "http://127.0.0.1:5232/";

            extraConfig = ''
              proxy_set_header X-Script-Name /radicale;
              proxy_pass_header Authorization;
            '';
          };
        };
      };
      "srv2.elangley.org" = {
        enableACME = true;
        forceSSL = true;
        root = "/srv/srv2";
        locations = add_request_id {
          "/" = {};
          "/hub/" = {
            proxyPass = "http://127.0.0.1:5000/";

            extraConfig = ''
              proxy_set_header X-Script-Name /hub/;
              proxy_pass_header Authorization;
            '';
          };
        };
      };
      "summa.fwoar.co" = {
        enableACME = true;
        forceSSL = true;
        root = "/home/edwlan/public_html/summa-test";
        locations = add_request_id {
          "/" = {};
        };
      };
    };
  };
  security.acme = {
    defaults.email = "el-github@elangley.org";
    acceptTerms = true;
  };
  systemd.services.nginx.serviceConfig.ProtectHome = "read-only";
}
