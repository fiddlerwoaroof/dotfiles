{
  config,
  pkgs,
  ...
}: {
  # Node exporter - collects hwmon/drm metrics including MI100 temps
  services.prometheus.exporters.node = {
    enable = true;
    port = 9100;
    openFirewall = true;
    enabledCollectors = ["hwmon" "drm" "systemd"];
  };

  # Prometheus - scrapes and stores metrics
  services.prometheus = {
    enable = true;
    scrapeConfigs = [
      {
        job_name = "node";
        scrape_interval = "10s";
        static_configs = [
          {
            targets = ["127.0.0.1:${toString config.services.prometheus.exporters.node.port}"];
            labels = {host = "srv2";};
          }
        ];
      }
      {
        job_name = "titan";
        scrape_interval = "10s";
        static_configs = [
          {
            targets = ["titan.h.elangley.org:9100"];
            labels = {host = "titan";};
          }
        ];
      }
    ];
  };

  # Grafana - dashboards
  services.grafana = {
    enable = true;
    settings.server = {
      http_addr = "0.0.0.0";
      http_port = 3000;
    };
    provision = {
      datasources.settings.datasources = [
        {
          name = "Prometheus";
          type = "prometheus";
          url = "http://localhost:${toString config.services.prometheus.port}";
          isDefault = true;
        }
      ];
      dashboards.settings = {
        apiVersion = 1;
        providers = [
          {
            name = "nixos-provisioned";
            options.path = "/etc/grafana-dashboards";
          }
        ];
      };
    };
  };

  environment.etc."grafana-dashboards/node-overview.json" = {
    source =
      pkgs.writeText "node-overview.json" (builtins.toJSON
        (import ./nix/srv2/overview-dashboard.nix).dashboard);
    user = "grafana";
    group = "grafana";
  };

  sops.secrets.grafana_password = {
    owner = "grafana";
    group = "grafana";
  };
}
