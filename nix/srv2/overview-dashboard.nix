let
  datasource = {
    type = "prometheus";
    uid = "PBFA97CFB590B2093";
  };
  mkPanel = {
    title,
    expr,
    gridPos,
    type ? "timeseries",
    unit ? null,
    legendMode ? "list",
    overrides ? [],
    fieldConfig ? {},
    options ? {},
  }: {
    inherit title type;
    inherit gridPos;
    datasource = datasource;
    targets =
      if builtins.isList expr
      then
        builtins.genList (i: {
          inherit datasource;
          expr = builtins.elemAt (builtins.map (e: e.expr) expr) i;
          legendFormat = builtins.elemAt (builtins.map (e: e.legend) expr) i;
          refId = builtins.substring 0 1 (builtins.toString (65 + i));
        }) (builtins.length expr)
      else [
        {
          inherit datasource expr;
          legendFormat = "{{instance}}";
          refId = "A";
        }
      ];
    fieldConfig =
      {
        defaults =
          {
            unit =
              if unit != null
              then unit
              else "";
          }
          // (fieldConfig.defaults or {});
        overrides = overrides;
      }
      // (builtins.removeAttrs fieldConfig ["defaults"]);
    options =
      {
        legend.displayMode = legendMode;
      }
      // options;
  };
  mkStat = {
    title,
    expr,
    gridPos,
    unit ? null,
    fieldConfig ? {},
  }:
    mkPanel {
      inherit title expr gridPos unit fieldConfig;
      type = "stat";
      legendMode = "hidden";
      options = {reduceOptions.calcs = ["lastNotNull"];};
    };
  mkRow = {
    title,
    gridPos,
  }: {
    inherit title gridPos;
    type = "row";
    collapsed = false;
    panels = [];
  };
in {
  dashboard = {
    title = "Node Overview";
    uid = "node-overview";
    editable = true;
    timezone = "browser";
    refresh = "30s";
    time = {
      from = "now-1h";
      to = "now";
    };
    templating.list = [
      {
        name = "instance";
        type = "query";
        datasource = datasource;
        query = "label_values(up, instance)";
        refresh = 2;
        includeAll = true;
        current = {
          selected = true;
          text = "All";
          value = "$__all";
        };
      }
    ];
    panels = [
      # Row 1 - Overview
      (mkRow {
        title = "Overview";
        gridPos = {
          x = 0;
          y = 0;
          w = 24;
          h = 1;
        };
      })
      (mkStat {
        title = "Uptime";
        expr = "time() - node_boot_time_seconds";
        gridPos = {
          x = 0;
          y = 1;
          w = 6;
          h = 4;
        };
        unit = "s";
      })
      (mkStat {
        title = "CPU Cores";
        expr = "count without(cpu, mode) (node_cpu_seconds_total{mode=\"idle\"})";
        gridPos = {
          x = 6;
          y = 1;
          w = 6;
          h = 4;
        };
      })
      (mkStat {
        title = "Total RAM";
        expr = "node_memory_MemTotal_bytes";
        gridPos = {
          x = 12;
          y = 1;
          w = 6;
          h = 4;
        };
        unit = "bytes";
      })
      (mkStat {
        title = "ZFS ARC Hit Ratio";
        expr = "rate(node_zfs_arc_hits[5m]) / (rate(node_zfs_arc_hits[5m]) + rate(node_zfs_arc_misses[5m])) * 100";
        gridPos = {
          x = 18;
          y = 1;
          w = 6;
          h = 4;
        };
        unit = "percent";
      })

      # Row 2 - CPU
      (mkRow {
        title = "CPU";
        gridPos = {
          x = 0;
          y = 5;
          w = 24;
          h = 1;
        };
      })
      (mkPanel {
        title = "CPU Usage %";
        expr = "100 - (avg by(instance)(rate(node_cpu_seconds_total{mode=\"idle\"}[5m])) * 100)";
        gridPos = {
          x = 0;
          y = 6;
          w = 8;
          h = 8;
        };
        unit = "percent";
      })
      (mkPanel {
        title = "CPU by Mode";
        gridPos = {
          x = 8;
          y = 6;
          w = 8;
          h = 8;
        };
        unit = "percent";
        expr = [
          {
            expr = "avg by(mode)(rate(node_cpu_seconds_total{mode!=\"idle\"}[5m])) * 100";
            legend = "{{mode}}";
          }
        ];
        fieldConfig = {defaults = {custom.stacking.mode = "normal";};};
      })
      (mkPanel {
        title = "Load Average";
        gridPos = {
          x = 16;
          y = 6;
          w = 8;
          h = 8;
        };
        expr = [
          {
            expr = "node_load1";
            legend = "1m";
          }
          {
            expr = "node_load5";
            legend = "5m";
          }
          {
            expr = "node_load15";
            legend = "15m";
          }
        ];
      })

      # Row 3 - Memory
      (mkRow {
        title = "Memory";
        gridPos = {
          x = 0;
          y = 14;
          w = 24;
          h = 1;
        };
      })
      (mkPanel {
        title = "Memory Used %";
        expr = "(1 - node_memory_MemAvailable_bytes / node_memory_MemTotal_bytes) * 100";
        gridPos = {
          x = 0;
          y = 15;
          w = 8;
          h = 8;
        };
        unit = "percent";
      })
      (mkPanel {
        title = "Memory Breakdown";
        gridPos = {
          x = 8;
          y = 15;
          w = 8;
          h = 8;
        };
        unit = "bytes";
        expr = [
          {
            expr = "node_memory_MemTotal_bytes - node_memory_MemFree_bytes - node_memory_Buffers_bytes - node_memory_Cached_bytes";
            legend = "Used";
          }
          {
            expr = "node_memory_Buffers_bytes";
            legend = "Buffers";
          }
          {
            expr = "node_memory_Cached_bytes";
            legend = "Cached";
          }
        ];
        fieldConfig = {defaults = {custom.stacking.mode = "normal";};};
      })
      (mkPanel {
        title = "Swap Used";
        expr = "node_memory_SwapTotal_bytes - node_memory_SwapFree_bytes";
        gridPos = {
          x = 16;
          y = 15;
          w = 8;
          h = 8;
        };
        unit = "bytes";
      })

      # Row 4 - ZFS
      (mkRow {
        title = "ZFS";
        gridPos = {
          x = 0;
          y = 23;
          w = 24;
          h = 1;
        };
      })
      (mkPanel {
        title = "ARC Size";
        gridPos = {
          x = 0;
          y = 24;
          w = 8;
          h = 8;
        };
        unit = "bytes";
        expr = [
          {
            expr = "node_zfs_arc_size";
            legend = "ARC Size";
          }
          {
            expr = "node_zfs_arc_c";
            legend = "ARC Target";
          }
        ];
      })
      (mkPanel {
        title = "ARC Hit Rate %";
        expr = "rate(node_zfs_arc_hits[5m]) / (rate(node_zfs_arc_hits[5m]) + rate(node_zfs_arc_misses[5m])) * 100";
        gridPos = {
          x = 8;
          y = 24;
          w = 8;
          h = 8;
        };
        unit = "percent";
      })
      (mkPanel {
        title = "ZFS Filesystem Usage %";
        expr = "(1 - node_filesystem_avail_bytes{fstype=\"zfs\"} / node_filesystem_size_bytes{fstype=\"zfs\"}) * 100";
        gridPos = {
          x = 16;
          y = 24;
          w = 8;
          h = 8;
        };
        unit = "percent";
        legendMode = "table";
      })

      # Row 5 - Disk I/O
      (mkRow {
        title = "Disk I/O";
        gridPos = {
          x = 0;
          y = 32;
          w = 24;
          h = 1;
        };
      })
      (mkPanel {
        title = "Disk Throughput Read";
        expr = "rate(node_disk_read_bytes_total{device=~\"nvme.*\"}[5m])";
        gridPos = {
          x = 0;
          y = 33;
          w = 8;
          h = 8;
        };
        unit = "Bps";
        legendMode = "table";
      })
      (mkPanel {
        title = "Disk Throughput Write";
        expr = "rate(node_disk_written_bytes_total{device=~\"nvme.*\"}[5m])";
        gridPos = {
          x = 8;
          y = 33;
          w = 8;
          h = 8;
        };
        unit = "Bps";
        legendMode = "table";
      })
      (mkPanel {
        title = "Disk I/O Latency";
        gridPos = {
          x = 16;
          y = 33;
          w = 8;
          h = 8;
        };
        unit = "s";
        expr = [
          {
            expr = "rate(node_disk_read_time_seconds_total{device=~\"nvme.*\"}[5m]) / rate(node_disk_reads_completed_total{device=~\"nvme.*\"}[5m])";
            legend = "read {{device}}";
          }
          {
            expr = "rate(node_disk_write_time_seconds_total{device=~\"nvme.*\"}[5m]) / rate(node_disk_writes_completed_total{device=~\"nvme.*\"}[5m])";
            legend = "write {{device}}";
          }
        ];
      })

      # Row 6 - Network
      (mkRow {
        title = "Network";
        gridPos = {
          x = 0;
          y = 41;
          w = 24;
          h = 1;
        };
      })
      (mkPanel {
        title = "Network Bandwidth";
        gridPos = {
          x = 0;
          y = 42;
          w = 12;
          h = 8;
        };
        unit = "bps";
        expr = [
          {
            expr = "rate(node_network_receive_bytes_total{device=~\"enp5s0|tailscale0\"}[5m]) * 8";
            legend = "RX {{device}}";
          }
          {
            expr = "rate(node_network_transmit_bytes_total{device=~\"enp5s0|tailscale0\"}[5m]) * 8";
            legend = "TX {{device}}";
          }
        ];
      })
      (mkPanel {
        title = "Network Errors & Drops";
        gridPos = {
          x = 12;
          y = 42;
          w = 6;
          h = 8;
        };
        expr = [
          {
            expr = "rate(node_network_receive_errs_total{device!=\"lo\"}[5m])";
            legend = "RX err {{device}}";
          }
          {
            expr = "rate(node_network_transmit_errs_total{device!=\"lo\"}[5m])";
            legend = "TX err {{device}}";
          }
          {
            expr = "rate(node_network_receive_drop_total{device!=\"lo\"}[5m])";
            legend = "RX drop {{device}}";
          }
        ];
      })
      (mkStat {
        title = "TCP Established";
        expr = "node_netstat_Tcp_CurrEstab";
        gridPos = {
          x = 18;
          y = 42;
          w = 6;
          h = 8;
        };
      })

      # Row 7 - Temps & GPU
      (mkRow {
        title = "Temperatures & GPU";
        gridPos = {
          x = 0;
          y = 50;
          w = 24;
          h = 1;
        };
      })
      (mkPanel {
        title = "CPU Temperature";
        expr = "node_hwmon_temp_celsius{chip=~\".*k10temp.*|.*coretemp.*\",sensor=\"temp1\"}";
        gridPos = {
          x = 0;
          y = 51;
          w = 6;
          h = 8;
        };
        unit = "celsius";
      })
      (mkPanel {
        title = "GPU Temperature";
        expr = "node_hwmon_temp_celsius{chip=~\".*amdgpu.*\"}";
        gridPos = {
          x = 6;
          y = 51;
          w = 6;
          h = 8;
        };
        unit = "celsius";
        legendMode = "table";
      })
      (mkPanel {
        title = "NVMe Temperatures";
        expr = "node_hwmon_temp_celsius{chip=~\"nvme_nvme.*\",sensor=\"temp1\"}";
        gridPos = {
          x = 12;
          y = 51;
          w = 6;
          h = 8;
        };
        unit = "celsius";
        legendMode = "table";
      })
      (mkPanel {
        title = "GPU Busy & VRAM";
        gridPos = {
          x = 18;
          y = 51;
          w = 6;
          h = 8;
        };
        unit = "percent";
        expr = [
          {
            expr = "node_drm_gpu_busy_percent";
            legend = "GPU Busy %";
          }
          {
            expr = "node_drm_memory_vram_used_bytes / node_drm_memory_vram_size_bytes * 100";
            legend = "VRAM Used %";
          }
        ];
      })

      # Row 8 - PSI Pressure
      (mkRow {
        title = "Pressure Stall Info (PSI)";
        gridPos = {
          x = 0;
          y = 59;
          w = 24;
          h = 1;
        };
      })
      (mkPanel {
        title = "CPU Pressure";
        expr = "rate(node_pressure_cpu_waiting_seconds_total[5m])";
        gridPos = {
          x = 0;
          y = 60;
          w = 8;
          h = 8;
        };
        unit = "percentunit";
      })
      (mkPanel {
        title = "I/O Pressure";
        gridPos = {
          x = 8;
          y = 60;
          w = 8;
          h = 8;
        };
        unit = "percentunit";
        expr = [
          {
            expr = "rate(node_pressure_io_waiting_seconds_total[5m])";
            legend = "IO waiting";
          }
          {
            expr = "rate(node_pressure_io_stalled_seconds_total[5m])";
            legend = "IO stalled";
          }
        ];
      })
      (mkPanel {
        title = "Memory Pressure";
        expr = "rate(node_pressure_memory_waiting_seconds_total[5m])";
        gridPos = {
          x = 16;
          y = 60;
          w = 8;
          h = 8;
        };
        unit = "percentunit";
      })
    ];
  };
}
