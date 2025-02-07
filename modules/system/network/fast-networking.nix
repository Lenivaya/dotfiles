{ config, lib, ... }:
with lib;
with my;
let
  cfg = config.modules.fast-networking;
in
{
  options.modules.fast-networking.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    boot = {
      kernel.sysctl = {
        # https://github.com/lovesegfault/nix-config/blob/23d432581755927dd08a11c84e078bc106698425/hardware/fast-networking.nix#L4
        "net.core.default_qdisc" = mkForce "cake";
        "net.core.optmem_max" = 65536;
        "net.core.rmem_default" = 1048576;
        "net.core.rmem_max" = 16777216;
        "net.core.somaxconn" = 8192;
        "net.core.wmem_default" = 1048576;
        "net.core.wmem_max" = 16777216;
        "net.ipv4.conf.all.log_martians" = 1;
        "net.ipv4.conf.all.rp_filter" = mkForce 1;
        "net.ipv4.conf.default.log_martians" = 1;
        "net.ipv4.conf.default.rp_filter" = mkForce 1;
        "net.ipv4.ip_local_port_range" = "16384 65535";
        "net.ipv4.tcp_congestion_control" = mkForce "bbr";
        "net.ipv4.tcp_fastopen" = mkForce 3;
        "net.ipv4.tcp_max_syn_backlog" = 8192;
        "net.ipv4.tcp_max_tw_buckets" = 2000000;
        "net.ipv4.tcp_mtu_probing" = 1;
        "net.ipv4.tcp_rfc1337" = mkForce 1;
        "net.ipv4.tcp_rmem" = "4096 1048576 2097152";
        "net.ipv4.tcp_slow_start_after_idle" = 0;
        "net.ipv4.tcp_syncookies" = mkForce 1;
        "net.ipv4.tcp_tw_reuse" = 1;
        "net.ipv4.tcp_wmem" = "4096 65536 16777216";
        "net.ipv4.udp_rmem_min" = 8192;
        "net.ipv4.udp_wmem_min" = 8192;
        "net.netfilter.nf_conntrack_generic_timeout" = 60;
        "net.netfilter.nf_conntrack_max" = 1048576;
        "net.netfilter.nf_conntrack_tcp_timeout_established" = 600;
        "net.netfilter.nf_conntrack_tcp_timeout_time_wait" = 1;

        # https://github.com/garuda-linux/garuda-nix-subsystem/blob/main/internal/modules/base/networking.nix
        "net.ipv4.tcp_fin_timeout" = 5;
      };
      # https://github.com/lovesegfault/nix-config/blob/23d432581755927dd08a11c84e078bc106698425/hardware/fast-networking.nix#L4
      kernelModules = [
        "tls"
        "tcp_bbr"
      ];
    };
  };
}
