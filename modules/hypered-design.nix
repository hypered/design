{ config, lib, pkgs, ... }:
{
  systemd.services.hypered-design = {
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "notify";
      ExecStart = ''
        ${(import ../.).binaries}/bin/hypered-design \
          serve \
          --port 8999 \
          --static-dir ${(import ../.).site}
      '';

      # Hardening options
      CapabilityBoundingSet = [ "" ];
      DevicePolicy = "closed";
      LockPersonality = true;
      MemoryDenyWriteExecute = true;
      NoNewPrivileges = true;
      PrivateDevices = true;
      ProtectClock = true;
      ProtectHome = true;
      ProtectHostname = true;
      ProtectControlGroups = true;
      ProtectKernelLogs = true;
      ProtectKernelModules = true;
      ProtectKernelTunables = true;
      ProtectProc = "invisible";
      ProcSubset = "pid";
      RemoveIPC = true;
      RestrictNamespaces = true;
      RestrictRealtime = true;
      RestrictSUIDSGID = true;
      SystemCallArchitectures = "native";
    };
  };
}
