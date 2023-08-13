{lib, ...}:
with lib; {
  # make a service that is a part of the graphical session target
  mkGraphicalService = recursiveUpdate {
    partOf = ["graphical-session.target"];
    after = ["graphical-session.target"];
    wantedBy = ["graphical-session.target"];
  };

  disableService = service: {
    systemd.services."${service}".wantedBy = mkForce [];
  };
  disableUserService = service: {
    systemd.user.services."${service}".wantedBy = mkForce [];
  };
}
