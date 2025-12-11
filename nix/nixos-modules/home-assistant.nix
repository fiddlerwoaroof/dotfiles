{
  networking.firewall = {
    allowedTCPPorts = [21064 32919 40000 8123];
    allowedUDPPorts = [1900 47497 49756 5353 55131 55489 56880 45091];
  };
  virtualisation.oci-containers.containers.home-assistant = {
    image = "ghcr.io/home-assistant/home-assistant:stable";
    environment.TZ = "America/Los_Angeles";
    privileged = true;
    networks = ["host"];
    ports = ["0.0.0.0:8123:8123"];
    #extraOptions = ["--device=/dev/ttyACM0:/dev/ttyACM0"];
    volumes = ["/var/lib/hass:/config" "/run/dbus:/run/dbus:ro"];
  };
}
