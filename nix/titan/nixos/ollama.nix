{pkgs, ...}: {
  hardware.amdgpu.opencl.enable = true;
  #hardware.amdgpu.amdvlk.enable = true;
  services.xserver.videoDrivers = ["amdgpu"];
  hardware.graphics = {
    enable = true;
    enable32Bit = true;
  };
  #services.librechat = {
  #  enable = true;
  #  host = "0.0.0.0";
  #  openFirewall = true;
  #};
  services.open-webui = {
    enable = false;
    host = "0.0.0.0";
    openFirewall = true;
    port = 65080;
    environment = {
      WEBUI_URL = "http://172.16.31.3:65080";
      ANONYMIZED_TELEMETRY = "False";
      DO_NOT_TRACK = "True";
      SCARF_NO_ANALYTICS = "True";
      AIOHTTP_CLIENT_TIMEOUT = "1200";
    };
  };
  services.ollama = {
    enable = false;
    #acceleration = "rocm";
    host = "0.0.0.0";
    loadModels = ["codellama" "llama3.2" "gemma3:4b-it-qat" "nomic-embed-text:v1.5"];
    openFirewall = true;
    package = pkgs.ollama-rocm;
  };
}
