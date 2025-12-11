{pkgs, ...}: {
  hardware.amdgpu.opencl.enable = true;
  hardware.amdgpu.amdvlk.enable = true;
  services.xserver.videoDrivers = ["amdgpu"];
  hardware.graphics = {
    enable = true;
    enable32Bit = true;
  };
  services.open-webui = {
    enable = true;
    host = "0.0.0.0";
    openFirewall = true;
    port = 65080;
  };
  services.ollama = {
    enable = true;
    acceleration = "rocm";
    host = "0.0.0.0";
    loadModels = ["codellama" "llama3.2" "gemma3:4b-it-qat" "nomic-embed-text:v1.5"];
    openFirewall = true;
    package = pkgs.ollama-rocm;
  };
}
