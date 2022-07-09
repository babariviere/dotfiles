{
  services.xserver.synaptics.enable = false;
  services.xserver.libinput = {
    enable = true;
    touchpad.tapping = false;
    touchpad.scrollMethod = "twofinger";
    touchpad.clickMethod = "clickfinger";
  };
}
