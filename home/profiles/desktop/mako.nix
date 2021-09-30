{
  programs.mako = {
    enable = true;
    font = "MonoLisa 10";
    defaultTimeout = 0;
    backgroundColor = "#282a36";
    borderColor = "#282a36";
    textColor = "#f8f8f2";
    extraConfig = ''
      [urgency=low]
      border-color=#282a36

      [urgency=normal]
      border-color=#f1fa8c

      [urgency=high]
      border-color=#ff5555
    '';
  };

}
