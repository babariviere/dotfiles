{ config, inputs, lib, pkgs, ... }:

{
  programs.fish = {
    enable = true;
    interactiveShellInit =
      let flow = "${inputs.flow.defaultPackage.${pkgs.system}}/bin/flow";
      in ''
        set fish_cursor_default block
        set fish_cursor_insert line
        set fish_cursor_replace_one underscore
        set fish_cursor_visual block
        fish_vi_cursor
        fish_vi_key_bindings

        ${flow} setup $HOME/src --path ${flow} fish | source
      '';
    loginShellInit = ''
      fenv source $HOME/.profile
    '';
    plugins = [
      {
        name = "dracula";
        src = pkgs.fetchFromGitHub {
          owner = "dracula";
          repo = "fish";
          rev = "28db361b55bb49dbfd7a679ebec9140be8c2d593";
          sha256 = "07kz44ln75n4r04wyks1838nhmhr7jqmsc1rh7am7glq9ja9inmx";
        };
      }
      {
        name = "pure";
        src = pkgs.fishPlugins.pure.src;
      }
      {
        name = "foreign-env";
        src = pkgs.fishPlugins.foreign-env.src;
      }
    ];
  };
}
