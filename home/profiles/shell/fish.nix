{ config, inputs, lib, pkgs, ... }:

{
  programs.fish = {
    enable = true;
    interactiveShellInit =
      ''
${pkgs.direnv}/bin/direnv hook fish | source

function b -d "jump to a directory"
  set -l _brycus_dir (/home/babariviere/src/github.com/babariviere/brycus/brycus query $argv)
  set -l _brycus_status $status
  if [ $_brycus_status = 0 ]; and [ "$_brycus_dir" != (pwd) ]
    cd "$_brycus_dir"
  end
  return $_brycus_status
end

function bq -d "query a directory"
  /home/babariviere/src/github.com/babariviere/brycus/brycus query $argv
end

function _brycus_add_directory --on-event fish_prompt
	/home/babariviere/src/github.com/babariviere/brycus/brycus add "$PWD" &
end
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
