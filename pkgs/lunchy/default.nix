{ bundlerApp, fetchFromGitHub, lib }:

bundlerApp {
  pname = "lunchy";
  gemdir = ./.;
  exes = [ "lunchy" ];
  meta = with lib; {
    homepage = "https://github.com/eddiezane/lunchy";
    description = "A friendly wrapper for launchctl";
    license = "MIT";
    platforms = platforms.darwin;
    maintainers = [ maintainers.babariviere ];
  };
}
