{ lib, buildGo118Package, src, version }:

buildGo118Package rec {
  pname = "lxd-agent";
  inherit version src;

  goPackagePath = "github.com/lxc/lxd";

  ldflags = [ "-extldflags=-static" "-s" "-w" ];

  subPackages = [ "lxd-agent" ];

  preConfigure = ''
    export CGO_ENABLED=0
  '';

  meta = with lib; {
    description = "Agent allowing LXD to acces VM functionalities";
    homepage = "https://linuxcontainers.org/lxd/";
    license = licenses.asl20;
    maintainers = with maintainers; [ fpletz marsam astralbijection ];
    platforms = platforms.linux;
  };
}

