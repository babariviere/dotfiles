{ lib, hwdata, pkg-config, lxc, buildGo118Package, fetchurl, fetchpatch
, makeWrapper, acl, rsync, gnutar, xz, btrfs-progs, gzip, dnsmasq, attr
, squashfsTools, iproute2, iptables, libcap, dqlite, raft-canonical
, sqlite-replication, udev, writeShellScriptBin, apparmor-profiles
, apparmor-parser, criu, bash, installShellFiles, version, src, nixosTests
, extraBinPath ? [ ], LXD_OVMF_PATH ? "" }:
let
  apparmor_parser = writeShellScriptBin "apparmor_parser" ''
    exec '${apparmor-parser}/bin/apparmor_parser' -I '${apparmor-profiles}/etc/apparmor.d' "$@"
  '';

  binPath = lib.makeBinPath ([
    apparmor_parser
    iptables
    acl
    rsync
    gnutar
    xz
    btrfs-progs
    gzip
    dnsmasq
    squashfsTools
    iproute2
    bash
    criu
    attr
  ] ++ extraBinPath ++ [ "$out" ]);
in buildGo118Package rec {
  pname = "lxd";
  inherit version src;

  goPackagePath = "github.com/lxc/lxd";

  tags = [ "libsqlite3" ];

  postPatch = ''
    substituteInPlace shared/usbid/load.go \
      --replace "/usr/share/misc/usb.ids" "${hwdata}/share/hwdata/usb.ids"
  '';

  preBuild = ''
    # required for go-dqlite. See: https://github.com/lxc/lxd/pull/8939
    export CGO_LDFLAGS_ALLOW="(-Wl,-wrap,pthread_create)|(-Wl,-z,now)"
  '';

  postInstall = ''
    installShellCompletion --cmd lxd --bash go/src/github.com/lxc/lxd/scripts/bash/lxd-client
    wrapProgram $out/bin/lxd --prefix PATH : ${binPath} \
      --set LXD_OVMF_PATH '${LXD_OVMF_PATH}'
  '';

  nativeBuildInputs = [ makeWrapper installShellFiles pkg-config ];

  buildInputs = [
    lxc
    acl
    libcap
    dqlite
    raft-canonical
    sqlite-replication
    udev.dev
  ];

  passthru.tests = {
    lxd = nixosTests.lxd;
    lxd-useQemu = nixosTests.lxd-useQemu;
  };

  meta = with lib; {
    description = "Daemon based on liblxc offering a REST API to manage containers";
    homepage = "https://linuxcontainers.org/lxd/";
    license = licenses.asl20;
    maintainers = with maintainers; [ fpletz marsam astralbijection ];
    platforms = platforms.linux;
  };
}

