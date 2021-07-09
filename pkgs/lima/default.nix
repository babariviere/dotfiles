{ buildGoModule, fetchFromGitHub, lib, makeWrapper, qemu }:

buildGoModule rec {
  name = "lima";
  version = "0.5.0";

  src = fetchFromGitHub {
    owner = "AkihiroSuda";
    repo = "lima";
    rev = "v${version}";
    sha256 = "sha256-1952xGSfVFI2Fs5HLJKCyB6ZxKFf5uPKXIlctM/T+8o=";
  };

  buildFlagsArray = [
    "-ldflags=-s -w -X github.com/AkihiroSuda/lima/pkg/version.Version=${version}"
  ];

  postBuild = ''
    GOOS=linux GOARCH=amd64 buildGoDir install ./cmd/lima-guestagent
    GOOS=linux GOARCH=arm64 buildGoDir install ./cmd/lima-guestagent
  '';

  postInstall = ''
    cp cmd/lima $out/bin
    cp cmd/nerdctl.lima $out/bin
    wrapProgram $out/bin/lima --set LIMACTL $out/bin/limactl
    wrapProgram $out/bin/nerdctl.lima --prefix PATH : $out/bin
    wrapProgram $out/bin/limactl --prefix PATH : ${lib.makeBinPath [ qemu ]}

    mkdir -p $out/share/lima
    mv $out/bin/linux_amd64/lima-guestagent $out/share/lima/lima-guestagent.Linux-x86_64
    mv $out/bin/linux_arm64/lima-guestagent $out/share/lima/lima-guestagent.Linux-aarch64
    rm -rf $out/bin/linux_amd64 $out/bin/linux_arm64
  '';

  vendorSha256 = "sha256-rPL/jxMHMkKffoYLSI3FFtFRYGtARKmrODmL9w+rN0E=";

  CGO_ENABLED = 0;

  buildInputs = [ makeWrapper ];
  propagatedBuildInputs = [ qemu ];
}
