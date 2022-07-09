{ lib, stdenv, tailscale, iproute2, iptables }:

tailscale.overrideAttrs (old: {
  postInstall = ''
    ${lib.optionalString stdenv.isLinux ''
      wrapProgram $out/bin/tailscaled --prefix PATH : ${
        lib.makeBinPath [ iproute2 iptables ]
      }
    ''}
    sed -i -e "s#/usr/sbin#$out/bin#" -e "/^EnvironmentFile/d" ./cmd/tailscaled/tailscaled.service
    install -D -m0444 -t $out/lib/systemd/system ./cmd/tailscaled/tailscaled.service

  '';

  meta = with lib; { platforms = platforms.linux ++ platforms.darwin; };
})
