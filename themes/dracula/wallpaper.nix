{ fetchurl }:

{
  src = fetchurl {
    url =
      "https://images.wallpapersden.com/image/download/moon-space-minimal_61440_1920x1080.jpg";
    sha256 = "1fwflzgkd4gbsj12v3vfpghnx0p12jq2vvizdvwk1afbx57667sv";
  };

  meta = {
    src = "https://wallpapersden.com/moon-space-minimal-wallpaper/1920x1080";
  };
}
