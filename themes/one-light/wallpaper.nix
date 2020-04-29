{ fetchurl }:

{
  src = fetchurl {
    url = "https://unsplash.com/photos/_f2m3mEkaaU/download?force=true";
    sha256 = "08zgkh8d20gq4xvd49y129f9ba5vgwwq9bcsx8gzwr4agd8kkz4p";
  };

  meta = { src = "https://unsplash.com/photos/_f2m3mEkaaU"; };
}
