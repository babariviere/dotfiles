{ fetchurl }:

{
  src = fetchurl {
    url = "https://unsplash.com/photos/ppk4z65O3MU/download?force=true";
    sha256 = "kRNGwqoPwsTZmr2mPe1jpgLhCoRnuE6/6RxaS/HHuIM=";
  };

  meta.src = "https://unsplash.com/photos/ppk4z65O3MU";
}
