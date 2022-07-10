{ iosevka }:

iosevka.override {
  privateBuildPlan = {
    family = "Biosevka";
    spacing = "fixed";
    serifs = "sans";
    no-cv-ss = true;

    widths.normal = {
      shape = 550;
      menu = 5;
      css = "normal";
    };
  };

  set = "biosevka";
}
