{ iosevka }:
name: set: withLigature:

iosevka.override {
  privateBuildPlan = {
    design = if withLigature then
      [ "ligset-haskell" ]
    else
      [ "term" ] ++ [
        "v-at-fourfold"
        "v-a-singlestorey"
        "v-i-zshaped"
        "v-g-singlestorey"
        "v-l-zshaped"
        "v-brace-straight"
        "v-numbersign-slanted"
        "v-asterisk-hexlow"
      ];
    family = name;
  };

  inherit set;
  extraParameters =
    if withLigature then builtins.readFile ./iosevka.toml else null;
}
