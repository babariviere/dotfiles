{ config, pkgs }:

{
  programs.rofi = {
    enable = true;
    theme = let inherit (config.lib.formats.rasi) mkLiteral;
    in {
      "*" = {
        # Dracula theme colour palette
        drac-bgd = mkLiteral "#282a36";
        drac-cur = mkLiteral "#44475a";
        drac-fgd = mkLiteral "#f8f8f2";
        drac-cmt = mkLiteral "#6272a4";
        drac-cya = mkLiteral "#8be9fd";
        drac-grn = mkLiteral "#50fa7b";
        drac-ora = mkLiteral "#ffb86c";
        drac-pnk = mkLiteral "#ff79c6";
        drac-pur = mkLiteral "#bd93f9";
        drac-red = mkLiteral "#ff5555";
        drac-yel = mkLiteral "#f1fa8c";

        font = "MonoLisa 10";

        foreground = mkLiteral "@drac-fgd";
        background = mkLiteral "@drac-bgd";
        background-color = mkLiteral "@drac-bgd";
        active-background = mkLiteral "@drac-pnk";
        urgent-background = mkLiteral "@drac-red";

        selected-background = mkLiteral "@active-background";
        selected-urgent-background = mkLiteral "@urgent-background";
        selected-active-background = mkLiteral "@active-background";
        separatorcolor = mkLiteral "@active-background";
        bordercolor = mkLiteral "#6272a4";
      };
      "window" = {
        background-color = mkLiteral "@background";
        border = 3;
        border-radius = 6;
        border-color = mkLiteral "@bordercolor";
        padding = 5;
      };
      "mainbox" = {
        border = 0;
        padding = 5;
      };
      "message" = {
        border = mkLiteral "1px dash 0px 0px";
        border-color = mkLiteral "@separatorcolor";
        padding = mkLiteral "1px";
      };
      "textbox" = { text-color = mkLiteral "@foreground"; };
      "listview" = {
        fixed-height = 0;
        border = mkLiteral "2px dash 0px 0px";
        border-color = mkLiteral "@bordercolor";
        spacing = mkLiteral "2px";
        scrollbar = false;
        padding = mkLiteral "2px 0px 0px";
      };
      "element" = {
        border = 0;
        padding = mkLiteral "1px";
      };
      "element-text" = {
        background-color = mkLiteral "inherit";
        text-color = mkLiteral "inherit";
      };
      "element-icon" = { background-color = mkLiteral "inherit"; };
      "element.normal.normal" = {
        background-color = mkLiteral "@background";
        text-color = mkLiteral "@foreground";
      };
      "element.normal.urgent" = {
        background-color = mkLiteral "@urgent-background";
        text-color = mkLiteral "@urgent-foreground";
      };
      "element.normal.active" = {
        background-color = mkLiteral "@active-background";
        text-color = mkLiteral "@background";
      };
      "element.selected.normal" = {
        background-color = mkLiteral "@selected-background";
        text-color = mkLiteral "@foreground";
      };
      "element.selected.urgent" = {
        background-color = mkLiteral "@selected-urgent-background";
        text-color = mkLiteral "@foreground";
      };
      "element.selected.active" = {
        background-color = mkLiteral "@selected-active-background";
        text-color = mkLiteral "@background";
      };
      "element.alternate.normal" = {
        background-color = mkLiteral "@background";
        text-color = mkLiteral "@foreground";
      };
      "element.alternate.urgent" = {
        background-color = mkLiteral "@urgent-background";
        text-color = mkLiteral "@foreground";
      };
      "element.alternate.active" = {
        background-color = mkLiteral "@active-background";
        text-color = mkLiteral "@foreground";
      };
      "scrollbar" = {
        width = mkLiteral "2px ";
        border = 0;
        handle-width = mkLiteral "8px";
        padding = 0;
      };
      "sidebar" = {
        border = mkLiteral "2px dash 0px 0px";
        border-color = mkLiteral "@separatorcolor";
      };
      "button.selected" = {
        background-color = mkLiteral "@selected-background";
        text-color = mkLiteral "@foreground";
      };
      "inputbar" = {
        spacing = 0;
        text-color = mkLiteral "@foreground";
        padding = mkLiteral "1px";
      };
      "case-indicator" = {
        spacing = 0;
        text-color = mkLiteral "@foreground";
      };
      "entry" = {
        spacing = 0;
        text-color = mkLiteral "@drac-cya";
      };
      "prompt" = {
        spacing = 0;
        text-color = mkLiteral "@drac-grn";
      };
      "inputbar" = {
        children = map mkLiteral [
          "prompt"
          "textbox-prompt-colon"
          "entry"
          "case-indicator"
        ];
      };
      "textbox-prompt-colon" = {
        expand = false;
        str = ":";
        margin = mkLiteral "0px 0.3em 0em 0em ";
        text-color = mkLiteral "@drac-grn";
      };

    };
    terminal = "${pkgs.foot}/bin/foot";
  };

}
