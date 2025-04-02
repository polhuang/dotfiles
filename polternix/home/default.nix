{ config, pkgs, ... }:

{
  home.packages = [
    pkgs.slack
  ];

  home.stateVersion = "23.11";
}
