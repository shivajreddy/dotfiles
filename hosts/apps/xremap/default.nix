{inputs, ... }:

let
      yamlConfiguration = builtins.readFile(./config.yml);
in
{
    imports = [
      inputs.xremap-flake.nixosModules.default
    ];

    services.xremap = {
        withWlroots = true;
        userName = "shiva";
        yamlConfig = ''
        ${yamlConfiguration}
        '';
    };
}
