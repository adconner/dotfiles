# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  nix.settings.substituters = [
    "https://nix-community.cachix.org"
  ];

  nix.settings.trusted-public-keys = [
    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
  ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Use latest kernel.
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.tmp.useTmpfs = true;

  networking.hostName = "oscar"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone
  time.timeZone = "America/New_York";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.nvidia.open = true;
  hardware.graphics.enable = true;

  # services.xserver.displayManager.lightdm.enable = true;
  # services.xserver.desktopManager.xfce.enable = true;
  
  services.displayManager.cosmic-greeter.enable = true;
  services.desktopManager.cosmic.enable = true;
  programs.hyprland = {
    enable = true;
    withUWSM = true;
  };
  # services.blueman.enable = true;
  services.playerctld.enable = true;
  
  # services.xserver.displayManager.gdm.enable = true;
  # services.xserver.desktopManager.gnome.enable = true;
  
  # services.xserver.displayManager.startx.enable = true;
  # services.xserver.windowManager.xmonad = {
  #   enable = true;
  #   enableContribAndExtras = true;
  #   config = builtins.readFile /home/austin/.xmonad/xmonad.hs;
  # };
  # hardware.bluetooth = {
  #   enable = true;
  #   powerOnBoot = true;
  #   settings = {
  #     General = {
  #       # Shows battery charge of connected devices on supported
  #       # Bluetooth adapters. Defaults to 'false'.
  #       Experimental = true;
  #       # When enabled other devices can connect faster to us, however
  #       # the tradeoff is increased power consumption. Defaults to
  #       # 'false'.
  #       FastConnectable = true;
  #     };
  #     Policy = {
  #       # Enable all controllers when they are found. This includes
  #       # adapters present on start as well as adapters that are plugged
  #       # in later on. Defaults to 'true'.
  #       AutoEnable = true;
  #     };
  #   };
  # };


  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "dvorak";
    options = "ctrl:nocaps";
  };

  # Configure console keymap
  console.keyMap = "dvorak";

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

 # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.austin = {
    isNormalUser = true;
    description = "Austin Conner";
    extraGroups = [ "networkmanager" "wheel" ];
    shell = pkgs.zsh;
    packages = with pkgs; [
    ];
  };
  
  programs.zsh = {
    enable = true;
    interactiveShellInit = ''
      source ${pkgs.grml-zsh-config}/etc/zsh/zshrc
      source ${pkgs.fzf}/share/fzf/key-bindings.zsh
      fpath+=${pkgs.fetchFromGitHub {
        owner = "rkh";
        repo = "zsh-jj";
        rev = "b6453d6ff5d233d472e5088d066c6469eb05c71b";
        hash = "sha256-GDHTp53uHAcyVG+YI3Q7PI8K8M3d3i2+C52zxnKbSmw=";
      }}/functions
      zstyle ':vcs_info:*' enable jj
      alias cd="z"
      source <(COMPLETE=zsh jj)
    '';
    promptInit = ""; # otherwise it'll override the grml prompt

    enableCompletion = true;
    autosuggestions.enable = true;
    syntaxHighlighting.enable = true;

    shellAliases = {
      vi = "nvim";
      vim = "nvim";
      ll = "ls -l";
      edit = "sudo -e";
    };

    histSize = 10000;
    histFile = "$HOME/.zsh_history";
    setOptions = [
      "HIST_IGNORE_ALL_DUPS"
    ];
  };
  programs.firefox.enable = true;
  programs.neovim.enable = true;
  programs.neovim.defaultEditor = true;
  programs.git.enable = true;
  programs.htop.enable = true;
  programs.direnv.enable = true;
  programs.steam.enable = true;
  programs.nix-ld.enable = true;
  programs.zoxide.enable = true;

  fonts.packages = builtins.filter lib.attrsets.isDerivation (builtins.attrValues pkgs.nerd-fonts);

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;
  # nixpkgs.config.cudaSupport = true; # better enable for packages wanted for
  nixpkgs.overlays = [
    (self: super: {
      btop = super.btop.override { cudaSupport = true; };
    })
  ];

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    wget
    tmux
    clang
    dfc
    tree
    wl-clipboard
    ripgrep
    fzf
    fd
    jujutsu
    # sage
    pyright
    viddy
    mosh
    devenv
    btop
    nvitop
    nix-search-cli
    nnn
    yazi
    pulsemixer
    alacritty
    
    ##xmonad desktop utils
    #xmobar
    # alacritty
    #autocutsel
    #feh
    #unclutter
    #redshift
    #dmenu-rs
    
    # # hyprland desktop utils
    # waybar
    # alacritty
    # kitty
    # ghostty
    # # networkmanagerapplet
    # hyprpaper
    # hyprpolkitagent
    # swaynotificationcenter
    # waybar-mpris
    # pamixer
    # pavucontrol
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
    };
  };
  # systemd.oomd.enable = true;
  services.earlyoom.enable = true;
  zramSwap.enable = true;

  nix.settings.experimental-features = [ 
    "nix-command" 
    "flakes" 
  ];

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "25.05"; # Did you read the comment?

}
