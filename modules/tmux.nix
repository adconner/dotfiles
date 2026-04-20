{ lib, ... }:
{
  flake.modules.homeManager.base = { pkgs, lib, ... }: {
    programs.tmux = {
      enable = true;
      keyMode = "vi";
      terminal = "tmux-256color";
      extraConfig = ''
unbind-key -T copy-mode-vi C-h
unbind-key -T copy-mode-vi C-j
bind-key -r C-b send-prefix
set -g repeat-time 700

bind-key C-v split-window -h
bind-key v split-window -h
bind-key - split-window
bind-key C-c new-window

bind-key -r C-j resize-pane -D 5
bind-key -r C-k resize-pane -U 5
bind-key -r C-h resize-pane -L 5
bind-key -r C-l resize-pane -R 5

bind-key -n C-h select-pane -L
bind-key -n C-j select-pane -D
bind-key -n C-k select-pane -U
bind-key -n C-l select-pane -R

bind-key -n M-j swap-pane -D
bind-key -n M-k swap-pane -U

bind-key j next-window
bind-key k previous-window

bind-key -n M-1 select-window -t :=1
bind-key -n M-2 select-window -t :=2
bind-key -n M-3 select-window -t :=3
bind-key -n M-4 select-window -t :=4
bind-key -n M-5 select-window -t :=5
bind-key -n M-6 select-window -t :=6
bind-key -n M-7 select-window -t :=7
bind-key -n M-8 select-window -t :=8
bind-key -n M-9 select-window -t :=9
bind-key -n M-0 select-window -t :=0


set -g status-justify left
set -g status-bg default
set -g status-fg colour12
set -g status-interval 60

setw -g window-status-format " #F#I:#W#F "
setw -g window-status-current-format " #F#I:#W#F "
setw -g window-status-format "#[fg=magenta]#[bg=black] #I #[bg=cyan]#[fg=colour8] #W "
setw -g window-status-current-format "#[bg=brightmagenta]#[fg=colour8] #I #[fg=colour8]#[bg=colour14] #W "

set -g status-position bottom
set -g status-bg colour234
set -g status-fg colour137
set -g status-left ""
set -g status-right '#[fg=colour233,bg=colour241,bold] %d/%m #[fg=colour233,bg=colour245,bold] %l:%M '
set -g status-right-length 50
set -g status-left-length 20

setw -g window-status-current-format ' #I#[fg=colour250]:#[fg=colour255]#W#[fg=colour50]#F '

setw -g window-status-format ' #I#[fg=colour237]:#[fg=colour250]#W#[fg=colour244]#F '
'';
    };
  };
}
