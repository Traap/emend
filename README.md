The **Bootstrap** repository uses a SCRUM framework adapted to standard GitHub
tooling.  **Bootstrap** is integrated with Travis-ci.org for continuous
integration and AllanConsulting.slack.com for centralized notification.

# Installation
## Bootstrap 
```bash
$ cd $HOME
$ git clone http://github.com/Traap/bootstrap.git \
      && cd bootstrap \
      && chmod +x install.sh \
      && ./install.sh --all \
      && source ${HOME}.bashrc
```

## Note
This installation instructions installs my personal dotfiles in a directory named
~/git/dotfiles.

## Custom installations:
This example shows how to install TMUX.

```bash
cd ~\bootstrap
ruby ruby/bootstrap.rb --nodryrun --verbose --file apps/tmux/tmux.yaml
```

# YAML File
## Symbolic Links
TODO:  Describe YAML syntax for Symbolic Links.

## Clone Repositories
TODO:  Describe YAML syntax for cloning repositories.

## Installations
TODO:  Describe YAML syntax to install a program and or run a command.

# Project Management
Please refer to my [Lightweight Project Management](https://github.com/Traap/lpm)
for the project management strategy I use.
