# dotfiles

My dotfiles. They're organized in layers which 

  - can be applied sequentially
  - are installed to their relative path
  - keep backups

## Prelude

Set your root password to something secure:

```shell
# passwd
```

If you want to create a user account (`jtmoulia` for the sake of history):

```shell
# useradd --create-home -G wheel jtmoulia
# passwd jtmoulia
```

Note: the ansible deploy scripts will ensure that the user is properly configured.

Make sure that you've installed the necessary deps for running `wifi-menu`,
otherwise you won't be able to connect to the network:

```shell
# pacman -S dialog wpa_supplicant
```

## Basic Usage

To install some software (well, Emacs) and the common layer:

```shell
make
```

Typically you'll want to install one or more layers. Here's how to install the
default layer:

```shell
make layer
```

You can layer them on. This adds configuration specific to my ThinkPad X1:

```shell
make LAYER_DIR=layers/thinkpadx1 layer
```

This updates the version controlled layer file from what exists at the
corresponding path:

```shell
make LAYER_DIR=layers/thinkpadx1 layers/thinkpadx1/HOME/.Xmodmap
```

You can commit the layer's updates to version control.
