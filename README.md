# dotfiles

My dotfiles. They're organized in layers which 

  - can be applied sequentially
  - are installed to their relative path
  - keep backups

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
