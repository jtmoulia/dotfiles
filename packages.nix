# Install all packages
# $ nix-env -f packages.nix -i

# Install certain packages
# $ nix-env -f packages.nix -iA emacs

with (import <nixpkgs> {});
{ inherit git emacs erlang; }