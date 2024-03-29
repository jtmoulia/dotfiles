# Set GUIX_PROFILE to help with sourcing.
# NOTE: this is also set by env vars.
GUIX_PROFILE="${GUIX_PROFILE:-"$HOME/.guix-profile"}"
source "$GUIX_PROFILE/etc/profile"

# Add my local dir to the path
PATH="~/.local/bin:$PATH"

# Get flatpak XDG paths added
if [ -f "$GUIX_PROFILE/etc/profile.d/flatpak.sh" ]; then
    source "$GUIX_PROFILE/etc/profile.d/flatpak.sh"
fi
