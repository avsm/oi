#!/usr/bin/env bash
# Publish $REGISTRY_LOCAL to $REGISTRY_RSYNC_DEST over SSH.
#
# Called from .github/workflows/registry.yml with:
#   REGISTRY_LOCAL       local path containing <os_key>/ subdirs (and
#                        optionally oi-linux-<arch>) to upload
#   REGISTRY_RSYNC_DEST  user@host:/absolute/path
#   REGISTRY_SSH_KEY     private key authorised to rsync into the target
#
# Each runner writes to a disjoint <os_key>/ subtree, so the three jobs
# (linux-amd64 / linux-arm64 / macos) can rsync concurrently without
# stepping on each other. Trailing slashes matter: we copy the *contents*
# of $REGISTRY_LOCAL/ into the remote, merging with whatever's there.

set -euo pipefail

: "${REGISTRY_LOCAL:?REGISTRY_LOCAL is required}"
: "${REGISTRY_RSYNC_DEST:?REGISTRY_RSYNC_DEST is required}"
: "${REGISTRY_SSH_KEY:?REGISTRY_SSH_KEY is required (secret not set?)}"

# Extract just the hostname for ssh-keyscan:
#   avsm@oi.ci.dev:/srv/registry → oi.ci.dev
host_and_path=${REGISTRY_RSYNC_DEST#*@}
host=${host_and_path%%:*}
if [[ -z "$host" || "$host" == "$REGISTRY_RSYNC_DEST" ]]; then
  echo "REGISTRY_RSYNC_DEST must be in user@host:/path form, got: $REGISTRY_RSYNC_DEST" >&2
  exit 2
fi

ssh_dir="$HOME/.ssh"
mkdir -p "$ssh_dir"
chmod 700 "$ssh_dir"

key_path="$ssh_dir/id_registry"
umask 077
printf '%s\n' "$REGISTRY_SSH_KEY" > "$key_path"
chmod 600 "$key_path"

# TOFU-style known_hosts via ssh-keyscan. Fine for publishing-side flows;
# if you want pinned host keys, preseed ~/.ssh/known_hosts instead and skip
# this block.
known_hosts="$ssh_dir/known_hosts"
ssh-keyscan -H "$host" >> "$known_hosts" 2>/dev/null
chmod 600 "$known_hosts"

# -aHz: preserve perms/links/times, hardlinks (for layer dedup), compress.
# --partial: resume half-transferred large tarballs on retry.
# --mkpath: create any missing path components on the remote.
rsync -aHz --partial --mkpath \
  -e "ssh -i $key_path -o UserKnownHostsFile=$known_hosts -o StrictHostKeyChecking=yes" \
  "$REGISTRY_LOCAL/" "$REGISTRY_RSYNC_DEST/"

echo "rsync complete: $REGISTRY_LOCAL/ -> $REGISTRY_RSYNC_DEST/"
