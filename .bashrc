# Setup npm to use a user-specific packages directory as per
# https://github.com/sindresorhus/guides/blob/master/npm-global-without-sudo.md
function node_setup() {
    local NODE_STUFF=""

    read -r -d '' NODE_STUFF <<'EOF'
NPM_PACKAGES="${HOME}/.npm-packages"

export PATH="$NPM_PACKAGES/bin:$PATH"

# Unset manpath so we can inherit from /etc/manpath via the `manpath` command
unset MANPATH # delete if you already modified MANPATH elsewhere in your config
export MANPATH="$NPM_PACKAGES/share/man:$(manpath)"
EOF

    echo "${NODE_STUFF}"
}
