#!/bin/bash
set -euo pipefail

# Only run on Claude Code for web
if [[ "${CLAUDE_CODE_REMOTE:-}" != "true" ]]; then
    echo "Not running on Claude Code web, skipping setup"
    exit 0
fi

echo "Setting up Racket environment for Claude Code on the web..."

# Install Racket if not already present
if ! command -v racket &> /dev/null; then
    echo "Installing Racket..."
    mkdir -p ~/.local
    curl -L https://mirror.racket-lang.org/installers/9.0/racket-9.0-x86_64-linux-cs.sh -o /tmp/racket-install.sh
    chmod +x /tmp/racket-install.sh
    /tmp/racket-install.sh --in-place --dest ~/.local/racket
    rm /tmp/racket-install.sh
fi

# Add Racket to PATH
export PATH="$HOME/.local/racket/bin:$PATH"

# Persist PATH changes
if [[ -n "${CLAUDE_ENV_FILE:-}" ]]; then
    echo "PATH=$HOME/.local/racket/bin:\$PATH" >> "$CLAUDE_ENV_FILE"
else
    echo 'export PATH="$HOME/.local/racket/bin:$PATH"' >> ~/.bashrc
fi

# Verify Racket installation
if ! command -v racket &> /dev/null; then
    echo "ERROR: Racket installation failed"
    exit 1
fi

echo "Racket $(racket --version) installed successfully"

# Install rackcheck-lib dependency (not in main package catalog)
if [[ ! -d "$HOME/.local/share/racket/pkgs/rackcheck-lib" ]]; then
    echo "Installing rackcheck-lib..."
    git clone --depth 1 https://github.com/Bogdanp/rackcheck.git /tmp/rackcheck
    raco pkg install --auto --skip-installed /tmp/rackcheck/rackcheck-lib
    rm -rf /tmp/rackcheck
fi

# Link the current project as a Racket package
echo "Linking recspecs packages..."
raco pkg install --auto --skip-installed ./recspecs-lib ./recspecs

echo "Setup complete!"
