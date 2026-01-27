#!/bin/bash
#
# manage-themes.sh - Manage Hugo theme submodules
#
# Usage:
#   ./scripts/manage-themes.sh list          - List all available themes
#   ./scripts/manage-themes.sh status        - Show currently installed themes
#   ./scripts/manage-themes.sh remove-all    - Remove all themes except hugo-bootstrap-gallery
#   ./scripts/manage-themes.sh install NAME  - Install a specific theme
#   ./scripts/manage-themes.sh install-all   - Install all themes
#

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
THEMES_DIR="$REPO_ROOT/themes"

# Theme registry: name -> GitHub URL
declare -A THEMES=(
    ["beautifulhugo"]="https://github.com/halogenica/beautifulhugo"
    ["hugo-clarity"]="https://github.com/chipzoller/hugo-clarity"
    ["Hugo-Octopress"]="https://github.com/parsiya/hugo-octopress"
    ["hugo-paper"]="https://github.com/nanxiaobei/hugo-paper"
    ["hugo-theme-m10c"]="https://github.com/vaga/hugo-theme-m10c"
    ["hugo-theme-notrack"]="https://github.com/gevhaz/hugo-theme-notrack"
    ["hugo-xmag"]="https://github.com/yihui/hugo-xmag"
    ["hyde"]="https://github.com/spf13/hyde"
    ["lightbi-hugo"]="https://github.com/binokochumolvarghese/lightbi-hugo"
    ["Mainroad"]="https://github.com/vimux/mainroad"
    ["risotto"]="https://github.com/joeroe/risotto"
)

# Theme kept permanently (not managed by this script)
KEEP_THEME="hugo-bootstrap-gallery"

list_themes() {
    echo "Available themes for installation:"
    echo "=================================="
    for name in $(echo "${!THEMES[@]}" | tr ' ' '\n' | sort); do
        printf "  %-20s %s\n" "$name" "${THEMES[$name]}"
    done
    echo ""
    echo "Permanently kept: $KEEP_THEME (not managed by this script)"
}

show_status() {
    echo "Theme status in $THEMES_DIR:"
    echo "============================="

    # Check for submodules
    cd "$REPO_ROOT"
    if [ -f .gitmodules ]; then
        echo ""
        echo "Installed as submodules:"
        grep -E '^\[submodule|path = ' .gitmodules | paste - - | while read line; do
            path=$(echo "$line" | sed 's/.*path = //')
            echo "  $path"
        done
    fi

    # Check for directories
    echo ""
    echo "Theme directories present:"
    for dir in "$THEMES_DIR"/*/; do
        if [ -d "$dir" ]; then
            name=$(basename "$dir")
            if [ -d "$dir/.git" ] || [ -f "$REPO_ROOT/.git/modules/themes/$name/HEAD" ]; then
                echo "  $name (submodule)"
            else
                echo "  $name (regular directory)"
            fi
        fi
    done
}

remove_submodule() {
    local name="$1"
    local path="themes/$name"

    cd "$REPO_ROOT"

    if grep -q "path = $path" .gitmodules 2>/dev/null; then
        echo "Removing submodule: $name"
        git submodule deinit -f "$path" 2>/dev/null || true
        git rm -f "$path" 2>/dev/null || true
        rm -rf ".git/modules/$path" 2>/dev/null || true
        rm -rf "$path" 2>/dev/null || true
    else
        echo "  $name is not a submodule, skipping"
    fi
}

remove_all() {
    echo "Removing all theme submodules except $KEEP_THEME..."
    echo ""

    cd "$REPO_ROOT"

    for name in "${!THEMES[@]}"; do
        remove_submodule "$name"
    done

    echo ""
    echo "Committing changes..."
    git add -A
    git commit -m "Remove unused theme submodules

Themes can be reinstated using scripts/manage-themes.sh install <name>

Removed themes:
$(for name in "${!THEMES[@]}"; do echo "- $name: ${THEMES[$name]}"; done | sort)
" || echo "Nothing to commit"

    echo ""
    echo "Done. Remaining themes:"
    ls -1 "$THEMES_DIR" 2>/dev/null || echo "  (none)"
}

install_theme() {
    local name="$1"

    if [ -z "$name" ]; then
        echo "Error: Please specify a theme name"
        echo "Usage: $0 install <theme-name>"
        list_themes
        exit 1
    fi

    if [ "$name" = "$KEEP_THEME" ]; then
        echo "Error: $KEEP_THEME is permanently kept and not managed by this script"
        exit 1
    fi

    local url="${THEMES[$name]}"
    if [ -z "$url" ]; then
        echo "Error: Unknown theme '$name'"
        echo ""
        list_themes
        exit 1
    fi

    cd "$REPO_ROOT"

    if [ -d "$THEMES_DIR/$name" ]; then
        echo "Theme $name already exists at $THEMES_DIR/$name"
        exit 1
    fi

    echo "Installing theme: $name"
    echo "  URL: $url"

    git submodule add "$url" "themes/$name"
    git add -A
    git commit -m "Add theme submodule: $name

Source: $url
"

    echo ""
    echo "Done. Theme installed at themes/$name"
}

install_all() {
    echo "Installing all themes..."
    echo ""

    cd "$REPO_ROOT"

    for name in "${!THEMES[@]}"; do
        if [ -d "$THEMES_DIR/$name" ]; then
            echo "Skipping $name (already exists)"
        else
            echo "Installing $name..."
            git submodule add "${THEMES[$name]}" "themes/$name" || true
        fi
    done

    git add -A
    git commit -m "Add all theme submodules

$(for name in "${!THEMES[@]}"; do echo "- $name: ${THEMES[$name]}"; done | sort)
" || echo "Nothing to commit"

    echo ""
    echo "Done."
}

# Main
case "${1:-}" in
    list)
        list_themes
        ;;
    status)
        show_status
        ;;
    remove-all)
        remove_all
        ;;
    install)
        install_theme "$2"
        ;;
    install-all)
        install_all
        ;;
    *)
        echo "Usage: $0 {list|status|remove-all|install <name>|install-all}"
        echo ""
        echo "Commands:"
        echo "  list        - List all available themes with URLs"
        echo "  status      - Show currently installed themes"
        echo "  remove-all  - Remove all submodules except $KEEP_THEME"
        echo "  install     - Install a specific theme by name"
        echo "  install-all - Install all themes"
        exit 1
        ;;
esac
