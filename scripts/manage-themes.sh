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

run_example() {
    local example_dir="$THEMES_DIR/$KEEP_THEME/exampleSite"

    if [ ! -d "$example_dir" ]; then
        echo "Error: exampleSite not found at $example_dir"
        exit 1
    fi

    echo "Starting hugo-bootstrap-gallery exampleSite..."
    echo "URL: http://localhost:1313/"
    echo ""

    cd "$example_dir"
    hugo server --themesDir ../.. --disableFastRender
}

sync_example() {
    local example_dir="$THEMES_DIR/$KEEP_THEME/exampleSite"
    local content_dir="$example_dir/content"
    local static_dir="$example_dir/static"

    echo "Syncing content from hugo-unified to exampleSite..."
    echo ""

    # Clear existing content and static (preserve config.toml)
    echo "Clearing existing content..."
    rm -rf "$content_dir"
    rm -rf "$static_dir"
    mkdir -p "$content_dir"
    mkdir -p "$static_dir"

    # --- Content sections to sync ---
    # art--gallery: select a few categories (images as page bundles)
    echo "Syncing art--gallery (sample categories as page bundles)..."
    for category in animals monsters landscapes portraits; do
        if [ -d "$REPO_ROOT/content/art--gallery/$category" ]; then
            mkdir -p "$content_dir/art--gallery/$category"
            # Copy index.md
            cp "$REPO_ROOT/content/art--gallery/$category/index.md" \
               "$content_dir/art--gallery/$category/" 2>/dev/null || true
            # Copy images from STATIC into content (page bundles)
            if [ -d "$REPO_ROOT/static/art--gallery/$category" ]; then
                find "$REPO_ROOT/static/art--gallery/$category" -maxdepth 1 -type f \
                    \( -name "*.jpg" -o -name "*.png" -o -name "*.webp" \) | head -12 | \
                    xargs -I {} cp {} "$content_dir/art--gallery/$category/" 2>/dev/null || true
            fi
        fi
    done

    # emacs: copy recent posts (limit to 10)
    echo "Syncing emacs posts (10 most recent)..."
    mkdir -p "$content_dir/emacs"
    ls -t "$REPO_ROOT/content/emacs"/*.md 2>/dev/null | head -10 | \
        xargs -I {} cp {} "$content_dir/emacs/" 2>/dev/null || true

    # tags: copy tag index pages
    echo "Syncing tags..."
    if [ -d "$REPO_ROOT/content/tags" ]; then
        cp -r "$REPO_ROOT/content/tags" "$content_dir/"
    fi

    # Create _index.md for homepage
    cat > "$content_dir/_index.md" << 'EOF'
---
title: "hugo-bootstrap-gallery Demo"
---
This is a demo site for the hugo-bootstrap-gallery theme, populated with real content.
EOF

    # --- Static assets ---
    echo "Syncing static assets..."

    # Core assets (CSS, JS)
    if [ -d "$REPO_ROOT/static/assets" ]; then
        cp -r "$REPO_ROOT/static/assets" "$static_dir/"
    fi

    # Banner images
    if [ -d "$REPO_ROOT/static/images" ]; then
        cp -r "$REPO_ROOT/static/images" "$static_dir/"
    fi

    # Note: art--gallery images are now in content/ as page bundles (not static/)

    # emacs static images (scan markdown for image references)
    echo "Syncing emacs images..."
    mkdir -p "$static_dir/emacs"
    for md in "$content_dir/emacs"/*.md; do
        [ -f "$md" ] || continue
        # Extract image paths from markdown
        grep -oE '/emacs/[^")]+\.(png|jpg|gif|webp)' "$md" 2>/dev/null | while read img; do
            imgfile="$REPO_ROOT/static$img"
            if [ -f "$imgfile" ]; then
                cp "$imgfile" "$static_dir/emacs/" 2>/dev/null || true
            fi
        done
    done

    # Summary
    echo ""
    echo "=== Sync complete ==="
    echo "Content:"
    find "$content_dir" -name "*.md" | wc -l | xargs printf "  Markdown files: %d\n"
    find "$content_dir" -type f \( -name "*.jpg" -o -name "*.png" -o -name "*.webp" \) | wc -l | xargs printf "  Image files: %d\n"
    echo "Static:"
    du -sh "$static_dir" 2>/dev/null | cut -f1 | xargs printf "  Total size: %s\n"
    echo ""
    echo "Run './scripts/manage-themes.sh example' to preview"
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
    example)
        run_example
        ;;
    sync-example)
        sync_example
        ;;
    *)
        echo "Usage: $0 {list|status|remove-all|install <name>|install-all|sync-example|example}"
        echo ""
        echo "Commands:"
        echo "  list         - List all available themes with URLs"
        echo "  status       - Show currently installed themes"
        echo "  remove-all   - Remove all submodules except $KEEP_THEME"
        echo "  install      - Install a specific theme by name"
        echo "  install-all  - Install all themes"
        echo "  sync-example - Sync real content to exampleSite"
        echo "  example      - Run the hugo-bootstrap-gallery exampleSite"
        exit 1
        ;;
esac
