# Session: Convert Themes to Git Submodules

**Date:** 2026-01-26

## Task

Replace all Hugo themes (except hugo-bootstrap-gallery which is user's own fork) with git submodules pointing to their upstream repositories.

## Changes Made

### 1. Updated `.gitignore`

Changed from:
```
# Ignore all themes except hyde
themes/*
!themes/hyde/
```

To:
```
# Themes are managed as git submodules
```

### 2. Removed Old Theme Directories

Deleted all theme directories except `hugo-bootstrap-gallery`:
- beautifulhugo
- hugo-clarity
- Hugo-Octopress
- hugo-paper
- hugo-theme-m10c
- hugo-theme-notrack
- hugo-xmag
- hyde
- lightbi-hugo
- Mainroad
- risotto

### 3. Added Git Submodules

| Theme | Upstream Repository |
|-------|---------------------|
| beautifulhugo | https://github.com/halogenica/beautifulhugo |
| hugo-clarity | https://github.com/chipzoller/hugo-clarity |
| Hugo-Octopress | https://github.com/parsiya/hugo-octopress |
| hugo-paper | https://github.com/nanxiaobei/hugo-paper |
| hugo-theme-m10c | https://github.com/vaga/hugo-theme-m10c |
| hugo-theme-notrack | https://github.com/gevhaz/hugo-theme-notrack |
| hugo-xmag | https://github.com/yihui/hugo-xmag |
| hyde | https://github.com/spf13/hyde |
| lightbi-hugo | https://github.com/binokochumolvarghese/lightbi-hugo |
| Mainroad | https://github.com/vimux/mainroad |
| risotto | https://github.com/joeroe/risotto |

### 4. Kept Unchanged

- `hugo-bootstrap-gallery` - User's own fork, remains as standalone directory (untracked)

## Future Theme Updates

To update all themes to latest upstream:
```bash
git submodule update --remote --merge
```

To update a specific theme:
```bash
cd themes/<theme-name>
git pull origin master
```

## Status

Changes are staged but not committed. Run `git status` to see pending changes.
