---
title: "CSharp-setup-linux-build"
author: ["James Dyer"]
lastmod: 2025-06-28T08:50:00+01:00
tags: ["emacs", 2025]
categories: ["emacs", "linux"]
draft: true
thumbnail: "/emacs/20250628085057-emacs--CSharp-setup-linux-build.jpg"
---

Here's the best way to install VS Code on Arch Linux with full C# support:


## 1. Install Visual Studio Code {#1-dot-install-visual-studio-code}


### Option A: AUR (Recommended for full Microsoft version) {#option-a-aur--recommended-for-full-microsoft-version}

```bash
# Using yay
yay -S visual-studio-code-bin

OR

# Using makepkg manually
git clone https://aur.archlinux.org/visual-studio-code-bin.git
cd visual-studio-code-bin
makepkg -si
```


## 2. Install .NET SDK {#2-dot-install-dot-net-sdk}

```bash
# Install specific version from AUR if needed
yay -S dotnet-sdk-9.0

# Verify installation
dotnet --version
```


## 3. Install C# Extensions {#3-dot-install-c-extensions}

```bash
# Install C# Dev Kit and related extensions
code --install-extension ms-dotnettools.csdevkit
code --install-extension ms-dotnettools.vscodeintellicode-csharp
code --install-extension ms-dotnettools.vscode-dotnet-runtime
```


## 4. Install Additional Development Dependencies {#4-dot-install-additional-development-dependencies}

```bash
# Essential development tools
sudo pacman -S git base-devel

# Optional but useful for C# development
sudo pacman -S nuget mono
```


## 5. Verify Everything Works {#5-dot-verify-everything-works}

```bash
# Check VS Code version
code --version

# Check .NET
dotnet --version
dotnet --list-sdks

# List installed extensions
code --list-extensions | grep dotnet
```


## 6. Create a Test C# Project {#6-dot-create-a-test-c-project}

```bash
# Create and test a simple C# project
mkdir test-csharp
cd test-csharp
dotnet new console
code .
```


## Arch-Specific Tips: {#arch-specific-tips}


### If you encounter issues with extensions: {#if-you-encounter-issues-with-extensions}

```bash
# Clear extension cache
rm -rf ~/.vscode/extensions
# Then reinstall extensions
```


### For better performance: {#for-better-performance}

```bash
# Install additional fonts for better rendering
sudo pacman -S ttf-fira-code ttf-dejavu
```


### If you need multiple .NET versions: {#if-you-need-multiple-dot-net-versions}

```bash
# Check available versions in AUR
yay -Ss dotnet-sdk

# Install specific versions if needed
yay -S dotnet-sdk-6.0 dotnet-sdk-7.0 dotnet-sdk-8.0
```


### Environment setup: {#environment-setup}

Add to your `~/.bashrc` or `~/.zshrc`:

```bash
export DOTNET_ROOT=/usr/share/dotnet
export PATH=$PATH:$DOTNET_ROOT:$DOTNET_ROOT/tools
```


## Troubleshooting: {#troubleshooting}


### If C# Dev Kit doesn't work: {#if-c-dev-kit-doesn-t-work}

1.  Make sure you installed `visual-studio-code-bin` (not `code`)
2.  Sign in with Microsoft account in VS Code
3.  Restart VS Code after installing extensions


### If IntelliSense is slow: {#if-intellisense-is-slow}

```bash
# Increase inotify limits
echo 'fs.inotify.max_user_watches=524288' | sudo tee -a /etc/sysctl.conf
sudo sysctl -p
```

This setup will give you the complete VS Code experience on Arch Linux with full C# development capabilities, including debugging, IntelliSense, and project management!
