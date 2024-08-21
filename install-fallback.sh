#!/bin/bash

# Kiểm tra có dùng sudo không
if [ "$UID" -eq 0 ]; then
    echo -e "\e[31m Do not use sudo \e[0m"
    exit
fi

# Kiểm tra xem thư mục dotfiles có tồn tại không
if [ ! -e "$HOME/dotfiles" ]; then
    echo -e "\e[31m Please clone this repo to /home/$USER/ \e[0m"
    exit
fi

# Không cần sử dụng "source ~/.bashrc" nữa
export PATH="$PATH:/usr/local/bin:/usr/local/sbin:/opt/bin:$HOME/.local/bin:$HOME/bin"

# Cài đặt các gói cần thiết
echo -e "\e[32m [ INFO ] Install package \e[0m"
sudo pacman -Syu && sudo pacman -S --needed \
    neofetch ripgrep fzf git base-devel bat dbus \
    eza feh firefox github-cli kitty \
    xf86-input-libinput xorg-xinput \
    network-manager-applet nodejs npm picom \
    polybar pulseaudio pulseaudio-bluetooth \
    python-pynvim rofi xdg-utils zoxide zsh \
    noto-fonts-emoji ttf-jetbrains-mono-nerd \
    ibus thefuck xclip go less

# Cài đặt Oh My Zsh
echo -e "\e[32m [ INFO ] Oh-my-zsh \e[0m"
cd "$HOME"
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}/plugins/zsh-autosuggestions

# Sao chép các file cấu hình
echo -e "\e[32m [ INFO ] Copy dotfiles and config files \e[0m"
if [ ! -e "$HOME/.config" ]; then
    mkdir "$HOME/.config"
fi
cd $HOME/dotfiles
cp -rf config/* "$HOME/.config/"
cp -rf home/.* "$HOME"

# Cấu hình touchpad
echo -e "\e[32m [ INFO ] Config touchpad (natural scrolling, tap, etc) \e[0m"
sudo cp 30-touchpad.conf /etc/X11/xorg.conf.d

# Cấu hình module điều chỉnh độ sáng của Polybar
echo -e "\e[32m [ INFO ] Need by Polybar's backlight module (adjust brightness) \e[0m"
sudo usermod -aG video "$USER"
sudo cp backlight.rules /etc/udev/rules.d

# Cài đặt yay
echo -e "\e[32m [ INFO ] Install yay \e[0m"
git clone https://aur.archlinux.org/yay.git "$HOME/yay"
cd "$HOME/yay"
makepkg -si
cd "$HOME/dotfiles"
sudo rm -rf "$HOME/yay"

# Cài đặt các yêu cầu của XMonad (build từ source)
echo -e "\e[32m [ INFO ] Requirements of XMonad (Build from source) \e[0m"
sudo pacman -S --needed \
    git \
    xorg-server xorg-apps xorg-xinit xorg-xmessage \
    libx11 libxft libxinerama libxrandr libxss \
    pkgconf

if [ ! -e "$HOME/.config/xmonad" ]; then
    mkdir "$HOME/.config/xmonad"
fi
git clone https://github.com/xmonad/xmonad "$HOME/.config/xmonad/xmonad"
git clone https://github.com/xmonad/xmonad-contrib "$HOME/.config/xmonad/xmonad-contrib"

# Cài đặt GHCup và Stack để build XMonad
echo -e "\e[32m [ INFO ] Install GHCup, stack to build xmonad \e[0m"
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

source "$HOME/.ghcup/env" # Load ghcup env
if command -v "stack" > /dev/null; then
    echo -e "\e[32m [ INFO ] Stack found. Build XMonad. \e[0m"
    cd "$HOME/.config/xmonad"
    stack init
    stack install
else
    echo -e "\e[31m Stack not found. Try using new shell. \e[0m"
    echo "Press Enter to continue"
    read
    bash -c "source $HOME/.ghcup/env && cd $HOME/.config/xmonad && stack init && stack install"
fi

if command -v "xmonad" > /dev/null; then
    echo -e "\e[32m XMonad installed! \e[0m"
else
    echo -e "\e[31m XMonad not installed! \e[0m"
    echo "Press Enter to continue"
    read
fi

# Cấu hình Bluetooth
echo -e "\e[32m [ INFO ] Bluetooth \e[0m"
sudo pacman -S --needed bluez bluez-utils blueman
sudo modprobe btusb
sudo systemctl enable bluetooth
sudo systemctl start bluetooth

yay -S bluetuith

# Cài đặt Ibus-Bamboo
echo -e "\e[32m [ INFO ] Ibus-Bamboo \e[0m"
ibus-daemon &
bash -c "$(curl -fsSL https://raw.githubusercontent.com/BambooEngine/ibus-bamboo/master/archlinux/install.sh)"

# Cài đặt Dark theme
echo -e "\e[32m [ INFO ] Dark theme \e[0m"
sudo pacman -S --needed gnome-themes-extra
yay -S adwaita-qt5-git adwaita-qt6-git

# Cấu hình Rofi
cd "$HOME/dotfiles/rofi"
chmod +x setup.sh
./setup.sh

echo "You must config Rofi manually."
echo "=== DONE ==="
