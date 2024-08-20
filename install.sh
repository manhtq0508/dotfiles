if [ ! -e "$HOME/dotfiles" ]; then
	echo "Please clone this repo to $HOME"
	exit
fi

echo "[*] Install package"
sudo pacman -Syu
sudo pacman -S --needed neofetch ripgrep fzf git base-devel bat dbus eza feh firefox \
	       github-cli kitty xf86-input-libinput xorg-input network-manager-applet \
	       nodejs npm picom polybar pulseaudio pulseaudio-bluetooth python-pynvim \
	       rofi xdg-utils zoxide zsh noto-fonts-emoji ttf-jetbrains-mono-nerd \
	       ibus thefuck xclip

echo "[*] Copy dotfiles and config files"
if [ ! -e "$HOME/.config" ]; then
	mkdir $HOME/.config
fi
cp -rf config/* $HOME/.config/
cp -rf home/.* $HOME

echo "[*] Config touchpad (natural scrolling, tap, etc)"
sudo cp 30-touchpad.conf /etc/X11/xorg.conf.d

echo "[*] Need by Polybar's backlight module (adjust brightness)" 
sudo usermod -aG video $USER
sudo cp backlight.rules /etc/udev/rules.d

echo "[*] Install yay"
git clone https://aur.archlinux.org/yay.git $HOME/yay
cd $HOME/yay
makepkg -si
cd $HOME/dotfiles
sudo rm -rf $HOME/yay $HOME/go

echo "[*] Requirements of xmonad (Build from source)"
sudo pacman -S \
git \
xorg-server xorg-apps xorg-xinit xorg-xmessage \
libx11 libxft libxinerama libxrandr libxss \
pkgconf

if [ ! -e "$HOME/.config/xmonad" ]; then
	mkdir $HOME/.config/xmonad
fi
git clone https://github.com/xmonad/xmonad $HOME/.config/xmonad/xmonad
git clone https://github.com/xmonad/xmonad-contrib $HOME/.config/xmonad/xmonad-contrib

echo "[*] Install GHCup, stack to build xmonad"
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
source $HOME/.bashrc

if command -v "stack" > /dev/null 2>&1; then 
	stack init $HOME/.config/xmonad
	stack install $HOME/.config/xmonad
else 
	echo "Stack not found. Please check and try again."
	echo "Press Enter to continue"
	read
fi
source $HOME/.bashrc

echo "[*] Bluetooth"
sudo pacman -S bluez bluez-utils blueman
sudo modprobe btusb
sudo systemctl enable bluetooth
sudo systemctl start bluetooth

yay -S bluetuith

echo "[*] Oh-my-zsh"
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}/plugins/zsh-autosuggestions

echo "[*] Ibus-bamboo"
ibus-daemon &
bash -c "$(curl -fsSL https://raw.githubusercontent.com/BambooEngine/ibus-bamboo/master/archlinux/install.sh)"

echo "[*] Dark theme"
sudo pacman -S gnome-themes-extra
yay -S adwaita-qt5-git adwaita-qt6-git

# Rofi
cd $HOME/dotfiles/rofi
chmod +x setup.sh
./setup.sh

echo You must config Rofi manuall
