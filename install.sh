sudo pacman -Syu
sudo pacman -S neofetch ripgrep fzf git base-devel bat dbus eza feh firefox \
			   github-cli kitty xf86-input-libinput xorg-input network-manager-applet \
			   nodejs npm picom polybar pulseaudio pulseaudio-bluetooth python-pynvim xclip rofi xdg-utils \
			   zoxide zsh noto-fonts-emoji ttf-jetbrains-mono-nerd ibus thefuck

read

cp -rf config/* ~/.config/
cp -rf home/* ~

read

sudo cp 30-touchpad.conf /etc/X11/xorg.conf.d

read

sudo usermod -aG video $USER
sudo cp backlight.rules /etc/udev/rules.d

read

git clone https://aur.archlinux.org/yay.git ~/yay
cd ~/yay
makepkg -si
cd ~/dotfiles
rm -rf ~/yay

read

sudo pacman -S \
git \
xorg-server xorg-apps xorg-xinit xorg-xmessage \
libx11 libxft libxinerama libxrandr libxss \
pkgconf

cd ~/.config/xmonad

git clone https://github.com/xmonad/xmonad
git clone https://github.com/xmonad/xmonad-contrib

read

curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
source ~/.bashrc

stack init
stack install

source ~/.bashrc

cd ~/dotfiles

sudo pacman -S bluez bluez-utils blueman
sudo modprobe btusb
sudo systemctl enable bluetooth
sudo systemctl start bluetooth

yay -S bluetuith

sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions

ibus-daemon &
bash -c "$(curl -fsSL https://raw.githubusercontent.com/BambooEngine/ibus-bamboo/master/archlinux/install.sh)"

sudo pacman -S gnome-themes-extra
yay -S adwaita-qt5-git adwaita-qt6-git

cd rofi
chmod +x setup.sh
./setup.sh
cd ~/dotfiles

echo You must config Rofi manualy
