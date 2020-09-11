# Arch Linux Installation
### My process to install my favourite linux distribution

**Create bootable pendrive**

```
dd bs=4M if=path/to/arch.iso of=/dev/sd(pendrive) status=progress && sync
```

**Connect to internet**

check if NI is listed:
```
ip link
```

enter iwd:
```
iwctl
```

```
device list
```

scan the networks:
```
station "device" scan
```

list available networks:
```
station "device" get-networks
```

connect:
```
station "device" connect "network"
```
enter password

exit iwd

**Create partitions**

with cfdisk, create the following partitions:

- EFI System [syze = 512M] minimum
- Linux Swap [syze = (RAM size)]
- Root Partition [syze = (remaining disk space)]

**Give partitions a file system**

root:
```
mkfs.fat -F32 /dev/sda(number of root partition)
```

efi system:
```
mkfs.ext4 /dev/sda(number of efi partition)
```

**Make and activate swap**

```
mkswap /dev/sda(number of swap partition)

swapon /dev/sda(number of swap partition)
```

**Mount partitions**

Mount root:
```
mount /dev/sda(number of root partition) /mnt
```

Create efi directory:
```
mkdir /boot/efi
```

Mount efi:
```
mount /dev/sda(number of efi partition) /boot/efi
```

**Select mirrors**

Sync package manager to the pacman repository:
```
pacman -Syy
```

Install "reflector" for fresh and fast nearby mirrors:
```
pacman -S reflector

reflector -c "US" -f 12 -l 10 -n 12 --save /etc/pacman.d/mirrorlist
```

**Install firmware, linux and editors**

Using pacstrap script:
```
pacstrap /mnt base linux linux-firmware vim nano
```

**Configure file system usage**

Generate a fstab file:
```
genfstab -U /mnt >> /mnt/etc/fstab
```

**Configure installed arch**

Use arch-chroot pointing to the just mounted root partition:
```
arch-chroot /mnt
```

**Timezone**

Change timezone to desired area:
```
timedatectl set-timezone America/Sao_Paulo
```

**Language**

Open /etc/locale.gen with a text editor and uncomment the desired language:
```
vim /etc/locale.gen
```

Generate config file int /etc:
```
locale-gen

echo LANG=en_US.UTF-8 > /etc/locale.conf

export LANG=en_US.UTF-8
```

**Network**

Create /etc/hostname and add the computer's name:
```
echo ArchLinux > /etc/hostname
```

Create the host files:
```
touch /etc/hosts
```

Open /etc/hosts with vim and add:

```
127.0.0.1   localhost
::1         localhost
127.0.0.1   (hostname you chose earlier)
```

**Password**

```
passwd
```

**Install grub bootloader**

Install required packages:
```
pacman -S grub efibootmgr
```

Install grub:
```
grub-install --target=x86_64-efi --bootloader-id=GRUB --efi-directory=/boot/efi
```

Create grub config file:
```
grub-mkconfig -o /boot/grub/grub.cfg
```

**Gksu**
```
sudo pacman -S gksu
```

**Users**

Enable "wheel" group:
```
sudo EDITOR=vim visudo
```

Uncomment the following line:
```
# %wheel ALL=(ALL) NOPASSWD: ALL
```

Add user:
```
sudo useradd -m josuetm
```

Add password:
```
sudo passwd josuetm
```

Open sudoers:
```
sudo EDITOR=vim visudo
```

Add user permission: (under root)
```
josuetm ALL=(ALL) ALL
```

## Post Instalation

**install terminal emulator**
```
pacman -S termite
```

**Install base-devel pakg**
```
pacman -S base-devel
```

**Install xorg**
```
pacman -S xorg
```

### Install display manager

**LightDM**
```
sudo pacman -S lightdm
sudo pacman -S lightdm-webkit2-greeter
```

Configure lightdm:
```
vim /etc/lightdm/lightdm.conf
```

Uncomment and add the following:
```
greeter-session = lightdm-webkit2-greeter
user-session = awesome
```

Install lightdm screenlocker:
```
sudo pacman -S light-locker
```

Enable lightdm:
```
systemctl enable lightdm
```

optional
**gnome**
```
sudo pacman -S gnome
```

Enable gnome:
```
systemctl enable gdm.service
```
optional

**Load NetworkManager**
```
systemctl enable NetworkManager.service
```

### Install window manager
```
sudo pacman -S awesome
```

Create config file:
```
mkdir -p ~/.config/awesome/
```

Copy from template file:
```
cp /etc/xdg/awesome/rc.lua ~/.config/awesome/
```

Optional packages I use:
- thunar: file manager
- rofi: file/app finder
- gvim: programming and file editing
- Compton: visual effects
- Firefox: browser
- Git: Version Control

**Install codecs/plugins**

```
sudo pacman -S a52dec faac faad2 flac jasper lame libdca libdv libmad libmpeg2 libtheora libvorbis libxv wavpack x264 xvidcore gstreamer0.10-plugins
```

or just install vlc media player that comes with all of them:
```
sudo pacman -S vlc
```

**install fonts**
```
sudo pacman -S ttf-proggy-clean
sudo pacman -S adobe-source-pro-fonts
sudo pacman -S terminus-font
sudo pacman -S ttf-dejavu
sudo pacman -S noto-fonts
sudo pacman -S gnu-free-fonts
sudo pacman -S ttf-liberation
sudo pacman -S ttf-ubuntu-font-family
sudo pacman -S ttf-linux-libertine
sudo pacman -S ttf-inconsolata
sudo pacman -S ttf-sarasa-gothic
sudo pacman -S ttf-roboto
sudo pacman -S ttf-hack
```

**Install visual plugins**
```
sudo pacman -S compton
sudo pacman -S nitrogen
```

**Install GUIMP toolkit**
```
sudo pacman -S gtk2
sudo pacman -S gtk3
```

**Install themes**
```
sudo pacman -S arc-gtk-theme
```

**Install softwares**

```
sudo pacman -S firefox libreoffice git
```

**Install archive managers**

```
sudo pacman -S p7zip p7zip-plugins unrar tar rsync
```

**Install yay for AUR**

Clone repository:
```
sudo git clone https://aur.archlinux.org/yay-git.git 
```

Build:
```
cd yay
makepkg -si
```

**Install snap**
```
yay -S snapd
```

**Install polybar**

```
yay -S polybar
```

Copy config file:
```
mkdir ~/.config/polybar/

cp /usr/share/doc/polybar/config ~/.config/polybar/
```

Make file on ~/.config/polybar/launch.sh
```
touch ~/.config/polybar/launch.sh
```

Add "launch.sh" to awesome

**Useful directories**

- awesome: ~/.config/awesome/

- lightdm: /etc/lightdm/

- polybar: ~/.config/polybar/

- rofi: ~/.config/rofi/

- vim: ~/.vimrc
