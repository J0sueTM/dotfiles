# Arch Linux Installation
### My process to installing my favourite linux distribution

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

1 - EFI System [syze = 512M] minimum
2 - Linux Swap [syze = (RAM size)]
3 - Root Partition [syze = (remaining disk space)]

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

## Post Instalation

**install terminal emulator**
```
pacman -S termite
```

**Install xorg**
```
pacman -S xorg
```

### Install display manager

**LightDM**
I use lightdm:
```
sudo pacman -S lightdm
sudo pacman -S lightdm-gtk-greeter
```

Configure lightdm startup:
```
systemctl start lightdm.service
systemctl enable lightdm.service
```

Install lightdm screenlocker:
```
sudo pacman -S light-locker
```

Load lightdm:
```
systemctl enable lightdm
```

**gnome**
```
sudo pacman -S gnome
```

Load:
```
systemctl start gdm.service
systemctl enable gdm.service
```

**Load NetworkManager**
```
systemctl enable NetworkManager.service
```

**Install window manager**
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
- vicious: widget api
- Naughty: notifications
- thunar: file manager
- rofi: file/app finder
- gvim: programming and file editing
- Compton: visual effects
- Firefox: browser
- lain: extra layouts to awesome manager

**Install visual plugins**
```
sudo pacman -S compton
sudo pacman -S nitrogen
```

**Install codecs/plugins**

```
sudo pacman -S a52dec faac faad2 flac jasper lame libdca libdv libmad libmpeg2 libtheora libvorbis libxv wavpack x264 xvidcore gstreamer0.10-plugins
```

or just install vlc media player that comes with all of them:
```
sudo pacman -S vlc
```

**Install softwares**

```
sudo pacman -S firefox libreoffice
```

**Install archive managers**

```
sudo pacman -S p7zip p7zip-plugins unrar tar rsync
```
