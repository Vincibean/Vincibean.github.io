---
title: Void Linux Adventures
---

Introduction
-------------

```bash
xbps-install git
mkdir src
cd src
git clone https://github.com/void-linux/void-packages
cd void-packages
./xbps-src binary-bootstrap
echo XBPS_ALLOW_RESTRICTED=yes >> etc/conf
./xbps-src pkg b43-firmware # for 6.30.163.46, b43-firmware-classic for 5.100.138
sudo xbps-install -R hostdir/binpkgs/nonfree b43-firmware # for 6.30.163.46, b43-firmware-classic for 5.100.138
sudo poweroff # or reboot

sudo sv down dhcpcd
# sudo sv down wpa_supplicant
sudo rm /var/service/dhcpcd
sudo ln -s /etc/sv/dbus/ /var/service/
sudo xbps-install NetworkManager
sudo ln -s /etc/sv/NetworkManager/ /etc/runit/runsvdir/default/ # or you could just do: sudo ln -s /etc/sv/NetworkManager/ /var/service/

sudo usermode -a -G network [your-username]
sudo poweroff # or reboor, or just log ogg and then log in again
sudo xbps-install polkit
sudo ln -s /etc/sv/polkitd/ /var/service/
sudo poweroff # or reboot, or just log ogg and then log in again
sudo sv status polkitd
sudo sv status dbus
sudo sv status NetworkManager
nmtui
```
