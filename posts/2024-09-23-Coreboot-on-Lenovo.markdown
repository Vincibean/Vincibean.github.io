---
title: I Installed Coreboot on a Lenovo t440p 
---

*This will guide you through the process of installing `coreboot` on the Lenovo t440p with a Raspberry Pi 3 and some of the
accessories that are sold with a `CH341a`.* 

*Using a CH341a is discouraged because it has 5V logic levels on data lines instead of the recommended 3.3V and therefore will
damage your SPI flash and also the southbridge that it's connected to, plus anything else that it's connected to.*

*A Rasperry Pi instead has the recommended 3.3V and hence it can be used instead of the CH341a.*

# Introduction

The [Lenovo ThinkPad t440p](https://support.lenovo.com/nz/en/solutions/pd100280-detailed-specifications-thinkpad-t440p) is a great laptop 
that I bought years ago for the incredible upgradability that it offers, which makes it the perfect workstation for devs and tinkerers.

One of the options I was extremely excited about is the possibility to add a custom BIOS.
I wanted to replace the BIOS because Lenovo's default BIOS comes with a Wi-Fi whitelist that would prevent me from upgrading
the existing Wi-Fi card to a new, more powerful one; so I thought I might as well install a (mostly) Open Source BIOS.

`coreboot`, according to its [Wikipedia page](https://en.wikipedia.org/wiki/Coreboot), is a software project aimed at 
replacing proprietary firmware (BIOS or UEFI) found in most computers with a lightweight firmware designed to perform only 
the minimum number of tasks necessary to load and run a modern 32-bit or 64-bit operating system. Since coreboot initializes 
the bare hardware, it must be ported to every chipset and motherboard that it supports. As a result, coreboot is available only
for a limited number of hardware platforms and motherboard models. 

I quickly found [a nice video on YouTube](https://www.youtube.com/watch?v=pqjsM18pKCE) detailing how to do it. 
In laymen terms, I would have to connect the BIOS chip to another computer via a flasher and a "clip"; the flasher and clip would
transmit electric impulses to the BIOS chip, and these electric impulses would write ("flash") the new BIOS onto the chip.

I soon found (and bought) the flasher I needed: a [CH341a](https://www.amazon.co.uk/KeeYees-SOIC8-EEPROM-CH341A-Programmer/dp/B07SNTL5V6).

With that done, I thought I was ready to flash my new BIOS. Then I finally realized that using the CH341a - the flasher I bought - was discouraged.
According to the [Libreboot documentation](https://libreboot.org/docs/install/spi.html#do-not-use-ch341a) 
(`Libreboot` being one of the coreboot variants partly free of proprietary blobs), using a CH341a is discouraged 
because this flasher has 5V logic levels on data lines instead of the recommended 3.3V and therefore will damage the SPI flash and also the 
southbridge that it’s connected to, plus anything else that it’s connected to.

This was disheartening. I'm not an expert on this topic and the CH341a was the only flasher I knew thanks to the guides I found. 
Despite my best efforts I couldn't find a flasher that would use the recommended 3.3V. In fact, the CH341a was the _only_ flasher I could find.
It's crazy how popular this flasher is considering how dangerous it can be!
Other guides would either not mention different flashers, or mention flashers that I couldn't find on Amazon.

It should be noted that it _could_ have been possible to make my flasher send 3.3V instead of 5V if I had applied [this](https://www.eevblog.com/forum/repair/ch341a-serial-memory-programmer-power-supply-fix/msg1323775/#msg1323775)
fix and bypassed the 1117-3.3V regulator. However, as I mentioned before, I'm not an expert on this topic and I have a very
rough idea of what "bypassing the 1117-3.3V regulator" means; plus, achieving that requires soldering skills that, sadly, I don't have.
So this option was also out of the question.

To make things more complicated, I really wanted to reuse the accessories the CH341a came with: while using the CH341a is undoubtedly dangerous,
as far as I could tell the accessories were just fine and could be safely used for flashing a coreboot BIOS. I already wasted some money on the flasher
itself, I didn't want to buy some new accessories too!

Then I finally found the most popular flasher around: the Raspberry Pi!

Sounds crazy, uh? But the Raspberry Pi is very suitable for this kind of thing as it has a SPI interface and is able to run Linux. 
And, as luck would have it, I already had a Rasperry Pi 3 lying around!

I quickly found [a couple](https://tomvanveen.eu/flashing-bios-chip-raspberry-pi/) [of guides](https://www.instructables.com/Lenovo-T420-Coreboot-WRaspberry-Pi/) 
on how to flash a BIOS chip with a Raspberry Pi. I still had the video guide I mentioned before, plus the 
[(written) guide](https://blog.0xcb.dev/lenovo-t440p-coreboot/) that was used for that video.
All I had to do was mixing them all together. 

So here you have it!

----------------------------------------------------------



```
sudo nala install git build-essential gnat flex bison libncurses5-dev wget zlib1g-dev
```

----------------------------------------------------------------------------------------------

sudo apt update
sudo apt install nala
sudo nala upgrade
sudo reboot
sudo nala upgrade
sudo raspi-config 
sudo nala install git build-essentials gnat flex bison libncurses5-dev wget zlib1g-dev
sudo nala install git build-essential gnat flex bison libncurses5-dev wget zlib1g-dev
sudo poweroff
sudo nala upgrade
sudo raspi-config
cd work/roms/
flashrom -p linu_spi:dev=/dev/spidev0.0,spispeed=512
flashrom -p linux_spi:dev=/dev/spidev0.0,spispeed=512
sudo reboot
cd work/roms/
flashrom -p linux_spi:dev=/dev/spidev0.0,spispeed=512
flashrom -p linux_spi:dev=/dev/spidev0.0,spispeed=512 -r 4mb_backup1.bin
flashrom -p linux_spi:dev=/dev/spidev0.0,spispeed=512 -r 4mb_backup2.bin
diff 4mb_backup1.bin 4mb_backup2.bin 
sudo poweroff
which flashrom
cd work/roms/
flashrom -p linux_spi:dev=/dev/spidev0.0,spispeed=512
ls
ls -latr
flashrom -p linux_spi:dev=/dev/spidev0.0,spispeed=512 -r 8mb_backup1.bin
flashrom -p linux_spi:dev=/dev/spidev0.0,spispeed=512 -c "W25Q64.v" -r 8mb_backup1.bin
flashrom -p linux_spi:dev=/dev/spidev0.0,spispeed=512 -c "W25Q64.V" -r 8mb_backup1.bin
flashrom -L
flashrom -L | less
flashrom -v
flashrom --version
flashrom --help
cd ../
git clone https://github.com/flashrom/flashrom.git
sudo nala remove flashrom
cd flashrom/
make
ls
sudo make install
meson setup builddir
meson compile -C builddir
which flashrom
meson test -C builddir
meson install -C builddir
sudo nala install libmocka
sudo nala install libcmocka
sudo nala install cmocka
sudo nala install libcmocka-dev
meson setup builddir
meson compile -C builddir
meson test -C builddir
meson install -C builddir
sudo su
meson install -C builddir
sudo meson install -C builddir
cd work/roms/
ls
cd ../flashrom/
meson install -C builddir
cd ../flashrom/
cd work/roms/
cd ../flashrom/
meson install -C builddir
cd /home/dre/work/roms/
pwd
sudo nala install xscreensaver
sudo reboot
cd work/coreboot/build/
dd if=coreboot.rom of=bottom.rom bs=1M count=8
dd if=coreboot.rom of=top.rom bs=1M skip=8 
flashrom -p linux_spi:dev=/dev/spidev0.0,spispeed=512
sudo reboot
flashrom -p linux_spi:dev=/dev/spidev0.0,spispeed=512
cd work/coreboot/build/
flashrom -p linux_spi:dev=/dev/spidev0.0,spispeed=512 -w top.rom 
flashrom -p linux_spi:dev=/dev/spidev0.0,spispeed=512 -c "W25Q32BV/W25Q32CV/W25Q32DV" -w top.rom 
flashrom -p linux_spi:dev=/dev/spidev0.0,spispeed=512 -w top.rom 
flashrom -p linux_spi:dev=/dev/spidev0.0,spispeed=512 -c "W25Q32FV" -w top.rom 
sudo poweroff
cd work/coreboot/build/
flashrom -p linux_spi:dev=/dev/spidev0.0,spispeed=512 -c "W25Q32FV"
flashrom -p linux_spi:dev=/dev/spidev0.0,spispeed=512 -c "W25Q32FV" -w top.rom 
sudo poweroff
cd work/coreboot/build/
flashrom -p linux_spi:dev=/dev/spidev0.0,spispeed=512
flashrom -p linux_spi:dev=/dev/spidev0.0,spispeed=512 -c "W25Q64BV/W25Q64CV/W25Q64FV" -w bottom.rom 
sudo poweroff


----------------------------------------------------------------------------------------------------------------------

Coreboot is an open source bios replacement. This guide will describe the steps needed to install it on a Lenovo T440p.

Before you start you should be comfortable using a Linux terminal as well as disassembling your laptop.

There is a chance that this will brick your laptop you do this at your own risk.

## Supplies
- Ponoma 5250 Test Clip - For connecting to the bios chip.
- Female to Female Breadboard Jumper Cables - Also known as Dupont wires.
- Phillips Screwdriver
- Lenovo T440p
- Computer running Linux. "Main PC"
- Raspberry Pi(3 or 4) - running the latest version or Raspberry Pi OS - Instructions on installing can be found here.

## Step 1: Update the Embedded Controller on the T440p

It is a good idea to update the Embedded Controller to the latest version. The easiest way to do this is install the latest version of the factory bios. Coreboot is unable to touch the EC. You will be unable to update it after flashing unless you revert to the factory bios.

## Step 2: Prepare the Raspberry Pi for Flashing. (ON RPI)

In order to read/write to the bios chip you need to enable some kernel modules.

Access the raspberry pi config utility.
```
sudo raspi-config
```
Under interface options enable :
- P2 SSH - if you will be running the pi headless
- P4 SPI
- P5 I2C
- P8 Remote GPIO - If using ssh to connect to the pi

## Step 3: Prepare the 'Main' Computer for Building Coreboot (On Main PC)

First thing to do is install the dependencies needed to build coreboot.

For a Debian based system
```
sudo apt install git build-essential gnat flex bison libncurses5-dev wget zlib1g-dev
```
For an Arch based system
```
sudo pacman -S base-devel gcc-ada flex bison ncurses wget zlib git
```
Make a directory in your home dir to work in. For this example I will be calling it 'work'. You will also want a directory to store the factory images. I will call that directory 'roms' You can do this in one line to save time
```
mkdir -p ~/work/roms
```
Move into the work directory
```
cd ~/work
```
Download the latest version of ME_Cleaner from github
```
git clone https://github.com/corna/me_cleaner
```
Download the latest version of Coreboot
```
git clone  https://review.coreboot.org/coreboot
```
Move into the coreboot directory
```
cd ~/work/coreboot
```
Download the required submodules
```
git submodule update --init --checkout
```
Make a directory to hold some files specific to your T440p it will be needed later.
```
mkdir -p ~/work/coreboot/3rdparty/blobs/mainboard/lenovo/t420
```
Build the ifd tool. This will be used to split the factory bios into it's different regions.
```
cd ~/work/coreboot/utils/ifdtool
make
```
## Step 4: Wire Up the Clip.

Use the 6 female to female wire to connect the clip to the Pi:
- Bios 1 > Pi 24
- Bios 2 > Pi 21
- Bios 4 > Pi 25
- Bios 5 > Pi 19
- Bios 7 > Pi 23
- Bios 8 > Pi 17

Pins 3 and 7 on the Bios are not used.

## Step 5: Access the Bios Chip
With the Pi powered OFF connect the clip to the bios chip.

## Step 7: Read the Flash Chip (On RPI)
Power on the Pi

Create a roms directory and move to it .
```
mkdir -p ~/work/roms

cd ~/work/roms
```
To read and write the chip you will need to use a program called Flashrom. First make sure it is installed
```
sudo apt install flashrom
```
Use flashrom to probe the chip and make sure it is connected
```
flashrom -p linux_spi:dev=/dev/spidev0.0,spispeed=128
```
Read the factory bios off the chip 2 times and save them as factory1.rom and factory2.rom

Use the -c option to specify your flash chip. Make sure to enter everything between the quotes

Each read will take some time depending on the chip it could be between 30-45 min each read. Dont worry if it seems like the pi is hung.
```
flashrom -p linux_spi:dev=/dev/spidev0.0,spispeed=128 -c <chip name> -r factory1.rom

flashrom -p linux_spi:dev=/dev/spidev0.0,spispeed=128 -c <chip name> -r factory2.rom
```

## Step 8: Compare the 3 Files (On RPI)

Next you want to compare the 2 files to make sure you had a good read / connections
```
sha512sum factory*.rom
```
If they all match copy them to the main computer in the ~/work/roms directory.

Power off the Pi. You can leave the clip connected.

## Step 12: Build Coreboot (On Main PC)

Time to compile!

First built the gcc toolchain
```
make crossgcc-i386 CPUS=X
```
X = the number of threads your CPU has.

Build coreboot
```
make iasl
make
```
This will produce a file ~/work/coreboot/build/coreboot.rom.

Power on the Pi and copy that file to your ~/work/roms directory.
## Step 13: Write Coreboot to T440p (On RPI)
Move to the roms directory
```
cd ~/work/roms
```
Probe the chip to make sure its detected
```
flashrom -p linux_spi:dev=/dev/spidev0.0,spispeed=128
```
Write the coreboot image. This will take longer then reading the image.
```
flashrom -p linux_spi:dev=/dev/spidev0.0,spispeed=128 -c <chip name> -w coreboot.rom
```
After the write is verified power off the pi. Remove the Clip and reassemble the T440p.

Congrats you have just flashed Coreboot.

------

This will guide you through the process of installing coreboot on the Lenovo t440p.

The T440p features great upgradability which makes it the perfect dev workstation.

## Materials
- ch341a SPI flasher always disconnect the programmer from the USB port before connecting/disconnecting it from the EEPROM and check if it uses 3.3V
- Alternativly use a Raspberry PI as SPI programmer https://tomvanveen.eu/flashing-bios-chip-raspberry-pi/
- Screwdriver for dissassembly according to the hardware maintenance manual
- Coreboot
- A PC running a Linux Distro (install flashrom with your package manager)
- also install these with your package manager

```
sudo pacman -S base-devel curl git gcc-ada ncurses zlib nasm sharutils unzip flashrom
```

## Organization

To make rebuilding easier create a directory for your backups and blobs. I've called mine ~/t4/

mkdir ~/t4/

## The process
### Disassembly and reading

Next take out the battery and unscrew the access door.
Take it apart until you see both EEPROM-chips next to the RAM:

Assemble the SPI flasher and make sure it's the 3.3v variant:
Connect the SPI flasher to the 4MB (also called top) chip and connect it to your PC. Then:
```
cd ~/t4/

sudo flashrom --programmer ch341a_spi -r 4mb_backup1.bin

sudo flashrom --programmer ch341a_spi -r 4mb_backup2.bin

diff 4mb_backup1.bin 4mb_backup2.bin
```
Only if diff outputs nothing continue - else retry
Then connect to the 8MB (also called bottom) chip and repeat:
```
sudo flashrom --programmer ch341a_spi -r 8mb_backup1.bin

sudo flashrom --programmer ch341a_spi -r 8mb_backup2.bin

diff 8mb_backup1.bin 8mb_backup2.bin
```
Again: only if it outputs nothing continue
### Original ROM

Combine the files to one ROM (the System sees these chips combined anyway)
```
cat 8mb_backup1.bin 4mb_backup1.bin > t440p-original.rom
```
SAVE THE ROM SOMEWHERE SAFE and in multiple locations!!!

###Export blobs

Clean old attepts, pull from github, checkout to master and build ifdtool
```
cd
rm -rf ~/coreboot

git clone https://review.coreboot.org/coreboot 
cd ~/coreboot
git checkout e1e762716cf925c621d58163133ed1c3e006a903
git submodule update --init --checkout

cd util/ifdtool && make
```
Use the rom from before, export the blobs and move them to your t4 folder:
```
./ifdtool -x ~/t4/t440p-original.rom

mv flashregion_0_flashdescriptor.bin ~/t4/ifd.bin

mv flashregion_2_intel_me.bin ~/t4/me.bin

mv flashregion_3_gbe.bin ~/t4/gbe.bin
```
### Obtaining mrc.bin

We'll obtain this blob from a haswell chromebook firmware image (peppy in this case) but first we need to build the cbfstool for extraction:
```
cd ~/coreboot

make -C util/cbfstool

cd util/chromeos

./crosfirmware.sh peppy

../cbfstool/cbfstool coreboot-*.bin extract -f mrc.bin -n mrc.bin -r RO_SECTION

mv mrc.bin ~/t4/mrc.bin
```
### Configuration

Now the fun part :)
Either you add my known working config to ~/coreboot/.config, or you configure coreboot yourself via nconfig.

### Easy and working route: my .config

go back to the coreboot folder and open .config in nano or vim ...
```
cd ~/coreboot
nano .config
```
Paste my config and edit the path to the t4 folder to reflect your path (if you followed me here so far you should just have to change the username to yours). Omit the last two lines if you have no bootsplash image - here is mine
```
CONFIG_USE_OPTION_TABLE=y
CONFIG_TIMESTAMPS_ON_CONSOLE=y
CONFIG_VENDOR_LENOVO=y
CONFIG_CBFS_SIZE=0x200000
CONFIG_LINEAR_FRAMEBUFFER_MAX_HEIGHT=1600
CONFIG_LINEAR_FRAMEBUFFER_MAX_WIDTH=2560
CONFIG_IFD_BIN_PATH="/home/conor/t4/ifd.bin"
CONFIG_ME_BIN_PATH="/home/conor/t4/me.bin"
CONFIG_GBE_BIN_PATH="/home/conor/t4/gbe.bin"
CONFIG_CONSOLE_CBMEM_BUFFER_SIZE=0x20000
CONFIG_TIANOCORE_BOOT_TIMEOUT=2
CONFIG_HAVE_IFD_BIN=y
CONFIG_BOARD_LENOVO_THINKPAD_T440P=y
CONFIG_TIANOCORE_BOOTSPLASH_FILE="/home/conor/t4/bootsplash.bmp"
CONFIG_HAVE_MRC=y
CONFIG_MRC_FILE="/home/conor/t4/mrc.bin"
CONFIG_UART_PCI_ADDR=0x0
CONFIG_VALIDATE_INTEL_DESCRIPTOR=y
CONFIG_H8_SUPPORT_BT_ON_WIFI=y
CONFIG_HAVE_ME_BIN=y
CONFIG_CHECK_ME=y
CONFIG_USE_ME_CLEANER=y
CONFIG_HAVE_GBE_BIN=y
CONFIG_SUBSYSTEM_VENDOR_ID=0x0000
CONFIG_SUBSYSTEM_DEVICE_ID=0x0000
CONFIG_I2C_TRANSFER_TIMEOUT_US=500000
CONFIG_SMMSTORE_SIZE=0x40000
CONFIG_DRIVERS_PS2_KEYBOARD=y
CONFIG_TPM_DEACTIVATE=y
CONFIG_SECURITY_CLEAR_DRAM_ON_REGULAR_BOOT=y
CONFIG_POST_IO_PORT=0x80
CONFIG_PAYLOAD_TIANOCORE=y
CONFIG_TIANOCORE_BOOT_MANAGER_ESCAPE=y
CONFIG_TIANOCORE_SD_MMC_TIMEOUT=1000
```
run nconfig to populate the other options and exit
```
make nconfig
```
(save with F6 and exit with F9)

#### Normal route: your .config

Go back to the coreboot folder and run nconfig, select the device (Mainboard: Lenovo, Mainboard model: T440p, Chipset: add the blobs paths from before)
Then configure to your liking...
```
cd ~/coreboot

make nconfig
```
(save with F6 and exit with F9)

### Building and flashing
```
make crossgcc-i386 CPUS=16

make
```
(CPUS is the number of threads to use so please change it to your PC's specs)

(Skip to "Update process" now if you already have coreboot installed)
Split the built ROM for the 8MB chip (bottom) and the 4MB chip (top)
```
cd ~/coreboot/build

dd if=coreboot.rom of=bottom.rom bs=1M count=8

dd if=coreboot.rom of=top.rom bs=1M skip=8 
```
Connect the programmer to the 4MB chip and run:
```
sudo flashrom --programmer ch341a_spi -w top.rom
```
Connect the programmer to the 8MB chip and run:
```
sudo flashrom --programmer ch341a_spi -w bottom.rom
```

### Boot

and hope for the best ;D

## Update process

Build a new coreboot ROM.
Then set the kernel to iomem=relaxed so it allows internal flashing.

In /etc/default/grub add iomem=relaxed to the space seperated list:
```
GRUB_CMDLINE_LINUX_DEFAULT="iomem=relaxed quit splash"
```
Then apply the config:
```
sudo grub-mkconfig -o /boot/grub/grub.cfg
```
Reboot.

Make a backup of the known working ROM:
```
sudo flashrom -p internal:laptop=force_I_want_a_brick -r ~/t4/coreboot-backup.rom
```
Then flash new ROM:
```
sudo flashrom -p internal:laptop=force_I_want_a_brick -w ~/coreboot/build/coreboot.rom
```

##Reverting to Stock

Remember the backup you made? Good thing you still have it ;)
Either you messed up and can't boot - then you need to hardware flash, or you just want to have the old bios back so you can run hackintosh or whatever...
(In this tutorial the orignal ROM was named t440p.rom but in this part I'll refer to it as original_backup.rom)

### Can't boot

Good thing you saved the t440p-original.rom in multiple places right?
Take you backup ROM and split it so you have a part for the 4MB chip and a part for the 8MB chip (the backup.rom should be 12MB):
```
dd if=t440p-original.rom of=bottom.rom bs=1M count=8

dd if=t440p-original.rom of=top.rom bs=1M skip=8 
```
Connect the programmer to the 4MB chip and run:
```
sudo flashrom --programmer ch341a_spi -w top.rom
```
Connect the programmer to the 8MB chip and run:
```
sudo flashrom --programmer ch341a_spi -w bottom.rom
```
Done.

### Can boot

Take your t440p-original (12MB)
Then set the kernel to iomem=relaxed so it allows internal flashing.

In /etc/default/grub add iomem=relaxed to the space seperated list:
```
GRUB_CMDLINE_LINUX_DEFAULT="iomem=relaxed quit splash"
```
Then apply the config:
```
sudo grub-mkconfig -o /boot/grub/grub.cfg
```
Reboot.

Make a backup of the known working ROM:
```
sudo flashrom -p internal:laptop=force_I_want_a_brick -r ~/t4/coreboot-backup.rom
```
Then flash the backup ROM:
```
sudo flashrom -p internal:laptop=force_I_want_a_brick -w ~/t4/t440p-original.rom
```
Done.

---

Sources:
- [https://hackaday.com/2023/03/06/programming-spi-flash-chips-use-your-pico/](https://hackaday.com/2023/03/06/programming-spi-flash-chips-use-your-pico/)
- [https://www.eevblog.com/forum/repair/ch341a-serial-memory-programmer-power-supply-fix/msg1323775/#msg1323775](https://www.eevblog.com/forum/repair/ch341a-serial-memory-programmer-power-supply-fix/msg1323775/#msg1323775)
- [https://github.com/OpenIPC/wiki/blob/master/en/flash-chip-interfacing.md](https://github.com/OpenIPC/wiki/blob/master/en/flash-chip-interfacing.md)
- [https://libreboot.org/docs/install/spi.html](https://libreboot.org/docs/install/spi.html)
- [https://www.reddit.com/r/libreboot/comments/1ai6qel/raspberry_pi_pico_to_test_clip_wiring/](https://www.reddit.com/r/libreboot/comments/1ai6qel/raspberry_pi_pico_to_test_clip_wiring/)_
- [https://tomvanveen.eu/flashing-bios-chip-raspberry-pi/](https://tomvanveen.eu/flashing-bios-chip-raspberry-pi/)
- [https://libreboot.org/docs/install/spi.html#identify-which-flash-type-you-have](https://libreboot.org/docs/install/spi.html#identify-which-flash-type-you-have)
- [https://www.instructables.com/Lenovo-T420-Coreboot-WRaspberry-Pi/](https://www.instructables.com/Lenovo-T420-Coreboot-WRaspberry-Pi/)
- [https://www.youtube.com/watch?v=pqjsM18pKCE](https://www.youtube.com/watch?v=pqjsM18pKCE)
- [https://blog.0xcb.dev/lenovo-t440p-coreboot/](https://blog.0xcb.dev/lenovo-t440p-coreboot/)
- [https://web.archive.org/web/20231019205252/https://octoperf.com/blog/2018/11/07/thinkpad-t440p-buyers-guide/](https://web.archive.org/web/20231019205252/https://octoperf.com/blog/2018/11/07/thinkpad-t440p-buyers-guide/)
- [https://libreboot.org/docs/install/spi.html#do-not-use-ch341a](https://libreboot.org/docs/install/spi.html#do-not-use-ch341a)
