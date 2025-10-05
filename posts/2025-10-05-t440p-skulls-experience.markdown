---
title: ThinkPad T440p - The Skulls Experience 
---

As you may already know from one of my past posts, I've flashed [`coreboot`](https://www.coreboot.org/) as the BIOS for my [Lenovo ThinkPad T440p](https://support.lenovo.com/gb/en/solutions/pd100280-detailed-specifications-thinkpad-t440p);
I found the process quite difficult and under-documented - at least for a noob like myself -
so I decided to write a post that could serve as a guide for other people.

Later on, I found myself having trouble updating coreboot. The error message led me to believe that
the whole flashing process didn't go as well as I thought it did, a notion that, as we will see later,
may actually have been wrong.

Worse yet, I didn't manage to flash coreboot again, due to a mismatch between the sizes of the two images.

All of a sudden I thought I was doomed to live with a faulty coreboot installation. 

That was until I discovered `Skulls`.

[`Skulls`](https://github.com/merge/skulls) is a project that offers pre-built coreboot images, making it easy to install an unlocked,
up-to-date and easy to use coreboot-based BIOS on your laptop.

Why "unlocked"? Because the software has no restrictions for connected hardware.

Why "easy to use"? Because Skulls comes with `SeaBIOS`, an open-source implementation of an x86 BIOS,
which is essentially just a boot menu, no frills.

Why "up to date"? Because Skulls is frequently updated with a new image with the latest versions of all components.
OK, this one was easy to guess.

Skulls also claims to have an easy installation process. And, I'll be honest, they hit the nail on the head here.
Just look at [these](https://github.com/merge/skulls/blob/master/t440p/README.md) docs for the T440p!
Had I known that these docs existed I wouldn't have written my own post.

I did the installation process, which unfortunately required some more wires and jumpers, but fortunately went smoothly,
and now my T440p is sporting a brand new BIOS from Skulls.

And now to the question that everyone's asking: are you able to update the BIOS now?

Well, it's complicated...

The official docs state [here](https://github.com/merge/skulls/blob/master/t440p/README.md#updating) that to do an update (i.e. flash internally)
all you have to do is:

1. boot Linux with the `iomem=relaxed` boot parameter (for example in `/etc/default/grub` `GRUB_CMDLINE_LINUX_DEFAULT`)
2. [download](https://github.com/merge/skulls/releases) the latest Skulls release tarball and unpack it or check for updates by running `./skulls.sh -b t440p -U`.
3. run `sudo ./skulls.sh -b t440p` and choose the image to flash.

Well, this didn't work for me. In fact, the error message is suspiciously similar to the original error I had.

How to fix it then? Well, it turns out that I'm not the only one having this issue. There's an issue on GitHub [here](https://github.com/merge/skulls/releases) about this.

The solution comes from this comment [here](https://github.com/merge/skulls/issues/258#issuecomment-1489374206) from
absolute legend [cthulux](https://github.com/cthulux), who in turn refers to [flashrom FAQ](https://www.flashrom.org/FAQ).

Here's the solution

As root, do:

```bash
modprobe -r lpc_ich
```

Or, if you want to do it on one line coupled with the normal Skulls update, as root again:

```bash
modprobe -r lpc_ich; sleep 4; ./skulls.sh -b t440p
```

According to cthulux there's no need to boot Linux with the `iomem=relaxed` boot parameter.
In my case, I had the `iomem=relaxed` boot parameter set, so I cannot confirm if this is really not needed.

And that's all! Problem solved.

Skulls (and coreboot) have been pretty nice to use.
Although not simple, and maybe not the ideal solution for a BIOS power user who may wants to have a higher degree of control over their BIOS, for me - a noob who doesn't need much beyond selecting which drive to boot from, it has been okay.

The only issue I've had (and this may not be due to Skulls) is that sometimes booting from USB doesn't work.

However, after much tinkering, I think I've found a workaround: when flashing your ISO to USB, make sure to flash using [DD Image (disk image) mode](https://www.msftnext.com/what-are-iso-and-dd-image-modes-in-rufus/).

For instance, if you're using [Rufus](https://rufus.ie/en/), you should set it something like this:

![Rufus in DD Image (disk image) mode](../assets/img/2025-10-05/rufus.png "Rufus in DD Image (disk image) mode")

That's all! Hope this helps!
