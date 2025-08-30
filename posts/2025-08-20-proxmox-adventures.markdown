---
title: My Proxmox Adventures
---
I've started experimenting with [homelabbing](https://www.howtogeek.com/what-is-a-homelab-and-how-do-you-start-one/) and more specifically with [Proxmox](https://www.proxmox.com/en/). 

Proxmox (also known as Proxmox Virtual Environment, or PVE) is a virtualization platform based on a modified Ubuntu LTS kernel designed for the provisioning of hyper-converged infrastructure. It allows the deployment and management of virtual machines and containers.

Here's my experience so far.


**Note**

**This is not meant to be a guide. It's mostly a "hey, look what I've done!" kind of guide, with a bit of "I'm writing this down so that I'll know where to find it". Don't take anything written in here too seriously. I'm still learning this stuff.**


# Before We Begin: Which Guide To Follow?
I like video tutorials, and among the many that I've found on YouTube, [this one](https://www.youtube.com/watch?v=qmSizZUbCOA) from [TechHut](https://www.youtube.com/@TechHut) was really useful, especially the first part, up until to (and including) `FIX THOSE REPOS` (i.e. until [7:56](https://www.youtube.com/watch?v=qmSizZUbCOA&t=476s))

[These](https://www.youtube.com/watch?v=_sfddZHhOj4) [two](https://www.youtube.com/watch?v=BoMlfk397h0) videos from [Hardware Haven](https://www.youtube.com/@HardwareHaven) have also been very helpful. He has a slightly different take on the tutorial format that I appreciated.

# BIOS Setup
This was pretty easy. I pretty much enabled everything (which is a testament of how few options the BIOS of my computer has).

The only one suggestion that nobody else seems to give is: if, like me, you are repurposing an old PC as a Server, install Windows first (you can download Windows 11 for free, even without a Product Key) and then update the BIOS; for better or worse, BIOS manufacturers seem to favour Windows when they release BIOS updates, so if you want an updated BIOS Windows is your best bet.

# Installer: `nomodeset`
Right off the bat I had a problem here: the installation media did not boot to the installer. Instead, the computer got stuck. This seems to be caused by an issue with the graphics drivers that are present in the installer.

Disabling the Intel graphics features fixed it. You can do that by setting the `nomodeset` option before boot.

To do that, when you see the `GNU GRUB` bootloader screen, move the cursor to the option you intend to select and press `E`.

An editor will open. Use the arrow keys to locate the end of the line that starts with `linux /boot/vmlinuz***`, or, if you prefer, the line with `quiet` in it.

Once that is done, click `Ctrl + x` and the boot will start again (and this time it won't stop. Hopefully)

# Administrator email
I've heard that it is preferable to insert a real email address here, as this is actually used by the system. I don't know for what, but you have been warned.

# Hostname, Host IP, Gateway, DNS
This was a bit more difficult. In fact, this was _very_ difficult! I pretty much had to redo everything from scratch because of this page only! And not just once!

The most difficult bit, and the one that can have an effect on the others, is the `Gateway`, so let's start with that one.

## Finding the default gateway
In simple terms, a [Gateway](https://en.wikipedia.org/wiki/Gateway_(telecommunications)) is a piece of hardware (or software) used in telecommunications to let data flow from one network to another. Intuitively, you can think of it as the router.

To find the default gateway on a Linux system, I used the `ip route` command. In the results, look for the line with `default via` in the output; this will tell you the gateway's IP address.

For example:

```bash
$ ip route
default via 10.10.10.1 dev wlan0 proto dhcp src 10.10.10.233 metric 600 
10.10.10.0/24 dev wlan0 proto kernel scope link src 10.10.10.233 metric 600 
10.10.10.1 dev wlan0 proto dhcp scope link src 10.10.10.233 metric 600
```

In this example, the gateway would be: `10.10.10.1`

## Host IP
Once you know the Gateway, knowing the Host IP is relatively easy: you don't really need to _know_ it, you need to _choose_ it. 

Given that the host IP is the IP of your host, i.e. your Proxmox server, you just need to choose an IP that belongs to the same network defined by the Gateway.

If your network is `10.10.10.1`, then your network will start with `10.10.10`, so you just need to choose the last number. The only restriction you have here is that the address shouldn't be shared with _any_ other device in your network.

## DNS
Not too sure about this one. I used `1.1.1.1`, as that's the one I used more often when `ping`ing. It's also near my location.

I'm not sure this is correct, even after doing [some research](https://www.reddit.com/r/Proxmox/comments/13fc7o4/do_i_need_to_change_my_dns_server/), but so far it has worked fine.

## Hostname
I've simply used `pve.lan` here. There seems to be more research needed than I expected here, but this seems to work fine so far.

---

In the end you should have something like this:


| Option               | Selected value                   |
|----------------------|----------------------------------|
| Bootdisk filesystem  | ext4                             |
| Bootdisk(s)          | /dev/sda                         |
| Timezone             | Europe/London                    |
| Keyboard layout      | U.S. English                     |
| Administrator email  | my.very.secret.email@example.com |
| Management interface | enp3s0                           |
| Hostname             | pve.lan                          |
| Host IP (CIDR)       | 10.10.10.123/24                  |
| Gateway              | 10.10.10.1                       |
| DNS                  | 1.1.1.1                          |

You're ready to install Proxmox and start playing with it!

# Windows GPU Passthrough
To absolutely no-one's surprise, having a Windows Virtual Machine is a PITA. 
This great [guide](https://www.reddit.com/r/homelab/comments/b5xpua/the_ultimate_beginners_guide_to_gpu_passthrough/) from this [absolute legend](https://www.reddit.com/user/cjalas/) helped me a lot.

If you're a video tutorial enjoyer like me, this [video tutorial](https://www.youtube.com/watch?v=S6jQx4AJlFw) from TechHut is also good: he pretty much just follows the guide.

You are going to need Windows, of course. I used Windows 11. You can download it from [here](https://www.microsoft.com/en-us/software-download/windows11).

You are also going to need VirtIO drivers. You can find them [here](https://fedorapeople.org/groups/virt/virtio-win/direct-downloads/stable-virtio/virtio-win.iso) or [here](https://pve.proxmox.com/wiki/Windows_VirtIO_Drivers).

The only things I had to do differently were:
- that `machine: q35` when setting the options for the VM is not needed
- that `cpu: host,hidden=1,flags=+pcid` also doesn't seem to be needed. I'm not too sure about this though. Proxmox will happily provide its own (in my case, it is `cpu: x86-64-v2-AES`) and for me this seems to work just fine. Your mileage may vary.
- it is not possible to install Windows without an internet connection like in the guide; in order to fix this you just do a change the content of the CD-Rom drive and install the VirtIO drivers, like you did before for storage


---

That's it! That's what I've done so far.

# Sources
- [https://www.howtogeek.com/what-is-a-homelab-and-how-do-you-start-one/](https://www.howtogeek.com/what-is-a-homelab-and-how-do-you-start-one/)
- [https://www.proxmox.com/en/](https://www.proxmox.com/en/)
- [https://www.dell.com/support/kbdoc/en-us/000123893/manual-nomodeset-kernel-boot-line-option-for-linux-booting](https://www.dell.com/support/kbdoc/en-us/000123893/manual-nomodeset-kernel-boot-line-option-for-linux-booting)
- [https://www.youtube.com/watch?v=qmSizZUbCOA](https://www.youtube.com/watch?v=qmSizZUbCOA)
- [https://www.youtube.com/playlist?list=PLT98CRl2KxKHnlbYhtABg6cF50bYa8Ulo](https://www.youtube.com/playlist?list=PLT98CRl2KxKHnlbYhtABg6cF50bYa8Ulo)
- [https://en.wikipedia.org/wiki/Gateway_(telecommunications)](https://en.wikipedia.org/wiki/Gateway_(telecommunications))
- [https://serverfault.com/questions/31170/how-to-find-the-gateway-ip-address-in-linux](https://serverfault.com/questions/31170/how-to-find-the-gateway-ip-address-in-linux)
- [https://www.reddit.com/r/Proxmox/comments/13fc7o4/do_i_need_to_change_my_dns_server/](https://www.reddit.com/r/Proxmox/comments/13fc7o4/do_i_need_to_change_my_dns_server/)
- [https://www.youtube.com/watch?v=S6jQx4AJlFw](https://www.youtube.com/watch?v=S6jQx4AJlFw)
- [https://www.microsoft.com/en-us/software-download/windows11](https://www.microsoft.com/en-us/software-download/windows11)
- [https://fedorapeople.org/groups/virt/virtio-win/direct-downloads/stable-virtio/virtio-win.iso](https://fedorapeople.org/groups/virt/virtio-win/direct-downloads/stable-virtio/virtio-win.iso)
- [https://pve.proxmox.com/wiki/Windows_VirtIO_Drivers](https://pve.proxmox.com/wiki/Windows_VirtIO_Drivers)
- [https://www.reddit.com/r/homelab/comments/b5xpua/the_ultimate_beginners_guide_to_gpu_passthrough/](https://www.reddit.com/r/homelab/comments/b5xpua/the_ultimate_beginners_guide_to_gpu_passthrough/)
- [https://woshub.com/create-windows-vm-proxmox/](https://woshub.com/create-windows-vm-proxmox/)
- [https://pve.proxmox.com/wiki/Windows_10_guest_best_practices](https://pve.proxmox.com/wiki/Windows_10_guest_best_practices)
