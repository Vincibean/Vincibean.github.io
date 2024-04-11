---
title: Steps to Unbrick a OnePlus Nord (`avicii`)
---

Some time ago I installed LineageOS on my smartphone, a OnePlus Nord (`avicii`); however, after a major update of LineageOS / Android, the WiFi just stopped working.
The reason? The LineageOS update didn't come with an update to the WiFi firmware.
The solution? Re-install the original OS ([OxygenOS](https://en.wikipedia.org/wiki/OxygenOS), an Android-based operating system developed by OnePlus exclusively for their smartphones).
But how? I installed LineageOS precisely because I didn't want anything to do with OxygenOS; I threw the whole OxygenOS out of the window thinking I wouldn't need it anymore; how could I reinstall it on my phone?

Just as with my old Samsung Galaxy II (`i9100`), it took me a disproportionate amount of time to find a solution online, and even more to find one that would actually work with my phone.
The problem is the huge amount of deprecated guides, broken links, wrong instructions, forums that used to house the information I needed but are now closed for good, copycat guides, etc... 
In summary, it's a mess!

However, I somehow did it and now the WiFi firmware on my phone is up to date.

In the unlikely event that I will ever need to go through this procedure again, I'm leaving here a short guide on how to do that again, just like I [did](https://github.com/Vincibean/i9100) with my old Samsung Galaxy II.
Unfortunately, this time around I cannot leave the files here on GitHub (even after zipping everything, it's still a whopping 3GB!) and according to GitHub my repository is "over its data quota"; therefore, this time around I'm leaving the files on Google Drive; hopefully I'll remember not to delete them.

> **_Disclaimer:_** By attempting any of the processes listed in this post you accept full responsibility for your actions. I will not be held responsible if your device stops working, catches fire, or turns into a hipster and claims to have been modified before it was cool.

In this guide we will restore a OnePlus Nord phone to factory settings using the tool that the service centers use. 
This tool go by the name of `MSM tool` (or `EDL package`, or `unbrick tool`) and work on both locked and unlocked bootloaders.
It can revive a bricked OnePlus Nord. It can also rollback our phone to a previous release of OxygenOS if we would like to go back to an older firmware, like in our case.
however, it will completely wipe our phone of all data (pictures, music, app data, etc), so make sure you make a backup and save it to your PC first.


** Instructions:

1. Download the MSM tool [here](https://drive.google.com/file/d/1zeKgols_2xrGxGC_7Jf5-lxnP1FBKltL/view?usp=drive_link)
2. Unzip the content of the zip file in a folder of your choice
3. Launch the MSM Download Tool on your PC (double click on the `MsmDownloadTool V4.0.exe` file for that) 
4. On the login prompt under `User Type` select `Other` in the dropdown menu and click on `Next`
5. Now click on the `Target` button. From the drop-down menu that appears, select `EU` (we are using the European tool)
6. Press the `Start` button. This should be situated at the top left of the tool. This is done so that the device will be "captured" automatically by the tool instead of going back to normal boot after 10 seconds
7. Power off your device and leave it in that state for a minute or so.
8. Boot your OnePlus Nord into `Qualcomm EDL Mode`. To do that, press and hold the `Volume Up` and `Volume Down` keys for at least 40 seconds. Note that your screen will not post anything while in Qualcomm EDL mode. In other words, it will look like your phone is still powered off, but trust me, if you've done everything correctly your phone is now in Qualcomm EDL Mode.
9. Plug your device to your computer using the official (stock) OnePlus USB cable (recommended). You may let volume keys go once done.
10. Wait for around 300-400 seconds and let the tool do its job. After this, your device will then boot to the Android OS.
11. Enjoy your newly rolled back device.

** FAQ:

> Does this work on Mac or Linux?
Unfortunately no, tool is Windows only. You should need at least Windows 7.

> Why is my antivirus freaking out when unzipping the archive or running the tool?
In an effort to protect reverse engineering from being done, OnePlus now use VM Protect V3 in their MSM tools. As this tries to detect debug environment, this is seen as malicious behaviour by some antivirus.

> My device isn't detected
Go to device manager and make sure your phone shows up as QDLOADER 9008.
If it shows up as `QHUSB_BULK`, it means Qualcomm driver wasn't installed automatically by Windows Update. 
Download the latest one from Microsoft website [here](http://download.windowsupdate.com/c/msdownload/update/driver/drvs/2017/03/fe241eb3-d71f-4f86-9143-c6935c203e12_fba473728483260906ba044af3c063e309e6259d.cab) ([source](https://www.catalog.update.microsoft.com/Search.aspx?q=qualcomm%20hs%20usb%209008)) and install it manually by right clicking on `QHUSB_BULK` and selecting `Update driver software` and `Browse my computer for driver software` to where you downloaded CAB file.

> MSM tool is stuck on "Param pre-processing"
Ensure you're using the Qualcomm drivers linked above.

> MSM tool is stuck on "Sahara communication failed"
Unplug your phone, get in fastboot mode, turn off phone, wait 15 secondes and get back in Qualcomm EDL mode. You can also try using a USB 2.0 port instead of a 3.0 one.

---

_Originally posted [here](https://xdaforums.com/t/opnord-oos-ac01aa-ba-da-unbrick-tool-to-restore-your-device-to-oxygenos.4148415/)_

_Some instructions were originally posted [here](https://www.getdroidtips.com/unbrick-oneplus-nord/)_
 
