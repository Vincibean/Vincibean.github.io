---
title: Proxmox, Windows and DayZ. A Retrospective 
---
If you've read my previous post, you'll know that I've started experimenting with [homelabbing](https://www.howtogeek.com/what-is-a-homelab-and-how-do-you-start-one/) and more specifically with [Proxmox](https://www.proxmox.com/en/). This post is going to be a continuation of what I've done in my previous post.

---

As an avid [DayZ](https://dayz.com/) and [Bohemia Interactive](https://www.bohemia.net/) fan, it was only a matter of time before I tried to use my new homelabbing skills to set up a DayZ server.

In fact, that may have been the main reason why I wanted to delve into homelabbing to begin with...

I'm nowhere near finished. I feel like this is going to be a pretty long journey with a lot of stuff to do and learn.

Here's what I've done (and learned) so far.

# Before We Even Start: Windows Is Too Bloated!
Seriously, as a Linux person, I can't believe how much stuff a (new and clean) version of Windows comes with!

Given how little RAM and CPU we have at our disposal (Proxmox is giving us just a Virtual Machine after all), the first thing you want to do is remove some bloatware.

Well, technically, the first thing you want to do is give Windows an update. Even if you've just installed it and the update was part of the installation process, you still need to update Windows.

The _second_ thing you want to do is remove all the bloatware. The [Chris Titus Windows Tool](https://christitus.com/windows-tool/) is just perfect for that.

Just run it straight from the Windows Shell, configure it, and let it do its magic. You don't want to choose all those settings yourself? You can use [my own settings](https://github.com/Vincibean/CTT-Configs). I try to keep them updated every time I set up a Windows machine (virtual or otherwise), but feel free to change them.

# Before We Even Start: Does Windows See Windows?
You have your Windows Server, the one we just set up; and you have your Windows machine, the one where (I presume?) you usually play DayZ.

Can these two machines see each other?

In my first attempt, this wasn't the case. Why? I wasn't under the same router. The simplest suggestion I can give you is: make sure that both machines are behind the same router!

Once that is done, you need to tell each machine that you are in a private network (and hence network discovery and file sharing are safe).

The easiest way to do it is: on each machine, open `File Explorer`, go to `Network`, wait for a message saying `Network discovery and file sharing are turned off. Network computers and devices are not visible. Click to change...`. As the message suggests, click it to change. This was enough for me.

If that doesn't work, consider following either one of [these](https://support.microsoft.com/en-us/windows/file-sharing-over-a-network-in-windows-b58704b2-f53a-4b82-7bc1-80f9994725bf) [two](https://www.configserverfirewall.com/windows-10/turn-on-network-discovery-file-sharing-windows-10/) guides.

---

OK, time to focus on our DayZ Server.

I've tried two different ways. Let's see them both.

# The App Way: Steam Client
The simplest way to install a DayZ Server is definitely via [Steam](https://store.steampowered.com/).

I installed it with [`winget`](https://github.com/microsoft/winget-cli) (nowadays, I tend to install everything Windows related using `winget`):
```powershell
winget install --id=Valve.Steam -e
```

Once you have Steam installed, it's time to install the DayZ server proper. There's a lot of nice guides on how to do it. I followed [this one](https://write.corbpie.com/installing-and-setting-up-a-dayz-standalone-server-on-windows-server-2016-guide/), but you could also give [my own guide](https://vincibean.github.io/my-dayz-guide/guide/local-dayz-server.html) a try. 

There really isn't anything else to add here. This worked pretty much flawlessly.

The only issue I'm having with this approach is: the Steam Client seems to have a relatively high footprint. I can see from my dashboards that both CPU and Memory usages are pretty high, even when the server isn't running. At the same time, I've seen Steam struggling a couple of time to display the store page. 

I'm not really _that_ surprised that Steam isn't the lightest app: it needs to do a lot here (displaying a store, updating our library, keeping track of what we and our friends are doing, etc...), and while all of these feature can be desirable in a desktop environment, we don't really need them here. And on top of that, as I've mentioned before, I already don't have many resources at my disposal.

So we need a different, lighter approach.

# The CMD Way: SteamCMD
Valve provides [`SteamCMD`](https://developer.valvesoftware.com/wiki/SteamCMD) as an alternative to the Steam client. 

The `Steam Console Client` or `SteamCMD` is a command-line version of the Steam Client. Its primary use is to install and update various dedicated servers available on Steam using a command-line interface. Exactly what we need!

This way of installing a dedicated server is a little bit less documented, but [these](https://write.corbpie.com/dayz-server-setup-and-install-on-windows-server-2019-with-steamcmd/) [two](https://steamcommunity.com/sharedfiles/filedetails/?id=2390568079) guides (the first one in particular) have been very helpful.

I installed `SteamCMD` with (you guessed it!) `winget`:
```powershell
winget install --id=Valve.SteamCMD -e
```

Then I needed a place for my server to live. I wanted it to be a nice, easy to remember folder, but all of the folders I had in mind ended up requiring admin privileges. Surprisingly, creating a folder under `C:` didn't need any privilege, so in the end I created a new folder there. I called `servers` (hence the full path was: `C:\servers`).

I created a new script to deal with the whole installing. I called my installation script `installdayzserver.bat`.
So its full path was: `C:\servers\installdayzserver.bat`

```powershell
@echo off
set "serverpath=C:\servers\dayzserver"
set /p login=Steam Login: 
echo.
set /p pass=Steam Password:
echo.
%steamcmdpath%\steamcmd +force_install_dir "%serverpath%" +login %login% %pass% +app_update 223350 validate +quit
```

One nice consequence of the fact that we used `winget` is that we don't need to care too much about the full path of `SteamCMD`: `winget` took care of it, so we can just call it with `steamcmd`.

Note: the script needs third party verification. If you have the Steam app installed on your phone, you will get an authorization request from your phone.

That not only the DayZ server needs `DirectX`; it also needs `Visual C++`.

To my great joy, both of these can be installed with `winget`:

```powershell
winget install -e --id Microsoft.DirectX
```

And:

```powershell
winget install -e --id Microsoft.VCRedist.2015+.x64
```

Once that is done, all that is left to do is to run the server. There is a script for that (that will work irrespective of whether you installed the DayZ server with the Steam Client or with SteamCMD):

```powershe
@echo off
:start
::Server files location
set serverLocation="C:\servers\dayzserver"
::Server Port
set serverPort=2302
::Server config
set serverConfig=serverDZ.cfg
::Server profile
set serverProfiles=profiles
::Logical CPU cores to use (Equal or less than available)
set serverCPU=4
:: Go to DayZServer location
cd "%serverLocation%"
::Launch parameters (edit end: -profiles=|-config=|-port=|-cpuCount=|-doLogs|-adminLog|-netLog|-freezeCheck|-filePatching|-BEpath=)
start "DayZ Server" /min "DayZServer_x64.exe" -config=%serverConfig% -port=%serverPort% -profiles=%serverProfiles% -cpuCount=%serverCPU% -dologs -adminlog -netlog -freezecheck
```

Just run the script and your DayZ server will start running (but be patient, it may take a while).

After that, you can connect to it via your DayZ client. Your server will be listed under `LAN`.

---

That's it! That's all I've done so far.

Now you could be wondering: "What about everything else? What about mod support? What happens when mods get updated and your server needs to update them? What if I need to schedule my server to restart on a regular basis?"

I'm planning to use yet another tool to install a DayZ server, and that tool should come with a lot of nice functionalities to take care of all of this, but if you really want to do all that now with the tools at our disposal (and a lot of scripts), I cannot recommend [this website](https://write.corbpie.com/dayz-server-setup-and-install-on-windows-server-2019-with-steamcmd/) enough. It covers all of this and more.

See you later and have fun!
