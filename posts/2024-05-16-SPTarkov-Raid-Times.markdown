---
title: How to Change Raid Times in SPT-AKI (SPTarkov)
---

With the recent drama with Escape from Tarkov's [Unheard Edition](https://www.forbes.com/sites/mikestubbs/2024/04/25/new-escape-from-tarkov-unheard-edition-has-a-full-single-player-mode/) and the [accusations](https://www.msn.com/en-us/sports/other/battlestate-games-accuse-arena-breakout-infinite-of-plagiarising-from-tarkov/ar-AA1nIaAQ?ocid=ob-fb-frfr-839) of assets thefts from Battlestate Games towards Arena Breakout Infinite, many players are (re-)evaluating `SPT-AKI` (a.k.a. Single Player Tarkov, or `SPTarkov`).

As someone who has always had an on-and-off relationship with Escape From Tarkov and hasn't invested too much time into learning the maps, I've always found the timed raids a bit annoying.
While I understand why it's needed in a multiplayer environment (you want to clean up and release resources for other players), it doesn't make too much sense when playing single player. 
When you (the single player) get out of raid, the resources are released and the "server" (your machine) can get ready for your (and yours only) next raid.

Fortunately, SPT-AKI offers a simple solution, requiring just a bit of configs change.

Here's how to do it:

- in the folder where you installed your SPT-AKI server, go to `AKI_data/Server/database/locations` 

- each folder here represents a different map: `bigmap` is Customs, `woods` is Woods, `interchange` is Interchange, `rezerbase` is Reserve, `shoreline` is Shoreline, `lighthouse` is Lighthouse, `tarkovstreets` is Streets of Tarkov, `factory4_day` is Factory (day time), `factory4_night` is Factory (night time); open the folder of the map you're interested in

- open the `base.json` file, search for the config parameter `EscapeTimeLimit` and change the number (the number of minutes per raid) to whatever you want.

- if you have a very old version of SPT-AKI, the parameter name may be `escape_time_limit`(but you should really consider updating SPT-AKI) 

You can change the times to be (almost) unlimited: just use an unreasonably high number like `9999`.

Of course, you can also use a dedicated mod for this. Here are some examples:
- https://hub.sp-tarkov.com/files/file/771-custom-raid-times/
- https://hub.sp-tarkov.com/files/file/941-simple-raid-config/

Pretty cool, uh?

---

Source:
[https://www.reddit.com/r/SPTarkov/comments/pfnx34/increase_raid_time_limit/](https://www.reddit.com/r/SPTarkov/comments/pfnx34/increase_raid_time_limit/)
