---
title: My first month as an indie game developer 
---

After years, I've finally decided to dabble into game development. There are many factors that have led me to this decision.

First of all, I've been working as a software developer for years now, and I think I've finally come to a point where new things don't really seem new anymore; the same concepts seem to be used across the board. This is actually really good, and will help me massively; it means that, even though I don't have any experience in game development, I should be able to pick up the main concepts easily. Yeah. "_Easily_".

Secondly, I think I've come to terms with the fact that it's going to take me years before I have something ready. This is also okay. Let me give you an example. [Kenshi](https://store.steampowered.com/app/233860/Kenshi/) is one of my favourite games. It took the developer ([Chris Hunt](https://twitter.com/lofigames?lang=en)) roughly ten years (_ten years!_) before it was finished. However, I think that, like good wine, that time only made it better. Chris strongly believed in his product and what he was doing and today, looking at the final product, it shows! Kenshi is huge, innovative, with endless possibilities, loads of mods and a strong fan base. For all his patience and his decade long continued passion for what he was doing, Chris Hunt is one of my personal heroes. And who doesn’t love his hair? Totally to die for! :D

Lastly, I've been playing a lot of video games lately. I think the pandemic is only partially responsible for this, at least in this case; I've always had quite a conflicting relationship with video games: back when I was a teenager I used to spend most of my time playing games and very little time with other human beings, and that led me to become… a bit of a weirdo. At the time I was very ashamed of it, and I tried to remedy it by suddenly quitting my video games addiction and trying to make friends instead. Needless to say, it didn't work ^^'

Nowadays I accept - and embrace - my weirdness! This has _nothing_ to do with the fact that my attempts to make friends failed miserably. Nothing at all! ^^'

Anyway (erm!) lately I don't see videogames as the source of all my misery, but simply as another way to spend my time. Interestingly, I've recently started to play multiplayer games and realized that they could have provided the kind of connection with peers that I was looking for when I was younger.

I've started to particularly enjoy multiplayer, open world survival games. I like the sense of progression I get from these games, with your loot, your technology, and your effectiveness as a player improving over time. I also like the fact that there is no enforced objective in these games: it is up to you as a player to make sense of the wipe. Some like the PvP aspects of the game; some like the PvE aspects; some like to meet new players; some like the challenge and therefore give themselves impossible constraints; some like to get into goofy and hilarious situations; some like to… erm… address other players with racist slurs, apparently.

I've been playing this genre for a while now and tried several games, but each and every one of them seems to lack a certain something that would make it the perfect game for me.

Here’s some of the games I’ve played:

[Rust](https://store.steampowered.com/app/252490/Rust/)? I love its atmosphere, the focus on base building, the fact that the world is procedurally generated, the flexibility you have in playing the game, and [Facepunch](https://facepunch.com/)'s approach to game development, but I hate the fact that it's such a time sink, completely unplayable for a more casual player. 

[DayZ](https://store.steampowered.com/app/221100/DayZ/)? I love the player controllers, its more tactical focus, the fact that it's more approachable for a casual player (no one is going to loot your body while you're off working, for example), and once again [Bohemia Interactive](https://www.bohemia.net/)'s approach to the development of the game, but I hate its complexity and its atmosphere (or, more precisely, I grew tired of its atmosphere after seeing it copy-pasted so many times in so many games).

[Deadside](https://store.steampowered.com/app/895400/Deadside/)? I love its AI, how even it is in treating the bots as if they were actual players, how fun PvE can be, and in general how approachable it is for casual players, but just like DayZ I grew tired of its atmosphere.

[7 Days to Die](https://store.steampowered.com/app/251570/7_Days_to_Die/)? I love its atmosphere, the fact that everything in the game is lootable and destructible, and once again the fact that the world is procedurally generated, but I hate how complex it is, the poor support for PvP, the fact that, once again, your enemies are zombies, and how primlocked you are (or, at least, I am); it takes ages to get a gun, and even then there isn't too much need for it, since (most) zombies don't shoot at you; also, will I ever see it get out of alpha?

[Hurtworld](https://store.steampowered.com/app/393420/Hurtworld/)? I love its atmosphere; I hate the fact that it's not supported anymore, and how little you can do with the environment; you can't even chop trees!

And so, after months and months of searching for the perfect game, I've decided to make it myself.

So, first things first: what do I want? I want the best traits of all those popular survival games, but none of their flaws. I want a procedurally generated world where everything (_everything_) is fully destructible, a multiplayer first person shooter where AIs could be looted as if they were actual players (that's right: no more zombies!), with nice post-apocalyptic vibes and a focus on gunfights for PvP and PvE.

Okay, how can I achieve that? Firstly, if I don't want to reinvent the wheel (and I _don't want_ to reinvent the wheel!) I need to choose a game engine. The best candidates for 3D FPS games are [Unity](https://unity.com/) and [Unreal](https://www.unrealengine.com/).

So, Unity or Unreal? Unity seems to be more approachable: there's more tutorials available on the Internet. And most of the games I like and look up to are made with it. Rust? Unity. [Subnautica](https://store.steampowered.com/app/264710/Subnautica/)? Unity. [Unturned](https://store.steampowered.com/app/304930/Unturned/)? Unity. Hurtworld? Unity. 7 Days to Die? Unity.

Let's go with Unity then.

I should begin with a prototype. Generally, when I prototype something, I begin with whatever I expect to be the most difficult to code.

Of all the features that I've mentioned before, the ones that frighten me the most are: multiplayer support, enemy AI, and fully destructible environment. I randomly picked the latter to start with.

When I think of a fully destructible environment, the most notorious example that comes to mind is [Minecraft](https://www.minecraft.net/).

(However, there was one game called [Ace of Spades](https://en.wikipedia.org/wiki/Ace_of_Spades_(video_game)) that would have been an even better fit; do you guys know it? Have you ever tried it?)

Anyway, back to Minecraft. I searched "Minecraft in Unity" on YouTube and found a surprisingly high amount of videos. In the end, I decide to follow what I think is the most complete: [a nice series](https://www.youtube.com/playlist?list=PLVsTSlfj0qsWEJ-5eMtXsYp03Y9yF1dEn) from [b3agz](https://www.youtube.com/c/b3agz).

(Actually, first I follow [Brackey](https://www.youtube.com/c/Brackeys)'s tutorial on [how to make your first video game in Unity](https://www.youtube.com/playlist?list=PLPV2KyIb3jR53Jce9hP7G5xC4O9AgnOuL). But we don't talk about that, do we? ^^')

During the "course" I create my first _voxel_, then my first _chunk_, then apply textures, procedurally generate hills and biomes, do some [gardening](https://www.youtube.com/watch?v=4pzLLwGl_n0&list=PLVsTSlfj0qsWEJ-5eMtXsYp03Y9yF1dEn&index=11), and finally I create [a dangerous, relentless virus that leaves a suspicious green substance on top of my chunks](https://www.youtube.com/watch?v=DjQ6yFRuZ7Q&list=PLVsTSlfj0qsWEJ-5eMtXsYp03Y9yF1dEn&index=31)…

Most importantly, I learn some of the techniques needed to optimise the game, and what a dramatic effect they can have on performance.

I also learn how challenging all this can be. I mean, I've been working on my prototype for only one month and it's already full to the brim with weird bugs and glitches!

The more I work on this, the more I realize that I can't code the voxel engine myself. I need to use a pre-existing solution. Unreal boasts a pretty nice one ([Voxel Plugin](https://voxelplugin.com/)) which seems to have everything I need. Most importantly, Voxel Plugin seems to be well documented and maintained.

Unity's candidates… not so much!

However, Unity itself is well documented. There are more tutorials out there about Unity alone than all the other game engines combined! But Unreal seems to be more naturally geared towards what I'm trying to achieve: some of the functions that I would have to code (or buy) in Unity come out of the box in Unreal.

Therefore, I find myself wondering which one I should use: Unity or Unreal? I guess that the best course of action would be to try them both: create the same prototype with each of them and then make an informed decision. The bad news is that now, instead of one prototype, I need to work on two (and with two different technologies!) The good news is that I'll have loads of content for the future, I guess ^^'

Anyways, back to b3agz's course. Overall, I liked it. Except for the fact that it's not a course at all! At the end of it I didn't have a fully functional Voxel engine, but lessons and techniques to code a game. I know, I know, what I've learnt along the way is more valuable than any given line of code, and I agree with you, but was it so wrong to get lessons _and_ a fully functional Voxel engine in the end? :(

What I really got from all this is a whole new level of confidence: coding a game wasn't the impossible, unapproachable endeavour I thought it was (although I think I would have coded everything in a different way and I'm not sure I would be able to redo everything without looking back at the tutorial)

I was even told that just moving (_moving!_) in the editor was quite a challenge; in all fairness, even with my unfamiliarity with game development, I didn't find it that hard. I suppose that spending years playing [Black & White](https://en.wikipedia.org/wiki/Black_%26_White_(video_game)) helped here. Man, I miss that game!

However, this hobby is very multidisciplinary, especially as a solo indie developer. I'll need more than just coding skills. I'll need art, scores, sound effects, net code, and probably something else that I don't even know exists. It will take me some time to have at least a basic grasp of all this.

I think that I am at a bit of a disadvantage when compared to other game developers; some terms that might sound familiar to others devs, like "meshes" and "UV mapping" and "shaders", were completely unfamiliar to me. Hell, I'm still not too familiar with them! This slows me down, but also gives me more interesting stuff to learn. And learning is awesome! Yay!

(By the way, at the end of the course/playlist I discovered that b3agz has another course/playlist on [how to create a game like 7 Days to Die](https://www.youtube.com/playlist?list=PLVsTSlfj0qsWt0qafrT6blp5yvchzO4ee). I should have picked that!)