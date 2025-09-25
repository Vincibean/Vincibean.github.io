---
title: Thinkpad t440p - The Skulls Experience 
---

https://github.com/merge/skulls/issues/258#issuecomment-1489374206


Hi! I have both x230 and t440p and had the same issue only in t440p (Archlinux kernel 6.1.15-lts) which I was able to fix following [flashrom FAQ](https://www.flashrom.org/FAQ).

That is as root:

```bash
modprobe -r lpc_ich
```

Or oneline with update:

```bash
modprobe -r lpc_ich; sleep 4; ./skulls.sh -b t440p
```

No need to enable `iomem=relaxed` in grub parameters.

Hope it helps
