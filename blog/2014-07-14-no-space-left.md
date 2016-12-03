----
title: 'btrfs: No space left on device'
tags: Linux, btrfs
----

I've been using btrfs on a SSD for around six months now. I have a slightly unusual setup: btrfs on LVM, even though [btrfs lets you create subvolumes and resize them at will](https://wiki.archlinux.org/index.php/Btrfs#Sub-volumes). My reasoning was that I might not necessarily stick with btrfs, but I'd appreciate the flexibility to change my partition layouts and use other filesystems.

Anywho, around a week ago, I started seeing messages like the following:

``` {.pre .sourceCode}
No space left on device
```

This was odd. `df -h` doesn't report any full volumes. Doing `btrfs check` reports no errors, and playing around with my LVM settings didn't change anything. What could be going wrong?

``` {.pre .sourceCode}
Filesystem              Size  Used Avail Use% Mounted on
/dev/dm-0                15G  368M   13G   3% /
udev                     10M     0   10M   0% /dev
tmpfs                   1.6G  2.7M  1.6G   1% /run
tmpfs                   3.8G  136K  3.8G   1% /dev/shm
tmpfs                   3.8G     0  3.8G   0% /sys/fs/cgroup
tmpfs                   5.0M     0  5.0M   0% /run/lock
tmpfs                    16G  768K   16G   1% /tmp
tmpfs                   100M  8.0K  100M   1% /run/user
/dev/sda1               1.9G   42M  1.7G   3% /boot
/dev/mapper/linux-opt    15G   19M   13G   1% /opt
/dev/mapper/linux-var   7.5G  2.4G  4.4G  36% /var
/dev/mapper/linux-usr    15G   11G  3.5G  75% /usr
/dev/mapper/linux-home  299G  159G  138G  54% /home
```

I panicked for a bit, thinking my SSD was failing, before deleting a few gigabytes of downloaded files and forgetting about the problem like it never existed.

But they came back.

``` {.pre .sourceCode}
cp: cannot create directory ‘linux-kernel/kernel/arch/um’: No space left on device
cp: cannot create directory ‘linux-kernel/kernel/arch/ia64’: No space left on device
cp: cannot create directory ‘linux-kernel/kernel/arch/parisc’: No space left on device
cp: cannot create directory ‘linux-kernel/kernel/arch/unicore32’: No space left on device
cp: cannot create directory ‘linux-kernel/kernel/arch/x86’: No space left on device
cp: cannot create directory ‘linux-kernel/kernel/arch/powerpc’: No space left on device
```

I looked around everywhere. `tmpfs` wasn't full, but I tried mounting it explicitly in `/etc/fstab` with 16GiB of space just to be sure.

Eventually my system became unusable.

## Solution

By some magic, I happened upon [this btrfs wiki page](https://btrfs.wiki.kernel.org/index.php/Problem_FAQ#I_get_.22No_space_left_on_device.22_errors.2C_but_df_says_I.27ve_got_lots_of_space). They warn about metadata. First, a verification that `home` isn't full:

``` {.pre .sourceCode}
root@dionysus:/# btrfs filesystem show
Label: 'home'  uuid: 4216df47-7fb8-442f-b432-5e732ab24166
	Total devices 1 FS bytes used 147.94GiB
	devid    1 size 298.02GiB used 298.02GiB path /dev/mapper/linux-home

Btrfs v3.14.1
```

And surely enough:

``` {.pre .sourceCode}
root@dionysus:/# btrfs fi df /home
Data, single: total=292.50GiB, used=145.70GiB
System, DUP: total=8.00MiB, used=40.00KiB
System, single: total=4.00MiB, used=0.00
Metadata, DUP: total=2.75GiB, used=2.25GiB
Metadata, single: total=8.00MiB, used=0.00
unknown, single: total=512.00MiB, used=0.00
```

Note `Metadata, DUP` - that's 81% usage. The btrfs wiki says that 75% or greater metadata space usage can cause `No space left on device` errors, even when the filesystem isn't full.

> For now, a workaround is to run a partial balance:
>
> ``` {.pre .sourceCode}
> $ sudo btrfs filesystem balance start -dusage=5 /mount/point
> ```

The `-dusage=5` bit means only chunks that are at most 5% used will be relocated.

Running `btrfs filesystem balance` spreads usage across the disk and reclaims allocated but unused metadata chunks.

``` {.pre .sourceCode}
root@dionysus:/# btrfs fi balance start -dusage=5 -v /home
Dumping filters: flags 0x1, state 0x0, force is off
  DATA (flags 0x2): balancing, usage=5
Done, had to relocate 0 out of 303 chunks
```

And now, the `Metadata, DUP` field shows 72% usage. Better, I suppose.

``` {.pre .sourceCode}
root@dionysus:/# btrfs fi df /home
Data, single: total=285.50GiB, used=151.25GiB
System, DUP: total=8.00MiB, used=40.00KiB
System, single: total=4.00MiB, used=0.00
Metadata, DUP: total=3.25GiB, used=2.34GiB
Metadata, single: total=8.00MiB, used=0.00
unknown, single: total=512.00MiB, used=0.00
```

I'll be exploring this more if I see any more issues, and update this post with information. For reference, I run a custom Linux 3.15 release, and have built and booted the 3.16-rc1, rc2, and some in-between RCs.
