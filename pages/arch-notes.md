---
title: Arch Linux security notes
status: scratchpad
tags: notes
---

## Security

- install
    - `linux-hardened`
    - `apparmor`
    - `auditd`

### Check status of kernel security modules

Default settings:

```default
# cat /sys/kernel/security/{lsm,lockdown}
capability,landlock,lockdown,yama,bpf
[none] integrity confidentiality
```

### apparmor status

```default
# aa-enabled ; aa-status
No - disabled at boot.
apparmor module is loaded.
apparmor filesystem is not mounted.
```

I'm using `ukify`, so I added the `apparmor` option to my kernel params in my `/usr/local/bin/rebuild-ukis-for-sbctl.sh` script.

### auditd

```default
$ sudo groupadd -r audit
$ sudo gpasswd -a $USER audit
Adding user $USER to group audit
```

In `/etc/audit/auditd.conf`:

```default
log_group = audit
```

### unified kernel image (uki) inspection

```default
# for f in *; do echo -n "$f: options "; objcopy -O binary --only-section=.cmdline "$f" /dev/stdout; echo; done
arch.efi: options root=/dev/mapper/vg-main rw rd.luks.name=[...]=cryptroot rootflags=subvol=@ /boot/efi/EFI/Linux/arch-secure.efi
arch-hardened.efi: options root=/dev/mapper/vg-main rw rd.luks.name=[...]=tpm2-device=auto lsm=capability,landlock,lockdown,yama,apparmor,bpf
arch-hardened-troubleshoot.efi: options root=/dev/mapper/vg-main rw rd.luks.name=[...]=cryptroot rootflags=subvol=@ log_level=7 rd.debug
arch-troubleshoot.efi: options root=/dev/mapper/vg-main rw rd.luks.name=1b6944df-369f-474b-97b5-76375d6449kcc=cryptroot rootflags=subvol=@ log_level=7 rd.debug
```

## Laptop

### Battery health

```default
# pacman -S tlp
# systemctl enable --now tlp
# cat <<EOF > /etc/tlp.d/50-battery.conf

# 00-template.conf - Template for TLP drop-in customizations
# See full explanation: https://linrunner.de/tlp/settings
#
START_CHARGE_THRESH_BAT0=75
STOP_CHARGE_THRESH_BAT0=90```
```

To check for and (apparently) optimize battery, install `powertop`:[^cpufreq-stats]

[^cpufreq-stats]: `cpufreq_stats` is part of the kernel now and `modprobe` will fail.

```
# powertop --auto-tune
modprobe cpufreq_stats failed
Failed to mount debugfs!
Should still be able to auto tune...
Cannot load from file /var/cache/powertop/saved_results.powertop
Cannot load from file /var/cache/powertop/saved_parameters.powertop
File will be loaded after taking minimum number of measurement(s) with battery only
RAPL device for cpu 0
RAPL Using PowerCap Sysfs : Domain Mask 5
RAPL device for cpu 0
RAPL Using PowerCap Sysfs : Domain Mask 5
Devfreq not enabled
glob returned GLOB_ABORTED
Cannot load from file /var/cache/powertop/saved_parameters.powertop
File will be loaded after taking minimum number of measurement(s) with battery only
Leaving PowerTOP
```

### SSD

Install the `smartmontools` package.

```default
# systemctl enable --now smartd
```

For drive health pulls, use `smartctl -a /dev/nvme0n1`[^smartctl].

[^smartctl]: A week into my Framework 13:

    ```default
    # sudo smartctl -a /dev/nvme0n1
    smartctl 7.5 2025-04-30 r5714 [x86_64-linux-6.13.12-hardened1-2-hardened] (local build)
    Copyright (C) 2002-25, Bruce Allen, Christian Franke, www.smartmontools.org

    === START OF INFORMATION SECTION ===
    Model Number:                       WD_BLACK SN7100 2TB
    Serial Number:                      24461T802167
    Firmware Version:                   7612M0WD
    PCI Vendor/Subsystem ID:            0x15b7
    IEEE OUI Identifier:                0x001b44
    Total NVM Capacity:                 2,000,398,934,016 [2.00 TB]
    Unallocated NVM Capacity:           0
    Controller ID:                      1
    NVMe Version:                       2.0
    Number of Namespaces:               1
    Namespace 1 Size/Capacity:          2,000,398,934,016 [2.00 TB]
    Namespace 1 Formatted LBA Size:     512
    Namespace 1 IEEE EUI-64:            001b44 8b40fa508d
    Local Time is:                      Fri May  9 03:06:23 2025 EDT
    Firmware Updates (0x14):            2 Slots, no Reset required
    Optional Admin Commands (0x0017):   Security Format Frmw_DL Self_Test
    Optional NVM Commands (0x00df):     Comp Wr_Unc DS_Mngmt Wr_Zero Sav/Sel_Feat Timestmp Verify
    Log Page Attributes (0x7e):         Cmd_Eff_Lg Ext_Get_Lg Telmtry_Lg Pers_Ev_Lg Log0_FISE_MI Telmtry_Ar_4
    Maximum Data Transfer Size:         128 Pages
    Warning  Comp. Temp. Threshold:     90 Celsius
    Critical Comp. Temp. Threshold:     94 Celsius
    Namespace 1 Features (0x92):        NA_Fields NP_Fields *Other*

    Supported Power States
    St Op     Max   Active     Idle   RL RT WL WT  Ent_Lat  Ex_Lat
     0 +     4.60W    4.60W       -    0  0  0  0        0       0
     1 +     3.00W    3.00W       -    0  0  0  0        0       0
     2 +     2.50W    2.50W       -    0  0  0  0        0       0
     3 -   0.0200W       -        -    3  3  3  3     2000    3000
     4 -   0.0050W       -        -    4  4  4  4     4000   12000
     5 -   0.0030W       -        -    5  5  5  5   176000   25000

    Supported LBA Sizes (NSID 0x1)
    Id Fmt  Data  Metadt  Rel_Perf
     0 +     512       0         2
     1 -    4096       0         1

    === START OF SMART DATA SECTION ===
    SMART overall-health self-assessment test result: PASSED

    SMART/Health Information (NVMe Log 0x02, NSID 0xffffffff)
    Critical Warning:                   0x00
    Temperature:                        29 Celsius
    Available Spare:                    100%
    Available Spare Threshold:          10%
    Percentage Used:                    0%
    Data Units Read:                    550,467 [281 GB]
    Data Units Written:                 614,641 [314 GB]
    Host Read Commands:                 4,341,638
    Host Write Commands:                7,858,138
    Controller Busy Time:               6
    Power Cycles:                       28,231
    Power On Hours:                     7
    Unsafe Shutdowns:                   28,080
    Media and Data Integrity Errors:    0
    Error Information Log Entries:      0
    Warning  Comp. Temperature Time:    0
    Critical Comp. Temperature Time:    0
    Temperature Sensor 1:               30 Celsius
    Temperature Sensor 2:               30 Celsius

    Error Information (NVMe Log 0x01, 16 of 256 entries)
    No Errors Logged

    Self-test Log (NVMe Log 0x06, NSID 0xffffffff)
    Self-test status: No self-test in progress
    No Self-tests Logged
    ```
