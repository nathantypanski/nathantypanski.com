---
title: Arch Linux security notes
status: scratchpad
tags: notes, 
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

### Unified Kernel Image (uki) inspection

```default
# for f in *; do echo -n "$f: options "; objcopy -O binary --only-section=.cmdline "$f" /dev/stdout; echo; done
arch.efi: options root=/dev/mapper/vg-main rw rd.luks.name=[...]=cryptroot rootflags=subvol=@ /boot/efi/EFI/Linux/arch-secure.efi
arch-hardened.efi: options root=/dev/mapper/vg-main rw rd.luks.name=[...]=tpm2-device=auto lsm=capability,landlock,lockdown,yama,apparmor,bpf
arch-hardened-troubleshoot.efi: options root=/dev/mapper/vg-main rw rd.luks.name=[...]=cryptroot rootflags=subvol=@ log_level=7 rd.debug
arch-troubleshoot.efi: options root=/dev/mapper/vg-main rw rd.luks.name=1b6944df-369f-474b-97b5-76375d6449kcc=cryptroot rootflags=subvol=@ log_level=7 rd.debug
```

## TPM

References:

- [Protecting SSH authentication with TPM 2.0](https://www.sstic.org/media/SSTIC2021/SSTIC-actes/protecting_ssh_authentication_with_tpm_20/SSTIC2021-Article-protecting_ssh_authentication_with_tpm_20-iooss.pdf)[^protecting-ssh-tpm-archive]

[^protecting-ssh-tpm-archive]: PDF [archived @ 2025-05-10](SSTIC2021-Article-protecting_ssh_authentication_with_tpm_20-iooss.pdf)

### View current tpm crypt status for partition

For example, to see measured PCRs enforced for tpm device unlock:

```default
# cryptsetup luksDump /dev/nvme0n1p2 | grep hash-pcrs
        tpm2-hash-pcrs:   0+6+7
```

### Dump all PCR hashes

To see all the measurements for the current boot:

```default
# systemd-analyze pcrs
NR NAME                SHA256
 0 platform-code       [                            sha256                            ]
 1 platform-config     [                            sha256                            ]
 2 external-code       [                            sha256                            ]
 3 external-config     [                   (same as external-code)                    ]
 4 boot-loader-code    [                            sha256                            ]
 5 boot-loader-config  [                            sha256                            ]
 6 host-platform       [                            sha256                            ]
 7 secure-boot-policy  [                            sha256                            ]
 8 -                   0000000000000000000000000000000000000000000000000000000000000000
 9 kernel-initrd       [                            sha256                            ]
10 ima                 0000000000000000000000000000000000000000000000000000000000000000
11 kernel-boot         [                            sha256                            ]
12 kernel-config       0000000000000000000000000000000000000000000000000000000000000000
13 sysexts             0000000000000000000000000000000000000000000000000000000000000000
14 shim-policy         0000000000000000000000000000000000000000000000000000000000000000
15 system-identity     [                            sha256                            ]
16 debug               0000000000000000000000000000000000000000000000000000000000000000
17 -                   ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
18 -                   ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
19 -                   ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
20 -                   ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
21 -                   ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
22 -                   ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
23 application-support 0000000000000000000000000000000000000000000000000000000000000000
```

Or to see the relevant ones for a block device (e.g., `nvme0n1p2`):

```default
# cryptsetup luksDump /dev/nvme0n1p2 --dump-json-metadata \
    | jq -r '.tokens[]|select(.type=="systemd-tpm2")|."tpm2-pcrs"|@csv' \
    | tr ',' '\n' \
    | xargs -I{} tpm2_pcrread -o pcrs 'sha256:{}'
```

### Arbitrary key storage:w

```default
# pacman -S tpm2-tools tpm2-abrmd
```

#### Session manager daemon

Use this if you want your regular user to be able to acces the TPM. It's still enforcing based on PCRs, but we should be careful who we let call the TPM unseal functions.

```default
# systemctl enable --now tpm2-abrmd
```

Access is governed by membership in the `tss` group.
As your regular user:

```default
$ usermod -aG "$(whoami)" tss
```

Now log back in and you can run many of these commands as a non-root user.

#### Policy creation

First, start a session. The `session.ctx` file will be used to exit the session
later.

```default
$ tpm2_startauthsession --policy-session -S session.ctx
```

Now write a PCR policy to `pcr.policy`, matching that of `${DEVICE}`.

```default
$ export DEVICE='/dev/nvme0n1p2'
$ export PCRS="$(cryptsetup luksDump "${DEVICE}" --dump-json-metadata \
    | jq -r '.tokens[]|select(.type=="systemd-tpm2")|."tpm2-pcrs"|@csv')"
$ echo "${PCRS}"
0,6,7
```

If we like those settings, we can match them. Otherwise, have a look at `tpm2_pcrread`'s output.

```default
$ tpm2_policypcr \
    --session policy.session \
    --policy pcr.policy \
    --pcr-list "sha256:${PCRS}"
```

Now end your session.

```default
$ tpm2_flushcontext policy.session
$ rm policy.session
```

#### create a parent key

You need a top-level parent key to store objects.

First, list available algos:

```default
$ tpm2_getcap algorithms | grep -E '^[a-z][a-z0-9\-_]+:' | sed 's|:||g'
rsa
sha1
hmac
aes
keyedhash
xor
sha256
sha384
[ ... 14 lines omitted ]
kdf2
ecc
symcipher
ctr
ofb
cbc
cfb
ecb
```

Create your primary key with algorithms of your selection. The defaults are bad,
like RSA and AES CBC. I picked ecc384 with aes-256-ctr, which are the best
available algorithms for my TPM.[^primary-algos]

```default
$ tpm2_createprimary \
    --hierarchy o \
    -g sha384 \
    -G ecc384:aes256ctr \
    -o 'prim384-public.pem' \
    -c prim384.ctx
name-alg:
  value: sha384
  raw: 0xc
attributes:
  value: fixedtpm|fixedparent|sensitivedataorigin|userwithauth|restricted|decrypt
  raw: 0x30072
type:
  value: ecc
  raw: 0x23
curve-id:
  value: NIST p384
  raw: 0x4

[ ... 12 lines omitted ... ]

sym-alg:
  value: aes
  raw: 0x6
sym-mode:
  value: ctr
  raw: 0x40
sym-keybits: 256
x: [ redacted by author ]
y: [ redacted by author ]

```

Confirm the output matches your expectations.

#### Seal a blob

Let's say I have a secret that I would like to protect.
We can protect that secret using the TPM. Note you may have to send `^D` (CTRL+D)

```default
$ tpm2_create \
    --parent-context=prim384.ctx \
    --sealing-input=- \
    --policy=pcr.policy \
    --public=blob-sealed-p384.pub \
    --private=blob-sealed-p384.priv
  value: sha256
  raw: 0xb
attributes:
  value: fixedtpm|fixedparent
  raw: 0x12
type:
  value: keyedhash
  raw: 0x8
algorithm:
  value: null
  raw: 0x10
keyedhash: [ redacted by author ]
authorization policy: [ redacted by author ]
```

It will read the secret via stdin. You can pipe the secret into it.

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

```default
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

[^primary-algos]: [Avoid RSA](https://blog.trailofbits.com/2019/07/08/fuck-rsa/). [P-384](https://www.johndcook.com/blog/2019/05/11/elliptic-curve-p-384/) is the recommended elliptic curve until post-quantum cryptography.
    [CFB](https://en.wikipedia.org/wiki/Block_cipher_mode_of_operation#Cipher_feedback_(CFB)) is also the default algorithm for the symmetric side of the key. This is old; it's a good idea to move to [CTR](https://en.wikipedia.org/wiki/Block_cipher_mode_of_operation#Counter_(CTR)).
