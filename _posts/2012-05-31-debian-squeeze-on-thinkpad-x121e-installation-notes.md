---
id: 801
title: Debian Squeeze on Thinkpad X121e installation notes
date: 2012-05-31T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2012/05/31/debian-squeeze-on-thinkpad-x121e-installation-notes/
permalink: /2012/05/31/debian-squeeze-on-thinkpad-x121e-installation-notes/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
Here are some notes on installing [Debian Squeeze](http://wiki.debian.org/DebianSqueeze) on a Thinkpad X121e. Before following this guide, check the system details (at the end of this post) to see if you have the same model of graphics card, wifi, ethernet, etc. </p> 

## Installation notes 

The ethernet card is not detected by the Debian installer, so at the first opportunity flip to a console, load the atl1c driver, and echo this magic string:

<pre>modprobe atl1c
echo "1969 1083" &gt; /sys/bus/pci/drivers/atl1c/new_id
</pre>

(credit: <http://wiki.debian.org/InstallingDebianOn/Thinkpad/X121e-30515YG>) 

## Wrong resolution in xorg 

The installation process runs ok but on rebooting the screen resolution in xorg was 1024&#215;768 instead of 1366&#215;768 so everything looked weird and squashed. Upgrading to a 3.2 kernel fixes the problem. Add this line to /etc/apt/sources.list:

<pre>deb http://backports.debian.org/debian-backports squeeze-backports main
</pre>

then install the new kernel: 

<pre>sudo apt-get update
sudo apt-get install -t squeeze-backports  linux-image-3.2.0-0.bpo.2-amd64  # (previously said: linux-image-2.6.39-bpo.2-amd64 which is wrong)
</pre>

After rebooting into the new kernel the resolution should be ok. If graphics are slow (e.g. moving an xterm flickers) then also grab the backported xorg packages:

<pre>sudo apt-get install -t squeeze-backports xorg xserver-xorg xserver-xorg-core 
xserver-xorg-input-all xserver-xorg-video-all
</pre>

## Broadcom wifi driver 

Getting the Broadcom wifi driver to work with the 3.2 kernel was not completely straightforward. The [Debian Wiki](http://wiki.debian.org/InstallingDebianOn/Thinkpad/X121e-30515YG) says to install the broadcom-sta-source model and then compile it with module-assist:

<pre>sudo apt-get install broadcom-sta-source
sudo apt-get install module-assistant
sudo m-a prepare

Getting source for kernel version: 3.2.0-0.bpo.2-amd64
apt-get install kernel-headers-3.2.0-0.bpo.2-amd64
Reading package lists... Done
Building dependency tree
Reading state information... Done
E: Unable to locate package kernel-headers-3.2.0-0.bpo.2-amd64
E: Couldn't find any package by regex 'kernel-headers-3.2.0-0.bpo.2-amd64'

</pre>

This fails because the backported kernel header package is not called kernel-headers-3.2.0-0.bpo.2-amd64; it is actually linux-headers-3.2.0-0.bpo.2-amd64. Fortunately we can install the headers ourselves: 

<pre>sudo apt-get install -t squeeze linux-headers-3.2.0-0.bpo.2-amd64
</pre>

Now try the module-assist again:

<pre>sudo m-a update
sudo m-a a-i broadcom-sta
</pre>

However we are compiling against the 3.2 kernel, and the broadcom-sta package in Squeeze is not new enough. It failed with an error about C pointers (sorry, forgot to save the details). However, [happysumo on Whirlpool](http://forums.whirlpool.net.au/archive/1853313) had a similar issue and suggested temporarily updating to the testing distribution. So edit /etc/apt/sources.list and change squeeze to testing, then

<pre>sudo apt-get update
sudo apt-get install broadcom-sta-source
sudo m-a update
sudo m-a a-i broadcom-sta
</pre>

This successfully built the module. Then edit /etc/apt/sources.list again and change testing back to squeeze and do an update:

<pre>sudo apt-get update
</pre>

After a reboot the built-in ethernet and wifi both worked. 

## VirtualBox 

The version of VirtualBox in Squeeze is not new enough to compile against a 3.2 kernel, so just grab the 4.x release from [www.virtualbox.org](https://www.virtualbox.org/). 

## Where&#8217;s the Insert key!? 

I think it&#8217;s strange that there is no Insert key on the X121e. According to [this post](http://www.msfn.org/board/topic/153124-thinkpad-x121e-annoyances/) there are some key combos to get the missing keys:

<pre>Fn + I = Insert
Fn + P = Pause
Fn + B = Break
Fn + S = Scroll Lock
</pre>

## System info 

Output of &#8220;lspci -v&#8221;: 

<pre>00:00.0 Host bridge: Intel Corporation Sandy Bridge DRAM Controller (rev 09)
	Subsystem: Lenovo Device 21ed
	Flags: bus master, fast devsel, latency 0
	Capabilities: [e0] Vendor Specific Information: Len=0c 
	Kernel driver in use: agpgart-intel

00:02.0 VGA compatible controller: Intel Corporation Sandy Bridge Integrated Graphics Controller (rev 09) (prog-if 00 [VGA controller])
	Subsystem: Lenovo Device 21ed
	Flags: bus master, fast devsel, latency 0, IRQ 42
	Memory at d0000000 (64-bit, non-prefetchable) [size=4M]
	Memory at c0000000 (64-bit, prefetchable) [size=256M]
	I/O ports at 4000 [size=64]
	Expansion ROM at  [disabled]
	Capabilities: [90] MSI: Enable+ Count=1/1 Maskable- 64bit-
	Capabilities: [d0] Power Management version 2
	Capabilities: [a4] PCI Advanced Features
	Kernel driver in use: i915

00:16.0 Communication controller: Intel Corporation Cougar Point HECI Controller #1 (rev 04)
	Subsystem: Lenovo Device 21ed
	Flags: bus master, fast devsel, latency 0, IRQ 11
	Memory at d1605000 (64-bit, non-prefetchable) [size=16]
	Capabilities: [50] Power Management version 3
	Capabilities: [8c] MSI: Enable- Count=1/1 Maskable- 64bit+

00:1a.0 USB Controller: Intel Corporation Cougar Point USB Enhanced Host Controller #2 (rev 04) (prog-if 20 [EHCI])
	Subsystem: Lenovo Device 21ed
	Flags: bus master, medium devsel, latency 0, IRQ 16
	Memory at d160a000 (32-bit, non-prefetchable) [size=1K]
	Capabilities: [50] Power Management version 2
	Capabilities: [58] Debug port: BAR=1 offset=00a0
	Capabilities: [98] PCI Advanced Features
	Kernel driver in use: ehci_hcd

00:1b.0 Audio device: Intel Corporation Cougar Point High Definition Audio Controller (rev 04)
	Subsystem: Lenovo Device 21ed
	Flags: bus master, fast devsel, latency 0, IRQ 41
	Memory at d1600000 (64-bit, non-prefetchable) [size=16K]
	Capabilities: [50] Power Management version 2
	Capabilities: [60] MSI: Enable+ Count=1/1 Maskable- 64bit+
	Capabilities: [70] Express Root Complex Integrated Endpoint, MSI 00
	Capabilities: [100] Virtual Channel
	Capabilities: [130] Root Complex Link
	Kernel driver in use: snd_hda_intel

00:1c.0 PCI bridge: Intel Corporation Cougar Point PCI Express Root Port 1 (rev b4) (prog-if 00 [Normal decode])
	Flags: bus master, fast devsel, latency 0
	Bus: primary=00, secondary=01, subordinate=01, sec-latency=0
	Capabilities: [40] Express Root Port (Slot+), MSI 00
	Capabilities: [80] MSI: Enable- Count=1/1 Maskable- 64bit-
	Capabilities: [90] Subsystem: Lenovo Device 21ed
	Capabilities: [a0] Power Management version 2
	Kernel driver in use: pcieport

00:1c.1 PCI bridge: Intel Corporation Cougar Point PCI Express Root Port 2 (rev b4) (prog-if 00 [Normal decode])
	Flags: bus master, fast devsel, latency 0
	Bus: primary=00, secondary=02, subordinate=02, sec-latency=0
	Memory behind bridge: d1500000-d15fffff
	Capabilities: [40] Express Root Port (Slot+), MSI 00
	Capabilities: [80] MSI: Enable- Count=1/1 Maskable- 64bit-
	Capabilities: [90] Subsystem: Lenovo Device 21ed
	Capabilities: [a0] Power Management version 2
	Kernel driver in use: pcieport

00:1c.2 PCI bridge: Intel Corporation Cougar Point PCI Express Root Port 3 (rev b4) (prog-if 00 [Normal decode])
	Flags: bus master, fast devsel, latency 0
	Bus: primary=00, secondary=03, subordinate=07, sec-latency=0
	I/O behind bridge: 00003000-00003fff
	Memory behind bridge: d0d00000-d14fffff
	Prefetchable memory behind bridge: 00000000d0400000-00000000d0bfffff
	Capabilities: [40] Express Root Port (Slot+), MSI 00
	Capabilities: [80] MSI: Enable- Count=1/1 Maskable- 64bit-
	Capabilities: [90] Subsystem: Lenovo Device 21ed
	Capabilities: [a0] Power Management version 2
	Kernel driver in use: pcieport

00:1c.5 PCI bridge: Intel Corporation Cougar Point PCI Express Root Port 6 (rev b4) (prog-if 00 [Normal decode])
	Flags: bus master, fast devsel, latency 0
	Bus: primary=00, secondary=08, subordinate=08, sec-latency=0
	I/O behind bridge: 00002000-00002fff
	Memory behind bridge: d0c00000-d0cfffff
	Capabilities: [40] Express Root Port (Slot+), MSI 00
	Capabilities: [80] MSI: Enable- Count=1/1 Maskable- 64bit-
	Capabilities: [90] Subsystem: Lenovo Device 21ed
	Capabilities: [a0] Power Management version 2
	Kernel driver in use: pcieport

00:1d.0 USB Controller: Intel Corporation Cougar Point USB Enhanced Host Controller #1 (rev 04) (prog-if 20 [EHCI])
	Subsystem: Lenovo Device 21ed
	Flags: bus master, medium devsel, latency 0, IRQ 23
	Memory at d1609000 (32-bit, non-prefetchable) [size=1K]
	Capabilities: [50] Power Management version 2
	Capabilities: [58] Debug port: BAR=1 offset=00a0
	Capabilities: [98] PCI Advanced Features
	Kernel driver in use: ehci_hcd

00:1f.0 ISA bridge: Intel Corporation Cougar Point LPC Controller (rev 04)
	Subsystem: Lenovo Device 21ed
	Flags: bus master, medium devsel, latency 0
	Capabilities: [e0] Vendor Specific Information: Len=0c 

00:1f.2 SATA controller: Intel Corporation Cougar Point 6 port SATA AHCI Controller (rev 04) (prog-if 01 [AHCI 1.0])
	Subsystem: Lenovo Device 21ed
	Flags: bus master, 66MHz, medium devsel, latency 0, IRQ 40
	I/O ports at 4088 [size=8]
	I/O ports at 4094 [size=4]
	I/O ports at 4080 [size=8]
	I/O ports at 4090 [size=4]
	I/O ports at 4060 [size=32]
	Memory at d1608000 (32-bit, non-prefetchable) [size=2K]
	Capabilities: [80] MSI: Enable+ Count=1/1 Maskable- 64bit-
	Capabilities: [70] Power Management version 3
	Capabilities: [a8] SATA HBA v1.0
	Capabilities: [b0] PCI Advanced Features
	Kernel driver in use: ahci

00:1f.3 SMBus: Intel Corporation Cougar Point SMBus Controller (rev 04)
	Subsystem: Lenovo Device 21ed
	Flags: medium devsel, IRQ 18
	Memory at d1604000 (64-bit, non-prefetchable) [size=256]
	I/O ports at efa0 [size=32]

02:00.0 Network controller: Broadcom Corporation Device 0576 (rev 01)
	Subsystem: Broadcom Corporation Device 0576
	Flags: bus master, fast devsel, latency 0, IRQ 17
	Memory at d1500000 (64-bit, non-prefetchable) [size=16K]
	Capabilities: [40] Power Management version 3
	Capabilities: [58] Vendor Specific Information: Len=78 
	Capabilities: [48] MSI: Enable- Count=1/1 Maskable- 64bit+
	Capabilities: [d0] Express Endpoint, MSI 00
	Capabilities: [100] Advanced Error Reporting
	Capabilities: [13c] Virtual Channel
	Capabilities: [160] Device Serial Number 00-00-12-ff-ff-dc-ac-81
	Capabilities: [16c] Power Budgeting 
	Kernel driver in use: wl

03:00.0 Unassigned class [ff00]: Realtek Semiconductor Co., Ltd. Device 5209 (rev 01)
	Subsystem: Lenovo Device 21ed
	Flags: bus master, fast devsel, latency 0, IRQ 18
	Memory at d0d00000 (32-bit, non-prefetchable) [size=4K]
	Capabilities: [40] Power Management version 3
	Capabilities: [50] MSI: Enable- Count=1/1 Maskable- 64bit+
	Capabilities: [70] Express Endpoint, MSI 00
	Capabilities: [100] Advanced Error Reporting
	Capabilities: [140] Device Serial Number 00-00-00-01-00-4c-e0-00
	Kernel driver in use: rts_pstor

08:00.0 Ethernet controller: Atheros Communications Device 1083 (rev c0)
	Subsystem: Lenovo Device 21f2
	Flags: bus master, fast devsel, latency 0, IRQ 43
	Memory at d0c00000 (64-bit, non-prefetchable) [size=256K]
	I/O ports at 2000 [size=128]
	Capabilities: [40] Power Management version 3
	Capabilities: [48] MSI: Enable+ Count=1/1 Maskable- 64bit+
	Capabilities: [58] Express Endpoint, MSI 00
	Capabilities: [6c] Vital Product Data
	Capabilities: [100] Advanced Error Reporting
	Capabilities: [180] Device Serial Number ff-0f-c2-94-04-7d-7b-ff
	Kernel driver in use: atl1c

</pre>

<pre>cat /proc/cpuinfo:

processor	: 0
vendor_id	: GenuineIntel
cpu family	: 6
model		: 42
model name	: Intel(R) Core(TM) i3-2367M CPU @ 1.40GHz
stepping	: 7
microcode	: 0x1b
cpu MHz		: 800.000
cache size	: 3072 KB
physical id	: 0
siblings	: 4
core id		: 0
cpu cores	: 2
apicid		: 0
initial apicid	: 0
fpu		: yes
fpu_exception	: yes
cpuid level	: 13
wp		: yes
flags		: fpu vme de pse tsc msr pae mce cx8 apic sep mtrr pge mca cmov pat pse36 clflush dts acpi mmx fxsr sse sse2 ss ht tm pbe syscall nx rdtscp lm constant_tsc arch_perfmon pebs bts nopl xtopology nonstop_tsc aperfmperf pni pclmulqdq dtes64 monitor ds_cpl vmx est tm2 ssse3 cx16 xtpr pdcm pcid sse4_1 sse4_2 x2apic popcnt tsc_deadline_timer xsave avx lahf_lm arat epb xsaveopt pln pts dts tpr_shadow vnmi flexpriority ept vpid
bogomips	: 2793.59
clflush size	: 64
cache_alignment	: 64
address sizes	: 36 bits physical, 48 bits virtual
power management:

processor	: 1
vendor_id	: GenuineIntel
cpu family	: 6
model		: 42
model name	: Intel(R) Core(TM) i3-2367M CPU @ 1.40GHz
stepping	: 7
microcode	: 0x1b
cpu MHz		: 800.000
cache size	: 3072 KB
physical id	: 0
siblings	: 4
core id		: 0
cpu cores	: 2
apicid		: 1
initial apicid	: 1
fpu		: yes
fpu_exception	: yes
cpuid level	: 13
wp		: yes
flags		: fpu vme de pse tsc msr pae mce cx8 apic sep mtrr pge mca cmov pat pse36 clflush dts acpi mmx fxsr sse sse2 ss ht tm pbe syscall nx rdtscp lm constant_tsc arch_perfmon pebs bts nopl xtopology nonstop_tsc aperfmperf pni pclmulqdq dtes64 monitor ds_cpl vmx est tm2 ssse3 cx16 xtpr pdcm pcid sse4_1 sse4_2 x2apic popcnt tsc_deadline_timer xsave avx lahf_lm arat epb xsaveopt pln pts dts tpr_shadow vnmi flexpriority ept vpid
bogomips	: 2793.64
clflush size	: 64
cache_alignment	: 64
address sizes	: 36 bits physical, 48 bits virtual
power management:

processor	: 2
vendor_id	: GenuineIntel
cpu family	: 6
model		: 42
model name	: Intel(R) Core(TM) i3-2367M CPU @ 1.40GHz
stepping	: 7
microcode	: 0x1b
cpu MHz		: 800.000
cache size	: 3072 KB
physical id	: 0
siblings	: 4
core id		: 1
cpu cores	: 2
apicid		: 2
initial apicid	: 2
fpu		: yes
fpu_exception	: yes
cpuid level	: 13
wp		: yes
flags		: fpu vme de pse tsc msr pae mce cx8 apic sep mtrr pge mca cmov pat pse36 clflush dts acpi mmx fxsr sse sse2 ss ht tm pbe syscall nx rdtscp lm constant_tsc arch_perfmon pebs bts nopl xtopology nonstop_tsc aperfmperf pni pclmulqdq dtes64 monitor ds_cpl vmx est tm2 ssse3 cx16 xtpr pdcm pcid sse4_1 sse4_2 x2apic popcnt tsc_deadline_timer xsave avx lahf_lm arat epb xsaveopt pln pts dts tpr_shadow vnmi flexpriority ept vpid
bogomips	: 2793.66
clflush size	: 64
cache_alignment	: 64
address sizes	: 36 bits physical, 48 bits virtual
power management:

processor	: 3
vendor_id	: GenuineIntel
cpu family	: 6
model		: 42
model name	: Intel(R) Core(TM) i3-2367M CPU @ 1.40GHz
stepping	: 7
microcode	: 0x1b
cpu MHz		: 800.000
cache size	: 3072 KB
physical id	: 0
siblings	: 4
core id		: 1
cpu cores	: 2
apicid		: 3
initial apicid	: 3
fpu		: yes
fpu_exception	: yes
cpuid level	: 13
wp		: yes
flags		: fpu vme de pse tsc msr pae mce cx8 apic sep mtrr pge mca cmov pat pse36 clflush dts acpi mmx fxsr sse sse2 ss ht tm pbe syscall nx rdtscp lm constant_tsc arch_perfmon pebs bts nopl xtopology nonstop_tsc aperfmperf pni pclmulqdq dtes64 monitor ds_cpl vmx est tm2 ssse3 cx16 xtpr pdcm pcid sse4_1 sse4_2 x2apic popcnt tsc_deadline_timer xsave avx lahf_lm arat epb xsaveopt pln pts dts tpr_shadow vnmi flexpriority ept vpid
bogomips	: 2793.65
clflush size	: 64
cache_alignment	: 64
address sizes	: 36 bits physical, 48 bits virtual
power management:

</pre>

**Archived Comments**

Date: 2012-07-24 11:50:28 UTC

Author: steve rinsler

your command for installing a 3.2 kernel specifies 2.6.39?? Isn&#8217;t that a mistake/typo?

Date: 2012-08-03 09:06:57 UTC

Author: carlo

Steve,

Thanks, I&#8217;ve updated the instructions to refer to the 3.2 kernel.