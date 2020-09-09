# Arch Linux Installation
### My processes to installing my favourite linux ditribution

**Create bootable pendrive**

```
dd bs=4M if=path/to/arch.iso of=/dev/sd(pendrive) status=progress && sync
```

**Create partitions**

with ```cfdisk``` create the following partitions:

1 - EFI System [syze = 512M]
2 - Linux Swap [syze = (RAM size)]
3 - Root Parition [syze = (remaining disk space)]

**Mount partitions**

root:
```
mkfs.fat -F32 /dev/sda(number of root partition)
```

efi system:
```
mkfs.ext4 /dev/sda(number of efi partition)
```
