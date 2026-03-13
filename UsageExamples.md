# CVE-2012-0809

[CVE-2012-0809](https://nvd.nist.gov/vuln/detail/CVE-2012-0809) is a the following:
> Format string vulnerability in the sudo_debug function in Sudo 1.8.0 through 1.8.3p1 allows local users to execute arbitrary code via format string sequences in the program name for sudo.

Original vuln:
```c
/*
 * Simple debugging/logging.
 */
void
sudo_debug(int level, const char *fmt, ...)
{
    va_list ap;
    char *fmt2;

    if (level > debug_level)
	return;

    /* Backet fmt with program name and a newline to make it a single write */
    easprintf(&fmt2, "%s: %s\n", getprogname(), fmt);
    va_start(ap, fmt);
    vfprintf(stderr, fmt2, ap);
    va_end(ap);
    efree(fmt2);
}
```

The return of `getprogname()` is `argv[0]` and by this user controlled. So `argv[0]` goes to `fmt2` which then goes to `vfprintf()` to stderr. This is a classic format string vulnerability. 

## Compiling with `.debug` section

You can pull the source code like this:
```bash
$ wget https://www.sudo.ws/dist/sudo-1.8.3p1.tar.gz
$ tar -xf sudo-1.8.3p1.tar.gz 
$ rm sudo-1.8.3p1.tar.gz

# this is the buggy file with the controlled format string
$ find . -name "sudo.c"

# its inside sudo_debug
# that fmt2 in vfprintf(stderr, fmt2, ap); is dangerous
```

Then, you can compile from source:
```bash
$ cd sudo-1.8.3p1 && ./configure --prefix=/tmp/sudo_build --disable-pam-session --disable-root-mailer --with-all-insults
$ cd src
$ make CFLAGS="-g -O0 -fdebug-prefix-map=$$HOME=" sudo

# check it
$ file sudo.o
$ nm sudo.o | grep sudo_debug
```

## Running `flint` on `sudo`

Go to the `/blaze-platform/flint` directory:
```bash
$ cd /your/directory/blaze-platform/flint
```

Then, `cd` into the `blaze-platform/flint` repository

Create sone `.config` files:
```bash
$ echo -e "main
fix_fds
fill_group_list
get_user_groups
get_user_info
command_info_to_details
disable_coredumps
disable_execute
exec_setup
run_command
policy_open
policy_close
policy_show_version
policy_check
policy_list
policy_validate
policy_invalidate
policy_init_session
iolog_open
iolog_close
iolog_show_version" > sudo-blacklist.txt

$ echo -e "sudo_debug" > sudo-filter.txt
```

Run `flint` with the following commnand:

```bash
stack run -- flint \
  --doNotUseSolver \
  --outputSMTish \
  --verbosity Debug \
  --blacklist sudo-blacklist.txt \
  --filterFuncs sudo-filter.txt \
  -o sudo.json \
  /your/directory/sudo-1.8.3p1/src/sudo.o
```

## Understanding the Flint Output

If you look at `sudo.json`, `flint` finds a `ControlledFormatString` WMI:
```json
[
    {
        "name": "ControlledFormatString",
        "operation": {
            "effects": [
                {
                    "arguments": {
                        "fmt": "fmt2@0"
                    },
                    "id": "-2045074009792424813",
                    "type": "ControlledFormatString"
                }
            ],
            "name": "ControlledFormatString",
            "preconditions": [
                "(bvsle (ARG 0) (DEREF 32 (GLOBAL {addr: 0x0000000000103068, name: \"debug_level\"})))"
            ],
            "variables": [
                "; variables section\n; TODO\n"
            ]
        },
        "trigger": "sudo_debug((ARG 0), (ARG 1));"
    }
]
```

### WMI Explained

Here is each line described in human-readable format:
- Name: The name of the Weird Machine Instruction is called `ControlledFormatString`.
- Operation:
    - Effects:
        - Arguments: The arguments effected are `fmt2@0`.
        - ID: Unique identifier is `-2045074009792424813`.
        - Type: The effect type is called `ControlledFormatString`.
    - Name: This name can be an optional human-readable name is `ControlledFormatString`.
    - Preconditions: The WMI is only reachable when the first argument `level` is less than  to the global `debug_level` variable.
    - Variables: Variables that get referenced by `effects` or `preconditions.
- Trigger: The WMI is trigged when `sudo_debug` function is called.

> TODO: Name, Type, and Operation->Name will have more unique naming later.

## Running `flint` on stripped binaries

Go back to the directory where we built `sudo.o`:
```bash
$ cd /your/directory
```

You can pull the stripped version of `sudo.o` like this:
```bash
$ wget -q "https://snapshot.debian.org/archive/debian/20111113T050447Z/pool/main/s/sudo/sudo_1.8.3p1-2_amd64.deb" -O sudo_1.8.3p1.deb
$ dpkg-deb -x sudo_1.8.3p1.deb sudo_1.8.3p1_bin
$ rm -rf sudo_1.8.3p1.deb 
$ file sudo_1.8.3p1_bin/usr/bin/sudo
```

Go back to the `blaze-platform/flint` directory and create some config files:
```bash
$ echo -e "FUN_00402640
FUN_00402e10
FUN_00403e58
FUN_00403e70
FUN_00403ee0
FUN_00403f10
FUN_00404090
FUN_00404160
FUN_004042f0
FUN_00404390
FUN_00404430
FUN_004044d0
FUN_00404500
FUN_00404600
FUN_00404630
FUN_00404710
FUN_00404750
FUN_00404790
FUN_00404860
FUN_004051b0
FUN_00405220
FUN_00405270
FUN_004052b0
FUN_00405340
FUN_004053d0
FUN_00405460
FUN_004054f0
FUN_00405580
FUN_00405610
FUN_00405650
FUN_004056d0
FUN_00405700
FUN_004057d0
FUN_00405830
FUN_00405910
FUN_00405b30
FUN_00405da0
FUN_00405fc0
FUN_00406e10
FUN_00406f50
FUN_00407080
FUN_00407110
FUN_004073a0
FUN_00407950
FUN_00407960
FUN_00407970
FUN_00407a80
FUN_00407ab0
FUN_00408970
FUN_004089f0
FUN_00408ee0
FUN_00409390
FUN_004094e0
FUN_004095d0
FUN_0040a180
FUN_0040a1a0
FUN_0040a320
FUN_0040a350
FUN_0040a9d0
FUN_0040aa90
FUN_0040acb0
FUN_0040ada0
FUN_0040adf0
FUN_0040ae70
FUN_0040aed0
FUN_0040af60
FUN_0040afc0
FUN_0040b030
FUN_0040b0f0
FUN_0040b130
FUN_0040b150
FUN_0040b220
FUN_0040b280
FUN_0040b2b0
FUN_0040b3a0
FUN_0040b440
FUN_0040b470
FUN_0040b490
FUN_0040b6d0
FUN_0040b860
FUN_0040bae0
FUN_0040bb30
FUN_0040bb60
FUN_0040bb80
FUN_0040bbb0
FUN_0040bc10
FUN_0040bc90
FUN_0040bcd0
FUN_0040bda0
FUN_0040bea0
FUN_0040bfa0
FUN_0040bfd0
FUN_0040bff0
FUN_0040c040
FUN_0040c090
FUN_0040c150
FUN_0040c160
FUN_0040c1f0" > sudo-stripped-blacklist.txt

$ echo -e "FUN_004092c0" > sudo-stripped-filter.txt
```

`FUN_004092c0` is `sudo_debug` in the stripped `sudo.o`.

This is what `FUN_004092c0` without any cleaning up:
```c
void FUN_004092c0(undefined8 param_1,undefined8 param_2,undefined8 param_3,undefined8 param_4,
                 undefined8 param_5,undefined8 param_6,undefined8 param_7,undefined8 param_8,
                 int param_9,undefined8 param_10,undefined8 param_11,undefined8 param_12,
                 undefined8 param_13,undefined8 param_14)

{
  char in_AL;
  undefined4 local_d8;
  undefined4 local_d4;
  undefined1 *local_d0;
  undefined1 *local_c8;
  char *local_c0;
  undefined1 local_b8 [16];
  undefined8 local_a8;
  undefined8 local_a0;
  undefined8 local_98;
  undefined8 local_90;
  undefined8 local_88;
  undefined8 local_78;
  undefined8 local_68;
  undefined8 local_58;
  undefined8 local_48;
  undefined8 local_38;
  undefined8 local_28;
  undefined8 local_18;
  
  if (in_AL != '\0') {
    local_88 = param_1;
    local_78 = param_2;
    local_68 = param_3;
    local_58 = param_4;
    local_48 = param_5;
    local_38 = param_6;
    local_28 = param_7;
    local_18 = param_8;
  }
  if (param_9 <= DAT_00612e10) {
    local_a8 = param_11;
    local_a0 = param_12;
    local_98 = param_13;
    local_90 = param_14;
    FUN_0040b030(&local_c0,"%s: %s\n",program_invocation_short_name,param_10);
    local_d0 = &stack0x00000008;
    local_d8 = 0x10;
    local_d4 = 0x30;
    local_c8 = local_b8;
    vfprintf(stderr,local_c0,&local_d8);
    FUN_0040b130(local_c0);
  }
  return;
}
```

Run `flint` against the stripped binary:
```bash
stack run -- flint \
  --doNotUseSolver \
  --outputSMTish \
  --verbosity Debug \
  --blacklist sudo-stripped-blacklist.txt  \
  --filterFuncs sudo-stripped-filter.txt \
  -o sudo-stripped.json \
  /your/directory/sudo_1.8.3p1_bin/usr/bin/sudo
```

Flint is able to catch the same `ControlledFormatString` in a stripped `sudo.o` binary:
```json
[
    {
        "name": "ControlledFormatString",
        "operation": {
            "effects": [
                {
                    "arguments": {
                        "fmt": "local_c0@0"
                    },
                    "id": "-8992885277207334651",
                    "type": "ControlledFormatString"
                }
            ],
            "name": "ControlledFormatString",
            "preconditions": [
                "(bvsle (ARG 8) (DEREF 32 (GLOBAL {addr: 0x0000000000612e10, name: \"DAT_00612e10\"})))"
            ],
            "variables": [
                "; variables section\n; TODO\n"
            ]
        },
        "trigger": "FUN_004092c0((ARG 0), (ARG 1), (ARG 2), (ARG 3), (ARG 4), (ARG 5), (ARG 6), (ARG 7), (ARG 8), (ARG 9), (ARG 10), (ARG 11), (ARG 12), (ARG 13));"
    }
]
```
