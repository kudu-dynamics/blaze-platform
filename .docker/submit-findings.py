#!/usr/bin/env python3

import argparse
import base64
from dataclasses import dataclass
import io
import os
from pathlib import Path
import subprocess
import sys
from textwrap import indent
from urllib.parse import urljoin
import zipfile

import requests

FLINT_ARGS = ["--doNotUseSolver", "--isKernelModule", "--outputJSON"]

BUILD_FILE = """\
set -eu
echo 'No build needed. Run ./run'
"""

RUN_FILE = """\
set -eu
cat flint.out.txt
"""


@dataclass
class Submission:
    tag: str
    zip: bytes


def build_zip(flint_output: bytes, flint_errors: bytes) -> bytes:
    def zi(path: str, mode: int) -> zipfile.ZipInfo:
        info = zipfile.ZipInfo(path)
        info.external_attr = (mode & 0o777) << 16
        return info

    buf = io.BytesIO()
    with zipfile.ZipFile(buf, "w") as z:
        z.writestr(zi("build", 0o770), BUILD_FILE)
        z.writestr(zi("run", 0o770), RUN_FILE)
        z.writestr(zi("flint.out.txt", 0o660), flint_output)
        z.writestr(zi("flint.err.txt", 0o660), flint_errors)

    buf.seek(0)
    return buf.getvalue()


def submit(
    zip: bytes,
    tags: list[str],
    owner: str,
    base_url: str,
    build_type: str = "shell",
    run_type: str = "shell",
    endpoint: str = "/test",
) -> requests.Response:
    j = {
        "content": base64.b64encode(zip).decode("ascii"),
        "tags": ",".join(t.replace("\\", "\\\\").replace(",", "\\,") for t in tags),
        "owner": owner,
        "build-type": build_type,
        "build-file": "build",
        "run-type": run_type,
        "run-file": "run",
    }
    return requests.post(urljoin(base_url, endpoint), json=j)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Run Flint on kernel modules and submit findings")

    parser.add_argument("blaze_platform_commit", help="Commit hash of blaze-platform")
    parser.add_argument("cpio_hash", help="SHA256 of rootfs.cpio")
    parser.add_argument("rootfs_dir", help="Extracted directory of rootfs.cpio")
    parser.add_argument("base_url", help="Base URL for submission API")
    parser.add_argument("endpoint", help="Endpoint for submission API")
    parser.add_argument("owner", help="`owner` field for submission API")
    parser.add_argument("flint_executable", help="Location of `flint`")

    args = parser.parse_args()

    args.rootfs_dir = Path(args.rootfs_dir)
    if not args.rootfs_dir.is_dir():
        print(f"No such directory: {args.rootfs_dir}", file=sys.stderr)
        exit(2)

    args.flint_executable = Path(args.flint_executable)
    if not (args.flint_executable.is_file() and os.access(args.flint_executable, os.X_OK)):
        print(f"No such file or not executable: {args.flint_executable}", file=sys.stderr)
        exit(2)

    modules = list(args.rootfs_dir.glob("**/*.ko"))
    results = []

    for module in modules:
        retries = 3
        while True:
            print(f"\nRunning Flint on path: {module}")
            p = subprocess.run([args.flint_executable, *FLINT_ARGS, module], capture_output=True)
            print("============\n" "FLINT STDOUT\n" "============")
            print(indent(p.stdout.decode("utf8", errors="ignore"), "    "))
            print("============\n" "FLINT STDERR\n" "============")
            print(indent(p.stderr.decode("utf8", errors="ignore"), "    "))
            print("===\n" "END\n" "===")

            if p.returncode == 0:
                print("Flint succeeded")
                relpath = str(module.relative_to(args.rootfs_dir))
                # tag = f"kudu-{args.blaze_platform_commit}-linux-{args.cpio_hash}-{relpath}"
                tag = f"kudu-{relpath[-17:]}"
                z = build_zip(p.stdout, p.stderr)
                results.append(Submission(tag, z))
                break

            if retries <= 0:
                print("Running Flint failed too many times, skipping module")
                break

            print("Running Flint failed, retrying")
            retries -= 1

    print(f"Collected {len(results)} results from {len(modules)} modules")

    for result in results:
        print(f"Submitting result for {result.tag}")
        r = submit(result.zip, [result.tag], args.owner, args.base_url, endpoint=args.endpoint)
        print(f"{r.status_code = }")
        print(f"{r.json() = !r}")
        r.raise_for_status()
        # with open(f"zips/{result.tag.replace('/', '_')}.zip", "wb") as f:
        #     f.write(result.zip)
