import sys

import binaryninja
from binaryninja.update import UpdateChannel, UpdateResult
from binaryninja.enums import LogLevel


def main(channel: str, version: str) -> None:
    binaryninja.log.log_to_stderr(LogLevel.DebugLog)
    binja_channel = UpdateChannel[channel]
    print(f'Fetching version {version} on channel {channel}...')
    if version.lower() == 'latest':
        binja_version = binja_channel.latest_version
    else:
        binja_version = next((v for v in binja_channel.versions if v.version == version), None)
        if not binja_version:
            raise ValueError(f'No such version: {version}')

    status = binja_version.update()
    if status == UpdateResult.AlreadyUpToDate:
        print(f'Already up to date, {binja_version.version}')
        exit(0)
    if status == UpdateResult.UpdateFailed:
        print('Update failed')
        exit(1)
    if status == UpdateResult.UpdateAvailable:
        print('What does this mean?')
        exit(1)
    elif status == UpdateResult.UpdateSuccess:
        print(f'Downloaded version {binja_version.version}, installing update')
        try:
            binaryninja.install_pending_update()
        except OSError as e:
            print("FIXME: Binary Ninja still raises OSError when it shouldn't")
            print(repr(e))
        else:
            print(f'Successfully installed version {binja_version.version}')


if __name__ == '__main__':
    main(sys.argv[1], sys.argv[2])
