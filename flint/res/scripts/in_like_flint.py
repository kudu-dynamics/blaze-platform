
#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Module: in_like_flint.py
Purpose: Combine one or more FLINT output JSONs into output sqlite DB.
Version: 1.0.0

Usage:
    python in_like_flint.py <JSON input folder path> <SQLite DB path>
"""
import sys
import json
import time
import sqlite3
from pathlib import Path


def create_database(db_path: Path) -> tuple[sqlite3.Connection, sqlite3.Cursor]:
    """Create SQLite database and WMI table."""
    conn = sqlite3.connect(db_path)
    cursor = conn.cursor()

    cursor.execute('''
        CREATE TABLE IF NOT EXISTS wmis (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            filename TEXT,
            function_name TEXT,
            function_address INTEGER,
            prim_name TEXT,
            location_write_right INTEGER,
            ptr TEXT,
            value TEXT,
            constraints TEXT
        )
    ''')

    conn.commit()
    return conn, cursor


def process_json_file(json_path: Path, cursor: sqlite3.Cursor) -> int:
    """Process a JSON file and insert its WMI data into the database."""
    with json_path.open('r') as f:
        data = json.load(f)

    filename = json_path.name
    callable_wmis = data.get('callableWMIs', [])
    wmi_entry_count = 0

    # Iterate through WMI categories
    for wmi_category in callable_wmis:
        if not wmi_category or len(wmi_category) < 2:
            continue
        wmi_type, wmi_list = wmi_category[0], wmi_category[1]
        wmi_entry_count += len(wmi_list)

        # Process each WMI entry and insert them into the DB
        for wmi in wmi_list:
            func_name = wmi.get('func', [None, None])[0]
            func_address = wmi.get('func', [None, None])[1]
            prim_name = wmi.get('primName')
            location_write_right = wmi.get('locations', {}).get('write', {}).get('Right')
            ptr = wmi.get('vars', {}).get('ptr')
            value = wmi.get('vars', {}).get('value')
            # Store as JSON string for now
            constraints = json.dumps(wmi.get('constraints', []))

            cursor.execute('''
                INSERT INTO wmis (
                    filename, function_name, function_address, prim_name,
                    location_write_right, ptr, value, constraints
                ) VALUES (?, ?, ?, ?, ?, ?, ?, ?)
            ''', (
                filename, func_name, func_address, prim_name,
                location_write_right, ptr, value, constraints
            ))

    return wmi_entry_count


def populate_database(json_dir: Path, db_path: Path) -> tuple[int, int]:
    """Process all JSON files in the directory and populate the database."""
    # Iterate input JSON files
    json_files = list(json_dir.glob('*.json'))
    file_count = len(json_files)
    conn, cursor = create_database(db_path)
    total_wmi_count = 0

    for index, json_path in enumerate(json_files, start=1):
        print(f" [{index}] of {file_count}: {json_path.name}...")
        total_wmi_count += process_json_file(json_path, cursor)

    conn.commit()
    conn.close()
    return file_count, total_wmi_count


def main() -> int:
    if len(sys.argv) != 3:
        print(f"Usage: {sys.argv[0]} <json_dir> <db_path>")
        return 1

    json_input_folder = Path(sys.argv[1])
    db_output_path = Path(sys.argv[2])

    start_time = time.time()
    file_count, total_wmi_count = populate_database(json_input_folder, db_output_path)
    runtime = (time.time() - start_time)

    print(f"Done. Processed {file_count:,} JSON input files, with {total_wmi_count:,} total WMI entries, in {runtime:.1f} seconds.")
    return 0


if __name__ == '__main__':
    sys.exit(main())
