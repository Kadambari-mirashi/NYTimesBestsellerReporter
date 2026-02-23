# fetch_bestsellers.py
# NYT Bestseller Data Fetcher
# Fetches all current bestseller lists and books from the NYT Books API
# and saves them to data/bestsellers.json for the Shiny app and AI reporter.

import os
import sys
import json
import time
import requests
from pathlib import Path
from datetime import datetime, timezone
from dotenv import load_dotenv

# --- Environment and paths ---

script_dir = Path(__file__).resolve().parent
env_paths = [script_dir / ".env", Path.cwd() / ".env"]

for p in env_paths:
    if p.exists():
        load_dotenv(p)
        break
else:
    load_dotenv()

API_KEY = os.getenv("NYT_API_KEY")
if not API_KEY or "your_" in API_KEY.lower():
    print("ERROR: NYT_API_KEY missing or still a placeholder.")
    print("  Create a .env file in the project root with:")
    print("    NYT_API_KEY=your_real_nyt_api_key")
    print("  Get a key at: https://developer.nytimes.com/get-started")
    sys.exit(1)

BASE_URL = "https://api.nytimes.com/svc/books/v3"
OUT_DIR = script_dir / "data"
OUT_FILE = OUT_DIR / "bestsellers.json"


# --- API helpers ---

def nyt_get(path, params=None):
    """GET a NYT Books API endpoint; returns parsed JSON or None."""
    params = dict(params or {}, **{"api-key": API_KEY})
    url = f"{BASE_URL}/{path}"
    try:
        resp = requests.get(url, params=params, timeout=25)
    except requests.RequestException as e:
        print(f"  ERROR: Request failed: {e}")
        return None
    if resp.status_code != 200:
        print(f"  ERROR: Status {resp.status_code} for {path}")
        return None
    data = resp.json()
    if data.get("status") != "OK":
        return None
    return data


def encode_list_name(name):
    """Encode list name for API: lowercase, spaces to hyphens, strip apostrophes."""
    if not name:
        return ""
    s = name.lower().strip().replace("'", "").replace(" ", "-")
    return "".join(c for c in s if c.isalnum() or c == "-")


def get_list_names():
    """Fetch all bestseller list names from the overview endpoint."""
    out = nyt_get("lists/overview.json")
    if not out:
        return []
    lists_raw = (out.get("results") or {}).get("lists") or []
    result, seen = [], set()
    for lst in lists_raw:
        name = lst.get("list_name") or lst.get("list_name_encoded") or ""
        encoded = lst.get("list_name_encoded") or encode_list_name(name)
        if not name or not encoded or encoded in seen:
            continue
        seen.add(encoded)
        result.append({"list_name": name, "list_name_encoded": encoded})
    return result


def get_current_list(list_name_encoded):
    """Fetch the full current bestseller list for one category."""
    out = nyt_get(f"lists/current/{list_name_encoded}.json")
    if not out:
        return []
    return list((out.get("results") or {}).get("books") or [])


# --- Main fetch routine ---

def main():
    print("\n--- NYT Bestseller Data Fetcher ---\n")
    print("Fetching list names...")
    lists_meta = get_list_names()
    if not lists_meta:
        print("ERROR: No list names returned. Check API key and network.")
        sys.exit(2)

    print(f"Found {len(lists_meta)} lists. Fetching books for each...\n")
    payload = {
        "updated": datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
        "lists": [],
    }

    for i, meta in enumerate(lists_meta):
        if i > 0:
            time.sleep(1.5)
        name = meta["list_name"]
        encoded = meta["list_name_encoded"]
        books = get_current_list(encoded)
        payload["lists"].append({
            "list_name": name,
            "list_name_encoded": encoded,
            "books": books,
        })
        print(f"  [{i+1}/{len(lists_meta)}] {name}: {len(books)} books")

    OUT_DIR.mkdir(parents=True, exist_ok=True)
    with open(OUT_FILE, "w", encoding="utf-8") as f:
        json.dump(payload, f, indent=2, ensure_ascii=False)

    total_books = sum(len(l["books"]) for l in payload["lists"])
    print(f"\nSaved {total_books} books across {len(payload['lists'])} lists to {OUT_FILE}")
    print("Done.\n")
    return 0


if __name__ == "__main__":
    sys.exit(main())
