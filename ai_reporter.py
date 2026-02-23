# ai_reporter.py
# AI-Powered NYT Bestseller Reporter (standalone script)
# Queries the NYT Books API, processes data, and uses Ollama or OpenAI
# to generate an insightful report on current bestseller trends.

import os
import sys
import json
import requests
from pathlib import Path
from dotenv import load_dotenv

# --- Setup ---

script_dir = Path(__file__).resolve().parent
DATA_FILE = script_dir / "data" / "bestsellers.json"

for p in [script_dir / ".env", Path.cwd() / ".env"]:
    if p.exists():
        load_dotenv(p)
        break
else:
    load_dotenv()

NYT_API_KEY = os.getenv("NYT_API_KEY")
OPENAI_API_KEY = os.getenv("OPENAI_API_KEY", "")
OLLAMA_API_KEY = os.getenv("OLLAMA_API_KEY", "")
OLLAMA_PORT = int(os.getenv("OLLAMA_PORT", "11434"))
OLLAMA_MODEL = os.getenv("OLLAMA_MODEL", "smollm2:1.7b")
OLLAMA_CLOUD_MODEL = os.getenv("OLLAMA_CLOUD_MODEL", "gemma3:4b")

# Priority: OpenAI > Ollama Cloud > Ollama Local
if OPENAI_API_KEY:
    AI_BACKEND = "openai"
elif OLLAMA_API_KEY:
    AI_BACKEND = "ollama_cloud"
else:
    AI_BACKEND = "ollama_local"


# --- 1. Load or fetch data ---

def load_data():
    """Load bestseller data from cache or live API."""

    # Prefer cached JSON (from fetch_bestsellers.py)
    if DATA_FILE.exists():
        print(f"Loading cached data from {DATA_FILE}...")
        with open(DATA_FILE, "r", encoding="utf-8") as f:
            cached = json.load(f)
        if cached.get("lists"):
            return cached
        print("  Cache empty; falling back to API.")

    # Fall back to live API
    if not NYT_API_KEY:
        print("ERROR: No cached data and NYT_API_KEY not set.")
        print("  Run fetch_bestsellers.py first, or add NYT_API_KEY to .env")
        sys.exit(1)

    print("Fetching live data from NYT API...")
    url = "https://api.nytimes.com/svc/books/v3/lists/overview.json"
    resp = requests.get(url, params={"api-key": NYT_API_KEY}, timeout=25)
    resp.raise_for_status()
    data = resp.json()
    lists_raw = (data.get("results") or {}).get("lists") or []
    return {"lists": [
        {
            "list_name": lst.get("list_name", ""),
            "list_name_encoded": lst.get("list_name_encoded", ""),
            "books": lst.get("books", []),
        }
        for lst in lists_raw
    ]}


# --- 2. Process data ---

def process_data(raw):
    """Flatten all books into rows and compute summary stats."""
    rows = []
    for lst in raw.get("lists", []):
        list_name = lst.get("list_name", "Unknown")
        for book in lst.get("books", []):
            rows.append({
                "list": list_name,
                "rank": book.get("rank"),
                "title": book.get("title", "Unknown"),
                "author": book.get("author", "Unknown"),
                "weeks_on_list": book.get("weeks_on_list", 0),
                "publisher": book.get("publisher", "Unknown"),
                "description": book.get("description", ""),
            })
    return rows


def format_table(books, columns):
    """Format a list of book dicts into an aligned text table."""
    header = " | ".join(col.ljust(22) for col in columns)
    lines = [header, "-" * len(header)]
    for b in books:
        line = " | ".join(str(b.get(col, "")).ljust(22) for col in columns)
        lines.append(line)
    return "\n".join(lines)


def build_summary(rows):
    """Build a structured text summary of the bestseller data for the AI."""
    total_books = len(rows)
    total_lists = len(set(r["list"] for r in rows))

    top_ranked = [r for r in rows if r["rank"] == 1]
    top_longevity = sorted(rows, key=lambda r: r["weeks_on_list"], reverse=True)[:10]

    ranked_table = format_table(top_ranked, ["list", "title", "author", "weeks_on_list"])
    longevity_table = format_table(top_longevity, ["list", "rank", "title", "author", "weeks_on_list"])

    return f"""NYT Bestseller Data (current week):
- Total lists: {total_lists}
- Total books: {total_books}

#1 Ranked Books by List:
{ranked_table}

Top 10 Books by Weeks on List (longest-running bestsellers):
{longevity_table}
"""


# --- 3. AI report generation ---

def build_system_prompt():
    from datetime import date
    today = date.today().strftime("%B %d, %Y")
    return f"""You are a book industry analyst writing a weekly NYT Bestseller briefing.
Start the report with this exact title line:
Weekly NYT Bestseller Briefing \u2013 {today}

Then write 6-8 bullet points covering:
1. \U0001f4ca Key trends across the bestseller lists this week
2. \U0001f31f Which books have the longest staying power and why that matters
3. \U0001f4da Notable patterns in genres, publishers, or authors
4. \U0001f4a1 One actionable recommendation for a reader looking for their next book

Use emojis at the start of each bullet point to make the report lively and scannable.
Use **bold** (double asterisks) for emphasis — never use *italics* (single asterisks).
Use clear, professional but friendly language.
Do NOT end with questions, offers for follow-up, or suggestions to the reader.
End the report cleanly after the last bullet point."""


def generate_report_ollama_local(data_summary):
    """Generate a report using Ollama running locally on your machine."""
    url = f"http://localhost:{OLLAMA_PORT}/api/generate"
    body = {
        "model": OLLAMA_MODEL,
        "prompt": f"{build_system_prompt()}\n\nDATA:\n{data_summary}",
        "stream": False,
    }
    resp = requests.post(url, json=body, timeout=180)
    resp.raise_for_status()
    return resp.json().get("response", "(no response)")


def generate_report_ollama_cloud(data_summary):
    """Generate a report using Ollama Cloud (ollama.com hosted models)."""
    url = "https://ollama.com/api/chat"
    headers = {
        "Authorization": f"Bearer {OLLAMA_API_KEY}",
        "Content-Type": "application/json",
    }
    body = {
        "model": OLLAMA_CLOUD_MODEL,
        "messages": [
            {"role": "system", "content": build_system_prompt()},
            {"role": "user", "content": f"Here is the current data:\n\n{data_summary}"},
        ],
        "stream": False,
    }
    resp = requests.post(url, headers=headers, json=body, timeout=120)
    resp.raise_for_status()
    return resp.json()["message"]["content"]


def generate_report_openai(data_summary):
    """Generate a report using the OpenAI API."""
    url = "https://api.openai.com/v1/chat/completions"
    headers = {
        "Authorization": f"Bearer {OPENAI_API_KEY}",
        "Content-Type": "application/json",
    }
    body = {
        "model": "gpt-4o-mini",
        "messages": [
            {"role": "system", "content": build_system_prompt()},
            {"role": "user", "content": f"Here is the current data:\n\n{data_summary}"},
        ],
        "temperature": 0.7,
        "max_tokens": 1024,
    }
    resp = requests.post(url, headers=headers, json=body, timeout=60)
    resp.raise_for_status()
    return resp.json()["choices"][0]["message"]["content"]


# --- 4. Main ---

def main():
    print("\n" + "=" * 60)
    print("  NYT Bestseller AI Reporter")
    print("=" * 60 + "\n")

    # Step 1: Load data
    raw = load_data()
    rows = process_data(raw)
    print(f"  Processed {len(rows)} books across {len(set(r['list'] for r in rows))} lists.\n")

    if not rows:
        print("ERROR: No book data available.")
        sys.exit(1)

    # Step 2: Build summary
    data_summary = build_summary(rows)
    print(f"  Summary ready ({len(data_summary)} chars).\n")

    # Step 3: Generate AI report
    backend_labels = {
        "openai": "OpenAI (gpt-4o-mini)",
        "ollama_cloud": f"Ollama Cloud ({OLLAMA_CLOUD_MODEL})",
        "ollama_local": f"Ollama Local ({OLLAMA_MODEL})",
    }
    backend = backend_labels[AI_BACKEND]
    print(f"  Generating report via {backend}...\n")

    try:
        if AI_BACKEND == "openai":
            report = generate_report_openai(data_summary)
        elif AI_BACKEND == "ollama_cloud":
            report = generate_report_ollama_cloud(data_summary)
        else:
            report = generate_report_ollama_local(data_summary)
    except requests.RequestException as e:
        print(f"ERROR: AI request failed: {e}")
        if AI_BACKEND == "ollama_local":
            print("  Make sure Ollama is running: ollama serve")
        sys.exit(1)

    # Step 4: Display
    print("=" * 60)
    print("  AI-GENERATED BESTSELLER REPORT")
    print("=" * 60)
    print(report)
    print("=" * 60)

    # Step 5: Save
    out_path = script_dir / "data" / "ai_report.txt"
    out_path.parent.mkdir(parents=True, exist_ok=True)
    with open(out_path, "w", encoding="utf-8") as f:
        f.write("AI-Generated NYT Bestseller Report\n")
        f.write(f"Backend: {backend}\n")
        f.write("=" * 40 + "\n\n")
        f.write(report)
        f.write("\n\n--- Data Summary Used ---\n\n")
        f.write(data_summary)

    print(f"\n  Report saved to {out_path}\n")
    print("  Done.\n")
    return 0


if __name__ == "__main__":
    sys.exit(main())
