"""
ai_portfolio_extraction.py
═══════════════════════════
Extract AI-recommended portfolios from all 762 non-prime single-AI
conversation transcripts using GPT-4o, with concurrency 10.

Requires OPENAI_API_KEY env var.

Outputs:
    ai_portfolios_extracted.csv  — one row per participant w/ 25 weight columns
    ai_portfolios_raw.jsonl      — raw GPT responses for debugging / re-use
"""

import os
import sys
import json
import glob
import time
from concurrent.futures import ThreadPoolExecutor, as_completed

import pandas as pd
from openai import OpenAI


ROOT        = "/Users/shiyang/Desktop/26 Spring/Biased AI"
TRANSCRIPTS = f"{ROOT}/transcripts"
OUT_CSV     = f"{ROOT}/notebooks/ai_portfolios_extracted.csv"
OUT_JSONL   = f"{ROOT}/notebooks/ai_portfolios_raw.jsonl"

CONCURRENCY = 10
MODEL       = "gpt-4o"

ASSETS = [
    "SHY", "IEF", "LQD", "GLD", "VNQ",
    "SPY", "XLF", "XLE", "XLI", "XLP", "XLU",
    "AAPL", "MSFT", "AMZN", "GOOGL", "NVDA", "TSLA",
    "SHOP", "SNOW", "PLTR", "DKNG", "RIVN", "CRSP",
    "BTC", "ETH",
]


SYSTEM_PROMPT = """You are a precise information-extraction assistant. Given a conversation transcript between a human participant and an AI investment coach, your job is to extract the AI's FINAL portfolio recommendation as structured JSON.

Be conservative: only assign weights the AI explicitly proposed in its most recent recommendation. Do not infer values the AI did not state. Output strict JSON with no surrounding commentary."""

USER_PROMPT_TEMPLATE = """The conversation below is from a paper-trading experiment in which an AI investment coach was instructed to recommend an allocation across EXACTLY these 25 assets:

SHY, IEF, LQD, GLD, VNQ, SPY, XLF, XLE, XLI, XLP, XLU, AAPL, MSFT, AMZN, GOOGL, NVDA, TSLA, SHOP, SNOW, PLTR, DKNG, RIVN, CRSP, BTC, ETH

Ticker aliases (map to canonical form):
  - BTC, BTC-USD, Bitcoin  → "BTC"
  - ETH, ETH-USD, Ethereum → "ETH"

TASK
Extract the AI's MOST RECENT (i.e., final) specific portfolio recommendation. If the AI revised its allocation across multiple rounds, use only the last one.

EXTRACTION RULES
1. If the AI proposed a specific percentage for an asset, use that number.
2. If the AI proposed a range (e.g., "15–20%"), use the midpoint.
3. If the AI did not include an asset in its FINAL recommendation, set its weight to 0.
4. If the AI's stated percentages do not sum to exactly 100, record the raw sum but also provide a normalized version that sums to 100 (proportionally rescaled).
5. If the AI's "final recommendation" is delta-based ("keep X, reduce Y by 5%") rather than a complete portfolio, AND you cannot construct the full portfolio from the conversation alone, mark extractable=false.
6. If no specific portfolio recommendation exists at all (e.g., the AI only gave qualitative advice), mark extractable=false.

OUTPUT FORMAT
Return JSON with exactly this schema:

If extraction succeeded:
{
  "extractable": true,
  "final_round": <integer indicating which round contained the final rec>,
  "raw_sum": <number; sum of AI's stated percentages before any normalization>,
  "weights": {
    "SHY": <number>,  "IEF": <number>,  ... ,  "ETH": <number>
  },
  "notes": "<optional brief note about ambiguity or normalization>"
}

If extraction failed:
{
  "extractable": false,
  "reason": "<brief explanation: e.g. 'delta-only', 'no specific percentages', 'unclear'>"
}

The "weights" object MUST contain all 25 canonical tickers as keys, with numeric values. Use 0 for assets the AI did not include.

CONVERSATION TRANSCRIPT:
========================
{conversation}
========================"""


def format_conversation(convo_json):
    lines = []
    round_num = 0
    for msg in convo_json:
        role  = msg.get("role")
        parts = str(msg.get("parts", ""))
        if role == "user":
            round_num += 1
        label = "USER" if role == "user" else "AI"
        lines.append(f"{label} (round {round_num}): {parts}")
    return "\n\n".join(lines)


def get_persona(pid):
    fp = os.path.join(TRANSCRIPTS, f"{pid}-singleAI-persona")
    if not os.path.exists(fp):
        return None
    try:
        with open(fp) as f:
            obj = json.loads(f.read().strip())
        return obj.get("risk_bias", "?")
    except Exception:
        return None


def extract_one(client, conversation_text):
    response = client.chat.completions.create(
        model=MODEL,
        messages=[
            {"role": "system", "content": SYSTEM_PROMPT},
            {"role": "user",   "content": USER_PROMPT_TEMPLATE.replace("{conversation}", conversation_text)},
        ],
        temperature=0,
        response_format={"type": "json_object"},
    )
    return json.loads(response.choices[0].message.content)


def process_one(client, fp):
    pid  = os.path.basename(fp).split("-")[0]
    bias = get_persona(pid) or "unknown"
    try:
        with open(fp) as f:
            convo = json.loads(f.read().strip())
        convo_text = format_conversation(convo)
        result = extract_one(client, convo_text)
    except Exception as e:
        result = {"extractable": False, "reason": f"API error: {type(e).__name__}: {e}"}
    return {"pid": pid, "bias": bias, "result": result}


def main():
    if "OPENAI_API_KEY" not in os.environ:
        sys.exit("ERROR: OPENAI_API_KEY env var not set. Run:\n  export OPENAI_API_KEY=sk-...")
    client = OpenAI()

    files = sorted(glob.glob(os.path.join(TRANSCRIPTS, "*-1-singleAI-conversation")))
    files = [f for f in files if not f.endswith("-prime")]
    print(f"Processing {len(files)} non-prime conversations  (model={MODEL}, concurrency={CONCURRENCY})\n")

    results = []
    start = time.time()
    with ThreadPoolExecutor(max_workers=CONCURRENCY) as ex:
        futures = {ex.submit(process_one, client, fp): fp for fp in files}
        for i, fut in enumerate(as_completed(futures)):
            results.append(fut.result())
            if (i + 1) % 50 == 0 or (i + 1) == len(files):
                elapsed = time.time() - start
                n_ok = sum(1 for r in results if r["result"].get("extractable"))
                rate = (i + 1) / elapsed if elapsed > 0 else 0
                eta = (len(files) - (i + 1)) / rate if rate > 0 else 0
                print(f"  [{i+1:>4}/{len(files)}]  {elapsed:>5.0f}s elapsed  |  {n_ok:>4} extractable  |  ETA ~{eta:.0f}s")

    # Save raw responses for debugging
    with open(OUT_JSONL, "w") as f:
        for r in results:
            f.write(json.dumps(r) + "\n")
    print(f"\nRaw responses → {OUT_JSONL}")

    # Build flat CSV
    rows = []
    for r in results:
        res = r["result"]
        row = {"pid": r["pid"], "bias": r["bias"],
               "extractable": bool(res.get("extractable"))}
        if res.get("extractable"):
            weights = res.get("weights") or {}
            for a in ASSETS:
                row[f"ai_w_{a}"] = float(weights.get(a, 0) or 0)
            row["raw_sum"]      = res.get("raw_sum")
            row["final_round"]  = res.get("final_round")
            row["notes"]        = res.get("notes", "")
            row["fail_reason"]  = ""
        else:
            for a in ASSETS:
                row[f"ai_w_{a}"] = None
            row["raw_sum"]      = None
            row["final_round"]  = None
            row["notes"]        = ""
            row["fail_reason"]  = res.get("reason", "")
        rows.append(row)

    df = pd.DataFrame(rows)
    df.to_csv(OUT_CSV, index=False)
    print(f"Flat CSV → {OUT_CSV}")

    n_ok = int(df["extractable"].sum())
    print(f"\n=== EXTRACTION SUMMARY ===")
    print(f"Extracted : {n_ok} / {len(df)}  ({n_ok/len(df)*100:.1f}%)")
    print(f"Failed    : {len(df) - n_ok}")
    if (len(df) - n_ok) > 0:
        fail_reasons = df.loc[~df["extractable"], "fail_reason"].value_counts().head(10)
        print(f"Top fail reasons:")
        print(fail_reasons.to_string())
    print(f"\nBy bias condition:")
    print(df.groupby("bias")["extractable"].agg(["count", "sum"]).rename(columns={"sum": "extracted"}).to_string())


if __name__ == "__main__":
    main()
