"""
ai_portfolio_extraction_dual.py
════════════════════════════════
Extract BOTH AIs' final recommended portfolios from the dual-AI conversation
transcripts. The AI1/AI2 conversation files are identical copies of the full
interleaved conversation (each model message prefixed "(AI 1)" / "(AI 2)"),
so ONE GPT call per participant extracts both recommendations.

Mirrors ai_portfolio_extraction.py (single-AI): GPT-4o, temperature 0,
JSON mode, concurrency 10. Requires OPENAI_API_KEY (env or ~/.openai_key).

Outputs:
    ai_portfolios_dual_extracted.csv — one row per participant,
        ai1_w_* / ai2_w_* weight columns + per-AI extraction metadata
    ai_portfolios_dual_raw.jsonl     — raw GPT responses
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
OUT_CSV     = f"{ROOT}/notebooks/ai_portfolios_dual_extracted.csv"
OUT_JSONL   = f"{ROOT}/notebooks/ai_portfolios_dual_raw.jsonl"

CONCURRENCY = 10
MODEL       = "gpt-4o"

ASSETS = [
    "SHY", "IEF", "LQD", "GLD", "VNQ",
    "SPY", "XLF", "XLE", "XLI", "XLP", "XLU",
    "AAPL", "MSFT", "AMZN", "GOOGL", "NVDA", "TSLA",
    "SHOP", "SNOW", "PLTR", "DKNG", "RIVN", "CRSP",
    "BTC", "ETH",
]


SYSTEM_PROMPT = """You are a precise information-extraction assistant. Given a conversation transcript between a human participant and TWO AI investment coaches (labeled "(AI 1)" and "(AI 2)"), your job is to extract EACH AI's FINAL portfolio recommendation as structured JSON.

Be conservative: only assign weights an AI explicitly proposed in its own most recent recommendation. Never mix the two AIs' recommendations. Do not infer values an AI did not state. Output strict JSON with no surrounding commentary."""

USER_PROMPT_TEMPLATE = """The conversation below is from a paper-trading experiment in which TWO AI investment coaches each recommend an allocation across EXACTLY these 25 assets:

SHY, IEF, LQD, GLD, VNQ, SPY, XLF, XLE, XLI, XLP, XLU, AAPL, MSFT, AMZN, GOOGL, NVDA, TSLA, SHOP, SNOW, PLTR, DKNG, RIVN, CRSP, BTC, ETH

Ticker aliases (map to canonical form):
  - BTC, BTC-USD, Bitcoin  → "BTC"
  - ETH, ETH-USD, Ethereum → "ETH"

Every AI message is prefixed with "(AI 1)" or "(AI 2)" identifying the speaker.

TASK
For EACH of the two AIs separately, extract that AI's MOST RECENT (i.e., final) specific portfolio recommendation. If an AI revised its allocation across multiple rounds, use only its last one. Attribute each recommendation strictly to the AI whose message contains it.

EXTRACTION RULES (applied to each AI independently)
1. If the AI proposed a specific percentage for an asset, use that number.
2. If the AI proposed a range (e.g., "15–20%"), use the midpoint.
3. If the AI did not include an asset in its FINAL recommendation, set its weight to 0.
4. If the AI's stated percentages do not sum to exactly 100, record the raw sum but also provide a normalized version that sums to 100 (proportionally rescaled).
5. If the AI's "final recommendation" is delta-based ("keep X, reduce Y by 5%") rather than a complete portfolio, AND you cannot construct the full portfolio from the conversation alone, mark that AI's extractable=false.
6. If an AI never gave a specific portfolio recommendation at all (e.g., only qualitative advice), mark that AI's extractable=false.

OUTPUT FORMAT
Return JSON with exactly this schema:

{
  "ai1": <extraction object for AI 1>,
  "ai2": <extraction object for AI 2>
}

where each extraction object is, on success:
{
  "extractable": true,
  "final_round": <integer round of that AI's final rec>,
  "raw_sum": <number; sum of that AI's stated percentages before normalization>,
  "weights": { "SHY": <number>, "IEF": <number>, ..., "ETH": <number> },
  "notes": "<optional brief note about ambiguity or normalization>"
}

or, on failure:
{
  "extractable": false,
  "reason": "<brief explanation: e.g. 'delta-only', 'no specific percentages', 'unclear'>"
}

Each "weights" object MUST contain all 25 canonical tickers as keys, with numeric values. Use 0 for assets that AI did not include.

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
    fp = os.path.join(TRANSCRIPTS, f"{pid}-dualAI-persona")
    if not os.path.exists(fp):
        return {}
    try:
        with open(fp) as f:
            return json.loads(f.read().strip())
    except Exception:
        return {}


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
    pid     = os.path.basename(fp).split("-")[0]
    persona = get_persona(pid)
    try:
        with open(fp) as f:
            convo = json.loads(f.read().strip())
        convo_text = format_conversation(convo)
        result = extract_one(client, convo_text)
    except Exception as e:
        err = {"extractable": False, "reason": f"API error: {type(e).__name__}: {e}"}
        result = {"ai1": err, "ai2": err}
    return {"pid": pid,
            "condition": persona.get("condition", "unknown"),
            "ai1_bias":  persona.get("ai1_bias", ""),
            "ai2_bias":  persona.get("ai2_bias", ""),
            "result": result}


def slot_cols(res, slot):
    """Flatten one AI's extraction object into CSV columns."""
    obj = res.get(slot) or {}
    row = {f"{slot}_extractable": bool(obj.get("extractable"))}
    if obj.get("extractable"):
        weights = obj.get("weights") or {}
        for a in ASSETS:
            row[f"{slot}_w_{a}"] = float(weights.get(a, 0) or 0)
        row[f"{slot}_raw_sum"]     = obj.get("raw_sum")
        row[f"{slot}_final_round"] = obj.get("final_round")
        row[f"{slot}_notes"]       = obj.get("notes", "")
        row[f"{slot}_fail_reason"] = ""
    else:
        for a in ASSETS:
            row[f"{slot}_w_{a}"] = None
        row[f"{slot}_raw_sum"]     = None
        row[f"{slot}_final_round"] = None
        row[f"{slot}_notes"]       = ""
        row[f"{slot}_fail_reason"] = obj.get("reason", "")
    return row


def main():
    if "OPENAI_API_KEY" not in os.environ:
        key_file = os.path.expanduser("~/.openai_key")
        if os.path.exists(key_file):
            os.environ["OPENAI_API_KEY"] = open(key_file).read().strip()
        else:
            sys.exit("ERROR: OPENAI_API_KEY env var not set and ~/.openai_key not found.")
    client = OpenAI()

    files = sorted(glob.glob(os.path.join(TRANSCRIPTS, "*-1-dualAI-AI1-conversation")))
    print(f"Processing {len(files)} dual-AI conversations  (model={MODEL}, concurrency={CONCURRENCY})\n")

    results = []
    start = time.time()
    with ThreadPoolExecutor(max_workers=CONCURRENCY) as ex:
        futures = {ex.submit(process_one, client, fp): fp for fp in files}
        for i, fut in enumerate(as_completed(futures)):
            results.append(fut.result())
            if (i + 1) % 50 == 0 or (i + 1) == len(files):
                elapsed = time.time() - start
                n_ok = sum(1 for r in results
                           if r["result"].get("ai1", {}).get("extractable")
                           and r["result"].get("ai2", {}).get("extractable"))
                rate = (i + 1) / elapsed if elapsed > 0 else 0
                eta = (len(files) - (i + 1)) / rate if rate > 0 else 0
                print(f"  [{i+1:>4}/{len(files)}]  {elapsed:>5.0f}s elapsed  |  "
                      f"{n_ok:>4} both-extractable  |  ETA ~{eta:.0f}s", flush=True)

    with open(OUT_JSONL, "w") as f:
        for r in results:
            f.write(json.dumps(r) + "\n")
    print(f"\nRaw responses → {OUT_JSONL}")

    rows = []
    for r in results:
        row = {"pid": r["pid"], "condition": r["condition"],
               "ai1_bias": r["ai1_bias"], "ai2_bias": r["ai2_bias"]}
        row.update(slot_cols(r["result"], "ai1"))
        row.update(slot_cols(r["result"], "ai2"))
        rows.append(row)

    df = pd.DataFrame(rows)
    df.to_csv(OUT_CSV, index=False)
    print(f"Flat CSV → {OUT_CSV}")

    both = int((df["ai1_extractable"] & df["ai2_extractable"]).sum())
    print(f"\n=== EXTRACTION SUMMARY ===")
    print(f"Both AIs extracted : {both} / {len(df)}  ({both/len(df)*100:.1f}%)")
    for slot in ("ai1", "ai2"):
        n_ok = int(df[f"{slot}_extractable"].sum())
        print(f"{slot} extracted     : {n_ok} / {len(df)}")
        n_fail = len(df) - n_ok
        if n_fail:
            print(df.loc[~df[f"{slot}_extractable"], f"{slot}_fail_reason"]
                  .value_counts().head(5).to_string())
    print(f"\nBy condition:")
    print(df.assign(both=df["ai1_extractable"] & df["ai2_extractable"])
            .groupby("condition")["both"].agg(["count", "sum"])
            .rename(columns={"sum": "both_extracted"}).to_string())


if __name__ == "__main__":
    main()
