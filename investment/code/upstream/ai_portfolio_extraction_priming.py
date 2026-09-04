"""
ai_portfolio_extraction_priming.py
═══════════════════════════════════
Extract AI-recommended portfolios from the PRIMING-condition conversation
transcripts (`{pid}-1-singleAI-conversation-prime`). In the priming experiment
the AI is always the default model; participants are merely *told* it is biased.
Same GPT-4o prompt/schema as the main extraction. Writes ai_portfolios_priming.csv.

Usage:
    export OPENAI_API_KEY=sk-...
    python ai_portfolio_extraction_priming.py
"""

import os, sys, json, glob, time
from concurrent.futures import ThreadPoolExecutor, as_completed
import pandas as pd
from openai import OpenAI

ROOT        = "/Users/shiyang/Desktop/26 Spring/Biased AI"
TRANSCRIPTS = f"{ROOT}/transcripts"
OUT_CSV     = f"{ROOT}/notebooks/ai_portfolios_priming.csv"
CONCURRENCY, MODEL = 10, "gpt-4o"

ASSETS = ["SHY","IEF","LQD","GLD","VNQ","SPY","XLF","XLE","XLI","XLP","XLU",
          "AAPL","MSFT","AMZN","GOOGL","NVDA","TSLA","SHOP","SNOW","PLTR","DKNG",
          "RIVN","CRSP","BTC","ETH"]

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
2. If the AI proposed a range (e.g., "15-20%"), use the midpoint.
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
  "reason": "<brief explanation>"
}

The "weights" object MUST contain all 25 canonical tickers as keys, with numeric values. Use 0 for assets the AI did not include.

CONVERSATION TRANSCRIPT:
========================
{conversation}
========================"""


def format_conversation(convo):
    lines, rnd = [], 0
    for m in convo:
        if m.get("role") == "user":
            rnd += 1
        lines.append(f"{'USER' if m.get('role')=='user' else 'AI'} (round {rnd}): {str(m.get('parts',''))}")
    return "\n\n".join(lines)


def get_persona(pid):
    fp = os.path.join(TRANSCRIPTS, f"{pid}-singleAI-persona-prime")
    if not os.path.exists(fp):
        return None
    try:
        return json.loads(open(fp).read().strip()).get("risk_bias", "?")
    except Exception:
        return None


def extract_one(client, text):
    resp = client.chat.completions.create(
        model=MODEL, temperature=0, response_format={"type": "json_object"},
        messages=[{"role": "system", "content": SYSTEM_PROMPT},
                  {"role": "user", "content": USER_PROMPT_TEMPLATE.replace("{conversation}", text)}])
    return json.loads(resp.choices[0].message.content)


def process_one(client, fp):
    pid = os.path.basename(fp).split("-")[0]
    try:
        convo = json.loads(open(fp).read().strip())
        res = extract_one(client, format_conversation(convo))
    except Exception as e:
        res = {"extractable": False, "reason": f"API error: {type(e).__name__}: {e}"}
    return {"pid": pid, "ai_risk_bias": get_persona(pid) or "", "result": res}


def to_row(rec):
    res = rec["result"]
    row = {"pid": rec["pid"], "ai_risk_bias": rec["ai_risk_bias"], "extractable": bool(res.get("extractable"))}
    w = res.get("weights") or {}
    for a in ASSETS:
        row[f"ai_w_{a}"] = float(w.get(a, 0) or 0) if res.get("extractable") else None
    row["raw_sum"]     = res.get("raw_sum") if res.get("extractable") else None
    row["fail_reason"] = "" if res.get("extractable") else res.get("reason", "")
    return row


def main():
    if "OPENAI_API_KEY" not in os.environ:
        sys.exit("ERROR: set OPENAI_API_KEY")
    client = OpenAI()
    files = sorted(glob.glob(os.path.join(TRANSCRIPTS, "*-1-singleAI-conversation-prime")))
    print(f"Priming (-prime) conversation files: {len(files)}")

    results, start = [], time.time()
    with ThreadPoolExecutor(max_workers=CONCURRENCY) as ex:
        futs = [ex.submit(process_one, client, f) for f in files]
        for i, fut in enumerate(as_completed(futs)):
            results.append(fut.result())
            if (i + 1) % 50 == 0 or (i + 1) == len(files):
                ok = sum(1 for r in results if r["result"].get("extractable"))
                print(f"  [{i+1:>4}/{len(files)}]  {time.time()-start:>4.0f}s  |  {ok} extractable")

    df = pd.DataFrame([to_row(r) for r in results])
    df.to_csv(OUT_CSV, index=False)
    n_ok = int(df["extractable"].sum())
    print(f"\nExtracted {n_ok}/{len(df)} ({100*n_ok/len(df):.1f}%) → {OUT_CSV}")


if __name__ == "__main__":
    main()
