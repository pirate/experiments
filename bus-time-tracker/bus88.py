"""
Bus 88 Departure Tracker — AC Transit

Polls the AC Transit real-time API for Route 88 vehicles heading
from Piedmont (Highland Way & Highland Av) toward Downtown Berkeley.

When a bus has *left* the Piedmont terminus and started its journey,
the script prints an alert (and optionally fires a Home Assistant
webhook so you can flash lights / send a notification).

Requirements:
    pip install requests

Usage:
    # One-shot check
    python bus88.py

    # Poll every 60 seconds
    python bus88.py --poll 60

Environment variables:
    ACTRANSIT_API_TOKEN   – required, get one free at https://api.actransit.org/transit/
    HA_WEBHOOK_URL        – optional, Home Assistant webhook URL to POST when bus departs
"""

import argparse
import json
import os
import sys
import time
from datetime import datetime

import requests

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

API_TOKEN = os.environ.get("ACTRANSIT_API_TOKEN", "")
HA_WEBHOOK_URL = os.environ.get("HA_WEBHOOK_URL", "")

BASE_URL = "https://api.actransit.org/transit"

ROUTE = "88"

# The Piedmont terminus — first stop of the "To Downtown Berkeley" direction.
# Highland Way & Highland Av is the origin.  We use the predictions endpoint
# for this stop to know when the next bus is *about* to leave or has just left.
PIEDMONT_TERMINUS_NAME = "Highland Way & Highland Av"

# Direction of interest: Piedmont → Downtown Berkeley
DIRECTION = "To Downtown Berkeley"


# ---------------------------------------------------------------------------
# API helpers
# ---------------------------------------------------------------------------

def api_get(path: str, params: dict | None = None) -> dict | list:
    """Make an authenticated GET to the AC Transit API."""
    if not API_TOKEN:
        sys.exit(
            "Error: set ACTRANSIT_API_TOKEN env var.\n"
            "Register free at https://api.actransit.org/transit/"
        )
    params = params or {}
    params["token"] = API_TOKEN
    url = f"{BASE_URL}/{path}"
    resp = requests.get(url, params=params, timeout=15)
    resp.raise_for_status()
    return resp.json()


def get_route_stops() -> list[dict]:
    """Return the stops for Route 88 in the Berkeley-bound direction."""
    stops = api_get(f"route/{ROUTE}/stops", params={"direction": DIRECTION})
    return stops


def get_stop_id_for_terminus(stops: list[dict]) -> str | None:
    """Find the stop ID for the Piedmont terminus from the stops list."""
    for stop in stops:
        name = stop.get("Name", "") or stop.get("name", "")
        stop_id = stop.get("StopId") or stop.get("stopId") or stop.get("stpid")
        if "highland" in name.lower() and "highland" in name.lower():
            return str(stop_id)
    return None


def get_realtime_vehicles() -> list[dict]:
    """Return real-time vehicle positions for Route 88."""
    return api_get("actrealtime/vehicle", params={"rt": ROUTE})


def get_predictions_for_stop(stop_id: str, direction: str | None = None) -> list[dict]:
    """Return arrival/departure predictions for a specific stop."""
    params = {"stpid": stop_id, "rt": ROUTE}
    data = api_get("actrealtime/prediction", params=params)
    if direction:
        data = [p for p in data if direction.lower() in (p.get("rtdir", "") or "").lower()]
    return data


# ---------------------------------------------------------------------------
# Core logic
# ---------------------------------------------------------------------------

def find_departed_buses(vehicles: list[dict], terminus_stop_id: str) -> list[dict]:
    """
    From the list of real-time vehicles on Route 88, find those that:
      1. Are heading in the Berkeley-bound direction
      2. Have already passed the Piedmont terminus (i.e. the bus is in
         service and moving — its next predicted stop is NOT the terminus).

    This means the bus has started its run and you have ~5 minutes.
    """
    departed = []
    for v in vehicles:
        vid = v.get("vid") or v.get("VehicleId") or v.get("vehicleId")
        rt_dir = v.get("rtdir", "") or v.get("Direction", "") or ""
        next_stop = v.get("stpnm", "") or v.get("NextStopName", "") or ""
        pattern_dist = v.get("pdist") or v.get("patternDistance")

        # Only care about Berkeley-bound buses
        if "berkeley" not in rt_dir.lower() and "westbound" not in rt_dir.lower():
            # The direction string might vary; also try matching on the
            # raw direction field the API returns.
            if DIRECTION.lower() not in rt_dir.lower():
                continue

        # If the bus's next stop is still the terminus, it hasn't left yet.
        if "highland" in next_stop.lower():
            continue

        departed.append({
            "vehicle_id": vid,
            "direction": rt_dir,
            "next_stop": next_stop,
            "pattern_distance": pattern_dist,
            "lat": v.get("lat"),
            "lon": v.get("lon"),
        })

    return departed


def check_predictions(stop_id: str) -> list[dict]:
    """
    Check predictions at the Piedmont terminus.  If a prediction shows
    a very short ETA (or 'DUE'), the bus is about to leave / just left.
    """
    preds = get_predictions_for_stop(stop_id, direction=DIRECTION)
    upcoming = []
    for p in preds:
        eta_text = p.get("prdctdn", "") or p.get("PredictedMinutes", "")
        upcoming.append({
            "vehicle_id": p.get("vid"),
            "route": p.get("rt"),
            "direction": p.get("rtdir"),
            "stop": p.get("stpnm"),
            "eta_minutes": eta_text,
            "predicted_time": p.get("prdtm"),
        })
    return upcoming


def notify_home_assistant(bus_info: dict):
    """Fire a Home Assistant webhook with bus departure info."""
    if not HA_WEBHOOK_URL:
        return
    try:
        requests.post(HA_WEBHOOK_URL, json=bus_info, timeout=10)
    except Exception as e:
        print(f"  [HA webhook error: {e}]")


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def run_check():
    """Run a single check and print results."""
    now = datetime.now().strftime("%H:%M:%S")
    print(f"\n[{now}] Checking Route 88 — Piedmont → Downtown Berkeley")
    print("-" * 55)

    # --- Step 1: resolve the terminus stop ID ---------------------------------
    try:
        stops = get_route_stops()
    except Exception as e:
        print(f"  Could not fetch stops: {e}")
        stops = []

    terminus_id = get_stop_id_for_terminus(stops) if stops else None
    if terminus_id:
        print(f"  Piedmont terminus stop ID: {terminus_id}")
    else:
        print("  Could not resolve terminus stop ID from route data.")
        print("  Falling back to vehicle-position-only detection.")

    # --- Step 2: check predictions at the terminus ----------------------------
    if terminus_id:
        try:
            preds = check_predictions(terminus_id)
            if preds:
                print(f"\n  Predictions at Piedmont terminus ({terminus_id}):")
                for p in preds:
                    eta = p["eta_minutes"]
                    label = "DUE / DEPARTING NOW" if str(eta).upper() in ("DUE", "0", "1") else f"{eta} min"
                    print(f"    Bus {p['vehicle_id']}  →  {p['direction']}  |  ETA: {label}  ({p['predicted_time']})")
            else:
                print("  No upcoming predictions at the terminus right now.")
        except Exception as e:
            print(f"  Prediction lookup failed: {e}")

    # --- Step 3: check vehicle positions for already-departed buses -----------
    try:
        vehicles = get_realtime_vehicles()
    except Exception as e:
        print(f"  Could not fetch vehicles: {e}")
        vehicles = []

    if vehicles:
        departed = find_departed_buses(vehicles, terminus_id or "")
        if departed:
            print(f"\n  ** {len(departed)} bus(es) have LEFT Piedmont and are en route to Berkeley:")
            for b in departed:
                print(f"    Bus {b['vehicle_id']}  |  next stop: {b['next_stop']}  |  pos: ({b['lat']}, {b['lon']})")
                notify_home_assistant(b)
        else:
            print("  No Berkeley-bound buses have departed Piedmont yet.")
    else:
        print("  No active vehicles on Route 88 right now.")

    print()


def main():
    parser = argparse.ArgumentParser(description="Track AC Transit Route 88 departures from Piedmont")
    parser.add_argument(
        "--poll",
        type=int,
        default=0,
        metavar="SECONDS",
        help="Poll interval in seconds (0 = one-shot, default)",
    )
    args = parser.parse_args()

    if args.poll > 0:
        print(f"Polling every {args.poll}s — Ctrl-C to stop.\n")
        try:
            while True:
                run_check()
                time.sleep(args.poll)
        except KeyboardInterrupt:
            print("\nStopped.")
    else:
        run_check()


if __name__ == "__main__":
    main()
