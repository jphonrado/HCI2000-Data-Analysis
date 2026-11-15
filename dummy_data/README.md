# Dummy data

Simulated data for building and testing the analysis pipeline. **Not real participants.**

## How it was made
`generate_dummy_data.py` runs the **real controller** (`~/.hammerspoon/study/controller.lua` and
`storage.lua`, schema 2.1, controller 2.3.0) against a simulated Hammerspoon: a fake clock,
Chrome window, mouse, audio playback and accessibility title notifications. Simulated
participants click tabs. The controller detects the title changes and writes its records
exactly as it would in a live session, including timer jitter, the 20 s timeout,
automatic progression and mouse traces.

The three Google Forms exports are simulated separately, using matching codes and times.

Regenerate it (seeded, same output every time; move the old `records/` folder away first):
```
python3 generate_dummy_data.py --hammerspoon ~/.hammerspoon
```
This needs `lupa` (Lua 5.4) and, optionally, `ffprobe`. ffprobe reads the real prompt
lengths from `orlando_audio/`.

## Files
```
records/main/P001/
    COMPLETED.json                       # participant finished (main-mode guard)
    20261116T010120Z-316433980481813/    # one folder per session (session_id)
        main_data.csv     # THE ANALYSIS TABLE: 24 rows, one per trial, 25 columns
        events.csv        # every controller event (prompts, reveals, title changes, mouse traces...)
        trials_aux.csv    # raw polling observations behind main_data.csv
        session.json      # identity, block plan, versions, settings
        checkpoint.json   # last controller snapshot
records/main/P002/ ... P016/
screening_survey.csv    # screening form: 18 rows (16 enrolled + 2 screened out)
block_survey.csv        # NASA-TLX block survey: 64 rows, ratings 0-10
weighting_form.csv      # NASA-TLX pairwise weights: 16 rows, 15 pairs
```
Each session writes **its own** `main_data.csv`, so the pipeline must find and stack all 16.
The participant code is the same everywhere (`P001`-`P016`). Groups come from
`study/allocation.lua`, and condition order, content sets and targets from `study/materials.lua`.

The screening sheet has an extra last column, `Participant Code (Filled by Researcher)`.
You hard-code the code there after each session. It is blank for people who were screened out.

## Effects built in (the pipeline should recover these)
- **Time to the correct click** (median, error-free, before noise): H-L 1.9 s, V-L 2.1 s,
  H-H 3.3 s, V-H 2.6 s.
  - Main effect: high load is slower.
  - Interaction: vertical helps at high load only.
  - Also: participant speed differences, about 4% faster per block (practice), repeated
    targets 10% faster, and tabs further along are slower at high load (a stronger effect
    for horizontal tabs).
- **Wrong first click**: H-L 7%, V-L 8%, H-H 27%, V-H 13%, scaled per participant.
  A wrong click adds 0.6-1.5 s before the correct one.
- **TLX** (0-10): H-H > V-H > V-L ~ H-L. Physical demand stays low.
  Weights favour Mental > Effort > Temporal > Performance > Frustration > Physical.

## Edge cases planted
| Participant | Condition | Trial | What happened | Result in `main_data.csv` |
|---|---|---|---|---|
| P005 | V-H | T3 | Chrome window lost focus after one wrong click | `interrupted`, `technical_loss=true`, no time |
| P009 | H-L | T5 | Facilitator pressed pause (participant break) | `interrupted`, `technical_loss=false`, no time |
| P012 | H-H | T2 | No click before the 20 s deadline | `timeout`, `no_selection=true` |
| P002 | H-H | T4 | Three wrong tabs, then the 20 s deadline | `timeout`, `no_selection=false`, 3 incorrect |
| P014 | V-L | T4 | Target tab showed a stale title; facilitator pressed "." | `completed_manual`, no time |

Interrupted trials are **not replayed**. The session moves on to the next trial, so every
participant still has 24 rows.

## Real controller behaviour this exposed
At the end of each block, input stays locked while the participant fills in the TLX form.
The 120 s input-lock watchdog then fires, which logs `PAUSE_OR_INTERRUPTION` /
"Input-lock watchdog expired" in `events.csv` (64 times here) and shows a "Stopped" alert.
It never creates or changes a `main_data.csv` row.

## Assumptions
- The answer options for the screening questions are guesses.
- Weighting-form answers are recorded as the dimension name (e.g. `Mental Demand`).
- Accessibility title notifications are assumed to work on the study Mac
  (`measurement_source = controller_accessibility_and_poll`). Without them the value would be
  `controller_poll_only`.
- The researcher approves "All tabs visible" for every block, so trials advance automatically.
