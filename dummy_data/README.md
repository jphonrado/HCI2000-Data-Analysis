# Dummy data

Simulated data for building and testing the analysis pipeline. **Not real participants.**
Regenerate with `python3 generate_dummy_data.py` (seeded, same output every time).

## Files

| File | Mirrors | Rows |
|---|---|---|
| `main_data.csv` | Controller export (same columns as the pilot `main_data.csv`) | 384 = 16 participants x 4 blocks x 6 trials |
| `screening_survey.csv` | "Effects of Browser Tab Orientation..." screening form | 18 (16 enrolled + 2 screened out) |
| `block_survey.csv` | NASA-TLX post-condition block survey, ratings on **0-10** | 64 = 16 x 4 |
| `weighting_form.tsv` | NASA-TLX pairwise weighting form (15 pairs) | 16 |

Every file uses the same participant codes, `P001` to `P016`.

## Design used
- Groups come from `allocation.csv`. Condition and content-set order follow paper Table 1:
  - G1: H-L, H-H, V-L, V-H with sets A, B, C, D
  - G2: H-H, H-L, V-H, V-L with sets C, D, A, B
  - G3: V-L, V-H, H-L, H-H with sets D, C, B, A
  - G4: V-H, V-L, H-H, H-L with sets B, A, D, C
- Target schedules follow Table 3. High-load tab positions follow Table 2a.
- The screening sheet has an extra column at the end, `Participant Code (Filled by Researcher)`.
  That is where you hard-code the code after a session. It is blank for people who were screened out.

## Effects built in (the pipeline should recover these)
- **Retrieval time**, median before noise: H-L 2.2 s, V-L 2.4 s, H-H 4.0 s, V-H 3.2 s.
  - Load effect: high is slower.
  - Interaction: vertical helps at high load and is slightly worse at low load.
  - Also: a small practice effect across blocks, repeated targets are faster,
    tabs further along are slower at high load, and each wrong selection adds 0.6-1.4 s.
- **First-selection accuracy**: H-L .93, V-L .91, H-H .72, V-H .85.
- **TLX**: H-H > V-H > V-L ~ H-L. Physical demand stays low.
  - Weights favour Mental > Effort > Temporal > Performance > Frustration > Physical.

## Edge cases planted (for testing exclusion rules)
| Participant | Block | Trial | Case |
|---|---|---|---|
| P007 | 3 | T4 | `technical_loss`: no time recorded |
| P012 | 2 | T5 | `timeout`: `no_selection=true`, no time recorded |
| P003 | 1 | T2 | `manual_review`: time taken from video coding |
| P014 | 4 | T6 | `manual_review`: time taken from video coding |

Screening: one person agreed to consent but doesn't use a laptop browser.
Another declined consent.

## Assumptions to check against the real forms
- The answer options for the screening questions are guesses (e.g. "Horizontal (tabs along the top)").
- Weighting-form answers are recorded as the dimension name (e.g. `Mental Demand`).
