---
name: org-window-habit
description: Understand, create, inspect, and edit window-based Org habits in .org data files. Use for org-window-habit entries; CONFIG or OWH_CONFIG properties; rolling or calendar-aligned completion goals; window specs, assessment and reminder cadence, day restrictions, completion counting, conformity, versioned requirements, reset dates, and dated pauses or resumptions. Preserve Org history and local property-prefix conventions. Do not use for ordinary org-habit entries without window-habit configuration.
---

# Org Window Habit

Use this skill when working on Org data, even when the `org-window-habit` package repository is not present. Treat the configuration as executable history: understand the intended habit first, then make the smallest data edit that expresses it.

## Understand the model

A window habit asks: “At this evaluation time, have enough completions occurred inside the relevant time window?” This differs from ordinary `org-habit`, which mainly asks whether a task was completed on a fixed repeating schedule.

Model a habit with these concepts:

- **Completion:** A logged transition into any state in `org-done-keywords`. Completion history may be in a `LOGBOOK` drawer or inline state-change logs.
- **Window spec:** A lookback duration plus a required completion count. `(:duration (:days 7) :repetitions 3)` means three counted completions in the relevant seven-day window.
- **Assessment interval:** The step or bucket at which conformity, graphs, and streaks are evaluated. A seven-day window assessed daily rolls forward one day at a time; the same window assessed weekly advances one week at a time.
- **Conforming ratio:** Counted completions divided by the effective target. `1.0` means conforming; lower values mean behind. New habits scale the target while only part of the first window has elapsed.
- **Rescheduling:** After completion, schedule the next reminder for the first checked time at which the aggregated ratio falls below the threshold, subject to minimum spacing and allowed reminder days. The repeater on the Org timestamp is required for Org integration but does not define the actual cadence.

Distinguish three separate durations:

1. `:duration` inside a window spec controls how much completion history is considered.
2. `:assessment-interval` controls graph/streak evaluation buckets and how multiple completions are capped.
3. `:reschedule-assessment-interval` controls how finely the package searches for the next required reminder.

Do not set all three to the same value by reflex. For example, a five-times-per-calendar-week habit can use a weekly window and weekly streak assessment while retaining daily reminder checks.

## Choose the habit shape

Use these patterns as starting points.

### Rolling frequency

Require three completions in the trailing seven days and reassess daily:

```org
:OWH_CONFIG: (:window-specs ((:duration (:days 7) :repetitions 3)) :assessment-interval (:days 1))
```

At each daily assessment, look back seven days. Completing early builds buffer; the next reminder appears when the ratio is projected to fall below the threshold.

### Calendar-aligned period

Require five completions in each Monday-to-Monday week, count streaks weekly, but check daily for the next reminder:

```org
:OWH_CONFIG: (:window-specs ((:duration (:weeks 1 :start :monday) :repetitions 5)) :assessment-interval (:weeks 1 :start :monday) :reschedule-assessment-interval (:days 1) :max-reps-per-interval 5)
```

Set `:max-reps-per-interval` above the default `1` here because the assessment bucket is a whole week and up to five completions must count inside it.

Use `(:months 1)` for calendar months and `(:weeks 1 :start :DAY)` for calendar weeks. Use `(:days 7)` for a seven-day duration rather than a calendar week.

### Multiple simultaneous constraints

Prevent both long gaps and low overall frequency:

```org
:OWH_CONFIG: (:window-specs ((:duration (:days 4) :repetitions 1) (:duration (:days 10) :repetitions 4)))
```

By default, aggregate multiple specs with the minimum conforming ratio, so every spec must be satisfied. Use `org-window-habit-weighted-average-aggregation-fn` only when tradeoffs between specs are intentional; assign numeric `:value` weights to the specs.

### Completion days versus reminder days

Use `:only-days` to decide which completion days count. It also supplies reminder days when `:reschedule-days` is absent:

```org
:OWH_CONFIG: (:window-specs ((:duration (:days 7) :repetitions 3)) :only-days (:monday :wednesday :friday))
```

Use `:reschedule-days` when completions on all days should count but reminders should appear only on selected days:

```org
:OWH_CONFIG: (:window-specs ((:duration (:days 7) :repetitions 3)) :reschedule-days (:monday :tuesday :wednesday :thursday :friday))
```

When both are present, keep `:reschedule-days` within `:only-days`.

## Maintain a valid Org entry

Create an active habit with:

- a non-done TODO heading;
- a `SCHEDULED` or `DEADLINE` timestamp carrying any repeater;
- `STYLE` set to `habit`;
- a unified config containing `:window-specs`; and
- TODO-state logging so completions are recorded.

For example:

```org
* TODO Exercise
DEADLINE: <2026-08-09 Sun .+1d>
:PROPERTIES:
:STYLE: habit
:OWH_CONFIG: (:window-specs ((:duration (:days 7) :repetitions 3)))
:END:
```

Determine the property prefix from the target repository and neighboring habits. The default property is `OWH_CONFIG`, but a setup with a nil or custom `org-window-habit-property-prefix` may use `CONFIG` or another prefix. Preserve the local convention.

Prefer the unified config for new habits. Continue to support legacy scattered properties such as `WINDOW_SPECS`, `WINDOW_DURATION`, and `REPETITIONS_REQUIRED`, but do not migrate them unless asked. When unified and scattered properties coexist, the unified config takes precedence.

## Configure the unified property

Use a Lisp plist as the property value and keep it on the property line: Org property values cannot span lines, and a value split across lines is ignored. Keep durations in plist form inside unified configs, such as `(:days 7)`, `(:hours 8)`, `(:months 1)`, or `(:weeks 1 :start :monday)`.

### Habit-level keys

- `:window-specs` — Required list of one or more complete window specs.
- `:assessment-interval` — Graph/streak assessment bucket and rolling step. Default: `(:days 1)`.
- `:reschedule-assessment-interval` — Search step for finding the next required reminder. Default: `(:days 1)` independently of `:assessment-interval`.
- `:reschedule-interval` — Minimum time after the latest completion before another reminder. Default: `(:days 1)`.
- `:reschedule-threshold` — Require a reminder when the aggregate ratio drops below this number. Default: `1.0`.
- `:max-reps-per-interval` — Maximum completions counted in each assessment bucket. Default: `1`; raise it when several completions in one bucket should count.
- `:aggregation-fn` — Function combining multiple window ratios. Default: `org-window-habit-default-aggregation-fn`, the minimum ratio.
- `:only-days` — Days on which completions count. Without `:reschedule-days`, also restrict reminders.
- `:reschedule-days` — Days on which reminders may appear without filtering completion counts.
- `:from` / `:until` — Active date bounds for starts, resets, historical versions, and pauses.
- `:weight` — Optional weight when aggregating scores across multiple habits with the meta-evaluation API; it does not weight specs inside one habit.

### Window-spec keys

- `:duration` — Required lookback duration.
- `:repetitions` — Required completion count. Default at the object level is `1`, but write it explicitly in Org data.
- `:value` — Numeric weight used by weighted-average aggregation. The default minimum aggregation does not use it to relax a failing spec.
- `:conforming-baseline` — Fraction of the target that produces ratio `1.0`. Default: `1.0`; for example, `0.8` makes four of five fully conforming.
- `:max-conforming-ratio` — Cap on extra credit. Default: `1.0`; for example, `1.2` allows a ratio up to 120%.
- `:find-window` — Advanced custom Emacs Lisp window function. Preserve it when present; do not introduce one for ordinary data edits.

Use the default aggregation unless the user explicitly wants averaging. When using weighted averaging, pair it with numeric spec values:

```org
:OWH_CONFIG: (:window-specs ((:duration (:days 2) :repetitions 1 :value 1.0) (:duration (:days 8) :repetitions 4 :value 2.0)) :aggregation-fn org-window-habit-weighted-average-aggregation-fn)
```

## Reason about counting and boundaries

Remember these non-obvious rules:

- Cap completions separately in each assessment bucket using `:max-reps-per-interval`. With daily assessment and the default cap, two completions on the same day count as one.
- Anchor fixed-length assessment intervals to the habit start, derived from `:from`/reset time or the earliest completion. Do not assume every multi-day interval begins on a calendar boundary.
- Align `(:weeks N :start :DAY)` and calendar-month durations to their calendar boundaries.
- Filter non-allowed completion days before calculating conformity when `:only-days` is present.
- Scale the target during the initial partial window after a start/reset instead of demanding the full target immediately.

## Version requirements over time

Use a bare plist for one configuration. Use a list of plists for versioned configurations, newest first. Treat every version as a complete configuration rather than a patch: repeat `:window-specs` and every setting that should remain effective.

Treat each version as the half-open interval `[from, until)` in local time:

- `:from` is inclusive at midnight.
- `:until` is exclusive at midnight.
- Missing `:from` means unbounded past.
- Missing `:until` means unbounded future.
- A lone `:from` on a single config starts/resets the habit and ignores older completions; it does not describe a pause.

Accept `"YYYY-MM-DD"`, `"[YYYY-MM-DD Day]"`, or `"<YYYY-MM-DD Day>"`, and preserve the file's existing style. If the user says “through June 1,” use `:until "2026-06-02"` because the end bound is exclusive.

For a continuous rule change on June 1, use matching bounds:

```org
:OWH_CONFIG: ((:from "2026-06-01" :window-specs ((:duration (:days 7) :repetitions 5))) (:until "2026-06-01" :window-specs ((:duration (:days 7) :repetitions 3))))
```

The parser infers a newer version's missing `:from` from the adjacent older version's `:until`, but prefer explicit paired bounds when directly editing data. Avoid overlaps and keep versions newest first.

## Pause and resume

Pause an active single-config habit at date `P` by adding `:until P`. Preserve the configuration as history; no config is active at or after `P`:

```org
:OWH_CONFIG: (:window-specs ((:duration (:days 7) :repetitions 2)) :until "2026-05-21")
```

Pause an already-versioned habit by adding `:until P` to the newest currently active plist without discarding older versions.

Resume at date `R` by prepending a complete newest config with `:from R` and retaining `:until P` on the prior version:

```org
:OWH_CONFIG: ((:from "2026-08-09" :window-specs ((:duration (:days 7) :repetitions 2))) (:until "2026-05-21" :window-specs ((:duration (:days 7) :repetitions 2))))
```

Interpret the gap `[P, R)` as inactive. If requirements change at resume, change the new first plist and preserve the historical plist.

Do not substitute `STYLE: habit-paused`, removal of a repeater, deletion of history, or an arbitrary TODO-state change for a dated config pause. Those may affect agenda visibility under local conventions but do not encode a historical inactive interval.

## Edit safely

1. Locate the exact heading and read the complete entry: planning line, property drawer, logbook or inline state history, and nearby repository conventions.
2. Translate the user's goal into window duration, repetitions, assessment cadence, counting cap, reminder cadence, and any day restrictions before editing.
3. Preserve unrelated properties, IDs, timestamps, log entries, heading state, and formatting.
4. Re-read the complete config and check Lisp parentheses, quoting, and required `:window-specs`.
5. Check version order and ensure each version is complete. Require equal bounds for continuous changes and `until < from` for completed pauses.
6. Check midnight semantics and distinguish completion restrictions from reminder restrictions.
7. Review the diff and verify that no completion history or unrelated Org data changed.
8. For data-only edits, perform syntax and diff review. When changing package behavior, run the narrow relevant ERT tests and the repository checks.
