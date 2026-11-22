---
date: 2026-11-23
title: Sungrow in Lean, part 4
url: /2026/11/23/sungrow-lean-part4
---

In [part 3](https://carlo-hamalainen.net/2026/10/25/sungrow-lean-part3/) we
implemented chunked (block) reading of registers and also writing control
registers. The next step is to implement our own charging algorithms and manual
overrides so that we can control the inverter and battery in real time.

In the Go implementation, charging algorithms and state (the user's
preferences) are persisted to an SQLite database in JSON format. I've followed a Clojure-style
philosophy of "just use maps". The first charging algorithm was
a simple fixed rate in a time window and later I added an adaptive algorithm that
keeps the total grid draw under a specified limit.

For the Lean port I've taken the opportunity to fix a few quirks of the Go
JSON format: from now on, a key is present only when it means something. No
more `null`s or `"enabled": false` to switch off other fields. This was my own
messy implementation, mapping the internal register structure to JSON, while
battling Go's lack of sum types. It's always tempting to bash out some code quickly instead of sitting
quietly with pen and paper and designing a robust and extensible schema.

The Lean daemon still reads everything the Go daemon wrote, so the existing
schedule and database rows carry over, but it only writes the new format.

## Fixed charging

The user wants charging at 800 W from midnight to 6am, with a maximum SOC (state of charge) of 40%:

```json
{"mode": "fixed", "start": "00:00", "end": "06:00",
 "charge_watts": 800, "max_soc": 40}
```

I rarely use this mode as overnight adaptive charging is much more suited to my needs.

## Adaptive charging

The user wants charging from 1am to 6am, keeping the total grid draw under 4320 W, and a maximum SOC of 80%.

```json
{"mode": "adaptive", "start": "01:00", "end": "06:00",
 "ceiling_watts": 4320, "max_soc": 80}
```

Each mode has exactly one power field of its own: `charge_watts` for fixed, `ceiling_watts`
for adaptive. Giving an adaptive window a `charge_watts` is an error rather than
something silently ignored.

## Persistent state

The user has an adaptive charge window set, but they also have a manual
override forcing the battery to charge at 3 kW until 6am Brisbane time:

```json
{
  "charge_window": {"mode": "adaptive", "start": "01:00", "end": "06:00",
                    "ceiling_watts": 4320, "max_soc": 80},
  "manual_override": {"action": "charge", "watts": 3000,
                      "expiry": "2026-11-23T20:00:00Z"}
}
```

(The expiry is written in UTC; 20:00 UTC is 6am in Brisbane.)

Alternatively, there is no charge window, and a manual override puts the inverter
in self-consumption mode, where it uses solar, then battery, then grid to satisfy
the house load. With no `expiry`, the override lasts until it is cleared:

```json
{"manual_override": {"action": "self_consumption"}}
```

With no override, the key is simply absent:

```json
{"charge_window": {"mode": "fixed", "start": "00:00", "end": "06:00",
                   "charge_watts": 800, "max_soc": 40}}
```

and with neither, the intent is just `{}`, the empty "map".

An override's `action` is one of `self_consumption`, `stop`, `charge` or
`discharge`, and only the last two take `watts`.

## The Go format

For comparison, here is the first example as the Go daemon stores it. The override
is the three raw control registers, with labels for humans:

```json
{
  "charge_window": {"enabled": true, "mode": "adaptive", "start": "01:00", "end": "06:00",
                    "charge_watts": 0, "ceiling_watts": 4320, "max_soc": 80},
  "manual_override": {
    "registers": {"ems_mode": 2, "ems_mode_label": "forced",
                  "ems_command": 170, "ems_command_label": "charge",
                  "forced_power_w": 3000},
    "expiry": "2026-11-24T06:00:00+10:00"
  }
}
```

The zero field `"charge_watts": 0` is a "code smell".

For reference, the register values are:

- `ems_mode`: 0 is self_consumption, 2 is forced.
- `ems_command`: 170 (0xAA) is charge, 187 (0xBB) is discharge, 204 (0xCC) is stop.

Telling the two formats apart is easy: a Go schedule always has `enabled` and a Go
override always has `registers`, and neither key exists in the new format.

## Time of day

The [Datastar](https://data-star.dev) UI in the Go daemon uses HTML `time` input fields so that a user can enter `01:23` for 1:23am:

```html
<input type="time" data-bind:schedule-start>
<input type="time" data-bind:schedule-end>
```

In the Go code we store these as a simple struct, with the invariants in comments:

```go
// TimeOfDay is a wall-clock minute-of-day (no date, no zone), as a user
// enters it: "01:00", "05:55". Range 00:00–23:59.
type TimeOfDay struct {
	Hour   uint8 // 0–23
	Minute uint8 // 0–59
}
```

Even a simple type like this is nicer in Lean, where we can use natural numbers
and enforce the valid ranges for hours and minutes using dependent types:

```lean
structure TimeOfDay where
  hour      : Nat
  minute    : Nat
  hour_lt   : hour < 24
  minute_lt : minute < 60
  deriving Repr, DecidableEq
```

For arithmetic, we convert a `TimeOfDay` to its minute of the day, and prove that range too:

```lean
def TimeOfDay.minuteOfDay (t : TimeOfDay) : Nat := 60 * t.hour + t.minute

theorem TimeOfDay.minuteOfDay_lt (t : TimeOfDay) : t.minuteOfDay < 1440 := by
  have h_lt := t.hour_lt
  have m_lt := t.minute_lt
  unfold minuteOfDay
  omega
```

Perhaps this isn't rocket science compared to the [other things](https://math.mit.edu/~drew/grh/) people
are doing in Lean, but it makes me happy to have types that encode my real intention and the precisely valid
states that can be represented. Even in Haskell one would have to resort to smart constructors and so on.

## Charge mode and charge window

I like Go, but its lack of sum types is frustrating at times. I really
want to say that a charge mode is fixed or adaptive, nothing else!

```go
// ChargeMode selects how the charge rate is decided during the window.
type ChargeMode string

const (
	// ModeFixed charges at the window's ChargeWatts.
	ModeFixed ChargeMode = "fixed"
	// ModeAdaptive recomputes the charge rate every tick so that total grid
	// import stays at (or under) the window's CeilingWatts.
	ModeAdaptive ChargeMode = "adaptive"
)
```

With Lean's dependent types we can even add some nice touches like encoding a
minimum charge rate of 10 W (the inverter snaps power to 10 W increments) and a
minimum adaptive ceiling of 100 W.

```lean
inductive ChargeMode
  /-- Charge at a constant rate. The inverter snaps to 10 W increments, so let's avoid anything under 10. -/
  | fixed    (power : ForcedPower) (h : 10 ≤ power.watts)
  /-- Recompute the rate every tick so that total grid import stays at `ceilingW`. -/
  | adaptive (ceilingW : Nat) (h : 100 ≤ ceilingW)
  deriving Repr, DecidableEq
```

A charge window can also enforce sensible things like the start time being
strictly before the end time, and the SOC target being at most 100%.

```lean
/-- Charge between `start` and `stop` (half-open, same day) until SOC reaches `maxSOC`. -/
structure ChargeWindow where
  start    : TimeOfDay
  stop     : TimeOfDay
  mode     : ChargeMode
  maxSOC   : Nat
  start_lt : start.minuteOfDay < stop.minuteOfDay
  maxSOC_le : maxSOC ≤ 100
  deriving Repr, DecidableEq
```

## Actions instead of registers

The inverter is controlled by three registers, and Go models them directly:

```go
type ControlRegisters struct {
	EMSMode     uint16
	EMSCommand  uint16
	ForcedPower uint16
}
```

Bare `uint16` values are entirely unconstrained. The Lean version from
[part 3](https://carlo-hamalainen.net/2026/10/25/sungrow-lean-part3/)
is already tighter, since each field only holds values the inverter
understands:

```lean
structure ControlRegisters where
  mode    : EMSMode
  command : Command
  power   : ForcedPower
```

But it can still encode intent that doesn't make sense, like "self-consumption,
charge at 3000 W". The inverter ignores the command and power outside forced
mode, and the power for a stop, so most combinations of the three fields mean
the same thing. There are really only four things we can ask the inverter to
do, so that's the type for an override:

```lean
inductive Action
  | selfConsumption
  | stop
  | charge (power : ForcedPower)
  | discharge (power : ForcedPower)
  deriving DecidableEq, Repr
```

A power appears only where the inverter uses one. This is also why the JSON has
`watts` only for `charge` and `discharge`.

The inverter still wants registers, so there is a function from actions to registers:

```lean
def Action.registers : Action → ControlRegisters
  | .selfConsumption => ⟨.selfConsumption, .stop,     ⟨0, rfl, by decide⟩⟩
  | .stop            => ⟨.forced,          .stop,     ⟨0, rfl, by decide⟩⟩
  | .charge p        => ⟨.forced,          .charge,    p⟩
  | .discharge p     => ⟨.forced,          .discharge, p⟩
```

And in the other direction, we convert control registers to an action:

```lean
def Action.ofRegisters (cr : ControlRegisters) : Action :=
  match cr.mode, cr.command with
  | .selfConsumption, _ => .selfConsumption
  | .forced, .stop      => .stop
  | .forced, .charge    => .charge cr.power
  | .forced, .discharge => .discharge cr.power
```

Going from an action to registers and back should give the same action. In Go I'd
write a table test with a few cases. In Lean we can prove it for every action:

```lean
theorem Action.ofRegisters_registers (a : Action) :
    Action.ofRegisters a.registers = a := by
  cases a <;> rfl
```

The tactic `cases a` splits the goal into one goal for each of the four constructors of `Action`,
then `<;>` runs the next tactic on every goal that produced, and `rfl` closes each one by
computation. For example, in the `charge p` case the goal is

```
Action.ofRegisters (Action.registers (.charge p)) = .charge p
```

`Action.registers (.charge p)` evaluates to `⟨.forced, .charge, p⟩`. Then `ofRegisters`
matches on `.forced, .charge` and returns `.charge cr.power`, which is `.charge p`. Both
sides are now identical, which is all `rfl` needs.

Note that `p` is a variable, so this covers every power at once, not just the
ones a test would pick!

The other direction is deliberately *not* true. "Self-consumption, charge at 3000 W"
becomes `selfConsumption`, which goes back to "self-consumption, stop, 0 W". Those are
different registers, but the inverter does the same thing with both, and an `Action`
can't tell them apart because it doesn't hold what the inverter ignores. A Go override
with leftover values is read as the action it actually performs.

## Ticking

The controller is one pure function, called once per tick (every 5 seconds in the
Go daemon):

```lean
def tick (intent : Intent) (snap : Snapshot) (nowUnix : Int) (minute : Nat)
    (st : ControllerState) : Intent × Decision × ControllerState :=
  let intent := intent.expireOverride nowUnix
  let (d, st) := decide intent snap minute st
  (intent, d, st)
```

Its inputs:

- the user's `Intent`: the charge window and the manual override;
- a `Snapshot` of the sensors: battery SOC, grid power and battery power, each an
  `Option`, because a sensor read can fail or return the inverter's NaN sentinel;
- the time, as Unix seconds (for override expiry) and as minute of the day in
  Brisbane (for the charge window);
- the `ControllerState`, which is what the controller remembers between ticks.

It returns the intent (with an expired override cleared, for the caller to persist),
a `Decision`, and the new state. A decision either says which registers the inverter
should hold, with a reason for the log, or says to leave the inverter alone because
there isn't enough information:

```lean
inductive Decision
  | apply (registers : ControlRegisters) (reason : Reason)
  | leave (why : String)
```

`decide` is a priority list, and reads almost like the spec:

```lean
def decide (intent : Intent) (snap : Snapshot) (minute : Nat) (st : ControllerState) :
    Decision × ControllerState :=
  -- A change of intent resets the latch and soft-starts the adaptive controller.
  let st := if st.lastIntent == some intent then st
    else { targetReached := false, setpointW := 0, cmdW := 0, lastIntent := some intent }
  match intent.override, intent.window with
  | some o, _ => (.apply o.action.registers .manualOverride, st.resetAdaptive)
  | none, none => (.apply .selfConsumption .scheduleDisabled, st.resetAdaptive)
  | none, some w =>
    match snap.socTenths with
    | none => (.leave "no fresh battery SOC; cannot evaluate the schedule", st)
    | some soc =>
      if !w.contains minute then
        (.apply .selfConsumption .outsideWindow, { st.resetAdaptive with targetReached := false })
      else if st.targetReached || soc ≥ 10 * w.maxSOC then
        (.apply forcedStop .targetReached, { st.resetAdaptive with targetReached := true })
      else
        match w.mode with
        | .fixed p _ => (.apply ⟨.forced, .charge, p⟩ .scheduledCharge, st.resetAdaptive)
        | .adaptive ceilingW _ =>
          match snap.gridW, snap.batteryW with
          | some gridW, some battW => adaptiveTick st ceilingW gridW battW
          | _, _ => (.leave "no fresh grid/battery power; cannot run the adaptive controller", st)
```

In order:

1. A manual override always wins.
2. With no charge window, hand control to the inverter (self-consumption).
3. Without a fresh SOC reading we can't evaluate the window, so leave the inverter as
   it is rather than guess.
4. Outside the window, self-consumption. This also releases the "target reached"
   latch, so tomorrow's window charges again.
5. Once SOC reaches the target, stop and latch. The latch matters: without it, SOC
   dipping from 80.0% to 79.9% would start charging again.
6. Otherwise charge: at the fixed rate, or with the adaptive controller.

The adaptive branch is its own function. It works out how much the house is drawing
from the grid, moves the setpoint toward whatever the ceiling leaves (dropping at once,
but climbing at most 500 W per tick), and snaps the result to a 100 W ladder with
hysteresis, so load jitter doesn't flip the command every tick:

```lean
def adaptiveTick (st : ControllerState) (ceilingW gridW battW : Int) : Decision × ControllerState :=
  let importW   := max 0 (-gridW)
  let chargingW := max 0 (-battW)
  let setpointW := adaptiveStep st.setpointW ceilingW importW chargingW maxForcedPowerW
  let cmdW      := hystQuantize setpointW st.cmdW
  -- The one place a new command is made, so the one place that needs a proof.
  have cmdOk : CmdOk cmdW := hystQuantize_ok _ _ (adaptiveStep_ok ..) st.cmdOk
  let st        := { st with setpointW, cmdW, cmdOk }
  if cmdW < minChargeW then (.apply forcedStop .adaptiveHold, st)
  else (.apply (chargeAt cmdW cmdOk) .adaptiveCharge, st)
```

The `have cmdOk` line is a proof that the new command is on the 100 W ladder and
within the inverter's rating. `ControllerState` carries that proof as a field, so a
state with a bad command can't be built, and `chargeAt` takes the proof instead of
clamping. Because `decide` is pure, it's also easy to state and prove properties
like "a manual override always wins" and "never charge once SOC has reached the
target", which are in `Policy.lean`.

Everything with side effects is in `sungrowd tick`, which runs one tick against the
real inverter:

1. load the intent file (and, with `--state`, the controller state from the previous
   run);
2. read the control registers and the sensors;
3. call `tick`;
4. if the decision is `apply` and the inverter doesn't already hold those registers,
   write them, but only with `--apply`. The write is read back to check that it
   landed.

## A sample run

These are all real runs against my inverter, on a sunny afternoon after I'd discharged
the battery a little. It's 3:35pm in Brisbane, the house is drawing about 800 W from the
grid, and the battery is at 84%.

First, the Go daemon's own `schedule.json`: a fixed 800 W charge from midnight to 6am.
It's the afternoon, so we're outside the window and the decision is self-consumption:

```
$ sungrowd tick schedule.json
now       2026-11-23T05:35:53Z (15:35 Brisbane)
intent    {"charge_window":{"charge_watts":800,"end":"06:00","max_soc":40,"mode":"fixed","start":"00:00"}}
soc       84.4 %
grid      -814 W (+export/-import)
battery   0 W (+discharge/-charge)
inverter  forced stop 3000 W
decision  self consumption (outside window): self_consumption stop 0 W
state     {"cmd_w":0,"last_intent":{...},"setpoint_w":0,"target_reached":false}
action    would write self_consumption stop 0 W (pass --apply to write)
```

The Go file is read without complaint, but the intent is printed in the new format.
Note the inverter's current state: "forced stop 3000 W", a stop with a leftover power,
which is exactly the kind of register value that `Action` can't represent.

Next, an adaptive window covering the whole day, with a 4320 W ceiling and an 80% target. The battery is
already at 84.4%, so the controller stops and latches:

```
$ jq . adaptive-all-day.json
{
  "charge_window": {
    "mode": "adaptive",
    "start": "00:00",
    "end": "23:59",
    "ceiling_watts": 4320,
    "max_soc": 80
  }
}

$ sungrowd tick adaptive-all-day.json
...
soc       84.4 %
decision  stop (target SOC reached): forced stop 0 W
state     {"cmd_w":0,"last_intent":{...},"setpoint_w":0,"target_reached":true}
action    would write forced stop 0 W (pass --apply to write)
```

There's a small quirk here: the inverter is already stopped, but `tick` compares raw
registers, sees 3000 W against 0 W, and would write. It's harmless (it tidies up the
leftover power), but comparing actions instead of registers would avoid it.

With the same window but a 95% target, there's room to charge, and the controller soft-starts at 500 W:

```
$ sungrowd tick adaptive-all-day-95.json
...
soc       84.4 %
grid      -814 W (+export/-import)
battery   0 W (+discharge/-charge)
inverter  forced stop 3000 W
decision  adaptive charge: forced charge 500 W
state     {"cmd_w":500,"last_intent":{...},"setpoint_w":500,"target_reached":false}
action    would write forced charge 500 W (pass --apply to write)
```

Now for real. With `--apply`, `tick` writes the registers and reads them back to
check that the write landed. With `--state`, the controller state carries over
from one run to the next, so three runs about half a minute apart behave like three
ticks of the daemon:

```
$ sungrowd tick adaptive-all-day-95.json --state state.json --apply
...
inverter  forced stop 3000 W
decision  adaptive charge: forced charge 500 W
state     {"cmd_w":500,"last_intent":{...},"setpoint_w":500,"target_reached":false}
{"recorded_at": "2026-11-23T05:37:29Z",
 "intended": {"forced_power_w": 500, "ems_mode_label": "forced", "ems_mode": 2,
              "ems_command_label": "charge", "ems_command": 170},
 "before":   {"forced_power_w": 3000, "ems_mode_label": "forced", "ems_mode": 2,
              "ems_command_label": "stop", "ems_command": 204},
 "after":    {"forced_power_w": 500, "ems_mode_label": "forced", "ems_mode": 2,
              "ems_command_label": "charge", "ems_command": 170}}
```

The `after` matches `intended`: the inverter is charging at 500 W. Thirty-five seconds
later:

```
$ sungrowd tick adaptive-all-day-95.json --state state.json --apply
...
soc       84.5 %
grid      -1294 W (+export/-import)
battery   -541 W (+discharge/-charge)
inverter  forced charge 500 W
decision  adaptive charge: forced charge 1000 W
```

The battery is charging at 541 W and grid import has gone up by about the same
amount. The rest of the house draws 1294 − 541 = 753 W from the grid, so the target is
4320 − 753 = 3567 W, but the setpoint climbs at most 500 W per tick: 1000 W. And again:

```
$ sungrowd tick adaptive-all-day-95.json --state state.json --apply
...
soc       84.6 %
grid      -1819 W (+export/-import)
battery   -1039 W (+discharge/-charge)
inverter  forced charge 1000 W
decision  adaptive charge: forced charge 1500 W
```

The house is steady at 1819 − 1039 = 780 W, and the charge steps up to 1500 W. Left to
itself, the controller would keep climbing until total grid import sat just under the
4320 W ceiling.

Finally, a manual override wins over the charge window, whatever the battery is doing:

```
$ sungrowd tick override-discharge.json
now       2026-11-23T05:38:51Z (15:38 Brisbane)
intent    {"charge_window":{...},"manual_override":{"action":"discharge","watts":2000}}
soc       84.7 %
grid      -2319 W (+export/-import)
battery   -1535 W (+discharge/-charge)
inverter  forced charge 1500 W
decision  manual override: forced discharge 2000 W
state     {"cmd_w":0,"last_intent":{...},"setpoint_w":0,"target_reached":false}
action    would write forced discharge 2000 W (pass --apply to write)
```

To finish, I handed control back to the inverter. This uses the `write` command from
[part 3](https://carlo-hamalainen.net/2026/10/25/sungrow-lean-part3/),
which goes through the same read, write and read-back as `tick --apply`:

```
$ sungrowd write self-consumption
{"recorded_at": "2026-11-23T05:39:22Z",
 "intended": {"forced_power_w": 0, "ems_mode_label": "self_consumption", "ems_mode": 0,
              "ems_command_label": "stop", "ems_command": 204},
 "before":   {"forced_power_w": 1500, "ems_mode_label": "forced", "ems_mode": 2,
              "ems_command_label": "charge", "ems_command": 170},
 "after":    {"forced_power_w": 0, "ems_mode_label": "self_consumption", "ems_mode": 0,
              "ems_command_label": "stop", "ems_command": 204}}
```

The `before` is the 1500 W charge the adaptive controller left running, and the `after`
confirms the inverter is back on its own EMS.

## Summary

The Lean port now runs the control policy as well as reading the inverter. A
test run against the real inverter shows that it writes the correct control
registers and the adaptive charging algorithm works as expected.

Next we will implement a real ticking loop and perhaps the Prometheus endpoints
for monitoring.

Full source code is at <https://github.com/carlohamalainen/sungrowd-lean>.

