#!/usr/bin/env python3
"""Generate dummy data for the HCI2000 analysis pipeline.

Controller records (records/main/<participant>/<session_id>/...) are written by the
REAL controller code in ~/.hammerspoon (study/controller.lua + study/storage.lua),
driven through a simulated Hammerspoon: a fake clock, Chrome window, mouse, audio
player and accessibility title notifications. Simulated participants "click" tabs;
the controller detects title changes and records outcomes exactly as it would live.

The three Google Forms exports are simulated separately with matching codes/times.

Requirements: Python 3.10+, the `lupa` package (Lua 5.4), ffprobe (optional; for
prompt durations).  Example:
    python3 generate_dummy_data.py --hammerspoon ~/.hammerspoon
Seeded: identical output on every run. Refuses to overwrite an existing records/ folder.
"""
from __future__ import annotations
import argparse, csv, importlib, json, math, random, shutil, subprocess, sys, tempfile
from datetime import datetime, timedelta, timezone
from pathlib import Path

HERE = Path(__file__).resolve().parent
MANILA = timezone(timedelta(hours=8))
REAL_ROOT = "/Users/jphonrado/.hammerspoon"   # written into session.json paths

# ----------------------------------------------------------------------------
# Lua side: fake Hammerspoon + thin driver around the real controller
# ----------------------------------------------------------------------------
LUA_SIM = r'''
package.path = PACKAGE_ROOT .. "/?.lua;" .. PACKAGE_ROOT .. "/?/init.lua;" .. package.path
local Controller = require("study.controller")
local Config = require("study.config")
local Materials = require("study.materials")
local S = {}
local env, c, hs
local realTime, realDate = os.time, os.date

local function schedule(at, fn)
  local o = {at = at, callback = fn, stopped = false}
  function o:stop() self.stopped = true end
  env.timers[#env.timers + 1] = o
  return o
end
local function advance(delta)
  local target = env.time + delta
  for _ = 1, 1000000 do
    local nextTimer
    for _, item in ipairs(env.timers) do
      if not item.stopped and item.at <= target and (not nextTimer or item.at < nextTimer.at) then nextTimer = item end
    end
    if not nextTimer then env.time = target; break end
    env.time = nextTimer.at
    -- Real timers drift slightly; add 0-4 ms of scheduling jitter to repeating timers.
    if nextTimer.interval then nextTimer.at = nextTimer.at + nextTimer.interval + math.random() * 0.004
    else nextTimer.stopped = true end
    nextTimer.callback()
  end
  local live = {}
  for _, item in ipairs(env.timers) do if not item.stopped then live[#live + 1] = item end end
  env.timers = live
  if c and c.fatal then error("Controller stopped: " .. table.concat(env.notices, " | ")) end
end

local function dispatch(name)
  local types = hs.eventtap.event.types
  local ev = {}
  function ev:getType() return types[name] end
  function ev:getProperty(prop)
    if prop == 1 then return 0 end
    if prop == 2 then return 1 end
    if prop == 3 then return 4711 end
  end
  function ev:location() return {x = env.position.x, y = env.position.y} end
  function ev:timestamp() return math.floor(env.time * 1e9) end
  local blocked = false
  for _, tap in ipairs(env.taps) do
    if tap.enabled and tap.types[types[name]] then
      if tap.callback(ev) == true then blocked = true end
    end
  end
  return blocked
end

function S.init(opts)
  math.randomseed(opts.seed)
  env = {time = opts.mono0, timers = {}, title = "Study Start - Google Chrome", buttons = {},
    front = true, position = {x = opts.pointer_x, y = opts.pointer_y},
    frame = {x = 0, y = 33, w = 1470, h = 923}, taps = {}, notices = {}, audio = opts.audio}
  local wallOffset = opts.wall0 - opts.mono0
  os.time = function(t) if t then return realTime(t) end return math.floor(env.time + wallOffset) end
  os.date = function(fmt, t) return realDate(fmt, t or math.floor(env.time + wallOffset)) end
  hs = {timer = {}, eventtap = {event = {types = {}}}, canvas = {windowLevels = {overlay = 1}},
    settings = {}, mouse = {}, window = {}, sound = {}, alert = {}, dialog = {}, application = {}, axuielement = {}}
  hs.fs = {attributes = function(p)
      if p:find("/orlando_audio/", 1, true) then return {mode = "file"} end
      return BRIDGE.attributes(p) end,
    mkdir = BRIDGE.mkdir, rmdir = BRIDGE.rmdir}
  hs.json = {encode = BRIDGE.json_encode, decode = BRIDGE.json_decode}
  local saved = {
    [Config.settingsPrefix .. "pointer"] = {x = opts.pointer_x, y = opts.pointer_y},
    [Config.settingsPrefix .. "mask_H"] = {x = 0, y = 0, w = 1470, h = 78, frame_w = 1470, frame_h = 923, screen_id = "1"},
    [Config.settingsPrefix .. "mask_V"] = {x = 0, y = 0, w = 262, h = 923, frame_w = 1470, frame_h = 923, screen_id = "1"},
  }
  hs.settings.get = function(k) return saved[k] end
  hs.settings.set = function(k, v) saved[k] = v end
  hs.timer.absoluteTime = function() return env.time * 1e9 end
  hs.timer.secondsSinceEpoch = function() return env.time + wallOffset end
  local function timer(delay, cb, rep) local o = schedule(env.time + delay, cb); o.interval = rep and delay or nil; return o end
  hs.timer.doAfter = function(d, cb) return timer(d, cb, false) end
  hs.timer.doEvery = function(d, cb) return timer(d, cb, true) end
  hs.alert.show = function(m) env.notices[#env.notices + 1] = m end
  hs.dialog.blockAlert = function(_, _, first) return first end
  hs.dialog.textPrompt = function(_, prompt)
    if prompt:find("^Mode") then return "OK", opts.mode end
    if prompt:find("^Participant") then return "OK", opts.participant end
    if prompt:find("^Allocated group") then return "OK", tostring(opts.group) end
    return "Cancel", nil
  end
  hs.mouse.absolutePosition = function(v)
    if v then env.position = {x = v.x, y = v.y} end
    return {x = env.position.x, y = env.position.y}
  end
  for index, key in ipairs({"leftMouseDown","leftMouseUp","rightMouseDown","rightMouseUp","otherMouseDown",
      "otherMouseUp","mouseMoved","leftMouseDragged","rightMouseDragged","otherMouseDragged","scrollWheel"}) do
    hs.eventtap.event.types[key] = index
  end
  hs.eventtap.checkMouseButtons = function() return env.buttons end
  hs.eventtap.event.properties = {mouseEventButtonNumber = 1, mouseEventClickState = 2, mouseEventWindowUnderMousePointer = 3}
  hs.eventtap.new = function(events, cb)
    local tap = {callback = cb, enabled = false, types = {}}
    for _, t in ipairs(events) do tap.types[t] = true end
    function tap:start() self.enabled = true end
    function tap:stop() self.enabled = false end
    function tap:isEnabled() return self.enabled end
    env.taps[#env.taps + 1] = tap
    return tap
  end
  hs.eventtap.keyStroke = function() env.title = "Study Start - Google Chrome" end
  local app = {name = function() return "Google Chrome" end, isFrontmost = function() return env.front end,
    activate = function() env.front = true; return true end, pid = function() return 812 end}
  local win = {}
  function win:id() return 4711 end
  function win:application() return app end
  function win:isMinimized() return false end
  function win:frame() return {x = env.frame.x, y = env.frame.y, w = env.frame.w, h = env.frame.h} end
  function win:screen() return {id = function() return 1 end} end
  function win:title() return env.title end
  function win:focus() env.front = true end
  function win:focusTab() env.title = "Study Start - Google Chrome"; return true end
  hs.window.get = function(id) return id == 4711 and win or nil end
  hs.window.frontmostWindow = function() return env.front and win or nil end
  hs.application.get = function() return {mainWindow = function() return win end} end
  hs.canvas.new = function(frame)
    local canvas = {rect = frame}
    function canvas:appendElements() return self end
    function canvas:clickActivating() return self end
    function canvas:level() return self end
    function canvas:show() self.visible = true; return self end
    function canvas:bringToFront() return self end
    function canvas:delete() self.visible = false; return self end
    return canvas
  end
  hs.sound.getByFile = function(path)
    local name = path:match("([^/]+)$")
    local s = {}
    function s:setCallback(fn) self.cb = fn; return self end
    function s:play()
      local me = self
      me.timer = schedule(env.time + env.audio[name] + 0.01 + math.random() * 0.03,
        function() if me.cb then me.cb(true) end end)
      return true
    end
    function s:stop() if self.timer then self.timer:stop() end end
    return s
  end
  hs.axuielement.windowElement = function(w) return {window = w} end
  hs.axuielement.observer = {new = function()
    local o = {running = false}
    function o:callback(fn) self.fn = fn end
    function o:addWatcher() end
    function o:start() self.running = true; env.observer = self end
    function o:isRunning() return self.running end
    function o:stop() self.running = false end
    return o
  end}
  _G.print = function(...) env.notices[#env.notices + 1] = table.concat({...}, " ") end
  c = Controller.new(hs, opts.root)
end

-- Researcher actions (hotkeys) ------------------------------------------------
function S.newSession() c:newSession(); assert(c.model and c.store, "Session not created: " .. table.concat(env.notices, " | ")) end
function S.captureFrame() c:captureFrame() end
function S.approve() c:guard(function() c:approveSetup() end); assert(c.model.phase == "READY", "approve failed: " .. tostring(env.notices[#env.notices])) end
function S.start() c:guard(function() c:startTrial() end) end
function S.setup() c:guard(function() c:setup() end) end
function S.nextBlock() c:guard(function() c:nextBlock() end) end
function S.advance(dt) advance(dt) end

-- Scheduled participant / facilitator events (absolute monotonic seconds) ------
function S.click(at, title, x, y, holdDown)
  schedule(at, function()
    env.position = {x = x, y = y}
    env.buttons = {[1] = true, left = true}
    local blocked = dispatch("leftMouseDown")
    -- Chrome activates a tab on mouse-down; the window title follows shortly after.
    schedule(env.time + 0.018 + math.random() * 0.03, function()
      if blocked then return end
      env.title = title .. " - Google Chrome"
      local o = env.observer
      if o and o.running then
        schedule(env.time + 0.003 + math.random() * 0.012, function() if o.running and o.fn then o.fn() end end)
      end
    end)
    schedule(env.time + holdDown, function() env.buttons = {}; dispatch("leftMouseUp") end)
  end)
end
function S.focusLoss(at) schedule(at, function() env.front = false end) end
function S.restoreFocus() env.front = true end
function S.hotkeyPause(at, reason) schedule(at, function() c:guard(function() c:emergency(reason) end) end) end
function S.manualComplete(at) schedule(at, function() c:guard(function() c:finish("completed_manual") end) end) end

-- Waiting / state ---------------------------------------------------------------
function S.waitRunning(maxSeconds)
  local deadline = env.time + maxSeconds
  while env.time < deadline do
    local p = c.model.phase
    if p == "RUNNING" and c.revealTime then return c.revealTime end
    if p == "BLOCK_COMPLETE" or p == "PAUSED" or p == "SETUP" or p == "SESSION_COMPLETE" then return nil end
    advance(0.005)
  end
  error("Timed out waiting for RUNNING; phase=" .. tostring(c.model.phase))
end
function S.waitLeave(phase, maxSeconds)
  local deadline = env.time + maxSeconds
  while env.time < deadline and c.model.phase == phase do advance(0.01) end
  assert(c.model.phase ~= phase, "Still in " .. phase)
end
function S.waitSettled(maxSeconds)
  local deadline = env.time + maxSeconds
  while env.time < deadline and (c.model.phase == "OUTCOME") do advance(0.01) end
end
function S.phase() return c.model and c.model.phase end
function S.now() return env.time end
function S.trialIndex() return c.model.trialIndex end
function S.blockIndex() return c.model.blockIndex end
function S.sessionId() return c.meta.session_id end
function S.storePath() return c.store.path end
function S.notices() return table.concat(env.notices, "\n") end
function S.blockInfo()
  local b = c.model:currentBlock()
  local set = Materials.sets[b.contentSet]
  local titleByPath = {}
  for _, p in ipairs(set.pages) do titleByPath[p.path] = p.title end
  local order = {}
  for i, p in ipairs(b.taskOrder) do order[i] = titleByPath[p] end
  local trials = {}
  for i, t in ipairs(b.trials) do
    local pos
    for j, p in ipairs(b.taskOrder) do if p == t.path then pos = j end end
    trials[i] = {id = t.id, target_id = t.targetID, title = t.targetTitle, pos = pos}
  end
  return {name = b.name, set = b.contentSet, orientation = b.orientation, load = b.tabLoad,
    n = #b.taskOrder, order = order, trials = trials}
end
function S.allocation()
  local A = require("study.allocation")
  local out = {}
  for code, e in pairs(A) do out[code] = {group = e.group, practice = e.practiceFirst} end
  return out
end
function S.audioNames()
  local out = {}
  for _, set in pairs(Materials.sets) do for _, t in ipairs(set.trials) do out[t.audio] = true end end
  local list = {}
  for k in pairs(out) do list[#list + 1] = k end
  return list
end
return S
'''

# ----------------------------------------------------------------------------
# lupa bridge (same conversions as tools/run_checks.py)
# ----------------------------------------------------------------------------
def build_runtime(lua54, hammerspoon: Path):
    runtime = lua54.LuaRuntime(unpack_returned_tuples=True)

    def to_python(value):
        if lua54.lua_type(value) == "table":
            pairs = list(value.items())
            keys = [k for k, _ in pairs]
            if keys and all(isinstance(k, (int, float)) and k == int(k) for k in keys):
                numbered = sorted(int(k) for k in keys)
                if numbered == list(range(1, len(keys) + 1)):
                    return [to_python(value[i]) for i in numbered]
            return {str(k): to_python(v) for k, v in pairs}
        return value

    def to_lua(value):
        if isinstance(value, dict):
            return runtime.table_from({k: to_lua(v) for k, v in value.items()})
        if isinstance(value, list):
            return runtime.table_from([to_lua(v) for v in value])
        return value

    def attributes(p):
        path = Path(p)
        if not path.exists():
            return None
        return to_lua({"mode": "directory" if path.is_dir() else "file", "size": path.stat().st_size})

    def mkdir(p):
        try:
            Path(p).mkdir(); return True
        except OSError as e:
            return None, str(e)

    def rmdir(p):
        try:
            Path(p).rmdir(); return True
        except OSError as e:
            return None, str(e)

    def encode(value, pretty=False):
        return json.dumps(to_python(value), indent=2 if pretty else None, ensure_ascii=False, allow_nan=False)

    def decode(text):
        try:
            return to_lua(json.loads(text))
        except (ValueError, TypeError):
            return None

    g = runtime.globals()
    g.PACKAGE_ROOT = hammerspoon.as_posix()
    g.BRIDGE = runtime.table_from({"attributes": attributes, "mkdir": mkdir, "rmdir": rmdir,
                                   "json_encode": encode, "json_decode": decode})
    return runtime, to_python, to_lua

# ----------------------------------------------------------------------------
# Simulated participant behaviour ("true" effects; see README)
# ----------------------------------------------------------------------------
MEDIAN_TIME = {"H-L": 1.9, "V-L": 2.1, "H-H": 3.3, "V-H": 2.6}   # s to the correct click, error-free
P_WRONG_FIRST = {"H-L": 0.07, "V-L": 0.08, "H-H": 0.27, "V-H": 0.13}

def tab_xy(info, pos):
    """Screen point of task tab `pos` (Study Start is tab n+1, last)."""
    n = info["n"] + 1
    if info["orientation"] == "H":
        width = (1470 - 160) / n
        return round(80 + (pos - 0.5) * width, 1), 51.0
    return 130.0, round(33 + 96 + (pos - 0.5) * 30, 1)

def wrong_pos(rng, info, target_pos):
    n = info["n"]
    if rng.random() < 0.04:
        return n + 1                                  # back to Study Start
    r = rng.random()
    if r < 0.5:
        offs = [-1, 1]
    elif r < 0.8:
        offs = [-2, 2]
    else:
        offs = [p - target_pos for p in range(1, n + 1) if p != target_pos]
    cands = [target_pos + o for o in offs if 1 <= target_pos + o <= n]
    return rng.choice(cands) if cands else (1 if target_pos != 1 else 2)

def title_at(info, pos):
    return "Study Start" if pos == info["n"] + 1 else info["order"][pos - 1]

def plan_trial(rng, person, info, trial, block, repeat):
    """Return a list of (seconds after reveal, task position) clicks."""
    cond, pos = info["name"], trial["pos"]
    med = MEDIAN_TIME[cond] * person["speed"] * (0.96 ** (block - 1)) * (0.9 if repeat else 1.0)
    if info["load"] == "high":
        med *= 1 + (0.035 if info["orientation"] == "H" else 0.02) * (pos - 8)
    t_target = max(0.55, med * math.exp(rng.gauss(0, 0.28)))
    clicks = []
    if rng.random() < min(0.9, P_WRONG_FIRST[cond] * person["err"]):
        t_wrong = max(0.45, t_target * rng.uniform(0.5, 0.85))
        clicks.append((t_wrong, wrong_pos(rng, info, pos)))
        t = t_wrong + rng.uniform(0.6, 1.5)
        if rng.random() < 0.15:
            clicks.append((t, wrong_pos(rng, info, pos)))
            t += rng.uniform(0.6, 1.3)
        t_target = t
    clicks.append((t_target, pos))
    return clicks

# Deliberate edge cases, keyed by (participant, condition, trial).
EDGE = {
    ("P005", "V-H", "T3"): "technical_interruption",   # window loses focus mid-trial
    ("P009", "H-L", "T5"): "human_pause",              # participant break hotkey
    ("P012", "H-H", "T2"): "timeout_no_selection",     # never clicks; 20 s deadline
    ("P002", "H-H", "T4"): "timeout_wrong_clicks",     # three wrong tabs, then 20 s deadline
    ("P014", "V-L", "T4"): "manual_completion",        # target tab shows a stale title; facilitator presses "."
}

# ----------------------------------------------------------------------------
def run_session(lua54, hammerspoon, root, pid, group, wall0, mono0, audio, seed):
    rng = random.Random(seed)
    runtime, to_python, to_lua = build_runtime(lua54, hammerspoon)
    S = runtime.execute(LUA_SIM)
    S.init(to_lua({"seed": seed, "mono0": mono0, "wall0": wall0, "pointer_x": 735, "pointer_y": 520,
                   "audio": audio, "mode": "main", "participant": pid, "group": group, "root": str(root)}))
    person = {"speed": math.exp(rng.gauss(0, 0.15)), "err": math.exp(rng.gauss(0, 0.3))}
    S.newSession()
    S.captureFrame()
    S.advance(rng.uniform(420, 600))            # consent check, familiarisation, TLX instructions
    blocks, tlx_submit = [], {}
    for b in range(1, 5):
        if b > 1:
            S.nextBlock()                        # after the block survey was submitted
            S.advance(rng.uniform(45, 100))      # facilitator prepares tabs (participant not watching)
        info = to_python(S.blockInfo())
        S.approve(); S.advance(rng.uniform(1.5, 4.0)); S.start()
        seen = set()
        while True:
            reveal = S.waitRunning(120)
            if reveal is None:
                phase = S.phase()
                if phase == "BLOCK_COMPLETE":
                    break
                if phase == "PAUSED":            # resume after an interruption: Setup -> approve -> Return
                    S.advance(rng.uniform(25, 60)); S.restoreFocus(); S.setup()
                    S.advance(rng.uniform(10, 25)); S.approve(); S.advance(rng.uniform(1.5, 3)); S.start()
                    continue
                raise RuntimeError(f"{pid}: unexpected phase {phase}\n{S.notices()}")
            trial = info["trials"][int(S.trialIndex()) - 1]
            repeat = trial["target_id"] in seen; seen.add(trial["target_id"])
            edge = EDGE.get((pid, info["name"], trial["id"]))
            hold = lambda: rng.uniform(0.07, 0.14)
            if edge is None:
                for dt, pos in plan_trial(rng, person, info, trial, b, repeat):
                    S.click(reveal + dt, title_at(info, pos), *tab_xy(info, pos), hold())
            elif edge == "technical_interruption":
                p = wrong_pos(rng, info, trial["pos"])
                S.click(reveal + 1.4, title_at(info, p), *tab_xy(info, p), hold())
                S.focusLoss(reveal + 2.3)
            elif edge == "human_pause":
                S.hotkeyPause(reveal + 0.9, "Participant break / facilitator pause")
            elif edge == "timeout_no_selection":
                pass
            elif edge == "timeout_wrong_clicks":
                for dt in (2.6, 6.1, 11.4):
                    p = wrong_pos(rng, info, trial["pos"])
                    S.click(reveal + dt, title_at(info, p), *tab_xy(info, p), hold())
            elif edge == "manual_completion":
                S.click(reveal + 1.8, trial["title"].replace(" and ", " & "), *tab_xy(info, trial["pos"]), hold())
                S.manualComplete(reveal + 3.2)
            S.waitLeave("RUNNING", 25)
        end_wall = S.now() + (wall0 - mono0)
        tlx = rng.uniform(150, 260)              # time to complete the block survey
        tlx_submit[info["name"]] = end_wall + tlx * rng.uniform(0.8, 0.95)
        blocks.append(info["name"])
        S.advance(tlx)
    S.nextBlock()                                # closes the session (status completed)
    assert S.phase() == "SESSION_COMPLETE", S.notices()
    return {"pid": pid, "group": group, "session_id": S.sessionId(), "blocks": blocks,
            "tlx_submit": tlx_submit, "close_wall": S.now() + (wall0 - mono0)}

# ----------------------------------------------------------------------------
# Google Forms exports
# ----------------------------------------------------------------------------
def fmt_ts(epoch):
    d = datetime.fromtimestamp(epoch, MANILA)
    return f"{d.month}/{d.day}/{d.year} {d.hour}:{d.minute:02d}:{d.second:02d}"

COND_LABEL = {"H-L": "H-L: Horizontal Low", "H-H": "H-H: Horizontal High",
              "V-L": "V-L: Vertical Low", "V-H": "V-H: Vertical High"}
TLX_BASE = {  # mental, physical, temporal, performance, effort, frustration (0-10)
    "H-L": [3.0, 1.0, 2.5, 2.0, 3.0, 1.5], "V-L": [3.2, 1.2, 2.5, 2.2, 3.2, 1.8],
    "H-H": [6.5, 1.8, 5.0, 4.5, 6.0, 5.0], "V-H": [5.0, 1.5, 4.0, 3.2, 4.8, 3.2]}
DIMS = ["Mental Demand", "Physical Demand", "Temporal Demand", "Performance", "Effort", "Frustration"]
DIM_IMPORTANCE = [2.0, -1.0, 1.0, 0.8, 1.2, 0.6]

def write_forms(out: Path, sessions, rng):
    # --- screening ---
    cols = ["Timestamp", "Do you consent?", "Last name, First name (e.g. Dela Cruz, Juan)", "Age",
            "Do you regularly use a desktop or laptop browser when browsing the web ",
            "How often do you use a desktop or laptop computer to browse the web?  ",
            "Do you use Google Chrome as your main web browser?",
            "How often do you have multiple tabs open at the same time while browsing the web?",
            "Do you usually keep your open tabs in a single browser window, or spread them across several windows?  ",
            "Which tab orientation do you typically use? ",
            "Participant Code (Filled by Researcher)"]
    last = ["Santos", "Reyes", "Cruz", "Bautista", "Garcia", "Mendoza", "Torres", "Villanueva", "Ramos",
            "Aquino", "Navarro", "Castillo", "Flores", "Morales", "Domingo", "Salazar", "Ocampo", "Tan"]
    first = ["Andrea", "Miguel", "Bea", "Carlo", "Danica", "Enzo", "Faith", "Gabriel", "Hannah", "Iñigo",
             "Jasmine", "Kyle", "Lia", "Marco", "Nicole", "Paolo", "Rica", "Sam"]
    earliest = min(s["close_wall"] for s in sessions)
    rows = []
    for i in range(18):
        eligible = i < 16
        ts = (sessions[i]["close_wall"] if eligible else earliest) - rng.uniform(2, 5) * 86400
        uses = "Yes" if eligible or i == 17 else "No"
        rows.append((ts, [fmt_ts(ts), "I agree" if eligible or i == 16 else "I do not agree",
                          f"{last[i]}, {first[i]}", rng.randint(19, 24), uses,
                          rng.choice(["Daily", "Daily", "Daily", "A few times a week"]) if uses == "Yes" else "Rarely",
                          rng.choice(["Yes", "Yes", "No"]),
                          rng.choice(["Almost always", "Often", "Sometimes"]),
                          rng.choice(["Usually in a single window", "Usually in a single window",
                                      "Usually across several windows"]),
                          rng.choice(["Horizontal (tabs along the top)"] * 3 + ["Vertical (tabs along the side)"]),
                          sessions[i]["pid"] if eligible else ""]))
    rows.sort(key=lambda r: r[0])
    with open(out / "screening_survey.csv", "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f); w.writerow(cols); w.writerows(r for _, r in rows)

    # --- NASA-TLX block survey (0-10) ---
    cols = ["Timestamp", "Score", "Participant Code (Filled by researcher)", "What condition block did you just finish?",
            "Mental Demand: How much mental and perceptual activity was required (e.g., thinking, deciding, calculating, remembering, looking, searching, etc.)? Was the task easy or demanding, simple or complex, exacting or forgiving?",
            "Physical Demand: How much physical activity was required (e.g., pushing, pulling, turning, controlling, activating, etc.)? Was the task easy or demanding, slow or brisk, slack or strenuous, restful or laborious?",
            "Temporal Demand: How much time pressure did you feel due to the rate or pace at which the tasks or task elements occurred? Was the pace slow and leisurely or rapid and frantic?",
            "Performance: How successful do you think you were in accomplishing the goals of the task set by the experimenter (or yourself)? How satisfied were you with your performance in accomplishing these goals?",
            "Effort: How hard did you have to work (mentally and physically) to accomplish your level of performance?",
            "Frustration Level: How insecure, discouraged, irritated, stressed, and annoyed versus secure, gratified, content, relaxed, and complacent did you feel during the task?",
            "Optional: Any thoughts you would like to share in related to this condition?"]
    comments = {"H-H": ["Hard to read the tab titles when they got squished", "Too many tabs, had to hover a lot"],
                "V-H": ["Easier to scan the list top to bottom", "Vertical list helped with many tabs"],
                "H-L": ["Easy"], "V-L": ["Fine, a bit unfamiliar"]}
    rows = []
    for s in sessions:
        offset = rng.gauss(0, 0.8)
        for cond in s["blocks"]:
            ratings = [max(0, min(10, round(m + offset + rng.gauss(0, 1.0)))) for m in TLX_BASE[cond]]
            note = rng.choice(comments[cond]) if rng.random() < 0.3 else ""
            rows.append((s["tlx_submit"][cond], [fmt_ts(s["tlx_submit"][cond]), "", s["pid"], COND_LABEL[cond], *ratings, note]))
    rows.sort(key=lambda r: r[0])
    with open(out / "block_survey.csv", "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f); w.writerow(cols); w.writerows(r for _, r in rows)

    # --- NASA-TLX weighting form (15 pairs) ---
    pairs = [("Mental Demand vs. Physical Demand", 0, 1), ("Mental Demand vs. Temporal Demand", 0, 2),
             ("Mental Demand vs. Performance ", 0, 3), ("Mental Demand vs. Effort", 0, 4),
             ("Mental Demand vs. Frustration", 0, 5), ("Physical Demand vs. Temporal Demand", 1, 2),
             ("Physical Demand vs Performance", 1, 3), ("Physical Demand vs. Effort", 1, 4),
             ("Physical Demand vs. Frustration", 1, 5), ("Temporal Demand vs. Performance", 2, 3),
             ("Temporal Demand vs. Effort", 2, 4), ("Temporal Demand vs. Frustration", 2, 5),
             ("Performance vs Frustration", 3, 5), ("Performance vs. Effort", 3, 4), ("Frustration vs. Effort", 5, 4)]
    cols = ["Timestamp", "Score", "Participant Code (Filled by Researcher)"] + [h for h, _, _ in pairs]
    rows = []
    for s in sessions:
        strength = [d + rng.gauss(0, 0.7) for d in DIM_IMPORTANCE]
        picks = [DIMS[a] if rng.random() < 1 / (1 + math.exp(-(strength[a] - strength[b]))) else DIMS[b]
                 for _, a, b in pairs]
        ts = s["close_wall"] + rng.uniform(90, 240)
        rows.append([fmt_ts(ts), "", s["pid"]] + picks)
    with open(out / "weighting_form.csv", "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f); w.writerow(cols); w.writerows(rows)

# ----------------------------------------------------------------------------
def audio_durations(hammerspoon: Path, names):
    out = {}
    for name in names:
        path = hammerspoon / "orlando_audio" / name
        try:
            res = subprocess.run(["ffprobe", "-v", "error", "-show_entries", "format=duration",
                                  "-of", "default=nw=1:nk=1", str(path)], capture_output=True, text=True, check=True)
            out[name] = float(res.stdout.strip())
        except (OSError, subprocess.CalledProcessError, ValueError):
            out[name] = 3.5
    return out

def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--hammerspoon", type=Path, default=Path.home() / ".hammerspoon")
    ap.add_argument("--lua-python-path", type=Path, help="Folder containing lupa (default: <hammerspoon>/.dev-cache/python)")
    ap.add_argument("--out", type=Path, default=HERE)
    ap.add_argument("--seed", type=int, default=2026)
    args = ap.parse_args()
    hsdir = args.hammerspoon.resolve()
    lp = args.lua_python_path or hsdir / ".dev-cache" / "python"
    if lp.exists():
        sys.path.insert(0, str(lp))
    lua54 = importlib.import_module("lupa.lua54")
    if (args.out / "records").exists():
        sys.exit(f"{args.out/'records'} already exists; move it away before regenerating.")

    rng = random.Random(args.seed)
    runtime, to_python, _ = build_runtime(lua54, hsdir)
    S = runtime.execute(LUA_SIM)
    alloc = to_python(S.allocation())
    audio = audio_durations(hsdir, to_python(S.audioNames()))

    pids = sorted(alloc)
    day0 = datetime(2026, 11, 16, tzinfo=MANILA)
    slots = [(9, 0), (10, 40), (13, 30), (15, 10)]
    sessions = []
    with tempfile.TemporaryDirectory(prefix="hci2000-dummy-") as tmp:
        root = Path(tmp) / "hammerspoon"
        (root / "records").mkdir(parents=True)
        for i, pid in enumerate(pids):
            h, m = slots[i % 4]
            start = day0 + timedelta(days=i // 4, hours=h, minutes=m + rng.randint(0, 12), seconds=rng.randint(0, 59))
            mono0 = rng.uniform(30_000, 600_000)          # seconds since the laptop booted
            s = run_session(lua54, hsdir, root, pid, alloc[pid]["group"], start.timestamp(), mono0, audio,
                            args.seed * 100 + i)
            sessions.append(s)
            print(f"{pid}: session {s['session_id']} completed")
        # Make recorded paths look like the real Mac location.
        for path in (root / "records").rglob("*.json"):
            path.write_text(path.read_text(encoding="utf-8").replace(str(root), REAL_ROOT), encoding="utf-8")
        shutil.copytree(root / "records", args.out / "records")
    write_forms(args.out, sessions, rng)
    print("Wrote records/, screening_survey.csv, block_survey.csv, weighting_form.csv")

if __name__ == "__main__":
    main()
