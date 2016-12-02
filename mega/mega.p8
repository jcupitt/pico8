pico-8 cartridge // http://www.pico-8.com
version 8
__lua__
-- mega-roids
-- jcupitt

-- particle system

particles={}

-- don't use all the colours for particles, just the bright ones
particle_cmap = {[0] = 6, 7, 8, 9, 10, 11, 12, 14, 15}

function add_particle(x, y)
 local p = {}

 p.x = x
 p.y = y
 p.dx = 0
 p.dy = 0
 p.ddx = 0
 p.ddy = 0
 p.l = 30		-- life
 p.dc = 0		-- colour delta
 p.c = 0
 p.s = 0		-- size ... 0 is one pixel across

 add(particles, p)

 return p
end

-- m == magnitude
function explosion(x, y, m)
 if(#particles > 200) return

 for n = 1, m + m * 2 * rnd() do
  local p = add_particle(x, y)
  local a = rnd()
  local s = rnd() * (2 + 0.05 * m)

  p.dx = s * sin(a)
  p.dy = s * cos(a)
  p.l = 40 * rnd() + 5
  p.dc = 1
  p.c = #particle_cmap * rnd()
  p.s = rnd()
 end
end

function spiral_explosion(x, y)
 if(#particles > 200) return

 for n = 1, 100 do
  local p = add_particle(x, y)
  local a = n / 3.1
  local s = n / 15

  p.dx = s * sin(a)
  p.dy = s * cos(a)
  p.l = 5 * s + 5
  p.dc = 1
  p.c = #particle_cmap * rnd()
 end
end

function jet(s, a)
 s.jt = (s.jt + 1) % 3
 if(s.jt != 0) return

 local j = add_particle(s.x + 4, s.y + 4)

 a += (rnd() - 0.5) * 0.15
 j.dx = sin(a) + s.dx
 j.dy = cos(a) + s.dy
 j.l = 15 * rnd() + 15
 j.dc = 0.5
 j.c = 15 * rnd()
 j.s = 0
end

function update_particle(p)
 p.dx += p.ddx - p.dx / 10
 p.dy += p.ddy - p.dy / 10
 p.x += p.dx
 p.y += p.dy
 p.c = (p.c + p.dc) % #particle_cmap
 p.l -= 1

 if(p.l < 0) del(particles, p)
end

function draw_particle(p)
 local x = p.x - screen_x
 local y = p.y - screen_y

 rectfill(x, y, x + p.s, y + p.s, particle_cmap[flr(p.c)])
end

-- star field
stars = {}

function draw_star(s)
 local x = (s.x - screen_x / 2) % 128
 local y = (s.y - screen_y / 2) % 128

 rectfill(x, y, x, y, 1)
end

function add_stars()
 for i = 1, 20 do
  local s = {}

  s.x = flr(rnd() * 128)
  s.y = flr(rnd() * 128)

  add(stars, s)
 end
end

-- world building

function circle(cx, cy, r, v)
 local n = 0

 for y = cy - r, cy + r do
  for x = cx - r, cx + r do
   dx = cx - x
   dy = cy - y 
   d = sqrt(dx * dx + dy * dy)
   if d < r then
    if(mget(x, y) == 16) n += 1
    mset(x, y, v)
   end
  end
 end

 return n
end

function worm(x, y, d)
 local r = 1
 local l = 10 + 60 * rnd()

 for j = 1, l do
  circle(x, y, r, 0)

  d += (rnd() - 0.5) / 10

  x += cos(d)
  y += sin(d)

  r += rnd() - 0.5
  r = max(min(r, 5), 1.5)
 end

 x -= cos(d)
 y -= sin(d)
 mset(x, y, 13)
end

-- test sprite for "rockyness"
function rocky(s)
 return fget(s, 0)
end

-- test sprite for "monsterness"
function monster(s)
 return fget(s, 1)
end

-- test sprite for "growthiness"
function growth(s)
 return fget(s, 2)
end

--[[ bits are laid out as

      2
    4 x 1
      8

 so values are

 0     isolated block
 1     finger pointing left
 2     finger pointing down
 3     corner pointing down and left
 4     finger pointing right
 5     horizontal line
 6     corner pointing down and right
 7     bottom edge
 8     finger pointing up
 9     corner pointing up and left
 10    vertical line
 11    left edge
 12    corner pointing up and right
 13    top edge
 14    right edge
 15    centre block

]]

function get_bits(x, y)
 local b

 b = 0
 if(rocky(mget(x + 1, y))) b += 1
 if(rocky(mget(x, y - 1))) b += 2
 if(rocky(mget(x - 1, y))) b += 4
 if(rocky(mget(x, y + 1))) b += 8

 return b
end

-- {bits, {{prob, value}, {prob, value}, ...}},

object_probs = {
 [7]  = {{0.05, 49}, {0.05, 51}, {0.05, 53}, {0.05, 64}},
 [11] = {{0.05,  7}, {0.05,  8}, {0.05, 48}, {0.05, 64}},
 [13] = {{0.05,  5}, {0.05, 38}, {0.05, 33}, {0.05, 64}},
 [14] = {{0.05, 50}, {0.05, 52}, {0.05, 54}, {0.05, 64}}
}

edge_probs = {
 [0] = {{1, 30}},
       {{0.2, 6}, {1, 29}},
       {{1, 26}},
       {{0.2, 47}, {1, 21}},
       {{0.2, 27}, {1, 45}},
       {{1, 31}},
       {{0.4, 42}, {1, 20}},
       {{1, 25}},
       {{0.2, 44}, {1, 28}},
       {{0.2, 15}, {1, 18}},
       {{1, 32}},
       {{0.4, 43}, {1, 22}},
       {{1, 19}},
       {{1, 23}},
       {{0.2, 46}, {1, 24}},
       {{0.05, 14}, {0.05, 16}, {1, 17}}
}

function fix_tiles(tiles, x1, y1, x2, y2, decorate)
 local n = 0

 for y = y1, y2 do
  for x = x1, x2 do
   local v = mget(x, y)

   if rocky(v) then
    local b = get_bits(x, y)
    local p = tiles[b]

    if p then
     -- count number of diamonds we destroy
     if(v == 16) n += 1

     if decorate then
      for i = 1, #p do
       if rnd() < p[i][1] then
        v = p[i][2]
        break
       end
      end
     else
      v = p[#p][2]
     end

     mset(x, y, v)
    end
   end
  end
 end

 return n
end

-- vegetation templates

vegetation = {
 [42] = {{0.1, 1, 1, 60}, {0.1, 1, 1, 62}, {0.1, 1, 1, 61}},
 [59] = {{0.1, -1, 1, 59}, {0.2, -1, 1, 63}},
 [60] = {{0.1, 1, 1, 60}, {0.5, 1, 1, 62}, {0.4, 1, 1, 61}},
 [62] = {{0.2, -1, 1, 59}, {0.4, -1, 1, 63}},
 [63] = {{0.2, 1, 1, 60}, {0.4, 1, 1, 62}, {0.1, 1, 1, 61}}
}

function grow_veg()
 for y = 0, 63 do
  for x = 0, 127 do
   local v = mget(x, y)

   if growth(v) then
    local g = vegetation[v]

    if g then
     for i = 1, #g do
      local tx = x + g[i][2]
      local ty = y + g[i][3]

      if rnd() < g[i][1] and mget(tx, ty) == 0 then
       mset(tx, ty, g[i][4])
       break
      end
     end
    end
   end
  end
 end
end

function generate_world()
 for y = 0, 63 do
  for x = 0, 127 do
   local dx = 64 - x
   local dy = 64 - 2 * y

   if sqrt(dx * dx + dy * dy) < 63 then
    mset(x, y, 17)
   else
    mset(x, y, 0)
   end
  end
 end

 for i = 1, 20 do
  local a = rnd()
  local sx = 64 + 63 * cos(a)
  local sy = 32 + 32 * sin(a)

  worm(sx, sy, a + 0.5)
 end

 fix_tiles(object_probs, 0, 0, 127, 63, true)
 fix_tiles(edge_probs, 0, 0, 127, 63, true)
 for i = 1, 5 do grow_veg() end
end

-- start actors

actors = {}

-- the actor map 

-- have 32 across, so 1024 / 32 == 32 pixels 
-- square for each actor map cell

actor_map = {}
actor_map_extras = {}
actor_map_width = 32
actor_map_height = 16

function build_actor_map()
 for y = 1, actor_map_height do
  actor_map[y - 1] = {}
  for x = 1, actor_map_width do
   actor_map[y - 1][x - 1] = {}
  end
 end
end

function update_actor_map(a)
 local x = flr(actor_map_width * a.x / 1024)
 local y = flr(actor_map_height * a.y / 512)

 if x >= 0 and x < actor_map_width and
  y >= 0 and y < actor_map_height then
  m = actor_map[y][x]
 else
  m = actor_map_extras
 end

 if a.actor_map != m then
  del(a.actor_map, a)
  add(m, a)
  a.actor_map = m
 end
end

function nearby_actors(a, r)
 -- do the divide first or we'll overflow
 local x = flr(actor_map_width * (a.x / 1024))
 local y = flr(actor_map_height * (a.y / 512))

 local searched_extras
 local processed 

 searched_extras = false
 processed = {}

 for i = -r, r do
  for j = -r, r do
   local ix = x + i
   local iy = y + j
   local m

   if ix >= 0 and ix < actor_map_width and 
    iy >= 0 and iy < actor_map_height then
    m = actor_map[iy][ix]
   elseif not searched_extras then
    m = actor_map_extras
    searched_extras = true
   else
    m = nil
   end

   if m then 
    foreach(m, function (x)
     add(processed, x)
    end)
   end
  end
 end

 return processed
end

-- loop over nearby actors without making a table .. good for searches not 
-- good for nearby table updates
function foreach_nearby_actors(a, r, f)
 -- do the divide first or we'll overflow
 local x = flr(actor_map_width * (a.x / 1024))
 local y = flr(actor_map_height * (a.y / 512))

 local searched_extras

 searched_extras = false

 for i = -r, r do
  for j = -r, r do
   local ix = x + i
   local iy = y + j
   local m

   if ix >= 0 and ix < actor_map_width and 
    iy >= 0 and iy < actor_map_height then
    m = actor_map[iy][ix]
   elseif not searched_extras then
    m = actor_map_extras
    searched_extras = true
   else
    m = nil
   end

   if m then 
    foreach(m, f)
   end
  end
 end
end

function add_actor(x, y)
 local a = {}

 a.x = x
 a.y = y
 a.dx = 0
 a.dy = 0
 a.ddx = 0
 a.ddy = 0
 a.sp = 1	-- base sprite number
 a.f = 0	-- frame of animation
 a.r = 3	-- radius

 -- set false when removed from actor table and being removed from system
 a.alive = true	

 -- the actor map cell we are on
 a.actor_map = nil

 add(actors, a)
 update_actor_map(a)

 return a
end

function remove_actor(a)
 if a.alive then 
  if(a.remove) a:remove()
  if(a.actor_map) del(a.actor_map, a) a.actor_map = nil
  del(actors, a)
  a.alive = false
 end
end

function update_actor(a)
 if(a.alive) a:update()

 a.x += a.dx
 a.y += a.dy		

 -- we have to do this after changing x, since dx/dy are set by update_monster
 -- and update_ship to keep us out of walls and we mustn't change them
 a.dx += a.ddx
 a.dy += a.ddy
end

function closer(a, b, d)
 -- numbers in pico8 are 16.16 bit fixed point, so we can't square 
 -- anything bigger than sqrt(32767), about 170

 -- scale down by 1000 before squaring 
 -- this will give us enough range for this game
 local dx = (a.x - b.x) / 1000
 local dy = (a.y - b.y) / 1000

 -- we want (1000 * sqrt(dx * dx + dy * dy) < d)
 -- divide by 1000 and square both sides to remove the sqrt
 d /= 1000

 return dx * dx + dy * dy < d * d
end

function collision(a, b)
 return closer(a, b, a.r + b.r)
end

-- limit actor's max speed ... scale the unit vector
function max_speed(a, s)
 local l = sqrt(a.dx * a.dx + a.dy * a.dy)
 if l > s then
  a.dx = s * a.dx / l
  a.dy = s * a.dy / l
 end
end

function test_map(x, y)
 local v = mget(flr(x / 8), flr(y / 8))

 return rocky(v)
end

-- will moving actor a by dx/dy hit a wall
function hit_wall(a, dx, dy)
 local nx = a.x + dx + 4
 local ny = a.y + dy + 4

 return test_map(nx - a.r, ny - a.r) or
  test_map(nx + a.r, ny - a.r) or
  test_map(nx - a.r, ny + a.r) or
  test_map(nx + a.r, ny + a.r)
end

function update_bullet(b)
 b.l -= 1
 if b.l < 0 then
  remove_actor(b)
 end

 if hit_wall(b, b.dx, b.dy) then
  explosion(b.x + 4, b.y + 4, 5) 
  wake_monsters(b.x + 4, b.y + 4)
  remove_actor(b)
 end

 foreach_nearby_actors(b, 1, function(a)
  if a.monster and collision(b, a) then
   explosion(b.x + 4, b.y + 4, 10) 
   remove_actor(b)
   remove_actor(a)
  end
 end)
end

function add_bullet(x, y, a)
 local b = add_actor(x, y)

 b.dx = cos(a)
 b.dy = sin(a)
 b.x += b.dx
 b.y += b.dy
 b.sp = 11
 b.l = 100
 b.r = 2
 b.update = update_bullet

 return b
end

function update_diamond(d)
 if hit_wall(d, d.dx, d.dy) then
  if(hit_wall(d, d.dx, 0)) d.dx *= -0.5
  if(hit_wall(d, 0, d.dy)) d.dy *= -0.5
 end

 if(closer(d, ship, 5)) score += 1 remove_actor(d)
end

function add_diamond(x, y)
 local d = add_actor(x, y)

 d.dx = rnd() - 0.5
 d.dy = rnd() - 0.5
 d.sp = 12
 d.update = update_diamond

 return d
end

function kill_ship()
 -- alive = false
 dead_timer = 200
 explosion(ship.x + 4, ship.y + 4, 100)
 sfx(8)
end

function update_bomb(b)
 b.f = (b.f + 0.2) % 2

 b.l-=1

 if b.l < 0 or hit_wall(b, b.dx, b.dy) then
  local nx = b.x + b.dx + 4
  local ny = b.y + b.dy + 4
  local cx = flr(nx / 8)
  local cy = flr(ny / 8)
  local r = 2 + 1.5 * rnd()
  local n

  n = circle(cx, cy, r, 0)
  n += fix_tiles(edge_probs, cx - r, cy - r, cx + r + 1, cy + r + 1, false)
  spiral_explosion(nx, ny)
  sfx(8)

  for i = 1, n do
   add_diamond(b.x, b.y)
  end

  if alive and closer(b, ship, 10) then
   --kill_ship() 
  end

  remove_actor(b)
 end
end

function add_bomb(x, y)
 local b = add_actor(x, y)

 b.sp = 9
 b.l = 400
 b.update = update_bomb

 return b
end

function update_ship(s)
 s.ddx = -0.01 * s.dx
 s.ddy = -0.01 * s.dy

 if alive then

  if btn(4) then
   -- strafe mode
   local t, a

   t = 0
   s.dx = 0
   s.dy = 0
   if btn(0) then 
    t = 0.5
    a = s.angle + 0.25
   elseif btn(1) then
    t = 0.5
    a = s.angle - 0.25
   elseif btn(2) then
    t = 0.5
    a = s.angle
   elseif btn(3) then
    t = 0.5
    a = s.angle + 0.5
   end

   if t > 0 then
    s.dx = t * cos(a) 
    s.dy = t * sin(a)
    jet(s, 1 - a + 0.25)
   end
  else
   -- rotate mode
   if(btn(1)) s.angle -= 1 / 64
   if(btn(0)) s.angle += 1 / 64

   if btn(2) then 
    s.dx += 0.03 * cos(s.angle) 
    s.dy += 0.03 * sin(s.angle)
    jet(s, 1 - s.angle + 0.25)
    t = 0.03
   end
  end

  max_speed(s, 1)

  s.bt = max(0, s.bt - 1)
  if btn(5) and s.bt == 0 then 
   local b = add_bullet(s.x, s.y, s.angle)
   if not btn(4) then
    -- strafe mode bullets are absolute
    b.dx += s.dx
    b.dy += s.dy
   end
   s.bt = 20
  end

  s.bm = max(0, s.bm - 1)
  if btn(3, 1) and s.bm == 0 then 
   local b = add_bomb(s.x, s.y)
   b.dx += s.dx
   b.dy += s.dy

   s.bm = 40
  end
 end

 if hit_wall(s, s.dx, s.dy) then
  if(hit_wall(s, s.dx, 0)) s.dx *= -0.1
  if(hit_wall(s, 0, s.dy)) s.dy *= -0.1
 end
end

function draw_ship(s)
 if alive then
  local nx = s.x + 4 - screen_x
  local ny = s.y + 4 - screen_y
  local sz = 2

  x1 = nx + sz * cos(s.angle - 0.4)
  y1 = ny + sz * sin(s.angle - 0.4)
  x2 = nx + sz * cos(s.angle)
  y2 = ny + sz * sin(s.angle)
  x3 = nx + sz * cos(s.angle + 0.4)
  y3 = ny + sz * sin(s.angle + 0.4)
 
  color(7)
  line(x1, y1, x2, y2)
  line(x2, y2, x3, y3)
 end
end

function add_ship(x, y)
 local s = add_actor(x, y)

 s.r = 2
 s.angle = 0.25
 s.bt = 0
 s.bm = 0
 s.update = update_ship
 s.draw = draw_ship
 s.jt = 2

 return s
end

-- monsters

function update_monster(m)
 m:sub_update()

 if hit_wall(m, m.dx, m.dy) then
  if(hit_wall(m, m.dx, 0)) m.dx *= -0.1
  if(hit_wall(m, 0, m.dy)) m.dy *= -0.1
 end

 if alive and closer(m, ship, 4) then
  remove_actor(m)
  kill_ship()
 end
end

function add_monster(x, y)
 local m = add_actor(x, y)

 m.r = 3
 m.monster = true
 m.sleeping = false
 m.update = update_monster
 m.angle = 0
 m.state = 1
 m.timer = 500

 return m
end

function monster_transition(m)
 m.timer = max(0, m.timer - 1)
 if m.timer == 0 then
  local actions = m.transition[m.state]

  for i = 1, #actions do
   if rnd() < actions[i][1] then
    m.timer = rnd() * actions[i][2]
    m.state = actions[i][3]
    m.dx = 0
    m.dy = 0
    break
   end
  end
 end
end

-- look for wall directions, same order as for bits and sin()/cos()
-- right, up, left, down
roost_x = { 1,  0, -1,  0}
roost_y = { 0, -1,  0,  1}

function monster_try_roost(m)
 local cx = flr(m.x / 8)
 local cy = flr(m.y / 8)

 -- don't try to roost off the map
 if(cx < 0 or cx > 127 or cy < 0 or cy > 63) return

 if mget(cx, cy) == 0 then
  for i = 1, #m.roost do
   if rocky(mget(cx + roost_x[i], cy + roost_y[i])) then
    mset(cx, cy, m.roost[i])
    remove_actor(m)
    m.sleeping = true
   end
  end
 end
end

function accellerate_to(m, x, y, s)
 m.ddx = (x - m.x) * s
 m.ddy = (y - m.y) * s
end

-- s1 = speed of turn, s2 = speed of movement
function circle_to(m, x, y, s1, s2)
 local dx = x - m.x
 local dy = y - m.y
 local a = atan2(dx, dy)

 if(m.angle < a) m.angle += s1
 if(m.angle > a) m.angle -= s1

 m.dx = s2 * cos(m.angle)
 m.dy = s2 * sin(m.angle)
end

-- follow monster f, lagging by .delay positions
-- return false if the buffer is empty (and therefore f 
-- died a while ago)
function follow_to(s, f)
 -- init
 if not s.queue then
  s.queue = {}
  s.delay = 15
  for i = 1, s.delay do s.queue[i - 1] = {} end
  s.read = 0
  s.write = 0
 end

 local full = (s.write + 1) % s.delay == s.read
 local empty = s.write == s.read

 if f.alive then
  local r = s.queue[s.write]

  r[1] = f.x
  r[2] = f.y
  r[3] = f.dx
  r[4] = f.dy
  s.write = (s.write + 1) % s.delay
 end

 -- if f is alive and the buffer is full, read out
 -- if f is dead and the buffer is not empty, read out
 if (f.alive and full) or (not f.alive and not empty) then
  local p = s.queue[s.read]

  s.x = p[1]
  s.y = p[2]
  s.dx = p[3]
  s.dy = p[4]

  s.read = (s.read + 1) % s.delay
 end

 return not (s.read == s.write)
end

function hover(m)
 -- init
 if not m.hover_timer then
  m.hover_timer = 0
 end

 m.hover_timer = max(0, m.hover_timer - 1)
 if m.hover_timer == 0 then
  m.hover_timer = 60 * rnd() 
  m.dx = 0.5 * (rnd() - 0.5)
  m.dy = 0.5 * (rnd() - 0.5)
 end
end

function update_octo(o)
 o.f = (o.f + 0.2) % 4

 monster_transition(o)

 if o.state == 1 then
  accellerate_to(o, ship.x, ship.y, 0.0002)
 elseif o.state == 2 then
  hover(o)
 else 
  -- sleepy
  accellerate_to(o, 512, 256, 0.0002)
  monster_try_roost(o)
 end

 max_speed(o, 1.05)
end

function add_octo(x, y)
 local o = add_monster(x, y)

 o.sub_update = update_octo
 --[state] = {{probability, timer, new state}}
 o.transition = {
  [1] = {{1, 200, 2}},			-- attack
  [2] = {{0.5, 500, 1}, {1, 200, 3}},	-- hover
  [3] = {{1, 500, 2}},			-- sleepy
 }
 o.roost = {7, 49, 50, 5}
 o.sp = 1

 return o
end

function update_bat(b)
 b.f = (b.f + 0.2) % 4

 monster_transition(b)

 if b.state == 1 then
  accellerate_to(b, ship.x, ship.y, 0.0002)
 elseif b.state == 2 then
  hover(b)
 else 
  accellerate_to(b, 512, 256, 0.0002)
  monster_try_roost(b)
 end

 max_speed(b, 1.05)
end

function add_bat(x, y)
 local b = add_monster(x, y)

 b.sub_update = update_bat
 --[state] = {{probability, timer, new state}}
 b.transition = {
  [1] = {{1, 200, 2}},			-- attack
  [2] = {{0.5, 500, 1}, {1, 200, 3}},	-- hover
  [3] = {{1, 500, 1}},			-- sleepy
 }
 b.roost = {48, 51, 52, 33}
 b.sp = 34

 return b
end

function update_segment(s)
 local f = s.following

 if f then
  if not follow_to(s, f) then
   if f.sleeping then
    s.sleeping = true
    remove_actor(s)
   else
    s.l = 50
   end

   s.following = nil
  end
 else
  s.l = max(0, s.l - 1)
  if(s.l == 0) remove_actor(s) explosion(s.x + 4, s.y + 4, 10) 
 end
end

function add_segment(h)
 local s = add_monster(h.x, h.y)

 s.following = h
 s.sub_update = update_segment
 s.sp = 41 

 return s
end

function update_centi(m)
 m.f = (m.f + 0.1) % 2

 monster_transition(m)

 if m.state == 1 then
  -- attack
  circle_to(m, ship.x, ship.y, 0.001, 0.5)
 else 
  -- sleepy
  accellerate_to(m, 512, 256, 0.00002)
  monster_try_roost(m)
 end

 max_speed(m, 0.5)
end

function add_centi(x, y)
 local m = add_monster(x, y)

 local s, x

 m.sub_update = update_centi
 m.sp = 39 

 --[state] = {{probability, timer, new state}}
 m.transition = {
  [1] = {{1, 500, 3}},			-- attack
  [3] = {{1, 100, 1}},			-- sleepy
 }
 m.roost = {8, 53, 54, 38}

 x = m
 for i = 1, 15 do
  s = add_segment(x)
  x = s
 end

 return m
end

function update_mush(m)
 m.f = (m.f + 0.1) % 4

 -- init
 if not m.block_x then
  local cx = flr(m.x / 8)
  local cy = flr(m.y / 8)

  -- on init, .angle points away from the block we are walking along
  m.block_x = cx + cos(m.angle + 0.5)
  m.block_y = cy + sin(m.angle + 0.5)

  -- edges use the bits convention, so 0 - 3, right/down/left/up
  -- from block_x/_y
  m.edge = 4 - (m.angle * 4)

  -- 0 is the centre of the edge, +ve moves clockwise
  m.offset = 0

  if rnd() < 0.5 then
   m.direction = 1
  else
   m.direction = -1
  end

  -- we change x/y directly
  m.dx = 0
  m.dy = 0
 end

 m.offset += m.direction
 


end

function add_mush(x, y)
 local m = add_monster(x, y)

 -- m.sub_update = update_mush
 m.sub_update = update_centi
 m.sp = 55

 --[state] = {{probability, timer, new state}}
 m.transition = {
  [1] = {{1, 200, 3}},			-- attack
  [3] = {{1, 500, 1}},			-- sleepy
 }
 m.roost = {64, 64, 64, 64}

 return m
end

monster_table = {
 [5] = add_octo,
 [7] = add_octo,
 [8] = add_centi,
 [33] = add_bat,
 [38] = add_centi,
 [48] = add_bat,
 [49] = add_octo,
 [50] = add_octo,
 [51] = add_bat,
 [52] = add_bat,
 [53] = add_centi,
 [54] = add_centi,
 [64] = add_mush,
}

-- shake monster at cell x, y ... p is probability of waking it
function shake_monster(x, y, p)
 local v = mget(x, y)
 local add = monster_table[v]

 if add and rnd() < p then
  local m = add(x * 8, y * 8)
  mset(x, y, 0)

  if m.roost then
   for i = 1, #m.roost do
    if m.roost[i] == v then 
     m.angle = ((i - 1) / #m.roost) + 0.5
     m.dx = 0.5 * cos(m.angle)
     m.dy = 0.5 * sin(m.angle)
    end
   end
  end
 end
end

function wake_monsters(x, y)
 local cx = flr(x / 8)
 local cy = flr(y / 8)

 for i = cx - 1, cx + 1 do
  for j = cy - 1, cy + 1 do
   if(monster(mget(i, j))) shake_monster(i, j, 1)
  end
 end
end

function update_screen()
 local tx, ty, damp

 tx = ship.x + 4
 ty = ship.y + 4
 damp = 0.2
 if btn(4) then
  -- displace in strafe mode
  tx += 48 * cos(ship.angle)
  ty += 48 * sin(ship.angle)

  -- and damp less, since we're near the screen edge
  damp = 0.4
 end

 screen_dx += (tx - 64 - screen_x) * 0.1
 screen_dy += (ty - 64 - screen_y) * 0.1

 screen_dx *= damp
 screen_dy *= damp

 screen_x += screen_dx
 screen_y += screen_dy
end

function _update60()
 local nearby

 nearby = nearby_actors(ship, 5)
 foreach(nearby, update_actor)
 foreach(nearby, function (a)
  if(a.alive) update_actor_map(a)
 end)

 foreach(particles, update_particle)
 update_screen()

 if not alive then 
  dead_timer = max(0, dead_timer - 1)
  if(dead_timer == 0) alive = true score = 0
 end

 monster_timer = max(0, monster_timer - 1)
 if monster_timer == 0 then
  monster_timer = 100 * rnd()

  local cx = flr(ship.x / 8)
  local cy = flr(ship.y / 8)
  local r = 5

  for y = cy - r, cy + r do
   for x = cx - r, cx + r do
    if monster(mget(x, y)) then 
     -- roughly 0 - 1 probability of waking
     local dx = cx - x
     local dy = cy - y
     local p = 1 - sqrt(dx * dx + dy * dy) / r

     shake_monster(x, y, 2 * p)
    end
   end
  end
 end
end

-- start draw

function draw_actor(a)
 if a.draw then
  a:draw()
 else
  spr(a.sp + a.f, a.x - screen_x, a.y - screen_y)
 end
end

function ctext(s, y)
 if(y) ty = y
 print(s, 64 - 4 * #s / 2, ty)
 ty += 8
end

function draw_map()
 local cx = flr(screen_x / 8)
 local cy = flr(screen_y / 8)
 local x = screen_x % 8
 local y = screen_y % 8

 map(cx, cy, -x, -y, 17, 17)
end

function draw_scanner()
 local bx = (ship.x / 8) - 64
 local by = (ship.y / 8) - 64

 for x = 0, 127 do
  for y = 0, 127 do
   local v = mget(bx + x, by + y)
   local c

   c = 0
   if(rocky(v)) c = 4
   if(monster(v)) c = 8
   if(v == 13) c = 10

   pset(x, y, c)
  end
 end
 for i = 1, #actors do
  pset((actors[i].x / 8) - bx, (actors[i].y / 8) - by, 8)
 end
 pset(64, 64, 7)
 rect(56, 56, 72, 72, 7)

 color(15)
 print("score", 0, 110)
 print(score, 0, 120)

 color(5)
 print("#nearby " .. #nearby_actors(ship, 5), 80, 100)
 print("#acts " .. #actors, 80, 110)
end

function _draw()
 rectfill(0, 0, 127, 127, 0)

 foreach(stars, draw_star)
 draw_map()
 foreach(particles, draw_particle)

 foreach_nearby_actors(ship, 3, draw_actor)

 if(btn(4, 1)) draw_scanner() 
 if(btn(5, 1)) grow_veg() 

 if not alive then
  color(6)
  ctext("mega-roids", 20)
  ctext("")
  ctext("left/right rotate ship")
  ctext("up to thrust")
  ctext("hold z for strafe")
  ctext("x to fire")
  ctext("d to release bomb")
  ctext("a to grow veg")
 end

 color(5)
 print("cpu " .. flr((stat(1) * 100)) .. "%", 80, 120)
end

-- start init

generate_world()
build_actor_map()
add_stars()

-- game state

score = 0
dead_timer = 200
ship = add_ship(512, 768)
alive = false
screen_x = ship.x + 4 - 64
screen_y = ship.y + 4 - 64
screen_dx = 0
screen_dy = 0
monster_timer = 100

music(37)
__gfx__
0000000003333330033333300333333003333330033333300660000003330000000255500007000070000070000000000000000004a44a4044444444000cc000
0000000033333333333337733333333337733333333333336446006633133030202212250997990007999700000000000076670004a44a404244044400cccc66
0000000037733773333331733773377331733333311331136488664433133303022212220988890009888900000800000766766044a44a44440004440cc99cc4
00000000317331733773333331733173333337733333333368888444333333000022222277878770098789000009a80006676650aaaaaaaa400541440cc99cc4
00000000033333300173333003333330033331700333333088aa884233333300002222220988890009888900008a900000766500444994444605444406cccc44
00000000003333000033330000333300003333000033330088aa88443313330302d21222099799000799970000008000000650004449944444444444064cc444
000000000300003003000300003000300030003003000030088886663313303020d2122200070000700000700000000000000000444444444442444406424444
0000000030000300003000300030003000030003003003000088000003330000000d222000000000000000000000000000000000444444444444444406444444
44444444444444440660000006000660444444600644444406444444000000004444446044444444064444600333066066000060066600000666066000000000
447667444244444464460006646006464444460006444444006441446666000644442460444444146442446063b3344664666646644460066444644666000006
47667664444444440644666444466466444426000644424400644444444460064444446044444444064444604333344606444446641446646414446044666664
466766544444414400644444424446004444446006444444064444444444466444444460442444440064446041b3346006424460644444440644460044442444
447665444444444406444144444444604144446006444444064444444144444444444460444444440064416044442dd006444460064444420644260041444444
41465444444444440644444444441460466444460644144400624444444444444414460044446664064444464444d88d64444600064444440644446044444464
44444424444244440642444444444460600666466464446600644444444442444444460066660006644664466666d88d64404600644666666466644666444606
444444444444444406444444444444600000006066066600064444444444444444444460000000000660066000000dd006444460066000000600066000666000
0644446009000900090009000900090000900090009000900200002022000022002002000022220044444460064444430ee000bb066660604444446006444444
06444460099099000990990009909900009909900099099000200200002002000200002002ddd2204444460000644133e99e6b3b644446464448880006666444
0641446099999990977977909999999a09999999a99999990dd2222002222220022222202d2222224334260000633334e99e43b0444444464488888006777644
6444460091191190971917909779779a09999999a9779779d222222227722772222222222d2222524433bbb00633b3340ee4b460444244604887878867878764
64444600099999000999999a0719179aa9779770a97191702112211527122175277227752d222252414b3bb006333334064b4460144444604888888867777764
06444600aa999aa0aa9999aaaa9999aaaa71917aaa9999aa22222225222222252712217522222252466bb3bb0333334464444460444444464878878867877764
06424460aa999aa0aa999aaaaa999aa0aaa999aa0aa999aa22222225222222252222222502255520600bbb3b03b33b4464404460666664464488888006778666
06444460a99999a0a09990a0a09990000a09990a0009990a0222225002222250022222500022220000000bbb0333444406444460000006604448880000666600
0000000000300300000033300a99999aaaa09900052222200222d000008888000088880000888800008888000000003333000000330000003000000000000003
00990aaa03000030030331330aa999aa9aa919995222222222212d0208888880088888800888888007788770000bb330033bb000033000000330000000000330
99919aa900333300303331330aa999aa999919905222222222212d208778877888888888877887788718817800bb33b00b33bb00003ee0000303000000003030
09919999033333300033333300999990999999005112211222222200871881788778877887188178888888880bbb3bb00bb3bbb000eeee000300300030030030
00999999333333330033333309119119999919902222222d2222220088888888871881788888888888888888bbb3bb0000bb3bbb0ee11ee00300033003300030
099199993113311330333133099999999aa9199902222dd02221222000077000000770000007700000077000bb3bbb0000bbb3bb0ee11ee00300000000000030
99919aa9333333330303313300990990aaa09900002002005221220200777700007777000007700000077000b3bbb000000bbb3b00eeee000300000000000003
00990aaa03333330000033300090009000000000020000200555200000000000000000000077770000777700bbbb00000000bbbb000ee0003000000000000003
00888800008880000088800000888000008880000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
08888880087780000887700008778000077880000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
81188118887180708887107088718007871880070000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
88888888888887708888877088888777888887770000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
88888878888887708888877088888777888887770000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
78878888887180708887107088718007871880070000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
08788880087780000887700008778000077880000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00888800008880000088800000888000008880000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000900
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000999900
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000009900900
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000cccc00
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000c000c00
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000c000c00
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000cc00c00
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000cccc00
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000444440
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000440004
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000004444444
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000004000044
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000004000040
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000004444440
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000044
__gff__
0002020202020102020000000000010101010101010101010101010101010101010202020202020202020501010101010202020202020202020202040400040402020202020000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__map__
1000100000000000000000171700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1010121717130000000000171711000000000011000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0012111111111300000011111111110000001111111111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0015191111111800000011001111110000111111111111110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000001511111800000011000011110000111111110000110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000015111400001111000000000000000011110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000001a0000001111110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000011111111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0011000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0011111100001111000000000011110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000111111111111110000001111111100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000001111111111110000111100111100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__sfx__
001800200c0351004515055170550c0351004515055170550c0351004513055180550c0351004513055180550c0351104513055150550c0351104513055150550c0351104513055150550c035110451305515055
010c00201a2451a0501a0501a2351a0501a0501a2251a0501a0501a2251a0501a0501a2151a050000001021013245000001320013235000001320013225000001320013225000001320013215000001320013215
003000202874028740287302872026740267301c7401c7301d7401d7401d7401d7401d7301d7301d7201d72023740237402373023720267402674026730267201c7401c7401c7401c7401c7301c7301c7201c720
0030002000040000400003000030020400203004040040300504005040050300503005020050200502005020070400704007030070300b0400b0400b0300b0300c0400c0400c0300c0300c0200c0200c0200c020
00180020176151761515615126150e6150c6150b6150c6151161514615126150d6150e61513615146150e615136151761517615156151461513615126150f6150e6150a615076150561504615026150161501615
00180020010630170000000010631f633000000000000000010630000000000000001f633000000000000000010630000000000010631f633000000000000000010630000001063000001f633000000000000000
001800200e0351003511035150350e0351003511035150350e0351003511035150350e0351003511035150350c0350e03510035130350c0350e03510035130350c0350e03510035130350c0350e0351003513035
011800101154300000000001054300000000000e55300000000000c553000000b5630956300003075730c00300000000000000000000000000000000000000000000000000000000000000000000000000000000
000300000c6700d6700c6700c6700b6700b6700c6600c6600e6400e6300f620106201362013620156201864018640196501a6501c6401d6301e63020620226202362026620286202b6102c6102e6103360036600
01240020051450c145051450c145051450c145051450c145071450e145071450e145071450e145071450e1450d145141450d145141450d145141450d145141450c145071450c145071450c145071450c14507145
014800202174421740217402274024744247401f7441f7402074420740207401f7401d7401f7401c7441c7402174421740217402274024744247401c7441c7401d7441f740207402274024744247402474024745
012400200e145151450e145151450e145151450e145151450c145131450c145131450c145131450c145131450f145161450f145161450f145161450f145161450e145151450e145151450c145131450c14513145
011200200c1330960509613096131f6330960509615096150c1330960509613096130062309605096050e7130c1330960509613096131f6330960509615096150c1330960509613096130062309605096050e713
014800200c5240c5200c5200c52510524105201052010525115241152011520115251352413520135201352511524115201152011525135241352013520135251452414520145201452013520135201352013525
014800200573405730057300573507734077300773007735087340873008730087350c7340c7300c7300c73505734057300573005735077340773007730077350d7340d7300d7300d7350c7340c7300c7300c735
000100001c010250102801029010250101d020150201502016020190201e020210202401025010220101e01018010150201502015020160201a0101f020230202302023010200201902013010110101102014020
013200202005420050200502005520054200502005020055200542005020050200551e0541e0501c0541c05023054230502305023055210542105020054200501c0541c0501c0501c0501c0501c0501c0501c055
0132002025054250502505025055230542305021054210502805428050280502805527054270502305423050250542505025050250551e0541e0501e0501e0552305423050230502305023050230502305023055
0132002010140171401914014140101401714019140141400f14014140171401b1400f14014140171401b1400d1401014015140141400d1401014017140191400d1401014015140141400d140101401714019140
0132002015140191401c1401914015140191401c1401914014140191401b14017140121401414015140191401e1401914015140191401214014140151401914017140141401014012140171401e1401b14017140
013200202372423720237202372523724237202372023725237242372023720237252172421720207242072028724287202872028725257242572023724237202072420720207202072020720207202072020725
0132002028724287202872028725287242872028720287252c7242c7202c7202c7252a7242a72028724287202a7242a7202a7202a725257242572025720257252872428720287202872527724277202772027725
0019002001610016110161101611016110161104611076110b61112611166111b6112061128611306113561138611336112d6112961125611206111c6111861112611106110c6110861104611026110261101611
011e00200c505155351853517535135051553518535175350050015535185351a5350050515535185351a53500505155351c5351a53500505155351c5351a53500505155351a5351853500505155351a53518535
010f0020001630020000143002000f655002000020000163001630010000163002000f655001000010000163001630010000163002000f655002000010000163001630f65500163002000f655002000f60300163
013c002000000090750b0750c075090750c0750b0750b0050b0050c0750e075100750e0750c0750b0750000000000090750b0750c0750e0750c0751007510005000000e0751007511075100750c0751007510005
013c00200921409214092140921409214092140421404214022140221402214022140221402214042140421409214092140921409214092140921404214042140221402214022140221402214022140421404214
013c00200521405214052140521404214042140721407214092140921409214092140b2140b214072140721405214052140521405214042140421407214072140921409214092140921409214092140921409214
0001000031606396263b6263b6203a6203862030600036000a6000560001600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
011400181862500000000001862518625186251862500000186051862018625000001862500000000001862500000000001862518605186251862518605186250000000000000000000000000000000000000000
010f00200c0730000018605000000c0730000000000000000c0730000000000000000c0730000000000000000c0730000000000000000c0730000000000000000c0730000000000000000c073000000000000000
013c0020025500255004550055500455004550055500755005550055500755007550045500455000550005500255002550045500555004550045500555007550055500555007550095500a550095500755009550
013c00201a54526305155451a5451c545000001a5451c5451d5451c5451a545185451a5450000000000000001a5452100521545180051c5450000018545000001a545000001c545000001a545000000000000000
011e00200557005575025650000002565050050557005575025650000002565000000457004570045750000005570055750256500000025650000005570055750256500000025650000007570075700757500000
0001000010530185401c55020560231602316025570251702417024560231602216021550205501d5501a5401955017550155401354012530105300f5200e5200d5200c5200a5200952008510075100651005500
013c00201d1151a1151a1151d1151a1151a1151c1201c1251d1151a1151a1151d1151a1151a1151f1201f1251d1151a1151a1151d1151a1151a1151c1201c1251d1151a1151a1151d1151a1151a1151f1201f125
011e0020091351500009135000050920515000091350000009145000000920500000071400714007145000000913500000091350000009205000000913500000091450000009205000000c2000c2050020000000
015000200706007060050600506003060030600506005060030600306005060050600206002060030600306007060070600506005060030600306005060050600306003060050600506007060070600706007060
01280020131251a1251f1251a12511125181251d125181250f125161251b125161250e125151251a125151250f125161251b1251612511125181251d125181250e125151251a125151251f1251a125131250e125
01280020227302273521730227301f7301f7301f7301f7352473024735227302273521730217351d7301d7351f7301f7352173022730217302173522730247302673026730267302673500000000000000000000
012800202773027735267302473524730247302473024735267302673524730267352273022730227302273524730247352273021735217302173021730217351f7301f7301f7301f7301f7301f7301f7301f735
015000200f0600f0600e0600e060070600706005060050600c0600c060060600606007060090600a0600e0650f0600f0600e0600e060070600706005060050600c0600a060090600206007060070600706007065
012800200f125161251b125161250e125151251a12515125131251a1251f1251a12511125181251d125181250f125161251b125161250e125151251a12515125131251a1251f1251a125131251a1251f1251a125
012800201a5201a525185201a525135101351013510135151b5201b5251a5201a525185201852515520155251652016525185201a52518520185251a5201b520155201552015520155251f5001f5001f5001f505
012800201f5201f5251d5201b525155101551015510155151d5201d5251b5201d5251a5101a5101a5101a5151b5201b5251a5201a52518520185201552015525165201652016520165251a5001a5001a5001a505
013c00201003500500000001003509000000000e0300e0351003500000000001003500000000000e0000e00511035000000000011035000000000010030100351103500000000001103500000000000400004005
011e00201813518505000001713517505000001513515505000001013010130101350000000000000000000015135000000000010135000000000011500115001150011500111301113011130111350000000000
01180020071550e1550a1550e155071550e1550a1550e155071550e1550a1550e155071550e1550a1550e155051550c155081550c155051550c155081550c155051550c155081550c155051550c137081550c155
01180020071550e1550a1550e155071550e1550a1550e155071550e1550a1550e155071550e1550a1550e155081550f1550c1550f155081550f1550c1550f155081550f1550c1550f155081550f1370c1550f155
01180020081550f1550c1550f155081550f1550c1550f155081550f1550c1550f155081550f1550c1550f155071550e1550a1550e155071550e1550a1550e155071550e1550a1550e155071550e1370a1550e155
011800201305015050160501605016050160551305015050160501605016050160551605015050160501a05018050160501805018050180501805018050180550000000000000000000000000000000000000000
011800201305015050160501605016050160551305015050160501605016050160551605015050160501a0501b0501b0501b0501b0501b0501b0501b0501b0550000000000000000000000000000000000000000
011800201b1301a1301b1301b1301b1301b1351b1301a1301b1301b1301b1301b1351b1301a1301b1301f1301a130181301613016130161301613016130161350000000000000000000000000000000000000000
011800201b1301a1301b1301b1301b1301b1351b1301a1301b1301b1301b1301b1351b1301a1301b1301f1301d1301d1301d1301d1301d1301d1301d1301d1350000000000000000000000000000000000000000
01180020081550f1550c1550f155081550f1550c1550f155081550f1550c1550f155081550f1550c1550f1550a155111550e155111550a155111550e155111550a155111550e155111550a155111550e15511155
011800202271024710267102671026710267152271024710267102671026710267152671024710267102971027710267102471024710247102471024710247150000000000000000000000000000000000000000
01180020227102471026710267102671026715227102471026710267102671026715267102471026710297102b7102b7102b7102b7102b7102b7102b7102b7150000000000000000000000000000000000000000
011800202b720297202b7202b7202b7202b7252b720297202b7202b7202b7202b7252b720297202b7202e72029720277202672026720267202672026720267250000000000000000000000000000000000000000
011800202b720297202b7202b7202b7202b7252b720297202b7202b7202b7202b7252b720297202b7202e7202e7202e7202e7202e7202e7202e7202e7202e7250000000000000000000000000000000000000000
010c00200c133000000061500615176550000000615006150c133000000061500615176550000000615006150c133000000061500615176550000000615006150c13300000006150061517655000000061500615
0118002002070020700207002070040700407004070040700c0700c0700c0700c0700a0700a0700a0700a0700e0700e0700e0700e0700d0700d0700d0700d070100701007010070100700e0700e0700e0700e075
011800200000015540155401554015545115401154011540115451354013540135401354510540105401054010545115401154011540115451054010540105401054513540135401354013545095400954009545
0118002009070090700907009070070700707007070070700907009070090700907002070020700207002070030700307003070030700a0700a0700a0700a0700707007070070700707007070070700707007075
01180020000001054010540105401054511540115401154011545105401054010540105450e5400e5400e5400e545075400754007540075450e5400e5400e5400e54505540055400554005540055400554005545
__music__
01 08004243
00 08014300
00 03014300
00 02030500
00 02030500
00 03414300
00 08014500
00 03040500
00 03020500
00 03020500
02 08010706
01 0a4d0949
00 0a0d090c
00 0a4c0b4c
00 0a0d0e4e
02 0f4d0c09
01 10124316
00 11134316
00 10121416
00 11131516
00 12424316
02 13424316
01 19425b18
00 19175a18
00 19171a18
00 1b425c18
02 1a194318
01 1f1d5e60
00 1f1d5e20
00 1f1d4320
00 221d211e
00 231d211e
02 1c1d2444
01 25262744
00 292a2844
00 2526272b
02 292a282c
01 2d181e24
00 2d181e24
00 2d181e2e
00 2d181e2e
00 2d181e6e
02 2d181e6e
01 2f454305
00 30424305
00 2f324344
00 30334344
00 2f323705
00 30333805
00 31344344
00 36354344
00 31343905
02 36353a05
01 3c423b41
00 3c423b44
00 3c3d3b44
00 3c3d3b44
00 3e523b41
00 3e423b41
00 3e3f3b44
00 3e3f3b44
00 3e013b41
02 3e013b41
00 41424344

