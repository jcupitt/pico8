pico-8 cartridge // http://www.pico-8.com
version 8
__lua__
-- mega-roids
-- jcupitt

-- particle system

particles = {}

function add_particle(x, y)
 local p = {}

 p.x = x
 p.y = y
 p.dx = 0
 p.dy = 0
 p.drag = 0
 p.life = 30
 p.c = 8 * rnd()
 p.size = 0		-- 0 is one pixel across

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
  p.life = 40 * rnd() + 5
  p.size = rnd()
  p.drag = 0.02
 end
end

function spiral_explosion(x, y)
 if(#particles > 200) return

 local ba = rnd()

 for n = 1, 100 do
  local p = add_particle(x, y)
  local a = ba + n / 3.1
  local s = n / 15

  p.dx = s * sin(a)
  p.dy = s * cos(a)
  p.life = 5 * s + 5
  p.drag = 0.1
 end
end

function reanimate_circle(x, y)
 for n = 1, 40 do
  local a = n / 40
  local dx = 5 * cos(a + 0.2)
  local dy = 5 * sin(a + 0.2)
  local px = x + 50 * cos(a) - 35 * dx
  local py = y + 50 * sin(a) - 35 * dy
  local p = add_particle(px, py)
  p.dx = dx
  p.dy = dy

  p.life = 30
  p.size = 3
 end
end

function reanimate_in(x, y)
 for n = 1, 40 do
  local a = n / 40
  local px = x + 50 * cos(a)
  local py = y + 50 * sin(a)
  local p = add_particle(px, py)

  p.dx = 0.1 * cos(a + 0.5)
  p.dy = 0.1 * sin(a + 0.5)
  p.life = 50
  p.size = 3
  p.drag = -0.1
 end
end

function jet(s, a)
 s.jt = (s.jt + 1) % 3
 if(s.jt != 0) return

 local j = add_particle(s.x + 4, s.y + 4)

 a += (rnd() - 0.5) * 0.15
 j.dx = sin(a) + s.dx
 j.dy = cos(a) + s.dy
 j.life = 15 * rnd() + 15
 j.drag = 0.04
end

function spark(m)
 local s = add_particle(m.x + 3, m.y + 3)

 s.dx = m.dx + 0.5 * rnd()
 s.dy = m.dy + 0.5 * rnd()
 s.life = 15 * rnd() + 15
 s.drag = 0.1
end

function update_particle(p)
 p.dx -= p.dx * p.drag
 p.dy -= p.dy * p.drag
 p.x += p.dx
 p.y += p.dy
 p.c = (p.c + 1) % 8
 p.life -= 1

 if(p.life < 0) del(particles, p)
end

function draw_particle(p)
 local x = p.x - screen_x
 local y = p.y - screen_y

 rectfill(x, y, x + p.size, y + p.size, 8 + p.c)
end

-- star field
stars = {}

function draw_star(s)
 local x = (s.x - screen_x * 0.5) % 128
 local y = (s.y - screen_y * 0.5) % 128

 rectfill(x, y, x, y, 12)
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

function circle(cx, cy, rx, ry, v)
 local n = 0

 for y = cy - ry, cy + ry do
  for x = cx - rx, cx + rx do
   dx = (cx - x) / rx
   dy = (cy - y) / ry
   if sqrt(dx * dx + dy * dy) < 1 then
    if(mget(x, y) == 16) n += 1
    mset(x, y, v)
   end
  end
 end

 return n
end

function worm(x, y, a)
 local nx, ny, r
 local dx, dy

 nx = x
 ny = y
 r = 3
 while true do
  circle(nx, ny, r, r, 0)

  r += rnd() - 0.5
  r = max(min(r, world.max_diameter), 1.5)
  a += (rnd() - 0.5) / 10
  nx += cos(a)
  ny += sin(a)

  dx = nx - 64
  dy = 2 * (ny - 32)
  if(sqrt(dx * dx + dy * dy) > 64) break
 end

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

-- test sprite for plain rock, ie. not a sleeping monster or rocky veg
function plainrock(s)
 return rocky(s) and not monster(s) and not growth(s)
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

-- roost: first is the sprite we show in this cell if there's a wall to the
-- right ... turn a roost index into a bits
roost_to_bits = {11, 7, 14, 13}

function get_bits(x, y)
 local b

 b = 0
 if(plainrock(mget(x + 1, y))) b += 1
 if(plainrock(mget(x, y - 1))) b += 2
 if(plainrock(mget(x - 1, y))) b += 4
 if(plainrock(mget(x, y + 1))) b += 8

 return b
end

-- {bits, {{prob, value}, {prob, value}, ...}},

function fix_tiles(tiles, x1, y1, x2, y2)
 local n = 0

 for y = y1, y2 do
  for x = x1, x2 do
   local v = mget(x, y)

   if plainrock(v) then
    local b = get_bits(x, y)
    local p = tiles[b]

    if p then
     for i = 1, #p do
      if rnd() < p[i][1] then
       -- count number of diamonds we destroy
       if(v == 16) n += 1

       v = p[i][2]
       break
      end
     end

     mset(x, y, v)
    end
   end
  end
 end

 return n
end

-- vegetation growth templates
-- for each growth point, a table of {probability, direction, new sprite}

vegetation = {
 [42] = {{0.1, 1, 1, 60}, {0.1, 1, 1, 62}, {0.1, 1, 1, 61}},
 [59] = {{0.1, -1, 1, 59}, {0.2, -1, 1, 63}},
 [60] = {{0.1, 1, 1, 60}, {0.5, 1, 1, 62}, {0.4, 1, 1, 61}},
 [62] = {{0.1, -1, 1, 59}, {0.4, -1, 1, 63}, {0.1, 1, 0, 106}},
 [63] = {{0.1, 1, 1, 60}, {0.4, 1, 1, 62}, {0.1, 1, 1, 61}, {0.1, -1, 0, 105}},
 [105] = {{0.1, -1, 1, 59}, {0.2, -1, 1, 63}, {0.1, -1, 1, 62}},
 [106] = {{0.1, 1, 1, 60}, {0.2, 1, 1, 61}, {0.1, 1, 1, 62}},
 [46] = {{0.1, 1, 0, 107}},
 [47] = {{0.1, -1, 0, 108}, {0.1, 0, -1, 108}},
 [107] = {{0.1, 1, 0, 107}, {0.1, 0, 1, 107}, 
  {0.1, -1, 0, 107}, {0.1, 0, -1, 107}},
 [108] = {{0.1, 1, 0, 108}, {0.1, 0, 1, 108}, 
  {0.1, -1, 0, 108}, {0.1, 0, -1, 108}},
}

-- do growth_rate cells on each call
function grow_veg(growth_rate)
 if not current_x then
  current_x = 0
  current_y = 0
 end

 for i = 1, growth_rate do
  current_x = (current_x + 1) % 127
  if(current_x == 0) current_y = (current_y + 1) % 63

  local v = mget(current_x, current_y)

  if growth(v) then
   local g = vegetation[v]

   if g then
    for i = 1, #g do
     local tx = current_x + g[i][2]
     local ty = current_y + g[i][3]

     if rnd() < g[i][1] and mget(tx, ty) == 0 then
      mset(tx, ty, g[i][4])
      break
     end
    end
   end
  end
 end
end

function generate_world()
 circle(63, 32, 100, 100, 0)
 circle(63, 32, 63, 32, 17)

 for i = 1, world.n_worms do
  local a = rnd()
  local d = rnd()
  local sx = 64 + d * 50 * cos(a)
  local sy = 32 + d * 28 * sin(a)

  worm(sx, sy, a)
 end

 -- worm from the centre for the portal
 circle(63, 32, 4, 4, 0)
 worm(63, 32, rnd())

 -- make monster prob table
 -- for each bits setting, search for monsters which roost at that angle
 local object_probs 

 object_probs = {}
 for i = 1, 4 do
  local b = roost_to_bits[i]

  object_probs[b] = {}

  for j = 1, #world.monsters do 
   local ot = world.monsters[j]

   add(object_probs[b], {ot[1], ot[2].roost[i]})
  end
 end

 fix_tiles(object_probs, 0, 0, 127, 63)

 fix_tiles(world.edge_probs, 0, 0, 127, 63)

 for i = 1, 3 do 
  grow_veg(128 * 64) 
 end
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

-- loop over nearby actors .. good for searches, not good for nearby table 
-- updates, since that will modify the thing we are looping over
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
    for k = 1, #m do
     if(m[k]) f(m[k])
    end
   end
  end
 end
end

-- make a table of nearby actors ... we need to do this if we plan to 
-- modify the nearby table
function nearby_actors(a, r)
 local nearby 

 nearby = {}
 foreach_nearby_actors(a, r, function (m)
  add(nearby, m)
 end)

 return nearby
end

function add_actor(x, y)
 local a = {}

 a.x = x
 a.y = y
 a.cx = 0
 a.cy = 0
 a.dx = 0
 a.dy = 0
 a.ddx = 0
 a.ddy = 0
 a.sprite = 1	-- base sprite number
 a.frame = 0	-- frame of animation
 a.radius = 3
 a.bounce = 0.5	-- amount of energy retained in a wall bounce
 a.drag = 0.02	-- lose this proportion of velocity per frame

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

 -- the map cell this actor is in
 a.cx = flr((a.x + 4) / 8)
 a.cy = flr((a.y + 4) / 8)
 a.in_map = a.cx >= 0 and a.cx < 128 and a.cy >= 0 and a.cy < 63

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
 return closer(a, b, a.radius + b.radius)
end

-- limit actor's max speed ... scale the unit vector
function max_speed(a, s)
 local l = sqrt(a.dx * a.dx + a.dy * a.dy)
 if l > s then
  a.dx = s * a.dx / l
  a.dy = s * a.dy / l
 end
end

function get_map(x, y)
 return mget(flr(x / 8), flr(y / 8))
end

-- will moving actor a by dx/dy hit a wall
function hit_wall(a, dx, dy)
 local nx = a.x + dx + 4
 local ny = a.y + dy + 4

 return rocky(get_map(nx - a.radius, ny - a.radius)) or
  rocky(get_map(nx + a.radius, ny - a.radius)) or
  rocky(get_map(nx - a.radius, ny + a.radius)) or
  rocky(get_map(nx + a.radius, ny + a.radius))
end

-- test dx/dy and bounce
function bounce(a)
 if hit_wall(a, a.dx, a.dy) then
  if hit_wall(a, a.dx, 0) then
   local ndx = -a.bounce * a.dx

   if not hit_wall(a, ndx, 0) then
    a.dx = ndx
   else
    a.dx = 0
   end
  end

  -- we know the x movement is ok, use that and just change y
  if hit_wall(a, a.dx, a.dy) then
   local ndy = -a.bounce * a.dy

   if not hit_wall(a, a.dx, ndy) then
    a.dy = ndy
   else
    a.dy = 0
   end
  end
 end
end

chests = {
 [0] = {
  "fast reload powerup!!",
  500,
  function () ship.reload_time = 5 end,
  function () ship.reload_time = 20 end,
 },
 {
  "bomb bonus powerup!!",
  0,
  function () ship.bombs += 10 end,
  function () end,
 },
 {
  "super shield powerup!!",
  0,
  function () ship.shields = 5 end,
  function () end,
 },
 {
  "diamond bonus powerup!!",
  0,
  function () diamonds += 5 end,
  function () end,
 },
 {
  "bouncing bullets powerup!!",
  500,
  function () ship.bullet_bounce = true end,
  function () ship.bullet_bounce = false end,
 },
 {
  "three-way bullet powerup!!",
  500,
  function () ship.spread = 0.05 end,
  function () ship.spread = 0 end,
 },
 {
  "five-way bullet powerup!!",
  500,
  function () ship.spread = 0.1  end,
  function () ship.spread = 0 end,
 },
}

function open_chest()
 -- +1 since # is index of last element, not length of table
 local i = flr((#chests + 1) * rnd())
 local chest = chests[i]

 if(powerdown_timer > 0) powerdown_callback()

 message = chest[1]
 message_timer = 100
 powerdown_timer = chest[2]
 chest[3]()
 powerdown_callback = chest[4]
end

function add_bullet(s, a)
 local b = add_actor(s.x, s.y)

 b.dx = 2 * cos(a)
 b.dy = 2 * sin(a)
 if not btn(4) then
  -- strafe mode bullets are absolute
  b.dx += s.dx
  b.dy += s.dy
 end
 b.x += b.dx
 b.y += b.dy
 b.sprite = 11
 b.l = 100
 b.radius = 1
 b.bounce = 1
 b.bullet_bounce = s.bullet_bounce

 b.update = function (b)
  b.l -= 1
  if(b.l < 0) remove_actor(b)

  if hit_wall(b, b.dx, b.dy) then
   if b.bullet_bounce then
    bounce(b)
   else
    explosion(b.x + 4, b.y + 4, 5) 
    wake_monsters(b.x + 4, b.y + 4)
    remove_actor(b)
   end
  end

  foreach_nearby_actors(b, 1, function(a)
   if a != b then
    if a.monster and collision(b, a) then
     explosion(b.x + 4, b.y + 4, 10) 
     remove_actor(b)
     hit_monster(a)
    end
   end
  end)
 end

 return b
end

function add_enemy_bullet(m, t)
 local b = add_actor(m.x, m.y)

 local dx = t.x - m.x
 local dy = t.y - m.y
 local a = atan2(dx, dy)

 b.dx = cos(a)
 b.dy = sin(a)

 b.x += b.dx
 b.y += b.dy
 b.sprite = 87
 b.l = 100
 b.radius = 1
 b.st = 0

 b.update = function (b)
  b.l -= 1
  if(b.l < 0) remove_actor(b)

  b.st = (b.st + 1) % 10
  if(b.st == 0) spark(b)

  if hit_wall(b, b.dx, b.dy) then
   explosion(b.x + 4, b.y + 4, 5) 
   wake_monsters(b.x + 4, b.y + 4)
   remove_actor(b)
  end

  if closer(b, ship, 4) then
   hit_ship()
   remove_actor(b)
  end
 end

 return b
end

function add_diamond(x, y)
 local d = add_actor(x, y)

 d.dx = rnd() - 0.5
 d.dy = rnd() - 0.5
 d.sprite = 101
 d.n_sprite = 3
 d.sparkle = 0

 d.update = function (d)
  bounce(d)
  if(closer(d, ship, 5)) diamonds += 1 remove_actor(d)
 end

 d.draw = function (d)
  local sp

  d.sparkle = (d.sparkle + 1) % 100
  sp = d.sprite
  if(d.sparkle < 18) sp += d.sparkle / 6

  spr(sp, d.x - screen_x, d.y - screen_y)
 end

 return d
end

function add_power(x, y)
 local p = add_actor(x, y)

 p.dx = rnd() - 0.5
 p.dy = rnd() - 0.5
 p.radius = 1
 p.bounce = 0
 p.drag = 0.9

 p.update = function (p)
  local dx = ship.x - p.x
  local dy = ship.y - p.y
  local a = atan2(dx, dy)

  p.ddx = 0.1 * cos(a)
  p.ddy = 0.1 * sin(a)

  p.dx *= 0.9
  p.dy *= 0.9

  bounce(p)

  max_speed(p, 1)

  if closer(p, ship, 4) then
   ship.power += 1 
   if ship.power > 9 then
    ship.power = 0
    ship.bombs += 1
   end
   remove_actor(p)
  end
 end

 p.draw = function (p) 
  local nx = 4 + p.x - screen_x
  local ny = 4 + p.y - screen_y

  rectfill(nx, ny, nx + 1, ny + 1, 7)
 end

 return p
end

function add_portal(x, y)
 local p = add_actor(x, y)

 p.update = function (p) 
  if closer(p, ship, 10) then
   if diamonds >= 10 then
    message = "you warp to the next level!!"
   else
    message = "collect 10 diamonds to open"
   end

   message_timer = 100
  end
 end

 p.draw = function (p) 
  local nx = p.x - screen_x - 8
  local ny = p.y - screen_y - 8

  spr(104, nx + 4, ny + 4, 1, 1, false, false)
  spr(104, nx + 12, ny + 4, 1, 1, true, false)
  spr(104, nx + 4, ny + 12, 1, 1, false, true)
  spr(104, nx + 12, ny + 12, 1, 1, true, true)
 end

 return p
end

function reanimate_ship()
 if(reanimate_timer > 0) return
 reanimate_timer = 270
end

function hit_ship()
 if(reanimate_timer > 0) return
 if(invulnerability > 0) return

 explosion(ship.x + 4, ship.y + 4, 100)
 sfx(8)

 if ship.shields > 0 then
  ship.shields -= 1
  ship.shield_timer = 500
 else
  n_ships += 1
  reanimate_ship()
 end
end

function update_bomb(b)
 b.l-=1

 if b.l < 0 or hit_wall(b, b.dx, b.dy) then
  local r = 2 + 1.5 * rnd()

  local n

  n = circle(b.cx, b.cy, r, r, 0)
  n += fix_tiles(plain_edges, b.cx - r, b.cy - r, b.cx + r + 1, b.cy + r + 1)
  spiral_explosion(b.x, b.y)
  sfx(8)

  for i = 1, n do
   add_diamond(b.x, b.y)
  end

  if reanimate_timer > 0 and closer(b, ship, 10) then
   hit_ship() 
  end

  remove_actor(b)
 end
end

function add_bomb(x, y)
 local b = add_actor(x, y)

 b.sprite = 9
 b.l = 400
 b.update = update_bomb

 return b
end

function update_ship(s)
 s.ddx = -0.01 * s.dx
 s.ddy = -0.01 * s.dy

 invulnerability = max(0, invulnerability - 1)

 reanimate_timer = max(0, reanimate_timer - 1)

 if reanimate_timer == 69 then
  reanimate_circle(s.x + 3, s.y + 3)
 elseif reanimate_timer == 40 then
  reanimate_in(s.x + 3, s.y + 3)
 elseif reanimate_timer == 1 then
  s.shields = 3
  invulnerability = 30
 elseif reanimate_timer == 0 then
  -- alive

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

   if not s.reverse and btn(3) then
    s.angle += 0.5
    s.reverse = true
   end
   if not btn(3) then
    s.reverse = false
   end

   if btn(2) or s.reverse then 
    s.dx += 0.04 * cos(s.angle) 
    s.dy += 0.04 * sin(s.angle)
    jet(s, 1 - s.angle + 0.25)
    t = 0.03
   end
  end

  s.bt = max(0, s.bt - 1)
  if btn(5) and s.bt == 0 then 
   for a = s.angle - s.spread, s.angle + s.spread, 0.05 do
    add_bullet(s, a)
   end

   s.bt = s.reload_time
  end

  s.bm = max(0, s.bm - 1)
  if btn(3, 1) and s.bm == 0 and s.bombs > 0 then 
   local b = add_bomb(s.x, s.y)
   b.dx += s.dx
   b.dy += s.dy
   s.bombs -= 1
   s.bm = 40
  end

  s.shield_timer = max(0, ship.shield_timer - 1)
  if s.shield_timer == 1 then
   s.shield_timer = 500
   if(s.shields < 3) s.shields += 1
  end

  -- don't kill monsters while invul, too easy
  if invulnerability == 0 then
   foreach_nearby_actors(ship, 1, function (m)
    if m.monster and closer(m, ship, 4) then
     hit_monster(m)
     hit_ship()
    end
   end)
  end

  if mget(s.cx, s.cy) == 13 then
   mset(s.cx, s.cy, 0)
   open_chest()
  end

 end

 bounce(s)
 max_speed(s, 1)
end

shield_radius = {5, 8, 11, 14, 17}

function draw_ship(s)
 if reanimate_timer == 0 then
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

  s.shield_draw_timer = (s.shield_draw_timer + 0.1) % (s.shields * 5 + 1)
  for i = 1, min(s.shields, #shield_radius) do 
   local r = shield_radius[i]

   if i == flr(s.shield_draw_timer) then
    color(6)
    r += 1
   else
    color(5)
   end

   circ(nx, ny, r)
  end
 end
end

function add_ship(x, y)
 local s = add_actor(x, y)

 s.radius = 2
 s.angle = 0.25
 s.bt = 0
 s.bm = 0
 s.update = update_ship
 s.draw = draw_ship
 s.jt = 2
 s.shields = 3
 s.shield_timer = 0
 s.shield_draw_timer = 0
 s.bombs = 3
 s.power = 0
 s.reload_time = 20
 s.bounce = 0.1
 s.bullet_bounce = false
 s.spread = 0
 s.reverse = false

 return s
end

-- monsters

function accelerate_to(m, x, y, s)
 m.ddx = (x - m.x) * s
 m.ddy = (y - m.y) * s
end

function accelerate_away(m, x, y, s)
 m.ddx = (m.x - x) * s
 m.ddy = (m.y - y) * s
end

function fire_at(m, t, r)
 if(not m.bullet_timer) m.bullet_timer = 0
 m.bullet_timer = (m.bullet_timer + 1) % r
 if m.bullet_timer == 1 then
  add_enemy_bullet(m, t)
 end
end

-- look for wall directions, same order as for bits and sin()/cos()
-- right, up, left, down
roost_x = { 1,  0, -1,  0}
roost_y = { 0, -1,  0,  1}

function try_roost(m)
 -- don't try to roost off the map
 if(not m.in_map) return

 if mget(m.cx, m.cy) == 0 then
  for i = 1, #m.mt.roost do
   if plainrock(mget(m.cx + roost_x[i], m.cy + roost_y[i])) then
    mset(m.cx, m.cy, m.mt.roost[i])
    remove_actor(m)
    m.sleeping = true
   end
  end
 end
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

--[[ monster moods

	1 normal (no attack, just wandering around)
	2 angry (attacks)
	3 scared (runs away)
	4 sleepy (looks for roost)
	5 psycho (suicide attack)

]]

octo = {
 roost = {7, 49, 50, 5},
 sprite = 1,
 n_sprites = 4,

 eyes = true,
 eye_x = 0,
 eye_y = 0,

 level = 1,
 max_speed = 1.05,
 recolour = {
  [2] = {3, 11},
 },
 health = {
  [2] = 5,
 },

 transition = {
  [1] = {200, {1, 2}},			
  [2] = {500, {0.5, 1}, {1, 4}},
  [3] = {500, {0.5, 1}, {1, 2}},
  [4] = {500, {1, 1}},		
  [5] = {500, {1, 3}},		
 },

 update = {
  [1] = function (m) 
   hover(m)
   if(closer(m, ship, 30)) set_mood(m, 2)
  end,
  [2] = function (m) 
   accelerate_to(m, ship.x, ship.y, 0.0002) 
   fire_at(m, ship, 100)
  end,
  [3] = function (m) accelerate_away(m, ship.x, ship.y, 0.0003) end,
  [4] = function (m) 
   accelerate_to(m, 512, 256, 0.0002) 
   max_speed(m, 0.4)
   try_roost(m) 
  end,
  [5] = function (m) accelerate_to(m, ship.x, ship.y, 0.0004) end,
 },

 hit = function (m)
  if m.health == 2 and rnd() < 0.5 then
   set_mood(m, 5)
  else
   set_mood(m, 3)
  end
 end
}

bat = {
 roost = {48, 51, 52, 33},
 sprite = 34,
 n_sprites = 4,

 eyes = true,
 eye_x = 0,
 eye_y = 0,

 max_speed = 1.05,
 health = 5,

 transition = {
  [1] = {200, {1, 2}},			
  [2] = {500, {0.5, 1}, {1, 4}},
  [3] = {500, {1, 1}},
  [4] = {500, {1, 1}},		
  [5] = {500, {1, 2}},		
 },

 update = {
  [1] = function (m)
   hover(m)
   if(closer(m, ship, 30)) set_mood(m, 2)
  end,
  [2] = function (m) accelerate_to(m, ship.x, ship.y, 0.0002) end,
  [3] = function (m) accelerate_away(m, ship.x, ship.y, 0.0003) end,
  [4] = function (m) 
   accelerate_to(m, 512, 256, 0.0002) 
   max_speed(m, 0.4)
   try_roost(m) 
  end,
  [5] = function (m) accelerate_to(m, ship.x, ship.y, 0.0004) end,
 },

 hit = function (m)
  if m.health == 2 and rnd() < 0.5 then
   set_mood(m, 5)
  else
   set_mood(m, 3)
  end
 end
}

segment = {
 roost = nil,
 sprite = 41,
 n_sprites = 1,

 eyes = false,

 max_speed = 1000,

 transition = {
  [1] = {10000, {1, 1}}
 },

 update = {
  [1] = function (s) 
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
 }
}

centi = {
 -- if rock to right, down left, up
 roost = {8, 53, 54, 38},
 sprite = 39,
 n_sprites = 2,

 eyes = true,
 eye_x = 0,
 eye_y = 0,

 max_speed = 0.5,

 transition = {
  [1] = {500, {1, 2}},			
  [2] = {500, {1, 4}},			
  [4] = {500, {1, 1}}		
 },

 add = function (c)
  for i = 1, 15 do
   local s

   s = add_monster(c.x, c.y, segment)
   s.following = c
   c = s
  end
 end,

 update = {
  [1] = function (m) 
   circle_to(m, 512, 256, 0.002, 0.5)
   if(closer(m, ship, 30)) set_mood(m, 2)
  end,
  [2] = function (m) circle_to(m, ship.x, ship.y, 0.002, 0.5) end,
  [4] = function (m) 
   circle_to(m, 512, 256, 0.002, 0.5) 
   try_roost(m) 
  end
 }
}

mush = {
 -- if rock to right, down left, up
 roost = {64, 64, 64, 64},
 sprite = 55,
 n_sprites = 4,

 eyes = true,
 eye_x = 0,
 eye_y = 0,

 max_speed = 0.5,

 transition = {
  [1] = {200, {1, 2}},
  [2] = {500, {1, 4}},			
  [4] = {500, {1, 1}}
 },

 update = {
  [1] = function (m) 
   hover(m)
   if(closer(m, ship, 30)) set_mood(m, 2)
  end,
  [2] = function (m) accelerate_to(m, ship.x, ship.y, 0.0002) end,
  [4] = function (m) 
   circle_to(m, 512, 256, 0.002, 0.5) 
   try_roost(m) 
  end
 }
}

crab = {
 -- if rock to right, down left, up
 roost = {74, 75, 76, 73},
 sprite = 69,
 n_sprites = 4,

 eyes = true,
 eye_x = 0,
 eye_y = 1,

 max_speed = 1.05,

 transition = {
  [1] = {200, {1, 2}},
  [2] = {500, {1, 4}},			
  [4] = {500, {1, 1}}
 },

 update = {
  [1] = function (m) 
   hover(m) 
   if(closer(m, ship, 30)) set_mood(m, 2)
  end,
  [2] = function (m) accelerate_to(m, ship.x, ship.y, 0.0002) end,
  [4] = function (m) 
   circle_to(m, 512, 256, 0.002, 0.5) 
   try_roost(m) 
  end
 }
}

present = {
 -- if rock to right, down left, up
 roost = {97, 97, 97, 97},
 sprite = 97,
 n_sprites = 4,
 eyes = true,
 eye_x = 0,
 eye_y = 1,

 max_speed = 0.4,

 transition = {
  [1] = {100, {1, 2}},
  [2] = {200, {1, 3}},
  [3] = {200, {1, 1}},
 },

 update = {
  [1] = function (m) 
   hover(m) 
  end,
  [2] = function (m) 
   accelerate_to(m, ship.x, ship.y, 0.002) 
   fire_at(m, ship, 50)
  end,
  [3] = function (m)
   remove_actor(m)
   explosion(m.x + 4, m.y + 4, 10)
  end,
 }
}

tree = {
 -- if rock to right, down left, up
 roost = {94, 95, 96, 93},
 sprite = 89,
 n_sprites = 4,

 eyes = false,

 max_speed = 0.2,

 transition = {
  [1] = {200, {1, 2}},
  [2] = {200, {1, 4}},			
  [4] = {500, {1, 1}}
 },

 update = {
  [1] = function (m) 
   hover(m) 
   if(closer(m, ship, 50)) set_mood(m, 2)
  end,
  [2] = function (m) 
   accelerate_to(m, ship.x, ship.y, 0.0002) 

   if not m.present_timer then
    m.present_timer = 0
    m.n_presents = 0
   end

   m.present_timer = (m.present_timer + 1) % 100
   if m.present_timer == 0 and m.n_presents < 4 then
    add_monster(m.x, m.y, present)
    m.n_presents += 1
   end
  end,
  [4] = function (m) 
   circle_to(m, 512, 256, 0.002, 0.5) 
   try_roost(m) 
  end
 }
}

spawn = {
 -- if rock to right, down, left, up
 roost = {109, 110, 111, 88},
 sprite = 88,
 n_sprites = 1,

 eyes = false,

 max_speed = 0,

 transition = {
  [1] = {300, {1, 4}},
  [4] = {1000, {1, 1}},
 },

 update = {
  [1] = function (m) 
   if not m.spawn_timer then
    m.spawn_timer = 100
    m.n_monsters = 0
   end

   m.spawn_timer = (m.spawn_timer + 1) % 100
   if m.spawn_timer == 0 and m.n_monsters < 5 then
    -- 5 means we don't spwan spwans or trees
    local n = flr(rnd() * 5 + 1)
    local mt = monster_table[n]
    local nm = add_monster(m.x, m.y, mt)

    nm.angle = m.angle
    nm.dx = 3 * cos(nm.angle)
    nm.dy = 3 * sin(nm.angle)

    m.n_monsters += 1
   end
  end,
  [4] = function (m) 
   try_roost(m) 
  end
 },

 draw = function (m)
  local nx = m.x - screen_x
  local ny = m.y - screen_y

  spr(spawn.roost[(m.angle - 0.5) * 4 + 1], nx, ny)
 end,
}

--[[
function wall_walk(m)
 m.frame = (m.frame + 0.1) % 4

 -- init
 if not m.block_x then
  -- on init, .angle points away from the block we are walking along
  m.block_x = m.cx + cos(m.angle + 0.5)
  m.block_y = m.cy + sin(m.angle + 0.5)

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
]]

monster_table = {
 octo,
 bat,
 centi,
 mush,
 crab,
 tree,
 spawn,
}

function set_mood(m, i)
 if m.mood != i then
  local t = m.mt.transition[i][1]
  m.timer = 0.5 * (t * rnd() + t)
  m.mood = i
  m.dx = 0
  m.dy = 0
 end
end

function transition_monster(m)
 m.timer = max(0, m.timer - 1)
 if m.timer == 0 then
  local actions = m.mt.transition[m.mood]

  for i = 2, #actions do
   if rnd() < actions[i][1] then
    local new_mood = actions[i][2]

    set_mood(m, new_mood)
    break
   end
  end
 end
end

-- make eyes jiggle up and down
eye_jiggle = {[0] = 0, 1, 0, -1}

-- eye sprites, index with mood
-- for sleepy (4), normal plus high blink
eye_sprites = {77, 79, 80, 77, 81}

function add_monster(x, y, mt)
 local m = add_actor(x, y)

 m.mt = mt
 m.radius = 3
 if(mt.radius) m.radius = mt.radius
 m.health = 1
 if(mt.health and mt.level and mt.health[mt.level]) m.health = mt.health[mt.level]
 m.monster = true
 m.sleeping = false
 m.angle = 0
 m.mood = 1
 m.timer = mt.transition[m.mood][1]
 m.jiggle = 0
 m.blink_timer = 60
 m.sprite = m.mt.sprite
 m.bounce = 0.1

 m.update = function (m)
  m.frame = (m.frame + 0.2) % m.mt.n_sprites

  transition_monster(m)

  -- dead player sends monsters from attack to normal
  if(m.mood == 2 and reanimate_timer > 0) set_mood(m, 1)

  m.mt.update[m.mood](m)

  max_speed(m, m.mt.max_speed)

  bounce(m)
 end

 m.draw = function (m) 
  local mt = m.mt
  local recolour

  if mt.recolour and mt.level and mt.recolour[mt.level] then 
   recolour = mt.recolour[mt.level]
  end


  if mt.draw then
   mt.draw(m)
  else
   local nx = m.x - screen_x
   local ny = m.y - screen_y

   if recolour and not m.morph_timer then
    m.morph_timer = 60
   end

   if recolour then
    m.morph_timer = max(0, m.morph_timer - 1)
    if(m.morph_timer % 20 < 10) pal(recolour[1], recolour[2])
   end

   spr(m.sprite + m.frame, nx, ny)

   if(recolour) pal()

   if mt.eyes then
    local eye

    m.jiggle = (m.jiggle + 0.2) % 4
    eye = eye_sprites[m.mood]
    m.blink_timer = max(0, m.blink_timer - 1)
    if(m.blink_timer < 5) eye = 78
    if m.blink_timer == 0 then
     local time
 
     time = 60
     -- fast blink for sleepy 
     if(m.mood == 4) time = 10
     m.blink_timer = time * rnd() + time
    end
    spr(eye, nx + mt.eye_x, ny + mt.eye_y + eye_jiggle[flr(m.jiggle)])
   end
  end
 end

 if(mt.add) mt.add(m)

 return m
end

function hit_monster(m)
 if(m.mt.hit) m.mt.hit(m)

 m.health = max(0, m.health - 1)
 if m.health == 0 then
  add_power(m.x, m.y)
  remove_actor(m)
 end
end

-- shake monster at cell x, y ... p is probability of waking it
function shake_monster(cx, cy, p)
 local v = mget(cx, cy)

 local mt, angle

 mt = nil
 for i = 1, #monster_table do
  local roost = monster_table[i].roost

  for j = 1, #roost do
   if roost[j] == v then
    mt = monster_table[i]
    angle = ((j - 1) / #roost) + 0.5
    break
   end
  end

  if(mt) break
 end

 if mt and rnd() < p then
  local m = add_monster(cx * 8, cy * 8, mt)

  m.angle = angle
  m.dx = 0.5 * cos(m.angle)
  m.dy = 0.5 * sin(m.angle)

  mset(cx, cy, 0)
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

function reset_explored()
 explored = {}
 for y = 0, 63 do
   explored[y] = {}
  for x = 0, 127 do
   explored[y][x] = false
  end
 end
end

function mark_explored(x, y)
 local cx = flr(x / 8)
 local cy = flr(y / 8)

 local left = max(0, cx)
 local right = min(127, cx + 15)
 local top = max(0, cy)
 local bottom = min(63, cy + 15)

 for y = top, bottom do
  for x = left, right do
   explored[y][x] = true
  end
 end
end

function update_screen()
 local tx, ty, damp

 tx = ship.x + 4
 ty = ship.y + 4
 damp = 0.2
 if reanimate_timer == 0 and btn(4) then
  -- displace in strafe mode
  tx += 48 * cos(ship.angle)
  ty += 48 * sin(ship.angle)

  -- and damp less, since we're near the screen edge
  damp = 0.4
 end

 screen_dx += (tx - 64 - screen_x) * 0.2
 screen_dy += (ty - 64 - screen_y) * 0.2

 screen_dx *= damp
 screen_dy *= damp

 screen_x += screen_dx
 screen_y += screen_dy

 -- it's 1/8th scale, so every 10 frames should be fine
 if(not explore_timer) explore_timer = 0
 explore_timer = (explore_timer + 1) % 10
 if(explore_timer == 0) mark_explored(screen_x, screen_y) 
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

 monster_timer = max(0, monster_timer - 1)
 if monster_timer == 0 then
  monster_timer = 100 * rnd()
  for y = ship.cy - 8, ship.cy + 8 do
   for x = ship.cx - 8, ship.cx + 8 do
    if monster(mget(x, y)) then 
     -- roughly 0 - 1 probability of waking
     local dx = ship.cx - x
     local dy = ship.cy - y
     local p = 1 - sqrt(dx * dx + dy * dy) / 8

     shake_monster(x, y, 2 * p)
    end
   end
  end
 end

 powerdown_timer = max(0, powerdown_timer - 1)
 if(powerdown_timer == 1) powerdown_callback()
end

-- start draw

function draw_actor(a)
 if a.draw then
  a:draw()
 else
  spr(a.sprite + a.frame, a.x - screen_x, a.y - screen_y)
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
 local bx = ship.cx - 64
 local by = ship.cy - 64

 for x = 0, 127 do
  for y = 0, 127 do
   local nx = flr(bx + x)
   local ny = flr(by + y)

   local v
   local c

   v = 0
   if nx >= 0 and nx < 128 and ny >= 0 and ny < 64 then
    if(explored[ny][nx]) v = mget(nx, ny)
   end

   c = 0
   if(rocky(v)) c = 4
   if(monster(v)) c = 8
   if(v == 13) c = 10

   pset(x, y, c)
  end
 end

 for i = 1, #actors do
  pset(actors[i].cx - bx, actors[i].cy - by, 8)
 end

 if(not beacon_timer) beacon_timer = 0
 beacon_timer = (beacon_timer + 1) % 10
 if(beacon_timer < 5) pset(64 - bx, 32 - by, 7)

 pset(64, 64, 7)
 rect(56, 56, 72, 72, 7)

 color(15)
 print("diamonds", 0, 110)
 print(diamonds, 0, 120)

 color(5)
 print("#nearby " .. #nearby_actors(ship, 5), 80, 100)
 print("#acts " .. #actors, 80, 110)
end

instructions = {
 "",
 "",
 "hold left shift for scanner",
 "fly to the beacon!",
 "",
 "up to thrust",
 "left/right to rotate ship",
 "down to reverse",
 "",
 "press x to fire",
 "",
 "hold z to strafe",
 "",
 "press d to release a bomb",
 "use bombs to mine for diamonds",
 "shoot enemies to get more bombs",
 "",
 "reach the centre",
 "of the tenth asteroid",
 "to find your goal!",
}

function _draw()
 rectfill(0, 0, 127, 127, 0)

 foreach(stars, draw_star)
 draw_map()
 foreach(particles, draw_particle)
 foreach_nearby_actors(ship, 3, draw_actor)
 rectfill(3, 3, 4, 4, 7)
 color(6)
 print(ship.power, 8, 1)
 spr(9, 24, 0)
 print(ship.bombs, 32, 1)
 spr(101, 48, 0)
 print(diamonds, 56, 1)
 spr(112, 112, 0)
 print(n_ships, 120, 1)

 if(btn(4, 1)) draw_scanner() 

 grow_veg(world.growth_rate) 

 message_timer = max(0, message_timer - 1)
 if message_timer > 0 then
  ctext(message, 90)
 end

 if reanimate_timer == 0 and instruction_counter <= #instructions then
  color(7)
  ctext(instructions[instruction_counter], 24)
  instruction_timer = (instruction_timer + 1) % 120
  if(instruction_timer == 0) instruction_counter += 1
 end

 color(5)
 print("cpu " .. flr((stat(1) * 100)) .. "%", 80, 120)
end

-- start init

-- all these index with bits 

-- the plain dirt edges
dirt_edges = {
 [0] = 30, 29, 26, 21, 45, 31, 20, 25, 28, 18, 32, 22, 19, 23, 24
}

add_flowers = {
 [4] = {27},
 [6] = {42, 12},
 [8] = {44},
 [9] = {15},
 [11] = {43},
}

add_mushrooms = {
 [3] = {47},
 [14] = {46},
}

-- substitute these for green edges
swap_green_edges = {
 [18] = 113,
 [15] = 123,
 [19] = 114,
 [23] = 115,
 [28] = 116,
 [44] = 122,
 [29] = 117,
 [30] = 118,
 [31] = 119,
 [45] = 120,
 [27] = 121,
}

-- build a basic edge table ... index with bits, have a table of 
-- {probability, sprite} for each one
-- p is diamond probability
function edge_table(p)
 local edges

 edges = {}
 for i = 0, 14 do
  edges[i] = {{1, dirt_edges[i]}}
 end
 edges[15] = {{p, 16}, {0.05, 14}, {0.05, 10}, {1, 17}}

 return edges
end

-- add a set of edges with a probability
function add_edges(edges, new, p)
 for i = 0, 15 do
  if new[i] then
   local row

   row = {}
   for j = 1, #new[i] do 
    add(row, {p, new[i][j]}) 
   end

   for j = 1, #edges[i] do 
    add(row, edges[i][j]) 
   end

   edges[i] = row
  end
 end

 return edges
end

-- substitute a set of edge tiles, eg. to add the grass tiles
function substitute_edges(edges, substitute)
 for i = 0, 15 do
  for j = 1, #edges[i] do 
   local pair = edges[i][j]

   if(substitute[pair[2]]) pair[2] = substitute[pair[2]]
  end
 end

 return edges
end

-- used for explosions which musn't leave flowers or grass
plain_edges = edge_table(0.1)

worlds = {
 -- world 1
 {
  message = "you warp into a strange world!",
  monsters = {
   {0.5, octo},
   {0.4, crab},
   {0.05, tree},
  },
  growth_rate = 1,
  n_worms = 5,
  max_diameter = 2,
  edge_probs = substitute_edges(
   add_edges(edge_table(0.1), add_flowers, 0.2), swap_green_edges),
  enter = function ()
   octo.level = 2
  end
 },

 -- world 2
 {
  message = "welcome to the mushroom kingdom!",
  monsters = {
   {0.05, octo},
   {0.05, mush},
   {0.05, crab},
  },
  growth_rate = 2,
  n_worms = 20,
  max_diameter = 5,
  edge_probs = add_edges(edge_table(0.1), add_mushrooms, 0.3),
  enter = function ()
  end
 },

 -- world 3
 {
  message = "welcome to the world of flowers!!!",
  monsters = {
   {0.05, octo},
   {0.05, mush},
   {0.05, bat},
   {0.05, crab},
   {0.05, centi},
   {0.05, tree},
   {0.05, spawn},
  },

  growth_rate = 10,
  n_worms = 20,
  max_diameter = 5,
  edge_probs = substitute_edges(
   add_edges(edge_table(0.1), add_flowers, 0.3), swap_green_edges),
  enter = function ()
  end
 },

}

function enter_world(n)
 world = worlds[n]
 world.enter()
 generate_world()
 build_actor_map()
 add_stars()
 diamonds = 0
 reset_explored()
 add_portal(512, 256)
 ship = add_ship(512, 768)
 screen_x = ship.x + 4 - 64
 screen_y = ship.y + 4 - 64
 screen_dx = 0
 screen_dy = 0
 message = world.message
 message_timer = 200
 powerdown_timer = 0
 invulnerability = 0
 monster_timer = 100
 reanimate_ship()
end

-- game state

diamonds = 0
instruction_counter = 1
instruction_timer = 0
n_ships = 1
reanimate_timer = 0

octo.level = 1
mush.level = 1
bat.level = 1
crab.level = 1
centi.level = 1
tree.level = 1
spawn.level = 1

music(37)
enter_world(1)

__gfx__
0000000003333330033333300333333003333330033333300660000003330000000255500000000044444444000000004444446004a44a404444444400388300
0000000033333333333333333333333333333333333333336446006633133030202212250006d00044245444000000004444aa0004a44a404444042400888866
00000000333333333333333333333333333333333113311363ee63443313330302221222006ddd004445444400008000443aaaa344a44a444400044408811883
0000000033333333333333333333333333333333333333336eeee444333333000022222206ddddd0445444140089a00044aa88aaaaaaaaaa4005444438851884
000000000333333003333330033333300333333003333330ee11ee3233333300002222220ddddd5045444444000a980041aa88aa444994444605444406888834
000000000033330000333300003333000033330000333300ee51ee443313330302d2122200ddd5005444444400080000466aaaa6444994444444441406388444
0000000003000030030003000030003000300030030000300eeee3663313303020d212220005500044444444000000006003aa36444444444442444406424444
00000000300003000030003000300030000300030030030003ee000003330000000d222000000000444442440000000000300060444444444444444406444444
44444444444444440660000006000000444444600644444406444444000000004444446044444444064444600333066066000060066600000666066000000000
447667444244444464460006646000004444460006444444006441446666000641442460444444146442446063b3344664666646644460066444644666000006
47667664444444440644666444466000444426000644424400644444444460064444446044444444064444604333344606444446641446646414446044666664
466766544444414400644444424446004444446006444444064444444444466444444460442444440064446041b3346006424460644444440644460044442444
447665444444444406444144444444604144446006444444064444444144444444444460444444440064416044442dd006444460064444420644260041444444
41465444444444440644444444441460466444460644144400624444444444444414460044446664064444464444d88d64444600064444440644446044444464
44444424444244440642444442444460600666466464446600644444444442444444460066660006644664466666d88d64404600644666666466644666444606
444444444444444406444444444144600000006066066600064444444444444444424460000000000660066000000dd006444460066000000600066000666000
0644446000900090090009000900090000900090009000900200002022000022002002000022220044444460064444430ee000bb066660604444446006444444
06444460009909900990990009909900009909900099099000200200002002000200002002ddd2204444460000644133e99e6b3b644446464448880006666444
0641446009999999999999909999999a09999999a99999990d2222200d2222200dd222202d2222224334260000633334e99e43b0444444464488888006777644
6444460009119119999999909999999a09999999a9999999d2222222d2222222d22222222d2222524433bbb00633b3340ee4b460444244604887878867878764
64444600009999900999999a0999999aa9999990a9999990d1122115d2222225d22222252d222252414b3bb006333334064b4460144444604888888867777764
064446000a99999aaa9999aaaa9999aaaa99999aaa9999aad2222225d2222225d222222522222252466bb3bb0333334464444460444444464878878867877764
064244600aa999aaaa999aaaaa999aa0aaa999aa0aa999aa22222225222222252222222502255520600bbb3b03b33b4464404460666664464488888006778666
064444600aa999aaa09990a0a09990000a09990a0009990a0222225002222250022222500022220000000bbb0333444406444460000006604448880000666600
0000000000300300000033300a99999aaaa09900052222200222d00000888800008888000088880000888800000000033000000033000300300b000330000003
00990aaa03000030030331330aa999aa9aa919995222222222212d0208888880088888800888888008888880000bbb3003bbb000003cc00003300030030b0330
99919aa900333300303331330aa999aa999919905222222222212d208888888888888888888888888888888800bbb3b00b3bbb0000cccc000303330003003030
09919999033333300033333300999990999999005112211222222200888888888888888888888888888888880bbb3bb00bb3bbb00cc11cc303000b0000330030
00999999333333330033333309119119999919902222222d2222220088888888888888888888888888888888bbb3bb0000bb3bbb3cc51cc00300000000000030
099199993113311330333133099999999aa9199902222dd02221222000077000007777000007700000077000bb3bbb0000bbb3bb00cccc300300000000000b03
99919aa9333333330303313300990990aaa09900002002005221220200777700000000000077770000077000b3bbb000000bbb3b003cc0000300000000000003
00990aaa03333330000033300090009000000000020000200555200000000000000000000000000000777700bbbb00000000bbbb030000003000000000000003
00888800008800080088000000080000008800000a0000a000a00a000000000000a00a0000000000000088088000000880880000000000000000000001000010
08888880aa0080800a00808800a080000a0080880aa00aa00aa00aa0000000000aa00aa00000000000001aa00aa44aa00aa10000000000000000000000100100
811881180a088400aa08840000a88488aa08840080000008800000080aa00aa08000000800000000000814a08a4888a80a819000077007700110011000000000
88888888000888400008884000088840000888408099880880998808809988088099880800998800000888408118811804889000071001700000000007000070
88888888000988400009884000098840000988400988888009888880098888800988888081188118000988400088990004888000000000000000000007100170
788788870a098800aa09880000a98888aa098800008884000088840000888400008884008a8884a8000918a0000000000a418000000000000000000000000000
08788780aa0090800a00908800a090000a009088080440800804408000844800080440800aa44aa000001aa0000000000aa10000000000000000000000000000
00878800008800080088000000080000008800008000000808000080008008000800008080000008000088080000000080880000000000000000000000000000
010000100000000000000000000000000000000007000000000000000000000000000000000aa000000aa000000aa000000aa000000000000000003000088000
100000010000000000770000001000001007700070077000000770000000000000e00e0000000000000000000000000000000000000000000000303033333333
000000000000000000710000001000000100100000017000000770000000000000e00e000033c3000033330000c3330000333b00003333000030333000333300
071001700770077000000000000000000000000000000000000000000008b000000ee00000033000000330000003300000033000000330000033333803333330
07700770077007700000000000000000000000000000000000000000000b800000eeee0003b3333003e333300333333003e33330033333300033333800033000
00000000000000000071000000100000010010000001700000077000000000000ee8cee00033e30000333b000033e30000333c00003333000030333000333300
0000000000000000007700000010000010077000700770000007700000000000eeec8eee3333333333c333333b33333333333333333333330000303000000000
0000000000000000000000000000000000000000070000000000000000000000dddddddd00088000000880000008800000088000000880000000003000000000
03000000000000000880000000000000000000000000000000000000000000000000009900000033300000000088880000666600000000edddddddddde000000
0303000008880880800808800888088000000800007667000067660000667600000999aa0000030003000000088788800677776000000eedeee8ceeedee00000
0333030080008008000080088000800800888080076676600676676006676670009aaaaa000030000030000088888888677877760ee0eeed0eec8ee0deee0ee0
8333330000cc8cc800cc888000cc8cc808cc8c8006676650076676500676675009aaaaaa0b00300000030b008888887868777786000eec8d00eeee00dc8ee000
8333330000cc888000cc8cc000cc888080cc88c000766500006765000066750009aaaaaa00330b000b0300008788888867777776000ee8cd000ee000d8cee000
033303000088888000888880008888800088888000065000000650000007500009aaaaae030000000000300088878887677878760ee0eeed00e00e00deee0ee0
0303000000cc8cc000cc8cc000cc8cc000cc8cc00000000000000000000000009aaaaae83000000000000330088887800677776000000eed00e00e00dee00000
0300000000cc8cc000cc8cc000cc8cc000cc8cc00000000000000000000000009aaaae8830000000000000030088880000666600000000ed00000000de000000
000700000333000003b000000000000033000b300b33000003b30b300000000003b333300b3303300ee000bb0038830000000000000000000000000000000000
0007000033b333333333b000b33b000b33b3333333333003333333333b30000333333b3333b33b33e99e3b3b0088883b00000000000000000000000000000000
0070700033333b3bb333330033333033033333b333b3b33bb333b33b333b333bb33b333b3333333be99e33b00881188300000000000000000000000000000000
00707000033b333333b3333033b33b330b33b330b333333303b43330333333b33442443031b33b300ee3b3303885188b00000000000000000000000000000000
070007000b33333333343b3033333333034443b00334433303342330b33343331444443044442dd003333b300388883300000000000000000000000000000000
07000700033334444444133044433344334443000b4444440344443044444464444444464444d88d3b3443300b3883b300000000000000000000000000000000
00000000033244444244443044444244b3404330644666666466644666444606666664466666d88d334044b00332444400000000000000000000000000000000
00000000034444444441443044444444034444300660000006000660006660000000066000000dd0034444300344444400000000000000000000000000000000
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
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000aaaaaaaa00000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000aaaaaaaa00000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000aaaaaaaa00000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000aaaaaaaa00000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000aaaaaaaa00000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000aaaaaaaa00000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000aaaaaaaa00000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000aaaaaaaa00000000
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

__gff__
0002020202030103030001000100010101010101010101010101010101010101010302020202030202020501010105050303030303030302020202040400040403020202020202020203030303020202020202020202020203020202020303030302020202000000000404050503030300010101010101010101010100000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
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

