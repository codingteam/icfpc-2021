#!/usr/bin/python
# -*- coding: utf-8 -*-

"""
=====
USAGE:
  PYTHON2:
    sudo apt install python2-tk
    python2 lightning_gui08.py problem1.json solution1.json
  PYTHON3:
    sudo apt install python3-tk
    python3 lightning_gui08.py problem1.json solution1.json
  PYPY (fastest):
    sudo apt install pypy-tk
    pypy lightning_gui08.py problem1.json solution1.json

NB: specify path to "solution1.json" file EVEN IF IT DOES NOT EXIST YET.
=====
"""

import sys, math, json

#python3 compat
try: import Tkinter as Tk
except ImportError: import tkinter as Tk
try: xrange
except NameError: xrange=range

UNSTRETCH_SPEED = 0.1 # must be <1
STUFF_SPEED = 0.05 # must be < UNSTRETCH_SPEED ?
FILL_SPEED = 0.05 # must be < STUFF_SPEED ?
AUTO_PERIOD = 10 # milliseconds
BONUSCOLOR = {'GLOBALIST':'#880','WALLHACK':'#840','BREAK_A_LEG':'#008','SUPERFLEX':'#088'}

def sqdist(p1, p2):
	x1, y1 = p1
	x2, y2 = p2
	return (x2-x1)*(x2-x1)+(y2-y1)*(y2-y1)

def getquad(vx, vy):
	if vx > 0 and vy >= 0: return 0
	if vx <= 0 and vy > 0: return 1
	if vx < 0 and vy <= 0: return 2
	if vx >= 0 and vy < 0: return 3
	return -10
def point_in_hole(holepoints, point):
	px, py = point
	angle = 0
	x1, y1 = holepoints[-1]
	for x2,y2 in holepoints:
		q1, q2 = getquad(x1-px, y1-py), getquad(x2-px, y2-py)
		if q1 == q2:
			pass # nothing to do
		elif q1+1==q2 or (q1==3 and q2==0):
			angle += 1
		elif q2+1==q1 or (q2==3 and q1==0):
			angle -= 1
		else:
			mult = (px-x1)*(y2-y1)-(x2-x1)*(py-y1)
			if mult == 0:
				return True # on the line
			elif mult < 0:
				angle += 2
			else: # mult > 0
				angle -= 2
		x1,y1 = x2,y2
	assert (angle + 4*len(holepoints)) % 4 == 0
	return angle != 0

# px,py  x1,y1  x2,y2  (px-x1)*(y2-y1)-(x2-x1)*(py-y1)
#  1,1    0,3    3,0   3
#  1,1    3,0    0,3   -3
#  2,2    0,3    3,0   -3
#  2,2    3,0    0,3   3
#  1,2    0,3    3,0   0

def line_in_hole(holepoints, p1, p2):
	if not point_in_hole(holepoints, p1) or not point_in_hole(holepoints, p2):
		return False
	# make sure we don't intersect hole edges
	ax1,ay1 = p1
	ax2,ay2 = p2
	bx1,by1 = holepoints[-1]
	for bx2,by2 in holepoints:
		if ( (ax2-ax1)*(by1-ay1)-(bx1-ax1)*(ay2-ay1) ) * ( (ax2-ax1)*(by2-ay1)-(bx2-ax1)*(ay2-ay1) ) < 0 and \
		   ( (bx2-bx1)*(ay1-by1)-(ax1-bx1)*(by2-by1) ) * ( (bx2-bx1)*(ay2-by1)-(ax2-bx1)*(by2-by1) ) < 0:
			return False
		bx1,by1 = bx2,by2
	return True

def move_point( targetp, sourcep, move_speed, round_to_int = False ):
	srcx, srcy = sourcep
	tgtx, tgty = targetp
	newx, newy = (1-move_speed)*srcx + move_speed*tgtx, (1-move_speed)*srcy + move_speed*tgty
	if round_to_int:
		newx = int(newx+.0001) if tgtx < srcx else int(newx+.9999)
		newy = int(newy+.0001) if tgty < srcy else int(newy+.9999)
		return newx, newy
	if abs(tgtx-srcx)<0.5 and abs(tgty-srcy)<0.5:
		return tgtx, tgty
	return newx, newy

#def dist_line_point( line, point ):
#	x1,y1,x2,y2 = line
#	px,py = point
#	x1,y1,x2,y2 = x1-px,y1-py, x2-px,y2-py
#	return abs(x1*y2-x2*y1) / math.sqrt( (x2-x1)*(x2-x1) + (y2-y1)*(y2*y1) )
#assert dist_line_point( (1,1,3,1), (0,3) ) == 2

def closest_line_point( line, point ):
	x1,y1,x2,y2 = line
	px,py = point
	sqlinelen = sqdist( (x1,y1), (x2,y2) )
	scalmult = (px-x1)*(x2-x1)+(py-y1)*(y2-y1)
	hx = x1+(x2-x1)*scalmult*1.0/sqlinelen
	hy = y1+(y2-y1)*scalmult*1.0/sqlinelen
	if (x1-hx)*(x2-hx) <= 0 and (y1-hy)*(y2-hy) <= 0:
		return hx,hy
	else:
		d1 = sqdist( (x1,y1), (px,py) )
		d2 = sqdist( (x2,y2), (px,py) )
		return (x1,y1) if d1<d2 else (x2,y2)

assert closest_line_point( (1,1,3,1), (0,3) ) == (1,1)
assert closest_line_point( (1,1,3,1), (2,3) ) == (2,1)
assert closest_line_point( (1,1,3,1), (5,3) ) == (3,1)

class Scale:
	def __init__(self, TARGETWIDTH, TARGETHEIGHT, problem_json, dstp):
		srcp = problem_json['figure']['vertices']
		minhx = min( hx for hx,hy in problem_json['hole'] )
		minhy = min( hy for hx,hy in problem_json['hole'] )
		maxhx = max( hx for hx,hy in problem_json['hole'] )
		maxhy = max( hy for hx,hy in problem_json['hole'] )
		minpx = min( px for px,py in srcp )
		minpy = min( py for px,py in srcp )
		maxpx = max( px for px,py in srcp )
		maxpy = max( py for px,py in srcp )
		minx = min(minhx, minpx)
		miny = min(minhy, minpy)
		maxx = max(maxhx, maxpx)
		maxy = max(maxhy, maxpy)
		self.scale_kx = self.scale_ky = min( TARGETWIDTH*0.95/(maxx-minx), TARGETHEIGHT*0.95/(maxy-miny) )
		self.scale_bx = TARGETWIDTH/2 - (maxx+minx)/2 * self.scale_kx
		self.scale_by = TARGETHEIGHT/2 - (maxy+miny)/2 * self.scale_ky
		self.midpx, self.midpy = self.getpxy(TARGETWIDTH/2, TARGETHEIGHT/2)
	def getsxy(self, px, py):
		return px*self.scale_kx+self.scale_bx, py*self.scale_ky+self.scale_by
	def getpxy(self, sx, sy):
		return (sx-self.scale_bx)/self.scale_kx, (sy-self.scale_by)/self.scale_ky
	def noticeable_distance(self):
		return int( max( 20/self.scale_kx, 20/self.scale_ky, 1 ) )
	def getmidpxy(self):
		return self.midpx, self.midpy
	def zoomin(self, sx,sy):
		ZOOM = 1.3
		self.scale_kx *= ZOOM
		self.scale_bx = self.scale_bx*ZOOM - sx*(ZOOM-1)
		self.scale_ky *= ZOOM
		self.scale_by = self.scale_by*ZOOM - sy*(ZOOM-1)
	def zoomout(self, sx,sy):
		ZOOM = 1.3
		self.scale_kx /= ZOOM
		self.scale_bx = self.scale_bx/ZOOM + sx*(1-1/ZOOM)
		self.scale_ky /= ZOOM
		self.scale_by = self.scale_by/ZOOM + sy*(1-1/ZOOM)
	def zoommove(self, dx,dy):
		self.scale_bx += dx
		self.scale_by += dy

class Window:
	def __init__(self, problem_fname, solution_fname):
		self.problem = json.load( open(problem_fname) )
		initial_bonuses = {}
		try:
			solution_json = json.load( open(solution_fname) )
			self.dstp = solution_json['vertices']
			if 'bonuses' in solution_json and solution_json['bonuses']:
				for bonus in solution_json['bonuses']:
					initial_bonuses[ bonus['bonus'] ] = bonus['problem']
		except:
			self.dstp = self.problem['figure']['vertices']

		srcp = self.problem['figure']['vertices']
		self.dstp = [(x,y) for x,y in self.dstp] # make a copy, so we could edit it

		# Remember solution fname so we could save a better solution to it (strip digits at the end)
		# (strip suffix after ".json" if any)
		if solution_fname:
			self.solution_basefname = solution_fname.split(".json")[0] + ".json"
		else:
			self.solution_basefname = "solution.json"

		# convert bonuses to dict (it's easier to use)
		self.bonuses = {}
		if 'bonuses' in self.problem:
			for bonus in self.problem['bonuses']:
				bx,by = bonus['position']
				self.bonuses.setdefault( (bx,by), [] ).append( (bonus['bonus'],bonus['problem']) )

		# Calculating scale
		TARGETWIDTH, TARGETHEIGHT, RIGHTWIDTH = 800, 800, 200
		self.scale = Scale(TARGETWIDTH, TARGETHEIGHT, self.problem, self.dstp)

		self.root = Tk.Tk()
		self.status = Tk.Label(self.root, text="", bd=1, relief=Tk.SUNKEN, anchor=Tk.W)
		self.canvas = Tk.Canvas(self.root, width=TARGETWIDTH, height=TARGETHEIGHT, bg='black')
		rightFrame = Tk.Frame(self.root)
		self.stretchedLabel = Tk.Message(rightFrame, text="Overstretched", width=RIGHTWIDTH, anchor=Tk.W)
		self.holedLabel = Tk.Message(rightFrame, text="Out of hole", width=RIGHTWIDTH, anchor=Tk.W)
		self.dislikesLabel = Tk.Message(rightFrame, text="Dislikes", width=RIGHTWIDTH, anchor=Tk.W)
		self.origdislikesLabel = Tk.Message(rightFrame, text="", width=RIGHTWIDTH, anchor=Tk.W)
		self.saveddislikesLabel = Tk.Message(rightFrame, text="", width=RIGHTWIDTH, anchor=Tk.W)
		self.collectedLabel = Tk.Message(rightFrame, text="", width=RIGHTWIDTH, anchor=Tk.W)

		self.autounstretchVar = Tk.IntVar(value=0)
		unstretchCheck = Tk.Checkbutton(rightFrame, text="Auto-Unstretch", variable=self.autounstretchVar, anchor=Tk.W)
		self.guihint(unstretchCheck, "Auto-unstretch overstretched edges")
		self.autostuffVar = Tk.IntVar(value=0)
		stuffCheck = Tk.Checkbutton(rightFrame, text="Auto-Stuff", variable=self.autostuffVar, anchor=Tk.W)
		self.guihint(stuffCheck, "Move outside figure points towards closest hole boundary")
		self.autofillVar = Tk.IntVar(value=0)
		fillCheck = Tk.Checkbutton(rightFrame, text="Auto-Fill", variable=self.autofillVar, anchor=Tk.W)
		self.guihint(fillCheck, "Each hole vertext pulls closest figure vertex")

		debugLabel = Tk.Message(rightFrame, text="Debug options:", width=RIGHTWIDTH, anchor=Tk.W)
		self.unstretchallVar = Tk.IntVar(value=0)
		unstretchallCheck = Tk.Checkbutton(rightFrame, text="Unstretch all", variable=self.unstretchallVar, anchor=Tk.W)
		self.guihint(unstretchallCheck, "Unstretch all edges, not just over/understretched ones (needs Auto-Unstretch)")
		self.stickyholeVar = Tk.IntVar(value=1)
		stickyholeCheck = Tk.Checkbutton(rightFrame, text="Sticky hole vertex", variable=self.stickyholeVar, anchor=Tk.W)
		self.guihint(stickyholeCheck, "Don't unstretch figure vertex if it's in the hole vertex")
		self.localunstretchVar = Tk.IntVar(value=0)
		localunstretchCheck = Tk.Checkbutton(rightFrame, text="Local auto-unstretch", variable=self.localunstretchVar, anchor=Tk.W)
		self.guihint(localunstretchCheck, "When Auto-Unstretching, unstretch only edges of vertex moved by mouse (VERY UNSTABLE)")
		self.intunstretchVar = Tk.IntVar(value=0)
		intunstretchCheck = Tk.Checkbutton(rightFrame, text="Integer unstretch", variable=self.intunstretchVar, anchor=Tk.W)
		self.guihint(intunstretchCheck, "When auto-unstretching, round coords to integer (VERY UNSTABLE)")
		self.intdragVar = Tk.IntVar(value=1)
		intdragCheck = Tk.Checkbutton(rightFrame, text="Integer drag", variable=self.intdragVar, anchor=Tk.W)
		self.guihint(intdragCheck, "Snap mousedragged vertex to integer coords")

		bonusesLabel = Tk.Message(rightFrame, text="Bonus tweaks:", width=RIGHTWIDTH, anchor=Tk.W)
		self.bonusattractVar = Tk.IntVar(value=0)
		bonusattractCheck = Tk.Checkbutton(rightFrame, text="Auto-attract bonuses", variable=self.bonusattractVar, anchor=Tk.W)
		self.guihint(bonusattractCheck, "Bonuses pull closest figure vertex (like Auto-Fill)")
		self.bonusstickyVar = Tk.IntVar(value=1)
		bonusstickyCheck = Tk.Checkbutton(rightFrame, text="Sticky bonus", variable=self.bonusstickyVar, anchor=Tk.W)
		self.guihint(bonusstickyCheck, "Don't unstretch figure vertex if it's on the bonus")
		bonusGLabel = Tk.Message(rightFrame, text="Have GLOBALIST from PID\n(empty == no bonus):", width=RIGHTWIDTH, anchor=Tk.W)
		self.bonusGIDVar = Tk.StringVar(value=initial_bonuses.get('GLOBALIST',''))
		bonusGIDText = Tk.Entry(rightFrame, textvariable = self.bonusGIDVar)
		bonusWLabel = Tk.Message(rightFrame, text="Have WALLHACK from PID\n(empty == no bonus):", width=RIGHTWIDTH, anchor=Tk.W)
		self.bonusWIDVar = Tk.StringVar(value=initial_bonuses.get('WALLHACK',''))
		bonusWIDText = Tk.Entry(rightFrame, textvariable = self.bonusWIDVar)
		bonusSLabel = Tk.Message(rightFrame, text="Have SUPERFLEX from PID\n(empty == no bonus):", width=RIGHTWIDTH, anchor=Tk.W)
		self.bonusSIDVar = Tk.StringVar(value=initial_bonuses.get('SUPERFLEX',''))
		bonusSIDText = Tk.Entry(rightFrame, textvariable = self.bonusSIDVar)

		mirrorButton = Tk.Button(rightFrame, text="Mirror", command=self.mirror_clicked)
		self.guihint(mirrorButton, "Mirror the figure horizontally")
		spreadButton = Tk.Button(rightFrame, text="Spread", command=self.spread_clicked)
		self.guihint(spreadButton, "Spread all points (only useful with Auto-Unstretch)")
		search1Button = Tk.Button(rightFrame, text="Search1", command=self.search1_clicked)
		self.guihint(search1Button, "Find figure edges with length same as hole edge (almost useless)")
		search2Button = Tk.Button(rightFrame, text="Search2", command=self.search2_clicked)
		self.guihint(search2Button, "Find figure vertexes with edges matching hole edges (almost useless)")
		roundButton = Tk.Button(rightFrame, text="Round to int", command=self.round_clicked)
		self.guihint(roundButton, "Round all floating point coords to closest integer value")
		self.outfnameVar = Tk.StringVar()
		outfnameText = Tk.Entry(rightFrame, textvariable = self.outfnameVar)
		saveButton = Tk.Button(rightFrame, text="Save", command=self.save_clicked)

		helpLabel = Tk.Message(rightFrame, text="Mouse buttons:", width=RIGHTWIDTH, anchor=Tk.W)
		helpLabel['text'] += "\n - Left - drag"
		helpLabel['text'] += "\n - Middle - move"
		helpLabel['text'] += "\n - Right - rotate"
		helpLabel['text'] += "\n - Wheel - zoom"

		self.status.pack(side=Tk.BOTTOM, fill=Tk.X)
		rightFrame.pack(side=Tk.RIGHT, fill=Tk.Y)
		self.canvas.pack(side=Tk.TOP, fill=Tk.BOTH, expand=True)
		self.stretchedLabel.pack(side=Tk.TOP, fill=Tk.X)
		self.holedLabel.pack(side=Tk.TOP, fill=Tk.X)
		self.dislikesLabel.pack(side=Tk.TOP, fill=Tk.X)
		self.origdislikesLabel.pack(side=Tk.TOP, fill=Tk.X)
		self.saveddislikesLabel.pack(side=Tk.TOP, fill=Tk.X)
		self.collectedLabel.pack(side=Tk.TOP, fill=Tk.X)
		unstretchCheck.pack(side=Tk.TOP, fill=Tk.X)
		stuffCheck.pack(side=Tk.TOP, fill=Tk.X)
		fillCheck.pack(side=Tk.TOP, fill=Tk.X)
		debugLabel.pack(side=Tk.TOP, fill=Tk.X)
		unstretchallCheck.pack(side=Tk.TOP, fill=Tk.X)
		stickyholeCheck.pack(side=Tk.TOP, fill=Tk.X)
		localunstretchCheck.pack(side=Tk.TOP, fill=Tk.X)
		intunstretchCheck.pack(side=Tk.TOP, fill=Tk.X)
		intdragCheck.pack(side=Tk.TOP, fill=Tk.X)
		if self.bonuses:
			bonusesLabel.pack(side=Tk.TOP, fill=Tk.X)
			bonusattractCheck.pack(side=Tk.TOP, fill=Tk.X)
			bonusstickyCheck.pack(side=Tk.TOP, fill=Tk.X)
			bonusGLabel.pack(side=Tk.TOP, fill=Tk.X)
			bonusGIDText.pack(side=Tk.TOP, fill=Tk.X)
			bonusWLabel.pack(side=Tk.TOP, fill=Tk.X)
			bonusWIDText.pack(side=Tk.TOP, fill=Tk.X)
			bonusSLabel.pack(side=Tk.TOP, fill=Tk.X)
			bonusSIDText.pack(side=Tk.TOP, fill=Tk.X)
		mirrorButton.pack(side=Tk.TOP, fill=Tk.X)
		spreadButton.pack(side=Tk.TOP, fill=Tk.X)
		search1Button.pack(side=Tk.TOP, fill=Tk.X)
		search2Button.pack(side=Tk.TOP, fill=Tk.X)
		roundButton.pack(side=Tk.TOP, fill=Tk.X)
		outfnameText.pack(side=Tk.TOP, fill=Tk.X)
		saveButton.pack(side=Tk.TOP, fill=Tk.X)
		helpLabel.pack(side=Tk.BOTTOM, fill=Tk.X)

		# Help: about bonuses (adding to bottom, thus reverse order)
		for bname in reversed(BONUSCOLOR.keys()):
			Tk.Message(rightFrame, text=" - "+bname, width=RIGHTWIDTH, anchor=Tk.W, fg=BONUSCOLOR[bname])\
			    .pack(side=Tk.BOTTOM, fill=Tk.X)
		Tk.Message(rightFrame, text="Known bonuses:", width=RIGHTWIDTH, anchor=Tk.W).pack(side=Tk.BOTTOM, fill=Tk.X)

		self.canvas.bind("<Motion>", lambda ev: self.status.configure( text="(%d,%d)" % self.scale.getpxy(ev.x,ev.y) ))
		self.canvas.bind("<Button-1>", lambda ev: self.b1down(*self.scale.getpxy(ev.x,ev.y)))
		self.canvas.bind("<B1-Motion>", lambda ev: self.b1move(*self.scale.getpxy(ev.x,ev.y)))
		self.canvas.bind("<ButtonRelease-1>", lambda ev: self.b1up(*self.scale.getpxy(ev.x,ev.y)))
		self.canvas.bind("<Button-2>", lambda ev: self.b2down(*self.scale.getpxy(ev.x,ev.y)))
		self.canvas.bind("<B2-Motion>", lambda ev: self.b2move(*self.scale.getpxy(ev.x,ev.y)))
		self.canvas.bind("<Button-3>", lambda ev: self.b3down(*self.scale.getpxy(ev.x,ev.y)))
		self.canvas.bind("<B3-Motion>", lambda ev: self.b3move(*self.scale.getpxy(ev.x,ev.y)))
		self.canvas.bind("<Button-4>", lambda ev: [self.scale.zoomin(ev.x,ev.y),self.redraw_initial_canvas()])
		self.canvas.bind("<Button-5>", lambda ev: [self.scale.zoomout(ev.x,ev.y),self.redraw_initial_canvas()])

		self.dragidx = None

		## Draw lines, calculate stats
		self.redraw_initial_canvas()
		self.update_lines_and_stats()

		self.root.after(100, self.ontimer)
		self.root.mainloop()

	# gui shortcut - these two lines were used too often:
	def guihint(self, guielem, text):
		guielem.bind("<Enter>", lambda ev: self.status.configure(text=text) )
		guielem.bind("<Leave>", lambda ev: self.status.configure(text="") )

	def redraw_initial_canvas(self):
		self.canvas.delete("all")
		# draw bonuses first, so that they're behind everything
		for bonus in self.problem['bonuses']:
			color = BONUSCOLOR.get( bonus['bonus'], '#555' )
			sx,sy = self.scale.getsxy(*bonus['position'])
			self.canvas.create_oval(sx-9,sy-9,sx+9,sy+9, fill=color)

		# hole lines:
		x1,y1 = self.problem['hole'][-1]
		for x2,y2 in self.problem['hole']:
			sx1,sy1 = self.scale.getsxy(x1,y1) # TODO: optimize
			sx2,sy2 = self.scale.getsxy(x2,y2)
			self.canvas.create_line(sx1, sy1, sx2, sy2, fill='gray', width=4)
			x1,y1 = x2,y2
		# pose lines - just add some lines, we'll update them later :
		self.lineids = [ self.canvas.create_line( 1,1,2,2 ) for _ in self.problem['figure']['edges'] ]
		# hole points:
		for hx,hy in self.problem['hole']:
			sx,sy = self.scale.getsxy(hx,hy)
			#self.canvas.create_rectangle( (sx,sy,sx,sy), outline="purple" )
			#self.canvas.create_rectangle( (sx-1,sy-1,sx+1,sy+1), outline="#f29", fill="#f29" )
			self.canvas.create_polygon( [sx-1,sy, sx,sy+1, sx+1,sy, sx,sy-1], outline="#f29" )

	def update_lines_and_stats(self):
		srcp = self.problem['figure']['vertices']
		# Check pose lines, update stats:
		stats_overstretched, stats_outofhole = 0, 0

		global_epsilon_valid = None
		if self.bonusGIDVar.get() != "": # We have a GLOBALIST bonus, use different math:
			total_epsilon = 0
			for p1,p2 in self.problem['figure']['edges']:
				srcd, dstd = sqdist(srcp[p1],srcp[p2]), sqdist(self.dstp[p1],self.dstp[p2])
				total_epsilon += 1000000*abs(dstd*1.0/srcd - 1)
			global_epsilon_valid = total_epsilon <= (self.problem['epsilon']+.0000001) * len(self.problem['figure']['edges'])

		global_whitelist_vertex = None
		if self.bonusWIDVar.get() != "": # We have WALLHACK, check if there's 1 sticky vertex
			count_out_of_hole = 0
			for i in xrange( len(self.dstp) ):
				px,py = self.dstp[i]
				if not point_in_hole( self.problem['hole'], (px,py) ):
					count_out_of_hole += 1
					global_whitelist_vertex = i
			if count_out_of_hole > 1: # only one whitelist is allowed
				global_whitelist_vertex = None

		for i in xrange( len(self.problem['figure']['edges']) ):
			lineid = self.lineids[i]
			p1, p2 = self.problem['figure']['edges'][i]

			# check stretching
			srcd, dstd = sqdist(srcp[p1],srcp[p2]), sqdist(self.dstp[p1],self.dstp[p2])
			epsilon_valid = 1000000*abs(dstd*1.0/srcd - 1) <= self.problem['epsilon']+.0000001
			if not epsilon_valid: stats_overstretched += 1

			# check hole
			if p1 == global_whitelist_vertex or p2 == global_whitelist_vertex:
				hole_valid = True
			else:
				hole_valid = line_in_hole(self.problem['hole'], self.dstp[p1],self.dstp[p2])
			if not hole_valid: stats_outofhole += 1

			# if we have GLOBALIST bonus - use a single global color:
			if global_epsilon_valid is not None:
				epsilon_valid = global_epsilon_valid

			# update the line
			sx1,sy1 = self.scale.getsxy( *self.dstp[p1] )
			sx2,sy2 = self.scale.getsxy( *self.dstp[p2] )
			self.canvas.coords(lineid, sx1, sy1, sx2, sy2)
			color = ['red','orange','yellow','green'][ epsilon_valid + hole_valid*2 ]
			self.canvas.itemconfig(lineid, fill=color, width=(3 if dstd<=srcd else 1) )

		# Count the dislikes
		dislikes = sum( min( sqdist(hp,p) for p in self.dstp ) for hp in self.problem['hole'] )

		solution_is_valid = stats_outofhole==0
		if global_epsilon_valid is None:
			if self.bonusSIDVar.get() == "":
				solution_is_valid = solution_is_valid and (stats_overstretched == 0)
			else:
				solution_is_valid = solution_is_valid and (stats_overstretched <= 1)
		else:
			solution_is_valid = solution_is_valid and global_epsilon_valid

		# Show stats
		self.stretchedLabel['text'] = "Overstretched edges: %d" % stats_overstretched
		self.stretchedLabel['foreground'] = "green" if stats_overstretched <= (self.bonusSIDVar.get()!="") else "red"
		self.holedLabel['text'] = "Out of hole edges: %d" % stats_outofhole
		self.holedLabel['foreground'] = "green" if stats_outofhole == 0 else "red"
		self.dislikesLabel['text'] = "Dislikes: %d" % dislikes

		if self.origdislikesLabel['text'] == "":
			if solution_is_valid:
				self.origdislikesLabel['text'] = "Orig.dislikes: %d" % dislikes
			else:
				self.origdislikesLabel['text'] = " " # some non-empty value - a mark that it's set

		# Show collected bonuses
		collected = []
		for px,py in self.dstp:
			collected.extend( self.bonuses.get( (px,py), [] ) )
		if collected:
			self.collectedLabel['text'] = "Collected:\n" + "\n".join( "%s for PID %s" % (name,pid) for name,pid in collected )
		else:
			self.collectedLabel['text'] = ""

		# Last dislikes (for "Save" button), negative, if the solution is not valid
		self.last_dislikes_str = "" if solution_is_valid else "-"
		self.last_dislikes_str += "%d" % dislikes
		if self.solution_basefname:
			bonusstr = ""
			if self.bonusGIDVar.get() != "": bonusstr += "G" + self.bonusGIDVar.get()
			if self.bonusWIDVar.get() != "": bonusstr += "W" + self.bonusWIDVar.get()
			if self.bonusSIDVar.get() != "": bonusstr += "S" + self.bonusSIDVar.get()

			suffix = self.last_dislikes_str
			if bonusstr:
				suffix = bonusstr + "=" + suffix
			if collected:
				suffix += "-" + "-".join( "%s%s" % (name[0],pid) for name,pid in collected )
			self.outfnameVar.set(self.solution_basefname + "." + suffix)

	# Drag vertex
	def b1down(self, px, py):
		# find the point closest to these coords
		self.dragidx, mindist, bigdist = None, None, self.scale.noticeable_distance()
		for i in xrange( len(self.dstp) ):
			x, y = self.dstp[i]
			d = max( abs(x-px), abs(y-py) )
			if d < bigdist and ( mindist is None or mindist > d ):
				mindist = d
				self.dragidx = i
		if self.dragidx is None: # clicked on empty space - assume zoom-move
			self.zoommove = self.scale.getsxy(px,py)
		self.b1move(px,py) # update position of the point we've dragged
	def b1move(self, px, py):
		if self.dragidx is not None:
			if self.intdragVar.get():
				px,py = int(px+0.5), int(py+0.5)
			self.dstp[self.dragidx] = (px,py)
			self.update_lines_and_stats()
		else: # zoom-move
			prevsx,prevsy = self.zoommove
			sx,sy = self.scale.getsxy(px,py)
			if sx != prevsx or sy != prevsy:
				self.scale.zoommove(sx-prevsx,sy-prevsy)
				self.redraw_initial_canvas()
			self.zoommove = sx,sy
	def b1up(self, px, py):
		self.dragidx = None

	# Move figure
	def b2down(self, px, py):
		self.movex, self.movey = px, py
	def b2move(self, px, py):
		dx, dy = px-self.movex, py-self.movey
		for i in xrange( len(self.dstp) ):
			x,y = self.dstp[i]
			self.dstp[i] = x+dx, y+dy
		self.movex, self.movey = px, py
		self.update_lines_and_stats()

	# Rotate figure
	def b3down(self, px, py):
		# don't let users click in the middle of the screen
		midx, midy = self.scale.getmidpxy()
		bigdist = self.scale.noticeable_distance()
		if abs(px-midx) < bigdist and abs(py-midy) < bigdist:
			self.movex, self.movey = None, None
		else:
			self.movex, self.movey = px, py
	def b3move(self, px, py):
		if self.movex is None or self.movey is None:
			return
		midx, midy = self.scale.getmidpxy()
		bigdist = self.scale.noticeable_distance()
		if abs(px-midx) < bigdist and abs(py-midy) < bigdist:
			return # too close to the center, skip calculations
		angle = math.atan2(py-midy, px-midx) - math.atan2(self.movey-midy, self.movex-midx)
		for i in xrange( len(self.dstp) ):
			x,y = self.dstp[i]
			vx, vy = x-midx, y-midy
			vx, vy = vx*math.cos(angle)-vy*math.sin(angle), vx*math.sin(angle)+vy*math.cos(angle)
			self.dstp[i] = vx+midx, vy+midy
		self.movex, self.movey = px, py
		self.update_lines_and_stats()

	# Unstretch overstretched/understretched edges:
	def unstretch_edge_1step(self, p1,p2, srcd,dstd, dontmove_list, round_to_int):
		x1,y1 = self.dstp[p1]
		x2,y2 = self.dstp[p2]
		mx,my = (x1+x2)/2.0, (y1+y2)/2.0
		tx1, ty1 = mx+(x1-mx)*srcd/dstd, my+(y1-my)*srcd/dstd
		tx2, ty2 = mx+(x2-mx)*srcd/dstd, my+(y2-my)*srcd/dstd
		if (x1,y1) not in dontmove_list:
			self.dstp[p1] = move_point( (tx1,ty1), (x1,y1), UNSTRETCH_SPEED, round_to_int )
		if (x2,y2) not in dontmove_list:
			self.dstp[p2] = move_point( (tx2,ty2), (x2,y2), UNSTRETCH_SPEED, round_to_int )
	def unstretch_1step(self):
		srcp = self.problem['figure']['vertices']
		round_to_int = self.intunstretchVar.get()
		dontmove_list = set()
		if self.stickyholeVar.get():
			dontmove_list.update( (x,y) for x,y in self.problem['hole'] )
		if self.bonusstickyVar.get():
			dontmove_list.update( list(self.bonuses) )
		if self.bonusGIDVar.get() != "": # have GLOBALIST - unstretch just the most stretched edge
			totaleps, maxeps, maxp1,maxp2, maxsrcd,maxdstd = 0, None, None,None, None,None
			for p1,p2 in self.problem['figure']['edges']:
				if self.localunstretchVar.get() and self.dragidx not in (p1,p2): continue
				srcd, dstd = sqdist(srcp[p1],srcp[p2]), sqdist(self.dstp[p1],self.dstp[p2])
				if dstd == 0: dstd = 0.1 # avoid zero division
				e = 1000000*abs(dstd*1.0/srcd - 1)
				totaleps += e
				if maxeps is None or maxeps < e:
					maxeps, maxp1,maxp2, maxsrcd,maxdstd = e, p1,p2, srcd,dstd
			if totaleps > (self.problem['epsilon']+.0000001) * len(self.problem['figure']['edges']):
				self.unstretch_edge_1step( maxp1,maxp2, maxsrcd,maxdstd, dontmove_list, round_to_int )
		else: # unstrech all the edges
			for p1,p2 in self.problem['figure']['edges']:
				if self.localunstretchVar.get() and self.dragidx not in (p1,p2): continue
				srcd, dstd = sqdist(srcp[p1],srcp[p2]), sqdist(self.dstp[p1],self.dstp[p2])
				if dstd == 0: dstd = 0.1 # avoid zero division
				e = dstd*1.0/srcd - 1
				if abs(e)*1000000 > self.problem['epsilon']+.0000001 or self.unstretchallVar.get():
					self.unstretch_edge_1step( p1,p2, srcd,dstd, dontmove_list, round_to_int )

	# Stuff elements that are outside of the figure into the figure
	def stuff_1step(self):
		# stuff each point towards the closest edge
		for i in xrange( len(self.dstp) ):
			px,py = self.dstp[i]
			if not point_in_hole(self.problem['hole'], (px,py)):
				# find closest hole edge
				mindist,closest = None,None
				hx1,hy1 = self.problem['hole'][-1]
				for hx2,hy2 in self.problem['hole']:
					cp = closest_line_point( (hx1,hy1,hx2,hy2), (px,py) )
					d = sqdist( (px,py), cp )
					if mindist is None or mindist > d:
						mindist,closest = d,cp
					hx1,hy1 = hx2,hy2
				self.dstp[i] = move_point( closest, (px,py), STUFF_SPEED )

	# Try to fill the hole with the figure - move it closer to hole vertexes
	def find_closest_idx(self, px,py):
		mindist,iclosest = None,None
		for i in xrange( len(self.dstp) ):
			d = sqdist( self.dstp[i], (px,py) )
			if mindist is None or mindist > d:
				mindist,iclosest = d,i
		return iclosest
	def fill_1step(self):
		for hx,hy in self.problem['hole']:
			iclosest = self.find_closest_idx(hx,hy)
			self.dstp[iclosest] = move_point( (hx,hy), self.dstp[iclosest], FILL_SPEED )
	def bonus_fill_1step(self):
		for bx,by in self.bonuses:
			iclosest = self.find_closest_idx(bx,by)
			self.dstp[iclosest] = move_point( (bx,by), self.dstp[iclosest], FILL_SPEED )

	def mirror_clicked(self):
		mx, my = self.scale.getmidpxy()
		for i in xrange( len(self.dstp) ):
			px, py = self.dstp[i]
			self.dstp[i] = 2*mx-px, py
		self.update_lines_and_stats()

	def spread_clicked(self):
		minx = min( px for px,py in self.dstp )
		miny = min( py for px,py in self.dstp )
		maxx = max( px for px,py in self.dstp )
		maxy = max( py for px,py in self.dstp )
		midx, midy = (minx+maxx)/2.0, (miny+maxy)/2.0
		centerx, centery = self.scale.getmidpxy()
		for i in xrange( len(self.dstp) ):
			px, py = self.dstp[i]
			self.dstp[i] = (px-midx)*2+centerx, (py-midy)*2+centery
		self.update_lines_and_stats()

	def search1_clicked(self):
		srcp = self.problem['figure']['vertices']
		sqlens = {}
		for i in xrange( len(self.problem['figure']['edges']) ):
			p1,p2 = self.problem['figure']['edges'][i]
			sqlens.setdefault( sqdist(srcp[p1],srcp[p2]), [] ).append(i)
		hx1,hy1 = self.problem['hole'][-1]
		for hx2,hy2 in self.problem['hole']:
			d = sqdist( (hx1,hy1), (hx2,hy2) )
			ddelta = int((d*self.problem['epsilon']+1)/1000000)
			mind, maxd, candidates = d-ddelta, d+ddelta, []
			for d in xrange(mind,maxd+1):
				if d in sqlens:
					candidates.extend( sqlens[d] )
			if len(candidates) == 1:
				print("Search1: Found candidate for %d,%d,%d,%d" % (hx1,hy1,hx2,hy2)) # DEBUG
				p1,p2 = self.problem['figure']['edges'][ candidates[0] ]
				px1,py1 = self.dstp[p1]
				px2,py2 = self.dstp[p2]
				mx,my = (px1+px2)/2.0, (py1+py2)/2.0
				tgtx,tgty = (hx1+hx2)/2.0, (hy1+hy2)/2.0
				self.dstp[p1] = px1-mx+tgtx, py1-my+tgty
				self.dstp[p2] = px2-mx+tgtx, py2-my+tgty
			hx1,hy1=hx2,hy2

	def search2_clicked(self):
		srcp = self.problem['figure']['vertices']
		G = {}
		for p1,p2 in self.problem['figure']['edges']:
			G.setdefault(p1,set()).add(p2)
			G.setdefault(p2,set()).add(p1)
		sqlens2 = {}
		for pm in G:
			for p1 in G[pm]:
				d1 = sqdist(srcp[pm],srcp[p1])
				for p2 in G[pm]:
					if p2 <= p1: continue
					d2 = sqdist(srcp[pm],srcp[p2])
					sqlens2.setdefault( (d1,d2), [] ).append(pm)

		hx0,hy0 = self.problem['hole'][-2]
		hx1,hy1 = self.problem['hole'][-1]
		d1 = sqdist( (hx0,hy0), (hx1,hy1) )
		ddelta1 = int((d1*self.problem['epsilon']+1)/1000000)
		mind1, maxd1 = d1-ddelta1, d1+ddelta1
		for hx2,hy2 in self.problem['hole']:
			d2 = sqdist( (hx1,hy1), (hx2,hy2) )
			ddelta2 = int((d2*self.problem['epsilon']+1)/1000000)
			mind2, maxd2 = d2-ddelta2, d2+ddelta2
			candidates = []
			for td1 in xrange(mind1,maxd1+1):
				for td2 in xrange(mind2,maxd2+1):
					if (td1,td2) in sqlens2:
						candidates.extend( sqlens2[td1,td2] )
					if (td2,td1) in sqlens2:
						candidates.extend( sqlens2[td2,td1] )
			#print("%d,%d : %d candidates" % (hx1,hy1,len(candidates)) ) # DEBUG
			if len(candidates) == 1:
				print("Search2: Found candidate for %d,%d" % (hx1,hy1)) # DEBUG
				self.dstp[ candidates[0] ] = hx1,hy1
			hx1,hy1,mind1,maxd1=hx2,hy2,mind2,maxd2

	# Round all coords to integer
	def round_clicked(self):
		for i in xrange( len(self.dstp) ):
			px,py = self.dstp[i]
			self.dstp[i] = int(px+0.5), int(py+0.5)
		self.update_lines_and_stats()

	# Save current solution to a file, so we could load it later
	def save_clicked(self):
		self.saveddislikesLabel['text'] = "Saved dislikes: %s" % self.last_dislikes_str
		data = {"vertices":self.dstp}
		used_bonuses = []
		if self.bonusGIDVar.get() != "":
			used_bonuses.append( {'bonus':"GLOBALIST",'problem':self.bonusGIDVar.get()} )
		if self.bonusWIDVar.get() != "":
			used_bonuses.append( {'bonus':"WALLHACK",'problem':self.bonusWIDVar.get()} )
		if self.bonusSIDVar.get() != "":
			used_bonuses.append( {'bonus':"SUPERFLEX",'problem':self.bonusSIDVar.get()} )
		if used_bonuses:
			data['bonuses'] = used_bonuses
		f = open( self.outfnameVar.get(), "w" )
		json.dump( data, f )
		f.close()

	def ontimer(self):
		if self.autounstretchVar.get():
			self.unstretch_1step()
		if self.autostuffVar.get():
			self.stuff_1step()
		if self.autofillVar.get():
			self.fill_1step()
		if self.bonusattractVar.get() and self.bonuses:
			self.bonus_fill_1step()
		self.update_lines_and_stats()
		self.root.after(AUTO_PERIOD, self.ontimer)

if __name__ == "__main__":
	problem_fname = sys.argv[1]
	solution_fname = sys.argv[2] if len(sys.argv) > 2 else None
	Window(problem_fname, solution_fname)
