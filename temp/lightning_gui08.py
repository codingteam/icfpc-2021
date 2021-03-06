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
		self.scale_kx = TARGETWIDTH * 0.95 / (maxx-minx)
		self.scale_ky = TARGETHEIGHT * 0.95 / (maxy-miny)
		# (maxx+minx)/2 === TARGETWIDTH/2
		# (maxx+minx)/2 * scale_kx + scale_bx == TARGETWIDTH/2
		# scale_bx = TARGETWIDTH/2 - (maxx+minx)/2 * scale_kx
		# scale_bx = TARGETWIDTH/2 - (maxx+minx)/2 * TARGETWIDTH * 0.95 / (maxx-minx)
		# scale_bx = TARGETWIDTH/2 * ( 1 - (maxx+minx) * 0.95 / (maxx-minx) )
		# scale_bx = TARGETWIDTH/2 * ( (maxx-minx) - (maxx+minx)*0.95 ) / (maxx-minx)
		# scale_bx = TARGETWIDTH/2 * ( maxx-minx - maxx*0.95-minx*0.95 ) / (maxx-minx)
		# scale_bx = TARGETWIDTH/2 * (maxx*0.05 - minx*1.05) / (maxx-minx)
		self.scale_bx = TARGETWIDTH/2 * ( 1 - (maxx+minx) * 0.95 / (maxx-minx) )
		self.scale_by = TARGETHEIGHT/2 * ( 1 - (maxy+miny) * 0.95 / (maxy-miny) )
		self.midpx, self.midpy = self.getpxy(TARGETWIDTH/2, TARGETHEIGHT/2)
	def getsxy(self, px, py):
		return px*self.scale_kx+self.scale_bx, py*self.scale_ky+self.scale_by
	def getpxy(self, sx, sy):
		return (sx-self.scale_bx)/self.scale_kx, (sy-self.scale_by)/self.scale_ky
	def noticeable_distance(self):
		return int( max( 20/self.scale_kx, 20/self.scale_ky, 1 ) )
	def getmidpxy(self):
		return self.midpx, self.midpy

class Window:
	def __init__(self, problem_fname, solution_fname):
		self.problem = json.load( open(problem_fname) )
		try:
			self.dstp = json.load( open(solution_fname) )['vertices']
		except:
			self.dstp = self.problem['figure']['vertices']

		srcp = self.problem['figure']['vertices']
		self.dstp = [(x,y) for x,y in self.dstp] # make a copy, so we could edit it

		# Remember solution fname so we could save a better solution to it (strip digits at the end)
		while solution_fname and len(solution_fname)>0 and solution_fname[-1] in "0123456789.-":
			solution_fname = solution_fname[:-1]
		self.solution_basefname = solution_fname

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

		self.autounstretchVar = Tk.IntVar(value=0)
		unstretchCheck = Tk.Checkbutton(rightFrame, text="Auto-Unstretch", variable=self.autounstretchVar, anchor=Tk.W)
		self.guihint(unstretchCheck, "Auto-unstretch overstretched edges")
		self.autostuffVar = Tk.IntVar(value=0)
		stuffCheck = Tk.Checkbutton(rightFrame, text="Auto-Stuff", variable=self.autostuffVar, anchor=Tk.W)
		self.guihint(stuffCheck, "Move outside figure points towards closest hole boundary")
		self.autofillVar = Tk.IntVar(value=0)
		fillCheck = Tk.Checkbutton(rightFrame, text="Auto-Fill", variable=self.autofillVar, anchor=Tk.W)
		self.guihint(fillCheck, "Each hole vertext pulls closest figure vertex")

		debugLabel = Tk.Message(rightFrame, text="Debug options", width=RIGHTWIDTH, anchor=Tk.W)
		self.unstretchallVar = Tk.IntVar(value=0)
		unstretchallCheck = Tk.Checkbutton(rightFrame, text="Unstretch all", variable=self.unstretchallVar, anchor=Tk.W)
		self.guihint(unstretchallCheck, "Unstretch all edges, not just over/understretched ones (needs Auto-Unstretch)")
		self.stickyholeVar = Tk.IntVar(value=1)
		stickyholeCheck = Tk.Checkbutton(rightFrame, text="Sticky hole vertex", variable=self.stickyholeVar, anchor=Tk.W)
		self.guihint(stickyholeCheck, "Don't unstretch figure vertex if it's in the hole vertex")
		self.intunstretchVar = Tk.IntVar(value=0)
		intunstretchCheck = Tk.Checkbutton(rightFrame, text="Integer unstretch", variable=self.intunstretchVar, anchor=Tk.W)
		self.guihint(intunstretchCheck, "When auto-unstretching, round coords to integer (VERY UNSTABLE)")
		self.intdragVar = Tk.IntVar(value=1)
		intdragCheck = Tk.Checkbutton(rightFrame, text="Integer drag", variable=self.intdragVar, anchor=Tk.W)
		self.guihint(intdragCheck, "Snap mousedragged vertex to integer coords")

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
		helpLabel['text'] += "\nLeft - drag"
		helpLabel['text'] += "\nMiddle - move"
		helpLabel['text'] += "\nRight - rotate"

		self.status.pack(side=Tk.BOTTOM, fill=Tk.X)
		rightFrame.pack(side=Tk.RIGHT, fill=Tk.Y)
		self.canvas.pack(side=Tk.TOP, fill=Tk.BOTH, expand=True)
		self.stretchedLabel.pack(side=Tk.TOP, fill=Tk.X)
		self.holedLabel.pack(side=Tk.TOP, fill=Tk.X)
		self.dislikesLabel.pack(side=Tk.TOP, fill=Tk.X)
		self.origdislikesLabel.pack(side=Tk.TOP, fill=Tk.X)
		self.saveddislikesLabel.pack(side=Tk.TOP, fill=Tk.X)
		unstretchCheck.pack(side=Tk.TOP, fill=Tk.X)
		stuffCheck.pack(side=Tk.TOP, fill=Tk.X)
		fillCheck.pack(side=Tk.TOP, fill=Tk.X)
		debugLabel.pack(side=Tk.TOP, fill=Tk.X)
		unstretchallCheck.pack(side=Tk.TOP, fill=Tk.X)
		stickyholeCheck.pack(side=Tk.TOP, fill=Tk.X)
		intunstretchCheck.pack(side=Tk.TOP, fill=Tk.X)
		intdragCheck.pack(side=Tk.TOP, fill=Tk.X)
		mirrorButton.pack(side=Tk.TOP, fill=Tk.X)
		spreadButton.pack(side=Tk.TOP, fill=Tk.X)
		search1Button.pack(side=Tk.TOP, fill=Tk.X)
		search2Button.pack(side=Tk.TOP, fill=Tk.X)
		roundButton.pack(side=Tk.TOP, fill=Tk.X)
		outfnameText.pack(side=Tk.TOP, fill=Tk.X)
		saveButton.pack(side=Tk.TOP, fill=Tk.X)
		helpLabel.pack(side=Tk.BOTTOM, fill=Tk.X)

		self.canvas.bind("<Motion>", lambda ev: self.status.configure( text="(%d,%d)" % self.scale.getpxy(ev.x,ev.y) ))
		self.canvas.bind("<Button-1>", lambda ev: self.b1down(*self.scale.getpxy(ev.x,ev.y)))
		self.canvas.bind("<B1-Motion>", lambda ev: self.b1move(*self.scale.getpxy(ev.x,ev.y)))
		self.canvas.bind("<Button-2>", lambda ev: self.b2down(*self.scale.getpxy(ev.x,ev.y)))
		self.canvas.bind("<B2-Motion>", lambda ev: self.b2move(*self.scale.getpxy(ev.x,ev.y)))
		self.canvas.bind("<Button-3>", lambda ev: self.b3down(*self.scale.getpxy(ev.x,ev.y)))
		self.canvas.bind("<B3-Motion>", lambda ev: self.b3move(*self.scale.getpxy(ev.x,ev.y)))

		## Draw lines, calculate stats
		# hole lines:
		x1,y1 = self.problem['hole'][-1]
		for x2,y2 in self.problem['hole']:
			sx1,sy1 = self.scale.getsxy(x1,y1) # TODO: optimize
			sx2,sy2 = self.scale.getsxy(x2,y2)
			self.canvas.create_line(sx1, sy1, sx2, sy2, fill='gray', width=5)
			x1,y1 = x2,y2
		# pose lines - just add some lines, we'll update them later :
		self.lineids = [ self.canvas.create_line( 1,1,2,2 ) for _ in self.problem['figure']['edges'] ]
		# hole points:
		for hx,hy in self.problem['hole']:
			sx,sy = self.scale.getsxy(hx,hy)
			self.canvas.create_rectangle( (sx,sy,sx,sy), outline="purple" )

		self.update_lines_and_stats()

		self.root.after(100, self.ontimer)
		self.root.mainloop()

	# gui shortcut - these two lines were used too often:
	def guihint(self, guielem, text):
		guielem.bind("<Enter>", lambda ev: self.status.configure(text=text) )
		guielem.bind("<Leave>", lambda ev: self.status.configure(text="") )

	def update_lines_and_stats(self):
		srcp = self.problem['figure']['vertices']
		# Check pose lines, update stats:
		stats_overstretched, stats_outofhole = 0, 0
		for i in xrange( len(self.problem['figure']['edges']) ):
			lineid = self.lineids[i]
			p1, p2 = self.problem['figure']['edges'][i]

			# check stretching
			srcd, dstd = sqdist(srcp[p1],srcp[p2]), sqdist(self.dstp[p1],self.dstp[p2])
			epsilon_valid = 1000000 * abs(dstd*1.0/srcd - 1) <= self.problem['epsilon']
			if not epsilon_valid: stats_overstretched += 1

			# check hole
			hole_valid = line_in_hole(self.problem['hole'], self.dstp[p1],self.dstp[p2])
			if not hole_valid: stats_outofhole += 1

			# update the line
			sx1,sy1 = self.scale.getsxy( *self.dstp[p1] )
			sx2,sy2 = self.scale.getsxy( *self.dstp[p2] )
			self.canvas.coords(lineid, sx1, sy1, sx2, sy2)
			color = ['red','orange','yellow','green'][ epsilon_valid + hole_valid*2 ]
			self.canvas.itemconfig(lineid, fill=color, width=(3 if dstd<=srcd else 1) )

		# Count the dislikes
		dislikes = sum( min( sqdist(hp,p) for p in self.dstp ) for hp in self.problem['hole'] )

		# Show stats
		self.stretchedLabel['text'] = "Overstretched edges: %d" % stats_overstretched
		self.stretchedLabel['foreground'] = "green" if stats_overstretched == 0 else "red"
		self.holedLabel['text'] = "Out of hole edges: %d" % stats_outofhole
		self.holedLabel['foreground'] = "green" if stats_outofhole == 0 else "red"
		self.dislikesLabel['text'] = "Dislikes: %d" % dislikes

		if self.origdislikesLabel['text'] == "":
			if stats_overstretched == 0 and stats_outofhole == 0:
				self.origdislikesLabel['text'] = "Orig.dislikes: %d" % dislikes
			else:
				self.origdislikesLabel['text'] = " " # some non-empty value - a mark that it's set

		# Last dislikes (for "Save" button), negative, if the solution is not valid
		self.last_dislikes_str = "" if stats_overstretched==0 and stats_outofhole==0 else "-"
		self.last_dislikes_str += "%d" % dislikes
		if self.solution_basefname:
			self.outfnameVar.set(self.solution_basefname + "." + self.last_dislikes_str )

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
		self.b1move(px,py) # update position of the point we've dragged
	def b1move(self, px, py):
		if self.dragidx is None: return
		if self.intdragVar.get():
			px,py = int(px+0.5), int(py+0.5)
		self.dstp[self.dragidx] = (px,py)
		self.update_lines_and_stats()

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
	def unstretch_1step(self):
		srcp = self.problem['figure']['vertices']
		round_to_int = self.intunstretchVar.get()
		dontmove_list = []
		if self.stickyholeVar.get():
			dontmove_list = set( (x,y) for x,y in self.problem['hole'] )
		for p1,p2 in self.problem['figure']['edges']:
			# check stretching
			srcd, dstd = sqdist(srcp[p1],srcp[p2]), sqdist(self.dstp[p1],self.dstp[p2])
			if dstd == 0: dstd = 0.1 # avoid zero division
			e = dstd*1.0/srcd - 1
			if abs(e)*1000000 > self.problem['epsilon'] or self.unstretchallVar.get():
				x1,y1 = self.dstp[p1]
				x2,y2 = self.dstp[p2]
				mx,my = (x1+x2)/2.0, (y1+y2)/2.0
				tx1, ty1 = mx+(x1-mx)*srcd/dstd, my+(y1-my)*srcd/dstd
				tx2, ty2 = mx+(x2-mx)*srcd/dstd, my+(y2-my)*srcd/dstd
				if (x1,y1) not in dontmove_list:
					self.dstp[p1] = move_point( (tx1,ty1), (x1,y1), UNSTRETCH_SPEED, round_to_int )
				if (x2,y2) not in dontmove_list:
					self.dstp[p2] = move_point( (tx2,ty2), (x2,y2), UNSTRETCH_SPEED, round_to_int )

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
	def fill_1step(self):
		for hx,hy in self.problem['hole']:
			mindist,iclosest = None,None
			for i in xrange( len(self.dstp) ):
				d = sqdist( self.dstp[i], (hx,hy) )
				if mindist is None or mindist > d:
					mindist,iclosest = d,i
			self.dstp[iclosest] = move_point( (hx,hy), self.dstp[iclosest], FILL_SPEED )

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
		f = open( self.outfnameVar.get(), "w" )
		json.dump( {"vertices":self.dstp}, f )
		f.close()

	def ontimer(self):
		if self.autounstretchVar.get():
			self.unstretch_1step()
		if self.autostuffVar.get():
			self.stuff_1step()
		if self.autofillVar.get():
			self.fill_1step()
		self.update_lines_and_stats()
		self.root.after(AUTO_PERIOD, self.ontimer)

if __name__ == "__main__":
	problem_fname = sys.argv[1]
	solution_fname = sys.argv[2] if len(sys.argv) > 2 else None
	Window(problem_fname, solution_fname)
