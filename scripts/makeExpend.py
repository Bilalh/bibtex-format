#!/usr/bin/env python

def make_year_to_ord(name,year,endnum,plus, base, val):
	for i in range(endnum):
		y = year+i*plus
		b = ordinal(base+i)
		print('\t"{} {}": "{}",'.format(name,y, val.format(b) ))

def ordinal(n):
	return "%d%s" % (n,"tsnrhtdd"[(n/10%10!=1)*(n%10<4)*n%10::4])


ls = [
	("CP",      1995, 25, 1, 1, "The {} International Conference on Principles and Practice of Constraint Programming")
	,("CPAIOR", 1974, 15, 1, 1, "The {} International Conference on Integration of AI and OR Techniques in Constraint Programming for Combinatorial Optimization Problems")
	,("ECAI",   1974, 27, 2, 1, "The {} Eureopean Conference on Artificial Intelligence")
	,("IJCAI",  1969, 27, 2, 1, "The {} International Joint Conference on Artificial Intelligence")
	]


for fields in ls:
	make_year_to_ord(*fields)
