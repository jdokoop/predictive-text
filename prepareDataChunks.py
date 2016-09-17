#-----------------------------------------------------
# Script to take in 3 files containing the training
# set for a predictive text algorithm and break them
# down into more manageable chunks of 10000 lines 
# each
#-----------------------------------------------------

# How many lines to keep from each source file?
trainingLength = 200000
fragmentLength = 10000

# First, read the text sample from web blogs
nline = 0
nfile = 0

with open("en_US.blogs.txt") as f:
	for line in f:
		if nline < trainingLength:

			if nline % fragmentLength == 0:
				fout = open('blogs.{}.txt'.format(nfile), 'w')
				nfile = nfile + 1
				
		fout.write(line)
		fout.write('\n')
		nline = nline + 1
f.close

# Now, the text from news sources
nline = 0
nfile = 0;

with open("en_US.news.txt") as f:
	for line in f:
		if nline < trainingLength:

			if nline % fragmentLength == 0:
				fout = open('news.{}.txt'.format(nfile), 'w')
				nfile = nfile + 1
				
		fout.write(line)
		fout.write('\n')
		nline = nline + 1
f.close

# Finally, the twitter text
nline = 0
nfile = 0;

with open("en_US.twitter.txt") as f:
	for line in f:
		if nline < trainingLength:

			if nline % fragmentLength == 0:
				fout = open('twitter.{}.txt'.format(nfile), 'w')
				nfile = nfile + 1
				
		fout.write(line)
		fout.write('\n')
		nline = nline + 1
f.close
