all: slides

slides:
	pandoc -t dzslides \
	      --template template.html \
	      --highlight-style kate \
	       -s slides.md \
	       -o slides.html

standalone:
	pandoc -t dzslides \
				--embed-resources \
				--standalone \
	      --template template.html \
	      --highlight-style kate \
	      --metadata embed-css \
	       -s slides.md \
	       -o slides-standalone.html

with-notes:
	pandoc -t dzslides \
				--embed-resources \
				--standalone \
	      --template template.html \
	      --highlight-style breezedark \
	      --metadata display-notes \
	      --metadata embed-css \
	       -s slides.md \
	       -o slides-standalone.html

clean:
	-rm slides.html
	-rm slides-standalone.html

.PHONY: all $(MAKECMDGOALS)
