all: slides

slides:
	pandoc -t dzslides \
				--embed-resources \
				--standalone \
	      --template template.html \
	      --highlight-style kate \
	      --metadata embed-css \
	       -s slides.md \
	       -o slides.html

with-notes:
	pandoc -t dzslides \
				--embed-resources \
				--standalone \
	      --template template.html \
	      --highlight-style breezedark \
	      --metadata display-notes \
	      --metadata embed-css \
	       -s slides.md \
	       -o slides.html

clean:
	-rm slides.html

.PHONY: all $(MAKECMDGOALS)
