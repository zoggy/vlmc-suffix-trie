STOG=/home/guesdon/Personal/stog/stog
DEST_DIR=/home/guesdon/devel/vlmc-gh-pages
BASE_URL_OPTION=
STOG_OPTIONS=-d $(DEST_DIR) $(BASE_URL_OPTION) -v -v

build:
	$(MAKE) site

site:
	mkdir -p $(DEST_DIR)
	lessc less/style.less > $(DEST_DIR)/style.css
	$(STOG) --default-lang fr --package stog.disqus,stog-writing $(STOG_OPTIONS) . $(ONLY)

test:
	$(MAKE) DEST_DIR=/tmp/vlmc BASE_URL_OPTION="--site-url file:///tmp/vlmc" site

