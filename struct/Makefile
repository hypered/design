# Directory variables
SLAB_BASE_DIR := templates
SLAB_DIRS := templates/notes templates/partials templates/prototypes templates/specimens templates/index.slab templates/struct.slab
SCSS_DIR := scss/struct
STATIC_DIR := static

HTML_OUTPUT_BASE := _site
HTML_OUTPUT_DIR := _site/notes
SCSS_OUTPUT_DIR := _site/static/css/struct
STATIC_OUTPUT_DIR := _site/static

# Find sources
SLAB_SOURCES := $(shell find $(SLAB_DIRS) -name '*.slab')
SCSS_SOURCES := $(shell find $(SCSS_DIR) -name '*.scss')
STATIC_FILES := $(shell find $(STATIC_DIR) -type f)

# Targets
HTML_FILES := $(patsubst %.slab,%.html,$(SLAB_SOURCES))
CSS_FILES := $(patsubst %.scss,%.css,$(SCSS_SOURCES))

HTML_TARGETS := $(addprefix $(HTML_OUTPUT_BASE)/,$(subst $(SLAB_BASE_DIR)/,,$(HTML_FILES)))
CSS_TARGETS := $(patsubst $(SCSS_DIR)/%.css,$(SCSS_OUTPUT_DIR)/%.css,$(CSS_FILES))
STATIC_TARGETS := $(patsubst $(STATIC_DIR)/%, $(STATIC_OUTPUT_DIR)/%, $(STATIC_FILES))

# Combined output targets
ALL_TARGETS := $(HTML_TARGETS) $(CSS_TARGETS) $(STATIC_TARGETS) $(STATIC_OUTPUT_DIR)/css/struct.css $(STATIC_OUTPUT_DIR)/css/min/struct.css

# Default goal
.PHONY: all
all: $(ALL_TARGETS)

# HTML generation rule
$(HTML_OUTPUT_BASE)/%.html: $(SLAB_BASE_DIR)/%.slab
	mkdir -p $(dir $@)
	slab render $< > $@

# SCSS to CSS rule
$(STATIC_OUTPUT_DIR)/css/%.css: scss/%.scss
	mkdir -p $(dir $@)
	sass --precision 4 $< > $@

# Combined CSS rule
$(STATIC_OUTPUT_DIR)/css/struct.css: scss/struct.scss
	mkdir -p $(dir $@)
	sass --precision 4 $< > $@

# Combined and minified CSS rule
$(STATIC_OUTPUT_DIR)/css/min/struct.css: scss/struct.scss
	mkdir -p $(dir $@)
	sass --precision 4 --style=compressed $< > $@

# Copy static files rule
$(STATIC_OUTPUT_DIR)/%: $(STATIC_DIR)/%
	mkdir -p $(dir $@)
	cp $< $@

# Serve the site
.PHONY: serve
serve:
	httpd -f -p 9000 -h _site
