data_dir = ../../data
work_dir = $(data_dir)/work/maciejjan
output_dir = $(data_dir)/processed/maciejjan

all: \
  $(output_dir)/names_to_vd17.fsm \
  $(output_dir)/vd17_to_vd17.txt

$(work_dir)/first_names/input.training:
	mkdir -p $(work_dir)/first_names
	csvcut -c first_name $(data_dir)/input/vd17_name_family.csv \
	| tail -n +2 | sed 's/[[:space:]]/\n/g' | sort | uniq -c \
	| awk '{ print $$2"\t"$$1; }' | sort -nr -k2,2 > $@

$(work_dir)/last_names/input.training:
	mkdir -p $(work_dir)/last_names
	csvcut -c last_name $(data_dir)/input/vd17_name_family.csv \
	| tail -n +2 | sed 's/[[:space:]]/\n/g' | sort | uniq -c \
	| awk '{ print $$2"\t"$$1; }' | sort -nr -k2,2 \
	| head -n 20000 > $@

$(work_dir)/first_names/config.ini:
	mkdir -p $(work_dir)/first_names
	cp morle-config.ini $@

$(work_dir)/last_names/config.ini:
	mkdir -p $(work_dir)/last_names
	cp morle-config.ini $@

$(work_dir)/first_names/graph.txt: \
  $(work_dir)/first_names/config.ini \
  $(work_dir)/first_names/input.training
	morle -d $(work_dir)/first_names preprocess

$(work_dir)/first_names/rules.fsm: \
  $(work_dir)/first_names/graph.txt
	morle -d $(work_dir)/first_names compile

$(work_dir)/last_names/graph.txt: \
  $(work_dir)/last_names/config.ini \
  $(work_dir)/last_names/input.training
	morle -d $(work_dir)/last_names preprocess

$(work_dir)/last_names/rules.fsm: \
  $(work_dir)/last_names/graph.txt
	morle -d $(work_dir)/last_names compile

$(work_dir)/last_names_lexicon.fsm:
	mkdir -p $(work_dir)
	csvcut -c last_name $(data_dir)/input/vd17_name_family.csv \
	| tail -n +2 | sed 's/[[:space:]]/\n/g' | sort -u \
	| hfst-strings2fst -j -o $@

$(work_dir)/full_names_vd17.txt:
	mkdir -p $(work_dir)
	csvcut -c first_name,last_name $(data_dir)/input/vd17_name_family.csv \
	| tail -n +2 | sed 's/,/ /; s/[[:space:]]/_/g; s/:/\\:/g;' | sort -u  > $@

$(work_dir)/full_names_lexicon.fsm: $(work_dir)/full_names_vd17.txt
	hfst-strings2fst -j -i $< -o $@
	
$(output_dir)/names_to_vd17.fsm: \
  $(work_dir)/first_names/rules.fsm \
  $(work_dir)/last_names/rules.fsm \
  $(work_dir)/last_names_lexicon.fsm \
  $(work_dir)/full_names_lexicon.fsm
	mkdir -p $(output_dir)
	hfst-xfst -F names_to_vd17.xfst

$(output_dir)/vd17_to_vd17.txt: \
  $(output_dir)/names_to_vd17.fsm \
  $(work_dir)/full_names_vd17.txt
	hfst-lookup -p $(output_dir)/names_to_vd17.fsm \
	  < $(work_dir)/full_names_vd17.txt > $@

