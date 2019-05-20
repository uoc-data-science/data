all: run_with_r

run_with_r: clean_and_join_with_r
	Rscript src/visualization/script_111_render_all_notebooks.r

run_with_py: clean_and_join_with_py
#	...

clean_and_join_with_r: inflate
	Rscript src/data/script_02_join_data_with_column_names.r
	Rscript src/data/script_03_clean_data.r

clean_and_join_with_py: inflate
	python src/data/script_02_join_data_with_column_names.py
	python src/data/script_03_clean_data.py

inflate: clean
	bash src/data/script_01_unzip_data.sh

clean:
	bash src/data/script_00_clean.sh

