all: with_r

with_r: train_models_with_r create_seminar_paper_with_r

with_py: create_seminar_paper_with_py

create_seminar_paper_with_r:
	Rscript src/misc/script_901_render_final_seminar_paper.r

create_seminar_paper_with_py: train_models_with_py
# TODO: ...

train_models_with_r: visualize_with_r
	Rscript src/models/script_301_predict_purchases.r

train_models_with_py: visualize_with_py
# TODO: ...

visualize_with_r: clean_and_join_with_r
	Rscript src/visualization/script_111_render_all_notebooks.r

visualize_with_py: clean_and_join_with_py
# TODO: ...

clean_and_join_with_r: inflate
	Rscript src/data/script_02_join_data_with_column_names.r
	Rscript src/data/script_03_clean_data.r
#	Rscriptscript_004_join_data_sets.r

clean_and_join_with_py: inflate
	python src/data/script_02_join_data_with_column_names.py
	python src/data/script_03_clean_data.py
	python src/data/script_004_join_data_sets.py

inflate: clean
	bash src/data/script_01_unzip_data.sh

clean:
	bash src/data/script_00_clean.sh

install_deps:
	Rscript src/misc/install-dependent-packages.r
