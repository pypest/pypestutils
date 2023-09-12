import os
dirs = ["."]
notebook_count = 0
for d in dirs:
    nb_files = [os.path.join(d,f) for f in os.listdir(d) if f.lower().endswith(".ipynb")]
    for nb_file in nb_files:
        print("clearing",nb_file)
        os.system("jupyter nbconvert --ClearOutputPreprocessor.enabled=True --ClearMetadataPreprocessor.enabled=True --inplace {0}".format(nb_file))
        notebook_count += 1
print(notebook_count," notebooks cleared")
